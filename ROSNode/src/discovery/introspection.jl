# Introspection: the interfaces a peer uses to inspect this node, wired against the
# vendored `type_description_interfaces` / `rcl_interfaces` wire types. Two facilities:
#
#   • `~/get_type_description` — a Service answering from the Context type registry.
#     The served closure is canonicalized so its RIHS01 matches the hash advertised
#     on the graph, and an unknown (name, hash) gets a clean `successful=false` reply.
#     An `include_type_sources` request also returns `type_sources` regenerated from
#     each served entry's IL (`IL.unparse`).
#   • the `/rosout` bridge — an `AbstractLogger` teeing Julia logging to the parent
#     logger and to a node-owned `rcl_interfaces/msg/Log` publisher, so a Julia
#     `@info`/`@warn` on a node surfaces on the ROS `/rosout` topic.

import Logging
using Logging: AbstractLogger, LogLevel
using ROSZenoh: TypeInfo, type_hash_from_rihs_string

export RosoutLogger, with_rosout, Fatal, logger, set_logger_level!,
       bridge_zenoh_logs!, get_type_description, fetch_type_description,
       resolve_or_discover, wire_get_type_description!, describe_type

# ── ~/get_type_description: the registry-served type description ───────────────
# The serving side of rmw_zenoh's dynamic type discovery: a peer that sees a
# (name, RIHS01) on the graph but lacks the type fetches its `TypeDescription`
# here and codegens it. The request is
# `(type_name::String, type_hash::String, include_type_sources::Bool)`; the reply
# is `(successful::Bool, failure_reason::String, type_description, …)`. This file
# owns the registry lookup, the hash-string parsing, and the not-found contract;
# canonicalization and the entry shape are typesupport's.

"""
    describe_type(ctx_or_node, type_name, type_hash) -> Union{TypeDescriptionMsg, Nothing}

Look up the type registry ([`TypeRegistry`](@ref)) for `(type_name, type_hash)` and
return the canonical internal `TypeDescriptionMsg` (the main type plus its
referenced-type closure) that `~/get_type_description` serves, or `nothing` when no
entry with that exact name and version is registered. This is the server-side half of
rmw_zenoh's dynamic type discovery: a peer that sees a `(name, RIHS01)` on the graph
but lacks the type fetches its description here.

`type_hash` selects the version:

- `"RIHS01_<64-hex>"` — selects that exact version.
- empty string — matches any registered version of `type_name` (the "I have the name,
  send me whatever you advertise" form), resolving to the first matching entry under
  the registry lock.
- malformed RIHS string — resolves to `nothing` (treated as not-found).

The served description depends on the matched [`RegistryEntry`](@ref):

- entry storing a `td` — served from it directly, its closure sorted by `type_name` so
  the served description re-hashes to the RIHS01 advertised on the graph (the integrity
  invariant a receiving peer re-checks).
- IL-only entry (ament/static) — rebuilt: the main type from the struct AST, the closure
  collected from the registry by name, IL-only referenced types rebuilt the same way.
- a referenced type that cannot be resolved from the registry — throws an `ArgumentError`,
  since a truncated closure would fail the receiving peer's integrity gate anyway.

[`wire_get_type_description!`](@ref) converts the result to the wire form via
`to_wire_td`; a `nothing` result becomes a `successful=false` reply.
"""
function describe_type(ctx_or_node, type_name::AbstractString,
                       type_hash::AbstractString)
    reg = registry(_ctx(ctx_or_node))
    entry = if isempty(type_hash)
        # Name-only request: serve any registered version (each registry entry
        # carries the hash we advertise). A single-version registry — the common
        # case — has exactly one match.
        _lookup_any_version(reg, String(type_name))
    else
        h = type_hash_from_rihs_string(type_hash)
        h === nothing ? nothing :                       # malformed hash ⇒ not found
            lookup_type(reg, TypeInfo(String(type_name), h))
    end
    entry isa RegistryEntry || return nothing
    return _entry_type_description(reg, entry)
end

# The registry is keyed on `(name, hash)`, so a name-only lookup scans under the
# registry lock for the first entry whose name matches.
function _lookup_any_version(reg, name::String)
    @lock reg.lock begin
        for ((n, _h), entry) in reg.entries
            n == name && return entry
        end
    end
    return nothing
end

# Project a registry entry to the canonical internal `TypeDescriptionMsg`. A
# stored `td` is canonicalized and served directly; an IL-only entry is rebuilt
# from the struct AST with its referenced closure collected from the registry.
# `nothing` (a fail-safe not-found) when the entry has neither.
function _entry_type_description(reg, entry::RegistryEntry)
    entry.td !== nothing && return _canonicalize_tdmsg(entry.td)
    main = _entry_main_td(entry)
    main === nothing && return nothing
    return TypeDescriptionMsg(main, _collect_registry_closure(reg, main))
end

# Sort the referenced closure by `type_name`, the canonical order RIHS01 is
# defined over, so what we serve re-hashes to the hash advertised on the graph.
_canonicalize_tdmsg(td::TypeDescriptionMsg) =
    isempty(td.referenced_type_descriptions) ? td :
    TypeDescriptionMsg(td.type_description,
        sort(td.referenced_type_descriptions; by = t -> t.type_name))

# The main internal `TypeDescription` of a registry entry: its stored `td`'s main,
# else rebuilt from the IL via the struct AST. Reused for IL-only refs in a closure.
function _entry_main_td(e::RegistryEntry)
    e.td !== nothing && return e.td.type_description
    pkg, _, _ = split_ros_name(e.info.name)
    ast = try
        _scan_for_struct(lower(e.il; package = pkg))
    catch
        nothing
    end
    ast === nothing && return nothing
    return type_description_from_struct(ast, e.info.name; package = pkg, qualifier = "msg")
end

# Transitively collect `main`'s referenced types from the registry, sorted by
# `type_name` — the closure a peer needs to codegen a referencing type. An
# unresolvable ref throws: a truncated closure re-hashes to a RIHS01 differing
# from the advertised one and fails every peer's integrity gate.
function _collect_registry_closure(reg, main::TypeDescription)
    seen = Set{String}()
    out = TypeDescription[]
    function visit(td::TypeDescription)
        for f in td.fields
            nt = f.field_type.nested_type_name
            (isempty(nt) || nt in seen) && continue
            e = _lookup_any_version(reg, nt)
            sub = e isa RegistryEntry ? _entry_main_td(e) : nothing
            sub === nothing && throw(ArgumentError("cannot serve a complete type \
                description for $(main.type_name): referenced type $(nt) is not \
                resolvable from the registry"))
            push!(seen, nt); push!(out, sub); visit(sub)
        end
    end
    visit(main)
    sort!(out; by = t -> t.type_name)
    return out
end

"""
    get_type_description(client, type_name; type_hash="", include_sources=false,
                         timeout_ms=2000) -> GetTypeDescription_Response

Client side of `~/get_type_description`: ask a remote node for the `TypeDescription`
of `(type_name, type_hash)` (rmw_zenoh dynamic type discovery). Blocks the calling task
on the reply exactly like [`call`](@ref) — it yields while the request is in flight —
and returns the decoded `GetTypeDescription_Response`, whose `successful` flag
distinguishes "the remote has this type" from "it doesn't". Raises
[`ServiceError`](@ref) on an error reply, or when no reply arrives — a wait bounded by
`timeout_ms` plus `call`'s small hard backstop.

`client` is a [`ServiceClient`](@ref) built over the statically compiled
`GetTypeDescription` service type and pointed at the remote's `~/get_type_description`.
`type_hash` is the RIHS01 string (empty matches any advertised version);
`include_sources` requests the regenerated interface text (`type_sources`) alongside
the description. The higher-level [`fetch_type_description`](@ref) wraps this call with
the verify-and-register landing into the node's registry.
"""
function get_type_description(client, type_name::AbstractString;
                             type_hash::AbstractString="",
                             include_sources::Bool=false,
                             timeout_ms::Integer=2000)
    req = GetTypeDescription_Request(type_name = String(type_name),
                                     type_hash = String(type_hash),
                                     include_type_sources = include_sources)
    return call(client, req; timeout_ms = timeout_ms)
end

"""
    fetch_type_description(node, remote, type_name, hash; timeout_ms=2000) -> Union{RegistryEntry, Nothing}

The wire step of rmw_zenoh dynamic type discovery: call `<remote>/get_type_description`
for `(type_name, hash)`, then on a successful reply verify, lift, and register the
result into the node's type registry (via `register_type_description!`, which
also persists it to the content-addressed cache for the next run). Returns the
registered [`RegistryEntry`](@ref), or `nothing` in any of:

- the remote lacks the type;
- no server answered;
- the reply failed the RIHS01 integrity gate.

Blocks the calling task. First it waits up to `timeout_ms` for the remote's service
to become routable ([`wait_for_service`](@ref)) — discovery is eventually consistent,
so a query issued before the queryable is matched would silently match nothing — then
issues the [`get_type_description`](@ref) call under the same budget. `remote` is the
serving node's fully qualified name (e.g. `"/talker"`); `hash` is a `TypeHash` or a
RIHS string. The client is closed before return.

Error and return outcomes:

- malformed `hash` string — returns `nothing` immediately.
- [`ShutdownException`](@ref) — propagates.
- every other error (timeout, decode failure, hash mismatch from the gate) — caught
  and reported as `nothing`.

Choosing *which* source to try — registry, then cache, then ament, then this wire
path — is [`resolve_or_discover`](@ref)'s job; this function is only the wire leg.
"""
function fetch_type_description(node, remote::AbstractString, type_name::AbstractString,
                                hash; timeout_ms::Integer=2000)
    th = hash isa AbstractString ? type_hash_from_rihs_string(hash) : hash
    th === nothing && return nothing
    hash_str = to_rihs_string(th)
    svc = endswith(remote, "/") ? remote * "get_type_description" :
                                  remote * "/get_type_description"
    client = ServiceClient(node, svc, GetTypeDescription_Request)
    try
        # Discovery is eventually-consistent: a query issued before the remote's
        # queryable is routable silently matches nothing. Wait for the service to
        # become routable (same budget) before querying.
        if !wait_for_service(client; timeout = timeout_ms / 1000)
            @debug "fetch_type_description: service not discovered" remote svc timeout_ms
            return nothing
        end
        resp = get_type_description(client, type_name; type_hash = hash_str,
                                    timeout_ms = timeout_ms)
        if !resp.successful
            @debug "get_type_description: remote has no $(type_name) / $(hash_str)" remote reason=resp.failure_reason
            return nothing
        end
        tdmsg = from_wire_td(resp.type_description)
        info = TypeInfo(String(type_name), th)
        # register_type_description! runs the RIHS01 integrity gate: a definition
        # that doesn't hash to its advertised id throws rather than binding.
        return register_type_description!(registry(_ctx(node)), tdmsg, info)
    catch err
        err isa ShutdownException && rethrow()
        @debug "fetch_type_description failed" remote type_name exception=err
        return nothing
    finally
        close(client)
    end
end

# The node FQN advertising `(name, hash)` on the graph, to call its
# `~/get_type_description`. Scans the index for any endpoint with this exact
# TypeInfo and a known node identity, preferring a remote advertiser over a local
# one. `nothing` if none is visible.
function _find_remote_for(ctx, info::TypeInfo)
    local_fallback = nothing
    for ep in endpoints_snapshot(ctx)
        ep.type === nothing && continue
        (ep.type.name == info.name && ep.type.hash == info.hash) || continue
        isempty(ep.node_name) && continue
        fqn = _ns_join(_normalize_namespace(ep.namespace), ep.node_name)
        ep.is_local || return fqn          # prefer a remote server
        local_fallback = fqn
    end
    return local_fallback
end

"""
    resolve_or_discover(node, name, hash; ament=true, cache=true, wire=true,
                        timeout_ms=2000) -> Union{Type, Nothing}

Resolve `(name, hash)` to a realized (generated) Julia type, trying each source in
order and short-circuiting on the first hit:

1. **registry** — a type already realized this run.
2. **project cache** — a prior run's persisted `.json` blob (opt-in, [`@ros_cache`](@ref)).
3. **ament** — an installed `.msg`/`.srv` when inside a sourced ROS 2 environment.
4. **wire** — call the advertising remote's `~/get_type_description`
   ([`fetch_type_description`](@ref)), verify, register, and persist for next run.

Returns the generated type, or `nothing` when no source can resolve it. Steps 1–3 run
in [`resolve_type`](@ref); step 4 runs only when `wire=true` and steps 1–3 missed, and
blocks the calling task on discovery and the remote call (bounded by `timeout_ms`).
The wire step is taken against whichever node advertises `(name, hash)` on the graph,
preferring a remote advertiser over a local one. `ament`/`cache`/`wire` switch off the
corresponding source; `hash` is a `TypeHash` or RIHS string (a malformed string
returns `nothing`).

Type-revision trust (the `Context`'s `weak_types`): when a *pinned*
(`:static`/`:authored`) type is registered under `name` with a different hash, that
pin is enforced — the wire step is suppressed and the diverging peer revision is
*not* bound (the mismatch is already reported by `resolve_type`). `weak_types=true`
opts into the dynamic fallback: the wire step runs and binds the peer's revision.

The first sample of a type pays for discovery and codegen; subsequent samples are fast
registry lookups, and across runs the cache (or a baked `@ros_cache` static type) skips
re-discovery. The returned type lives in a newer world age than the compiled dispatch
path, so callers reach it through `Base.invokelatest`.
"""
function resolve_or_discover(node, name::AbstractString, hash;
                             ament::Bool=true, cache::Bool=true, wire::Bool=true,
                             timeout_ms::Integer=2000)
    ctx = _ctx(node)
    th = hash isa AbstractString ? type_hash_from_rihs_string(hash) : hash
    th === nothing && return nothing
    info = TypeInfo(String(name), th)

    # 1–3: registry → cache → ament (resolve_type's order). `warned` flags whether
    # the ament path already surfaced the revision diagnostic, to avoid a double warn.
    warned = Ref(false)
    entry = resolve_type(ctx, info; ament = ament, cache = cache, warned = warned)
    # 4: wire — ask whoever advertises it on the graph. Type-revision trust: a pinned
    # (`:static`/`:authored`) local type under a different hash enforces its RIHS01,
    # suppressing the wire bind of the peer's diverging revision unless the Context is
    # in weak mode.
    if entry === nothing && wire
        conflict = ctx.weak_types ? nothing : _pinned_conflict(registry(ctx), info.name, th)
        if conflict === nothing
            remote = _find_remote_for(ctx, info)
            remote === nothing ||
                (entry = fetch_type_description(node, remote, name, th; timeout_ms = timeout_ms))
        elseif !warned[]
            # Pinned entry under a different hash with no local ament file to trip the
            # acquire-path diagnostic: surface the mismatch here so the suppression is
            # visible to the user.
            _warn_revision_mismatch(info.name, conflict.info.hash, th; weak = false)
        end
    end

    entry isa RegistryEntry || return nothing
    realize!(entry)
    return entry.type
end

# A well-formed empty wire TypeDescription, for the not-found reply.
_empty_wire_td() =
    WireTypeDescription(
        type_description = WireIndividualTypeDescription(type_name = "", fields = WireField[]),
        referenced_type_descriptions = WireIndividualTypeDescription[])

# The "I don't have it" reply: a normal reply (not an error) with
# `successful=false` — the ROS2 contract for "type not available".
_gtd_not_found(type_name::AbstractString) =
    GetTypeDescription_Response(successful = false,
        failure_reason = "type not available: $(type_name)",
        type_description = _empty_wire_td(),
        type_sources = WireTypeSource[], extra_information = WireKeyValue[])

# One `WireTypeSource` regenerated from a registry entry's IL (for an
# `include_type_sources` request): `encoding` is the IL kind's qualifier
# (`msg`/`srv`/`action`), `raw_file_contents` the `IL.unparse` text. A bad unparse
# is contained to an empty body so one exotic type can't fail the whole reply.
function _entry_type_source(entry::RegistryEntry)
    encoding, _ = _il_qualifier_ext(entry.il)
    raw = try
        sprint(IL.unparse, entry.il)
    catch
        ""
    end
    return WireTypeSource(type_name = entry.info.name, encoding = encoding,
                          raw_file_contents = raw)
end

# Sources for the served closure, main type first then each referenced type in the
# served `tdmsg`. Iterating the served `tdmsg` (not a fresh registry walk) keeps
# the sources and `type_description` in lockstep. A concurrently-removed type just
# yields fewer sources.
function _type_sources(reg, tdmsg::TypeDescriptionMsg)
    out = WireTypeSource[]
    seen = Set{String}()
    names = String[tdmsg.type_description.type_name]
    for t in tdmsg.referenced_type_descriptions
        push!(names, t.type_name)
    end
    for name in names
        name in seen && continue
        push!(seen, name)
        entry = _lookup_any_version(reg, name)
        entry isa RegistryEntry || continue
        push!(out, _entry_type_source(entry))
    end
    return out
end

"""
    wire_get_type_description!(node) -> ServiceHandle

Declare the hidden `~/get_type_description` service on `node` and start serving it.
The handler answers from the Context's [`TypeRegistry`](@ref) via
[`describe_type`](@ref), converting the canonical internal `TypeDescriptionMsg` to the
wire `type_description_interfaces/srv/GetTypeDescription` form (`to_wire_td`).
A request for an unknown `(name, hash)` gets a clean `successful=false` reply rather
than an error. Every node serves this so peers can fetch the descriptions it
advertises on the graph.

Returns the [`ServiceHandle`](@ref), tracked on `node` and reaped by `close(node)`
like any other entity. The request's `include_type_sources` flag sets `type_sources`:

- `true` — carries one regenerated `IL.unparse` text per served type (via `_type_sources`).
- `false` — empty, since the description alone is enough to decode.
"""
function wire_get_type_description!(node)
    return Service(node, "~/get_type_description", GetTypeDescription_Request) do req
        tdmsg = describe_type(node, req.type_name, req.type_hash)
        tdmsg === nothing && return _gtd_not_found(req.type_name)
        sources = req.include_type_sources ?
            _type_sources(registry(_ctx(node)), tdmsg) : WireTypeSource[]
        return GetTypeDescription_Response(
            successful = true, failure_reason = "",
            type_description = to_wire_td(tdmsg),
            type_sources = sources,
            extra_information = WireKeyValue[])
    end
end

# ── /rosout: the Julia-logging → rcl_interfaces/msg/Log bridge ────────────────
# Julia's `@info`/`@warn`/`@error`/`@logmsg` are the ROS logging API.
# `RosoutLogger <: AbstractLogger` is node-scoped and, per record, (a) writes the
# ROS console line `[LEVEL] [stamp] [name]: msg` to stderr, (b) publishes an
# `rcl_interfaces/msg/Log` on the node's shared `/rosout` publisher, and (c)
# forwards to an optional `parent` logger (a file/Julia console sink). The
# dispatcher wraps every handler in the node logger, so a plain `@info` inside any
# handler routes to that node's `/rosout` with no user effort.
#
# Logger name = node FQN; `logger(node, "child")` gives a `.`-separated child. Per-
# logger min levels live in a node-local table (`set_logger_level!`). `maxlog` is
# the logger's responsibility, enforced here.
#
# The level mapping follows ROS2's `rcl_interfaces/msg/Log` constants (DEBUG=10,
# INFO=20, WARN=30, ERROR=40, FATAL=50). Julia's standard levels stop at Error, so
# `Fatal` (above `Error`) maps `@logmsg Fatal …` to ROS2 FATAL.

const LogMsg  = Interfaces.rcl_interfaces.msg.Log
const TimeMsg = Interfaces.builtin_interfaces.msg.Time

"""
    Fatal

A Julia `LogLevel` of `3000`, one step above `Logging.Error` (`2000`), giving Julia
code a way to request ROS 2's `FATAL` severity. A [`RosoutLogger`](@ref) maps
`@logmsg Fatal …` to the `rcl_interfaces/msg/Log` `FATAL` constant (`0x32`) and labels
the console line `[FATAL]`; a plain `@error` stays at ROS 2 `ERROR`.

```julia
using Logging
@logmsg Fatal "reactor breach"
```
"""
const Fatal = Logging.LogLevel(3000)

# ROS2 severity constants (rcl_interfaces/msg/Log). Named here so the bridge is
# self-contained; identical to the generated `Log_Constants.*`.
const _ROSOUT_DEBUG = 0x0a
const _ROSOUT_INFO  = 0x14
const _ROSOUT_WARN  = 0x1e
const _ROSOUT_ERROR = 0x28
const _ROSOUT_FATAL = 0x32

# Map a Julia `LogLevel` to the ROS2 `Log.level` byte. Julia levels are an ordered
# `Int32` scale (Debug=-1000, Info=0, Warn=1000, Error=2000); we bucket on the
# standard thresholds so a custom level between two standards rounds to the lower.
function _rosout_level(level::LogLevel)
    level >= Fatal        && return _ROSOUT_FATAL
    level < Logging.Info  && return _ROSOUT_DEBUG
    level < Logging.Warn  && return _ROSOUT_INFO
    level < Logging.Error && return _ROSOUT_WARN
    return _ROSOUT_ERROR
end

# Same bucketing as `_rosout_level`, but the ROS console label (`[INFO]` etc.).
function _console_level_label(level::LogLevel)
    level >= Fatal        && return "FATAL"
    level < Logging.Info  && return "DEBUG"
    level < Logging.Warn  && return "INFO"
    level < Logging.Error && return "WARN"
    return "ERROR"
end

"""
    RosoutLogger(node; name=node.fqn, parent=nothing, console=true,
                 min_level=Logging.BelowMinLevel)

An `AbstractLogger` that bridges Julia logging to ROS 2's `/rosout`. Per record it
drives up to three sinks:

- when `console=true` — writes the ROS console line `[LEVEL] [secs.nanosec] [name]: msg`
  to stderr.
- always — publishes an `rcl_interfaces/msg/Log` on `node`'s shared `/rosout` publisher.
- when a `parent` logger is supplied — forwards the raw record to that extra sink (a
  file or Julia console).

Installing it routes a plain `@info`/`@warn`/
`@error`/`@logmsg` to ROS 2 logging; the request dispatcher installs the node's
logger around every handler so handler logs reach `/rosout` automatically.

`name` is the ROS logger name (the node FQN by default; [`logger`](@ref)`(node,
"child")` makes a `.`-separated child sharing the same publisher and console).
`min_level` is this logger's own floor; per-name overrides come from the node's level
table ([`set_logger_level!`](@ref)) and take precedence by longest matching prefix.
The Julia level `Logging.Error` maps to ROS 2 `ERROR`; use [`Fatal`](@ref) for ROS 2
`FATAL`.

Each sink is best-effort and isolated: a failure in one (a bad `show`, a dead
`/rosout` route) is reported to stderr and never breaks the others or escapes the
handler. Installing the logger is always safe: the console and `parent` sinks work
unconditionally, and the `/rosout` sink engages whenever the node has a live session
to declare the publisher on.

```julia
lg = RosoutLogger(node; parent = ConsoleLogger())
with_logger(lg) do
    @info "armed"          # ROS console, /rosout, AND the Julia console
end
```
"""
mutable struct RosoutLogger <: AbstractLogger
    const node::Any                              # the Node whose /rosout it publishes to
    const name::String                           # ROS logger name (FQN or "<fqn>.child")
    const parent::Union{AbstractLogger, Nothing} # optional extra sink (file/Julia console)
    const console::Bool                          # write the ROS-format line to stderr
    const min_level::LogLevel
    # The shared `/rosout` publisher (a `PublisherHandle{LogMsg}`); `nothing` for a
    # sessionless node, where the bridge skips the /rosout sink.
    _pub::Any
    # Per-call-site-id `maxlog` emit counts (the logger enforces `maxlog`), mirroring
    # `ConsoleLogger`'s `message_limits`.
    const _maxlog_counts::Dict{Any, Int}
    # Guards `_maxlog_counts`: the node's one logger is shared across `Parallel(n)`
    # handler threads, and concurrent `Dict` mutation can corrupt it.
    const _maxlog_lock::ReentrantLock
    RosoutLogger(node, name, parent, console, min_level, _pub, _maxlog_counts) =
        new(node, name, parent, console, min_level, _pub, _maxlog_counts,
            ReentrantLock())
end

_default_logger_name(node) = node isa Node ? node.fqn : string(node)

function RosoutLogger(node; name::AbstractString=_default_logger_name(node),
                      parent::Union{AbstractLogger, Nothing}=nothing,
                      console::Bool=true,
                      min_level::LogLevel=Logging.BelowMinLevel)
    lg = RosoutLogger(node, String(name), parent, console, min_level, nothing,
                      Dict{Any, Int}())
    lg._pub = _rosout_publisher!(node)
    return lg
end

# The node-owned `/rosout` publisher, declared once and shared by the node logger
# and every `logger(node,"child")`. The QoS is ROS 2's rosout default (KEEP_LAST
# 1000, reliable, transient-local, 10 s lifespan) so native peers see a matching
# offer and late subscribers (`ros2 topic echo /rosout`) get the backlog. A
# sessionless/closed node returns `nothing` so the bridge skips the /rosout sink
# instead of throwing during logger construction.
function _rosout_publisher!(node)
    (node isa Node && isopen(node)) || return nothing
    @lock node.lock begin
        node._rosout_pub === nothing || return node._rosout_pub
        node._rosout_pub = try
            Publisher(node, "/rosout", LogMsg;
                      qos = QosProfile(durability = :transient_local,
                                       reliability = :reliable, depth = 1000,
                                       lifespan = ROSZenoh.Duration(10, 0)))
        catch err
            @debug "rosout bridge: /rosout publisher declaration failed" exception=(err, catch_backtrace())
            nothing
        end
        return node._rosout_pub
    end
end

"""
    logger(node) -> RosoutLogger
    logger(node, child::AbstractString) -> RosoutLogger

The node's default ROS logger, or a `.`-separated child of it. `logger(node)` returns
the one logger cached on the node (name = node FQN), building it once on first call;
`logger(node, child)` returns a fresh [`RosoutLogger`](@ref) named `"<fqn>.child"` that
shares the node's `/rosout` publisher and console.

The request dispatcher installs `logger(node)` around every handler, so handlers rarely
need this directly. Reach for it to build a child logger for sub-component hierarchy,
or to log outside a handler via `with_logger(node) do … end`. The single-argument form
uses double-checked locking so the per-message dispatch path stays lock-free once the
logger is cached.
"""
function logger(node::Node)
    # Double-checked locking: the dispatcher calls this per message, so the cached
    # `_logger` is read unlocked; a stale `nothing` falls to the locked re-check.
    node._logger === nothing || return node._logger
    @lock node.lock begin
        node._logger === nothing || return node._logger
        node._logger = RosoutLogger(node)
        return node._logger
    end
end
logger(node::Node, child::AbstractString) =
    RosoutLogger(node; name = string(node.fqn, ".", child))

# Run a user handler `f` under the node's `RosoutLogger`, so a plain `@info` inside
# it routes to that node's `/rosout` + ROS console. The dispatch sites call this; a
# non-`Node` owner runs `f` unwrapped.
_with_node_logger(f, node::Node) = Logging.with_logger(f, logger(node))
_with_node_logger(f, ::Any)      = f()

# Emit one record to up to three sinks: ROS console (stderr), an optional `parent`
# logger, and the `/rosout` publisher, after gating `maxlog`. Each sink is best-
# effort and isolated — a failure in one (a bad `show`, a dead `/rosout` route)
# must not break the others or escape `handle_message`. Errors go straight to
# stderr, not through logging, to avoid recursion under the active logger.
function Logging.handle_message(lg::RosoutLogger, level, message, _module, group,
                                id, file, line; kwargs...)
    _maxlog_exceeded!(lg, id, kwargs) && return nothing
    if lg.console
        try
            _print_ros_console(lg, level, message; kwargs...)
        catch err
            Base.println(stderr, "rosout: console format failed: ", err)
        end
    end
    if lg.parent !== nothing
        try
            Logging.handle_message(lg.parent, level, message, _module, group, id,
                                   file, line; kwargs...)
        catch err
            Base.println(stderr, "rosout: parent logger failed: ", err)
        end
    end
    if lg._pub !== nothing
        try
            _emit_rosout(lg, level, message, file, line; kwargs...)
        catch err
            Base.println(stderr, "rosout: /rosout publish failed: ", err)
        end
    end
    nothing
end

# `maxlog`: emit at most `maxlog` records per call-site `id`, the count kept on the
# logger as `ConsoleLogger` does. Locked because this one logger is shared across
# `Parallel(n)` concurrent handler threads.
function _maxlog_exceeded!(lg::RosoutLogger, id, kwargs)
    maxlog = get(kwargs, :maxlog, nothing)
    maxlog === nothing && return false
    @lock lg._maxlog_lock begin
        n = get(lg._maxlog_counts, id, 0)
        n >= maxlog && return true
        lg._maxlog_counts[id] = n + 1
        return false
    end
end

# The node's ROS clock instant as `(sec, nanosec)` for a stamp, falling back to
# `(0,0)` for a non-`Node` or closed node so the console line never throws.
function _stamp_sec_nanosec(node)
    try
        return _sec_nanosec(Dates.now(node).ns)   # `now(node)` extends `Dates.now`
    catch
        return (Int32(0), UInt32(0))
    end
end

# Write the ROS-format console line: `[LEVEL] [secs.nanosec] [logger_name]: msg`.
# Built as one string and printed in a single `print` so concurrent records don't
# interleave mid-line. Uses the logger's own `name`, so a child logger's records
# show the child name even sharing the parent's stderr.
function _print_ros_console(lg::RosoutLogger, level, message; kwargs...)
    sec, nanosec = _stamp_sec_nanosec(lg.node)
    Base.print(stderr, "[", _console_level_label(level), "] [", sec, ".",
               lpad(string(nanosec), 9, '0'), "] [", lg.name, "]: ",
               _render_message(message, kwargs), "\n")
    return nothing
end

# Internal keys `@info`/`@warn` add to a record that are not user payload: `:maxlog`
# (rate-limit count) and `:_id` (the call-site id). Skip them when folding kwargs
# into the message text.
const _ROSOUT_SKIP_KWARGS = (:maxlog, :_id)

# Render a log record's `message` + structured `kwargs` into one `Log.msg` string.
# `@warn "x" a=1 b=2` carries the extras as kwargs, appended as `(a=1, b=2)` so the
# structure survives onto a flat ROS string. An `:exception` renders as its message
# (the backtrace would bloat the line). Must never throw: a bad `show`/`string` on a
# user value falls back to a placeholder.
function _render_message(message, kwargs)
    base = try
        string(message)
    catch
        "<unprintable message>"
    end
    parts = String[]
    for (k, v) in pairs(kwargs)
        k in _ROSOUT_SKIP_KWARGS && continue
        rendered = try
            if k === :exception
                ex = v isa Tuple ? first(v) : v        # (exception, backtrace) or bare
                ex isa Exception ? sprint(showerror, ex) : string(ex)
            else
                string(v)
            end
        catch
            "<unprintable>"
        end
        push!(parts, string(k, "=", rendered))
    end
    isempty(parts) ? base : string(base, " (", join(parts, ", "), ")")
end

# Build and publish the `rcl_interfaces/msg/Log` for one record. The `function`
# field is empty: `handle_message` carries no enclosing-function name, matching what
# rmw_zenoh emits when the producing function is unknown.
function _emit_rosout(lg::RosoutLogger, level, message, file, line; kwargs...)
    log = LogMsg(; stamp = to_msg(TimeMsg, Dates.now(lg.node)),
                 level = _rosout_level(level),
                 name = lg.name,
                 msg = _render_message(message, kwargs),
                 file = string(file),
                 var"function" = "",
                 line = UInt32(line === nothing ? 0 : line))
    publish(lg._pub, log)
    return nothing
end

# The effective min level: the longest-prefix match in the node's level table (set
# by `set_logger_level!`), else the logger's own `min_level`. A child `/ns/n.child`
# inherits `/ns/n`'s level through the prefix match until it has its own entry.
function _effective_level(lg::RosoutLogger)
    lg.node isa Node || return lg.min_level
    lvls = lg.node._log_levels
    isempty(lvls) && return lg.min_level
    best = nothing; bestlen = -1
    for (nm, lv) in lvls
        (lg.name == nm || startswith(lg.name, nm * ".")) || continue
        length(nm) > bestlen && (best = lv; bestlen = length(nm))
    end
    return best === nothing ? lg.min_level : LogLevel(best)
end

# `AbstractLogger` interface. Gate on the effective level, then defer to a `parent`
# sink's `shouldlog`/`min_enabled_level` so its per-module settings still apply.
function Logging.shouldlog(lg::RosoutLogger, level, _module, group, id)
    level >= _effective_level(lg) || return false
    lg.parent === nothing && return true
    return Logging.shouldlog(lg.parent, level, _module, group, id)
end

Logging.min_enabled_level(lg::RosoutLogger) =
    lg.parent === nothing ? _effective_level(lg) :
        min(_effective_level(lg), Logging.min_enabled_level(lg.parent))

Logging.catch_exceptions(lg::RosoutLogger) =
    lg.parent === nothing ? false : Logging.catch_exceptions(lg.parent)

"""
    set_logger_level!(node, level::LogLevel)              # the node's own logger (FQN)
    set_logger_level!(node, name::AbstractString, level::LogLevel)  # a specific logger name

Set a per-logger minimum severity in the node's level table. A `name` entry gates
that logger and every `.`-separated descendant that lacks its own entry — so
`set_logger_level!(node, "/n", Logging.Warn)` also gates `"/n.child"` (longest matching
prefix wins). The two-argument form targets the node's own logger (its FQN). This is
the programmatic form of `--ros-args --log-level name:=…`.

Returns `nothing`. Levels are Julia `LogLevel`s; a [`RosoutLogger`](@ref) consults this
table on every `shouldlog`/`min_enabled_level` check.
"""
set_logger_level!(node::Node, name::AbstractString, level::LogLevel) =
    (node._log_levels[String(name)] = level.level; nothing)
set_logger_level!(node::Node, level::LogLevel) =
    set_logger_level!(node, node.fqn, level)

# Node forms of the standard scope/global installers: `with_logger(node) do … end`
# and `global_logger(node)` use the node's default `RosoutLogger`.
Logging.with_logger(f::Function, node::Node) = Logging.with_logger(f, logger(node))
Logging.global_logger(node::Node) = Logging.global_logger(logger(node))

"""
    with_rosout(f, node; name=node.fqn, parent=nothing, console=true,
                min_level=Logging.BelowMinLevel)

Run `f()` with a fresh [`RosoutLogger`](@ref) (built from the keyword arguments)
installed as the active logger for the dynamic extent of the call. Returns
whatever `f()` returns. Use this to override logger options — `parent=`, `console=`,
`min_level=`, `name=` — for one scope; `with_logger(node) do … end` is the everyday
form and reuses the node's cached logger.

```julia
with_rosout(node; parent = ConsoleLogger()) do
    @info "armed"          # ROS console, /rosout, AND the Julia console
end
```
"""
with_rosout(f, node; kwargs...) =
    Logging.with_logger(f, RosoutLogger(node; kwargs...))

# ── Zenoh transport logs → /rosout (opt-in) ───────────────────────────────────
# Zenoh.jl captures the Rust core's logs (session, transport, liveliness) into a
# bounded, pull-based `LogStream` (`open_log_stream`). A background task drains it
# and re-emits each record through Julia logging under a `<fqn>.zenoh` child logger,
# so it rides the same path to `/rosout` + ROS console + the level table.
#
# Opt-in, off by default: only this call claims Zenoh's process-global (one-shot)
# logger. Keep the `min_severity` floor at WARN: Zenoh applies it inside the stream,
# so a `/rosout` publish's own data-plane DEBUG/TRACE logs stay below it and
# steady-state forwarding can't feed itself; the drop-oldest ring backstops the rest.

_julia_log_level(s) =
    s == Zenoh.LogSeverities.ERROR ? Logging.Error :
    s == Zenoh.LogSeverities.WARN  ? Logging.Warn  :
    s == Zenoh.LogSeverities.INFO  ? Logging.Info  : Logging.Debug

"""
    bridge_zenoh_logs!(node; min_severity=Zenoh.LogSeverities.WARN, capacity=256) -> Zenoh.LogStream

Capture Zenoh's transport logs (session, transport, liveliness) at `min_severity` and
above, and re-emit each one through Julia logging under a `<fqn>.zenoh` child logger,
so they ride the same path to `/rosout`, the ROS console, and the node's level table.
Returns the `Zenoh.LogStream`.

Opt-in and off by default. It spawns a background task that drains the stream; the
node's Context registers an [`on_shutdown`](@ref) hook that closes the stream on drain,
which ends the iterator and lets the task exit cleanly. `capacity` bounds the stream's
drop-oldest ring.

Zenoh's logger is process-global and one-shot: call this at most once per process, and
never together with `Zenoh.setup_logging`. Keep `min_severity` at `WARN` (the default):
the WARN floor keeps steady-state forwarding self-contained, since a `/rosout` publish's
own data-plane DEBUG/TRACE records stay below it.
"""
function bridge_zenoh_logs!(node::Node;
                            min_severity = Zenoh.LogSeverities.WARN,
                            capacity::Integer = 256)
    stream = Zenoh.open_log_stream(; min_severity = min_severity, capacity = capacity)
    zlog = logger(node, "zenoh")
    task = Threads.@spawn Logging.with_logger(zlog) do
        try
            for rec in stream
                Logging.@logmsg _julia_log_level(rec.severity) rec.message
            end
        catch err
            err isa ShutdownException && return
            Base.println(stderr, "rosout: zenoh log bridge drain failed: ", err)
        end
    end
    # Close the stream on Context drain so the drain task exits and doesn't outlive
    # the session (the iterator ends when the stream closes).
    on_shutdown(_ctx(node)) do
        try; close(stream); catch; end
    end
    return stream
end

# `_ctx`, `registry`, `lookup_type`, `now`, and `to_msg` are defined elsewhere in
# the module and in scope here.
