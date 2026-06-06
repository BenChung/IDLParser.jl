# §13 Introspection — the dual of discovery: we *serve* the interfaces others use
# to inspect us. Two facilities live here:
#
#   • `~/get_type_description` — a Service answering from the Context type registry
#     (§11). Every registered type carries its `TypeDescription`; the served
#     closure is *canonicalized* so its RIHS01 matches the hash we advertise, and
#     a request for an unknown (name, hash) is a clean `successful=false` reply.
#   • the `/rosout` bridge — an `AbstractLogger` teeing Julia logging to the parent
#     logger *and* to a node-owned `rcl_interfaces/msg/Log` publisher, so a Julia
#     `@info`/`@warn` on a node surfaces on the ROS `/rosout` topic.
#
# Both are wired against the vendored `type_description_interfaces` / `rcl_interfaces`
# wire types (the `Interfaces` subtree, wellknown.jl): `~/get_type_description`
# serves real `TypeDescription`s from the §11 registry, and the `/rosout` bridge
# publishes `rcl_interfaces/msg/Log`. A request with `include_type_sources` also gets
# `type_sources` regenerated from each served entry's IL (`IL.unparse`).

import Logging
using Logging: AbstractLogger, LogLevel
# `TypeInfo` is in scope (core.jl re-exports it); the RIHS-string parser is not.
using ROSZenoh: TypeInfo, type_hash_from_rihs_string

export RosoutLogger, with_rosout, Fatal, logger, set_logger_level!,
       bridge_zenoh_logs!, get_type_description, fetch_type_description,
       resolve_or_discover, wire_get_type_description!, describe_type

# ── ~/get_type_description: the registry-served type description (§11/§13) ─────
# rmw_zenoh exposes `~/get_type_description` (a `type_description_interfaces/srv/
# GetTypeDescription` service) so a peer that sees our (name, RIHS01) on the graph
# but doesn't have the type can fetch its `TypeDescription` from us and codegen it
# (the §11 dynamic-discovery handshake, seen from the *serving* side). The request
# is `(type_name::String, type_hash::String, include_type_sources::Bool)`; the
# reply is `(successful::Bool, failure_reason::String, type_description, …)`.
#
# The answer is a registry read: `lookup_type(registry, TypeInfo(name, hash))`
# returns the §11 entry, from which the typesupport layer can produce a canonical
# `TypeDescription` whose `calculate_rihs01_hash` equals the requested hash. That
# canonicalization (and the entry shape) is typesupport's; here we own the lookup,
# the hash-string parsing, and the not-found contract.

"""
    describe_type(ctx_or_node, type_name, type_hash) -> Union{TypeDescriptionMsg, Nothing}

Look up the registry entry (§11) for `(type_name, type_hash)` and return the
canonical internal `TypeDescriptionMsg` (main + referenced closure) to serve over
`~/get_type_description`, or `nothing` if the exact name+version isn't registered.
`type_hash` is the REP-2011 RIHS string (e.g. `"RIHS01_<64-hex>"`); an empty hash
matches *any* registered version of `type_name` (the rmw_zenoh "I have the name,
send me whatever you advertise" form).

The result is canonicalized (referenced closure sorted by `type_name`) so its
`calculate_rihs01_hash` equals the hash we advertise (§13). `nothing` lookups
become a well-formed `successful=false` reply — the correct "type not available"
contract. The server ([`wire_get_type_description!`](@ref)) converts the result to
the wire form via [`to_wire_td`](@ref).
"""
function describe_type(ctx_or_node, type_name::AbstractString,
                       type_hash::AbstractString)
    reg = registry(_ctx(ctx_or_node))
    entry = if isempty(type_hash)
        # Name-only request: serve any registered version (each §11 entry carries
        # the hash we advertise). A single-version registry — the common case —
        # has exactly one match.
        _lookup_any_version(reg, String(type_name))
    else
        h = type_hash_from_rihs_string(type_hash)
        h === nothing ? nothing :                       # malformed hash ⇒ not found
            lookup_type(reg, TypeInfo(String(type_name), h))
    end
    entry isa RegistryEntry || return nothing
    return _entry_type_description(reg, entry)
end

# Registry is keyed on `(name, hash)`; a name-only lookup scans for the first
# entry whose name matches. The registry owns the lock — reach the table through
# it. Kept here (not in context.jl) because it's a §13-specific convenience over
# the generic `(name, hash)` key, not part of the registry's core contract.
function _lookup_any_version(reg, name::String)
    @lock reg.lock begin
        for ((n, _h), entry) in reg.entries
            n == name && return entry
        end
    end
    return nothing
end

# Project a §11 registry entry to the canonical internal `TypeDescriptionMsg` we
# serve. Prefer the entry's stored `td` (wire-discovered / cached / well-known
# entries carry it); otherwise rebuild from the IL (ament/static entries) — lower
# to the struct AST, hash-encode it, and collect the referenced closure from the
# registry. The closure is sorted by `type_name` so `calculate_rihs01_hash` of
# what we serve equals the hash we advertise (§13 hash parity). `nothing` when the
# entry has neither a `td` nor a recoverable struct AST — a fail-safe not-found.
function _entry_type_description(reg, entry::RegistryEntry)
    entry.td !== nothing && return _canonicalize_tdmsg(entry.td)
    pkg, _, _ = split_ros_name(entry.info.name)
    ast = try
        _scan_for_struct(lower(entry.il; package = pkg))
    catch
        nothing
    end
    ast === nothing && return nothing
    main = type_description_from_struct(ast, entry.info.name; package = pkg, qualifier = "msg")
    return TypeDescriptionMsg(main, _collect_registry_closure(reg, main))
end

# Re-sort a TypeDescriptionMsg's referenced closure by `type_name` — the canonical
# order the RIHS01 hash is defined over (the hash itself does not sort, §13).
_canonicalize_tdmsg(td::TypeDescriptionMsg) =
    isempty(td.referenced_type_descriptions) ? td :
    TypeDescriptionMsg(td.type_description,
        sort(td.referenced_type_descriptions; by = t -> t.type_name))

# The main internal `TypeDescription` of a registry entry, if recoverable (its
# stored `td`'s main; rebuilding from IL is the describe path's job, not here).
_entry_main_td(e::RegistryEntry) = e.td === nothing ? nothing : e.td.type_description

# Transitively collect the referenced types of `main` from the registry (by name),
# sorted by `type_name` — so a served description carries the closure a peer needs
# to codegen a referencing type (§11/§13). Refs not in the registry are skipped.
function _collect_registry_closure(reg, main::TypeDescription)
    seen = Set{String}()
    out = TypeDescription[]
    function visit(td::TypeDescription)
        for f in td.fields
            nt = f.field_type.nested_type_name
            (isempty(nt) || nt in seen) && continue
            e = _lookup_any_version(reg, nt)
            e isa RegistryEntry || continue
            sub = _entry_main_td(e)
            sub === nothing && continue
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

Client side of `~/get_type_description` (§11 dynamic discovery, §13): ask a remote
node for the `TypeDescription` of `(type_name, type_hash)`. Blocks the calling task
on the reply (like [`call`](@ref)); raises [`ServiceError`](@ref) on an error reply
(or no server within `timeout_ms`) and returns the decoded response otherwise
(whose `successful` flag distinguishes "have it" from "don't").

`client` is a [`ServiceClient`](@ref) for the remote's `~/get_type_description`,
built over the statically-compiled `GetTypeDescription` service type (S1). The
higher-level [`fetch_type_description`](@ref) wraps this with the registry landing.
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

The wire half of dynamic discovery (§11): call `<remote>/get_type_description` for
`(type_name, hash)`, and on a successful reply verify + lift + register the result
into the node's registry (the back half [`register_type_description!`](@ref), which
also persists it to the content-addressed cache). Returns the registered
[`RegistryEntry`](@ref), or `nothing` if the remote doesn't have it / no server
answered / the reply failed the RIHS01 integrity gate.

`remote` is the serving node's FQN (e.g. `"/talker"`); `hash` is a `TypeHash` (or a
RIHS string). The orchestration of *which* source to try (registry → cache → ament
→ this wire path) is [`resolve_or_discover`](@ref)'s; this is just the wire step.
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
        # Discovery is eventually-consistent (§12.1): a query issued before the remote's
        # queryable is routable matches nothing and never replies. Wait for the service
        # to appear (same budget) before querying — else the get silently round-trips to
        # nothing on a cold route.
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
        # register_type_description! runs the RIHS01 integrity gate; a mismatch
        # throws (never trust a definition that doesn't hash to its advertised id).
        return register_type_description!(registry(_ctx(node)), tdmsg, info)
    catch err
        err isa ShutdownException && rethrow()
        @debug "fetch_type_description failed" remote type_name exception=err
        return nothing
    finally
        close(client)
    end
end

# The remote node FQN advertising `(name, hash)` on the graph, to call its
# `~/get_type_description`. Scans the discovery index for any endpoint with this
# exact TypeInfo and a known node identity; prefers a remote (non-local) one (our
# own types were already checked in the registry). `nothing` if none is visible.
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

Resolve `(name, hash)` to a realized (generated) Julia type, trying every source in
order, each short-circuiting the next (§11 / D5 S4):

1. **registry** — already realized this run.
2. **project cache** — a prior run's persisted `.json` blob (opt-in; [`@ros_cache`](@ref)).
3. **ament** — an installed `.msg`/`.srv` when inside a sourced ROS2 env.
4. **wire** — call the advertising remote's `~/get_type_description`
   ([`fetch_type_description`](@ref)), verify, register, and persist for next run.

The first sample of a type pays discovery + codegen; the rest are fast registry
lookups; across runs the cache (or a baked `@ros_cache` static type) skips
re-discovery. Returns the generated type (reached by the caller via `invokelatest`,
newer world age, §11) or `nothing` if unresolvable from any source.
"""
function resolve_or_discover(node, name::AbstractString, hash;
                             ament::Bool=true, cache::Bool=true, wire::Bool=true,
                             timeout_ms::Integer=2000)
    ctx = _ctx(node)
    th = hash isa AbstractString ? type_hash_from_rihs_string(hash) : hash
    th === nothing && return nothing
    info = TypeInfo(String(name), th)

    # 1–3: registry → cache → ament (resolve_type's order).
    entry = resolve_type(ctx, info; ament = ament, cache = cache)
    # 4: wire — ask whoever advertises it on the graph.
    if entry === nothing && wire
        remote = _find_remote_for(ctx, info)
        remote === nothing ||
            (entry = fetch_type_description(node, remote, name, th; timeout_ms = timeout_ms))
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

# The "I don't have it" reply (§13): a normal reply (not an error) with
# `successful=false` — the ROS2 contract for "type not available".
_gtd_not_found(type_name::AbstractString) =
    GetTypeDescription_Response(successful = false,
        failure_reason = "type not available: $(type_name)",
        type_description = _empty_wire_td(),
        type_sources = WireTypeSource[], extra_information = WireKeyValue[])

# One `WireTypeSource` regenerated from a registry entry's IL (§13,
# `include_type_sources`): `encoding` is the IL kind's qualifier
# (`msg`/`srv`/`action`), `raw_file_contents` the `IL.unparse` text — mirrors
# `_export_interface_text`. The entry stores no original source, so the text is
# regenerated (fields-only for wire-discovered types, faithful for static/ament/
# well-known). The try/catch contains a bad unparse (exotic IL) to an empty body so
# one type can't fail the whole reply.
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

# Sources for the served closure (§13): main type first, then each referenced type
# in the (already-sorted) served `tdmsg`, each looked up by name in `reg` and
# regenerated via `_entry_type_source`. Iterating the served `tdmsg` (not a fresh
# registry walk) keeps sources and `type_description` in lockstep. De-dup by name;
# skip names with no entry (a concurrently-removed type ⇒ fewer sources, no crash).
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

Declare the hidden `~/get_type_description` service on `node` (§13): a
`type_description_interfaces/srv/GetTypeDescription` server whose handler answers
from the Context type registry (§11) via [`describe_type`](@ref), converting the
canonical internal `TypeDescriptionMsg` to the wire form ([`to_wire_td`](@ref)). A
request for an unknown `(name, hash)` is a clean `successful=false` reply. Every
node serves this so peers can fetch the descriptions it advertises. The handle is
tracked on `node` (closed with it) like any other entity.

When `include_type_sources` is set, `type_sources` is populated from the served
closure's registry entries via [`_type_sources`](@ref) (one per type, the
`IL.unparse` text); otherwise it is empty (the description alone suffices for decode).
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

# ── /rosout: the Julia-logging → rcl_interfaces/msg/Log bridge (§13, D7) ──────
# D7: Julia's `@info`/`@warn`/`@error`/`@logmsg` *are* the ROS logging API — no
# `RCLCPP_INFO`-style macros. `RosoutLogger <: AbstractLogger` is node-scoped and,
# per record, (a) writes the ROS console line `[LEVEL] [stamp] [name]: msg` to
# stderr, (b) publishes an `rcl_interfaces/msg/Log` on the node's shared `/rosout`
# publisher, and (c) optionally forwards to a `parent` logger (a file/Julia console
# sink), if one was supplied. The dispatcher wraps every handler in the node logger
# (`_with_node_logger`, called from node.jl/service.jl/action.jl), so a plain `@info`
# inside any handler routes to that node's `/rosout` with no user effort.
#
# Logger name = node FQN; `logger(node, "child")` gives a `.`-separated child. Per-
# logger min levels live in a node-local table (`set_logger_level!`); `--ros-args
# --log-level` will populate it once ros-args parsing lands (§5 TODO). `maxlog` is
# enforced here (it's the logger's responsibility, not the frontend's).
#
# The level mapping follows ROS2's `rcl_interfaces/msg/Log` constants (DEBUG=10,
# INFO=20, WARN=30, ERROR=40, FATAL=50). Julia has no FATAL, so we add one (`Fatal`,
# above `Error`) and map `@logmsg Fatal …` to ROS2 FATAL.

const LogMsg  = Interfaces.rcl_interfaces.msg.Log
const TimeMsg = Interfaces.builtin_interfaces.msg.Time

# A FATAL level above Julia's `Error` (2000) so a Julia program can request ROS2's
# FATAL severity explicitly (`@logmsg Fatal …`); plain `@error` stays at ERROR.
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

An `AbstractLogger` that, per record (§13, D7): writes the ROS console line
`[LEVEL] [stamp] [name]: msg` to stderr (when `console`), publishes an
`rcl_interfaces/msg/Log` on `node`'s shared `/rosout` publisher, and — if a `parent`
logger is supplied — forwards the raw record to it too (an extra file/Julia-console
sink). `name` is the ROS logger name (node FQN by default; `logger(node,"child")`
makes a `.`-separated child). `min_level` is the logger's own floor; per-name
overrides come from the node's level table ([`set_logger_level!`](@ref)).

Default ([`logger`](@ref)) is `console=true, parent=nothing` — a bare node logs in
ROS format. Pass a `parent` (e.g. a `ConsoleLogger`) to *also* get Julia's native
output. If `node` has no live session the `/rosout` publisher can't be declared and
that sink is skipped (console/parent still work), so installing the logger is always
safe.
"""
mutable struct RosoutLogger <: AbstractLogger
    const node::Any                              # the Node whose /rosout we publish to
    const name::String                           # ROS logger name (FQN or "<fqn>.child")
    const parent::Union{AbstractLogger, Nothing} # optional extra sink (file/Julia console)
    const console::Bool                          # write the ROS-format line to stderr
    const min_level::LogLevel
    # The shared `/rosout` publisher (a `PublisherHandle{LogMsg}`); `nothing` for a
    # sessionless node — then the bridge skips the /rosout sink (§13).
    _pub::Any
    # `maxlog` is the logger's responsibility (the frontend doesn't enforce it):
    # per-call-site-id emit counts, mirroring `ConsoleLogger`'s `message_limits`.
    const _maxlog_counts::Dict{Any, Int}
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

# The node-owned `/rosout` publisher, declared once and **shared** by the node logger
# and every `logger(node,"child")` (so children don't each declare a duplicate).
# rmw_zenoh publishes `/rosout` as a transient-local `rcl_interfaces/msg/Log` topic
# with a deep history, so late subscribers (`ros2 topic echo /rosout`) get the
# backlog. A sessionless/closed node can't carry one — return `nothing` and the
# bridge skips the /rosout sink rather than throwing during logger construction.
function _rosout_publisher!(node)
    (node isa Node && isopen(node)) || return nothing
    @lock node.lock begin
        node._rosout_pub === nothing || return node._rosout_pub
        node._rosout_pub = try
            Publisher(node, "/rosout", LogMsg;
                      qos = QosProfile(durability = :transient_local,
                                       reliability = :reliable, depth = 1000))
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

The node's default ROS logger (cached on the node; name = FQN), or a `.`-separated
child logger (`"<fqn>.child"`) sharing the node's `/rosout` publisher and console.
The dispatcher installs `logger(node)` around every handler (D7), so handlers rarely
need this directly; use it for hierarchy or outside a handler via
`with_logger(node) do … end`.
"""
function logger(node::Node)
    # Unlocked fast path: `_logger` is set once. The dispatcher calls this per
    # message (§4), so we must not take the node lock every time — a stale `nothing`
    # read just falls to the locked re-check below (double-checked locking).
    node._logger === nothing || return node._logger
    @lock node.lock begin
        node._logger === nothing || return node._logger
        node._logger = RosoutLogger(node)
        return node._logger
    end
end
logger(node::Node, child::AbstractString) =
    RosoutLogger(node; name = string(node.fqn, ".", child))

# §4 (D7): run a user handler `f` under the node's `RosoutLogger`, so a plain `@info`
# inside it routes to that node's `/rosout` + ROS console. Forward-referenced from
# the dispatch sites in node.jl/service.jl/action.jl (included before this file); a
# non-`Node` owner (shouldn't happen on those paths) just runs `f` unwrapped.
_with_node_logger(f, node::Node) = Logging.with_logger(f, logger(node))
_with_node_logger(f, ::Any)      = f()

# Emit one record to up to three sinks: ROS console (stderr), an optional `parent`
# logger, and the `/rosout` publisher. `maxlog` is gated first (the logger's job).
# Every sink is best-effort and isolated — a failure in one (a bad `show`, a dead
# `/rosout` route) must never break the others or escape `handle_message`. Errors go
# straight to stderr (NOT through logging) to avoid recursion under the active logger.
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

# `maxlog`: emit at most `maxlog` records per call-site `id`. Counts live on the
# logger (`ConsoleLogger` does the same). The frontend does NOT enforce `maxlog`,
# so without this `@info … maxlog=1` would log every time. (Counts race under
# `Parallel(n)` concurrent handlers — benign, like `ConsoleLogger`'s own limits.)
function _maxlog_exceeded!(lg::RosoutLogger, id, kwargs)
    maxlog = get(kwargs, :maxlog, nothing)
    maxlog === nothing && return false
    n = get(lg._maxlog_counts, id, 0)
    n >= maxlog && return true
    lg._maxlog_counts[id] = n + 1
    return false
end

# The node's ROS clock instant as `(sec, nanosec)` for a stamp; `(0,0)` if the clock
# can't be read (a non-`Node` owner / closed node) so the console line never throws.
function _stamp_sec_nanosec(node)
    try
        return _sec_nanosec(Dates.now(node).ns)   # `now(node)` is `Dates.now` extended (time.jl)
    catch
        return (Int32(0), UInt32(0))
    end
end

# Write the ROS-format console line: `[LEVEL] [secs.nanosec] [logger_name]: msg`.
# Built as one string and printed in a single `print` so concurrent records don't
# interleave mid-line. Uses the logger's *own* `name`, so a child logger's records
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
# `@warn "x" a=1 b=2` carries the extras as kwargs; we append them as `(a=1, b=2)`
# so the structure survives onto a flat ROS string. An `:exception` value renders
# as its message (the backtrace would bloat the line). Must never throw — a bad
# `show`/`string` on a user value can't be allowed to break logging.
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

# Build and publish the `rcl_interfaces/msg/Log` for one record. The stamp is the
# node's ROS clock (§7), the level the bucketed severity, `name` the node FQN, and
# `function` is unavailable from `handle_message`'s arguments (Julia carries no
# enclosing-function name there) so it's left empty — matching what rmw_zenoh emits
# when the producing function is unknown.
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

# The effective min level for this logger: the longest-prefix match in the node's
# level table (§7, set by `set_logger_level!` / future `--ros-args --log-level`),
# else the logger's own `min_level`. Hierarchy: `/ns/n.child` inherits `/ns/n`'s
# level unless it has its own entry.
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

# `AbstractLogger` interface. Gate on the effective level; when a `parent` sink is
# present also defer to its `shouldlog`/`min_enabled_level` so its per-module
# settings still apply (a `nothing` parent → console+/rosout only, gate on level).
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
    set_logger_level!(node, level)            # sets the node's own logger (FQN)
    set_logger_level!(node, name, level)      # sets a specific logger name

Set the per-logger minimum level (§7, D7). `name` matches the logger and any
`.`-separated descendant lacking its own entry (`set_logger_level!(node, "/n", Warn)`
also gates `"/n.child"`). The programmatic form of `--ros-args --log-level name:=…`,
which will populate the same table once ros-args parsing lands (§5).
"""
set_logger_level!(node::Node, name::AbstractString, level::LogLevel) =
    (node._log_levels[String(name)] = level.level; nothing)
set_logger_level!(node::Node, level::LogLevel) =
    set_logger_level!(node, node.fqn, level)

# Node forms of the standard scope/global installers (D7): `with_logger(node) do … end`
# and `global_logger(node)` use the node's default `RosoutLogger`.
Logging.with_logger(f::Function, node::Node) = Logging.with_logger(f, logger(node))
Logging.global_logger(node::Node) = Logging.global_logger(logger(node))

"""
    with_rosout(f, node; kwargs...)

Run `f()` with a fresh [`RosoutLogger`](@ref) (built with `kwargs`) installed as the
active logger (§13). `with_logger(node) do … end` is the usual form (it reuses the
node's cached logger); `with_rosout` is kept for explicitly overriding logger
options (`parent=`, `console=`, `min_level=`) for a scope.

```julia
with_rosout(node; parent = ConsoleLogger()) do
    @info "armed"          # → ROS console, /rosout, AND Julia console
end
```
"""
with_rosout(f, node; kwargs...) =
    Logging.with_logger(f, RosoutLogger(node; kwargs...))

# ── Zenoh transport logs → /rosout (§8 / D7, opt-in) ──────────────────────────
# Zenoh's Rust core emits its own logs (session, transport, liveliness). Zenoh.jl
# can capture them into a bounded, pull-based `LogStream` (`open_log_stream`); we
# drain it and re-emit each record through Julia logging under a `<fqn>.zenoh` child
# logger, so it rides the same path to `/rosout` + ROS console + the level table.
#
# Opt-in, off by default: nothing claims Zenoh's process-global (one-shot) logger
# unless this is called. Feedback-loop discipline (Zenoh docs/logging.md): the
# `WARN` floor is enforced *inside* Zenoh, so a `/rosout` publish's own data-plane
# DEBUG/TRACE logs never enter the stream and steady-state forwarding can't feed
# itself; the bounded drop-oldest ring is the backstop. Keep the floor at WARN.

_julia_log_level(s) =
    s == Zenoh.LogSeverities.ERROR ? Logging.Error :
    s == Zenoh.LogSeverities.WARN  ? Logging.Warn  :
    s == Zenoh.LogSeverities.INFO  ? Logging.Info  : Logging.Debug

"""
    bridge_zenoh_logs!(node; min_severity=Zenoh.LogSeverities.WARN, capacity=256)

Capture Zenoh's transport logs (at `min_severity`+) and re-emit them on `node`'s
`/rosout` under a `<fqn>.zenoh` child logger (§8, D7). Opt-in; spawns a drain task
that the node's Context drain closes (§14.1). Returns the `Zenoh.LogStream`.

Zenoh's logger is **process-global and one-shot** — call this at most once per
process, and not together with `Zenoh.setup_logging`. Keep `min_severity` at `WARN`
(the default): it's the primary defense against a log→publish→log feedback loop.
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

# `_ctx` (a Context from a Node-or-Context) is context.jl's, duck-typed on
# `.context`; a bare `Context` returns itself. `registry`/`lookup_type` are
# context.jl's. `now`/`to_msg` are time.jl's. All in module scope here.
