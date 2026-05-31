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
# publishes `rcl_interfaces/msg/Log`. The type-source side (`include_type_sources`)
# is the only piece still staged (`describe_type` returns the description; sources
# need IL.unparse).

import Logging
using Logging: AbstractLogger, LogLevel
# `TypeInfo` is in scope (core.jl re-exports it); the RIHS-string parser is not.
using ROSZenoh: TypeInfo, type_hash_from_rihs_string

export RosoutLogger, with_rosout, Fatal, get_type_description, fetch_type_description,
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

"""
    wire_get_type_description!(node) -> ServiceHandle

Declare the hidden `~/get_type_description` service on `node` (§13): a
`type_description_interfaces/srv/GetTypeDescription` server whose handler answers
from the Context type registry (§11) via [`describe_type`](@ref), converting the
canonical internal `TypeDescriptionMsg` to the wire form ([`to_wire_td`](@ref)). A
request for an unknown `(name, hash)` is a clean `successful=false` reply. Every
node serves this so peers can fetch the descriptions it advertises. The handle is
tracked on `node` (closed with it) like any other entity.

Sources (`type_sources`) are not yet populated (the description suffices for
decode); `include_type_sources` is accepted but currently returns an empty list.
"""
function wire_get_type_description!(node)
    return Service(node, "~/get_type_description", GetTypeDescription_Request) do req
        tdmsg = describe_type(node, req.type_name, req.type_hash)
        tdmsg === nothing && return _gtd_not_found(req.type_name)
        return GetTypeDescription_Response(
            successful = true, failure_reason = "",
            type_description = to_wire_td(tdmsg),
            type_sources = WireTypeSource[],          # TODO: IL.unparse sources when requested
            extra_information = WireKeyValue[])
    end
end

# ── /rosout: the Julia-logging → rcl_interfaces/msg/Log bridge (§13) ──────────
# rmw_zenoh mirrors a node's log output onto the `/rosout` topic as
# `rcl_interfaces/msg/Log`. We bridge Julia's logging stack: `RosoutLogger` is an
# `AbstractLogger` that *tees* — it forwards every record to a parent logger (so
# the console/file output a Julia program expects is unchanged) and *also* emits a
# `Log` message on a node-owned `/rosout` publisher. `with_rosout(node) do … end`
# installs it for a scope via `Logging.with_logger`.
#
# The level mapping follows ROS2's `rcl_interfaces/msg/Log` constants (DEBUG=10,
# INFO=20, WARN=30, ERROR=40, FATAL=50), which mirror Python's logging levels.
# Julia has no FATAL, so we add one (`Fatal`, above `Error`) and map `@logmsg
# Fatal …` to ROS2 FATAL.

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

"""
    RosoutLogger(node; parent=current_logger(), min_level=Logging.Debug)

An `AbstractLogger` that tees Julia log records to `parent` *and* to `node`'s
`/rosout` publisher as `rcl_interfaces/msg/Log` (§13). Install it for a scope with
[`with_rosout`](@ref) (or `Logging.with_logger`). `min_level` gates what reaches
both sinks; `shouldlog`/`catch_exceptions` defer to `parent` so per-module log
settings still apply.

If `node` has no live session/context the `/rosout` publisher can't be declared;
the logger then degrades to a pure tee to `parent` (so installing it is always
safe). See [`with_rosout`](@ref).
"""
mutable struct RosoutLogger <: AbstractLogger
    const node::Any                  # the Node whose /rosout we publish to
    const parent::AbstractLogger     # forwarded-to logger (console/file)
    const min_level::LogLevel
    # The `/rosout` publisher (a `PublisherHandle{LogMsg}`); `nothing` for a
    # sessionless node — then the logger is a pure tee (§13).
    _pub::Any
end

function RosoutLogger(node; parent::AbstractLogger=Logging.current_logger(),
                      min_level::LogLevel=Logging.Debug)
    lg = RosoutLogger(node, parent, min_level, nothing)
    lg._pub = _rosout_publisher!(node)
    return lg
end

# Declare the node-owned `/rosout` publisher. rmw_zenoh publishes `/rosout` as a
# `rcl_interfaces/msg/Log` topic, transient-local with a deep history so late
# subscribers (e.g. `ros2 topic echo /rosout`) get the backlog. A sessionless or
# closed node can't carry a publisher — return `nothing` so the bridge degrades to
# a pure tee (parent only) instead of throwing during logger construction.
function _rosout_publisher!(node)
    (node isa Node && isopen(node)) || return nothing
    try
        return Publisher(node, "/rosout", LogMsg;
                         qos = QosProfile(durability = :transient_local,
                                          reliability = :reliable, depth = 1000))
    catch err
        @debug "rosout bridge: /rosout publisher declaration failed" exception=(err, catch_backtrace())
        return nothing
    end
end

# Tee one record: forward to the parent logger, then (if a `/rosout` publisher is
# live) emit the `Log` message. A failure on the ROS side must never break the
# program's own logging, so the emit is best-effort and logged-through-parent only
# on error — it never escapes `handle_message`.
function Logging.handle_message(lg::RosoutLogger, level, message, _module, group,
                                id, file, line; kwargs...)
    # Parent first: the program's expected output is never lost to a ROS hiccup.
    Logging.handle_message(lg.parent, level, message, _module, group, id, file,
                           line; kwargs...)
    if lg._pub !== nothing
        try
            _emit_rosout(lg, level, message, file, line; kwargs...)
        catch err
            @debug "rosout bridge: publish failed" exception=(err, catch_backtrace())
        end
    end
    nothing
end

# The node FQN is the ROS logger name (`/ns/node`), matching what rmw_zenoh stamps
# on `Log.name` — not the Julia `_module`, which carries no ROS identity.
_logger_name(lg::RosoutLogger) = lg.node isa Node ? lg.node.fqn : string(lg.node)

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
                 name = _logger_name(lg),
                 msg = _render_message(message, kwargs),
                 file = string(file),
                 var"function" = "",
                 line = UInt32(line === nothing ? 0 : line))
    publish(lg._pub, log)
    return nothing
end

# `AbstractLogger` interface: gate on `min_level` and defer the rest to `parent`,
# so a `RosoutLogger` is a transparent tee — per-module `shouldlog` settings and
# exception handling behave exactly as the wrapped logger.
Logging.shouldlog(lg::RosoutLogger, level, _module, group, id) =
    level >= lg.min_level && Logging.shouldlog(lg.parent, level, _module, group, id)

Logging.min_enabled_level(lg::RosoutLogger) =
    min(lg.min_level, Logging.min_enabled_level(lg.parent))

Logging.catch_exceptions(lg::RosoutLogger) = Logging.catch_exceptions(lg.parent)

"""
    with_rosout(f, node; parent=current_logger(), min_level=Logging.Debug)

Run `f()` with a [`RosoutLogger`](@ref) installed as the active logger (§13): every
`@info`/`@warn`/… inside the scope is forwarded to `parent` *and* mirrored to
`node`'s `/rosout` topic. Restores the previous logger on exit (it's
`Logging.with_logger` under the hood). Use it to wrap a node's main loop so its
logging surfaces on ROS tooling.

```julia
with_rosout(node) do
    @info "armed"          # → console AND /rosout
end
```
"""
function with_rosout(f, node; parent::AbstractLogger=Logging.current_logger(),
                     min_level::LogLevel=Logging.Debug)
    Logging.with_logger(f, RosoutLogger(node; parent=parent, min_level=min_level))
end

# `_ctx` (a Context from a Node-or-Context) is context.jl's, duck-typed on
# `.context`; a bare `Context` returns itself. `registry`/`lookup_type` are
# context.jl's. `now`/`to_msg` are time.jl's. All in module scope here.
