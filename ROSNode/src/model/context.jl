# §5 Contexts — the process-level container. One Context = one Zenoh session;
# N nodes share it (hiroz's `ZContext`). It owns: the session (+ shared `z_id`),
# an atomic entity-id counter, the domain id, default namespace/enclave, the
# keyexpr formatter, the type registry (§11), the discovery index (§12) fed by a
# single `@ros2_lv/**` liveliness subscriber, a clock (§7), the `on_shutdown`
# hooks, and the shutdown state machine + drain (§14).
#
# Lower layers not yet landed are stubbed against what exists with `# TODO(layer):`
# markers: the Node type (§6) — name resolution is duck-typed against a node's
# `namespace`/`name`/`fqn`; the per-Context `/clock` sim source (§7); local-entity
# injection into the graph (§6/§12) — the index ingests liveliness today and
# exposes the injection seam for the entity layer to call.

using Zenoh: Zenoh, Config, Session, Keyexpr, zid, to_le_bytes, LivelinessSubscriber,
             LivelinessSubscriberHandler
import Zenoh
using ROSZenoh: ROSZenoh, ZenohId, RmwZenoh, KeyExprFormat, NodeEntity,
                EndpointEntity, EndpointKind, parse_liveliness

export Context, Node, # `Node` forward-declared here; the §6 layer specializes it
       resolve_name, on_shutdown, request_shutdown, is_shutdown,
       spin, session, next_entity_id!, registry, graph,
       EndpointInfo, NodeInfo, GraphIndex

# ── discovery index (§12) ────────────────────────────────────────────────
# One record per endpoint, parsed from a liveliness token (or injected for our
# own, §6). Carries the RIHS01 hash and gid (not just a type-name string) so a
# caller can run its own type/QoS compatibility check (§12.2) with the same data
# the auto-detectors use.

"""
    EndpointInfo

A single discovered (or locally-injected) endpoint, the atom of the [discovery graph](https://docs.ros.org/en/rolling/Concepts/Basic/About-Discovery.html)
(§12). `type === nothing` for an `EMPTY_TOPIC_TYPE` token; `is_local`
distinguishes our own injected entities (authoritative) from
liveliness-discovered remotes (eventually consistent).
"""
struct EndpointInfo
    node_name::String
    namespace::String
    kind::EndpointKind
    topic::String                        # resolved FQN
    type::Union{TypeInfo, Nothing}       # name + RIHS01 hash
    qos::QosProfile
    gid::NTuple{16, UInt8}
    is_local::Bool
end

"""
    NodeInfo(name, namespace, enclave)

Identity of a discovered node — what `node_names` returns (§12.1).
"""
struct NodeInfo
    name::String
    namespace::String
    enclave::String
end

# Build an `EndpointInfo` from a parsed `EndpointEntity`. The node fields come
# from the entity's node when present (rmw_zenoh carries it; ros2dds yields
# `node === nothing`, leaving them blank — the bridge-compat gap).
function _endpoint_info(e::EndpointEntity; is_local::Bool)
    nn = e.node === nothing ? "" : e.node.name
    ns = e.node === nothing ? "" : e.node.namespace
    # gid needs the node identity; without it we can't derive it, so zero-fill.
    gid = e.node === nothing ? ntuple(_ -> 0x00, 16) : ROSZenoh.entity_gid(e)
    EndpointInfo(nn, ns, e.kind, e.topic, e.type_info, e.qos, gid, is_local)
end

"""
    GraphIndex

The Context's discovery index: the observed `EndpointInfo` set keyed by
liveliness keyexpr (the token uniquely identifies an endpoint), guarded by a
lock, plus a `Condition` the graph waits (`wait_for_service`, `on_graph_change`,
§12) park on. The single `@ros2_lv/**` liveliness subscriber drives it; our own
entities are injected directly (§6).

This is the one change stream every §12 detector/wait is a view over.
"""
mutable struct GraphIndex
    lock::ReentrantLock
    # keyexpr → EndpointInfo. The liveliness token string is the natural key:
    # PUT inserts/updates, DELETE removes by the same key.
    endpoints::Dict{String, EndpointInfo}
    # Bumped on every change; waiters re-check the graph predicate when notified.
    changed::Threads.Condition
    # User `on_graph_change` listeners (added/removed EndpointInfo) — fired
    # outside the index lock. Stored as plain functions of one `NamedTuple`.
    listeners::Vector{Any}
end

GraphIndex() = GraphIndex(ReentrantLock(), Dict{String, EndpointInfo}(),
                          Threads.Condition(), Any[])

# ── type registry (§11) ──────────────────────────────────────────────────
# Keyed by (name, RIHS01-hash) so evolved versions of a type coexist. The value
# is left `Any` (the §11 registry entry: IL + generated module + TypeDescription)
# — that layer owns the entry shape; the Context only owns the table + lock so
# all nodes share discovered types.

"""
    TypeRegistry

Per-Context type table keyed by `(type_name, RIHS01-hash)` so versions coexist
(§11). The Context owns the storage + lock; the typesupport layer populates and
reads entries. Lookups normalize the key to `(String, TypeHash)`.
"""
mutable struct TypeRegistry
    lock::ReentrantLock
    entries::Dict{Tuple{String, ROSZenoh.TypeHash}, Any}
end

TypeRegistry() = TypeRegistry(ReentrantLock(), Dict{Tuple{String, ROSZenoh.TypeHash}, Any}())

"""
    register_type!(reg, info::TypeInfo, entry)

Store `entry` (the §11 IL/codegen/TypeDescription record — opaque here) under
`(info.name, info.hash)`. Returns `entry`.
"""
function register_type!(reg::TypeRegistry, info::TypeInfo, entry)
    @lock reg.lock reg.entries[(info.name, info.hash)] = entry
    entry
end

"""
    lookup_type(reg, info::TypeInfo) -> Union{Any, Nothing}

Fetch the registry entry for `(info.name, info.hash)`, or `nothing` if the exact
name+version isn't registered (the §11 dynamic-discovery trigger).
"""
lookup_type(reg::TypeRegistry, info::TypeInfo) =
    @lock reg.lock get(reg.entries, (info.name, info.hash), nothing)

# ── shutdown state (§14) ──────────────────────────────────────────────────
# `:running → :shutting_down → :shutdown`. The transition is the one drain path
# every trigger funnels through (close / request_shutdown / signal).

@enum ShutdownState running shutting_down shutdown_done

# ── the Context ───────────────────────────────────────────────────────────

"""
    Context(; domain_id, namespace, enclave, config, format, drain_timeout, …)
    Context(f; kwargs...)  do ctx … end

The process-level container (§5), the [ROS 2 node](https://docs.ros.org/en/rolling/Concepts/Basic/About-Nodes.html)
group root. Opens one Zenoh `Session` and holds everything nodes share: the
session and its `z_id`, the atomic entity-id counter, the `domain_id`, default
`namespace`/`enclave`, the keyexpr formatter, the type registry (§11), the
discovery index + its `@ros2_lv/**` liveliness subscriber (§12), a clock (§7),
the `on_shutdown` hooks, and the shutdown state machine (§14). `Context` is the
RAII/shutdown root: `close(ctx)` runs the drain.

The do-block form opens, runs `f(ctx)`, and `close`s on exit (the
`rclcpp::init`/`shutdown` bracket).
"""
mutable struct Context
    const session::Session
    const z_id::ZenohId                  # the session id in ROSZenoh's hex form
    const domain_id::Int
    const namespace::String              # default node namespace
    const enclave::String
    const format::KeyExprFormat
    @atomic _next_id::Int                # atomic entity-id allocator (§5)
    const registry::TypeRegistry
    const graph::GraphIndex
    # The `@ros2_lv/**` liveliness subscriber (channel form) feeding `graph`, drained
    # by a Julia consumer task (so no foreign thread runs Julia — see `_start_discovery!`).
    # `Any` because Zenoh's handler type isn't in scope as a field constraint here and
    # `close` is duck-typed.
    _lv_sub::Any
    # Clock handles by source — `clock(ctx, C())` returns/creates one. The §7
    # `Clock` is duck-typed against `node.clocks`; the Context fills that role.
    const clocks::Dict{DataType, Any}
    # Sim-time source (§7): the Context-hosted `/clock` value, atomically held.
    # `nothing` until any node sets `use_sim_time` and the sub lands.
    @atomic sim_time_ns::Union{Int64, Nothing}
    # §7 sim-time wiring. `_sim_users` is the set of nodes that opted into
    # `use_sim_time` (sim active iff non-empty — ONE process-level `/clock` source,
    # per-node opt-in). `_clock_node`/`_clock_sub` are the hidden internal node + its
    # `/clock` Subscription carrying that source. All guarded by `_sim_lock` (a
    # distinct lock — NOT `_state_lock`).
    _clock_node::Any
    _clock_sub::Any
    const _sim_users::Set{Any}
    const _sim_lock::ReentrantLock
    # Shutdown state + drain machinery (§14).
    const _state_lock::ReentrantLock
    @atomic _state::ShutdownState
    const _on_shutdown::Vector{Any}      # user cleanup hooks, run during the drain
    const _shutdown_done::Threads.Condition  # `spin`/`wait` park here until drained
    # Notified `all=true` at drain start so blocked clock-waits
    # (`_interruptible_sleep`, Rate, sleep_until) re-check `is_shutdown` and raise
    # ShutdownException. The terminal counterpart is `_shutdown_done`, fired at drain end.
    const _shutdown_wake::Threads.Condition
    const drain_timeout::Float64         # seconds to await in-flight work
    # Registered close-able handles (nodes, entities, timers) undeclared on drain
    # (step 4). The entity layer pushes here; `close(ctx)` walks it in reverse.
    const _resources::Vector{Any}
    # Dynamic-resolution lens: the module whose baked `__ros_resolve__` table this
    # Context looks through, so every node/sub on it sees one consistent, deterministic
    # picture. `nothing` ⇒ content-canonical resolution only.
    const home::Union{Module, Nothing}
end

# Render the session's `z_id_t` into ROSZenoh's canonical lowercase-hex form.
# zenoh's own `z_id_to_string` reverses the LE bytes (MSB first) and elides
# leading zero bytes; we reproduce that so the hex matches what rmw_zenoh writes
# into liveliness tokens (and what `parse_liveliness` reads back).
function _zid_hex(s::Session)
    le = to_le_bytes(zid(s))                       # NTuple{16,UInt8}, little-endian
    be = reverse(collect(le))
    i = findfirst(!=(0x00), be)                    # strip leading zero bytes
    i === nothing && return "0"                    # all-zero ⇒ invalid session
    bytes2hex(@view be[i:end])
end

# ROS env → values, with explicit kwargs winning. Kept tiny: domain/namespace/
# enclave here; the localhost/peers→Zenoh-config translation is below.
_env_domain_id() = (v = get(ENV, "ROS_DOMAIN_ID", nothing); v === nothing ? 0 : parse(Int, v))
_env_namespace() = get(ENV, "ROS_NAMESPACE", "")
_env_enclave()   = get(ENV, "ROS_ENCLAVE", "")

# Translate the ROS transport-shaping env vars into the Zenoh `Config` via its
# `c[key]=value` JSON5 setter (§5). Only the unambiguous mappings: localhost-only
# → scouting off + loopback connect; static peers → connect/endpoints. `domain_id`
# deliberately does *not* go here — it lives in the keyexprs, not the transport.
# TODO(layer): `ROS_AUTOMATIC_DISCOVERY_RANGE` → gossip/multicast scope once the
# exact rmw_zenoh scouting mapping is pinned down.
function _apply_ros_transport_config!(c::Config; localhost_only::Bool, peers::Vector{String})
    if localhost_only
        c["scouting/multicast/enabled"] = false
        isempty(peers) && (peers = ["tcp/localhost:7447"])
    end
    isempty(peers) || (c["connect/endpoints"] = peers)
    c
end

function Context(; domain_id::Union{Integer, Nothing}=nothing,
                   namespace::Union{AbstractString, Nothing}=nothing,
                   enclave::Union{AbstractString, Nothing}=nothing,
                   config::Union{Config, Nothing}=nothing,
                   format::KeyExprFormat=RmwZenoh(),
                   localhost_only::Bool=false,
                   peers::AbstractVector{<:AbstractString}=String[],
                   drain_timeout::Real=5.0,
                   shm_clients=nothing,
                   home::Union{Module, Nothing}=nothing)
    dom = domain_id === nothing ? _env_domain_id() : Int(domain_id)
    ns  = _normalize_namespace(namespace === nothing ? _env_namespace() : String(namespace))
    enc = enclave === nothing ? _env_enclave() : String(enclave)

    # Zenoh session config: reuse the caller's `Config` wholesale, else a default
    # (peer mode → local router). ROS transport env is layered in either way.
    cfg = config === nothing ? Config() : config
    _apply_ros_transport_config!(cfg; localhost_only=localhost_only,
                                       peers=collect(String, peers))

    sess = shm_clients === nothing ? Base.open(cfg) : Base.open(cfg; shm_clients=shm_clients)

    ctx = Context(sess, ZenohId(_zid_hex(sess)), dom, ns, enc, format,
                  0, TypeRegistry(), GraphIndex(), nothing,
                  Dict{DataType, Any}(), nothing,
                  nothing, nothing, Set{Any}(), ReentrantLock(),
                  ReentrantLock(), running, Any[], Threads.Condition(),
                  Threads.Condition(), Float64(drain_timeout), Any[], home)

    # Register the statically-compiled bootstrap types (type_description_interfaces,
    # §11 S1) so the §13 server can serve them and discovery can use them, plus any
    # @ros_import/@ros_cache statically-generated types (so keyexpr-only resolution
    # uses the precompiled type directly). Forward references into typesupport/
    # wellknown (included after this file).
    _register_canonical_types!(ctx)
    _register_static_types!(ctx)
    home === nothing && _maybe_hint_no_home()    # nudge if a resolvable home exists

    _start_discovery!(ctx)
    return ctx
end

function Context(f::Function; kwargs...)
    ctx = Context(; kwargs...)
    try
        f(ctx)
    finally
        close(ctx)
    end
end

"""
    @context([kwargs...]) do ctx … end

Create a [`Context`](@ref) whose dynamic-resolution `home` is the calling module — sugar
for `Context(f; home=@__MODULE__, kwargs...)`. Every keyexpr-only subscription
on the Context then resolves wire types to this module's `@ros_import` structs, giving
one consistent picture across the session. Use `Context(...; home=Mod)` for a different
lens, or a plain `Context()` for content-canonical resolution only.
"""
macro context(args...)
    # Julia hands a trailing do-block to a macro as its FIRST argument (an `->` lambda);
    # any `kwargs` written inside the parens follow it. So the body is `args[1]`, not `args[end]`.
    (!isempty(args) && args[1] isa Expr && args[1].head === :->) ||
        error("@context needs a trailing do-block: `@context(kwargs…) do ctx … end`")
    body = esc(args[1])
    kws  = map(esc, args[2:end])
    m = __module__
    return :(Context($body; home=$m, $(kws...)))
end

export @context

Base.show(io::IO, ctx::Context) =
    print(io, "Context(domain=", ctx.domain_id, ", z_id=", ctx.z_id,
              ", state=", (@atomic ctx._state), ")")

# ── accessors others need (§5) ─────────────────────────────────────────────

"The Context's Zenoh `Session` (shared by all its nodes)."
session(ctx::Context) = ctx.session

"The Context's type registry (§11)."
registry(ctx::Context) = ctx.registry

"The Context's discovery index (§12)."
graph(ctx::Context) = ctx.graph

"""
    next_entity_id!(ctx) -> Int

Allocate the next node/endpoint id from the Context's atomic counter (§5). Nodes
*and* endpoints draw from one counter — they populate `ROSZenoh` entity `id`
fields, which must be unique within the session.
"""
next_entity_id!(ctx::Context) = (@atomic ctx._next_id += 1)

# The Context fills the §7 clock's `node`-role for context-level reads; the time
# layer reads `node.clocks` duck-typed, so a Context works wherever a node would.
function clock(ctx::Context, src::ClockSource)
    C = typeof(src)
    @lock ctx._state_lock get!(ctx.clocks, C) do
        Clock(ctx, src)
    end::Clock{C}
end
clock(ctx::Context) = clock(ctx, ROS())

# ── §7 sim-time: use_sim_time → /clock routing ──────────────────────────────────
# ONE process-level `/clock` data source (a Subscription on a hidden internal node),
# per-node opt-in: `_sim_users` holds the nodes whose ROS clock follows sim time, so
# `now(node, ROS())` reads `sim_time_ns` only for an opted-in node (time.jl). Jump
# callbacks fire on the clock each node actually holds (`node.clocks[ROS]`): a per-node
# `sim_activated`/`sim_deactivated` on opt-in/opt-out, and a `time_forward`/`time_backward`
# to every active node on a `/clock` discontinuity. Follow-only (no interpolation): `now`
# holds between samples (rclcpp TimeSource); ROS `Timer`/`Rate` are NOT sim-driven here.
#
# Activation declares a real `/clock` Subscription, which needs a `NodeEntity`
# (`topic_keyexpr` requires one) — hence a hidden internal node, not a bare Context. It
# runs synchronously (under the param-commit `s.lock` via the event hook): toggling is
# rare/one-time, and nothing takes a param lock while holding `_sim_lock`/node locks, so
# there is no cycle. (A jump callback that re-enters parameter mutation is safe only
# because `s.lock` is a `ReentrantLock` — same-thread re-entry succeeds; a non-reentrant
# lock here would reintroduce a self-deadlock.)

_is_sim_user(ctx::Context, node) = @lock ctx._sim_lock (node in ctx._sim_users)

"""
    set_use_sim_time!(ctx, node, on::Bool) -> nothing

(De)activate sim-time routing for `node` (§7). The first opt-in declares the Context's
`/clock` Subscription; the last opt-out tears it down. Fires a per-node
`sim_activated`/`sim_deactivated` jump on the node's ROS clock.
"""
function set_use_sim_time!(ctx::Context, node, on::Bool)
    transition = :none
    @lock ctx._sim_lock begin
        if on
            if !(node in ctx._sim_users)
                was_empty = isempty(ctx._sim_users)
                push!(ctx._sim_users, node)
                transition = was_empty ? :activate_first : :join
            end
        elseif node in ctx._sim_users
            delete!(ctx._sim_users, node)
            transition = isempty(ctx._sim_users) ? :deactivate_last : :leave
        end
    end
    if transition === :activate_first
        _activate_sim!(ctx)
        _fire_jumps_to!(node, TimeJump(sim_activated, _activate_delta(ctx)))
    elseif transition === :join
        _fire_jumps_to!(node, TimeJump(sim_activated, _activate_delta(ctx)))
    elseif transition === :leave
        _fire_jumps_to!(node, TimeJump(sim_deactivated, Duration(0)))
    elseif transition === :deactivate_last
        _fire_jumps_to!(node, TimeJump(sim_deactivated, Duration(0)))
        _deactivate_sim!(ctx)
    end
    nothing
end

# Activation delta for a node joining an already-running sim: the actual sim−system
# offset (so threshold callbacks fire too); 0 for the first opt-in (no `/clock` sample
# yet — `on_clock_change` still triggers it).
_activate_delta(ctx::Context) =
    (s = @atomic ctx.sim_time_ns; s === nothing ? Duration(0) : Duration(s - _read_ns(System())))

# Declare the `/clock` Subscription on the hidden internal node (created once, reused
# across reactivations). Each sample's `builtin_interfaces/Time` is ingested as ns.
function _activate_sim!(ctx::Context)
    # Build into locals; commit BOTH to the Context only after the Subscription
    # succeeds — so a throwing `Subscription` ctor can't strand `_clock_node` without a
    # `/clock` sub (which would make every later reactivation a no-op).
    node = ctx._clock_node === nothing ?
        Node(ctx, "_ros_clock"; serve_type_description = false) : ctx._clock_node
    ClockMsg = Interfaces.rosgraph_msgs.msg.Clock
    sub = Subscription(node, "/clock", ClockMsg) do msg
        _ingest_clock!(ctx, _join_ns(msg.clock.sec, msg.clock.nanosec))
    end
    @lock ctx._sim_lock begin
        ctx._clock_node = node
        ctx._clock_sub = sub
    end
    nothing
end

# Tear down the `/clock` source: close the sub and clear sim time. A late in-flight
# sample is ignored by `_ingest_clock!` (it re-checks `_sim_users` under the lock).
function _deactivate_sim!(ctx::Context)
    sub = @lock ctx._sim_lock begin
        s = ctx._clock_sub
        ctx._clock_sub = nothing
        @atomic ctx.sim_time_ns = nothing
        s
    end
    sub === nothing && return nothing
    try
        close(sub)
    catch err
        @error "sim-time: closing /clock subscription failed" exception=(err, catch_backtrace())
    end
    nothing
end

# Ingest one `/clock` sample. Resurrection-safe: a sample arriving after deactivation
# (empty `_sim_users`) is ignored. The first post-activation sample only sets the
# baseline (the per-node `sim_activated` already fired at opt-in); a later change fires a
# `time_forward`/`time_backward` to every active node.
function _ingest_clock!(ctx::Context, ns::Int64)
    local jump
    @lock ctx._sim_lock begin
        if isempty(ctx._sim_users)
            jump = nothing
        else
            old = @atomic ctx.sim_time_ns
            @atomic ctx.sim_time_ns = ns
            jump = (old === nothing || ns == old) ? nothing :
                   TimeJump(ns > old ? time_forward : time_backward, Duration(ns - old))
        end
    end
    jump === nothing || _fire_clock_jumps!(ctx, jump)
    nothing
end

# Fire a jump to the callbacks on one node's ROS clock (the handle the user holds,
# `node.clocks[ROS]`), gated by `_triggers`. A snapshot copy decouples firing from a
# concurrent `register!`.
function _fire_jumps_to!(node, j::TimeJump)
    node === nothing && return nothing
    node isa Node && !isopen(node) && return nothing   # skip a closed/closing node (no fire on a torn-down clock)
    (hasproperty(node, :clocks) && haskey(node.clocks, ROS)) || return nothing
    c = node.clocks[ROS]::Clock{ROS}
    for cb in copy(c.jumps)
        _triggers(cb, j) || continue
        try
            cb.f(j)
        catch err
            @error "sim-time jump callback threw" exception=(err, catch_backtrace())
        end
    end
    nothing
end

# A `/clock` discontinuity affects every opted-in node — fan the jump to all of them.
function _fire_clock_jumps!(ctx::Context, j::TimeJump)
    nodes = @lock ctx._sim_lock collect(ctx._sim_users)
    for n in nodes
        _fire_jumps_to!(n, j)
    end
    nothing
end

# ── name resolution (§5) ────────────────────────────────────────────────────
# Turn a user name into a fully-qualified name *before* it reaches ROSZenoh:
# absolute `/foo` as-is; relative `foo` → prepend namespace; private `~/foo` →
# prepend the node FQN; then validate. Remap is a per-context rule table (the
# `from:=to` / `__ns:=` / `__node:=` forms) — applied last; today the table is
# empty until `--ros-args` parsing lands (TODO below). ROSZenoh stays
# naming-agnostic — it takes the final topic string.

# Normalize a namespace to a leading-slash, no-trailing-slash form. Empty ⇒ "/".
function _normalize_namespace(ns::AbstractString)
    isempty(ns) && return "/"
    s = startswith(ns, "/") ? String(ns) : "/" * String(ns)
    (length(s) > 1 && endswith(s, "/")) ? rstrip(s, '/') : s
end

# Join a namespace and a relative name without doubling the slash.
_ns_join(ns::AbstractString, name::AbstractString) =
    ns == "/" ? "/" * name : ns * "/" * name

# A node's FQN = namespace + "/" + name. Duck-typed: the §6 Node has
# `namespace`/`name`; a bare Context resolves relative names against its default
# namespace with no private (`~`) support.
_resolver_namespace(node) = hasproperty(node, :namespace) ? node.namespace : "/"
function _resolver_fqn(node)
    hasproperty(node, :fqn) && return node.fqn
    if hasproperty(node, :namespace) && hasproperty(node, :name)
        return _ns_join(_normalize_namespace(node.namespace), node.name)
    end
    return ""    # Context-level: no node FQN, so `~` is unresolvable
end

# Charset/structure validation of a resolved FQN: leading slash, no `//`, no
# trailing slash (except the bare root), tokens are `[A-Za-z_][A-Za-z0-9_]*`.
function _validate_fqn(name::AbstractString)
    startswith(name, "/") || throw(ArgumentError("resolved name must be absolute: $(repr(name))"))
    name == "/" && return name
    occursin("//", name) && throw(ArgumentError("name contains empty token (//): $(repr(name))"))
    endswith(name, "/") && throw(ArgumentError("name has trailing slash: $(repr(name))"))
    for tok in split(name[2:end], "/")
        (!isempty(tok) && (isletter(tok[1]) || tok[1] == '_') &&
         all(c -> isletter(c) || isdigit(c) || c == '_', tok)) ||
            throw(ArgumentError("invalid name token $(repr(tok)) in $(repr(name))"))
    end
    name
end

"""
    resolve_name(node_or_ctx, name; kind=:topic) -> String

Resolve a user-supplied `name` to a fully-qualified name (§5), the form ROSZenoh
keyexpr builders expect:

- **absolute** `"/foo"` — used as-is (after remap).
- **relative** `"foo"` — prepended with the node's namespace.
- **private** `"~/foo"` — prepended with the node's FQN (`namespace`/`name`).

Then remap rules apply and the result is charset/structure-validated. `kind`
(`:topic`/`:service`/`:node`) is carried for kind-scoped remap rules; it does not
change the algorithm today. Resolving a private name against a bare `Context`
(no node FQN) is an error.
"""
function resolve_name(node, name::AbstractString; kind::Symbol=:topic)
    isempty(name) && throw(ArgumentError("name may not be empty"))
    resolved = if startswith(name, "~")
        # private: `~/foo` (or bare `~`) → node FQN + remainder
        fqn = _resolver_fqn(node)
        isempty(fqn) && throw(ArgumentError("private name $(repr(name)) needs a node FQN"))
        rest = startswith(name, "~/") ? name[3:end] : name[2:end]
        isempty(rest) ? fqn : _ns_join(fqn, rest)
    elseif startswith(name, "/")
        String(name)                                   # absolute
    else
        _ns_join(_normalize_namespace(_resolver_namespace(node)), name)  # relative
    end
    resolved = _apply_remap(node, resolved, kind)
    return _validate_fqn(resolved)
end

# Remap rule application. The rule table lives on the node/context as `remaps`
# (an ordered `Vector{Pair{String,String}}`), empty until `--ros-args` parsing
# lands. Exact-match `from => to` on the resolved FQN (rclcpp's static remap).
# TODO(layer): `__ns:=`/`__node:=` and node-scoped `nodename:from:=to` parsing
# from `--ros-args`; today only an explicit `remaps` table is honored.
function _apply_remap(node, resolved::AbstractString, ::Symbol)
    hasproperty(node, :remaps) || return resolved
    for (from, to) in node.remaps
        from == resolved && return to
    end
    resolved
end

# ── discovery: the @ros2_lv/** liveliness subscriber (§12) ─────────────────
# One channel-form subscriber per Context, drained by a Julia consumer task. Each
# token PUT/DELETE updates the index and fans a `GraphChange` to listeners. Parse
# failures are logged and skipped — a malformed/foreign token must not kill the task.
#
# Channel (FIFO) form, NOT callback — this is load-bearing for deadlock-freedom. The
# callback form runs `_ingest_liveliness!` on a foreign libzenohc thread; a stop-the-
# world GC anywhere else in the process (e.g. the first service round-trip's first-
# call JIT — acute when a process queries *itself*, both serving and calling) must
# then halt that foreign thread, but it can be mid-Julia-callback and unable to reach
# a safepoint → deadlock. Draining a FIFO on a Julia-managed (GC-safe) consumer task
# keeps ALL Julia execution off foreign threads, so a GC can always complete (§12/D8).

# The wildcard liveliness keyexpr. rmw_zenoh tokens live under `@ros2_lv/**`;
# ros2dds under `@/<zid>/@ros2_lv/**`, so `**/@ros2_lv/**` covers both.
_lv_wildcard(::RmwZenoh) = "@ros2_lv/**"
_lv_wildcard(::KeyExprFormat) = "**/@ros2_lv/**"

function _start_discovery!(ctx::Context)
    ke = Keyexpr(_lv_wildcard(ctx.format))
    # `history=true` replays the live token set so a late-joining Context sees the
    # existing graph (§12 eventual consistency). Capacity is generous: a discovery
    # burst (many tokens at once) buffers rather than drops, and the consumer drains
    # it fast (a locked index update per token, no user handler).
    sub = LivelinessSubscriberHandler(ctx.session, ke; channel=:fifo,
                                      capacity=1024, history=true)
    ctx._lv_sub = sub
    # Plain `@spawn` (migratable): the index update is lock-guarded, needs no thread
    # affinity, and runs on any thread under `-t>=2`. Closing the handler (drain/§14)
    # disconnects the channel → the `for` loop ends → the task exits.
    Threads.@spawn begin
        try
            for sample in sub
                try
                    _ingest_liveliness!(ctx, sample)
                catch err
                    err isa ShutdownException && return
                    @error "discovery: liveliness sample handling failed" exception=(err, catch_backtrace())
                end
            end
        catch err
            err isa ShutdownException && return
            isopen(ctx) &&
                @error "discovery: liveliness consumer task failed" exception=(err, catch_backtrace())
        end
    end
    return ctx
end

# Apply one liveliness sample to the index. PUT = appeared (parse + insert),
# DELETE = withdrew (remove by the token key). Node tokens (`parse_liveliness`
# returns a `NodeEntity`) carry no endpoint, so they only register the node's
# presence — we track endpoints here and recover node identity from them.
function _ingest_liveliness!(ctx::Context, sample)
    key = Zenoh.keyexpr(sample)                       # the token string is the index key
    appeared = Zenoh.kind(sample) === Zenoh.SampleKinds.PUT

    if !appeared
        removed = nothing
        @lock ctx.graph.lock begin
            removed = pop!(ctx.graph.endpoints, key, nothing)
        end
        removed === nothing || _notify_graph_change!(ctx, EndpointInfo[], EndpointInfo[removed])
        return nothing
    end

    entity = parse_liveliness(ctx.format, key)
    # A node token (`NN` kind) parses to a `NodeEntity` — no endpoint to index.
    entity isa EndpointEntity || return nothing

    info = _endpoint_info(entity; is_local=false)
    @lock ctx.graph.lock ctx.graph.endpoints[key] = info
    _notify_graph_change!(ctx, EndpointInfo[info], EndpointInfo[])
    return nothing
end

# Wake graph waiters and fan a `GraphChange` to user listeners. Listeners run
# outside the index lock (their handlers may query the graph). The condition is
# how `wait_for_service`/`wait_for_matched` (§12) re-check on each change.
function _notify_graph_change!(ctx::Context, added::Vector{EndpointInfo},
                               removed::Vector{EndpointInfo})
    @lock ctx.graph.changed notify(ctx.graph.changed)
    # Snapshot under the index lock, then fire outside it: listeners may query the
    # graph or register more, and a concurrent `push!` must not reallocate the
    # Vector mid-iteration.
    fns = @lock ctx.graph.lock copy(ctx.graph.listeners)
    isempty(fns) && return nothing
    change = (; added=added, removed=removed)
    for f in fns
        try
            f(change)
        catch err
            @error "on_graph_change listener threw" exception=(err, catch_backtrace())
        end
    end
    nothing
end

"""
    inject_endpoint!(ctx, key::AbstractString, e::EndpointEntity)

Register one of *our own* entities in the discovery index immediately (§6/§12),
authoritative ahead of the liveliness round-trip. `key` is the entity's
liveliness keyexpr (its unique index key). The entity layer calls this when it
declares an endpoint; `remove_endpoint!` is the close-side inverse.
"""
function inject_endpoint!(ctx::Context, key::AbstractString, e::EndpointEntity)
    info = _endpoint_info(e; is_local=true)
    @lock ctx.graph.lock ctx.graph.endpoints[String(key)] = info
    _notify_graph_change!(ctx, EndpointInfo[info], EndpointInfo[])
    info
end

"""
    remove_endpoint!(ctx, key::AbstractString)

Drop a locally-injected entity from the index (the close-side inverse of
[`inject_endpoint!`](@ref)).
"""
function remove_endpoint!(ctx::Context, key::AbstractString)
    removed = @lock ctx.graph.lock pop!(ctx.graph.endpoints, String(key), nothing)
    removed === nothing || _notify_graph_change!(ctx, EndpointInfo[], EndpointInfo[removed])
    removed
end

"""
    endpoints_snapshot(ctx) -> Vector{EndpointInfo}

An immutable snapshot of the index (copied out under the lock, §12.1) — a
consistent instant, not a live alias. The §12 query layer (`endpoints`,
`topic_names_and_types`, …) filters over this.
"""
endpoints_snapshot(ctx::Context) =
    @lock ctx.graph.lock collect(values(ctx.graph.endpoints))

"""
    on_graph_change(f, node_or_ctx) -> f

Register `f(change)` (`change.added` / `change.removed :: Vector{EndpointInfo}`)
to fire on every discovery change (§12.3). Returns `f` so it can be removed.
"""
function on_graph_change(f::Function, ctx::Context)
    @lock ctx.graph.lock push!(ctx.graph.listeners, f)
    f
end
on_graph_change(f::Function, node) = on_graph_change(f, _ctx(node))

# ── shutdown state machine + drain (§14) ────────────────────────────────────

"""
    is_shutdown(ctx) -> Bool

True once the Context has begun shutting down (`:shutting_down` or `:shutdown`).
The loop-guard predicate behind `while isopen(node)` / `!is_shutdown(ctx)` (§14).
"""
is_shutdown(ctx::Context) = (@atomic ctx._state) !== running

"`isopen(ctx)` is the positive form of [`is_shutdown`](@ref)."
Base.isopen(ctx::Context) = (@atomic ctx._state) === running

"""
    on_shutdown(f, ctx) -> f

Register user cleanup `f()` to run during the drain (§14.1, step 2 — before the
session closes), each bounded by a timeout so one hook can't stall the rest.
Returns `f`.
"""
function on_shutdown(f::Function, ctx::Context)
    @lock ctx._state_lock push!(ctx._on_shutdown, f)
    f
end
on_shutdown(f::Function, node) = on_shutdown(f, _ctx(node))

"""
    register_resource!(ctx, handle) -> handle

Track a `close`-able handle (node, endpoint, timer) so the drain undeclares it
(§14.1, step 4). The entity layer calls this at construction; the drain walks the
list in reverse so dependents close before their dependencies.
"""
function register_resource!(ctx::Context, handle)
    @lock ctx._state_lock push!(ctx._resources, handle)
    handle
end

"""
    request_shutdown(ctx; reason="") -> Bool

Begin a graceful drain (§14). Idempotent and async-safe: the first caller flips
`:running → :shutting_down` and spawns the drain task; later calls are no-ops.
Returns `true` if this call initiated shutdown. Does *not* wait — `close(ctx)`
or `wait(ctx)`/`spin(ctx)` block for completion.
"""
function request_shutdown(ctx::Context; reason::AbstractString="")
    initiated = false
    @lock ctx._state_lock begin
        if (@atomic ctx._state) === running
            @atomic ctx._state = shutting_down
            initiated = true
        end
    end
    initiated || return false
    isempty(reason) || @info "context shutting down" reason
    # The drain runs on its own task so a signal/handler caller returns at once;
    # `spin`/`wait`/`close` park on `_shutdown_done` until it finishes.
    @async _drain!(ctx)
    return true
end

# The one drain path (§14.1) every trigger funnels through. Ordered so peers see
# a clean exit and in-flight work finishes:
#   1. (already flipped to :shutting_down) wake clock-waits / blocked calls
#   2. run on_shutdown hooks
#   3. await in-flight tasks up to drain_timeout  (TODO: task tracking, §8/§9)
#   4. undeclare entities (liveliness + routes)
#   5. close the session  → :shutdown, notify spin/wait waiters
function _drain!(ctx::Context)
    # The whole drain is wrapped so the terminal state-flip + waiter notify in the
    # `finally` *always* run — even if a step throws unexpectedly. This is the
    # invariant `close`/`wait`/`spin` rely on: a drain that started always
    # completes, so a blocked main task can never hang on a drain-internal bug.
    try
        # 1. Wake blocked clock-waits and the graph waits so they unwind with a
        # ShutdownException rather than hanging the drain. `_shutdown_wake` releases
        # `_interruptible_sleep`/Rate/sleep_until (time.jl), `graph.changed` the §12
        # graph waits; both re-check `is_shutdown` and raise.
        @lock ctx._shutdown_wake notify(ctx._shutdown_wake; all=true)
        @lock ctx.graph.changed notify(ctx.graph.changed; all=true)

        # 2. on_shutdown hooks, each bounded so one slow hook can't stall the rest.
        hooks = @lock ctx._state_lock copy(ctx._on_shutdown)
        for h in hooks
            _run_bounded(ctx, "on_shutdown hook", ctx.drain_timeout) do
                h()
            end
        end

        # 3. Await in-flight goal/service handler tasks (incl. detached cleanup)
        # up to drain_timeout. TODO(layer §8/§9): the Service/Action layers
        # register their handler tasks with the Context; until then there is
        # nothing to await and this is a no-op — the timeout still bounds the
        # whole drain via steps 2 and 5.

        # 4–5. Undeclare entities (clean graph departure, reverse order), stop
        # discovery, then close the session — all **bounded by drain_timeout**. A
        # wedged native close (e.g. libzenohc blocking under router contention) must
        # not hang shutdown forever: the drain gives up after the timeout, its
        # `finally` flips the state + wakes `close`/`spin`/`wait`, and the stuck
        # native call dies with the process. (Without this bound a single stuck
        # close is the catastrophic, SIGTERM-ignoring hang.)
        _run_bounded(ctx, "drain teardown", ctx.drain_timeout) do
            resources = @lock ctx._state_lock reverse(copy(ctx._resources))
            for r in resources
                try
                    close(r)
                catch err
                    @error "drain: closing resource failed" exception=(err, catch_backtrace())
                end
            end
            if ctx._lv_sub !== nothing
                try
                    close(ctx._lv_sub)
                catch err
                    @error "drain: closing discovery subscriber failed" exception=(err, catch_backtrace())
                end
                ctx._lv_sub = nothing
            end
            # §7 sim-time: the `/clock` sub rides `_clock_node` (a registered resource,
            # already closed in the reverse walk above). Just clear the references + the
            # opt-in set so nothing dangles.
            @lock ctx._sim_lock begin
                ctx._clock_sub = nothing
                ctx._clock_node = nothing
                empty!(ctx._sim_users)
                @atomic ctx.sim_time_ns = nothing
            end
            try
                isopen(ctx.session) && close(ctx.session)
            catch err
                @error "drain: closing session failed" exception=(err, catch_backtrace())
            end
        end
    finally
        @atomic ctx._state = shutdown_done
        @lock ctx._shutdown_done notify(ctx._shutdown_done; all=true)
    end
    return nothing
end

# Run `f()` with a wall-time bound: spawn it, poll for completion up to `secs`,
# log on overrun (the task keeps running detached — we don't hard-kill a hook
# mid-cleanup, the drain just moves on). Exceptions from `f` are logged, not
# raised. `Base.timedwait` is the bound — note `Timer` here would resolve to
# ROSNode's clock `Timer` (§7), so the wall-clock primitives are spelled `Base.`.
function _run_bounded(f, ::Context, what::AbstractString, secs::Real)
    t = @async try
        f()
    catch err
        @error "$what threw" exception=(err, catch_backtrace())
    end
    Base.timedwait(() -> istaskdone(t), Float64(secs); pollint=0.02) === :ok ||
        @warn "$what exceeded drain timeout; continuing" timeout_s=secs
    nothing
end

"""
    close(ctx::Context)

Synchronous shutdown (§14): request the drain (idempotent) and block the calling
task until it completes. Safe to call twice — a second `close` (e.g. a do-block
block-exit after `spin` already drained) parks momentarily and returns.
"""
function Base.close(ctx::Context)
    request_shutdown(ctx)
    wait(ctx)
    nothing
end

"""
    wait(ctx::Context)

Park the calling task until the Context has fully drained (`:shutdown`). Returns
immediately if already drained. This is the body `spin` parks on.
"""
function Base.wait(ctx::Context)
    (@atomic ctx._state) === shutdown_done && return nothing
    @lock ctx._shutdown_done begin
        while (@atomic ctx._state) !== shutdown_done
            wait(ctx._shutdown_done)
        end
    end
    nothing
end

"""
    spin(ctx; handle_signals=false)

Park the main task until the Context shuts down (§14). The scheduler runs the
callbacks; `spin` keeps the process alive and gives signals a home (the [executor](https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Executors.html)
role lives in the scheduler, not here). `handle_signals=true` installs
SIGINT/SIGTERM handlers for the duration of the spin — opt-in, so the host keeps
its own handlers when a library embeds us: first signal = graceful drain, a
second SIGINT = hard `exit(130)`.
"""
function spin(ctx::Context; handle_signals::Bool=false)
    if handle_signals
        _with_signal_handlers(ctx) do
            _park_until_drained(ctx)
        end
    else
        wait(ctx)
    end
    nothing
end

# Park on the drain, turning SIGINT into a graceful shutdown. With
# `exit_on_sigint(false)` set (see `_install_signal_handlers!`), Julia delivers a
# Ctrl-C as an `InterruptException` thrown into this task at its `wait` yield-point
# rather than exiting the process. The first requests the drain (idempotent) and
# re-parks until it completes; a second hard-exits.
function _park_until_drained(ctx::Context)
    interrupted = false
    while true
        try
            wait(ctx)
            return nothing
        catch err
            err isa InterruptException || rethrow()
            if !interrupted
                interrupted = true
                @info "SIGINT — draining; press Ctrl-C again to force exit"
                request_shutdown(ctx; reason="SIGINT")
            else
                @warn "second SIGINT — forcing exit"
                exit(130)
            end
        end
    end
end

# Signal handling (§14.3): opt-in, scoped to the spin call. Installs handlers,
# runs `f`, removes them on return. First signal → graceful `request_shutdown`;
# a second SIGINT while draining → immediate `exit(130)`.
# TODO(layer): a process-global manager that fans one signal to all opted-in
# Contexts (multi-Context processes); today this installs per-spin handlers.
function _with_signal_handlers(f, ctx::Context)
    installed = false
    try
        _install_signal_handlers!(ctx)
        installed = true
    catch err
        @warn "could not install signal handlers; spinning without them" exception=err
    end
    try
        f()
    finally
        installed && _remove_signal_handlers!(ctx)
    end
end

# Per the design's "scheduler does the work": we don't install a raw sigaction.
# `exit_on_sigint(false)` makes Julia route a Ctrl-C to the spin task as an
# `InterruptException`; `_park_until_drained` catches it and drives the graceful
# request_shutdown (first) / hard exit (second). TODO(layer): SIGTERM + the
# uv_async_send relay; today SIGINT (Ctrl-C) is the only signal wired.
function _install_signal_handlers!(::Context)
    Base.exit_on_sigint(false)
    return nothing
end
_remove_signal_handlers!(::Context) = (Base.exit_on_sigint(true); nothing)

# ── node ↔ context plumbing ────────────────────────────────────────────────
# The §6 Node holds its Context; until that type lands, accept either a Context
# directly or anything exposing a `.context` field (the shape Node will have).

"Recover the owning `Context` from a node-or-context argument."
_ctx(ctx::Context) = ctx
_ctx(node) = hasproperty(node, :context) ? node.context :
             throw(ArgumentError("expected a Context or a Node holding one"))

# `Node` is the §6 layer's type. Declared here only so the `Context`/`Node`
# vocabulary co-exports cleanly and duck-typed accessors above name it in docs;
# the entity layer defines the concrete type and its constructors.
# TODO(§6): the Node type + `Node(ctx, name; namespace)` constructor land in the
# object-model file, which owns this name.
function Node end
