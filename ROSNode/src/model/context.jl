# Contexts — the process-level container. One Context = one Zenoh session; N nodes
# share it. It is the single shutdown root: close(ctx) is the one authoritative
# teardown that undeclares entities, stops discovery, and closes the session in order.
#
# Name resolution is duck-typed against a node's `namespace`/`name`/`fqn`, so a bare
# Context works wherever a node would.

using Zenoh: Zenoh, Config, Session, Keyexpr, zid, to_le_bytes, LivelinessSubscriber,
             LivelinessSubscriberHandler
import Zenoh
using ROSZenoh: ROSZenoh, ZenohId, RmwZenoh, KeyExprFormat, NodeEntity,
                EndpointEntity, EndpointKind, parse_liveliness

export Context, Node, # `Node` forward-declared here; the node layer defines it
       resolve_name, on_shutdown, request_shutdown, is_shutdown,
       spin, session, next_entity_id!, registry, graph,
       EndpointInfo, NodeInfo, GraphIndex

# ── discovery index ────────────────────────────────────────────────

"""
    EndpointInfo

A single discovered or locally-injected endpoint — the atom of the [discovery
graph](https://docs.ros.org/en/rolling/Concepts/Basic/About-Discovery.html).
Each record names:

- the owning node (`node_name`, `namespace`)
- the endpoint `kind` (`Publisher`/`Subscription`/`Service`/`Client`)
- the resolved `topic` FQN
- its `type` (a `TypeInfo`: ROS 2 name plus RIHS01 hash)
- the `qos` profile
- the 16-byte `gid`
- `is_local`

Each record carries the RIHS01 hash and gid, so a caller can run its own type
and QoS compatibility checks against the same data the auto-detectors
use. `type === nothing` for a type-less, liveliness-only endpoint. `is_local`
true marks one of our own injected entities (authoritative, visible ahead of
the liveliness round-trip); false marks a liveliness-discovered remote
(eventually consistent).

For a remote endpoint whose liveliness token carries no node identity (the
ros2dds bridge-compat case), `node_name`/`namespace` are empty and `gid` is
zero-filled.
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

Identity of a discovered node: its `name`, `namespace`, and security `enclave`.
This is the shape `node_names` returns from a graph query. `node_names`
reports `enclave` as `""`: the enclave is unrecoverable from rmw_zenoh
liveliness tokens, which always carry `%` on the wire.
"""
struct NodeInfo
    name::String
    namespace::String
    enclave::String
end

# Build an `EndpointInfo` from a parsed `EndpointEntity`. A ros2dds token yields
# `node === nothing`, leaving node fields blank and the gid zero-filled.
function _endpoint_info(e::EndpointEntity; is_local::Bool)
    nn = e.node === nothing ? "" : e.node.name
    ns = e.node === nothing ? "" : e.node.namespace
    # gid is derived from the node identity; zero-fill when there is none.
    gid = e.node === nothing ? ntuple(_ -> 0x00, 16) : ROSZenoh.entity_gid(e)
    EndpointInfo(nn, ns, e.kind, e.topic, e.type_info, e.qos, gid, is_local)
end

"""
    EndpointDesc(kind, topic, type_info, qos)

The wire identity of one endpoint a node declares — `kind`, resolved `topic` FQN,
`type_info`, and `qos` — known before its `Entity`/Zenoh route exists. The
pre-materialisation half of an [`EndpointInfo`](@ref); the gid follows from the
reserved id at construction. Each pattern derives its own (`_publisher_desc`,
`_service_desc`, `_action_descs`, …) as the ONE source for that endpoint's identity;
`make_entity(node, ::EndpointDesc)` turns one into a live entity, and
[`node_endpoint_descs`](@ref) enumerates a node's whole set from the same functions —
so materialisation and enumeration share a single derivation, not two kept in sync.
"""
struct EndpointDesc
    kind::EndpointKind
    topic::String
    type_info::Union{TypeInfo, Nothing}
    qos::QosProfile
end

# An already-materialised (or directly-injected) entity's identity as a descriptor —
# for enumerating endpoints that bypass `make_entity`, e.g. the node-presence shell.
_endpoint_desc(e::EndpointEntity) = EndpointDesc(e.kind, e.topic, e.type_info, e.qos)

"""
    GraphIndex()

The Context's discovery index: the observed `EndpointInfo` set keyed by
liveliness keyexpr, behind a lock, plus a `Condition` that graph waits park on
and a list of user `on_graph_change` listeners.

The single `@ros2_lv/<domain>/**` liveliness subscriber drives it — a token PUT
inserts or updates a record, a DELETE removes it by the same key — and our own
entities are injected directly by the entity layer. The `changed` condition is
notified on every change so waiters (`wait_for_service`, `on_graph_change`)
re-check their predicate. This is the one change stream every discovery detector
and wait is a view over.

The no-argument constructor builds an empty index; the Context creates exactly
one.
"""
mutable struct GraphIndex
    lock::ReentrantLock
    # REMOTE endpoints, keyed by liveliness token string — discovered peers, mutated by the
    # liveliness consumer as tokens arrive (genuine runtime data).
    endpoints::Dict{String, EndpointInfo}
    # OUR OWN endpoints, same keying — held apart from the remote stream so a statically-known
    # node's local graph is not built by per-declare mutation of the discovery dict (the Stage B
    # seam). `endpoints_snapshot` unions the two; a remote echo of our own token (same key) is
    # dropped in favour of the local entry.
    local_endpoints::Dict{String, EndpointInfo}
    # Notified on every change; waiters re-check the graph predicate.
    changed::Threads.Condition
    # `on_graph_change` listeners, fired outside the lock since they may re-query the graph.
    listeners::Vector{Any}
end

GraphIndex() = GraphIndex(ReentrantLock(), Dict{String, EndpointInfo}(),
                          Dict{String, EndpointInfo}(), Threads.Condition(), Any[])

# ── type registry ──────────────────────────────────────────────────────────
# Keyed by (name, RIHS01 hash) so evolved versions of a type coexist. RIHS01 is the
# decode-safety identity, not a version tag: a hash match means the local struct can
# safely decode the peer's wire bytes.

"""
    TypeRegistry
    TypeRegistry()

The per-`Context` type table that backs runtime type support. Keyed by
`(type_name, RIHS01 hash)`, so several evolved versions of one interface coexist
under distinct keys — the hash (the ROS Interface Hashing Standard,
[REP 2011](https://ros.org/reps/rep-2011.html)) is the decode-safety identity, not
just a version tag (see [`RegistryEntry`](@ref) for the entry shape).

The `Context` owns the storage and the guarding lock; the typesupport layer fills
and reads it. Values are [`RegistryEntry`](@ref) records, held as `Any` so the
context layer stays free of the entry shape. Every node on a `Context` shares one
registry, so a type discovered by one node is visible to all. Use
[`register_type!`](@ref) to store and [`lookup_type`](@ref) to fetch; the
zero-argument constructor builds an empty registry with a fresh `ReentrantLock`
(the `Context` constructor does this for you).
"""
mutable struct TypeRegistry
    lock::ReentrantLock
    entries::Dict{Tuple{String, ROSZenoh.TypeHash}, Any}
end

TypeRegistry() = TypeRegistry(ReentrantLock(), Dict{Tuple{String, ROSZenoh.TypeHash}, Any}())

"""
    register_type!(reg::TypeRegistry, info::TypeInfo, entry) -> entry

Store `entry` (a [`RegistryEntry`](@ref), opaque to the context layer) in `reg`
under the key `(info.name, info.hash)`, returning `entry`. The store overwrites
any existing value at that exact `(name, RIHS01-hash)` key; a different hash
version of the same name lands under a separate key and coexists. Takes the
registry lock for the duration of the write, so concurrent registrations are
serialized.
"""
function register_type!(reg::TypeRegistry, info::TypeInfo, entry)
    @lock reg.lock reg.entries[(info.name, info.hash)] = entry
    entry
end

"""
    lookup_type(reg::TypeRegistry, info::TypeInfo) -> Union{Any, Nothing}

Fetch the value stored under `(info.name, info.hash)`, or `nothing` when that
exact name-and-version pair has not been registered. A `nothing` result is the
trigger to acquire the type (cache, ament, or dynamic over-the-wire
discovery). The lookup takes the registry lock for the read; the match is exact
on both name and RIHS01 hash, so a peer advertising a different version of the
same type misses and drives a fresh acquisition. The registry stores values as
`Any`, so the result comes back untyped — callers check it `isa RegistryEntry`
before trusting it (a [`RegistryEntry`](@ref) is what the typesupport layer ever
puts in).
"""
lookup_type(reg::TypeRegistry, info::TypeInfo) =
    @lock reg.lock get(reg.entries, (info.name, info.hash), nothing)

# ── shutdown state ──────────────────────────────────────────────────
# `running → shutting_down → shutdown_done`. Every trigger (close / request_shutdown /
# signal) funnels through the one drain path; the drain always reaches shutdown_done.

@enum ShutdownState running shutting_down shutdown_done

# ── the Context ───────────────────────────────────────────────────────────

"""
    Context(; domain_id=nothing, namespace=nothing, enclave=nothing,
            config=nothing, format=RmwZenoh(), localhost_only=false,
            peers=String[], drain_timeout=5.0, shm_clients=nothing, home=nothing,
            weak_types=false)
    Context(f::Function; kwargs...)

The process-level container shared by every node in a program — one
`Context` holds exactly one Zenoh `Session`, and N nodes draw on it. It owns the
session and its hex `z_id`, an atomic
entity-id counter, the `domain_id`, the default `namespace`/`enclave`, the
keyexpr formatter, the type registry, the discovery index with its
`@ros2_lv/<domain>/**` liveliness subscriber, a clock, the
`on_shutdown` hooks, and the shutdown state machine. It is the
RAII/shutdown root: `close(ctx)` runs the drain, undeclares entities, and
closes the session (the `rclcpp::init`/`shutdown` bracket).

Configuration is two layers. The Zenoh session config is Zenoh.jl's `Config`
(`config=`, default a fresh peer-mode `Config`); `localhost_only` and `peers`
layer the ROS transport-shaping env into it:

- `localhost_only=true` — disables multicast scouting and, when no explicit
  `peers` are given, connects to the loopback router (`tcp/localhost:7447`).
- `peers` — sets `connect/endpoints`.
- `shm_clients` — passes through to the session open.

The ROS settings `domain_id`/`namespace`/`enclave` each fall back to an
environment variable when left `nothing`, with the kwarg winning:

| kwarg | env var fallback |
|-------|------------------|
| `domain_id` | `ROS_DOMAIN_ID` |
| `namespace` | `ROS_NAMESPACE` |
| `enclave` | `ROS_ENCLAVE` |

`namespace`, `domain_id`, and `format` shape how names become Zenoh key
expressions — namespace normalization, domain scoping, and the wire dialect
(`RmwZenoh()`, the primary target, or `Ros2DDS()`). See
[Addressing & Key Expressions](@ref).

`home` binds the module whose baked `__ros_resolve__` table this Context
resolves wire types through, so every keyexpr-only subscription resolves against
one consistent picture; leave it `nothing` to resolve types by content alone
(see [Runtime Type Discovery](@ref)).

`weak_types` sets the process-wide type-revision trust. Left `false` (the
default), a pinned type (registered via `@ros_import`/`@ros_cache` or authored
with `@ros_message`/`@ros_service`/`@ros_action`) enforces its RIHS01: a peer
advertising the same name with a different hash is reported with a diagnostic
and not bound, so a definition-revision skew surfaces instead of silently
rebinding. Set `weak_types=true` to opt into the dynamic fallback — the
mismatched local pin is set aside and the peer's actual revision is
wire-discovered and bound for that endpoint. This is independent of the
per-subscription `weak` keyexpr flag, which only widens keyexpr matching.

The constructor opens the session, registers the canonical bootstrap and
statically-generated types, and starts the discovery consumer task before
returning.

The do-block form opens a Context, runs `f(ctx)`, and `close`s it on exit (even
on throw). [`@context`](@ref) is the sugar that also binds the caller's module
as `home`.

```julia
Context(localhost_only = true) do ctx
    node = Node(ctx, "talker")
    # ... build entities ...
    spin(ctx)
end
```
"""
mutable struct Context
    const session::Session
    const z_id::ZenohId                  # the session id in ROSZenoh's hex form
    const domain_id::Int
    const namespace::String              # default node namespace
    const enclave::String
    const format::KeyExprFormat
    @atomic _next_id::Int                # atomic entity-id allocator
    const registry::TypeRegistry
    const graph::GraphIndex
    # The `@ros2_lv/<domain>/**` liveliness subscriber (channel form) feeding `graph`,
    # drained by a Julia consumer task to keep Julia off foreign libzenohc threads
    # (see `_start_discovery!`). `Any` since Zenoh's handler type is out of scope here.
    _lv_sub::Any
    # Clock handles by source — `clock(ctx, C())` returns/creates one. The Context fills
    # the node-role for the duck-typed `node.clocks` lookup.
    const clocks::Dict{DataType, Any}
    # The Context-hosted `/clock` value, atomically held. `nothing` until a node opts
    # into `use_sim_time` and the `/clock` sub lands.
    @atomic sim_time_ns::Union{Int64, Nothing}
    # Sim-time wiring: one process-level `/clock` source, per-node opt-in. `_sim_users`
    # holds the opted-in nodes (sim active iff non-empty); `_clock_node`/`_clock_sub` are
    # the hidden internal node + its `/clock` Subscription. `_sim_lock` guards all four
    # and must never be acquired nested with `_state_lock`.
    _clock_node::Any
    _clock_sub::Any
    const _sim_users::Set{Any}
    const _sim_lock::ReentrantLock
    # Shutdown state + drain machinery.
    const _state_lock::ReentrantLock
    @atomic _state::ShutdownState
    const _on_shutdown::Vector{Any}      # user cleanup hooks, run during the drain
    const _shutdown_done::Threads.Condition  # `spin`/`wait` park here until drained
    # Notified `all=true` at drain start so blocked clock-waits and graph waits re-check
    # `is_shutdown` and raise ShutdownException. `_shutdown_done` is the terminal counterpart.
    const _shutdown_wake::Threads.Condition
    const drain_timeout::Float64         # seconds to await in-flight work
    # Registered close-able handles; `close(ctx)` walks them in reverse so dependents
    # close before their dependencies.
    const _resources::Vector{Any}
    # Type-resolution lens: the module whose baked `__ros_resolve__` table this Context
    # resolves wire types through, so every node/sub sees one consistent picture.
    # `nothing` ⇒ content-canonical resolution only.
    const home::Union{Module, Nothing}
    # Type-revision trust. `false` (default): a pinned (`:static`/`:authored`) type
    # enforces its RIHS01, so a peer advertising the same name with a different hash is
    # rejected with a diagnostic. `true` (weak mode): the dynamic fallback binds the
    # peer's wire-discovered revision past a pinned mismatch. Orthogonal to the
    # per-subscription `weak` keyexpr flag.
    const weak_types::Bool
end

# Z-ID hex reproduces zenoh's `z_id_to_string`: reverse the LE bytes (MSB first) and
# strip leading zero nibbles, so the hex matches what rmw_zenoh writes into liveliness
# tokens and `parse_liveliness` reads back. Any deviation breaks node-identity matching.
function _zid_hex(s::Session)
    le = to_le_bytes(zid(s))                       # NTuple{16,UInt8}, little-endian
    hex = bytes2hex(reverse(collect(le)))
    i = findfirst(!=('0'), hex)                    # strip leading zero nibbles
    i === nothing && return "0"                    # all-zero ⇒ invalid session
    hex[i:end]
end

# ROS env defaults; explicit kwargs win at the call site.
_env_domain_id() = (v = get(ENV, "ROS_DOMAIN_ID", nothing); v === nothing ? 0 : parse(Int, v))
_env_namespace() = get(ENV, "ROS_NAMESPACE", "")
_env_enclave()   = get(ENV, "ROS_ENCLAVE", "")

# Translate the ROS transport-shaping env vars into the Zenoh `Config`: localhost-only
# → scouting off + loopback connect; static peers → connect/endpoints. `domain_id` lives
# in the keyexprs rather than the transport, so it is set elsewhere.
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
                   home::Union{Module, Nothing}=nothing,
                   weak_types::Bool=false)
    dom = domain_id === nothing ? _env_domain_id() : Int(domain_id)
    ns  = _normalize_namespace(namespace === nothing ? _env_namespace() : String(namespace))
    enc = enclave === nothing ? _env_enclave() : String(enclave)

    # Reuse the caller's `Config` when given, else a default peer-mode one; either
    # way the ROS transport env is layered in.
    cfg = config === nothing ? Config() : config
    _apply_ros_transport_config!(cfg; localhost_only=localhost_only,
                                       peers=collect(String, peers))

    sess = shm_clients === nothing ? Base.open(cfg) : Base.open(cfg; shm_clients=shm_clients)

    ctx = Context(sess, ZenohId(_zid_hex(sess)), dom, ns, enc, format,
                  0, TypeRegistry(), GraphIndex(), nothing,
                  Dict{DataType, Any}(), nothing,
                  nothing, nothing, Set{Any}(), ReentrantLock(),
                  ReentrantLock(), running, Any[], Threads.Condition(),
                  Threads.Condition(), Float64(drain_timeout), Any[], home,
                  weak_types)

    # Register the bootstrap types (type_description_interfaces) so the type-description
    # server can serve them, plus any @ros_import/@ros_cache statically-generated types so
    # keyexpr-only resolution uses the precompiled struct directly.
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
    @context([kwargs...]) do ctx
        ...
    end

Create a [`Context`](@ref ROSNode.Context) whose type-resolution `home` is the calling module,
then run the do-block body with it and `close` on exit. Sugar for
`Context(f; home=@__MODULE__, kwargs...)`.

Binding the calling module as `home` makes every keyexpr-only subscription on
the Context resolve wire types to that module's `@ros_import` structs, giving
one consistent, load-order-independent picture across the session. Any keyword
arguments inside the parentheses pass through to the `Context` constructor
(`peers`, `localhost_only`, `domain_id`, ...).

Throws at macro-expansion time if invoked without a trailing do-block. For a
different resolution lens use `Context(...; home=Mod)`; for content-canonical
resolution only use a plain `Context()`.

```julia
@context(peers = ["tcp/localhost:7447"], domain_id = 0) do ctx
    node = Node(ctx, "listener")
    spin(ctx)
end
```
"""
macro context(args...)
    # Julia hands a trailing do-block to a macro as its FIRST argument (an `->` lambda),
    # with any in-paren `kwargs` following — so the body is `args[1]`.
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

# ── accessors others need ─────────────────────────────────────────────

"""
    session(ctx) -> Session

The Context's Zenoh `Session`, shared by all of its nodes (one session per
Context). Entity constructors use it to declare publishers, subscribers, and
queryables.
"""
session(ctx::Context) = ctx.session

"""
    registry(ctx) -> TypeRegistry

The Context's type registry — the per-Context table keyed by
`(type_name, RIHS01 hash)` so evolved versions of a type coexist. The Context
owns the storage and lock; the typesupport layer populates and reads entries
(static bootstrap types at construction, dynamically-discovered types over the
wire). Shared by all of the Context's nodes.
"""
registry(ctx::Context) = ctx.registry

"""
    graph(ctx) -> GraphIndex

The Context's discovery index: the live `EndpointInfo` set fed by the
`@ros2_lv/<domain>/**` liveliness subscriber and by our own injected entities.
The graph-query and graph-wait surface (`endpoints`, `topic_names_and_types`,
`wait_for_service`, `on_graph_change`) are views over this one index.
"""
graph(ctx::Context) = ctx.graph

"""
    next_entity_id!(ctx) -> Int

Allocate the next entity id from the Context's atomic counter and return it.
Nodes and endpoints draw from this one counter — the value populates a
ROSZenoh entity's `id` field, which must be unique within the session. The
first allocation returns `1` (the counter starts at 0 and increments before
reading). Thread-safe: the increment-and-fetch is a single atomic operation.
"""
next_entity_id!(ctx::Context) = (@atomic ctx._next_id += 1)

"""
    reserve_entity_ids!(ctx, n) -> Int

Reserve a contiguous block of `n` entity ids from the same atomic counter and return
the first. Lets a node compute its local discovery graph a-priori (the gid/liveliness
key derive from the id) and then materialise the matching entities on those exact ids,
so the pre-built graph and the declared tokens agree. `n == 0` is a no-op returning the
next id without advancing.
"""
reserve_entity_ids!(ctx::Context, n::Integer) = (@atomic ctx._next_id += n) - n + 1

# The Context fills the node-role for context-level clock reads, so a Context works
# wherever the duck-typed `node.clocks` lookup expects a node.
function clock(ctx::Context, src::ClockSource)
    C = typeof(src)
    @lock ctx._state_lock get!(ctx.clocks, C) do
        Clock(ctx, src)
    end::Clock{C}
end
clock(ctx::Context) = clock(ctx, ROS())

# ── sim-time: use_sim_time → /clock routing ──────────────────────────────────
# ONE process-level `/clock` source (a Subscription on a hidden internal node), per-node
# opt-in: `_sim_users` holds the nodes whose ROS clock follows sim time, so `now(node,
# ROS())` reads `sim_time_ns` only for an opted-in node. Jump callbacks fire on the clock
# each node holds (`node.clocks[ROS]`): per-node `sim_activated`/`sim_deactivated` on
# opt-in/opt-out, and `time_forward`/`time_backward` to every active node on a `/clock`
# discontinuity. Follow-only: `now` holds between samples; ROS `Timer`/`Rate` are not
# sim-driven here.
#
# Activation needs a `NodeEntity` for `topic_keyexpr`, hence the hidden internal node
# rather than a bare Context. It runs synchronously under the param-commit `s.lock` via
# the event hook; this is deadlock-free because no path takes a param lock while holding
# `_sim_lock` or a node lock, and `s.lock` is reentrant so a jump callback may re-enter
# parameter mutation on the same thread.

_is_sim_user(ctx::Context, node) = @lock ctx._sim_lock (node in ctx._sim_users)

"""
    set_use_sim_time!(ctx, node, on::Bool) -> nothing

(De)activate sim-time routing for `node`. The first opt-in declares the Context's
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

# Activation delta for a node joining an already-running sim: the sim−system offset so
# threshold callbacks fire; 0 for the first opt-in, which has no `/clock` sample yet.
_activate_delta(ctx::Context) =
    (s = @atomic ctx.sim_time_ns; s === nothing ? Duration(0) : Duration(s - _read_ns(System())))

# Declare the `/clock` Subscription on the hidden internal node (created once, reused
# across reactivations). Each sample's `builtin_interfaces/Time` is ingested as ns.
function _activate_sim!(ctx::Context)
    # Commit both references to the Context only after the Subscription ctor succeeds, so
    # a throw can't strand `_clock_node` without a `/clock` sub and wedge reactivation.
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

# Tear down the `/clock` source: close the sub and clear sim time. `_ingest_clock!`
# ignores a late in-flight sample by re-checking `_sim_users` under the lock.
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

# Ingest one `/clock` sample. A sample arriving after deactivation (empty `_sim_users`)
# is ignored. The first post-activation sample only sets the baseline; a later change
# fires `time_forward`/`time_backward` to every active node.
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

# Fire a jump to the callbacks on one node's ROS clock, gated by `_triggers`. Snapshot
# `c.jumps` under no lock and fire over the copy, decoupling from a concurrent `register!`.
function _fire_jumps_to!(node, j::TimeJump)
    node === nothing && return nothing
    node isa Node && !isopen(node) && return nothing   # skip a closed/closing node
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

# ── name resolution ────────────────────────────────────────────────────
# Resolve a user name to a fully-qualified name before it reaches ROSZenoh, which stays
# naming-agnostic and takes the final topic string. Forms: absolute `/foo` as-is;
# relative `foo` → prepend namespace; private `~/foo` → prepend the node FQN. The remap
# rule table is applied last, then the result is validated.

# Normalize a namespace to a leading-slash, no-trailing-slash form. Empty ⇒ "/".
function _normalize_namespace(ns::AbstractString)
    isempty(ns) && return "/"
    s = startswith(ns, "/") ? String(ns) : "/" * String(ns)
    (length(s) > 1 && endswith(s, "/")) ? rstrip(s, '/') : s
end

# Join a namespace and a relative name without doubling the slash.
_ns_join(ns::AbstractString, name::AbstractString) =
    ns == "/" ? "/" * name : ns * "/" * name

# A node's FQN = namespace + "/" + name. Duck-typed: the Node has
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

Resolve a user-supplied `name` to a fully-qualified name, the form the
ROSZenoh keyexpr builders expect. Three name forms, following ROS 2 name
resolution:

- absolute `"/foo"` — used as-is.
- relative `"foo"` — prepended with the node's namespace.
- private `"~/foo"` (or bare `"~"`) — prepended with the node's FQN
  (`namespace`/`name`).

The resolved name then passes through the remap rules (an ordered exact-match
`from => to` table on the node/context, honored when populated and empty by
default) and is charset/structure-validated: leading slash, no empty `//` token, no
trailing slash, each token starting with a letter or underscore and containing
only letters, digits, and underscores.

Returns the validated FQN. Throws `ArgumentError` for:

- an empty name.
- a private name resolved against a bare `Context` (no node FQN to anchor `~`).
- a resolved name that fails validation.

`kind` (`:topic`/`:service`/`:node`) is
carried for kind-scoped remap rules and does not change the resolution algorithm.

The first argument may be a `Context` (relative names resolve against its
default namespace; private `~` names need a node) or a node exposing
`namespace`/`name`/`fqn`.
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

# Apply the node/context `remaps` table: exact-match `from => to` on the resolved FQN
# (rclcpp's static remap). The table is honored when present; populating it from
# `--ros-args` is not yet wired.
function _apply_remap(node, resolved::AbstractString, ::Symbol)
    hasproperty(node, :remaps) || return resolved
    for (from, to) in node.remaps
        from == resolved && return to
    end
    resolved
end

# ── discovery: the @ros2_lv/<domain>/** liveliness subscriber ────────
# One channel-form subscriber per Context, drained by a Julia consumer task. Each token
# PUT/DELETE updates the index and fans a `GraphChange` to listeners. A parse failure is
# logged and skipped so a malformed or foreign token never kills the task.
#
# The FIFO channel form (not callback) is load-bearing for deadlock-freedom: the callback
# form would run `_ingest_liveliness!` on a foreign libzenohc thread, where a process-wide
# stop-the-world GC can halt it before it reaches a safepoint and deadlock. Draining a
# FIFO on a Julia-managed consumer task keeps all Julia execution off foreign threads, so
# a GC can always complete.

# The wildcard liveliness keyexpr. rmw_zenoh tokens carry the domain as their first
# segment, so the subscription itself scopes discovery to this Context's domain. ros2dds
# tokens (`@/<zid>/@ros2_lv/...`) carry no domain segment, so the generic form is unscoped.
_lv_wildcard(::RmwZenoh, domain_id::Integer) = "@ros2_lv/$domain_id/**"
_lv_wildcard(::KeyExprFormat, ::Integer) = "**/@ros2_lv/**"

function _start_discovery!(ctx::Context)
    ke = Keyexpr(_lv_wildcard(ctx.format, ctx.domain_id))
    # `history=true` replays the live token set so a late-joining Context sees the
    # existing graph. Capacity is sized so a discovery burst buffers rather than drops;
    # the consumer drains it fast (a locked index update per token, no user handler).
    sub = LivelinessSubscriberHandler(ctx.session, ke; channel=:fifo,
                                      capacity=1024, history=true)
    ctx._lv_sub = sub
    # Plain `@spawn` (migratable): the index update is lock-guarded, so it needs no thread
    # affinity. Closing the handler on drain disconnects the channel, ending the loop.
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

# Apply one liveliness sample to the index: PUT inserts or updates the record, DELETE
# removes it by the token key. Node identity is recovered from endpoint tokens, so a
# node-only token carries nothing to index.
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
    entity isa EndpointEntity || return nothing       # node token (`NN`): no endpoint to index
    # Backstop behind the domain-scoped wildcard. A ros2dds entity carries no node, hence
    # no domain, and passes through.
    entity.node === nothing || entity.node.domain_id == ctx.domain_id || return nothing

    info = _endpoint_info(entity; is_local=false)
    @lock ctx.graph.lock ctx.graph.endpoints[key] = info
    _notify_graph_change!(ctx, EndpointInfo[info], EndpointInfo[])
    return nothing
end

# Wake graph waiters and fan a `GraphChange` to user listeners. The condition is how
# `wait_for_service`/`wait_for_matched` re-check on each change.
function _notify_graph_change!(ctx::Context, added::Vector{EndpointInfo},
                               removed::Vector{EndpointInfo})
    @lock ctx.graph.changed notify(ctx.graph.changed)
    # Snapshot the listeners under the lock, then fire outside it: a listener may re-query
    # the graph or register more, which would re-enter the lock or reallocate mid-iteration.
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

Register one of *our own* entities in the discovery index immediately,
authoritative ahead of the liveliness round-trip. `key` is the entity's
liveliness keyexpr (its unique index key). The entity layer calls this when it
declares an endpoint; `remove_endpoint!` is the close-side inverse.
"""
function inject_endpoint!(ctx::Context, key::AbstractString, e::EndpointEntity)
    info = _endpoint_info(e; is_local=true)
    @lock ctx.graph.lock ctx.graph.local_endpoints[String(key)] = info
    _notify_graph_change!(ctx, EndpointInfo[info], EndpointInfo[])
    info
end

"""
    remove_endpoint!(ctx, key::AbstractString)

Drop a locally-injected entity from the index (the close-side inverse of
[`inject_endpoint!`](@ref)).
"""
function remove_endpoint!(ctx::Context, key::AbstractString)
    removed = @lock ctx.graph.lock pop!(ctx.graph.local_endpoints, String(key), nothing)
    removed === nothing || _notify_graph_change!(ctx, EndpointInfo[], EndpointInfo[removed])
    removed
end

"""
    endpoints_snapshot(ctx) -> Vector{EndpointInfo}

An immutable snapshot of the index (copied out under the lock) — a
consistent instant, not a live alias. The graph-query layer (`endpoints`,
`topic_names_and_types`, …) filters over this.

Unions our own (`local_endpoints`) with discovered remotes (`endpoints`); a remote
echo of one of our own tokens (same liveliness key) is dropped in favour of the
local entry, so an endpoint is never double-counted across the seam.
"""
function endpoints_snapshot(ctx::Context)
    @lock ctx.graph.lock begin
        out = collect(values(ctx.graph.local_endpoints))
        for (k, info) in ctx.graph.endpoints
            haskey(ctx.graph.local_endpoints, k) || push!(out, info)
        end
        out
    end
end

"""
    _local_remote_split(ctx) -> (locals, remotes)

The discovery index handed back as its two halves — our own endpoints and the discovered
remotes — under one lock. The compat detectors only ever pair one side against the other,
so they take the split directly off the seam rather than re-partitioning a unioned
snapshot on every change. A remote echo of one of our own tokens (same key) is dropped from
`remotes`, matching [`endpoints_snapshot`](@ref)'s dedup.
"""
function _local_remote_split(ctx::Context)
    @lock ctx.graph.lock begin
        locals  = collect(values(ctx.graph.local_endpoints))
        remotes = EndpointInfo[v for (k, v) in ctx.graph.endpoints
                               if !haskey(ctx.graph.local_endpoints, k)]
        (locals, remotes)
    end
end

"""
    on_graph_change(f, node_or_ctx) -> f

Register `f(change)` (`change.added` / `change.removed :: Vector{EndpointInfo}`)
to fire on every discovery change. Returns `f` so it can be removed.
"""
function on_graph_change(f::Function, ctx::Context)
    @lock ctx.graph.lock push!(ctx.graph.listeners, f)
    f
end
on_graph_change(f::Function, node) = on_graph_change(f, _ctx(node))

# ── shutdown state machine + drain ────────────────────────────────────

"""
    is_shutdown(ctx) -> Bool

True once the Context has begun shutting down — state `shutting_down` or
`shutdown_done`. The loop-guard predicate behind patterns like
`while !is_shutdown(ctx)`; `isopen(ctx)` is its positive complement. Reads the
atomic shutdown state, so it is safe to poll from any task.
"""
is_shutdown(ctx::Context) = (@atomic ctx._state) !== running

"`isopen(ctx)` is the positive form of [`is_shutdown`](@ref)."
Base.isopen(ctx::Context) = (@atomic ctx._state) === running

"""
    on_shutdown(f, ctx) -> f
    on_shutdown(f, node) -> f

Register a cleanup callback `f()` to run during the Context drain — after
blocked waits are woken and before the session closes (the
`rclcpp::on_shutdown` analog). Each hook runs under its own `drain_timeout`
bound: the drain moves on once a hook's timeout elapses, leaving the overrunning
hook running detached (so N slow hooks can extend the drain to roughly
N × `drain_timeout`). Exceptions a hook throws are logged, not propagated.

Returns `f`. The node form recovers the owning Context. Hooks fire in
registration order.

```julia
on_shutdown(ctx) do
    flush(logfile)
end
```
"""
function on_shutdown(f::Function, ctx::Context)
    @lock ctx._state_lock push!(ctx._on_shutdown, f)
    f
end
on_shutdown(f::Function, node) = on_shutdown(f, _ctx(node))

"""
    register_resource!(ctx, handle) -> handle

Track a `close`-able handle (node, endpoint, timer) so the drain undeclares it
during teardown. The entity layer calls this at construction; the drain walks the
list in reverse so dependents close before their dependencies.
"""
function register_resource!(ctx::Context, handle)
    @lock ctx._state_lock push!(ctx._resources, handle)
    handle
end

"""
    request_shutdown(ctx; reason="") -> Bool

Begin a graceful drain of the Context. Idempotent and task-safe (safe to
call from any task, including the Ctrl-C interrupt path): the first caller
flips the state `running -> shutting_down` and spawns the drain task; later
calls are no-ops. Returns `true` if this call initiated shutdown, `false` if a
drain was already underway.

Returns at once — the drain (wake blocked waits, run `on_shutdown` hooks,
undeclare entities, close the session) runs on its own task. To block for
completion use `close(ctx)`, `wait(ctx)`, or `spin(ctx)`. A non-empty `reason`
is logged at info level.
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

# The one drain path every trigger funnels through, ordered so peers see a clean exit
# and in-flight work finishes:
#   1. (state already :shutting_down) wake clock-waits / blocked calls
#   2. run on_shutdown hooks
#   3. await in-flight tasks up to drain_timeout
#   4. undeclare entities (liveliness + routes)
#   5. close the session → :shutdown_done, notify spin/wait waiters
function _drain!(ctx::Context)
    # The `finally` terminal state-flip + waiter notify always run, even if a step throws.
    # This is the invariant `close`/`wait`/`spin` rely on: a started drain always reaches
    # shutdown_done, so a blocked main task can never hang on a drain-internal bug.
    try
        # 1. Wake blocked clock-waits (`_shutdown_wake`) and graph waits (`graph.changed`)
        # so each re-checks `is_shutdown` and unwinds with a ShutdownException rather than
        # hanging the drain.
        @lock ctx._shutdown_wake notify(ctx._shutdown_wake; all=true)
        @lock ctx.graph.changed notify(ctx.graph.changed; all=true)

        # 2. on_shutdown hooks, each bounded so one slow hook can't stall the rest.
        hooks = @lock ctx._state_lock copy(ctx._on_shutdown)
        for h in hooks
            _run_bounded(ctx, "on_shutdown hook", ctx.drain_timeout) do
                h()
            end
        end

        # 3. Await in-flight handler tasks up to drain_timeout. The Service/Action layers
        # do not yet register handler tasks with the Context, so this is a no-op for now;
        # steps 2 and 5 still bound the whole drain.

        # 4–5. Undeclare entities (clean graph departure, reverse order), stop discovery,
        # then close the session, all bounded by drain_timeout. A wedged libzenohc close
        # ignores SIGTERM and would hang shutdown for tens of minutes; the bound makes the
        # drain give up, and its `finally` flips the state and wakes `close`/`spin`/`wait`
        # so the stuck native call dies with the process.
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
            # The `/clock` sub rides `_clock_node`, already closed in the reverse walk
            # above; clear the references and opt-in set so nothing dangles.
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

# Run `f()` with a wall-time bound: spawn it, poll up to `secs`, log on overrun. An
# overrunning task keeps running detached rather than being hard-killed mid-cleanup, so
# the drain moves on. Exceptions from `f` are logged. Wall-clock primitives are spelled
# `Base.` since a bare `Timer` would resolve to ROSNode's clock `Timer`.
#
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

Synchronous shutdown: request the drain (idempotent) and block the calling
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

Park the calling task until the Context has fully drained (state
`shutdown_done`). Returns immediately if already drained. This is the body
`spin` parks on.
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
    spin(ctx; handle_signals=false) -> nothing

Park the calling (typically main) task until the Context shuts down,
keeping the process alive while Julia's scheduler delivers messages and runs
callbacks on their own tasks (the scheduler fills the [executor](https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Executors.html)
role). Returns once the drain has fully completed.

`handle_signals` selects the Ctrl-C behavior:

| value | behavior |
|-------|----------|
| `false` (the default) | exactly `wait(ctx)`; Julia's default Ctrl-C handling applies. |
| `true` | a Ctrl-C (and, when deployed, SIGTERM) drains gracefully (see below). |

Under `handle_signals=true`, `exit_on_sigint(false)` makes Ctrl-C a catchable
`InterruptException`, and the spin task catches it: the first interrupt requests a
graceful drain (and keeps spinning until it finishes), a second forces `exit(130)`. The
spin task polls rather than parking, so it is reliably the task Julia delivers the
interrupt to — the drain therefore runs while the runtime is fully alive (the same way
in the REPL and deployed), never through process exit, where the teardown scheduler can
deadlock. The drain runs every `on_shutdown` hook and member `cleanup`. An `atexit` hook
is the backstop for the other exit paths (a normal return, an error, or SIGTERM — which
`ros2 launch` escalates to). Signal state is restored when `spin` returns, so a library
embedding ROSNode keeps its own handling.

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "listener")
    Subscription(node, "/chatter") do msg
        @info "heard" msg.data
    end
    spin(ctx; handle_signals = true)   # Ctrl-C drains gracefully
end
```
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

# Park on the drain, catching Ctrl-C. A first interrupt requests the drain and keeps
# polling; a second forces `exit(130)`.
#
# We POLL on a short `sleep` rather than parking on `wait(ctx)`. Under
# `exit_on_sigint(false)` Julia throws the SIGINT `InterruptException` into the task
# running on thread 0 at the next safepoint — but a task fully parked in `wait` is never
# that task, so the signal lands on no one and is lost (the process just ignores Ctrl-C).
# A task that wakes ~20×/s is reliably the one Julia interrupts, so the spin task itself
# proactively and deterministically catches Ctrl-C and turns it into a drain that runs
# while the runtime is fully alive — never the exit→atexit teardown path, where the
# scheduler/libuv loop can deadlock. The poll also lets a programmatic `request_shutdown`
# (or a managed-node shutdown) wake us within one tick.
function _park_until_drained(ctx::Context)
    interrupted = false
    while (@atomic ctx._state) !== shutdown_done
        try
            sleep(0.05)
        catch err
            err isa InterruptException || rethrow()
            if !interrupted
                interrupted = true
                @info "interrupt — draining; interrupt again to force exit"
                request_shutdown(ctx; reason="SIGINT")
            else
                @warn "second interrupt — forcing exit"
                exit(130)
            end
        end
    end
    return nothing
end

# Signal handling scoped to the spin call: install handlers, run `f`, restore on return,
# so a library embedding ROSNode keeps its own handlers.
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

# Ctrl-C handling, uniform across REPL and deployed (`julia -m`/script): set
# `exit_on_sigint(false)` and let the polling `_park_until_drained` catch the
# `InterruptException` and drive a graceful drain while the runtime is alive. This avoids
# the exit→atexit teardown path (where the scheduler can deadlock) entirely, and needs no
# OS-specific signal handler. The atexit hook is kept only as a backstop for non-Ctrl-C
# exits (a normal return, an error, or SIGTERM) so a clean shutdown still always drains.
function _install_signal_handlers!(ctx::Context)
    _register_atexit_drain!(ctx)
    # `false`: deliver Ctrl-C as a catchable `InterruptException` rather than exiting the
    # process. The drain then runs from the spin task while the runtime is fully alive
    # (see `_park_until_drained`), avoiding the exit→atexit path whose teardown scheduler
    # can deadlock. The same setting works in the REPL (its default) and deployed.
    Base.exit_on_sigint(false)
    return nothing
end

# Restore the interactivity-appropriate `exit_on_sigint` default (no query API exists for the
# prior value): the REPL runs with it false, a deployed process with it true. The atexit hook
# can't be unregistered, but `close` is idempotent so a stale hook is a no-op.
_remove_signal_handlers!(::Context) = (Base.exit_on_sigint(!isinteractive()); nothing)

# One process-wide atexit hook drains every Context that registered for it. A `WeakKeyDict`
# lets a GC'd Context drop out (the hook holds no strong ref to it), and `close` is idempotent
# so a Context already drained by `spin`/`close` costs nothing here.
const _ATEXIT_DRAIN = WeakKeyDict{Context, Nothing}()
const _ATEXIT_DRAIN_LOCK = ReentrantLock()
const _ATEXIT_INSTALLED = Ref(false)

function _register_atexit_drain!(ctx::Context)
    @lock _ATEXIT_DRAIN_LOCK begin
        _ATEXIT_DRAIN[ctx] = nothing
        if !_ATEXIT_INSTALLED[]
            _ATEXIT_INSTALLED[] = true
            atexit(_drain_registered_at_exit)
        end
    end
    return nothing
end

function _drain_registered_at_exit()
    for c in (@lock _ATEXIT_DRAIN_LOCK collect(keys(_ATEXIT_DRAIN)))
        try
            close(c)
        catch err
            @error "atexit: draining Context failed" exception = (err, catch_backtrace())
        end
    end
    return nothing
end

# ── node ↔ context plumbing ────────────────────────────────────────────────
# The Node holds its Context; accept either a Context directly or anything
# exposing a `.context` field (the Node shape).

"Recover the owning `Context` from a node-or-context argument."
_ctx(ctx::Context) = ctx
_ctx(node) = hasproperty(node, :context) ? node.context :
             throw(ArgumentError("expected a Context or a Node holding one"))

# Forward declaration: the node layer's `struct Node` (included after this file) replaces
# this binding, letting the `Context`/`Node` vocabulary co-export and the duck-typed
# accessors above name it.
function Node end
