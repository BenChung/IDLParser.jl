# The Node type and the per-subscription concurrency policy. A `Node(ctx, "name";
# namespace)` inherits the Context's shared state, materializes a `ROSZenoh.NodeEntity`,
# and declares its node liveliness token. Every entity created on the node is tracked
# here and dies with it.
#
# Naming: `Publisher`/`Subscription`/`Service`/`Client` here are the `EndpointKind`
# enum instances re-exported by core.jl; the pattern layer (separate files) owns the
# user-facing constructors of the same spelling. This file stays kind-agnostic — it
# takes an `EndpointKind` tag and builds the wire entity from it.

using Zenoh: Zenoh, Keyexpr, Publisher as ZPublisher, SubscriberHandler,
             LivelinessToken, Sample, AbstractSample, SampleHolder, recv!,
             with_payload_memory, with_payload_memory_checked,
             Reliabilities,
             CacheOptions, MissDetectionOptions, HistoryOptions, RecoveryOptions,
             DetectionOptions
import Zenoh
using ROSZenoh: ROSZenoh, NodeEntity, EndpointEntity, EndpointKind, QosProfile,
                TypeInfo, TypeHash, default_qos, liveliness_keyexpr, topic_keyexpr,
                parse_topic_keyexpr, entity_gid, to_rihs_string,
                RmwZenoh, KeyExprFormat
# `Subscription` here doubles as the node-presence shell's kind tag and the
# dynamic-sub graph match; the same-spelled pattern-layer constructors plug into
# this file's `make_entity`.
using ROSZenoh: Subscription, Publisher

export Node, Entity, dispose

# ── Node ────────────────────────────────────────────────────────────────────

"""
    Node(ctx, name; namespace=nothing, enclave=nothing, serve_type_description=true,
         warmup=:precompile, warmup_sync=false)

A ROS 2 node sharing Context `ctx`'s Zenoh session. The node is the identity
under which publishers, subscriptions, services, clients, timers, and parameters
are created, and the naming root for relative/private name resolution; see
https://docs.ros.org/en/rolling/Concepts/Basic/About-Nodes.html. The parameter
server itself is attached by the schema form `Node(ctx, name, P)`, reached
as `node.parameters`; this two-positional-arg form leaves `node.parameters ===
nothing`.

`name` is the bare node name and must be non-empty. `namespace` defaults to the
Context's namespace; the fully-qualified name is `namespace/name`, validated at
construction. `enclave` defaults to the Context's enclave (SROS2 security enclave).

Construction allocates an entity id from `ctx`, builds the wire identity
(`ROSZenoh.NodeEntity`), declares the node's liveliness token so peers discover it
(the `NN` node-liveliness kind), and injects the node into the local
discovery index so self-queries see it immediately. The node is registered
with `ctx`, so a Context drain closes it. `serve_type_description=true` also
declares the `~/get_type_description` service (matching real ROS 2 nodes on Jazzy
and later) so peers can resolve the types this node advertises; a failure
declaring it is logged and construction continues.

`warmup`/`warmup_sync` set the node-default `WarmupPolicy` — the warm-up policy
(precompile/execute/off, sync/async) that pre-JITs the encode/decode dispatch
chain — for entities created on the node. Each entity constructor's own `warmup`/`warmup_sync`
keywords override per-endpoint.

Every entity created against the node is tracked and dies with it: `close(node)`
leaves the Context's sim-time set, closes each owned entity in reverse creation
order (failures logged, teardown continues), withdraws the node liveliness token,
and drops the node from the discovery index. `close` is idempotent and
`isopen(node)` reports liveness. Clock access (`clock(node)`, `now(node)`,
`Timer(node, …)`) reads through the node's per-source clock table, routed to
the Context's session or sim time source.

```julia
ctx = Context()
node = Node(ctx, "sensor_driver"; namespace="/robot1")
# node.fqn == "/robot1/sensor_driver"
close(node)
```
"""
mutable struct Node
    const context::Context
    const name::String
    const namespace::String              # normalized, leading-slash
    const fqn::String                    # namespace + "/" + name (resolution root)
    const entity::NodeEntity             # the wire identity (domain, z_id, id, …)
    const lv_key::String                 # node liveliness keyexpr (the index key)
    _lv_token::Any                       # Zenoh LivelinessToken; nothing once closed
    # Entities owned by this node (Entity handles). Closed in reverse on `close`.
    const entities::Vector{Any}
    # Per-node clock handles by source: `clock(node, C())` populates lazily.
    const clocks::Dict{DataType, Any}
    # Cross-entity mutual exclusion: a shared `ReentrantLock` for handlers
    # that touch shared node state.
    const lock::ReentrantLock
    # Static remap table (`--ros-args` name remapping).
    const remaps::Vector{Pair{String, String}}
    # /rosout + logging, lazily populated on first use (introspection.jl):
    _rosout_pub::Any                     # node-owned /rosout publisher, shared by every node logger
    _logger::Any                         # the default node `RosoutLogger`
    const _log_levels::Dict{String, Int32}  # per-logger-name min levels (LogLevel.level)
    # A `ParameterServer{P}` when the node is built via the schema form
    # `Node(ctx, name, P)`, else `nothing`. A plain field so `node.parameters` and the
    # atomic `open` read both reach Julia's default `getproperty`.
    parameters::Any
    # Node-default `WarmupPolicy` for entities on this node; an entity ctor's own
    # `warmup`/`warmup_sync` kwargs override per-endpoint.
    const warmup::WarmupPolicy
    @atomic open::Bool
end

function Node(ctx::Context, name::AbstractString;
              namespace::Union{AbstractString, Nothing}=nothing,
              enclave::Union{AbstractString, Nothing}=nothing,
              serve_type_description::Bool=true,
              warmup::Symbol=:precompile, warmup_sync::Bool=false)
    ns  = _normalize_namespace(namespace === nothing ? ctx.namespace : String(namespace))
    enc = enclave === nothing ? ctx.enclave : String(enclave)
    nm  = String(name)
    isempty(nm) && throw(ArgumentError("node name may not be empty"))
    fqn = _validate_fqn(_ns_join(ns, nm))

    nid = next_entity_id!(ctx)
    ent = NodeEntity(ctx.domain_id, ctx.z_id, nid, nm, ns, enc)
    lv_key = liveliness_keyexpr(ctx.format, ent)

    node = Node(ctx, nm, ns, fqn, ent, lv_key, nothing,
                Any[], Dict{DataType, Any}(), ReentrantLock(),
                Pair{String, String}[], nothing, nothing,
                Dict{String, Int32}(), nothing, WarmupPolicy(warmup, warmup_sync), true)

    # Declare the node's liveliness token (peers discover the node), then inject
    # it into our own index so self-queries see it immediately.
    node._lv_token = LivelinessToken(ctx.session, Keyexpr(lv_key))
    inject_endpoint!(ctx, lv_key, _node_endpoint(ent))

    # Track with the Context so a drain undeclares us.
    register_resource!(ctx, node)

    # Serve `~/get_type_description` so peers can resolve the types we advertise, as
    # real ROS 2 nodes (Jazzy+) do by default. Best-effort: a failure here must not
    # abort node creation.
    if serve_type_description
        try
            wire_get_type_description!(node)
        catch err
            @error "node: declaring ~/get_type_description failed" node=fqn exception=(err, catch_backtrace())
        end
    end
    return node
end

# The graph index keys on `EndpointInfo`, and a node token (`NN`) carries no
# endpoint (the wire side's `_ingest_liveliness!` skips `NodeEntity`). Our own
# node's presence is therefore a topic-less endpoint shell carrying just the node
# identity; `node_names` recovers (name, namespace) from it, with the kind
# immaterial (`Subscription` is an arbitrary tag).
_node_endpoint(ent::NodeEntity) =
    EndpointEntity(; id=ent.id, node=ent, kind=Subscription, topic="",
                   type_info=nothing, qos=default_qos())

Base.isopen(node::Node) = @atomic node.open

Base.show(io::IO, node::Node) =
    print(io, "Node(", node.fqn, isopen(node) ? "" : ", closed", ")")

"The Context backing this node."
context(node::Node) = node.context

# `_ctx`/`_node_clock` from context.jl/time.jl are duck-typed on `.context`/
# `.clocks` — a Node satisfies both, so `resolve_name(node, …)`, `clock(node)`,
# `now(node)`, `Timer(node, …)`, `on_shutdown(f, node)` all work unchanged.

"""
    close(node::Node)

Undeclare everything the node owns: close each entity in reverse
creation order, withdraw the node liveliness token, and drop the node from the
discovery index. Idempotent — a second close is a no-op. Entity `close` failures
are logged and teardown continues to the remaining entities.
"""
function Base.close(node::Node)
    (@atomicswap node.open = false) || return nothing   # single-winner close latch
    # Leave the Context's sim-time set (idempotent; tears down the `/clock` source
    # if this was the last user). Runs after marking the node closed so `_fire_jumps_to!`
    # (which guards on `isopen`) skips the deactivation jump — close delivers no callbacks.
    try
        set_use_sim_time!(node.context, node, false)
    catch err
        @error "close(node): sim-time deopt failed" exception=(err, catch_backtrace())
    end
    # Entities first (a sub/service may reference the node), reverse order. Snapshot
    # under the lock so a concurrent `make_entity`/`dispose` can't mutate mid-walk.
    ents = @lock node.lock (es = copy(node.entities); empty!(node.entities); es)
    for e in Iterators.reverse(ents)
        try
            close(e)
        catch err
            @error "close(node): closing entity failed" exception=(err, catch_backtrace())
        end
    end
    if node._lv_token !== nothing
        try
            close(node._lv_token)
        catch err
            @error "close(node): withdrawing liveliness token failed" exception=(err, catch_backtrace())
        end
        node._lv_token = nothing
    end
    remove_endpoint!(node.context, node.lv_key)
    nothing
end

# ── concurrency policy ────────────────────────────────────────────────────────
# The `Concurrency` type (core.jl) selects the dispatch strategy. A scheduler is a
# `thunk -> nothing` closure built once per subscription that decides where the
# per-sample work runs: `Serial()` runs it inline on the (sticky) consumer task —
# single OS thread, order preserved, no user-side locks; `Parallel(n)` runs it on
# an OS thread bounded to `n` in flight by a semaphore (the Zenoh FIFO buffers
# upstream when all `n` are busy, so QoS-depth backpressure still applies);
# `Parallel(Inf)` spawns unbounded.

_make_scheduler(::Serial) = thunk -> (thunk(); nothing)

function _make_scheduler(c::Parallel)
    if isfinite(c.n)
        sem = Base.Semaphore(Int(c.n))
        return function (thunk)
            Base.acquire(sem)
            Threads.@spawn try
                thunk()
            finally
                Base.release(sem)
            end
            nothing
        end
    else
        return thunk -> (Threads.@spawn thunk(); nothing)
    end
end

