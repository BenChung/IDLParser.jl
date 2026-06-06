# §6 Nodes & entities + §4 runtime. A `Node(ctx, "name"; namespace)` inherits the
# Context's shared state, materializes a `ROSZenoh.NodeEntity`, and declares its
# node liveliness token. Entities (pub/sub/service/client) are created with
# *type-constructors, not `open`* (the `Timer(f,…)` precedent, §6): a registered
# callback that lives until `close` and dies with the node. This file owns the
# Node type and the generic close-able `Entity` base the §6 pattern files plug
# into, plus the §4 subscription dispatch runtime.
#
# Naming: `Publisher`/`Subscription`/`Service`/`Client` are the `EndpointKind`
# *enum instances* re-exported by core.jl — the pattern layer (separate files)
# owns the user-facing constructors of the same spelling. This file stays kind-
# agnostic: it takes an `EndpointKind` tag and builds the wire entity from it.

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
# `Subscription`/`Publisher` are the `EndpointKind` enum instances: `Subscription`
# is reused as the node-presence shell's kind tag and the dynamic-sub graph match;
# the pub/sub/service/client *constructors* of the same spelling are the §6 pattern
# layer's, plugging into this file's `make_entity`.
using ROSZenoh: Subscription, Publisher

export Node, Entity, dispose

# ── Node ────────────────────────────────────────────────────────────────────
# A Node is the §6 identity object: it inherits the Context's session/z_id/domain
# and adds a name + namespace (the ROS naming root for relative/private resolution,
# §5). It builds a `ROSZenoh.NodeEntity` and declares the node's own liveliness
# token (`NN` kind, §2.2) so peers discover it. Every entity created on the node
# is tracked here and dies with it (close-on-close, §6).

"""
    Node(ctx, name; namespace=nothing, enclave=nothing)

A ROS2 node sharing Context `ctx`'s session (§6). `name` is the bare node name;
`namespace` defaults to the Context's (FQN = `namespace`/`name`). Constructing a
Node allocates an entity id, builds a `ROSZenoh.NodeEntity`, and declares the
node liveliness token so peers discover it.

Entities (publishers, subscriptions, services, …) are created against the node
with type-constructors and tracked here: `close(node)` undeclares its token and
every entity it owns. The Node is also registered with the Context so a drain
(§14) closes it.

A clock surface (`clock(node)`, `now(node)`, `Timer(node, …)`, §7) reads through
the node's `clocks` table, routed to the Context's session/sim source.
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
    # Per-node clock handles by source (§7): `clock(node, C())` populates lazily.
    const clocks::Dict{DataType, Any}
    # Cross-entity mutual exclusion (§4): a shared `ReentrantLock` for handlers
    # that touch shared node state. The callback-group abstraction is deferred.
    const lock::ReentrantLock
    # Static remap table (§5): empty until `--ros-args` parsing lands.
    const remaps::Vector{Pair{String, String}}
    # /rosout + logging (§13, D7), lazily populated on first use (introspection.jl):
    _rosout_pub::Any                     # node-owned /rosout publisher, shared by every node logger
    _logger::Any                         # the default node `RosoutLogger`
    const _log_levels::Dict{String, Int32}  # §7 per-logger-name min levels (LogLevel.level)
    # §10 parameters: a `ParameterServer{P}` when the node is built via the schema
    # form `Node(ctx, name, P)`, else `nothing`. Reached as `node.parameters` (a
    # plain field — no custom `getproperty`, so atomic `open` reads stay direct).
    parameters::Any
    # Default warm-up policy (§D8) for entities on this node; an entity ctor's own
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
    # it into our own index so self-queries see it immediately (§12 authoritative).
    node._lv_token = LivelinessToken(ctx.session, Keyexpr(lv_key))
    inject_endpoint!(ctx, lv_key, _node_endpoint(ent))

    # Track with the Context so a drain undeclares us (§14, step 4).
    register_resource!(ctx, node)

    # Serve `~/get_type_description` so peers can resolve the types we advertise
    # (§13 / D5 S3) — real ROS2 nodes (Jazzy+) serve it by default too. Best-effort:
    # a failure here must not abort node creation. Forward reference into
    # introspection.jl (included later in the module).
    if serve_type_description
        try
            wire_get_type_description!(node)
        catch err
            @error "node: declaring ~/get_type_description failed" node=fqn exception=(err, catch_backtrace())
        end
    end
    return node
end

# The graph index keys on `EndpointInfo`, but a node token (`NN`) carries no
# endpoint — the wire side skips it (`_ingest_liveliness!` ignores `NodeEntity`).
# So our own node's presence is injected as a topic-less endpoint shell carrying
# just the node identity; `node_names` (§12) recovers (name, namespace) from it,
# and the kind is immaterial there (we use `Subscription` as an arbitrary tag).
_node_endpoint(ent::NodeEntity) =
    EndpointEntity(; id=ent.id, node=ent, kind=Subscription, topic="",
                   type_info=nothing, qos=default_qos())

Base.isopen(node::Node) = @atomic node.open

Base.show(io::IO, node::Node) =
    print(io, "Node(", node.fqn, isopen(node) ? "" : ", closed", ")")

"The Context backing this node (§5/§6)."
context(node::Node) = node.context

# `_ctx`/`_node_clock` from context.jl/time.jl are duck-typed on `.context`/
# `.clocks` — a Node satisfies both, so `resolve_name(node, …)`, `clock(node)`,
# `now(node)`, `Timer(node, …)`, `on_shutdown(f, node)` all work unchanged.

"""
    close(node::Node)

Undeclare everything the node owns (§6/§14): close each entity in reverse
creation order, withdraw the node liveliness token, and drop the node from the
discovery index. Idempotent — a second close is a no-op. Entity `close` failures
are logged so one can't abort the rest of the teardown.
"""
function Base.close(node::Node)
    (@atomicswap node.open = false) || return nothing
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
    # Withdraw the node token + drop from the index.
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

# ── concurrency policy (§4, D2/D3) ────────────────────────────────────────────
# The `Concurrency` type (core.jl) selects the dispatch strategy. A *scheduler* is
# a `thunk -> nothing` closure built once per subscription that decides *where* the
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

