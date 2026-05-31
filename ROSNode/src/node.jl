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
             LivelinessToken, Sample, with_memory, unsafe_memory, Reliabilities
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
    @atomic open::Bool
end

function Node(ctx::Context, name::AbstractString;
              namespace::Union{AbstractString, Nothing}=nothing,
              enclave::Union{AbstractString, Nothing}=nothing,
              serve_type_description::Bool=true)
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
                Pair{String, String}[], true)

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

# ── the generic Entity handle (§6) ────────────────────────────────────────────
# One close-able handle backing every pattern (publisher/subscription/service/
# client). It allocates the entity id, builds the `ROSZenoh.EndpointEntity`,
# declares the liveliness token, and — for the data-plane kinds — declares the
# data route (a `Zenoh.Publisher` for Publisher, a FIFO `SubscriberHandler` +
# consumer task for Subscription). Service/Client carry the entity + token only;
# their Zenoh queryable/querier wiring is the §8 layer's, attached via `wire`.
#
# The pattern files don't subtype this — they *hold* an `Entity` and add their
# typed surface (message type, handler, result cells). This keeps the id/token/
# route/graph lifecycle in exactly one place.

"""
    Entity

The generic close-able endpoint handle behind every §6 pattern. Owns the
`ROSZenoh.EndpointEntity` (id + kind + topic + type + QoS), its liveliness token,
and its data route (a Zenoh `Publisher` or a `SubscriberHandler` + consumer task,
depending on `kind`). Created via [`make_entity`](@ref); `close` undeclares the
route, withdraws the token, and drops the endpoint from the graph.

Pattern types (the publisher/subscription/service objects) *hold* an `Entity`
rather than subtype it — the id/token/route/graph lifecycle lives here once.
"""
mutable struct Entity
    const node::Node
    const endpoint::EndpointEntity       # the wire entity (id, kind, topic, …)
    const lv_key::String                 # liveliness keyexpr (graph index key)
    const gid::NTuple{16, UInt8}         # source_gid for attachments (§3.4)
    _lv_token::Any                       # Zenoh LivelinessToken
    # The data route: a `Zenoh.Publisher` (Publisher), a `SubscriberHandler`
    # (Subscription, the FIFO channel sub), or `nothing` (Service/Client — their
    # queryable/querier is wired by §8 and stored in `wire`).
    _route::Any
    # The subscription consumer task (Subscription only); `nothing` otherwise.
    _consumer::Union{Task, Nothing}
    # Slot for the pattern layer to stash its kind-specific wiring (queryable,
    # querier, pending-reply table) so it shares this handle's lifecycle.
    wire::Any
    @atomic open::Bool
end

"""
    make_entity(node, kind, topic, type_info, qos; consumer=nothing) -> Entity

Build and register an endpoint on `node` (§6). Allocates an entity id from the
Context, constructs the `ROSZenoh.EndpointEntity`, declares its liveliness token,
injects it into the discovery index (authoritative, §12), and tracks it on the
node so `close(node)` reaps it. The caller (a §6 pattern) attaches the data route
afterward via [`declare_publisher!`](@ref) / [`declare_subscription!`](@ref) or
its own queryable/querier into `entity.wire`.

`type_info` may be `nothing` (the `EMPTY_TOPIC_TYPE` token form). `qos` defaults
to `default_qos()`.
"""
function make_entity(node::Node, kind::EndpointKind, topic::AbstractString,
                     type_info::Union{TypeInfo, Nothing}=nothing;
                     qos::QosProfile=default_qos())
    isopen(node) || throw(ArgumentError("cannot create an entity on a closed node"))
    ctx = node.context
    eid = next_entity_id!(ctx)
    endpoint = EndpointEntity(; id=eid, node=node.entity, kind=kind,
                              topic=String(topic), type_info=type_info, qos=qos)
    lv_key = liveliness_keyexpr(ctx.format, endpoint)
    gid = entity_gid(endpoint)

    ent = Entity(node, endpoint, lv_key, gid, nothing, nothing, nothing,
                 nothing, true)
    # Declare liveliness first (peers discover us), then inject locally so our own
    # graph queries are immediately authoritative (§12).
    ent._lv_token = LivelinessToken(ctx.session, Keyexpr(lv_key))
    inject_endpoint!(ctx, lv_key, endpoint)

    @lock node.lock push!(node.entities, ent)
    return ent
end

Base.isopen(e::Entity) = @atomic e.open
Base.show(io::IO, e::Entity) =
    print(io, "Entity(", e.endpoint.kind, " ", e.endpoint.topic,
          isopen(e) ? "" : ", closed", ")")

"The 16-byte `source_gid` to stamp on this entity's `put`/`reply` attachments (§3.4)."
gid(e::Entity) = e.gid

# ── data routes (§6) ──────────────────────────────────────────────────────────

"""
    declare_publisher!(entity; kwargs...) -> Zenoh.Publisher

Declare the publish-side data route for a Publisher `entity`: a long-lived Zenoh
`Publisher` on the topic keyexpr (`topic_keyexpr`, §2.2), with QoS mapped onto
the Zenoh publisher options (reliability today; full mapping is a ROSZenoh TODO).
Stored on the entity and returned. The §6 publisher pattern calls this, then
publishes via `put(route, payload; attachment=…)`.
"""
function declare_publisher!(e::Entity; congestion_control=nothing, priority=nothing)
    ctx = e.node.context
    tk = topic_keyexpr(ctx.format, e.endpoint)
    route = ZPublisher(ctx.session, Keyexpr(tk);
                       reliability=_zenoh_reliability(e.endpoint.qos),
                       congestion_control=congestion_control, priority=priority)
    e._route = route
    return route
end

"""
    declare_subscription!(entity, msgtype, handler; view=false, concurrency=Serial())

Declare the subscribe-side data route for a Subscription `entity` and start its
dispatch runtime (§4). Opens a FIFO-channel Zenoh subscriber on the topic keyexpr
sized to the QoS history depth (`KeepLast(N)`→capacity N, the backpressure buffer
that lets a blocking handler keep buffering, §2.3), then spawns the consumer task
that decodes each sample to `msgtype` and runs `handler(msg)` per the concurrency
policy. Returns the entity.

`view=true` runs the handler over a zero-copy `CDRView` aliasing the payload
(§3.2, valid only for the handler's duration); the default materializes an owned
message.
"""
function declare_subscription!(e::Entity, msgtype::Type, handler;
                               view::Bool=false, concurrency::Concurrency=Serial())
    ctx = e.node.context
    tk = topic_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    sub = Base.open(ctx.session, Keyexpr(tk); channel=:fifo, capacity=cap)
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency)
    return e
end

"The data-route keyexpr for a pub/sub entity (`topic_keyexpr`, §2.2)."
topic_key(e::Entity) = topic_keyexpr(e.node.context.format, e.endpoint)

# Map QoS history → FIFO subscriber capacity (§2.3): KeepLast(N)→N, KeepAll→a
# large bound (libzenohc has no true unbounded channel; size generously so the
# common burst doesn't drop). Depth ≤ 0 is coerced to 1.
function _fifo_capacity(qos::QosProfile)
    qos.history === :keep_all && return 1024
    max(1, qos.depth)
end

# QoS reliability → Zenoh reliability singleton. The other policies (durability/
# deadline/lifespan/liveliness) are not Zenoh-transport concepts; they ride the
# liveliness token (QoS encoding) and are enforced at our layer (§12.3).
_zenoh_reliability(qos::QosProfile) =
    qos.reliability === :best_effort ? Reliabilities.BEST_EFFORT : Reliabilities.RELIABLE

# ── subscription dispatch runtime (§4) ────────────────────────────────────────
# The consumer task iterates the FIFO `SubscriberHandler` (blocking on the libuv
# thread until a sample arrives or the channel disconnects on close) and runs the
# handler per the concurrency policy. `Serial()` runs inline on the sticky consumer
# task — one at a time, order preserved, single OS thread; `Parallel(n)` spawns up
# to `n` handlers on OS threads. A handler throw is logged, never fatal: one bad
# message must not kill the subscription.

function _spawn_consumer(e::Entity, msgtype::Type, handler, view::Bool,
                         concurrency::Concurrency)
    sched = _make_scheduler(concurrency)
    # Sticky task: the consumer (and, under `Serial()`, the handler it runs inline)
    # stays on the thread that declared the subscription. So a node's Serial
    # handlers all run cooperatively on one OS thread — never simultaneous, no data
    # races, no user-side locks (D3) — even under `julia -t N`. `Parallel(n)` opts
    # individual handlers off this thread via the scheduler's `@spawn`.
    t = Task() do
        try
            for sample in e._route::SubscriberHandler
                if view
                    # View path: the borrow must scope to the dispatch, so decode
                    # happens *at the handler* under `with_memory` (D1). The sample
                    # rides to the handler (inline Serial / worker Parallel); the
                    # for-loop binds `sample` fresh per iteration, so the closure
                    # captures the right one.
                    sched(() -> _invoke_view(e, sample, msgtype, handler))
                else
                    # Owned path: decode on the consumer task (D1) — the handler
                    # receives an already-materialized message, so a slow handler
                    # never delays the next decode and decode stays off the worker.
                    msg = _decode_on_consumer(e, sample, msgtype)
                    msg === nothing && continue
                    sched(() -> _invoke_owned(e, msg, handler))
                end
            end
        catch err
            # A closed subscriber ends iteration cleanly (CHANNEL_DISCONNECTED →
            # `nothing`); a genuine transport error or a shutdown lands here.
            err isa ShutdownException && return
            isopen(e) &&
                @error "subscription consumer task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    t.sticky = true
    schedule(t)
    return t
end

# Owned decode on the consumer task (D1). Returns the materialized message, or
# `nothing` if decode threw — logged and skipped, so a malformed sample never kills
# the consumer loop. (No ROS message type is `Nothing`, so the sentinel is safe.)
function _decode_on_consumer(e::Entity, sample::Sample, msgtype::Type)
    try
        return decode(sample, msgtype; view=false)
    catch err
        err isa ShutdownException && rethrow()
        @error "subscription decode failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        return nothing
    end
end

# Run the handler on an already-decoded owned message; a throw is logged, never
# fatal (one bad message must not kill the subscription).
function _invoke_owned(e::Entity, msg, handler)
    try
        handler(msg)
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# View path: borrow the payload for the call via `with_memory` and decode a
# `CDRView` aliasing it — escaping the scope is a `BorrowError`, not a
# use-after-free (§3.2). Decode is at the dispatch so the borrow scopes here (D1).
function _invoke_view(e::Entity, sample::Sample, msgtype::Type, handler)
    try
        with_memory(sample, UInt8) do b
            handler(decode(unsafe_memory(b), msgtype; view=true))
        end
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# ── dynamic (keyexpr-only) subscription dispatch (§11/D5 S5) ───────────────────
# A subscription created without a compile-time type. The route is a *wildcard*
# data keyexpr matching every type on the topic; the type rides in each sample's
# keyexpr. Per sample we recover `(name, hash)`, resolve_or_discover the real
# runtime type `T`, then cross the world-age boundary with `invokelatest` for both
# decode and the handler call (`T` is born at runtime, §11). Owned-only — the
# min-copy `view` path is the static fast path you graduate to.
#
# Two-stage pipeline (D8 — decouple reception from codegen). The first sight of a
# type runs `realize!` → `Core.eval` (heavy-allocating codegen that triggers GC).
# If that ran on the task owning the blocking `z_recv`, the FIFO would stop draining
# mid-codegen; a libzenohc foreign thread mid-handoff would never reach a Julia
# safepoint, and the GC the codegen needs could never complete — a hang (the JIT
# racing Zenoh). So we split:
#   • a *receiver* task (sticky, owns the recv) does nothing but copy bytes and
#     enqueue them — it stays parked in the recv and keeps the FIFO drained, so
#     foreign threads always reach safepoints;
#   • a *worker* task (a plain `@spawn`) drains the buffer and does resolve →
#     codegen → dispatch. Its GC can now complete (the receiver is parked in a
#     GC-safe recv and draining), breaking the deadlock at the source.
# This holds even single-threaded: Zenoh.jl's FIFO recv is a `@threadcall` (the
# blocking C recv runs on a libuv pool thread; the Julia task yields), so the worker
# is scheduled even with `-t1`. While the worker is mid-`Core.eval` (no yield points)
# the libuv thread keeps draining Zenoh's ring — Zenoh QoS buffers upstream — and
# Julia resumes the receiver when codegen yields. `-t>=2` just adds true parallelism
# (worker codegen overlapping reception); it isn't required for correctness.
# The buffer is sized to the QoS depth: a slow first-sight realize backs samples up
# to that bound, then the receiver blocks on `put!` (real backpressure upstream),
# never a silent drop and never a process-wide hang (the recoverable soft-lock).

# Strip exactly one leading/trailing slash (mirrors ROSZenoh's topic stripping) so
# we can match a parsed-keyexpr topic (no slashes) against a graph FQN topic.
function _strip_one_slash_local(s::AbstractString)
    s = startswith(s, "/") ? SubString(s, nextind(s, 1)) : SubString(s, 1)
    s = endswith(s, "/")   ? SubString(s, 1, prevind(s, lastindex(s))) : s
    return String(s)
end

# The wildcard data keyexpr for a dynamic subscription. Under rmw_zenoh the data
# key is `<domain>/<topic>/<type…>/<hash>`, so `**` matches the whole type+hash
# tail — every type published on the topic routes to us. Other formats (ros2dds)
# carry no type in the key, so the plain topic key already matches all data and the
# per-sample type is recovered from the graph instead.
function _wildcard_data_keyexpr(::RmwZenoh, ep::EndpointEntity)
    ep.node === nothing && throw(ArgumentError("dynamic subscription requires a node"))
    return string(ep.node.domain_id, "/", _strip_one_slash_local(ep.topic), "/**")
end
_wildcard_data_keyexpr(fmt::KeyExprFormat, ep::EndpointEntity) = topic_keyexpr(fmt, ep)

"""
    declare_subscription!(entity, handler; concurrency=Serial())

The no-`msgtype` method of [`declare_subscription!`](@ref): declare a *type-less*
(dynamic, §11/D5 S5) subscription route — a FIFO Zenoh subscriber on the wildcard
data keyexpr (so samples of any type on the topic arrive), plus a consumer that
per-sample resolves the type and dispatches via `invokelatest`. Contrast the typed
method `declare_subscription!(entity, msgtype, handler; …)` which decodes a fixed
`T` directly.
"""
function declare_subscription!(e::Entity, handler;
                               concurrency::Concurrency=Serial())
    ctx = e.node.context
    ke = _wildcard_data_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    sub = Base.open(ctx.session, Keyexpr(ke); channel=:fifo, capacity=cap)
    e._route = sub
    e._consumer = _spawn_dynamic_consumer(e, handler, concurrency)
    return e
end

function _spawn_dynamic_consumer(e::Entity, handler, concurrency::Concurrency)
    sched = _make_scheduler(concurrency)
    logged = Set{Tuple{String, TypeHash}}()      # S6: log each discovered type once
    loglk = ReentrantLock()
    # The buffer between the (sticky) Zenoh receiver and the resolve/codegen/dispatch
    # worker. Bounded to the QoS depth so it mirrors the FIFO's own backpressure: a
    # slow first-sight realize fills it to `cap`, then the receiver blocks on `put!`
    # (upstream backpressure), never silently dropping. Closed by the receiver's
    # `finally` so the worker drains-then-exits on teardown.
    cap = _fifo_capacity(e.endpoint.qos)
    buf = Channel{Tuple{String, Vector{UInt8}}}(cap)

    # Stage 2 — worker. A plain `@spawn` (NOT sticky): its codegen GC completes while
    # the receiver sits parked in the (GC-safe, yielding `@threadcall`) recv (D8).
    # Drains the buffer in order (so `Serial()` order is preserved), resolving +
    # dispatching each item via `_dynamic_dispatch`. Runs even under `-t1` — the recv
    # yields, so the scheduler reaches the worker; `-t>=2` adds true overlap.
    worker = Threads.@spawn try
        for (ke, bytes) in buf
            _dynamic_dispatch(e, ke, bytes, handler, sched, logged, loglk)
        end
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "dynamic subscription worker task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end

    # Stage 1 — receiver. Owns the blocking `z_recv`; copies the payload + keyexpr
    # (D1: never pin the sample past the recv) and enqueues. Does nothing heavy, so
    # it returns to `recv` promptly and keeps the FIFO drained — the property that
    # lets the worker's GC complete. Closing the route ends this loop; `finally`
    # closes the buffer so the worker exits.
    t = Task() do
        try
            for sample in e._route::SubscriberHandler
                ke = string(Zenoh.keyexpr(sample))
                p = Zenoh.payload(sample)
                p === nothing && continue
                bytes = _own_bytes(p)
                try
                    put!(buf, (ke, bytes))
                catch err
                    # Buffer closed (teardown raced the route close) — stop cleanly.
                    err isa InvalidStateException && break
                    rethrow()
                end
            end
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @error "dynamic subscription receiver task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        finally
            close(buf)            # signal the worker to drain-and-exit
        end
    end
    t.sticky = true
    schedule(t)
    # `worker` needs no explicit handle: the scheduler roots it while it runs, and the
    # receiver's `finally close(buf)` (on route close) ends its drain loop cleanly.
    return t
end

# An owned `Vector{UInt8}` copy of a payload — independent of the sample's lifetime
# (the worker may run after the sample is freed under `Parallel`).
function _own_bytes(p)
    m = Zenoh.as_memory(p, UInt8)
    owned = Vector{UInt8}(undef, length(m))
    @inbounds copyto!(owned, m)
    return owned
end

# Resolve the sample's type on the worker task (a fast registry hit after first
# sight; the first sample of a new type pays discovery + codegen here — possibly a
# blocking wire call, HOL-blocking this sub only, by design, and off the receiver's
# `z_recv` thread so it can't stall delivery, D8). Then cross into the new world
# **once** via `invokelatest` into `_run_typed_dynamic`: the decode, the concurrency
# dispatch (incl. the `@spawn` for `Parallel`), and the handler all run as compiled
# new-world code from there — so the per-message hot path crosses the world-age
# boundary exactly once, not once per call.
function _dynamic_dispatch(e::Entity, ke::AbstractString, bytes::Vector{UInt8},
                           handler, sched, logged, loglk)
    try
        info = _sample_type_info(e, ke)
        info === nothing && return
        T = resolve_or_discover(e.node, info.name, info.hash)
        if T === nothing
            @warn "dynamic subscription: could not resolve type" topic=e.endpoint.topic type=info.name hash=to_rihs_string(info.hash) maxlog=1
            return
        end
        _log_discovered_once(e, info, logged, loglk)
        Base.invokelatest(_run_typed_dynamic, T, e, bytes, handler, sched)
    catch err
        err isa ShutdownException && return
        @error "dynamic subscription resolve failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# The new-world worker (reached only via `invokelatest`, since `T` is runtime-born):
# dispatch the decode+handler per the concurrency policy. Because we are already in
# the latest world here, the `@spawn` inside `sched` (Parallel) captures it, so the
# spawned task runs `handler(decode(bytes, T))` natively — the whole hot loop body
# is compiled new-world code, no further `invokelatest`.
function _run_typed_dynamic(::Type{T}, e::Entity, bytes::Vector{UInt8},
                            handler, sched) where {T}
    sched() do
        try
            handler(decode(bytes, T; view=false))
        catch err
            err isa ShutdownException && return
            @error "dynamic subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    nothing
end

# The TypeInfo for a received sample: from the data keyexpr (rmw_zenoh embeds it),
# falling back to the graph (ros2dds / empty-type keys carry no type identity).
function _sample_type_info(e::Entity, ke::AbstractString)
    ctx = e.node.context
    parsed = try
        parse_topic_keyexpr(ctx.format, ke)
    catch err
        @debug "dynamic sub: unparseable data keyexpr" ke exception=err
        return nothing
    end
    parsed.type_info === nothing || return parsed.type_info
    return _graph_type_for_topic(ctx, parsed.topic)
end

# A publisher's advertised TypeInfo for `topic` (stripped form), from the discovery
# index — the type source when the data keyexpr carries none.
function _graph_type_for_topic(ctx, topic::AbstractString)
    for ep in endpoints_snapshot(ctx)
        ep.type === nothing && continue
        ep.kind === Publisher || continue
        _strip_one_slash_local(ep.topic) == topic || continue
        return ep.type
    end
    return nothing
end

# Log a discovered (name, hash) once per subscription (§S6): tell the user what to
# write to graduate to the static fast path.
function _log_discovered_once(e::Entity, info::TypeInfo, logged, loglk)
    isnew = @lock loglk (info.name, info.hash) in logged ? false :
                        (push!(logged, (info.name, info.hash)); true)
    isnew || return nothing
    bare = split(info.name, '/')[end]
    @info "dynamic subscription discovered a type — for the min-copy fast path, \
           rewrite to the static form" topic=e.endpoint.topic type=info.name hash=to_rihs_string(info.hash) graduate="Subscription(node, \"$(e.endpoint.topic)\", $(bare))"
    nothing
end

# ── entity teardown (§6/§14) ──────────────────────────────────────────────────

"""
    close(entity::Entity)

Undeclare the entity (§6): close the data route (which stops the consumer task's
iteration for a Subscription), withdraw the liveliness token, drop it from the
discovery index, and detach any pattern-layer wiring (`entity.wire`). Idempotent.
"""
function Base.close(e::Entity)
    (@atomicswap e.open = false) || return nothing
    ctx = e.node.context

    # Close the data route first: undeclaring the subscriber disconnects its FIFO
    # channel, so the consumer task's `for sample in …` loop terminates cleanly.
    if e._route !== nothing
        try
            close(e._route)
        catch err
            @error "close(entity): closing data route failed" exception=(err, catch_backtrace())
        end
        e._route = nothing
    end

    # Pattern-layer wiring (queryable / querier / pending tables) shares our
    # lifecycle; close it if it's close-able.
    if e.wire !== nothing
        try
            applicable(close, e.wire) && close(e.wire)
        catch err
            @error "close(entity): closing pattern wiring failed" exception=(err, catch_backtrace())
        end
        e.wire = nothing
    end

    # Withdraw liveliness + drop from the index.
    if e._lv_token !== nothing
        try
            close(e._lv_token)
        catch err
            @error "close(entity): withdrawing liveliness token failed" exception=(err, catch_backtrace())
        end
        e._lv_token = nothing
    end
    remove_endpoint!(ctx, e.lv_key)
    nothing
end

"""
    dispose(node, entity::Entity)

Close `entity` and remove it from `node`'s tracked set (the explicit early-close
path, vs. waiting for `close(node)`). A no-op if `entity` isn't owned by `node`.
"""
function dispose(node::Node, e::Entity)
    close(e)
    @lock node.lock begin
        i = findfirst(===(e), node.entities)
        i === nothing || deleteat!(node.entities, i)
    end
    nothing
end
