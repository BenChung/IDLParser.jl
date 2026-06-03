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
                Dict{String, Int32}(), WarmupPolicy(warmup, warmup_sync), true)

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
    # D4 re-latch thunk (transient_local Subscription only): undeclare + redeclare
    # the advanced subscriber (re-runs the history query), novelty-gated. Called by
    # `_relatch!(e)` on a managed node's Inactive→Active transition; `nothing` for
    # every other entity (volatile subs, publishers, services).
    _relatch::Union{Function, Nothing}
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
                 nothing, nothing, true)
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

# ── data routes (§6) + transient_local advanced pub/sub (D4) ───────────────────
# `DURABILITY_TRANSIENT_LOCAL` (latched/cached delivery to late joiners) rides
# Zenoh's advanced pub/sub, byte/protocol-compatible with rmw_zenoh/hiroz: the
# publisher keeps a sample cache, the subscriber issues a history query on join.
# `volatile` (the default) takes neither — the plain `Publisher`/`open` route,
# zero overhead. Zenoh.jl routes the plain constructors to the advanced variants
# transparently when an advanced option keyword is present, so these builders just
# decide *which* keywords to pass (empty NamedTuple ⇒ plain).

const _DEFAULT_HISTORY_DEPTH = 42          # RMW_ZENOH_DEFAULT_HISTORY_DEPTH (KeepAll)
const _ADVANCED_HEARTBEAT_MS = 500         # rmw_zenoh sample-miss / recovery heartbeat
# History-query timeout for a joining subscriber. hiroz uses effectively-∞; we pick
# a generous finite bound so latched state from a momentarily-slow publisher still
# arrives, without wedging forever. (Open: byte-verify against an rmw_zenoh capture.)
const _ADVANCED_QUERY_TIMEOUT_MS = 60_000

# Cache/history sample count for a transient_local endpoint: the QoS depth, or the
# rmw_zenoh default for KeepAll (no true-unbounded cache). Mirrors `_fifo_capacity`.
_cache_depth(qos::QosProfile) =
    qos.history === :keep_all ? _DEFAULT_HISTORY_DEPTH : max(1, qos.depth)

# Advanced *publisher* keywords for a transient_local publisher (empty ⇒ plain):
# a sample cache sized to depth, liveliness detection, plus a periodic heartbeat
# for reliable (sample-miss detection — matches hiroz; best-effort omits it).
function _advanced_pub_kwargs(qos::QosProfile)
    qos.durability === :transient_local || return NamedTuple()
    base = (cache = CacheOptions(max_samples = _cache_depth(qos)),
            detection = DetectionOptions())
    qos.reliability === :reliable ?
        merge(base, (miss_detection =
            MissDetectionOptions(heartbeat = :periodic, period_ms = _ADVANCED_HEARTBEAT_MS),)) :
        base
end

# Advanced *subscriber* keywords for a transient_local subscriber (empty ⇒ plain):
# history replay on join (back-filling late publishers too), a bounded query
# timeout, liveliness detection, plus gap recovery for reliable.
function _advanced_sub_kwargs(qos::QosProfile)
    qos.durability === :transient_local || return NamedTuple()
    base = (history = HistoryOptions(detect_late_publishers = true,
                                     max_samples = _cache_depth(qos)),
            query_timeout_ms = _ADVANCED_QUERY_TIMEOUT_MS,
            detection = DetectionOptions())
    qos.reliability === :reliable ?
        merge(base, (recovery =
            RecoveryOptions(periodic_queries_period_ms = _ADVANCED_HEARTBEAT_MS),)) :
        base
end

# ── D4 novelty gate (transient_local re-latch dedup) ───────────────────────────
# A latched `transient_local` subscription that re-runs its history query (on a
# managed node's Inactive→Active, `_do_relatch!`) is redelivered cached samples it
# may already have processed. Replaying an effectful handler (replan, re-arm) on an
# unchanged value is the hazard D4 names. The gate suppresses it.
#
# Why we key on the *payload content-hash*, not the attachment `(gid, seq)` of §3.4:
# Zenoh's advanced-pubsub cache serves history replies as **bare payloads** — neither
# our per-message attachment nor a sample timestamp survives the cache round-trip
# (verified: `z_sample_attachment`/`z_sample_timestamp` are null on replays). The
# payload is the only thing that does, and for a latched *state* topic it is exactly
# the right key: an unchanged value is identical bytes (suppress), an updated value is
# different bytes (deliver) — D4's table verbatim.
#
#   • `delivered` — a bounded FIFO of recently-delivered payload hashes. Capped at the
#     cache depth, which is the *most* the publisher can ever replay, so an evicted
#     hash can't reappear — bounded memory with no correctness loss (and no leak on a
#     high-cardinality topic). Pushed only on delivery (NOT receipt): a sample dropped
#     by the inactive-window lifecycle gate doesn't record, so a value updated while
#     inactive still reads as novel on reactivation.
#   • `snapshot` — frozen at each re-latch (`_arm_relatch!`) to a copy of `delivered`.
#     A replayed sample whose hash ∈ snapshot is one we've already delivered → suppress;
#     a new hash is a genuine update → deliver. `nothing` (no re-latch yet) ⇒ deliver
#     everything — the plain non-lifecycle path is unchanged.
#   • `force` — escape hatch: deliver on every activation regardless, for idempotent
#     handlers that deliberately rebuild state from latched inputs.
#
# Invariant (D4): after reactivation the node's view is indistinguishable from one
# that stayed Active, save a gap where it processed nothing — effects fire once,
# possibly later, never twice. (Edge case, accepted: two publishers emitting byte-
# identical state are deduped across sources, and a live re-send of a byte-identical
# pre-relatch value is suppressed — both correct for idempotent state.)
mutable struct _NoveltyGate
    lock::ReentrantLock
    cap::Int
    delivered::Vector{UInt}                       # recent delivered payload hashes (≤cap, FIFO)
    snapshot::Union{Vector{UInt}, Nothing}
    force::Bool
end
_NoveltyGate(cap::Integer; force::Bool=false) =
    _NoveltyGate(ReentrantLock(), max(1, Int(cap)), UInt[], nothing, force)

# Freeze the recently-delivered hashes as the next replay-suppression baseline.
_arm_relatch!(g::_NoveltyGate) = @lock g.lock (g.snapshot = copy(g.delivered))

# Decide+record one sample by its payload hash: suppress a re-latch replay (hash in
# the frozen snapshot, unless `force`), else deliver and record (bounded FIFO).
function _gate_deliver!(g::_NoveltyGate, h::UInt)
    @lock g.lock begin
        !g.force && g.snapshot !== nothing && (h in g.snapshot) && return false
        push!(g.delivered, h)
        length(g.delivered) > g.cap && popfirst!(g.delivered)
        return true
    end
end

# The payload content-hash used by the gate (survives the advanced-cache replay that
# strips attachment + timestamp). One `as_memory` copy of the payload — paid only for
# transient_local subs, which are low-rate latched state.
_payload_hash(sample::AbstractSample) = hash(Zenoh.as_memory(Zenoh.payload(sample), UInt8))

"""
    declare_publisher!(entity; kwargs...) -> AbstractPublisher

Declare the publish-side data route for a Publisher `entity`: a long-lived Zenoh
publisher on the topic keyexpr (`topic_keyexpr`, §2.2), with QoS mapped onto the
Zenoh publisher options (reliability; `transient_local` ⇒ an `AdvancedPublisher`
with a sample cache, D4). Stored on the entity and returned (its concrete type
flows into `PublisherHandle{T,R}` for a type-stable `put`). The §6 publisher
pattern calls this, then publishes via `put(route, payload; attachment=…)`.
"""
function declare_publisher!(e::Entity; congestion_control=nothing, priority=nothing)
    ctx = e.node.context
    tk = topic_keyexpr(ctx.format, e.endpoint)
    route = ZPublisher(ctx.session, Keyexpr(tk);
                       reliability=_zenoh_reliability(e.endpoint.qos),
                       congestion_control=congestion_control, priority=priority,
                       _advanced_pub_kwargs(e.endpoint.qos)...)
    e._route = route
    return route
end

"""
    declare_subscription!(entity, msgtype, handler; view=Owned(), concurrency=Serial())

Declare the subscribe-side data route for a Subscription `entity` and start its
dispatch runtime (§4). Opens a FIFO-channel Zenoh subscriber on the topic keyexpr
sized to the QoS history depth (`KeepLast(N)`→capacity N, the backpressure buffer
that lets a blocking handler keep buffering, §2.3), then spawns the consumer task
that decodes each sample to `msgtype` and runs `handler(msg)` per the concurrency
policy. Returns the entity.

`view` is the [`ViewMode`](@ref): `Owned()` (default) materializes an owned
message; `Checked()`/`Unchecked()` deliver a zero-copy `CDRView` aliasing the
payload (valid only for the handler's duration, §3.2), guarded vs. bare. `true`
(⇒ `Checked()`) and `false` (⇒ `Owned()`) are accepted as shorthand.
"""
function declare_subscription!(e::Entity, msgtype::Type, handler;
                               view::Union{Bool, ViewMode}=Owned(),
                               concurrency::Concurrency=Serial(),
                               force_relatch::Bool=false)
    view = _view_mode(view)
    ctx = e.node.context
    tk = topic_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    qos = e.endpoint.qos
    # transient_local ⇒ an AdvancedSubscriber whose declaration issues a history
    # query (latched delivery); volatile ⇒ the plain FIFO subscriber. The D4
    # novelty gate is created only for the latched case (deduplicates re-latch
    # replays); a `nothing` gate is the volatile fast path (no per-sample work).
    sub = Base.open(ctx.session, Keyexpr(tk); channel=:fifo, capacity=cap,
                    _advanced_sub_kwargs(qos)...)
    gate = qos.durability === :transient_local ?
        _NoveltyGate(_cache_depth(qos); force=force_relatch) : nothing
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency, sub, gate)
    # The re-latch thunk (D4): a managed node's Inactive→Active redeclares the
    # advanced subscriber to re-run the history query, novelty-gated. Only meaningful
    # for transient_local — volatile subs leave `_relatch` nothing.
    if gate !== nothing
        e._relatch = () -> _do_relatch!(e, msgtype, handler, view, concurrency, gate, tk, cap, qos)
    end
    return e
end

# D4 re-latch: redeclare the advanced subscriber so its history query re-fires,
# recovering latched state the node missed while inactive. The novelty gate makes the
# redundant redelivery safe — a replayed value already delivered is dropped, only
# genuine updates fire (D4 effectful-replay fix).
#
# Order matters: tear down + **join** the old consumer *first*, so any sample it was
# about to deliver (e.g. the declaration-time history reply that arrived once the gate
# opened) is recorded, THEN arm the snapshot to capture it, THEN open the new route and
# re-query. Arming before the join would race that delivery — the old consumer could
# deliver a value after the snapshot froze, and the re-query would then deliver it a
# second time (the value isn't yet in the snapshot it's checked against).
function _do_relatch!(e::Entity, msgtype::Type, handler, view::ViewMode,
                      concurrency::Concurrency, gate::_NoveltyGate,
                      tk::AbstractString, cap::Integer, qos::QosProfile)
    isopen(e) || return nothing
    old_route, old_consumer = e._route, e._consumer
    if old_route !== nothing
        try; close(old_route); catch err
            @error "relatch: closing old route failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    # Join the old consumer so its in-flight deliveries land (and record in the gate)
    # before we snapshot. Its loop ends when the route's FIFO disconnects. Skip only if
    # we're somehow on that very task.
    if old_consumer isa Task && old_consumer !== current_task()
        try; wait(old_consumer); catch; end
    end
    _arm_relatch!(gate)        # freeze the now-complete delivered set as the baseline
    sub = Base.open(e.node.context.session, Keyexpr(tk); channel=:fifo, capacity=cap,
                    _advanced_sub_kwargs(qos)...)
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency, sub, gate)
    return nothing
end

"""
    _relatch!(e::Entity)

Re-run a transient_local subscription's latched-history fetch (D4), if it has one.
A no-op for any entity without a re-latch thunk (volatile subs, publishers,
services). Called on a managed node's Inactive→Active transition (lifecycle.jl).
"""
_relatch!(e::Entity) = (e._relatch === nothing ? nothing : (e._relatch(); nothing))

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
# The consumer task iterates the FIFO subscriber (blocking on the libuv thread until
# a sample arrives or the channel disconnects on close) and runs the handler per the
# concurrency policy. `Serial()` runs inline on the sticky consumer task — one at a
# time, order preserved, single OS thread; `Parallel(n)` spawns up to `n` handlers on
# OS threads. A handler throw is logged, never fatal: one bad message must not kill
# the subscription.

# Per-sample gating before delivery: the §14.2 lifecycle gate (drop while the node
# is inactive — no novelty side effect, so the delivered stream reflects only what
# the handler saw) and the D4 novelty gate (suppress re-latch replays). Returns
# whether to deliver. Volatile subs pass `gate === nothing` and skip the attachment
# decode entirely — the common fast path.
@inline function _predispatch(e::Entity, sample::AbstractSample, gate::Union{_NoveltyGate, Nothing})
    isactive(e) || return false               # §14.2 gate: drop while inactive (no record)
    gate === nothing && return true           # volatile fast path: no novelty work
    return _gate_deliver!(gate, _payload_hash(sample))   # D4: suppress re-latch replays
end

# `sub` is the concretely-typed route (plain `SubscriberHandler` or the advanced
# `AdvancedSubscriberHandler`); the consumer closes over it and iterates the typed
# local, so dispatch stays monomorphic regardless of which the QoS selected. (We do
# NOT read `e._route::Any` here — an abstract-typed iteration would box every sample;
# the bounded `S` recovers the type the erased field can't.)
# `T` is threaded as a type parameter (not a `::Type` value) so the per-message
# decode/dispatch specializes: `decode(sample, T)` is return-type-stable, the
# decoded message isn't boxed as `Any`, and the handler call is concrete. The
# call from `declare_subscription!` (where `msgtype::Type` is a value) resolves
# `T` with one dynamic dispatch per *subscription* — the per-*message* path
# inside the workers is monomorphic.
function _spawn_consumer(e::Entity, ::Type{T}, handler, view::ViewMode,
                         concurrency::Concurrency, sub::S,
                         gate::Union{_NoveltyGate, Nothing}) where {T, S<:Zenoh.AbstractSubscriberHandler}
    # Persistent worker pool — no per-message task spawn. Each worker owns a
    # reusable `SampleHolder` and runs its own recv!→dispatch loop, pulling
    # concurrently from `sub` (recv! is mutex-serialized). Returns one task handle
    # for `e._consumer` (the D4 re-latch `wait`s on it); workers exit when the
    # subscriber closes (recv! → nothing).
    if concurrency isa Serial
        # One sticky worker: ordered, pinned to the declaring thread (D3) so a
        # node's Serial handlers share one OS thread — no races, no user locks.
        w = Task(() -> _consume_loop(e, T, handler, view, sub, gate))
        w.sticky = true
        schedule(w)
        return w
    elseif concurrency isa Parallel && !isfinite(concurrency.n)
        return _spawn_unbounded_consumer(e, T, handler, view, sub, gate)
    end
    # Parallel(n): n non-sticky workers (run on OS threads, order not preserved),
    # wrapped by a sticky supervisor that joins them so `wait(e._consumer)`
    # transitively waits for every worker on close / re-latch.
    nworkers = Int(concurrency.n)
    sup = Task() do
        workers = map(1:nworkers) do _
            w = Task(() -> _consume_loop(e, T, handler, view, sub, gate))
            w.sticky = false
            schedule(w)
            w
        end
        for w in workers
            wait(w)
        end
    end
    sup.sticky = true
    schedule(sup)
    return sup
end

# The single decode→dispatch leaf, shared by the static consumer (`_consume_loop`
# and the `Parallel(Inf)` *view* path) and the dynamic worker (`_run_typed_dynamic`).
# Given a concrete `T`, a live `sample`/holder, the handler, and the `ViewMode`, it
# borrows the payload and runs the handler on the decoded message — ONE
# implementation, so the static and dynamic paths can't drift. Type-stable: `T` is
# concrete and `decode_view`/`decode_owned` infer concrete (the `CDR_LE` reader).
# A handler/decode throw is logged and swallowed so one bad message never kills the
# subscription; `ShutdownException` ends the dispatch quietly.
@inline function _dispatch_decoded(e::Entity, sample::AbstractSample, ::Type{T},
                                   handler, view::ViewMode) where {T}
    try
        _with_node_logger(e.node) do          # D7: handler logs → node's /rosout
            _run(sample, T, handler, view)
        end
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# Delivery strategies, dispatched on the (singleton) `ViewMode` so the choice is
# static per subscription. All borrow the payload (no pre-copy) and run the handler
# within the borrow scope (§3.2).
#   Owned     — materialize a self-contained `T` from the borrowed bytes (safe to
#               keep / forward / spawn).
#   Checked   — zero-copy `CDRView` + runtime escape guard: a `CDRView`/`CDRString`
#               used after the handler returns throws `BorrowError` instead of
#               reading freed memory. Allocates the guard — the validation tier.
#   Unchecked — bare isbits `PayloadView`: zero-copy, zero-alloc, no checks (an
#               escaping view is UB). The production tier; validate with `Checked`.
@inline _run(sample, ::Type{T}, handler, ::Owned) where {T} =
    with_payload_memory(sample) do mem
        handler(decode_owned(mem, T))
    end
@inline _run(sample, ::Type{T}, handler, ::Checked) where {T} =
    with_payload_memory_checked(sample) do mem
        handler(decode_view(mem, T))
    end
@inline _run(sample, ::Type{T}, handler, ::Unchecked) where {T} =
    with_payload_memory(sample) do mem
        handler(decode_view(mem, T))
    end

# Owned decode on the consumer task, returning the materialized message (or
# `nothing` if decode threw — logged and skipped). Used only by the `Parallel(Inf)`
# owned path, which must decode *inline* over its reused holder (before the next
# `recv!` overwrites it) and then spawn only the handler on the owned result.
# Borrow-based: materializes straight from the borrowed bytes, no payload pre-copy.
function _decode_on_consumer(e::Entity, sample::AbstractSample, ::Type{T}) where {T}
    try
        return with_payload_memory(sample) do mem
            decode_owned(mem, T)
        end
    catch err
        err isa ShutdownException && rethrow()
        @error "subscription decode failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        return nothing
    end
end

# Run the handler on an already-decoded owned message; a throw is logged, never
# fatal (one bad message must not kill the subscription). The `Parallel(Inf)` owned
# path spawns this after an inline `_decode_on_consumer`.
function _invoke_owned(e::Entity, msg, handler)
    try
        # D7: a plain `@info` inside the handler routes to this node's /rosout.
        _with_node_logger(e.node) do
            handler(msg)
        end
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# One worker's receive loop. Owns a `SampleHolder` and runs the full
# recv!→gate→decode→dispatch pipeline itself. N of these pull concurrently from
# the same subscriber — `recv!` is `uv_mutex`-serialized, so each gets a distinct
# sample, and the view path borrows the worker's *own* live holder (zero-copy,
# valid for the whole dispatch). The novelty gate is `ReentrantLock`-guarded, so
# concurrent delivery is safe. Decode runs here, on the worker (D1): with N
# workers a slow handler in one never stalls another's decode.
function _consume_loop(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    holder = SampleHolder()
    try
        while (sample = recv!(sub, holder)) !== nothing
            _predispatch(e, sample, gate) || continue
            # Inline (the holder is live for the whole dispatch) — both view and
            # owned go through the one shared leaf.
            _dispatch_decoded(e, sample, T, handler, view)
        end
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "subscription consumer task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# `Parallel(Inf)`: unbounded concurrency can't be a fixed pool, so keep the
# spawn-per-message model (its per-message task-launch cost is the honest price
# of "no ceiling"). A single receiver pulls and spawns a handler per message;
# the view path receives into a *fresh* holder the spawned worker owns until GC
# (still zero-copy), the owned path decodes inline and spawns on the message.
function _spawn_unbounded_consumer(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    reused = SampleHolder()
    t = Task() do
        try
            while true
                h = _is_view(view) ? SampleHolder() : reused
                recv!(sub, h) === nothing && break
                _predispatch(e, h, gate) || continue
                if _is_view(view)
                    # Fresh holder per message → the spawned task owns it and can
                    # borrow it for the full dispatch via the shared leaf.
                    Threads.@spawn _dispatch_decoded(e, h, T, handler, view)
                else
                    # Owned must decode INLINE over the reused holder (before the
                    # next recv! overwrites it); spawn only the handler on the owned
                    # result.
                    msg = _decode_on_consumer(e, h, T)
                    msg === nothing && continue
                    Threads.@spawn _invoke_owned(e, msg, handler)
                end
            end
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @error "subscription consumer task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    t.sticky = true
    schedule(t)
    return t
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
                               view::Union{Bool, ViewMode}=Owned(),
                               concurrency::Concurrency=Serial(),
                               warmup::WarmupPolicy=e.node.warmup)
    view = _view_mode(view)
    ctx = e.node.context
    ke = _wildcard_data_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    sub = Base.open(ctx.session, Keyexpr(ke); channel=:fifo, capacity=cap)
    e._route = sub
    e._consumer = _spawn_dynamic_consumer(e, handler, view, concurrency, warmup)
    return e
end

# A recycle pool of `SampleHolder`s for the dynamic receiver→worker handoff, so we
# don't allocate one per message. Size = `cap` (buffered) + in-flight decodes + 1
# (the receiver filling one). `nothing` for `Parallel(Inf)`: unbounded concurrent
# decodes can't be bounded by a fixed pool, so that mode keeps a fresh holder per
# message. The finalizer on each holder stays as a shutdown-time backstop — pooled
# holders are reused, so it ~never fires per message.
function _make_holder_pool(concurrency::Concurrency, cap::Integer)
    concurrency isa Parallel && !isfinite(concurrency.n) && return nothing
    inflight = concurrency isa Parallel ? Int(concurrency.n) : 1
    sz = Int(cap) + inflight + 1
    pool = Channel{SampleHolder}(sz)
    for _ in 1:sz
        put!(pool, SampleHolder())
    end
    return pool
end

function _spawn_dynamic_consumer(e::Entity, handler, view::ViewMode, concurrency::Concurrency,
                                 warmup::WarmupPolicy)
    sched = _make_scheduler(concurrency)
    logged = Set{Tuple{String, TypeHash}}()      # S6: log each discovered type once
    loglk = ReentrantLock()
    warmed = Set{DataType}()                      # §D8: warm each runtime type once at first sight
    warmlk = ReentrantLock()                       # §D9: `warmed` is now shared with the Tier-1 replay task
    # The buffer between the (sticky) Zenoh receiver and the resolve/codegen/dispatch
    # worker. Bounded to the QoS depth so it mirrors the FIFO's own backpressure: a
    # slow first-sight realize fills it to `cap`, then the receiver blocks on `put!`
    # (upstream backpressure), never silently dropping. Closed by the receiver's
    # `finally` so the worker drains-then-exits on teardown.
    cap = _fifo_capacity(e.endpoint.qos)
    # Carry an *owned* sample (a refcount bump, no payload copy) from receiver to
    # worker, not a copied byte buffer — the worker decodes by borrowing it. The
    # owned holder may sit in the buffer / be captured by a Parallel decode thunk
    # without pinning the ring (recv! moved it out of the ring slot), so D1 holds.
    buf = Channel{SampleHolder}(cap)
    # Recycle the holders instead of allocating one per message: receiver takes a
    # free holder, the decode thunk returns it after use. Sized for everything that
    # can pin a holder at once — `cap` in the buffer + the in-flight decodes + the
    # one the receiver is filling. `nothing` for `Parallel(Inf)` (unbounded in-flight
    # can't be pooled → fall back to a fresh holder per message).
    pool = _make_holder_pool(concurrency, cap)
    # Stage 2 — worker. A plain `@spawn` (NOT sticky): its codegen GC completes while
    # the receiver sits parked in the (GC-safe, yielding `@threadcall`) recv (D8).
    # Drains the buffer in order (so `Serial()` order is preserved) via the
    # world-age trampoline (`_dynamic_worker`), which hoists the `invokelatest`
    # from per-message to per-type. Runs even under `-t1` — the recv yields, so the
    # scheduler reaches the worker; `-t>=2` adds true overlap.
    worker = Threads.@spawn try
        _dynamic_worker(e, buf, handler, sched, view, pool, logged, loglk, warmed, warmlk, warmup)
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "dynamic subscription worker task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end

    # Stage 1 — receiver. `recv!`s each sample into a fresh owned `SampleHolder`
    # (moved out of the ring slot, so the ring isn't pinned — D1) and enqueues it.
    # No payload copy and no keyexpr String: the holder carries the refcounted
    # payload *and* keyexpr to the worker, which decodes + resolves the type by
    # borrowing them. Stays light, so it keeps the
    # ring drained while the worker codegens (D8). recv! returns nothing on close →
    # loop ends; `finally` closes the buffer so the worker drains-then-exits.
    t = Task() do
        route = e._route::SubscriberHandler
        try
            while true
                h = pool === nothing ? SampleHolder() : take!(pool)
                if recv!(route, h) === nothing                # subscriber closed
                    pool === nothing || put!(pool, h)
                    break
                end
                if Zenoh.payload(h) === nothing               # e.g. a delete/tombstone
                    pool === nothing || put!(pool, h)         # recycle, don't enqueue
                    continue
                end
                # No `string(keyexpr(h))` here: the owned holder carries the keyexpr,
                # so the worker reads it by borrowing (hash + memcmp cache hit), and
                # only a cold first-sight miss materializes the String.
                try
                    put!(buf, h)
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
    # §D9 — Tier-1 startup warm: replay this node's interaction manifest for this
    # topic, warming the codecs of types it used on prior runs *before* the first
    # message arrives (shifting the JIT off the hot path). Runs concurrently with the
    # receiver/worker; the shared `warmlk`-guarded `warmed` set makes the two paths
    # idempotent (whichever warms a type first, the other skips it). Skipped when
    # warm-up is off.
    warmup.mode === :off ||
        _spawn_manifest_warm(e, handler, warmed, warmlk, warmup)
    # `worker` needs no explicit handle: the scheduler roots it while it runs, and the
    # receiver's `finally close(buf)` (on route close) ends its drain loop cleanly.
    return t
end

# §D9 — schedule the Tier-1 manifest replay. `sync` runs it inline at construction
# (hard real-time: first message guaranteed warm); otherwise a plain `Threads.@spawn`
# (NOT sticky) — in cache-only mode the replay's `resolve_or_discover` may run
# `realize!`→`Core.eval` codegen, whose GC must complete while the node's other
# consumers keep safepoints reachable (the JIT-races-Zenoh discipline the dynamic
# worker also follows, §D8). With `@ros_cache` baked, the resolve is a registry hit
# and no codegen runs.
function _spawn_manifest_warm(e::Entity, handler, warmed, warmlk, warmup::WarmupPolicy)
    if warmup.sync
        _replay_manifest_warm(e, handler, warmed, warmlk)
    else
        Threads.@spawn try
            _replay_manifest_warm(e, handler, warmed, warmlk)
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @warn "dynamic manifest warm-up failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    nothing
end

# Resolve + warm each manifest entry for this subscription's topic. `wire=false`: a
# node coming up must not block on a remote `~/get_type_description` — a genuinely-new
# type still pays one reactive wire fetch on its first message, by design (§D9). A
# stale entry whose type no longer resolves (cache evicted, version bump) returns
# `nothing` and is skipped. Crosses to the runtime-born type's warm anchor via
# `invokelatest` (newer world age), exactly as the worker does.
function _replay_manifest_warm(e::Entity, handler, warmed, warmlk)
    for it in load_manifest(e.node.fqn)
        it.role === :subscription || continue
        it.topic == e.endpoint.topic || continue
        T = resolve_or_discover(e.node, it.name, it.hash; wire=false)
        T === nothing && continue
        isnew = @lock warmlk (T in warmed ? false : (push!(warmed, T); true))
        isnew || continue
        try
            Base.invokelatest(_warm_dynamic, T, handler)
        catch err
            err isa ShutdownException && rethrow()
            @warn "dynamic manifest warm-up: warming $(it.name) failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    nothing
end

# World-age trampoline for the dynamic worker. The per-message hot path must NOT
# cross the world-age boundary (you can't specialize through `invokelatest`), so
# instead of `invokelatest`-ing every message we hoist the crossing to once per
# *type*: `_dynamic_worker` runs a flat loop of `invokelatest` entries into
# `_drain_known`, which dispatches every already-seen type with a plain (cached)
# dynamic call — no `invokelatest` — and only returns when it meets a *new* type
# (whose `realize!` may have bumped the world). The outer loop then re-enters in
# the fresher world. Steady state = one long-lived `_drain_known` whose inner
# loop never returns ⇒ zero per-message `invokelatest`; the crossing count is
# ~one per distinct type (+ the once-per-type `_warm_dynamic`).
#
# Invariant: a key in `cache` was first seen in a prior incarnation that
# re-entered past its `realize!` world; worlds are monotonic, so it is ≤ this
# incarnation's world ⇒ `_run_typed_dynamic{T}` is directly callable. A single
# worker drains `buf` in order across re-entries, so `Serial` total order holds.
# `realize!` still runs here (off the receiver) so the FIFO keeps draining (D8).

# Incarnation counter — bumped once per `_drain_known` entry (i.e. once per
# re-entry), so ≈ distinct-types + 1, never per message. Observability + a test
# hook for "steady-state dispatch isn't re-`invokelatest`ing"; the increment is
# off the per-message path, so it costs nothing hot.
const _DYNAMIC_REENTRIES = Threads.Atomic{Int}(0)

# Keyexpr cache key: a hash of the borrowed keyexpr bytes, so the hot path never
# allocates a `String` (§3 alloc cleanup). FNV-1a over the raw bytes — allocation-
# free, no `unsafe_wrap`. Collisions are resolved by a `memcmp` against the stored
# bytes (`_ke_bytes_eq`), so the hash needn't be cryptographic, only well-spread.
@inline function _ke_fnv1a(ptr::Ptr{UInt8}, len::Int)
    h = 0xcbf29ce484222325 % UInt
    for i in 1:len
        h = (h ⊻ unsafe_load(ptr, i)) * (0x00000100000001b3 % UInt)
    end
    return h
end

# memcmp the borrowed keyexpr (ptr,len) against a stored byte copy — the collision
# check that makes the hash cache exact.
@inline function _ke_bytes_eq(ptr::Ptr{UInt8}, len::Int, b::Vector{UInt8})
    length(b) == len || return false
    return GC.@preserve b (ccall(:memcmp, Cint,
        (Ptr{UInt8}, Ptr{UInt8}, Csize_t), ptr, pointer(b), len) == 0)
end

function _dynamic_worker(e::Entity, buf, handler, sched, view::ViewMode, pool,
                         logged, loglk, warmed, warmlk, warmup::WarmupPolicy)
    # hash(keyexpr bytes) → (resolved type, keyexpr bytes for memcmp verify);
    # worker-private (no lock). The byte copy is the collision-safety net.
    cache   = Dict{UInt, Tuple{Type, Vector{UInt8}}}()
    pending = nothing                     # just-realized item to dispatch first in the fresh world
    while true
        pending = Base.invokelatest(_drain_known, e, buf, handler, sched, view, pool, cache,
                                    logged, loglk, warmed, warmlk, warmup, pending)::Union{Symbol, Nothing, Tuple{SampleHolder, Type}}
        pending === :closed && break
    end
    nothing
end

function _drain_known(e::Entity, buf, handler, sched, view::ViewMode, pool,
                      cache::Dict{UInt, Tuple{Type, Vector{UInt8}}},
                      logged, loglk, warmed, warmlk, warmup::WarmupPolicy, pending)
    Threads.atomic_add!(_DYNAMIC_REENTRIES, 1)
    if pending !== nothing
        (h, T) = pending
        _run_typed_dynamic(T, e, h, handler, sched, view, pool) # direct: realized ≤ our world
    end
    for h in buf
        # Resolve the cached type by borrowing the keyexpr bytes (no String): hash
        # them, look up, and `memcmp`-verify against the stored copy so a hash
        # collision can't mis-dispatch. Returns the cached `T` or `nothing`.
        T = Zenoh.keyexpr_view(h) do ptr, len
            entry = get(cache, _ke_fnv1a(ptr, len), nothing)
            (entry !== nothing && _ke_bytes_eq(ptr, len, entry[2])) ? entry[1] : nothing
        end
        if T !== nothing
            _run_typed_dynamic(T, e, h, handler, sched, view, pool)  # HOT PATH — direct, no invokelatest
            continue
        end
        # First sight of this keyexpr by this worker: materialize the String (cold,
        # once per type) and resolve (the first sample of a new type pays discovery +
        # codegen here, off the receiver, D8 — HOL-blocking this sub only). Then hand
        # the item back so the outer loop re-enters in the post-`realize!` world, where
        # `T` is directly callable for every later message.
        ke = string(Zenoh.keyexpr(h))
        try
            info = _sample_type_info(e, ke)
            if info === nothing
                pool === nothing || put!(pool, h)              # not dispatched → recycle the holder
                continue
            end
            Tnew = resolve_or_discover(e.node, info.name, info.hash)
            if Tnew === nothing
                @warn "dynamic subscription: could not resolve type" topic=e.endpoint.topic type=info.name hash=to_rihs_string(info.hash) maxlog=1
                pool === nothing || put!(pool, h)              # not dispatched → recycle the holder
                continue                                       # unresolved: skip, no cache, no re-entry
            end
            # Key on the same FNV-1a the hot path computes; stash the bytes for memcmp.
            kebytes = Vector{UInt8}(codeunits(ke))
            cache[GC.@preserve kebytes _ke_fnv1a(pointer(kebytes), length(kebytes))] = (Tnew, kebytes)
            _log_discovered_once(e, info, logged, loglk)
            # §D8/§D9: warm this runtime type's codec once (off the recv thread); `T`
            # is runtime-born so reach the warm anchor via `invokelatest`. `warmed` is
            # shared with the Tier-1 replay task, so the once-guard is under `warmlk`.
            if warmup.mode !== :off
                isnew = @lock warmlk (Tnew in warmed ? false : (push!(warmed, Tnew); true))
                if isnew
                    note_interaction!(e.node.fqn, :subscription, info.hash, info.name, e.endpoint.topic)
                    try
                        Base.invokelatest(_warm_dynamic, Tnew, handler)
                    catch err
                        @warn "dynamic warm-up failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
                    end
                end
            end
            return (h, Tnew)                # hand off; outer loop re-enters in the fresh world
        catch err
            err isa ShutdownException && rethrow()
            @error "dynamic subscription resolve failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
            pool === nothing || put!(pool, h)                  # not dispatched → recycle the holder
            continue
        end
    end
    return :closed
end

# The new-world worker (reached only via `invokelatest`, since `T` is runtime-born):
# dispatch the decode+handler per the concurrency policy. Because we are already in
# the latest world here, the `@spawn` inside `sched` (Parallel) captures it, so the
# spawned task runs `handler(decode(bytes, T))` natively — the whole hot loop body
# is compiled new-world code, no further `invokelatest`.
function _run_typed_dynamic(::Type{T}, e::Entity, h::SampleHolder,
                            handler, sched, view::ViewMode, pool) where {T}
    # Dispatch inside the sched thunk, which captures the owned holder `h` — so under
    # `Parallel` the spawned worker keeps the payload alive while the shared leaf
    # borrows + decodes it. The thunk returns `h` to the pool when done (after the
    # deferred decode under Parallel), recycling it for the next receive. The
    # decode→dispatch itself is the same `_dispatch_decoded` leaf the static consumer
    # runs — one implementation, no static/dynamic drift.
    sched() do
        try
            _dispatch_decoded(e, h, T, handler, view)
        finally
            pool === nothing || put!(pool, h)
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
