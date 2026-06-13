# ── the generic Entity handle ─────────────────────────────────────────────────
# One close-able handle backing every endpoint pattern (publisher/subscription/
# service/client). It allocates the entity id, builds the `ROSZenoh.EndpointEntity`,
# declares the liveliness token, and — for the data-plane kinds — declares the
# data route (a `Zenoh.Publisher` for Publisher, a FIFO-channel subscriber +
# consumer task for Subscription). Service/Client carry the entity + token only;
# their Zenoh queryable/querier wiring is the service/client pattern layer's,
# attached via `wire`.
#
# Pattern files hold an `Entity` and add their typed surface (message type,
# handler, result cells), keeping the id/token/route/graph lifecycle in one place.

"""
    Entity

The generic close-able endpoint handle behind every endpoint pattern (publisher,
subscription, service, client). It owns the wire entity (`ROSZenoh.EndpointEntity`:
id, kind, topic, type info, QoS), the entity's liveliness token, the 16-byte
`source_gid` stamped on its attachments, and its data route:

- Publisher: a Zenoh publisher.
- Subscription: a FIFO-channel Zenoh subscriber (the advanced variant for
  transient_local, whose declaration issues the latched-history query) plus a
  consumer task.
- Service/Client: `nothing` — its queryable/querier is wired by the pattern
  layer into the `wire` slot.

An `Entity` is built and registered by `make_entity`, which allocates the
id, declares the liveliness token, and tracks the entity on its node; the pattern
layer then attaches the data route. Pattern types (the user-facing publisher/
subscription/service/client objects) hold an [`Entity`](@ref) so the id/token/route/graph
lifecycle lives in one place.

`close(entity)` undeclares the route (which terminates a Subscription's consumer
task), detaches any pattern-layer wiring in `wire`, withdraws the liveliness
token, and drops the endpoint from the discovery index; it is idempotent, and
`isopen(entity)` reports liveness. A transient_local subscription's Entity also
carries a re-latch thunk, fired on a managed node's [`Inactive`](@ref)→[`Active`](@ref)
transition to re-run its latched-history query, deduplicated against
already-delivered samples.

A Subscription Entity's per-sample delivery is the skip-delivery point of the
managed-node dispatch gate ([`isactive`](@ref)): a sample is dropped unless the
[`Node`](@ref) is [`Active`](@ref). Use [`dispose`](@ref) to release a single
entity before its node closes.
"""
mutable struct Entity
    const node::Node
    const endpoint::EndpointEntity       # the wire entity (id, kind, topic, …)
    const lv_key::String                 # liveliness keyexpr (graph index key)
    const gid::NTuple{16, UInt8}         # source_gid for attachments
    _lv_token::Any                       # Zenoh LivelinessToken
    # The data route: a `Zenoh.Publisher` (Publisher), a FIFO-channel subscriber
    # handler — advanced variant under transient_local — (Subscription), or
    # `nothing` (Service/Client — their queryable/querier is wired by the
    # service/client pattern layer and stored in `wire`).
    _route::Any
    # The subscription consumer task (Subscription only); `nothing` otherwise.
    _consumer::Union{Task, Nothing}
    # Re-latch thunk (transient_local Subscription only): undeclare + redeclare
    # the advanced subscriber (re-runs the history query), novelty-gated. Called by
    # `_relatch!(e)` on a managed node's Inactive→Active transition; `nothing` for
    # every other entity (volatile subs, publishers, services).
    _relatch::Union{Function, Nothing}
    # Slot for the pattern layer to stash its kind-specific wiring (queryable,
    # querier, pending-reply table) so it shares this handle's lifecycle.
    wire::Any
    # Weak-static Subscription: a typed sub that wildcard-matches the topic and
    # runs the per-sample `check_sample_type` backstop (drop + `on_type_mismatch` on a
    # name/hash mismatch). Set by `declare_subscription!` (match=:weak); false for every
    # other entity. Written once before the consumer starts, so no atomic is needed.
    _weak_static::Bool
    # message-lost: set true the first time an `on_message_lost` listener
    # registers (graph.jl). The consumer hot path reads it per sample to decide
    # whether to do the attachment-sequence bookkeeping (`note_sequence!`); the
    # common no-listener case is one atomic load, no decode, no alloc.
    @atomic _track_lost::Bool
    @atomic open::Bool
end

"""
    make_entity(node, kind, topic, type_info=nothing; qos=default_qos()) -> Entity

Build and register an endpoint on `node`. Allocates an entity id from the
[`Context`](@ref), constructs the `ROSZenoh.EndpointEntity`, declares its liveliness
token, injects it into the discovery index (authoritative), and tracks it on the
node so `close(node)` reaps it. The caller (an endpoint pattern) attaches the data
route afterward via [`declare_publisher!`](@ref) / [`declare_subscription!`](@ref) or
its own queryable/querier into `entity.wire`.

`type_info` may be `nothing` (a type-less endpoint). `qos` defaults
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
                 nothing, nothing, false, false, true)
    # Declare liveliness first (peers discover us), then inject locally so our own
    # graph queries are immediately authoritative.
    ent._lv_token = LivelinessToken(ctx.session, Keyexpr(lv_key))
    inject_endpoint!(ctx, lv_key, endpoint)

    @lock node.lock push!(node.entities, ent)
    return ent
end

Base.isopen(e::Entity) = @atomic e.open
Base.show(io::IO, e::Entity) =
    print(io, "Entity(", e.endpoint.kind, " ", e.endpoint.topic,
          isopen(e) ? "" : ", closed", ")")

"The 16-byte `source_gid` to stamp on this entity's `put`/`reply` attachments."
gid(e::Entity) = e.gid

# ── data routes + transient_local advanced pub/sub ────────────────────────────
# `DURABILITY_TRANSIENT_LOCAL` (latched/cached delivery to late joiners) rides
# Zenoh's advanced pub/sub, byte/protocol-compatible with rmw_zenoh/hiroz: the
# publisher keeps a sample cache, the subscriber issues a history query on join.
# `volatile` (the default) uses the plain `Publisher`/`open` route at zero overhead.
# Zenoh.jl routes the plain constructors to the advanced variants transparently when
# an advanced option keyword is present, so these builders just decide which keywords
# to pass (empty NamedTuple ⇒ plain).

const _DEFAULT_HISTORY_DEPTH = 42          # RMW_ZENOH_DEFAULT_HISTORY_DEPTH (KeepAll)
const _ADVANCED_HEARTBEAT_MS = 500         # rmw_zenoh sample-miss / recovery heartbeat
# History-query timeout for a joining subscriber: a generous finite bound so latched
# state from a momentarily-slow publisher still arrives, without wedging forever
# (hiroz uses effectively-∞).
const _ADVANCED_QUERY_TIMEOUT_MS = 60_000

# Cache/history sample count for a transient_local endpoint: the QoS depth, or the
# rmw_zenoh default for KeepAll (no true-unbounded cache). Mirrors `_fifo_capacity`.
_cache_depth(qos::QosProfile) =
    qos.history === :keep_all ? _DEFAULT_HISTORY_DEPTH : max(1, qos.depth)

# Advanced publisher keywords for a transient_local publisher (empty ⇒ plain):
# a sample cache sized to depth, liveliness detection, plus a periodic heartbeat
# for reliable sample-miss detection (best-effort omits it).
function _advanced_pub_kwargs(qos::QosProfile)
    qos.durability === :transient_local || return NamedTuple()
    base = (cache = CacheOptions(max_samples = _cache_depth(qos)),
            detection = DetectionOptions())
    qos.reliability === :reliable ?
        merge(base, (miss_detection =
            MissDetectionOptions(heartbeat = :periodic, period_ms = _ADVANCED_HEARTBEAT_MS),)) :
        base
end

# Advanced subscriber keywords for a transient_local subscriber (empty ⇒ plain):
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

# ── novelty gate (transient_local re-latch dedup) ──────────────────────────────
# A latched `transient_local` subscription that re-runs its history query (on a
# managed node's Inactive→Active, `_do_relatch!`) is redelivered cached samples it
# may already have processed. Replaying an effectful handler (replan, re-arm) on an
# unchanged value is the hazard. The gate suppresses it.
#
# The gate keys on the payload content-hash because Zenoh's advanced-pubsub cache
# serves history replies as bare payloads: `z_sample_attachment`/`z_sample_timestamp`
# are null on replays, so the attachment `(gid, seq)` and the sample timestamp
# are both gone, and the payload is all that survives the cache round-trip. For a
# latched state topic the payload is exactly the right key: an unchanged value is
# identical bytes (suppress), an updated value is different bytes (deliver).
#
#   • `delivered` — a bounded FIFO of recently-delivered payload hashes. Capped at the
#     cache depth, the maximum the publisher can ever replay, so an evicted hash can't
#     reappear: bounded memory with no correctness loss (and no leak on a high-cardinality
#     topic). Recorded on delivery, not receipt, so a sample the inactive-window lifecycle
#     gate drops still reads as novel on reactivation.
#   • `snapshot` — frozen at each re-latch (`_arm_relatch!`) to a copy of `delivered`.
#     A replayed sample whose hash ∈ snapshot is one we've already delivered → suppress;
#     a new hash is a genuine update → deliver. `nothing` (no re-latch yet) ⇒ deliver
#     everything — the plain non-lifecycle path is unchanged.
#   • `force` — escape hatch: deliver on every activation regardless, for idempotent
#     handlers that deliberately rebuild state from latched inputs.
#
# Invariant: after reactivation the node's view is indistinguishable from one
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
publisher on the topic keyexpr (`topic_keyexpr`), with QoS mapped onto the
Zenoh publisher options (reliability; `transient_local` ⇒ an `AdvancedPublisher`
with a sample cache). Stored on the entity and returned (its concrete type
flows into [`PublisherHandle`](@ref){T,R} for a type-stable `put`). The publisher
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
dispatch runtime. Opens a FIFO-channel Zenoh subscriber on the topic keyexpr
sized to the QoS history depth (`KeepLast(N)`→capacity N, the backpressure buffer
that lets a blocking handler keep buffering), then spawns the consumer task that
decodes each sample to `msgtype` and runs `handler(msg)` under `concurrency`.
Returns the entity.

`view` selects the [`ViewMode`](@ref) the consumer threads into each delivery:
[`Owned`](@ref) (default), [`Checked`](@ref), or [`Unchecked`](@ref). `true`
(⇒ `Checked()`) and `false` (⇒ `Owned()`) are accepted as shorthand.

`concurrency` is the per-sample handler scheduling: [`Serial`](@ref) (default) or
[`Parallel`](@ref), defined by [`Concurrency`](@ref).

`weak` is a keyexpr concern only: it widens the subscriber to a wildcard data
keyexpr so off-type samples arrive for the per-sample backstop. It is unrelated to
the `Context` `weak_types` type-revision trust flag, which governs whether a pinned
type's RIHS01 is enforced against a diverging peer; the two are not coupled.
"""
function declare_subscription!(e::Entity, msgtype::Type, handler;
                               view::Union{Bool, ViewMode}=Owned(),
                               concurrency::Concurrency=Serial(),
                               force_relatch::Bool=false,
                               weak::Bool=false)
    view = _view_mode(view)
    ctx = e.node.context
    # A weak-static sub wildcard-matches the topic so off-type samples arrive for
    # the per-sample backstop, while still advertising its declared type in liveliness
    # /graph (make_entity set type_info); an exact sub uses its concrete type keyexpr.
    e._weak_static = weak
    tk = weak ? _wildcard_data_keyexpr(ctx.format, e.endpoint) :
                topic_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    qos = e.endpoint.qos
    # transient_local ⇒ an AdvancedSubscriber whose declaration issues a history
    # query (latched delivery); volatile ⇒ the plain FIFO subscriber. The novelty
    # gate is created only for the latched case (deduplicates re-latch
    # replays); a `nothing` gate is the volatile fast path (no per-sample work).
    sub = Base.open(ctx.session, Keyexpr(tk); channel=:fifo, capacity=cap,
                    allowed_origin = local_origin(ctx), _advanced_sub_kwargs(qos)...)
    gate = qos.durability === :transient_local ?
        _NoveltyGate(_cache_depth(qos); force=force_relatch) : nothing
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency, sub, gate)
    # Register for the intra-process short-circuit: same-Context publisher↔subscriber
    # pairs bypass serialize/Zenoh/decode (mechanism in performance/intraprocess.jl).
    # The `allowed_origin = local_origin(ctx)` on the sub above is what lets it coexist
    # with the short-circuit without double-delivery.
    register_local_subscription!(e, msgtype, handler; view = _is_view(view), concurrency = concurrency)
    # The re-latch thunk: a managed node's Inactive→Active redeclares the
    # advanced subscriber to re-run the history query, novelty-gated. Only meaningful
    # for transient_local — volatile subs leave `_relatch` nothing.
    if gate !== nothing
        e._relatch = () -> _do_relatch!(e, msgtype, handler, view, concurrency, gate, tk, cap, qos)
    end
    return e
end

# Re-latch: redeclare the advanced subscriber so its history query re-fires,
# recovering latched state the node missed while inactive. The novelty gate makes the
# redundant redelivery safe — a replayed value already delivered is dropped, only
# genuine updates fire (the effectful-replay fix).
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
    # we're somehow on that very task. Bounded (mirrors `close(::_ServiceWire)`): a
    # consumer wedged in a native call must not wedge the Inactive→Active transition —
    # move on after the budget and let the stuck task die with the process.
    if old_consumer isa Task && old_consumer !== current_task()
        Base.timedwait(() -> istaskdone(old_consumer), 3.0; pollint=0.02) === :ok ||
            @warn "relatch: old consumer didn't end within budget; proceeding" topic=e.endpoint.topic
    end
    _arm_relatch!(gate)        # freeze the now-complete delivered set as the baseline
    sub = Base.open(e.node.context.session, Keyexpr(tk); channel=:fifo, capacity=cap,
                    allowed_origin = local_origin(e.node.context), _advanced_sub_kwargs(qos)...)
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency, sub, gate)
    return nothing
end

"""
    _relatch!(e::Entity)

Re-run a transient_local subscription's latched-history fetch, if it has one.
A no-op for any entity without a re-latch thunk (volatile subs, publishers,
services). Called on a managed node's [`Inactive`](@ref)→[`Active`](@ref) transition (lifecycle.jl).
"""
_relatch!(e::Entity) = (e._relatch === nothing ? nothing : (e._relatch(); nothing))

"The data-route keyexpr for a pub/sub entity (`topic_keyexpr`)."
topic_key(e::Entity) = topic_keyexpr(e.node.context.format, e.endpoint)

# Map QoS history → FIFO subscriber capacity: KeepLast(N)→N, KeepAll→a
# large bound (libzenohc has no true unbounded channel; size generously so the
# common burst doesn't drop). Depth ≤ 0 is coerced to 1.
function _fifo_capacity(qos::QosProfile)
    qos.history === :keep_all && return 1024
    max(1, qos.depth)
end

# QoS reliability → Zenoh reliability singleton. The other policies (durability/
# deadline/lifespan/liveliness) are not Zenoh-transport concepts; they ride the
# liveliness token (QoS encoding) and are enforced at our layer.
_zenoh_reliability(qos::QosProfile) =
    qos.reliability === :best_effort ? Reliabilities.BEST_EFFORT : Reliabilities.RELIABLE

