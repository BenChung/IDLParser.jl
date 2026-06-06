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
    # A2 weak-static Subscription: a typed sub that wildcard-matches the topic and
    # runs the per-sample `check_sample_type` backstop (drop + `on_type_mismatch` on a
    # name/hash mismatch). Set by `declare_subscription!` (match=:weak); false for every
    # other entity. Written once before the consumer starts, so no atomic is needed.
    _weak_static::Bool
    # §12.3 message-lost: set true the first time an `on_message_lost` listener
    # registers (graph.jl). The consumer hot path reads it per sample to decide
    # whether to do the attachment-sequence bookkeeping (`note_sequence!`); the
    # common no-listener case is one atomic load, no decode, no alloc.
    @atomic _track_lost::Bool
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
                 nothing, nothing, false, false, true)
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
                               force_relatch::Bool=false,
                               weak::Bool=false)
    view = _view_mode(view)
    ctx = e.node.context
    # A2: a weak-static sub wildcard-matches the topic so off-type samples arrive for
    # the per-sample backstop, while still advertising its declared type in liveliness
    # /graph (make_entity set type_info); an exact sub uses its concrete type keyexpr.
    e._weak_static = weak
    tk = weak ? _wildcard_data_keyexpr(ctx.format, e.endpoint) :
                topic_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    qos = e.endpoint.qos
    # transient_local ⇒ an AdvancedSubscriber whose declaration issues a history
    # query (latched delivery); volatile ⇒ the plain FIFO subscriber. The D4
    # novelty gate is created only for the latched case (deduplicates re-latch
    # replays); a `nothing` gate is the volatile fast path (no per-sample work).
    sub = Base.open(ctx.session, Keyexpr(tk); channel=:fifo, capacity=cap,
                    allowed_origin = local_origin(ctx), _advanced_sub_kwargs(qos)...)
    gate = qos.durability === :transient_local ?
        _NoveltyGate(_cache_depth(qos); force=force_relatch) : nothing
    e._route = sub
    e._consumer = _spawn_consumer(e, msgtype, handler, view, concurrency, sub, gate)
    # §15.1: register for the intra-process short-circuit (a no-op when disabled). The
    # Zenoh sub above was opened with `allowed_origin = local_origin(ctx)`, so when the
    # short-circuit is on, the loopback of a same-Context publish is suppressed and the
    # data arrives via the direct path only — no double-delivery.
    register_local_subscription!(e, msgtype, handler; view = _is_view(view), concurrency = concurrency)
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

