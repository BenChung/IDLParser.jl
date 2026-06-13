# Intra-process short-circuit. When a publisher and a subscriber share a
# Context, the publisher hands the message object to the subscriber's handler
# directly — skipping the CDR serialize, the Zenoh hop, and the decode. Wired into
# publish/declare/close and tested, but **off by default** for now (opt in with
# `set_intra_process!(true)`) pending broad-suite validation before flipping the
# design's on-by-default; with it off, same-Context delivery rides Zenoh's loopback.
# The realization that keeps it API-minimal:
# *intra-process is the same receive model with an in-heap Julia object as the
# buffer instead of a Zenoh payload*, so the existing knobs carry it:
#
#   - the `view` flag is the share/copy choice — owned (default) hands each sub a
#     `copy` (mutate freely, the cross-process-compatible contract), `view=true`
#     shares the publisher's object read-only (true zero-copy);
#   - `Locality` does the dedup — an intra-process sub sets `allowed_origin =
#     REMOTE` on its Zenoh subscriber so the loopback of a local publication is
#     ignored and the data arrives via the direct path only.
#
# Wiring (threaded into the existing files):
#   - `declare_subscription!` calls `register_local_subscription!` and opens its Zenoh
#     subscriber with `allowed_origin = local_origin(ctx)`; `close(::Entity)` calls
#     `unregister_local_subscription!`.
#   - `publish` calls `deliver_local(pub, msg)` before the Zenoh `put` — handing the
#     message to each local sub's per-endpoint worker (not inline on the publisher's
#     task), then `put` still runs once for any remote subscribers.
#
# Scope is same-Context: two Contexts in one process have separate Zenoh
# sessions, so cross-Context-same-process is a cross-registry match, deferred.

using ROSZenoh: ROSZenoh, qos_compatible
using Zenoh: Localities

# ── drop-oldest backpressure ──────────────────────────────────────────────────
# Julia's `Channel` has no overwrite mode, so this is the KEEP_LAST ring: a
# non-blocking enqueue that evicts the oldest item(s) when the buffer is full.
# Operates under the channel lock and touches the same fields/conditions Base's
# buffered `put!`/`take!` do (`data` deque, `sz_max` capacity, `cond_take`), so a
# concurrent taker sees a consistent buffer and is woken. Shared by the
# intra-process inbox and the lifecycle feedback channel (called by exact name).

"""
    _put_drop_oldest!(ch::Channel, x) -> Bool

Non-blocking enqueue of `x` onto bounded `ch`, evicting the oldest item(s) to make
room if it is full (KEEP_LAST ring semantics). Returns `true` if anything was
dropped. Wakes a waiting taker. Throws if `ch` is closed (as `put!` would).
"""
function _put_drop_oldest!(ch::Channel, x)
    x = convert(eltype(ch), x)                       # `put!` parity: coerce to the channel's eltype
    lock(ch)
    try
        Base.check_channel_state(ch)
        dropped = false
        while length(ch.data) >= ch.sz_max          # full ⇒ evict oldest
            popfirst!(ch.data)
            Base._increment_n_avail(ch, -1)
            dropped = true
        end
        Base._increment_n_avail(ch, 1)
        push!(ch.data, x)
        notify(ch.cond_take, nothing, true, false)   # wake taker(s)/fetcher(s)
        return dropped
    finally
        unlock(ch)
    end
end

# ── intra-process toggle ──────────────────────────────────────────────────────
# Off by default (see the header); opt in process-wide. The design calls for a
# per-Context `intra_process` flag; the `Context` struct doesn't carry one yet
# (context.jl is a lower layer), so the switch is process-global here. When the
# flag lands on `Context`, `intra_process_enabled(ctx)` is the single seam to
# consult it instead.
const _INTRA_PROCESS = Ref(false)

"""
    set_intra_process!(on::Bool) -> Bool

Enable/disable the intra-process short-circuit process-wide. **Off by
default** for now (the mechanism is wired + tested but not yet flipped to the
design's on-by-default, pending broad-suite validation); opt in with
`set_intra_process!(true)`. Returns the previous setting. Affects only
same-Context delivery; the Zenoh data path is unchanged.

TODO: when the `Context` carries a per-Context `intra_process` flag this
becomes its default; [`intra_process_enabled`](@ref) is the seam to read it.
"""
function set_intra_process!(on::Bool)
    prev = _INTRA_PROCESS[]
    _INTRA_PROCESS[] = on
    prev
end

"""
    intra_process_enabled(ctx) -> Bool

Whether the intra-process short-circuit is active for `ctx`. The single
predicate the publish and subscription paths consult. Process-global today (see
[`set_intra_process!`](@ref)); per-Context when the flag lands on `Context`.
"""
intra_process_enabled(::Context) = _INTRA_PROCESS[]

# ── the per-Context local-subscription registry ───────────────────────────────
# Each registered subscription records exactly what direct delivery needs: the
# owning [`Entity`](@ref), the message type and the QoS (for type+RxO matching),
# the user handler, and the `view`/`concurrency` policies (delivery mirrors the
# subscription dispatch runtime). Keyed by `Entity` so close/dispose remove it by
# identity.
#
# The `Context` struct (a lower layer) has no slot for this, so the registry lives
# module-side, keyed by Context. A `WeakKeyDict` lets a dropped-but-not-closed
# Context be collected; per-Context state is a small mutable holder with its own
# lock so registration/delivery don't contend on a global lock.

"""
    LocalSubscription{T}

A same-Context subscription as the intra-process short-circuit sees it. Wraps the
backing [`Entity`](@ref) — which owns the id, liveliness token, data route, and
graph lifecycle — adding the typed surface direct delivery needs:

  - `T`, the message type, and the QoS, for type + RxO matching;
  - the user `handler`;
  - the `view`/`concurrency` policies (delivery mirrors the subscription dispatch
    runtime exactly).

Held in the Context's local registry; one per intra-process-eligible
[`SubscriptionHandle`](@ref).
"""
struct LocalSubscription{T}
    entity::Entity
    qos::QosProfile
    handler::Any
    view::Bool
    concurrency::Concurrency
    # Per-endpoint local-delivery queue. `deliver_local` `put!`s the publisher's
    # message here (so the publisher's task is never the one running the handler);
    # a worker task drains it and dispatches (see `register_local_subscription!`).
    inbox::Channel{Any}
end

# The type name + RIHS01 hash of the subscription's type (for matching). Recovered
# from the entity's `EndpointEntity.type_info` so it carries whatever the registry
# specialized `type_info(T)` to.
_local_type(ls::LocalSubscription) = ls.entity.endpoint.type_info

mutable struct _LocalRegistry
    lock::ReentrantLock
    # topic FQN → the subscriptions on it. Keyed by topic so a publish does one
    # dict lookup, not a scan of every sub in the Context.
    by_topic::Dict{String, Vector{LocalSubscription}}
end

_LocalRegistry() = _LocalRegistry(ReentrantLock(), Dict{String, Vector{LocalSubscription}}())

# One registry per Context, created on first touch. The outer lock guards only the
# WeakKeyDict; per-Context work takes the registry's own lock.
const _REGISTRIES = WeakKeyDict{Context, _LocalRegistry}()
const _REGISTRIES_LOCK = ReentrantLock()

function _registry(ctx::Context)
    @lock _REGISTRIES_LOCK get!(() -> _LocalRegistry(), _REGISTRIES, ctx)
end

# ── registration (called from Subscription declare / close) ────────────────────

"""
    register_local_subscription!(entity, ::Type{T}, handler; view, concurrency) -> Entity

Register a `Subscription` `entity` in its Context's intra-process registry
so same-Context publishers can deliver to its `handler` directly. Records the
message type, QoS, and the `view`/`concurrency` policies so direct delivery
mirrors the subscription dispatch runtime. The subscription's Zenoh subscriber should be
opened with `allowed_origin = local_origin(ctx)` so the loopback of a local
publication is suppressed (no double-delivery).

A no-op when the short-circuit is disabled ([`intra_process_enabled`](@ref)) —
in that case the subscription receives every publication over Zenoh as usual.
Called by the subscription declaration; [`unregister_local_subscription!`](@ref)
is the close-side inverse.
"""
function register_local_subscription!(e::Entity, ::Type{T}, handler;
                                      view::Bool=false,
                                      concurrency::Concurrency=Serial()) where {T}
    ctx = e.node.context
    intra_process_enabled(ctx) || return e
    inbox = Channel{Any}(max(1, _fifo_capacity(e.endpoint.qos)))
    ls = LocalSubscription{T}(e, e.endpoint.qos, handler, view, concurrency, inbox)
    reg = _registry(ctx)
    @lock reg.lock begin
        subs = get!(() -> LocalSubscription[], reg.by_topic, e.endpoint.topic)
        push!(subs, ls)
    end
    # The per-endpoint local-delivery worker (the "endpoint worker"). Serial ⇒ one
    # task pinned `sticky` to the SAME declaring thread as this sub's Zenoh consumer
    # (_spawn_consumer): two sticky tasks on one thread are cooperative, never
    # concurrent, so the handler still runs single-threaded on its declaring thread —
    # the dispatch runtime's guarantee — across BOTH local and remote deliveries. Parallel ⇒ a
    # non-sticky drainer that spawns a task per message (order not preserved, as on the
    # Zenoh path). The worker ends when `inbox` is closed (unregister, on close).
    w = Task(() -> _local_consume_loop(ls))
    w.sticky = concurrency isa Serial
    schedule(w)
    return e
end

# Drain the local inbox and dispatch each message exactly as the dispatch runtime would.
# `_with_node_logger` mirrors the Zenoh consumer so a handler's `@info` still routes to
# the node's /rosout. A closed inbox ends the loop (clean shutdown).
function _local_consume_loop(ls::LocalSubscription)
    try
        _with_node_logger(ls.entity.node) do
            for msg in ls.inbox
                if ls.concurrency isa Parallel
                    Threads.@spawn _invoke_local(ls, msg)
                else
                    _invoke_local(ls, msg)
                end
            end
        end
    catch err
        err isa ShutdownException && return
        @error "intra-process consumer task failed" topic=ls.entity.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

"""
    unregister_local_subscription!(entity) -> nothing

Drop `entity` from its Context's intra-process registry (the inverse of
[`register_local_subscription!`](@ref)). Called from `close(entity)`; idempotent
and safe to call for a subscription that was never registered.
"""
function unregister_local_subscription!(e::Entity)
    ctx = e.node.context
    reg = @lock _REGISTRIES_LOCK get(_REGISTRIES, ctx, nothing)
    reg === nothing && return nothing
    removed = nothing
    @lock reg.lock begin
        subs = get(reg.by_topic, e.endpoint.topic, nothing)
        subs === nothing && return nothing
        i = findfirst(ls -> ls.entity === e, subs)
        if i !== nothing
            removed = subs[i]
            deleteat!(subs, i)
        end
        isempty(subs) && delete!(reg.by_topic, e.endpoint.topic)
    end
    removed === nothing || close(removed.inbox)   # end the local-delivery worker
    return nothing
end

# ── locality: suppress the Zenoh loopback ─────────────────────────────────────

"""
    local_origin(ctx) -> Zenoh.Locality

The `allowed_origin` an intra-process subscriber's Zenoh subscriber should use:
`REMOTE` while the short-circuit is on (the loopback of a same-session
publication is ignored — the data arrives via the direct path only), `ANY`
otherwise (every publication arrives over Zenoh). The `Subscription` declaration
threads this into `Base.open(session, …; allowed_origin = local_origin(ctx))`.
"""
local_origin(ctx::Context) =
    intra_process_enabled(ctx) ? Localities.REMOTE : Localities.ANY

# ── matching (type + RxO QoS) ─────────────────────────────────────────────────
# A publisher may deliver to a local sub iff their types are the same name+version
# and the QoS is RxO-compatible (offered ≥ requested; the sub requests, the pub
# offers — the same direction graph.jl's detector uses). `EMPTY_TOPIC_TYPE`
# (`type_info === nothing`) on either side can't be type-checked, so we match on
# topic alone (the user opted out of a typed topic). Lifecycle gating and
# liveness are checked at delivery, not here.

function _types_match(pub_t, sub_t)
    (pub_t === nothing || sub_t === nothing) && return true   # untyped topic
    pub_t.name == sub_t.name && pub_t.hash == sub_t.hash
end

# RxO: subscriber requests, publisher offers; compatible iff no violations.
_qos_match(pub_qos::QosProfile, sub_qos::QosProfile) =
    isempty(qos_compatible(sub_qos, pub_qos))

# Deliver iff the endpoints carry the same *wire* type (`_types_match`: same name +
# RIHS01) and the QoS is RxO-compatible — *regardless* of whether the publisher's and
# subscriber's Julia structs are the identical alias. Two modules may hold
# distinct same-`(name,hash)` structs for one wire type; `_invoke_local` cross-materializes
# the publisher's `msg` into the subscriber's declared type. A genuinely *different* wire
# type (different name or hash) fails `_types_match` and is dropped here — graph.jl's
# detector flags that mismatch. (Same-session loopback is suppressed via `local_origin`, so
# a matched local sub is served *only* by this direct path — every matching sub, sibling
# aliases included, must be delivered here or the message is lost.)
function _deliverable(pub::PublisherHandle, ls::LocalSubscription)
    isopen(ls.entity) || return false
    _types_match(pub.entity.endpoint.type_info, _local_type(ls)) &&
        _qos_match(pub.entity.endpoint.qos, ls.qos)
end

# ── direct delivery ───────────────────────────────────────────────────────────
# Hand `msg` to each matching local sub. The `view` flag is the share/copy choice:
# owned (default) hands a `copy` so a sub may mutate
# freely without aliasing siblings or the publisher; `view=true` shares the object
# read-only (true zero-copy). Immutable (`@cdr1_compat`) messages have no mutation
# hazard, so `copy` of one is the identity and sharing is free either way.
#
# Delivery never runs the handler on the publisher's task: each sub has a per-endpoint
# worker draining its inbox (the dispatch runtime). `Serial()` is one sticky worker on
# the sub's declaring thread (ordered); `Parallel(n)` spawns a task per message (order
# not preserved). A handler throw is logged, never fatal —
# one bad delivery must not break the publisher (the dispatch runtime's contract).
# Unlike the cross-process view path there is no `with_memory`/`BorrowError`: the
# object is GC-backed, so escape is safe; only in-place mutation is shared state.

"""
    deliver_local(pub::PublisherHandle, msg) -> Bool

Deliver `msg` to every same-Context subscriber that matches `pub`'s topic, type,
and QoS — the intra-process short-circuit. Each matching subscriber's
handler runs with `msg` owned (a `copy`, the default) or shared read-only
(`view=true`), per *that subscriber's* `view` flag, scheduled by its `concurrency`
policy. The CDR serialize, the Zenoh hop, and the decode are all skipped.

Each sub's inbox is a QoS-depth ring: a full inbox (a slow same-process subscriber)
drops the oldest queued message rather than blocking `publish` ([`_put_drop_oldest!`](@ref)),
so a slow local sub never delays the wire `put`.

Returns `true` if at least one local subscriber was delivered to. The caller
(`publish`) **still** issues the Zenoh `put` afterwards so any *remote*
subscribers are served (the local subs suppress their own loopback via
`local_origin`, so they are not double-delivered). A no-op returning
`false` when the short-circuit is disabled or no local sub matches.
"""
function deliver_local(pub::PublisherHandle{T}, msg::T) where {T}
    e = pub.entity
    ctx = e.node.context
    intra_process_enabled(ctx) || return false
    isopen(e) || return false

    reg = @lock _REGISTRIES_LOCK get(_REGISTRIES, ctx, nothing)
    reg === nothing && return false

    # Snapshot the matching subs under the registry lock, then deliver outside it
    # so a handler (which may register/close subs, or block) can't deadlock or
    # stall other publishers.
    targets = @lock reg.lock begin
        subs = get(reg.by_topic, e.endpoint.topic, nothing)
        subs === nothing ? LocalSubscription[] :
            LocalSubscription[ls for ls in subs if _deliverable(pub, ls)]
    end
    isempty(targets) && return false

    # Hand off to each sub's per-endpoint worker — never run the handler on the
    # publisher's task. Drop-oldest (KEEP_LAST ring) so a slow same-process sub never
    # blocks publish — and thus never delays the wire `put` (pubsub.jl, which runs
    # after this). A closed inbox (the sub is closing concurrently) is skipped.
    delivered = false
    for ls in targets
        try
            _put_drop_oldest!(ls.inbox, msg)
            delivered = true
        catch err
            err isa InvalidStateException || rethrow()   # inbox closed mid-flight
        end
    end
    return delivered
end

# Hand the message to the handler *as the subscriber's declared type* `S`, with the
# share/copy semantics the sub's `view` flag selects. Owned (default) yields an
# independent value (mutate-freely); `view` shares read-only. When the publisher's
# struct is a sibling alias of `S` (same wire type, different Julia struct), the
# value is cross-materialized into `S` (`_ipc_own`/`_ipc_share`; same wire type, distinct
# Julia struct). A handler throw is logged, never fatal.
function _invoke_local(ls::LocalSubscription{S}, msg) where {S}
    try
        payload = ls.view ? _ipc_share(S, msg) : _ipc_own(S, msg)
        ls.handler(payload)
    catch err
        err isa ShutdownException && return
        @error "intra-process handler threw" topic=ls.entity.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# The owned-delivery copy. The contract is "mutate freely":
# a sub must be able to mutate its message without aliasing the publisher's object
# or a sibling sub's. Generated messages are *immutable* structs (gen.jl emits
# `@cdr1_compat`/`@kwdef struct`, no `mutable`), so the only mutable substructure
# is nested `Vector` fields of a variable-length (`@kwdef`) message; isolating
# those is `deepcopy`. An all-fixed (`@cdr1_compat`) message is `isbits` — no
# mutable substructure, nothing to alias — so it is shared as-is (a value copy is
# the identity and a `deepcopy` would needlessly churn the heap).
#
# `deepcopy` (not `copy`) because immutable generated structs carry no `Base.copy`
# method; `deepcopy` is the universally-defined, mutation-isolating copy. It is
# heavier than the `view=true` share — which is exactly why `view=true` is the
# opt-in for large arrays (the share/copy table).
_ipc_copy(msg::T) where {T} = isbitstype(T) ? msg : deepcopy(msg)

# Produce the subscriber's declared type `S` from the publisher's `msg`.
# Same-alias (`S === typeof(msg)`) keeps today's behavior; a *sibling* alias of the same
# wire type — distinct Julia struct, equal RIHS01 ⇒ identical CDR form — is
# cross-materialized: owned (`view=false`) owes an independent value, shared (`view=true`)
# is zero-copy where a cast suffices.
_ipc_own(::Type{S}, msg::S)   where {S} = _ipc_copy(msg)            # same alias, owned
_ipc_share(::Type{S}, msg::S) where {S} = msg                      # same alias, shared
# Cross-alias: owned round-trips through the codec (correct by the same invariant the wire
# uses, and always cheaper than the Zenoh fallback); a `CDRView` is copied out of its buffer
# first (`as`'s view method). Shared: a `CDRView` re-tags zero-copy; any other *materialized*
# sibling falls back to the owned copy — a var-length struct with a nested *message* field
# can't be shared by reference (its `Vector{B.Point}` ≠ `Vector{A.Point}`), the one corner
# where `view=true` loses zero-copy (isbits-bitcast is a deferred optimization).
_ipc_own(::Type{S}, msg)          where {S} = as(msg, S)            # codec round-trip (== `as`)
_ipc_own(::Type{S}, v::CDRView)   where {S} = as(v, S)              # materialize, then cast
_ipc_share(::Type{S}, v::CDRView) where {S} = CDRSerialization.retag(v, S)
_ipc_share(::Type{S}, msg)        where {S} = _ipc_own(S, msg)
