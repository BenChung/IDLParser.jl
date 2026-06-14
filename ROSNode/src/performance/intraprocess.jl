# Intra-process short-circuit. A publisher hands its message object straight to a
# same-Context subscriber's handler, skipping the CDR serialize, the Zenoh hop, and
# the decode. It reuses the receive model unchanged — same view/concurrency/KEEP_LAST
# machinery — with an in-heap Julia object as the buffer instead of a Zenoh payload;
# the only delivery-side difference is the heap buffer and a deepcopy on owned delivery.
# So the existing knobs carry it: `view` is the share/copy choice, and `Locality`
# suppresses double-delivery (a local sub opens with allowed_origin = REMOTE so a
# same-session publication arrives only via the direct path).
#
# Scope is same-Context: two Contexts in one process have separate Zenoh sessions.

using ROSZenoh: ROSZenoh, qos_compatible
using Zenoh: Localities

# ── drop-oldest backpressure ──────────────────────────────────────────────────
# KEEP_LAST ring over a `Channel` (which has no native overwrite mode). Touches
# Base's buffered-channel internals (`data` deque, `sz_max`, `cond_take`) under the
# channel lock, so the layout must track Base. Shared by the intra-process inbox and
# the lifecycle feedback channel (referenced by exact name).

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
        while length(ch.data) >= ch.sz_max
            popfirst!(ch.data)
            Base._increment_n_avail(ch, -1)
            dropped = true
        end
        Base._increment_n_avail(ch, 1)
        push!(ch.data, x)
        notify(ch.cond_take, nothing, true, false)
        return dropped
    finally
        unlock(ch)
    end
end

# ── intra-process toggle ──────────────────────────────────────────────────────
# Process-global today; `intra_process_enabled(ctx)` is the single seam, so a
# per-Context flag can replace this Ref without touching the publish/subscribe paths.
const _INTRA_PROCESS = Ref(false)

"""
    set_intra_process!(on::Bool) -> Bool

Enable the intra-process short-circuit process-wide (off by default). Returns the
previous setting. Affects only same-Context delivery; the Zenoh data path is
unchanged. [`intra_process_enabled`](@ref) reads the resulting state.
"""
function set_intra_process!(on::Bool)
    prev = _INTRA_PROCESS[]
    _INTRA_PROCESS[] = on
    prev
end

"""
    intra_process_enabled(ctx) -> Bool

Whether the intra-process short-circuit is active for `ctx`. The single predicate
the publish and subscription paths consult, set by [`set_intra_process!`](@ref).
"""
intra_process_enabled(::Context) = _INTRA_PROCESS[]

# ── the per-Context local-subscription registry ───────────────────────────────
# A WeakKeyDict keyed by Context lets a dropped-but-not-closed Context be collected;
# each per-Context holder carries its own lock so registration/delivery on one
# Context don't contend on a global lock.

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
    # Per-endpoint queue draining off the publisher's task, so the handler never
    # runs on the publisher; `register_local_subscription!` spawns its worker.
    inbox::Channel{Any}
end

# Name + RIHS01 of the subscription's type, for matching (the registry's `type_info(T)`).
_local_type(ls::LocalSubscription) = ls.entity.endpoint.type_info

mutable struct _LocalRegistry
    lock::ReentrantLock
    # Keyed by topic FQN so a publish is one dict lookup, not a scan of every sub.
    by_topic::Dict{String, Vector{LocalSubscription}}
end

_LocalRegistry() = _LocalRegistry(ReentrantLock(), Dict{String, Vector{LocalSubscription}}())

# One registry per Context, created on first touch. This outer lock guards only the
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
    # Serial pins the worker sticky to the same thread as this sub's Zenoh consumer:
    # two sticky tasks on one thread are cooperative, so the handler stays
    # single-threaded across both local and remote deliveries. Dropping sticky breaks
    # Serial ordering for local delivery while leaving Zenoh delivery intact.
    w = Task(() -> _local_consume_loop(ls))
    w.sticky = concurrency isa Serial
    schedule(w)
    return e
end

# `_with_node_logger` mirrors the Zenoh consumer so a handler's `@info` still routes
# to the node's /rosout. A closed inbox ends the loop; only ShutdownException unwinds
# silently, every other throw is logged so one bad message never kills the worker.
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

The `allowed_origin` an intra-process subscriber's Zenoh subscriber uses, threaded
into `Base.open(session, …; allowed_origin = local_origin(ctx))` at the subscription
declaration. Two cases:

  - short-circuit on: `REMOTE`, so a same-session publication arrives only via the
    direct path (the Zenoh loopback would otherwise double-deliver it);
  - short-circuit off: `ANY`, so every publication arrives over Zenoh.
"""
local_origin(ctx::Context) =
    intra_process_enabled(ctx) ? Localities.REMOTE : Localities.ANY

# ── matching (type + RxO QoS) ─────────────────────────────────────────────────
# A publisher delivers to a local sub when their wire types match (same name +
# RIHS01) and the QoS is RxO-compatible — the same direction the graph compatibility
# detector uses. An untyped topic (`type_info === nothing` either side) matches on
# topic alone. Lifecycle gating and liveness are checked at delivery, not here.

function _types_match(pub_t, sub_t)
    (pub_t === nothing || sub_t === nothing) && return true   # untyped topic
    pub_t.name == sub_t.name && pub_t.hash == sub_t.hash
end

# RxO: subscriber requests, publisher offers; compatible iff no violations.
_qos_match(pub_qos::QosProfile, sub_qos::QosProfile) =
    isempty(qos_compatible(sub_qos, pub_qos))

# Matches on wire type (name + RIHS01) and RxO QoS, independent of Julia struct
# identity: two modules may hold distinct same-`(name,hash)` structs for one wire
# type, and `_invoke_local` cross-materializes between them. Loopback is suppressed,
# so every matching sub — sibling aliases included — must be delivered here or its
# message is lost.
function _deliverable(pub::PublisherHandle, ls::LocalSubscription)
    isopen(ls.entity) || return false
    _types_match(pub.entity.endpoint.type_info, _local_type(ls)) &&
        _qos_match(pub.entity.endpoint.qos, ls.qos)
end

# ── direct delivery ───────────────────────────────────────────────────────────
# The intra-process buffer is a GC-backed Julia object, so there is no
# `with_memory`/`BorrowError` as on the cross-process view path — escape is safe and
# only in-place mutation is shared state (which is what `view` governs).

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

    # Snapshot the matching subs under the lock, then deliver outside it: a handler
    # may register/close subs or block, so running it under the lock would deadlock
    # re-entrant registration and stall other publishers.
    targets = @lock reg.lock begin
        subs = get(reg.by_topic, e.endpoint.topic, nothing)
        subs === nothing ? LocalSubscription[] :
            LocalSubscription[ls for ls in subs if _deliverable(pub, ls)]
    end
    isempty(targets) && return false

    # Drop-oldest enqueue (never run the handler on the publisher's task), so a slow
    # same-process sub never blocks publish and thus never delays the wire `put`.
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

# Hand the message to the handler as the subscriber's declared type `S`, owned or
# shared per the sub's `view` flag (`_ipc_own`/`_ipc_share` cross-materialize when
# the publisher's struct is a sibling alias of `S`). A handler throw is logged, never
# fatal; only ShutdownException unwinds.
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

# Owned delivery owes the handler a mutate-freely value. Generated structs are
# immutable with no `Base.copy`, so `deepcopy` is the universal mutation-isolating
# copy — it isolates the nested `Vector` substructure of a variable-length message.
# An all-fixed message is `isbits` with no mutable substructure, so it is shared
# as-is and `deepcopy` is skipped as a no-op.
_ipc_copy(msg::T) where {T} = isbitstype(T) ? msg : deepcopy(msg)

# Produce the subscriber's declared type `S` from the publisher's `msg`. A sibling
# alias of the same wire type (distinct Julia struct, equal RIHS01 ⇒ identical CDR
# form) is cross-materialized via the codec, the same invariant the wire uses.
_ipc_own(::Type{S}, msg::S)   where {S} = _ipc_copy(msg)            # same alias, owned
_ipc_share(::Type{S}, msg::S) where {S} = msg                      # same alias, shared
# Cross-alias: owned round-trips through the codec (a `CDRView` is materialized first).
# Shared re-tags a `CDRView` zero-copy; any other materialized sibling falls back to the
# owned copy, since a nested-message field cannot be shared by reference across aliases
# (`Vector{B.Point}` ≠ `Vector{A.Point}`).
_ipc_own(::Type{S}, msg)          where {S} = as(msg, S)            # codec round-trip (== `as`)
_ipc_own(::Type{S}, v::CDRView)   where {S} = as(v, S)              # materialize, then cast
_ipc_share(::Type{S}, v::CDRView) where {S} = CDRSerialization.retag(v, S)
_ipc_share(::Type{S}, msg)        where {S} = _ipc_own(S, msg)
