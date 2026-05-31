# §15.1 Intra-process short-circuit. When a publisher and a subscriber share a
# Context, the publisher hands the message object to the subscriber's handler
# directly — skipping the CDR serialize, the Zenoh hop, and the decode. On by
# default. The realization that keeps it API-minimal (DESIGN §Intra-process):
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
# Wiring (the verify step threads these into the existing files):
#   - `Subscription` registration calls `register_local_subscription!` (and uses
#     `local_origin(ctx)` for its subscriber's `allowed_origin`); `close` calls
#     `unregister_local_subscription!`.
#   - `publish` calls `deliver_local(pub, msg)` before the Zenoh `put` — direct to
#     local subs, then `put` still runs once for any remote subscribers.
#
# Scope is same-Context (§15.2): two Contexts in one process have separate Zenoh
# sessions, so cross-Context-same-process is a cross-registry match, deferred.

using ROSZenoh: ROSZenoh, qos_compatible
using Zenoh: Localities

# ── intra-process toggle ──────────────────────────────────────────────────────
# On by default; a process can opt the whole mechanism out. The DESIGN names a
# per-Context `intra_process=false` flag; the `Context` struct doesn't carry one
# yet (context.jl is a lower layer), so the switch is process-global here. When the
# flag lands on `Context`, `intra_process_enabled(ctx)` is the single seam to
# consult it instead.
const _INTRA_PROCESS = Ref(true)

"""
    set_intra_process!(on::Bool) -> Bool

Enable/disable the intra-process short-circuit (§15.1) process-wide. On by
default. Returns the previous setting. Affects only same-Context delivery; the
Zenoh data path is unchanged.

TODO(§5): when the `Context` carries a per-Context `intra_process` flag this
becomes its default; [`intra_process_enabled`](@ref) is the seam to read it.
"""
function set_intra_process!(on::Bool)
    prev = _INTRA_PROCESS[]
    _INTRA_PROCESS[] = on
    prev
end

"""
    intra_process_enabled(ctx) -> Bool

Whether the intra-process short-circuit is active for `ctx` (§15.1). The single
predicate the publish and subscription paths consult. Process-global today (see
[`set_intra_process!`](@ref)); per-Context when the flag lands on `Context`.
"""
intra_process_enabled(::Context) = _INTRA_PROCESS[]

# ── the per-Context local-subscription registry ───────────────────────────────
# Each registered subscription records exactly what direct delivery needs: the
# owning `Entity` (for liveness/topic/QoS/lifecycle gating), the message type and
# the QoS (for type+RxO matching), the user handler, and the `view`/`concurrency`
# policies (delivery mirrors the §4 dispatch). Keyed by `Entity` so close/dispose
# remove it by identity.
#
# The `Context` struct (a lower layer) has no slot for this, so the registry lives
# module-side, keyed by Context. A `WeakKeyDict` lets a dropped-but-not-closed
# Context be collected; per-Context state is a small mutable holder with its own
# lock so registration/delivery don't contend on a global lock.

"""
    LocalSubscription{T}

A same-Context subscription as the intra-process path sees it (§15.1): the backing
[`Entity`](@ref) plus the typed surface direct delivery needs — message type `T`,
QoS (for RxO matching), the user `handler`, and the `view`/`concurrency` policies
(delivery mirrors the §4 dispatch runtime exactly). Held in the Context's local
registry; one per intra-process-eligible `Subscription`.
"""
struct LocalSubscription{T}
    entity::Entity
    qos::QosProfile
    handler::Any
    view::Bool
    concurrency::Concurrency
end

# The type name + RIHS01 hash of the subscription's type (for matching). Recovered
# from the entity's `EndpointEntity.type_info` so it carries whatever the registry
# specialized `type_info(T)` to (§2.1).
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

Register a `Subscription` `entity` in its Context's intra-process registry (§15.1)
so same-Context publishers can deliver to its `handler` directly. Records the
message type, QoS, and the `view`/`concurrency` policies so direct delivery
mirrors the §4 dispatch runtime. The subscription's Zenoh subscriber should be
opened with `allowed_origin = local_origin(ctx)` so the loopback of a local
publication is suppressed (no double-delivery).

A no-op when the short-circuit is disabled ([`intra_process_enabled`](@ref)) —
in that case the subscription receives every publication over Zenoh as usual.
Called by the §6 subscription declaration; [`unregister_local_subscription!`](@ref)
is the close-side inverse.
"""
function register_local_subscription!(e::Entity, ::Type{T}, handler;
                                      view::Bool=false,
                                      concurrency::Concurrency=Serial()) where {T}
    ctx = e.node.context
    intra_process_enabled(ctx) || return e
    ls = LocalSubscription{T}(e, e.endpoint.qos, handler, view, concurrency)
    reg = _registry(ctx)
    @lock reg.lock begin
        subs = get!(() -> LocalSubscription[], reg.by_topic, e.endpoint.topic)
        push!(subs, ls)
    end
    return e
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
    @lock reg.lock begin
        subs = get(reg.by_topic, e.endpoint.topic, nothing)
        subs === nothing && return nothing
        i = findfirst(ls -> ls.entity === e, subs)
        i === nothing || deleteat!(subs, i)
        isempty(subs) && delete!(reg.by_topic, e.endpoint.topic)
    end
    return nothing
end

# ── locality: suppress the Zenoh loopback ─────────────────────────────────────

"""
    local_origin(ctx) -> Zenoh.Locality

The `allowed_origin` an intra-process subscriber's Zenoh subscriber should use
(§15.1): `REMOTE` while the short-circuit is on (the loopback of a same-session
publication is ignored — the data arrives via the direct path only), `ANY`
otherwise (every publication arrives over Zenoh). The `Subscription` declaration
threads this into `Base.open(session, …; allowed_origin = local_origin(ctx))`.
"""
local_origin(ctx::Context) =
    intra_process_enabled(ctx) ? Localities.REMOTE : Localities.ANY

# ── matching (type + RxO QoS, §12.2) ──────────────────────────────────────────
# A publisher may deliver to a local sub iff their types are the same name+version
# and the QoS is RxO-compatible (offered ≥ requested; the sub requests, the pub
# offers — the same direction graph.jl's detector uses). `EMPTY_TOPIC_TYPE`
# (`type_info === nothing`) on either side can't be type-checked, so we match on
# topic alone (the user opted out of a typed topic). Lifecycle gating (§14.2) and
# liveness are checked at delivery, not here.

function _types_match(pub_t, sub_t)
    (pub_t === nothing || sub_t === nothing) && return true   # untyped topic
    pub_t.name == sub_t.name && pub_t.hash == sub_t.hash
end

# RxO: subscriber requests, publisher offers; compatible iff no violations.
_qos_match(pub_qos::QosProfile, sub_qos::QosProfile) =
    isempty(qos_compatible(sub_qos, pub_qos))

# `ls::LocalSubscription{T}` for the publisher's own `T`: direct delivery hands the
# sub the publisher's message object, so the sub's Julia type must *be* `T` — the
# `LocalSubscription{T}` constraint enforces that at dispatch. A topic carrying two
# different Julia types is a type mismatch (graph.jl flags it); we don't hand one
# sub another type's object, so a non-`T` sub on the topic is simply not delivered
# to here (it still receives over Zenoh, where the type-mismatch backstop applies).
function _deliverable(pub::PublisherHandle{T}, ls::LocalSubscription{T}) where {T}
    isopen(ls.entity) || return false
    _types_match(pub.entity.endpoint.type_info, _local_type(ls)) &&
        _qos_match(pub.entity.endpoint.qos, ls.qos)
end
_deliverable(::PublisherHandle, ::LocalSubscription) = false

# ── direct delivery ───────────────────────────────────────────────────────────
# Hand `msg` to each matching local sub. The `view` flag is the share/copy choice
# (DESIGN §Intra-process): owned (default) hands a `copy` so a sub may mutate
# freely without aliasing siblings or the publisher; `view=true` shares the object
# read-only (true zero-copy). Immutable (`@cdr1_compat`) messages have no mutation
# hazard, so `copy` of one is the identity and sharing is free either way.
#
# Delivery mirrors the §4 dispatch: `Serial()` runs the handler inline on the
# publisher's task (preserving order; the publisher already owns the call), and
# `Parallel(n)` spawns a task per message. A handler throw is logged, never fatal —
# one bad delivery must not break the publisher (same contract as `_run_handler`).
# Unlike the cross-process view path there is no `with_memory`/`BorrowError`: the
# object is GC-backed, so escape is safe; only in-place mutation is shared state.

"""
    deliver_local(pub::PublisherHandle, msg) -> Bool

Deliver `msg` to every same-Context subscriber that matches `pub`'s topic, type,
and QoS (§15.1) — the intra-process short-circuit. Each matching subscriber's
handler runs with `msg` owned (a `copy`, the default) or shared read-only
(`view=true`), per *that subscriber's* `view` flag, scheduled by its `concurrency`
policy. The CDR serialize, the Zenoh hop, and the decode are all skipped.

Returns `true` if at least one local subscriber was delivered to. The caller
(`publish`) **still** issues the Zenoh `put` afterwards so any *remote*
subscribers are served (the local subs suppress their own loopback via
[`local_origin`](@ref), so they are not double-delivered). A no-op returning
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

    for ls in targets
        _deliver_one(ls, msg)
    end
    return true
end

# Schedule one delivery per the sub's concurrency policy. `Serial()` inline on the
# publisher's task (order preserved); `Parallel(n)` a task per message. (D6's
# "route local delivery through the endpoint worker, not inline" refinement is
# still pending — for now Serial delivers inline, matching the prior behavior.)
function _deliver_one(ls::LocalSubscription{T}, msg::T) where {T}
    if ls.concurrency isa Parallel
        Threads.@spawn _invoke_local(ls, msg)
    else
        _invoke_local(ls, msg)
    end
    nothing
end

# Hand the message to the handler with the share/copy semantics the sub's `view`
# flag selects. Owned (default) copies so the handler may mutate freely; `view`
# shares the publisher's object read-only. A handler throw is logged, never fatal.
function _invoke_local(ls::LocalSubscription{T}, msg::T) where {T}
    try
        payload = ls.view ? msg : _ipc_copy(msg)
        ls.handler(payload)
    catch err
        err isa ShutdownException && return
        @error "intra-process handler threw" topic=ls.entity.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# The owned-delivery copy. The contract (DESIGN §Intra-process) is "mutate freely":
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
# opt-in for large arrays (§3.1 / the share/copy table).
_ipc_copy(msg::T) where {T} = isbitstype(T) ? msg : deepcopy(msg)
