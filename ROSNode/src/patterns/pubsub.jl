# The pub/sub pattern layer. `Publisher` and `Subscription` are `EndpointKind` enum
# values; this file gives them call-methods so `Publisher(node, topic, T)` and
# `Subscription(node, topic, T) do msg … end` read as constructors, following the
# `Timer(f, …)` precedent. Each handle wraps a generic `Entity` for the
# id/token/route/graph lifecycle and adds the typed surface.

using Zenoh: Zenoh, put
using ROSZenoh: ROSZenoh

export Publisher, Subscription, publish

# ── Publisher ─────────────────────────────────────────────────────────────────

"""
    PublisherHandle{T}

The handle returned by `Publisher(node, topic, T)` — a long-lived publisher for
ROS 2 message type `T`. Send with [`publish`](@ref). Wraps an [`Entity`](@ref) and
owns the per-endpoint attachment sequence counter. `isopen`/`close`-able and
reaped when its node closes.

The second type parameter pins the concrete Zenoh route (a plain or Advanced
publisher, set by the QoS durability) so the `put` stays monomorphic; callers only
spell `PublisherHandle{T}`.
"""
mutable struct PublisherHandle{T, R}
    const entity::Entity
    # Route type R pins the concrete Zenoh route (plain Publisher / AdvancedPublisher)
    # so put dispatches statically with no plain-vs-advanced branch on the hot path.
    const route::R
    @atomic seq::Int64           # per-endpoint attachment sequence_number
end

function _make_publisher(node::Node, topic::AbstractString, ::Type{T};
                         qos::QosProfile=default_qos(),
                         congestion_control=nothing, priority=nothing,
                         warmup::Union{Symbol, WarmupMode, Nothing}=nothing,
                         warmup_sync::Union{Bool, Nothing}=nothing,
                         warmup_sample=nothing) where {T}
    name = resolve_name(node, topic)
    ent = make_entity(node, Publisher, name, type_info_of(T); qos=qos)
    route = declare_publisher!(ent; congestion_control=congestion_control, priority=priority)
    pub = PublisherHandle{T, typeof(route)}(ent, route, 0)
    pol = _resolve_warmup(node, warmup, warmup_sync)
    _warmup!(pol, () -> _warm_publisher(pol, pub, warmup_sample))
    return pub
end

"""
    publish(pub::PublisherHandle{T}, msg::T) -> nothing

Serialize and publish `msg` on `pub`'s topic. One exact-size `encode` pass
produces the CDR payload, then the Zenoh `put` transmits by borrowing that heap
buffer (zero-copy into transport). Returns `nothing`.

Each message carries the rmw_zenoh per-message attachment `(sequence_number,
source_timestamp, source_gid)`: the sequence increments once per publisher
per call, the timestamp is publish-time wall-clock nanoseconds — distinct from a
message's own `header.stamp` — and the gid identifies this endpoint. A
subscriber's `on_message_lost` listener reads the sequence to detect gaps.

Publishing is a no-op (returns `nothing` without sending) in three cases:

- the publisher is closed,
- its node is closed, or
- [`isactive`](@ref) is false.

This is the drop-on-publish point of the managed-node lifecycle gate
([`LifecycleNode`](@ref) outside [`Active`](@ref)).

When the intra-process short-circuit is enabled, same-Context publisher↔subscriber
pairs hand the message over directly, bypassing serialize/Zenoh/decode. Those
subscribers open their Zenoh subscriber with a remote-only origin filter, so the
direct hand-off is their only copy of this publish; the `put` still goes out and
reaches subscribers in other Contexts normally.

Passing a `msg` whose type differs from `T` throws an `ArgumentError` naming the
expected and actual types.

```julia
pub = Publisher(node, "/chatter", std_msgs.msg.String)
publish(pub, std_msgs.msg.String(data = "hello"))
```
"""
function publish(pub::PublisherHandle{T}, msg::T) where {T}
    e = pub.entity
    isopen(e) || return nothing
    isactive(e) || return nothing                   # lifecycle gate: inactive node drops the publish

    payload = encode(msg)
    ts = nanoseconds(Dates.now(e.node, System()))   # System clock stamps wire timestamps
    # Warm-up compiles the encode/attach path but skips the put and the seq commit:
    # the seq counter is wire state and the first real message must carry seq=1, the
    # invariant a subscriber's gap detection relies on.
    if _WARMUP[]
        encode_attachment((@atomic pub.seq) + 1, ts, gid(e))
        return nothing
    end
    seq = (@atomic pub.seq += 1)
    # The attachment (sequence_number, source_timestamp, fixed 16-byte source_gid) is
    # byte-exact and mandatory: a real ROS 2 peer unwraps it and panics if absent.
    attach = encode_attachment(seq, ts, gid(e))
    deliver_local(pub, msg)                           # intra-process short-circuit (no-op when disabled)
    put(pub.route, payload; attachment=attach)        # statically dispatched on route type R
    return nothing
end

# Name the wrong-message-type mistake; the bare MethodError is opaque.
publish(pub::PublisherHandle{T}, msg) where {T} =
    throw(ArgumentError("publish: expected a $(T), got a $(typeof(msg))"))

Base.isopen(pub::PublisherHandle) = isopen(pub.entity)
Base.close(pub::PublisherHandle) = close(pub.entity)
Base.show(io::IO, pub::PublisherHandle{T}) where {T} =
    print(io, "Publisher(", pub.entity.endpoint.topic, ", ", nameof(T),
          isopen(pub) ? "" : ", closed", ")")

"""
    entity(pub::PublisherHandle)
    entity(sub::SubscriptionHandle)
    entity(sub::DynamicSubscriptionHandle)
    entity(s::ServiceHandle)
    entity(c::ServiceClient) -> Entity

The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle) wrapped by
the handle.
"""
entity(pub::PublisherHandle) = pub.entity

# ── Subscription ──────────────────────────────────────────────────────────────

"""
    SubscriptionHandle{T}

The handle returned by `Subscription(node, topic, T) do msg … end` — a subscription
for ROS 2 message type `T`. Wraps the generic [`Entity`](@ref), whose consumer task
decodes each received sample and runs the stored handler under the subscription's
[`Concurrency`](@ref) policy ([`Serial`](@ref) or [`Parallel`](@ref)).
`isopen`/`close`-able and reaped when its node closes.
"""
mutable struct SubscriptionHandle{T}
    const entity::Entity
end

function _make_subscription(handler, node::Node, topic::AbstractString, ::Type{T};
                            qos::QosProfile=default_qos(), view::Union{Bool, ViewMode}=Owned(),
                            concurrency::Concurrency=Serial(),
                            force_relatch::Bool=false,
                            match::Union{Symbol, MatchPolicy}=ExactMatch(),
                            warmup::Union{Symbol, WarmupMode, Nothing}=nothing,
                            warmup_sync::Union{Bool, Nothing}=nothing,
                            warmup_sample=nothing) where {T}
    weak = _is_weak(_match_policy(match))
    view = _view_mode(view)
    name = resolve_name(node, topic)
    ent = make_entity(node, Subscription, name, type_info_of(T); qos=qos)
    # transient_local routes to an AdvancedSubscriber with latched history. The
    # transient_local re-latch dedup keys on payload content-hash, since cache replays
    # arrive as bare payloads with null attachment and timestamp; force_relatch bypasses
    # the dedup to re-deliver every latched sample for accumulating handlers.
    declare_subscription!(ent, T, handler; view=view, concurrency=concurrency,
                          force_relatch=force_relatch, weak = weak)
    pol = _resolve_warmup(node, warmup, warmup_sync)
    _warmup!(pol, () -> _warm_subscription(pol, ent, T, handler, view, warmup_sample))
    return SubscriptionHandle{T}(ent)
end

Base.isopen(sub::SubscriptionHandle) = isopen(sub.entity)
Base.close(sub::SubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::SubscriptionHandle{T}) where {T} =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", ", nameof(T),
          isopen(sub) ? "" : ", closed", ")")

entity(sub::SubscriptionHandle) = sub.entity

# ── DynamicSubscription: the keyexpr-only variant ─────────────────────────────
# A subscription with a runtime-resolved message type: it wildcards the data route,
# resolves each sample's type at runtime, and decodes + dispatches through
# `invokelatest` to cross the world-age boundary into the runtime-born type.

"""
    DynamicSubscriptionHandle

The handle returned by the type-less `Subscription(node, topic) do msg … end` — a
subscription whose message type is resolved at runtime. The data route is
wildcarded and each sample's type is resolved per sample, walking these sources in
order until one resolves:

1. the registry,
2. the project cache,
3. ament,
4. a wire `GetTypeDescription` query.

Wraps the generic [`Entity`](@ref); `isopen`/`close`-able and reaped when its node
closes.
"""
mutable struct DynamicSubscriptionHandle
    const entity::Entity
end

function _make_dynamic_subscription(handler, node::Node, topic::AbstractString;
                                    qos::QosProfile=default_qos(),
                                    view::Union{Bool, ViewMode}=Owned(),
                                    concurrency::Concurrency=Serial(),
                                    warmup::Union{Symbol, WarmupMode, Nothing}=nothing,
                                    warmup_sync::Union{Bool, Nothing}=nothing)
    # Per-sample realize! runs Core.eval codegen on the dispatch worker task, never on
    # a foreign libzenohc callback thread: codegen on this caller's task, with the
    # node's Zenoh sessions live, stalls on a JIT-vs-Zenoh GC race.
    name = resolve_name(node, topic)
    # No type identity: liveliness advertises the empty-type placeholder and the
    # data route wildcards type+hash (the type rides each sample's keyexpr).
    ent = make_entity(node, Subscription, name, nothing; qos=qos)
    # No compile-time T, so warm-up defers to first sight of each runtime type. :execute
    # degrades to :precompile here, since running a discovered handler on a synthesized
    # sample is the type manifest's job.
    pol = _resolve_warmup(node, warmup, warmup_sync)
    declare_subscription!(ent, handler; view=view, concurrency=concurrency, warmup=pol)
    return DynamicSubscriptionHandle(ent)
end

Base.isopen(sub::DynamicSubscriptionHandle) = isopen(sub.entity)
Base.close(sub::DynamicSubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::DynamicSubscriptionHandle) =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", dynamic",
          isopen(sub) ? "" : ", closed", ")")

entity(sub::DynamicSubscriptionHandle) = sub.entity

# ── enum-instance call-methods: the constructor spelling ──────────────────────
# `Publisher`/`Subscription` are `EndpointKind` values of one type, so a single
# call-method per arity on `EndpointKind` branches on the value to its builder. The
# do-block form desugars to a function-first call, so the handler-carrying arity
# takes `f` first, following the `Timer(f, …)` precedent.

"""
    Publisher(node::Node, topic, ::Type{T};
              qos=default_qos(), congestion_control=nothing, priority=nothing,
              warmup=nothing, warmup_sync=nothing, warmup_sample=nothing) -> PublisherHandle{T}

Declare a publisher for ROS 2 message type `T` on `topic` and return a long-lived
`PublisherHandle{T}`. Send messages with [`publish`](@ref).

`Publisher` is the `EndpointKind` enum value (re-exported from ROSZenoh); this
call-method gives it the constructor spelling `Publisher(node, topic, T)`,
paralleling `Timer(f, …)`. The kind is fixed by the spelling; a bare
`Subscription(node, topic, T)` in this 3-arg position raises an `ArgumentError`
demanding a do-block handler, and the other kinds raise too.

Construction resolves `topic` against the node's namespace, builds the
`ROSZenoh.EndpointEntity` (id, kind, topic, type identity, QoS), declares its
liveliness token so peers discover the endpoint, records it in the local discovery
graph, and declares the Zenoh data route on the topic keyexpr. The
handle is tracked on `node`, so `close(node)` reaps it; `close(pub)` undeclares the
route and withdraws the token on its own.

Keyword arguments shaping the route:

- `qos` — the ROS 2 QoS profile.
- `reliability` — maps onto the Zenoh publisher (`:reliable`/`:best_effort`).
- `durability=:transient_local` — selects the route that latches history to
  late-joining subscribers (an `AdvancedPublisher`).
- `congestion_control` — passes through to the Zenoh publisher.
- `priority` — passes through to the Zenoh publisher.

`warmup`, `warmup_sync`, and `warmup_sample` select the warm-up policy (a
[`WarmupPolicy`](@ref)) that pre-JITs the encode/decode dispatch chain so the first
real `publish` runs at full speed. Under [`Execute`](@ref) the synthesized publish
compiles the path with the wire `put` suppressed. Each defaults to the node's policy.

```julia
pub = Publisher(node, "/chatter", std_msgs.msg.String)
publish(pub, std_msgs.msg.String(data = "hello"))
```
"""
(k::EndpointKind)(node::Node, topic::AbstractString, ::Type{T}; kwargs...) where {T} =
    k === Publisher ? _make_publisher(node, topic, T; kwargs...) :
    k === Subscription ?
        throw(ArgumentError("Subscription requires a handler: `Subscription(node, topic, T) do msg … end`")) :
        throw(ArgumentError("$(k) is not a pub/sub kind (see Service/Client for request/reply)"))

# Arg2 is the Node here, vs the typed handler-form in service.jl (handler, node, …),
# so this dynamic arity stays unambiguous against the other EndpointKind call-methods.
"""
    Subscription(node::Node, topic;
                 qos=default_qos(), view=Owned(), concurrency=Serial(),
                 warmup=nothing, warmup_sync=nothing) do msg … end
        -> DynamicSubscriptionHandle

Declare a dynamic subscription whose message type is resolved at runtime from each
incoming sample, returning a [`DynamicSubscriptionHandle`](@ref). Use it
when the topic's type is unknown ahead of time (a recorder, a bridge); graduate to
`Subscription(node, topic, T)` once the type is known for the min-copy fast path.

This call-method is the type-less spelling of the `Subscription` enum value,
distinct from the typed `Subscription(node, topic, T) do … end`; `Subscription` is
the only kind with a type-less form — a `Service` needs its type to know
request/response. The data route wildcard-matches every type published on the
topic; each sample carries its own `(name, hash)`, resolved by walking these
sources in order until one resolves:

1. the registry,
2. the project cache,
3. ament,
4. a wire `GetTypeDescription` query.

The handler receives the **real generated type** `msg::T`, not a boxed `Any`.
Decode and the handler call cross the world-age boundary via `invokelatest`, so
only that dispatch boundary pays the hop while the handler body stays type-stable.
The first sample of each type pays type discovery plus codegen; subsequent samples
are fast registry lookups. Resolution and codegen run on a dispatch worker task off
the receive thread, so codegen's GC completes while the receiver keeps draining.

`view` is a [`ViewMode`](@ref), with `true`/`false` shorthand:

- [`Owned`](@ref) — the default.
- [`Checked`](@ref)
- [`Unchecked`](@ref)

Here the resulting `CDRView` aliases the runtime-resolved type's payload.
`concurrency` is a [`Concurrency`](@ref) ([`Serial`](@ref) or [`Parallel`](@ref))
as for the typed form. Warm-up is deferred to first sight of each runtime type, and
[`Execute`](@ref) degrades to [`Precompile`](@ref) here (running a discovered handler
on a synthesized sample is the manifest's job).

```julia
sub = Subscription(node, "/chatter") do msg
    @info "heard a \$(typeof(msg))" msg
end
```
"""
(k::EndpointKind)(handler, node::Node, topic::AbstractString; kwargs...) =
    k === Subscription ? _make_dynamic_subscription(handler, node, topic; kwargs...) :
        throw(ArgumentError("$(k) has no type-less form; only `Subscription(node, topic) do … end` \
                             omits the type (e.g. Service needs `Service(node, name, SrvType) do … end`)"))

# The typed handler-form call-method `Subscription(node, topic, T) do msg … end`
# lives once in service.jl, where it fans Subscription → `_make_subscription` and
# Service → `_make_service`. A duplicate Subscription-only definition here shares
# that signature, and precompile rejects the resulting method overwrite at load.
