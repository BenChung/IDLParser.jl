# The pub/sub pattern layer. Publishers and subscriptions are created with
# *type-constructors* — `Publisher(node, topic, T)` / `Subscription(node, topic, T)
# do msg … end` — following the `Timer(f, …)` precedent: a registered route
# that lives until `close` and dies with its node. `Publisher`/`Subscription` are
# the `EndpointKind` enum instances re-exported by core.jl; this file gives those
# instances call-methods so the spelling reads as a constructor. Each holds a
# generic `Entity` (node.jl) for the id/token/route/graph lifecycle and adds the
# typed surface: the message type, the publish path, the handler.
#
# Data plane: publish sizes-then-serializes once (`encode`) and `put`s with
# the per-message attachment; receive decodes owned (default) or view (opt-in)
# through the `Entity`'s consumer task (node.jl's `declare_subscription!`).

using Zenoh: Zenoh, put
using ROSZenoh: ROSZenoh

export Publisher, Subscription, publish

# ── Publisher ─────────────────────────────────────────────────────────────────
# A long-lived publish handle: an `Entity` carrying the Zenoh `Publisher` route,
# the message type, and the per-endpoint attachment sequence counter. The
# gid lives on the `Entity`; `publish` stamps `(seq, source_timestamp, gid)` on
# every `put`.

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
    const route::R               # concrete Zenoh route (plain Publisher or AdvancedPublisher)
    @atomic seq::Int64           # per-endpoint attachment sequence_number
end

function _make_publisher(node::Node, topic::AbstractString, ::Type{T};
                         qos::QosProfile=default_qos(),
                         congestion_control=nothing, priority=nothing,
                         warmup::Union{Symbol, Nothing}=nothing,
                         warmup_sync::Union{Bool, Nothing}=nothing,
                         warmup_sample=nothing) where {T}
    name = resolve_name(node, topic)
    ent = make_entity(node, Publisher, name, type_info_of(T); qos=qos)
    # `transient_local` routes to an AdvancedPublisher (sample cache); the
    # concrete route type flows into `PublisherHandle{T,R}` so `put` stays monomorphic.
    route = declare_publisher!(ent; congestion_control=congestion_control, priority=priority)
    pub = PublisherHandle{T, typeof(route)}(ent, route, 0)
    # Warm-up: precompile the publish/encode chain (and, under :execute, publish once
    # with the `put` null-routed) so the first real publish isn't a JIT spike.
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
pairs bypass serialize/Zenoh/decode; the intra-process delivery layer
(`performance/intraprocess.jl`) owns the mechanism. Those subscribers open their
Zenoh subscriber with a local-origin filter (`local_origin`), so the direct delivery
is their only copy of this publish; the `put` still goes out and reaches subscribers
in other Contexts normally.

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
    isactive(e) || return nothing                   # lifecycle gate: inactive node ⇒ drop

    payload = encode(msg)
    ts = nanoseconds(Dates.now(e.node, System()))   # publish-time wall ns
    # The :execute warm-up compiles and runs the encode/attach path in-memory but
    # skips the wire `put` and the seq commit — sequence numbers are wire state, so
    # the first real message must carry seq=1. The speculative read may duplicate a
    # racing real publish's seq; harmless, the warm-up attachment never leaves this
    # frame. The hot path reads only the scoped value: `publish` never runs during
    # package precompilation, so the `jl_generating_output` check in `is_warming()`
    # is unneeded here.
    if _WARMUP[]
        encode_attachment((@atomic pub.seq) + 1, ts, gid(e))
        return nothing
    end
    seq = (@atomic pub.seq += 1)
    attach = encode_attachment(seq, ts, gid(e))
    deliver_local(pub, msg)                           # intra-process short-circuit: hand to same-Context subs directly (no-op when disabled)
    put(pub.route, payload; attachment=attach)        # monomorphic on R (plain / advanced route)
    return nothing
end

# A non-`T` payload is the common "wrong message type for this topic" mistake; a
# `MethodError` is opaque, so name it.
publish(pub::PublisherHandle{T}, msg) where {T} =
    throw(ArgumentError("publish: expected a $(T), got a $(typeof(msg))"))

Base.isopen(pub::PublisherHandle) = isopen(pub.entity)
Base.close(pub::PublisherHandle) = close(pub.entity)
Base.show(io::IO, pub::PublisherHandle{T}) where {T} =
    print(io, "Publisher(", pub.entity.endpoint.topic, ", ", nameof(T),
          isopen(pub) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle)."
entity(pub::PublisherHandle) = pub.entity

# ── Subscription ──────────────────────────────────────────────────────────────
# A subscribe handle: an `Entity` carrying the FIFO Zenoh subscriber route and its
# consumer task (node.jl's dispatch runtime). The handler is a plain `do`-block;
# decode is owned by default or a zero-copy `CDRView` under `view=true`.

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
                            match::Symbol=:exact,
                            warmup::Union{Symbol, Nothing}=nothing,
                            warmup_sync::Union{Bool, Nothing}=nothing,
                            warmup_sample=nothing) where {T}
    match in (:exact, :weak) ||
        throw(ArgumentError("Subscription `match` must be :exact or :weak, got :$match"))
    view = _view_mode(view)
    name = resolve_name(node, topic)
    ent = make_entity(node, Subscription, name, type_info_of(T); qos=qos)
    # `transient_local` ⇒ an AdvancedSubscriber with latched history.
    # `force_relatch` is the latched-history escape hatch: re-deliver on every
    # activation regardless of sequence (for idempotent handlers that rebuild from
    # latched inputs); the default dedups replays by novelty.
    declare_subscription!(ent, T, handler; view=view, concurrency=concurrency,
                          force_relatch=force_relatch, weak = match === :weak)
    # Warm-up: precompile decode + the handler-dispatch frame (and, under :execute, run
    # the handler once on a sample) so the first real message isn't a JIT spike.
    pol = _resolve_warmup(node, warmup, warmup_sync)
    _warmup!(pol, () -> _warm_subscription(pol, ent, T, handler, view, warmup_sample))
    return SubscriptionHandle{T}(ent)
end

Base.isopen(sub::SubscriptionHandle) = isopen(sub.entity)
Base.close(sub::SubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::SubscriptionHandle{T}) where {T} =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", ", nameof(T),
          isopen(sub) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle)."
entity(sub::SubscriptionHandle) = sub.entity

# ── DynamicSubscription: the keyexpr-only variant ─────────────────────────────
# A subscription with a runtime-resolved message type. It wildcards the data route
# so samples of any type on the topic arrive, resolves each sample's type at runtime
# via the type-discovery path, and decodes + dispatches through `invokelatest`. The
# user observes the real typed `msg`, learns what to write, then graduates to the
# static `Subscription(node, topic, T)` for the min-copy fast path.

"""
    DynamicSubscriptionHandle

The handle returned by the type-less `Subscription(node, topic) do msg … end` — a
subscription whose message type is resolved at runtime. The data route is
wildcarded and each sample's type is resolved per sample (registry, then project
cache, then ament, then a wire `GetTypeDescription` query). Wraps the generic
[`Entity`](@ref); `isopen`/`close`-able and reaped when its node closes.
"""
mutable struct DynamicSubscriptionHandle
    const entity::Entity
end

function _make_dynamic_subscription(handler, node::Node, topic::AbstractString;
                                    qos::QosProfile=default_qos(),
                                    view::Union{Bool, ViewMode}=Owned(),
                                    concurrency::Concurrency=Serial(),
                                    warmup::Union{Symbol, Nothing}=nothing,
                                    warmup_sync::Union{Bool, Nothing}=nothing)
    # Trap: keep codegen off the caller's task. Per-sample `realize!` runs on the
    # dispatch *worker* (node.jl), off the recv thread and while the receiver drains
    # the FIFO GC-safe, so the codegen's GC always completes. `Core.eval` codegen on
    # this task — with the node's Zenoh sessions live (its `get_type_description`
    # queryable, liveliness traffic) — stalls on a JIT-vs-Zenoh GC race.
    name = resolve_name(node, topic)
    # No type identity: liveliness advertises the empty-type placeholder and the
    # data route wildcards type+hash (the type rides each sample's keyexpr).
    ent = make_entity(node, Subscription, name, nothing; qos=qos)
    # Warm-up: no compile-time `T`, so warm-up is deferred to *first sight* of each
    # runtime type (the dynamic consumer warms its codec via `invokelatest`). The
    # resolved policy rides into the consumer; `:execute` degrades to `:precompile`
    # there (a discovered handler can't be safely run on a synthesized sample —
    # that's the manifest's job).
    pol = _resolve_warmup(node, warmup, warmup_sync)
    declare_subscription!(ent, handler; view=view, concurrency=concurrency, warmup=pol)
    return DynamicSubscriptionHandle(ent)
end

Base.isopen(sub::DynamicSubscriptionHandle) = isopen(sub.entity)
Base.close(sub::DynamicSubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::DynamicSubscriptionHandle) =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", dynamic",
          isopen(sub) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle)."
entity(sub::DynamicSubscriptionHandle) = sub.entity

# ── enum-instance call-methods: the constructor spelling ──────────────────────
# `Publisher`/`Subscription` are `EndpointKind` *values* (same type), so a single
# call-method on `EndpointKind` branches on the value — `Publisher(node, …)` and
# `Subscription(node, …)` route to their builders, any other kind errors. The
# do-block form desugars to a function-first call, so the handler-carrying
# `Subscription` arity takes `f` first (the `Timer(f, …)` precedent).

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

`qos` is the ROS 2 QoS profile. `reliability` maps onto the Zenoh publisher
(`:reliable`/`:best_effort`); `durability=:transient_local` selects the route that
latches history to late-joining subscribers (an `AdvancedPublisher`).
`congestion_control` and `priority` pass through to the
Zenoh publisher.

`warmup`, `warmup_sync`, and `warmup_sample` select the warm-up policy
(precompile/execute/off, sync/async) that pre-JITs the encode/decode dispatch chain
so the first real `publish` isn't a JIT spike; the warm-up layer (`base/core.jl`)
owns it. Under `:execute` the synthesized publish runs with the wire `put`
suppressed. Each defaults to the node's policy.

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

# Distinct arity from both the no-handler form above (node, topic, ::Type) and the
# typed handler-form in service.jl (handler, node, name, ::Type), and the arg types
# don't overlap (here arg2 is the Node), so there's no method ambiguity.
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
topic; each sample carries its own `(name, hash)`, resolved through the registry,
then the project cache, then ament, then a wire `GetTypeDescription` query.

The handler receives the **real generated type** `msg::T`, not a boxed `Any`.
Decode and the handler call cross the world-age boundary via `invokelatest`, so
only that dispatch boundary pays the hop while the handler body stays type-stable.
The first sample of each type pays type discovery plus codegen; subsequent samples
are fast registry lookups. Resolution and codegen run on a dispatch worker task off
the receive thread, so codegen's GC completes while the receiver keeps draining.

`view` is a [`ViewMode`](@ref) — [`Owned`](@ref) (the default), [`Checked`](@ref),
or [`Unchecked`](@ref), with `true`/`false` shorthand; here the resulting `CDRView`
aliases the runtime-resolved type's payload. `concurrency` is a [`Concurrency`](@ref)
([`Serial`](@ref) or [`Parallel`](@ref)) as for the typed form. Warm-up is deferred
to first sight of each runtime type, and `:execute` degrades to `:precompile` here
(running a discovered handler on a synthesized sample is the manifest's job).

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

# The handler-form call-method (`Subscription(node, topic, T) do msg … end`) is
# defined once, in service.jl — it fans Subscription → `_make_subscription` and
# Service → `_make_service`. service.jl is included after this file; defining the
# Subscription-only form here too would be a method *overwrite*, which precompile
# forbids (the two signatures are identical). So the single owner is service.jl.
