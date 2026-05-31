# §3/§6 the pub/sub pattern layer. Publishers and subscriptions are created with
# *type-constructors* — `Publisher(node, topic, T)` / `Subscription(node, topic, T)
# do msg … end` — following the `Timer(f, …)` precedent (§6): a registered route
# that lives until `close` and dies with its node. `Publisher`/`Subscription` are
# the `EndpointKind` enum instances re-exported by core.jl; this file gives those
# instances call-methods so the spelling reads as a constructor. Each holds a
# generic `Entity` (node.jl) for the id/token/route/graph lifecycle and adds the
# typed surface: the message type, the publish path, the handler.
#
# Data plane (§3): publish sizes-then-serializes once (`encode`) and `put`s with
# the per-message attachment; receive decodes owned (default) or view (opt-in)
# through the `Entity`'s consumer task (node.jl's `declare_subscription!`).

using Zenoh: Zenoh, put
using ROSZenoh: ROSZenoh

export Publisher, Subscription, publish

# ── Publisher (§3.3/§6) ───────────────────────────────────────────────────────
# A long-lived publish handle: an `Entity` carrying the Zenoh `Publisher` route,
# the message type, and the per-endpoint attachment sequence counter (§3.4). The
# gid lives on the `Entity`; `publish` stamps `(seq, source_timestamp, gid)` on
# every `put`.

"""
    PublisherHandle{T}

A publisher for message type `T` on a topic (§6). Wraps the generic [`Entity`](@ref)
(id, liveliness token, Zenoh `Publisher` route) and owns the monotone attachment
sequence counter (§3.4). `close`-able; dies with its node.

Constructed via the `Publisher(node, topic, T)` spelling — see [`Publisher`](@ref).
"""
mutable struct PublisherHandle{T}
    const entity::Entity
    @atomic seq::Int64           # per-endpoint attachment sequence_number (§3.4)
end

"""
    Publisher(node, topic, ::Type{T}; qos=default_qos(),
              congestion_control=nothing, priority=nothing) -> PublisherHandle{T}

Declare a publisher for ROS message type `T` on `topic` (§6). Materializes the
`ROSZenoh.EndpointEntity`, declares its liveliness token and the Zenoh data route
(`topic_keyexpr`, §2.2), and tracks it on `node` so `close(node)` reaps it.

`Publisher` here is the `EndpointKind` enum instance given a call-method; the kind
is fixed by the spelling. Publish with [`publish`](@ref).
"""
# `transient_local` is *advertised* correctly in the liveliness QoS token, but the
# latched cache/history behavior needs Zenoh advanced pub/sub, which Zenoh.jl does
# not yet expose (see ROSNode/HANDOFF-zenoh-advanced-pubsub.md, D4). Until that
# lands, a `transient_local` endpoint behaves as `volatile` (no latched delivery).
# Warn once so it's not a silent surprise.
function _warn_transient_local(qos::QosProfile, kind::AbstractString)
    qos.durability === :transient_local && @warn(
        "transient_local $kind: QoS advertised, but latched delivery is pending the \
         Zenoh advanced-pubsub handoff — behaves as volatile for now \
         (ROSNode/HANDOFF-zenoh-advanced-pubsub.md)", maxlog=1)
    nothing
end

function _make_publisher(node::Node, topic::AbstractString, ::Type{T};
                         qos::QosProfile=default_qos(),
                         congestion_control=nothing, priority=nothing) where {T}
    _warn_transient_local(qos, "publisher")
    name = resolve_name(node, topic)
    ent = make_entity(node, Publisher, name, type_info_of(T); qos=qos)
    declare_publisher!(ent; congestion_control=congestion_control, priority=priority)
    return PublisherHandle{T}(ent, 0)
end

"""
    publish(pub::PublisherHandle, msg)

Serialize and publish `msg` (§3.3): one exact-size `encode` pass, then a Zenoh
`put` that *borrows* the heap buffer (no copy into transport). Each message
carries the per-message attachment `(sequence_number, source_timestamp,
source_gid)` (§3.4): the sequence increments per publisher, the timestamp is
publish-time wall ns (distinct from `header.stamp`, §7), the gid is the entity's.

A publish on a closed publisher (or closed node) is a no-op — gated like every
other entity at dispatch (§14.2 will route lifecycle gating through here too).
"""
function publish(pub::PublisherHandle{T}, msg::T) where {T}
    e = pub.entity
    isopen(e) || return nothing
    route = e._route
    route === nothing && return nothing

    payload = encode(msg)
    seq = (@atomic pub.seq += 1)
    ts = nanoseconds(Dates.now(e.node, System()))   # publish-time wall ns (§3.4)
    attach = encode_attachment(seq, ts, gid(e))
    put(route::ZPublisher, payload; attachment=attach)
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

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle, §6)."
entity(pub::PublisherHandle) = pub.entity

# ── Subscription (§3.1/§4/§6) ─────────────────────────────────────────────────
# A subscribe handle: an `Entity` carrying the FIFO Zenoh subscriber route and its
# consumer task (node.jl's dispatch runtime). The handler is a plain `do`-block;
# decode is owned by default or a zero-copy `CDRView` under `view=true` (§3.1).

"""
    SubscriptionHandle{T}

A subscription for message type `T` on a topic (§6). Wraps the generic
[`Entity`](@ref), whose consumer task (started by `declare_subscription!`, §4)
decodes each sample and runs the stored handler per the `concurrency` policy.
`close`-able; dies with its node.

Constructed via the `Subscription(node, topic, T) do msg … end` spelling — see
[`Subscription`](@ref).
"""
mutable struct SubscriptionHandle{T}
    const entity::Entity
end

"""
    Subscription(node, topic, ::Type{T}; qos=default_qos(), view=false,
                 concurrency=Serial()) do msg … end -> SubscriptionHandle{T}

Declare a subscription for ROS message type `T` on `topic` and start its dispatch
runtime (§4/§6). The `do`-block handler runs once per received message — owned by
default (storable / forwardable / spawnable, §3.1) or over a zero-copy `CDRView`
aliasing the payload when `view = true` (valid only for the handler's duration,
§3.2; `collect`/`decode_owned` to keep the data).

`concurrency` is `Serial()` (one handler at a time on a single sticky thread,
order preserved, no user-side locks needed — the default, D3) or `Parallel(n)`
(up to `n` handlers on OS threads, order not preserved, opt-in, D2). A handler
throw is logged, never fatal.

`Subscription` is the `EndpointKind` enum instance given a call-method; the kind
is fixed by the spelling.
"""
function _make_subscription(handler, node::Node, topic::AbstractString, ::Type{T};
                            qos::QosProfile=default_qos(), view::Bool=false,
                            concurrency::Concurrency=Serial()) where {T}
    _warn_transient_local(qos, "subscription")
    name = resolve_name(node, topic)
    ent = make_entity(node, Subscription, name, type_info_of(T); qos=qos)
    declare_subscription!(ent, T, handler; view=view, concurrency=concurrency)
    return SubscriptionHandle{T}(ent)
end

Base.isopen(sub::SubscriptionHandle) = isopen(sub.entity)
Base.close(sub::SubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::SubscriptionHandle{T}) where {T} =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", ", nameof(T),
          isopen(sub) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle, §6)."
entity(sub::SubscriptionHandle) = sub.entity

# ── DynamicSubscription (§11/D5 S5): the keyexpr-only variant ──────────────────
# A subscription created *without* a compile-time type. It wildcards the data route
# (so samples of any type on the topic arrive), and resolves each sample's type at
# runtime via §11 discovery, decoding + dispatching through `invokelatest`. The
# user observes the real typed `msg`, learns what to write, then graduates to the
# static `Subscription(node, topic, T)` for the min-copy fast path (D5's point).

"""
    DynamicSubscriptionHandle

A keyexpr-only subscription (§11/D5): no compile-time message type — the data route
is wildcarded and each sample's type is resolved at runtime (registry → cache →
ament → wire `GetTypeDescription`). Wraps the generic [`Entity`](@ref); `close`-able
and dies with its node. Constructed via the type-less
`Subscription(node, topic) do msg … end` spelling. Owned-only (no `view`).
"""
mutable struct DynamicSubscriptionHandle
    const entity::Entity
end

"""
    Subscription(node, topic; qos=default_qos(), concurrency=Serial()) do msg … end
        -> DynamicSubscriptionHandle

The type-less subscription (§11/D5 S5): resolve the message type at runtime from
each sample (no compile-time `T`). The handler receives the **real generated type**
`msg::T` (not a boxed `Any`) — decode and the handler call cross the world-age
boundary via `invokelatest`, so the handler *body* stays type-stable and fast while
only the dispatch boundary pays the hop. The first sample of a type pays discovery
+ codegen; the rest are fast registry lookups.

Owned-only — the zero-copy `view` path needs the static fast path; graduating to
`Subscription(node, topic, T)` is how you get it (the `@info` on first sight tells
you what to write).
"""
function _make_dynamic_subscription(handler, node::Node, topic::AbstractString;
                                    qos::QosProfile=default_qos(),
                                    concurrency::Concurrency=Serial())
    _warn_transient_local(qos, "subscription")
    # NB: no codegen on this (the caller's) task. Per-sample `realize!` runs on the
    # dispatch *worker* (node.jl), off the recv thread and while the receiver drains
    # the FIFO GC-safe, so the codegen's GC always completes. Running `Core.eval`
    # codegen here instead — while the node already has live Zenoh sessions (its own
    # `get_type_description` queryable, liveliness traffic) — is the JIT-racing-Zenoh
    # GC stall the worker design exists to avoid (§11/D8).
    name = resolve_name(node, topic)
    # No type identity: liveliness advertises the empty-type placeholder and the
    # data route wildcards type+hash (the type rides each sample's keyexpr, §11).
    ent = make_entity(node, Subscription, name, nothing; qos=qos)
    declare_subscription!(ent, handler; concurrency=concurrency)
    return DynamicSubscriptionHandle(ent)
end

Base.isopen(sub::DynamicSubscriptionHandle) = isopen(sub.entity)
Base.close(sub::DynamicSubscriptionHandle) = close(sub.entity)
Base.show(io::IO, sub::DynamicSubscriptionHandle) =
    print(io, "Subscription(", sub.entity.endpoint.topic, ", dynamic",
          isopen(sub) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle, §6)."
entity(sub::DynamicSubscriptionHandle) = sub.entity

# ── enum-instance call-methods: the §6 constructor spelling ───────────────────
# `Publisher`/`Subscription` are `EndpointKind` *values* (same type), so a single
# call-method on `EndpointKind` branches on the value — `Publisher(node, …)` and
# `Subscription(node, …)` route to their builders, any other kind errors. The
# do-block form desugars to a function-first call, so the handler-carrying
# `Subscription` arity takes `f` first (the `Timer(f, …)` precedent, §6).

# Publisher(node, topic, T; …) — no handler.
(k::EndpointKind)(node::Node, topic::AbstractString, ::Type{T}; kwargs...) where {T} =
    k === Publisher ? _make_publisher(node, topic, T; kwargs...) :
    k === Subscription ?
        throw(ArgumentError("Subscription requires a handler: `Subscription(node, topic, T) do msg … end`")) :
        throw(ArgumentError("$(k) is not a pub/sub kind (see Service/Client for §8)"))

# Subscription(node, topic) do msg … end — the type-less (dynamic, D5 S5) form.
# Distinct arity from both the no-handler form above (node, topic, ::Type) and the
# typed handler-form in service.jl (handler, node, name, ::Type), and the arg types
# don't overlap (here arg2 is the Node), so there's no method ambiguity. Only
# Subscription has a type-less form — a Service needs a type to know request/response.
(k::EndpointKind)(handler, node::Node, topic::AbstractString; kwargs...) =
    k === Subscription ? _make_dynamic_subscription(handler, node, topic; kwargs...) :
        throw(ArgumentError("$(k) has no type-less form; only `Subscription(node, topic) do … end` \
                             omits the type (e.g. Service needs `Service(node, name, SrvType) do … end`)"))

# The handler-form call-method (`Subscription(node, topic, T) do msg … end`) is
# defined once, in service.jl — it fans Subscription → `_make_subscription` and
# Service → `_make_service`. service.jl is included after this file; defining the
# Subscription-only form here too would be a method *overwrite*, which precompile
# forbids (the two signatures are identical). So the single owner is service.jl.
