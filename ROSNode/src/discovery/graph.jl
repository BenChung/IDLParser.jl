# §12 Discovery, the graph & QoS — all views over one change stream. The Context
# already owns the discovery index (`GraphIndex`), the liveliness subscriber that
# feeds it, and the inject/remove seam for our own entities (context.jl). This
# file is everything built *on top* of that one stream:
#
#   - graph queries (endpoints / publishers_info / count_* / topic_names_and_types
#     / node_names) over the index snapshot;
#   - graph waits (service_is_ready / wait_for_service / wait_for_graph_change),
#     parking on the index's `changed` condition, shutdown-interruptible (§14);
#   - the mismatch detectors (type-mismatch, QoS-incompat) as `on_graph_change`
#     listeners — deduped + throttled — firing `on_type_mismatch` /
#     `on_qos_incompatible` events (§12.2);
#   - the QoS event surface sourced from the same stream + the attachment:
#     liveliness-changed (from the stream) and message-lost (attachment `seq`
#     gaps). deadline/lifespan enforcement is a per-endpoint concern left as
#     TODO(§12.3) — the events feed off the data plane, not the graph.
#
# Everything here keys off `EndpointInfo` (context.jl): node identity, kind,
# resolved topic, `TypeInfo` (name + RIHS01 hash), `QosProfile`, gid, `is_local`.

using Zenoh: Zenoh
using ROSZenoh: ROSZenoh, EndpointKind, Publisher, Subscription, Service, Client,
                TypeInfo, QosProfile, qos_compatible, QosIncompatibility,
                parse_topic_keyexpr
import ROSZenoh

export endpoints, publishers_info, subscriptions_info,
       count_publishers, count_subscribers,
       topic_names_and_types, node_names,
       service_is_ready, wait_for_service, wait_for_action_server, wait_for_graph_change,
       on_type_mismatch, on_qos_incompatible, on_message_lost,
       TypeMismatch, QosIncompatible, MessageLost,
       qos_compatible, QosIncompatibility

# ── graph queries (§12.1) ───────────────────────────────────────────────────
# Symmetric self+remote queries over the index snapshot. Each filters
# `endpoints_snapshot(ctx)` (a consistent instant, copied under the lock by
# context.jl) — never the live dict, so a query can't race a discovery update.

"""
    endpoints(node_or_ctx; topic=nothing, kind=nothing, node=nothing,
              namespace=nothing) -> Vector{EndpointInfo}

Return every endpoint in the discovery graph matching the given filters, the
primary entry into the ROS 2 graph as this stack sees it (§12.1). Each result is
an `EndpointInfo` carrying node identity, `kind`, resolved topic, `TypeInfo`
(type name + RIHS01 hash), `QosProfile`, gid, and an `is_local` flag.

Filters combine conjunctively, and a `nothing` filter matches anything. `topic`
matches the resolved fully-qualified name: a relative name resolves against the
node first, so `endpoints(node; topic="chatter")` finds endpoints on `/chatter`.
`kind` is an `EndpointKind` (`Publisher`, `Subscription`, `Service`, or
`Client`). `node` and `namespace` match the owning node's name and namespace.

The query filters a snapshot of the index taken under the index lock, so it sees
one consistent instant. Local entities are authoritative the instant they are
declared (injected on construction); remote entities become visible as their
Zenoh liveliness tokens arrive, which is why [`wait_for_service`](@ref) exists
for code that must block until a peer appears.

See the ROS 2 discovery model:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Discovery.html

```julia
# every remote publisher this node can see on /chatter
for pub in endpoints(node; topic="chatter", kind=Publisher)
    pub.is_local || @info "remote pub" node=pub.node_name type=pub.type
end
```
"""
function endpoints(node_or_ctx; topic::Union{AbstractString, Nothing}=nothing,
                   kind::Union{EndpointKind, Nothing}=nothing,
                   node::Union{AbstractString, Nothing}=nothing,
                   namespace::Union{AbstractString, Nothing}=nothing)
    ctx = _ctx(node_or_ctx)
    # Resolve a relative topic filter against the node so it compares to the FQN
    # form stored in the index. A bare Context has no namespace root for `~`, but
    # absolute/relative names still resolve.
    want_topic = topic === nothing ? nothing : resolve_name(node_or_ctx, topic)
    out = EndpointInfo[]
    for e in endpoints_snapshot(ctx)
        want_topic  === nothing || e.topic == want_topic       || continue
        kind        === nothing || e.kind == kind              || continue
        node        === nothing || e.node_name == node         || continue
        namespace   === nothing || e.namespace == String(namespace) || continue
        push!(out, e)
    end
    out
end

"""
    publishers_info(node, topic) -> Vector{EndpointInfo}

Return the publishers currently advertised on `topic` (resolved to its
fully-qualified name). Each `EndpointInfo` carries the publisher's `TypeInfo`
(type name + RIHS01 hash), `QosProfile`, and gid, enough for a caller to run its
own type and QoS compatibility check (§12.2) against its local endpoint. The
ROS 2 `get_publishers_info_by_topic` equivalent; [`subscriptions_info`](@ref) is
the dual.
"""
publishers_info(node, topic::AbstractString) =
    endpoints(node; topic=topic, kind=Publisher)

"""
    subscriptions_info(node, topic) -> Vector{EndpointInfo}

Return the subscriptions currently present on `topic` (resolved to its
fully-qualified name), each carrying the subscriber's `TypeInfo`, `QosProfile`,
and gid. The ROS 2 `get_subscriptions_info_by_topic` equivalent and the dual of
[`publishers_info`](@ref).
"""
subscriptions_info(node, topic::AbstractString) =
    endpoints(node; topic=topic, kind=Subscription)

"""
    count_publishers(node, topic) -> Int

Return the number of publishers on `topic` (resolved to its fully-qualified
name), equal to `length(publishers_info(node, topic))`. The ROS 2
`count_publishers` equivalent.
"""
count_publishers(node, topic::AbstractString) = length(publishers_info(node, topic))

"""
    count_subscribers(node, topic) -> Int

Return the number of subscriptions on `topic` (resolved to its fully-qualified
name), equal to `length(subscriptions_info(node, topic))`. The ROS 2
`count_subscribers` equivalent.
"""
count_subscribers(node, topic::AbstractString) = length(subscriptions_info(node, topic))

"""
    topic_names_and_types(node_or_ctx; include_services=false) -> Dict{String, Vector{TypeInfo}}

Map each topic (resolved fully-qualified name) to the distinct `TypeInfo`s
advertised on it across the graph, the ROS 2 `get_topic_names_and_types`
equivalent (§12.1). By default the map covers pub/sub topics; pass
`include_services=true` to fold in service and client endpoints as well.

Two or more `TypeInfo` entries for one topic signal a type mismatch on the wire:
endpoints differ either in type name or in RIHS01 hash (a version skew).
Endpoints whose type is the `EMPTY_TOPIC_TYPE` token (`type === nothing`) are
omitted, since they carry no type to report.

```julia
for (topic, types) in topic_names_and_types(node)
    length(types) > 1 && @warn "mixed types on \$topic" types
end
```
"""
function topic_names_and_types(node_or_ctx; include_services::Bool=false)
    ctx = _ctx(node_or_ctx)
    out = Dict{String, Vector{TypeInfo}}()
    for e in endpoints_snapshot(ctx)
        include_services || (e.kind === Publisher || e.kind === Subscription) || continue
        e.type === nothing && continue                # EMPTY_TOPIC_TYPE token
        types = get!(() -> TypeInfo[], out, e.topic)
        any(==(e.type), types) || push!(types, e.type)
    end
    out
end

"""
    node_names(node_or_ctx) -> Vector{NodeInfo}

Return the distinct nodes seen in the graph, the ROS 2
`get_node_names_and_namespaces` equivalent (§12.1). Identity is recovered from
the owning-node fields carried on each endpoint (a node advertises its own
presence as a topic-less endpoint shell), deduplicated by `(namespace, name)`.

Endpoints with no node identity are skipped: the ros2dds bridge yields
liveliness tokens whose `parse_liveliness` leaves the node blank, and there is
no name to report for those. Each returned `NodeInfo` carries an empty enclave
string: rmw_zenoh always writes the `%` (empty) sentinel into the token's
enclave slot, so no non-default enclave survives the wire.
"""
function node_names(node_or_ctx)
    ctx = _ctx(node_or_ctx)
    seen = Set{Tuple{String, String}}()
    out = NodeInfo[]
    for e in endpoints_snapshot(ctx)
        isempty(e.node_name) && continue
        key = (e.namespace, e.node_name)
        key in seen && continue
        push!(seen, key)
        # rmw_zenoh always writes "%" in the enclave slot, so no enclave survives
        # the wire; report empty rather than invent one.
        push!(out, NodeInfo(e.node_name, e.namespace, ""))
    end
    out
end

# ── graph waits (§12.1) — park on the change stream, shutdown-interruptible ──
# Every wait re-checks a predicate over the snapshot, then blocks on the index's
# `changed` condition until the next discovery event re-checks. The Context's
# drain notifies the same condition (`all=true`, context.jl step 1), so a blocked
# wait wakes and raises `ShutdownException` rather than hanging the drain.

# Block until `pred()` holds or the deadline/shutdown fires. Returns true if the
# predicate became true; false on timeout. Raises ShutdownException if the Context
# drains while we wait (the cooperative unwind signal, §14). `timeout` is seconds;
# `nothing` waits forever.
function _wait_on_graph(ctx::Context, pred; timeout::Union{Real, Nothing})
    pred() && return true
    deadline = timeout === nothing ? nothing : time() + Float64(timeout)
    @lock ctx.graph.changed begin
        while true
            is_shutdown(ctx) && throw(ShutdownException())
            pred() && return true
            if deadline === nothing
                wait(ctx.graph.changed)
            else
                remaining = deadline - time()
                remaining <= 0 && return false
                # `Threads.Condition` has no timed wait; arm a one-shot Base.Timer
                # to notify us at the deadline so the loop re-checks and times out.
                # The timer fires on its own task and must take the lock to notify.
                t = Base.Timer(remaining) do _
                    @lock ctx.graph.changed notify(ctx.graph.changed)
                end
                try
                    wait(ctx.graph.changed)
                finally
                    close(t)
                end
            end
        end
    end
end

"""
    service_is_ready(client) -> Bool
    service_is_ready(node, service_name) -> Bool

Return `true` when a service server is present in the graph on the resolved
service name, the ROS 2 `Client.service_is_ready` equivalent (§12.1). The client
form reads the resolved name and owning node from the client handle's `Entity`;
the name form resolves `service_name` against the node. Readiness means a
`Service`-kind endpoint exists on that name; local entities count, so our own
service reads as ready the instant it is declared.

Reports graph liveliness: the same eventually-consistent view
[`wait_for_service`](@ref) blocks on. For the stronger guarantee that Zenoh
routing to the server has settled, await the client form of
[`wait_for_service`](@ref).
"""
function service_is_ready(node, service_name::AbstractString)
    ctx = _ctx(node)
    want = resolve_name(node, service_name; kind=:service)
    for e in endpoints_snapshot(ctx)
        e.kind === Service && e.topic == want && return true
    end
    false
end

# Client form, duck-typed against the handle's shape: any client holding an
# `Entity` in `.entity` (`ServiceClient` does) whose `endpoint.topic` is the
# resolved service name and whose `node` we query through.
function service_is_ready(client)
    e = _client_entity(client)
    service_is_ready(e.node, e.endpoint.topic)
end

"""
    wait_for_service(client::ServiceClient; timeout=nothing) -> Bool
    wait_for_service(node, service_name; timeout=nothing) -> Bool

Block the calling task until a matching service server is reachable, the ROS 2
`Client.wait_for_service` equivalent (§12.1). Return `true` once ready, or
`false` if `timeout` seconds elapse first (`nothing` waits forever). Raises
[`ShutdownException`](@ref) if the Context begins draining while the task is
parked, so a blocked wait cooperatively unwinds and lets the drain proceed.

The client form waits on a real routing match: the client's `Querier` has
actually matched a server's queryable ([`service_matched`](@ref)), the strong
guarantee that a subsequent [`call`](@ref) reaches a server. The name form has no
querier handle, so it waits on graph liveliness ([`service_is_ready`](@ref)) — a
`Service` endpoint on the resolved name; for a same-process server that becomes
true the instant the service is declared, before routing settles. Prefer the
client form whenever you hold the client.

Both forms park on the discovery change stream and re-check on each event, which
is why they exist: remotes and routing are eventually-consistent, so a client
created before its server must wait for it to appear.

```julia
wait_for_service(client; timeout=5.0) || error("no server within 5s")
resp = call(client, request)
```
"""
function wait_for_service(node, service_name::AbstractString;
                          timeout::Union{Real, Nothing}=nothing)
    ctx = _ctx(node)
    want = resolve_name(node, service_name; kind=:service)
    _wait_on_graph(ctx, () -> _service_present(ctx, want); timeout=timeout)
end

# Client form: wait on the routing-plane match (`service_matched`) — the guarantee
# that `call` will actually reach a server, not just liveliness. The (lazily
# declared) match listener notifies `ctx.graph.changed`, so we reuse
# `_wait_on_graph` for timeout + shutdown-interruptibility for free.
function wait_for_service(client::ServiceClient; timeout::Union{Real, Nothing}=nothing)
    _ensure_match_listener!(client)
    ctx = client.entity.node.context
    _wait_on_graph(ctx, () -> service_matched(client); timeout=timeout)
end

# Generic fallback for any other handle carrying an `.entity` (or an `Entity`):
# liveliness only — it has no querier to read routing match from.
function wait_for_service(client; timeout::Union{Real, Nothing}=nothing)
    e = _client_entity(client)
    wait_for_service(e.node, e.endpoint.topic; timeout=timeout)
end

"""
    wait_for_action_server(client::ActionClient; timeout=nothing) -> Bool

Block the calling task until the action client is routing-matched to a server,
the ROS 2 `ActionClient.wait_for_server` equivalent (§9). Matching requires both
the client's `send_goal` and `get_result` Queriers to have matched the server's
queryables. Return `true` once matched, or `false` if `timeout` seconds elapse
(`nothing` waits forever). Raises [`ShutdownException`](@ref) if the Context
begins draining while parked.

The action analogue of the client form of [`wait_for_service`](@ref), reusing the
same routing-match machinery (lazily-declared Queriers plus
[`action_server_matched`](@ref)). Both services must match because
[`send`](@ref) retries discovery on its own, while [`fetch`](@ref)'s
`get_result` is a one-shot get with a long timeout — awaiting the `get_result`
match keeps a `send` followed by `fetch` from blocking on an unmatched service.
The `cancel_goal` and feedback channels are not gated: cancel is opt-in and
feedback is an ordinary subscription.
"""
function wait_for_action_server(client::ActionClient; timeout::Union{Real, Nothing}=nothing)
    ws = _ensure_action_match!(client)
    _wait_on_graph(client.node.context, () -> all(_wire_matched, ws); timeout=timeout)
end

# Predicate over the snapshot: a Service endpoint on `want` exists.
function _service_present(ctx::Context, want::AbstractString)
    for e in endpoints_snapshot(ctx)
        e.kind === Service && e.topic == want && return true
    end
    false
end

# Recover the generic `Entity` from a client handle. Pattern handles (pubsub.jl's
# `PublisherHandle`/`SubscriptionHandle`, service.jl's `ServiceClient`) all *hold*
# an `Entity` in a `.entity` field — accept that shape directly, or an `Entity`.
_client_entity(e::Entity) = e
_client_entity(client) = hasproperty(client, :entity) ? client.entity::Entity :
    throw(ArgumentError("wait_for_service expects a service client (or node + name)"))

"""
    wait_for_graph_change(node_or_ctx; timeout=nothing) -> Bool

Block the calling task until the set of endpoints in the graph differs from the
set observed at entry — an endpoint appeared or disappeared — the low-level wait
behind the polling forms of the graph queries (§12.1). Return `true` when a
change is observed, or `false` if `timeout` seconds elapse (`nothing` waits
forever). Raises [`ShutdownException`](@ref) if the Context begins draining
while parked.

The predicate compares the index keyset before and after each wakeup, so a
spurious notify (an in-place endpoint update with no add or remove) keeps
waiting, and add/remove churn that restores the original keyset between wakeups
coalesces away. For a push-based callback on every change, register an
`on_graph_change` listener (context.jl).
"""
function wait_for_graph_change(node_or_ctx; timeout::Union{Real, Nothing}=nothing)
    ctx = _ctx(node_or_ctx)
    before = _graph_keyset(ctx)
    _wait_on_graph(ctx, () -> _graph_keyset(ctx) != before; timeout=timeout)
end

_graph_keyset(ctx::Context) = @lock ctx.graph.lock Set(keys(ctx.graph.endpoints))

# ── mismatch detection (§12.2) — views on the change stream ─────────────────
# Both detectors are `on_graph_change` listeners: when an endpoint appears, they
# compare it to our *local* endpoints on the same topic and, on a violation, fire
# a programmatic event and emit a throttled+deduped warning. Detection is per
# matched (ours, theirs) pair, keyed so we report each distinct mismatch once.
#
# The graph is the primary signal; for type-mismatch a per-sample backstop in the
# subscription path (`check_sample_type`) catches wildcard subs whose concrete
# remote type only shows up on the wire.

"""
    TypeMismatch(local_endpoint, remote_endpoint, reason, suggestion)

A detected type incompatibility between one of our endpoints and a remote
endpoint on the same topic, delivered to [`on_type_mismatch`](@ref) listeners
(§12.2). `local_endpoint` and `remote_endpoint` are the two `EndpointInfo`s
involved. `reason` is `:name` (the two carry entirely different types —
typically a wrong topic wiring) or `:hash` (same type name, different RIHS01
hash — a version skew that makes the remote's bytes decode-unsafe against our
struct). `suggestion` is a human-facing fix hint.
"""
struct TypeMismatch
    local_endpoint::EndpointInfo
    remote_endpoint::EndpointInfo
    reason::Symbol                  # :name | :hash
    suggestion::String
end

"""
    QosIncompatible(local_endpoint, remote_endpoint, issues, suggestion)

A detected QoS incompatibility between one of our endpoints and a remote
endpoint on the same topic, delivered to [`on_qos_incompatible`](@ref) listeners
(§12.2). The two `EndpointInfo`s are `local_endpoint` and `remote_endpoint`.
`issues::Vector{QosIncompatibility}` lists the offending policies under the DDS
Request-vs-Offered rule (computed by `qos_compatible`), and `suggestion`
is a human-facing fix hint naming each offered-versus-requested gap.
"""
struct QosIncompatible
    local_endpoint::EndpointInfo
    remote_endpoint::EndpointInfo
    issues::Vector{QosIncompatibility}
    suggestion::String
end

# The detector state hangs off the Context's graph, lazily attached on first
# `on_type_mismatch`/`on_qos_incompatible` registration. Holds the user event
# listeners + the dedupe/throttle bookkeeping. Stored in the GraphIndex's
# `listeners` indirectly (we register one umbrella listener that fans out), but
# kept here as a Context-keyed singleton so repeated registration is cheap.

mutable struct _Detectors
    lock::ReentrantLock
    type_listeners::Vector{Any}
    qos_listeners::Vector{Any}
    # signature → last-warned monotonic seconds, for log throttling + dedupe.
    seen::Dict{Any, Float64}
    installed::Bool                 # the umbrella on_graph_change is registered
    throttle_s::Float64             # min seconds between repeat warnings per signature
end
_Detectors() = _Detectors(ReentrantLock(), Any[], Any[], Dict{Any, Float64}(),
                          false, 5.0)

# One detector-state per Context, keyed by `objectid(ctx.graph)` so we don't extend
# the Context struct. The GraphIndex object's identity is stable for its lifetime.
const _DETECTORS = Dict{UInt, _Detectors}()
const _DETECTORS_LOCK = ReentrantLock()

function _detectors(ctx::Context)
    k = objectid(ctx.graph)
    @lock _DETECTORS_LOCK get!(() -> _Detectors(), _DETECTORS, k)
end

# Register the umbrella `on_graph_change` listener once: it scans `change.added`
# for endpoints that match (by topic) one of our locals and dispatches the two
# detectors. Idempotent — guarded by `installed`.
function _ensure_detectors_installed!(ctx::Context, d::_Detectors)
    @lock d.lock begin
        d.installed && return nothing
        d.installed = true
    end
    on_graph_change(ctx) do change
        isempty(change.added) && return nothing
        _run_detectors(ctx, d, change.added)
    end
    nothing
end

# For each freshly-added *remote* endpoint, find our local endpoints on the same
# topic with the complementary kind (their pub ↔ our sub, their sub ↔ our pub) and
# check type + QoS. Local-vs-local and remote-vs-remote are not our concern.
function _run_detectors(ctx::Context, d::_Detectors, added::Vector{EndpointInfo})
    locals = filter(e -> e.is_local, endpoints_snapshot(ctx))
    isempty(locals) && return nothing
    for remote in added
        remote.is_local && continue
        for local_e in locals
            local_e.topic == remote.topic || continue
            _complementary(local_e.kind, remote.kind) || continue
            _check_type_mismatch(d, local_e, remote)
            _check_qos_incompat(d, local_e, remote)
        end
    end
    nothing
end

# Endpoints "match" for compatibility when one publishes what the other consumes.
_complementary(a::EndpointKind, b::EndpointKind) =
    (a === Publisher && b === Subscription) ||
    (a === Subscription && b === Publisher) ||
    (a === Service && b === Client) ||
    (a === Client && b === Service)

# Compare a local endpoint's TypeInfo to a remote's. Skip if either side carries
# no type (an EMPTY_TOPIC_TYPE token — nothing to compare). Name differs ⇒ wrong
# type; same name, hash differs ⇒ wrong version (decode-unsafe).
function _check_type_mismatch(d::_Detectors, local_e::EndpointInfo, remote::EndpointInfo)
    lt = local_e.type
    rt = remote.type
    (lt === nothing || rt === nothing) && return nothing
    if lt.name != rt.name
        _emit_type_mismatch(d, local_e, remote, :name,
            "endpoints on $(local_e.topic) carry different types ($(lt.name) vs \
             $(rt.name)); check the topic name and message type")
    elseif lt.hash != rt.hash
        _emit_type_mismatch(d, local_e, remote, :hash,
            "type $(lt.name) on $(local_e.topic) has mismatched RIHS01 versions; \
             regenerate against the same message definition")
    end
    nothing
end

# Check RxO: offered must be ≥ requested. The subscription side is the requester.
# Run the check in the right direction for the local/remote roles, then merge —
# `qos_compatible(requested, offered)` (ROSZenoh) returns the violations.
function _check_qos_incompat(d::_Detectors, local_e::EndpointInfo, remote::EndpointInfo)
    requested, offered = if local_e.kind === Subscription || local_e.kind === Client
        (local_e.qos, remote.qos)
    else
        (remote.qos, local_e.qos)
    end
    issues = qos_compatible(requested, offered)
    isempty(issues) && return nothing
    _emit_qos_incompat(d, local_e, remote, issues,
        "QoS incompatible on $(local_e.topic): " *
        join(("offered $(i.policy)=$(i.offered) < requested $(i.requested)" for i in issues), ", "))
end

# Dedupe + throttle, then fan to user listeners. The signature collapses repeats
# of the same (topic, kinds, reason) so a flapping endpoint doesn't spam; the
# throttle bounds even distinct re-appearances. One `_mark_seen!` gate covers
# both the warning and the listener fan-out: a signature inside the throttle
# window fires neither.
function _emit_type_mismatch(d::_Detectors, local_e, remote, reason, suggestion)
    sig = (:type, local_e.topic, local_e.kind, remote.kind, reason)
    fresh = _mark_seen!(d, sig)
    fresh || return nothing
    @warn "type mismatch detected" topic=local_e.topic reason=reason suggestion=suggestion
    ev = TypeMismatch(local_e, remote, reason, suggestion)
    _fire(d.lock, d.type_listeners, ev, "on_type_mismatch")
end

# Route a pre-built `TypeMismatch` — from the per-sample weak-static backstop
# (`check_sample_type`, §12.2/A2) — through the SAME dedupe/throttle + listener
# fan-out as the graph-match detector, so one logical mismatch reports once across
# both the on-the-wire and graph paths (signature mirrors `_emit_type_mismatch`).
function report_type_mismatch!(ctx::Context, tm::TypeMismatch)
    d = _detectors(ctx)
    sig = (:type, tm.local_endpoint.topic, tm.local_endpoint.kind,
           tm.remote_endpoint.kind, tm.reason)
    _mark_seen!(d, sig) || return nothing
    @warn "type mismatch detected (on the wire)" topic=tm.local_endpoint.topic reason=tm.reason suggestion=tm.suggestion
    _fire(d.lock, d.type_listeners, tm, "on_type_mismatch")
    nothing
end

function _emit_qos_incompat(d::_Detectors, local_e, remote, issues, suggestion)
    sig = (:qos, local_e.topic, local_e.kind, remote.kind,
           Tuple(i.policy for i in issues))
    fresh = _mark_seen!(d, sig)
    fresh || return nothing
    @warn "QoS incompatibility detected" topic=local_e.topic suggestion=suggestion
    ev = QosIncompatible(local_e, remote, issues, suggestion)
    _fire(d.lock, d.qos_listeners, ev, "on_qos_incompatible")
end

# Record a signature; return true if it's the first time (or the throttle window
# has elapsed since we last reported it). Bounds repeat reports per signature.
function _mark_seen!(d::_Detectors, sig)
    now = time()
    @lock d.lock begin
        last = get(d.seen, sig, nothing)
        if last === nothing || (now - last) >= d.throttle_s
            d.seen[sig] = now
            return true
        end
        return false
    end
end

# Snapshot listeners under the lock, then fire outside it (their handlers may
# query the graph or register more listeners). A throwing listener is logged.
function _fire(lock::ReentrantLock, listeners::Vector{Any}, ev, what::AbstractString)
    fns = @lock lock copy(listeners)
    for f in fns
        try
            f(ev)
        catch err
            @error "$what listener threw" exception=(err, catch_backtrace())
        end
    end
    nothing
end

"""
    on_type_mismatch(f, node_or_ctx) -> f

Register `f(::TypeMismatch)` to fire when a remote endpoint with an incompatible
type appears on a topic this node also uses — a different type name, or the same
name with a different RIHS01 version (§12.2). Return `f`. The ROS 2
incompatible-type event surface.

Detection runs as a listener on the discovery change stream: when a remote
endpoint is added, it is compared against local endpoints of the complementary
kind (our subscription versus their publisher, and vice versa) on the same
topic. Each distinct mismatch reports once per throttle window, keyed by a
(topic, kinds, reason) signature, so a flapping endpoint does not spam
listeners. The per-sample backstop for wildcard subscriptions
(`check_sample_type`) feeds the same dedupe, so a mismatch seen both on
the wire and in the graph reports once. Listeners run outside the detector
lock; a throwing listener is logged and the others still run.

Detection covers endpoints that appear after registration. A mismatch already
standing in the graph when `f` is registered does not fire retroactively.

```julia
on_type_mismatch(node) do tm
    @error "type mismatch" topic=tm.local_endpoint.topic reason=tm.reason
end
```
"""
function on_type_mismatch(f::Function, node_or_ctx)
    ctx = _ctx(node_or_ctx)
    d = _detectors(ctx)
    @lock d.lock push!(d.type_listeners, f)
    _ensure_detectors_installed!(ctx, d)
    f
end

"""
    on_qos_incompatible(f, node_or_ctx) -> f

Register `f(::QosIncompatible)` to fire when a remote endpoint whose QoS is
incompatible with ours appears on a shared topic — under the DDS
Request-vs-Offered rule, the offered profile ranks below the requested one on
some policy (§12.2). Return `f`. The ROS 2 incompatible-QoS event surface,
covering reliability, durability, deadline, and liveliness (kind and lease).

Detection runs on the discovery change stream and is deduplicated by signature
and throttled, the same way [`on_type_mismatch`](@ref) is. The requester side is
the subscription (or client) and the offered side is the publisher (or service);
the check runs in the correct direction for whichever role is local. Listeners
run outside the detector lock, and a mismatch already standing in the graph at
registration does not fire retroactively.
"""
function on_qos_incompatible(f::Function, node_or_ctx)
    ctx = _ctx(node_or_ctx)
    d = _detectors(ctx)
    @lock d.lock push!(d.qos_listeners, f)
    _ensure_detectors_installed!(ctx, d)
    f
end

"""
    check_sample_type(sub, sample) -> Union{TypeMismatch, Nothing}

Per-sample type backstop (§12.2): for a wildcard subscription, the concrete remote
type only appears on the wire, not in a graph match — so on receipt, parse the
sample's topic keyexpr (`parse_topic_keyexpr`) and compare its `TypeInfo` to the
subscription's. Returns a `TypeMismatch` (and lets the caller decide to drop/warn)
or `nothing` when types agree or the key carries no type (ros2dds, EMPTY).

The subscription dispatch path (`_predispatch`, dispatch.jl) calls this before
decoding for weak-static subscriptions and routes a mismatch through
`report_type_mismatch!`; a hash mismatch means the bytes are
decode-unsafe against the local struct.
"""
function check_sample_type(sub, sample)
    e = _client_entity(sub)
    want = e.endpoint.type_info
    want === nothing && return nothing
    ctx = e.node.context
    parsed = try
        parse_topic_keyexpr(ctx.format, Zenoh.keyexpr(sample))
    catch
        return nothing                 # foreign/unparseable key — not our mismatch to flag
    end
    got = parsed.type_info
    got === nothing && return nothing  # ros2dds keys carry no type; graph is the only signal
    if got.name != want.name
        return TypeMismatch(_endpoint_info(e.endpoint; is_local=true),
            _remote_shell(e.endpoint, got), :name,
            "received $(got.name) on $(e.endpoint.topic), expected $(want.name)")
    elseif got.hash != want.hash
        return TypeMismatch(_endpoint_info(e.endpoint; is_local=true),
            _remote_shell(e.endpoint, got), :hash,
            "received $(got.name) with mismatched RIHS01 version on $(e.endpoint.topic)")
    end
    nothing
end

# A minimal remote `EndpointInfo` for a per-sample mismatch: we only learn the
# type from the key, not the remote's node/QoS, so fill what we know.
_remote_shell(local_endpoint, got::TypeInfo) =
    EndpointInfo("", "", Publisher, local_endpoint.topic, got,
                 local_endpoint.qos, ntuple(_ -> 0x00, 16), false)

# ── QoS event surface: liveliness-changed & message-lost (§12.3) ────────────

"""
    liveliness_changed(node_or_ctx, topic) -> (; alive, not_alive)

A snapshot of the liveliness of *remote* publishers on `topic` (resolved FQN),
sourced from the discovery stream (§12.3): `alive` is the count currently present
in the graph. There is no separate liveliness protocol — a Zenoh liveliness token
present ⇒ alive, withdrawn (DELETE) ⇒ not alive, which the index already tracks.
For change *events*, register an [`on_graph_change`](@ref) listener filtered to
the topic; this is the point-in-time count.

`not_alive` is `0` here: the graph holds only live tokens (a withdrawn token is
removed), so we report transitions via the change stream rather than a standing
count. Kept in the return shape to match the ROS2 `LivelinessChanged` fields.
"""
function liveliness_changed(node_or_ctx, topic::AbstractString)
    pubs = publishers_info(node_or_ctx, topic)
    alive = count(e -> !e.is_local, pubs)
    (; alive = alive, not_alive = 0)
end

"""
    MessageLost(topic, source_gid, expected_seq, got_seq, count)

A detected gap in a publisher's per-message sequence numbers, delivered to
[`on_message_lost`](@ref) listeners — the ROS 2 `MessageLost` event reconstructed
from the rmw_zenoh per-message sequence number (§12.3). `topic` is the resolved
topic; `source_gid` is the publisher whose stream gapped; `expected_seq` is the
next sequence number we expected from it; `got_seq` is the one that arrived; and
`count` is the number of messages lost in between (`got_seq - expected_seq`).

ROS 2 inherits this event from DDS; here it is reconstructed from the
`sequence_number` the wire protocol carries per message (§3.4) rather than from a
DDS-native lost count.

The sequence number rides in the rmw_zenoh put attachment. The detector feeding
this event currently reads the message payload instead of the attachment
(`note_sequence!` audit bug B3), so against a real peer the gap arithmetic runs
on body bytes, not the sequence number, and the event does not fire correctly.
"""
struct MessageLost
    topic::String
    source_gid::NTuple{16, UInt8}
    expected_seq::Int64
    got_seq::Int64
    count::Int64
end

# Per-subscription message-lost tracking: the last seen sequence number per source
# gid (a topic may have several publishers). A gap (`got > expected`) is a loss; a
# repeat/reorder (`got <= last`) is ignored (best-effort delivery can reorder).
mutable struct _LostTracker
    lock::ReentrantLock
    last_seq::Dict{NTuple{16, UInt8}, Int64}
    listeners::Vector{Any}
end
_LostTracker() = _LostTracker(ReentrantLock(), Dict{NTuple{16, UInt8}, Int64}(), Any[])

# One tracker per subscription Entity, attached lazily on first `on_message_lost`.
const _LOST = Dict{UInt, _LostTracker}()
const _LOST_LOCK = ReentrantLock()
_lost_tracker(e::Entity) = @lock _LOST_LOCK get!(() -> _LostTracker(), _LOST, objectid(e))

"""
    on_message_lost(f, sub) -> f

Register `f(::MessageLost)` to fire when a gap opens in a publisher's
per-message sequence numbers on subscription `sub` — one publisher's sequence
number jumping by more than one between consecutive received messages (§12.3).
Return `f`. The ROS 2 message-lost event surface.

Registering the first listener flips a per-subscription gate so the dispatch
path begins decoding each sample's metadata and feeding it to the detector
(`note_sequence!`); with no listener the common path skips that decode
entirely. Sequence tracking is per source gid, so a topic with several
publishers is tracked independently per publisher. Reordered or duplicate
messages (a sequence number at or below the high-water mark) are ignored,
matching best-effort delivery.

The decode currently reads the message payload rather than the put attachment
that carries the sequence number (audit bug B3, `note_sequence!`), so a
registered listener does not fire correctly against a real peer until that read
path is fixed.

```julia
on_message_lost(sub) do ml
    @warn "dropped messages" topic=ml.topic count=ml.count
end
```
"""
function on_message_lost(f::Function, sub)
    e = _client_entity(sub)
    t = _lost_tracker(e)
    @lock t.lock push!(t.listeners, f)
    # Flip the consumer hot-path gate so per-sample `note_sequence!` starts running
    # for this endpoint (the common no-listener path skips the attachment decode).
    @atomic e._track_lost = true
    f
end

"""
    note_sequence!(sub, sample) -> Union{MessageLost, Nothing}

Feed one received sample to the message-lost detector for `sub` (§12.3): decode
`(seq, _, gid)` and, if `seq` skipped ahead of the last value seen from that
`gid`, fire the `on_message_lost` listeners and return the `MessageLost`. Returns
`nothing` when in-order (or the very first message from a gid, or the decode
yields no value). The subscription dispatch path calls this per sample, gated on
`Entity._track_lost`.

The decode goes through `decode_attachment(sample)`, which currently
deserializes the message payload rather than the put attachment that carries the
sequence number (audit bug B3): against a real peer the `(seq, _, gid)` triple is
read from body bytes, so the gap test runs on the wrong source and the event does
not fire correctly until that read path is fixed.
"""
# dispatch.jl gates this call on `Entity._track_lost`, so the common no-listener
# path never decodes the attachment.
function note_sequence!(sub, sample)
    e = _client_entity(sub)
    seq, _, srcgid = try
        decode_attachment(sample)
    catch
        return nothing                 # no/garbled attachment — nothing to track
    end
    # No tracker ⇒ no listeners registered, skip the work. Guard the lookup: the
    # registry Dict isn't safe to read concurrently with a registration insert.
    t = @lock _LOST_LOCK get(_LOST, objectid(e), nothing)
    t === nothing && return nothing
    lost = nothing
    @lock t.lock begin
        last = get(t.last_seq, srcgid, nothing)
        if last !== nothing && seq > last + 1
            lost = MessageLost(e.endpoint.topic, srcgid, last + 1, seq, seq - last - 1)
        end
        # Track the high-water mark; reorders/dupes (seq <= last) don't lower it.
        if last === nothing || seq > last
            t.last_seq[srcgid] = seq
        end
    end
    lost === nothing && return nothing
    @warn "message lost" topic=e.endpoint.topic lost=lost.count
    _fire(t.lock, t.listeners, lost, "on_message_lost")
    lost
end

# Drop a subscription's detector state when it closes (called from the entity
# teardown path, teardown.jl); safe to call for entities that never registered one.
function _forget_lost_tracker!(e::Entity)
    @lock _LOST_LOCK delete!(_LOST, objectid(e))
    nothing
end

# deadline-missed (per-endpoint Timer) and lifespan (drop on attachment-ts age)
# enforcement are data-plane concerns, not graph views; they hang off the
# subscription's dispatch + a per-endpoint clock.
# TODO(§12.3): deadline-missed Timer (notify-only, matches ROS2) and lifespan
# drop (compare `source_timestamp` to `now` against `qos.lifespan`). Both want
# the §7 clock + the subscription dispatch hook; left as a stub here so this file
# stays a pure view over the graph + attachment stream and precompiles standalone.
