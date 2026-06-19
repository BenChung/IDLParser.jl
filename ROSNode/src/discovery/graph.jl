# Views over the Context's discovery index (`GraphIndex`), keyed off the
# `EndpointInfo` records the index holds — one per discovered endpoint, the unit
# every query and detector below filters and pairs:
#
#   - graph queries over the index snapshot (endpoints / publishers_info /
#     count_* / topic_names_and_types / node_names);
#   - graph waits that park on the index's `changed` condition, shutdown-
#     interruptible (service_is_ready / wait_for_service / wait_for_graph_change);
#   - the type-mismatch and QoS-incompat detectors as `on_graph_change`
#     listeners, deduped + throttled, firing `on_type_mismatch` /
#     `on_qos_incompatible`;
#   - liveliness-changed (from the stream) and message-lost (attachment `seq`
#     gaps).

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

# ── graph queries ───────────────────────────────────────────────────────────
# Each filters `endpoints_snapshot(ctx)` — a consistent instant copied under the
# index lock, never the live dict — so a query can't race a discovery update.

"""
    endpoints(node_or_ctx; topic=nothing, kind=nothing, node=nothing,
              namespace=nothing) -> Vector{EndpointInfo}

Return every endpoint in the discovery graph matching the given filters, the
primary entry into the ROS 2 graph as this stack sees it. Each result is
an [`EndpointInfo`](@ref) — the owning node, `kind`, topic, type, QoS, gid, and
locality of one discovered endpoint.

Filters combine conjunctively, and a `nothing` filter matches anything:

| Filter | Matches |
|--------|---------|
| `topic` | the resolved fully-qualified name; a relative name resolves against the node first, so `endpoints(node; topic="chatter")` finds endpoints on `/chatter` |
| `kind` | an `EndpointKind`: `Publisher`, `Subscription`, `Service`, or `Client` |
| `node` | the owning node's name |
| `namespace` | the owning node's namespace |

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
    # Resolve the topic filter to the FQN form the index stores.
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
fully-qualified name), each an [`EndpointInfo`](@ref) — enough for a caller to
run its own type and QoS compatibility check against its local endpoint.
The ROS 2 `get_publishers_info_by_topic` equivalent; [`subscriptions_info`](@ref)
is the dual.
"""
publishers_info(node, topic::AbstractString) =
    endpoints(node; topic=topic, kind=Publisher)

"""
    subscriptions_info(node, topic) -> Vector{EndpointInfo}

Return the subscriptions currently present on `topic` (resolved to its
fully-qualified name), each an [`EndpointInfo`](@ref). The ROS 2
`get_subscriptions_info_by_topic` equivalent and the dual of
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
equivalent. By default the map covers pub/sub topics; pass
`include_services=true` to fold in service and client endpoints as well.

Two or more `TypeInfo` entries for one topic signal a type mismatch on the wire:
endpoints differ either in type name or in RIHS01 hash (a version skew).
Endpoints with no declared type (`type === nothing`) are
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
`get_node_names_and_namespaces` equivalent. Identity is recovered from
the owning-node fields carried on each endpoint (a node advertises its own
presence as a topic-less endpoint shell), deduplicated by `(namespace, name)`.

- Endpoints with no node identity are skipped: the ros2dds bridge yields
  liveliness tokens whose `parse_liveliness` leaves the node blank, and there is
  no name to report for those.
- Each returned `NodeInfo` carries an empty enclave string: rmw_zenoh always
  writes the `%` (empty) sentinel into the token's enclave slot, so no
  non-default enclave survives the wire.
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
        # rmw_zenoh writes "%" in the enclave slot, so no enclave survives the wire.
        push!(out, NodeInfo(e.node_name, e.namespace, ""))
    end
    out
end

# ── graph waits — park on the change stream, shutdown-interruptible ──────────
# Every wait re-checks a predicate over the snapshot, then blocks on the index's
# `changed` condition until the next discovery event. The Context drain notifies
# the same condition, so a blocked wait wakes and raises `ShutdownException`
# rather than hanging the drain.

# Block until `pred()` holds (true), the deadline elapses (false), or the Context
# drains (raises ShutdownException). `timeout` is seconds; `nothing` waits forever.
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
                # `Threads.Condition` has no timed wait; a one-shot Timer notifies
                # at the deadline so the loop re-checks. It fires on its own task,
                # so it must take the lock to notify.
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
service name, the ROS 2 `Client.service_is_ready` equivalent.

- Client form: reads the resolved name and owning node off the handle's
  [`Entity`](@ref).
- Name form: resolves `service_name` against the node.
- Readiness means a `Service`-kind endpoint exists on that name; local entities
  count, so our own service reads as ready the instant it is declared.

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

# Client form: read the resolved name and node off the handle's `.entity`.
function service_is_ready(client)
    e = _client_entity(client)
    service_is_ready(e.node, e.endpoint.topic)
end

"""
    wait_for_service(client::ServiceClient; timeout=nothing) -> Bool
    wait_for_service(node, service_name; timeout=nothing) -> Bool

Block the calling task until a matching service server is reachable, the ROS 2
`Client.wait_for_service` equivalent. Return `true` once ready, or
`false` if `timeout` seconds elapse first (`nothing` waits forever). Raises
[`ShutdownException`](@ref) if the Context begins draining while the task is
parked, so a blocked wait cooperatively unwinds and lets the drain proceed.

- Client form: waits on a real routing match — the client's `Querier` has
  actually matched a server's queryable ([`service_matched`](@ref)), the strong
  guarantee that a subsequent [`call`](@ref) reaches a server.
- Name form: has no querier handle, so it waits on graph liveliness
  ([`service_is_ready`](@ref)) — a `Service` endpoint on the resolved name; for a
  same-process server that becomes true the instant the service is declared,
  before routing settles.

Prefer the client form whenever you hold the client.

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

# Client form: wait on the routing-plane match (`service_matched`), the guarantee
# that `call` reaches a server rather than mere graph liveliness. The lazily-
# declared match listener notifies `ctx.graph.changed`, so this reuses
# `_wait_on_graph` for the timeout and shutdown-interruptibility.
function wait_for_service(client::ServiceClient; timeout::Union{Real, Nothing}=nothing)
    _ensure_match_listener!(client)
    ctx = client.entity.node.context
    _wait_on_graph(ctx, () -> service_matched(client); timeout=timeout)
end

# Generic fallback for any other handle carrying an `.entity` (or an `Entity`):
# liveliness only, since it has no querier to read a routing match from.
function wait_for_service(client; timeout::Union{Real, Nothing}=nothing)
    e = _client_entity(client)
    wait_for_service(e.node, e.endpoint.topic; timeout=timeout)
end

"""
    wait_for_action_server(client::ActionClient; timeout=nothing) -> Bool

Block the calling task until the action client is routing-matched to a server,
the ROS 2 `ActionClient.wait_for_server` equivalent. Matching requires both
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

function _service_present(ctx::Context, want::AbstractString)
    for e in endpoints_snapshot(ctx)
        e.kind === Service && e.topic == want && return true
    end
    false
end

# Recover the [`Entity`](@ref) a client handle wraps: the pattern handles all hold
# one in a `.entity` field. Accept that shape, or a bare Entity.
_client_entity(e::Entity) = e
_client_entity(client) = hasproperty(client, :entity) ? client.entity::Entity :
    throw(ArgumentError("wait_for_service expects a service client (or node + name)"))

"""
    wait_for_graph_change(node_or_ctx; timeout=nothing) -> Bool

Block the calling task until the set of endpoints in the graph differs from the
set observed at entry — an endpoint appeared or disappeared — the low-level wait
behind the polling forms of the graph queries. Return `true` when a
change is observed, or `false` if `timeout` seconds elapse (`nothing` waits
forever). Raises [`ShutdownException`](@ref) if the Context begins draining
while parked.

The predicate compares the index keyset before and after each wakeup, so a
spurious notify (an in-place endpoint update with no add or remove) keeps
waiting, and add/remove churn that restores the original keyset between wakeups
coalesces away. For a push-based callback on every change, register an
[`on_graph_change`](@ref) listener.
"""
function wait_for_graph_change(node_or_ctx; timeout::Union{Real, Nothing}=nothing)
    ctx = _ctx(node_or_ctx)
    before = _graph_keyset(ctx)
    _wait_on_graph(ctx, () -> _graph_keyset(ctx) != before; timeout=timeout)
end

# Both halves of the seam — local + remote keys — so a wait sees our own endpoints
# appear/disappear, not just discovered ones.
_graph_keyset(ctx::Context) =
    @lock ctx.graph.lock union(Set(keys(ctx.graph.endpoints)), keys(ctx.graph.local_endpoints))

# ── mismatch detection — views on the change stream ─────────────────────────
# Both detectors are `on_graph_change` listeners: when an endpoint appears, they
# compare it to the standing endpoints across the local/remote divide on the same
# topic and, on a violation, fire an event and a throttled `@warn`. Detection is
# per matched (local, remote) pair, keyed so each distinct mismatch reports once.
#
# The graph is the primary signal. For type-mismatch a per-sample backstop in the
# subscription path (`check_sample_type`) catches wildcard subs whose concrete
# remote type shows up only on the wire; it shares the signature so a mismatch
# seen both ways still fires listeners once.

"""
    TypeMismatch(local_endpoint, remote_endpoint, reason, suggestion)

A detected type incompatibility between one of our endpoints and a remote
endpoint on the same topic, delivered to [`on_type_mismatch`](@ref) listeners.
`local_endpoint` and `remote_endpoint` are the two `EndpointInfo`s
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
endpoint on the same topic, delivered to [`on_qos_incompatible`](@ref) listeners.
The two `EndpointInfo`s are `local_endpoint` and `remote_endpoint`.
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

# Per-Context detector state: the user event listeners plus the two independent
# dedup gates. One umbrella `on_graph_change` listener fans out to both detectors.
mutable struct _Detectors
    lock::ReentrantLock
    type_listeners::Vector{Any}
    qos_listeners::Vector{Any}
    # signature → last-warned monotonic seconds, throttling repeat `@warn`s.
    seen::Dict{Any, Float64}
    # signatures whose listeners have already fired: edge-trigger with no time
    # component, so a hot per-sample mismatch fans out once.
    fired::Set{Any}
    installed::Bool                 # the umbrella on_graph_change is registered
    throttle_s::Float64             # min seconds between repeat warnings per signature
end
_Detectors() = _Detectors(ReentrantLock(), Any[], Any[], Dict{Any, Float64}(),
                          Set{Any}(), false, 5.0)

# Keyed by `objectid(ctx.graph)` (stable for the GraphIndex's lifetime) so the
# detector state need not be a field on the Context struct.
const _DETECTORS = Dict{UInt, _Detectors}()
const _DETECTORS_LOCK = ReentrantLock()

function _detectors(ctx::Context)
    k = objectid(ctx.graph)
    @lock _DETECTORS_LOCK get!(() -> _Detectors(), _DETECTORS, k)
end

# Register the umbrella `on_graph_change` listener once (guarded by `installed`):
# it dispatches both detectors over each change's added endpoints.
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

# For each added endpoint, pair it across the local/remote divide on the same
# topic with the complementary kind (pub ↔ sub, service ↔ client) and check type
# + QoS. Checks always run as (local, remote) pairs so a pair added in one change
# dedupes to one signature; local-vs-local and remote-vs-remote are skipped.
function _run_detectors(ctx::Context, d::_Detectors, added::Vector{EndpointInfo})
    # The local half is the static local graph; the remote half is the discovered stream.
    # Take them straight off the seam rather than re-filtering a unioned snapshot per change.
    locals, remotes = _local_remote_split(ctx)
    for e in added
        standing = e.is_local ? remotes : locals
        for other in standing
            other.topic == e.topic || continue
            _complementary(other.kind, e.kind) || continue
            local_e, remote = e.is_local ? (e, other) : (other, e)
            _check_type_mismatch(d, local_e, remote)
            _check_qos_incompat(d, local_e, remote)
        end
    end
    nothing
end

# Retroactive scan for one just-registered listener (`scan=true`): walk the
# standing snapshot once, pairing local↔remote complementary endpoints as
# `_run_detectors` does, and deliver each pre-existing mismatch to `f` alone. The
# shared `fired` edge-trigger set keeps a later change from double-firing.
function _scan_for_listener!(ctx::Context, d::_Detectors, f::Function, kind::Symbol)
    locals, remotes = _local_remote_split(ctx)
    for local_e in locals, remote in remotes
        local_e.topic == remote.topic || continue
        _complementary(local_e.kind, remote.kind) || continue
        kind === :type ? _scan_type_mismatch(d, f, local_e, remote) :
                         _scan_qos_incompat(d, f, local_e, remote)
    end
    nothing
end

# Scan variant of `_check_type_mismatch`: same comparison and signature, but
# delivers only to `f` (no full fan-out, no `@warn`).
function _scan_type_mismatch(d::_Detectors, f::Function, local_e::EndpointInfo, remote::EndpointInfo)
    lt = local_e.type
    rt = remote.type
    (lt === nothing || rt === nothing) && return nothing
    reason, suggestion = if lt.name != rt.name
        (:name, "endpoints on $(local_e.topic) carry different types ($(lt.name) vs \
                 $(rt.name)); check the topic name and message type")
    elseif lt.hash != rt.hash
        (:hash, "type $(lt.name) on $(local_e.topic) has mismatched RIHS01 versions; \
                 regenerate against the same message definition")
    else
        return nothing
    end
    sig = (:type, local_e.topic, local_e.kind, remote.kind, reason)
    _mark_fired!(d, sig) || return nothing
    _fire(d.lock, Any[f], TypeMismatch(local_e, remote, reason, suggestion), "on_type_mismatch")
    nothing
end

# Scan variant of `_check_qos_incompat`: same RxO check and signature, delivered
# only to `f`.
function _scan_qos_incompat(d::_Detectors, f::Function, local_e::EndpointInfo, remote::EndpointInfo)
    requested, offered = if local_e.kind === Subscription || local_e.kind === Client
        (local_e.qos, remote.qos)
    else
        (remote.qos, local_e.qos)
    end
    issues = qos_compatible(requested, offered)
    isempty(issues) && return nothing
    sig = (:qos, local_e.topic, local_e.kind, remote.kind, Tuple(i.policy for i in issues))
    _mark_fired!(d, sig) || return nothing
    suggestion = "QoS incompatible on $(local_e.topic): " *
        join(("offered $(i.policy)=$(i.offered) < requested $(i.requested)" for i in issues), ", ")
    _fire(d.lock, Any[f], QosIncompatible(local_e, remote, issues, suggestion), "on_qos_incompatible")
    nothing
end

# Endpoints "match" for compatibility when one publishes what the other consumes.
_complementary(a::EndpointKind, b::EndpointKind) =
    (a === Publisher && b === Subscription) ||
    (a === Subscription && b === Publisher) ||
    (a === Service && b === Client) ||
    (a === Client && b === Service)

# Compare TypeInfos (skipping an EMPTY_TOPIC_TYPE token on either side): a
# different name is the wrong type; the same name with a different RIHS01 is a
# version skew, and the remote's bytes are decode-unsafe against the local struct.
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

# DDS Request-vs-Offered: the subscription (or client) is the requester. Run
# `qos_compatible` in the role's direction; it returns the policy violations.
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

# Two independent gates: the `@warn` is time-throttled (`_mark_seen!`, 5 s per
# signature) for log-spam control, while the listener fan-out is edge-triggered
# (`_mark_fired!`, once per distinct (topic, kinds, reason)) so a listener sees
# every distinct mismatch but is never time-debounced. Conflating them would let
# a listener miss a mismatch during the throttle window.
function _emit_type_mismatch(d::_Detectors, local_e, remote, reason, suggestion)
    sig = (:type, local_e.topic, local_e.kind, remote.kind, reason)
    _mark_seen!(d, sig) &&
        @warn "type mismatch detected" topic=local_e.topic reason=reason suggestion=suggestion
    if _mark_fired!(d, sig)
        ev = TypeMismatch(local_e, remote, reason, suggestion)
        _fire(d.lock, d.type_listeners, ev, "on_type_mismatch")
    end
    nothing
end

# Route a pre-built `TypeMismatch` from the per-sample backstop
# (`check_sample_type`) through the same two gates and the same signature as
# `_emit_type_mismatch`, so a mismatch seen on the wire and in the graph reports
# once. Fires per mismatched sample, so the edge-trigger is what keeps a hot
# mismatched topic from flooding listeners.
function report_type_mismatch!(ctx::Context, tm::TypeMismatch)
    d = _detectors(ctx)
    sig = (:type, tm.local_endpoint.topic, tm.local_endpoint.kind,
           tm.remote_endpoint.kind, tm.reason)
    _mark_seen!(d, sig) &&
        @warn "type mismatch detected (on the wire)" topic=tm.local_endpoint.topic reason=tm.reason suggestion=tm.suggestion
    _mark_fired!(d, sig) && _fire(d.lock, d.type_listeners, tm, "on_type_mismatch")
    nothing
end

# Two-gate split mirrors `_emit_type_mismatch`: throttled `@warn`, edge-triggered
# listener fan-out.
function _emit_qos_incompat(d::_Detectors, local_e, remote, issues, suggestion)
    sig = (:qos, local_e.topic, local_e.kind, remote.kind,
           Tuple(i.policy for i in issues))
    _mark_seen!(d, sig) &&
        @warn "QoS incompatibility detected" topic=local_e.topic suggestion=suggestion
    if _mark_fired!(d, sig)
        ev = QosIncompatible(local_e, remote, issues, suggestion)
        _fire(d.lock, d.qos_listeners, ev, "on_qos_incompatible")
    end
    nothing
end

# Time-throttle gate: true the first time a signature is seen or once the throttle
# window has elapsed since the last report, bounding repeat `@warn`s per signature.
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

# Edge-trigger gate: true only the first time a signature stands up, so the
# listener fan-out fires once per distinct mismatch even on a hot per-sample topic.
function _mark_fired!(d::_Detectors, sig)
    @lock d.lock (sig in d.fired ? false : (push!(d.fired, sig); true))
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
    on_type_mismatch(f, node_or_ctx; scan=false) -> f

Register `f(::TypeMismatch)` to fire when one of our endpoints and a remote
endpoint on the same topic carry incompatible types — a different type name, or
the same name with a different RIHS01 version. Return `f`. The ROS 2
incompatible-type event surface.

Detection runs as a listener on the discovery change stream: when an endpoint
is added, it is compared against standing endpoints of the complementary kind
(our subscription versus their publisher, and vice versa) on the same topic —
a new remote against our locals, a new local endpoint against the remotes
already discovered.

- `f` fires once per distinct mismatch — keyed by a (topic, kinds, reason)
  signature, edge-triggered with no time throttle — so a flapping or
  hot-per-sample topic fans out exactly once per signature.
- The internal `@warn` is separately throttled (a logging concern only) and does
  not gate `f`.
- The per-sample backstop for wildcard subscriptions (`check_sample_type`)
  shares the signature, so a mismatch seen both on the wire and in the graph
  fires `f` once.
- Listeners run outside the detector lock; a throwing listener is logged and the
  others still run.

Detection is change-driven and covers only endpoints that appear after
registration:

- `scan=false` (default): a mismatch already standing in the graph when `f` is
  registered does not fire retroactively, so register before the endpoints exist
  to catch them.
- `scan=true`: also walks the standing graph snapshot once at registration and
  fires `f` for pre-existing mismatches (the watchdog/dashboard use case); the
  same per-signature edge-trigger applies, so a scanned mismatch is not fired
  again by a later change, and only the just-registered `f` receives the
  retroactive events.

```julia
on_type_mismatch(node) do tm
    @error "type mismatch" topic=tm.local_endpoint.topic reason=tm.reason
end
```
"""
function on_type_mismatch(f::Function, node_or_ctx; scan::Bool=false)
    ctx = _ctx(node_or_ctx)
    d = _detectors(ctx)
    @lock d.lock push!(d.type_listeners, f)
    _ensure_detectors_installed!(ctx, d)
    scan && _scan_for_listener!(ctx, d, f, :type)
    f
end

"""
    on_qos_incompatible(f, node_or_ctx; scan=false) -> f

Register `f(::QosIncompatible)` to fire when one of our endpoints shares a
topic with a remote endpoint whose QoS is incompatible with ours — under the
DDS Request-vs-Offered rule, the offered profile ranks below the requested one
on some policy. Return `f`. The ROS 2 incompatible-QoS event surface,
covering reliability, durability, deadline, and liveliness (kind and lease).

Detection runs on the discovery change stream and fires once per distinct
signature, the same way [`on_type_mismatch`](@ref) does — edge-triggered, not
time-throttled (only the internal `@warn` is throttled). The check runs in the
DDS Request-vs-Offered direction for whichever role is local:

| Local role | Requested side | Offered side |
|------------|----------------|--------------|
| Subscription or Client | the local endpoint | the remote |
| Publisher or Service | the remote | the local endpoint |

Listeners run outside the detector lock.

Detection is change-driven:

- `scan=false` (default): an incompatibility already standing in the graph at
  registration does not fire retroactively, so register before the endpoints
  exist to catch them.
- `scan=true`: also walks the standing graph snapshot once at registration and
  fires `f` for pre-existing incompatibilities; as in [`on_type_mismatch`](@ref)
  the per-signature edge-trigger applies and only the just-registered `f`
  receives the retroactive events.
"""
function on_qos_incompatible(f::Function, node_or_ctx; scan::Bool=false)
    ctx = _ctx(node_or_ctx)
    d = _detectors(ctx)
    @lock d.lock push!(d.qos_listeners, f)
    _ensure_detectors_installed!(ctx, d)
    scan && _scan_for_listener!(ctx, d, f, :qos)
    f
end

"""
    check_sample_type(sub, sample) -> Union{TypeMismatch, Nothing}

Per-sample type backstop: for a wildcard subscription, the concrete remote
type only appears on the wire, not in a graph match — so on receipt, parse the
sample's topic keyexpr and compare its `TypeInfo` to the subscription's.

- `TypeMismatch` with reason `:name` — the sample's type name differs from the
  subscription's.
- `TypeMismatch` with reason `:hash` — same type name, different RIHS01 version.
- `nothing` — the types agree.
- `nothing` — the key carries no type (ros2dds, EMPTY).

The caller decides whether to drop or warn on a returned `TypeMismatch`. The
subscription dispatch path calls this before decoding for weak-static
subscriptions and routes a mismatch to the type-mismatch listeners; a hash
mismatch means the bytes are decode-unsafe against the local struct.
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

# A minimal remote `EndpointInfo` for a per-sample mismatch: the keyexpr yields
# only the type, so the node and QoS fields stay empty.
_remote_shell(local_endpoint, got::TypeInfo) =
    EndpointInfo("", "", Publisher, local_endpoint.topic, got,
                 local_endpoint.qos, ntuple(_ -> 0x00, 16), false)

# ── QoS event surface: liveliness-changed & message-lost ────────────────────

"""
    liveliness_changed(node_or_ctx, topic) -> (; alive, not_alive)

A point-in-time count of live *remote* publishers on `topic` (resolved FQN),
read straight from the discovery stream. Liveliness rides the Zenoh tokens the
index already tracks — a present token means alive, a withdrawn (DELETE) one
means gone. For change *events*, register an [`on_graph_change`](@ref) listener
filtered to the topic.

The returned named tuple mirrors the ROS 2 `LivelinessChanged` fields:

- `alive` — the number of remote publishers currently present in the graph.
- `not_alive` — always `0`: the graph holds only live tokens (a withdrawn token
  is removed), so transitions are reported via the change stream rather than a
  standing count.
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
from the rmw_zenoh per-message sequence number. `topic` is the resolved
topic; `source_gid` is the publisher whose stream gapped; `expected_seq` is the
next sequence number we expected from it; `got_seq` is the one that arrived; and
`count` is the number of messages lost in between (`got_seq - expected_seq`).

ROS 2 inherits this event from DDS; here it is reconstructed from the
`sequence_number` the rmw_zenoh put attachment carries per message rather
than from a DDS-native lost count.
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
# small back-step is an ignored reorder/dupe (best-effort delivery can reorder); a
# large one is a publisher restart (per-instance counters) and rebaselines the mark.
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
number jumping by more than one between consecutive received messages.
Return `f`. The ROS 2 message-lost event surface.

Registering the first listener flips a per-subscription gate so the dispatch
path begins decoding each sample's metadata and feeding it to the detector
(`note_sequence!`); with no listener the common path skips that decode
entirely. Sequence tracking is per source gid, so a topic with several
publishers is tracked independently per publisher. Reordered or duplicate
messages (a sequence number slightly below the high-water mark) are ignored,
matching best-effort delivery; a sequence number far below it is treated as a
publisher restart and rebaselines tracking for that gid.

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

Feed one received sample to the message-lost detector for `sub`: decode
`(seq, _, gid)` and, if `seq` skipped ahead of the last value seen from that
`gid`, fire the `on_message_lost` listeners and return the `MessageLost`.

Returns `nothing` when:

- the message is in-order,
- it is the very first message from a gid, or
- the sample carries no usable attachment.

The subscription dispatch path calls this per sample, gated on
`Entity._track_lost`, so the common no-listener path never decodes the
attachment.
"""
function note_sequence!(sub, sample)
    e = _client_entity(sub)
    decoded = try
        decode_attachment(sample)
    catch
        nothing                        # garbled attachment
    end
    decoded === nothing && return nothing  # absent/garbled — nothing to track
    seq, _, srcgid = decoded
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
        # Track the high-water mark; small back-steps (≤64, comfortably above
        # best-effort reorder depth) are reorders/dupes and don't lower it. A larger
        # drop means the per-publisher-instance counter reset — rebaseline.
        if last === nothing || seq > last || seq < last - 64
            t.last_seq[srcgid] = seq
        end
    end
    lost === nothing && return nothing
    @warn "message lost" topic=e.endpoint.topic lost=lost.count
    _fire(t.lock, t.listeners, lost, "on_message_lost")
    lost
end

# Drop a subscription's detector state on close (from the entity teardown path);
# safe for entities that never registered one.
function _forget_lost_tracker!(e::Entity)
    @lock _LOST_LOCK delete!(_LOST, objectid(e))
    nothing
end

# deadline-missed and lifespan enforcement are data-plane concerns: they need the
# node clock and the subscription dispatch hook, so they belong with dispatch, not
# here. Keeping them out leaves this file a pure view over the graph + attachment
# stream that precompiles standalone.
# TODO: deadline-missed Timer (notify-only, matches ROS2) and lifespan drop
# (compare `source_timestamp` to `now` against `qos.lifespan`).
