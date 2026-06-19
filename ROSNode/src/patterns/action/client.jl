# ── ActionClient ─────────────────────────────────────────────────────────────
# Each of the three action services is reached with a one-shot Zenoh `get`;
# feedback is a Subscription on the feedback topic filtered by goal id.

"""
    ActionClient(node, name, A; qos=default_qos()) -> ActionClient

A ROS 2 action client for action type `A`, declared on `node` at the resolved
name `name`. Dispatch a goal with [`send`](@ref), then iterate
[`feedback`](@ref), block on `fetch` for the result, and [`cancel`](@ref) to
request cancellation. The three sub-messages of `A` — `Goal`/`Result`/`Feedback`
— type the call and reply surface.

Each protocol-service call is a fresh one-shot Zenoh `get` carrying the rmw_zenoh
request attachment (`sequence_number`, `source_timestamp`, `source_gid`) a real
ROS 2 peer echoes into its reply. Feedback is a per-goal subscription on the
`feedback` topic, filtered to the goal's id. Routing-match probes for `send_goal`
and `get_result` are declared lazily on the first
[`action_server_matched`](@ref)/[`wait_for_action_server`](@ref) call.

`send`, `fetch`, and `feedback` resolve the protocol-wrapper structs `@ros_msgs`
generates; an action generated with bare `@ros_msg` raises `ArgumentError` on the
first such call.

`close`-able and idempotent (reaping any lazy match probes); dies with its node.

Follows the ROS 2 actions concept:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html
"""
mutable struct ActionClient{A, G, R, F}
    const node::Node
    const name::String                   # resolved action FQN
    const support::ActionTypeSupport{A, G, R, F}
    const qos::QosProfile
    # The three SERVICE-level `TypeInfo`s (send_goal/get_result/cancel_goal), computed
    # ONCE here — a pure function of `A`. Recomputing it per `send`/`get_result` was the
    # dominant action-call allocation (~56 KB/goal of RIHS/keyexpr derivation). `nothing`
    # when the Goal/Result types lack registered wire descriptions (the action-TI fallback).
    const _service_tis::Any
    # The three service-call keyexpr strings (send_goal/get_result/cancel_goal), built
    # ONCE here. They are a pure function of the action FQN + the (cached) service
    # TypeInfos, but `_service_key` rebuilt them per call — an `EndpointEntity` +
    # `topic_keyexpr`/RIHS derivation, ~2.4 KB/goal. Cached, each `send`/`fetch`/`cancel`
    # just wraps the stored string in a `Keyexpr`.
    const _service_keys::NamedTuple{(:send_goal, :get_result, :cancel_goal), NTuple{3, String}}
    # Stable per-client gid + per-request seq for the rmw_zenoh request attachment.
    # A native queryable reads the request's (sequence_number, source_timestamp,
    # source_gid) to stamp its reply and hard-panics on an absent attachment, so the
    # gid stays a fixed 16-byte tuple and every service `get` carries one.
    const _gid::NTuple{16, UInt8}
    @atomic _seq::Int64
    @atomic open::Bool
    # The three service-call wires (send_goal/get_result/cancel_goal): each a `Querier`
    # on the service keyexpr plus a pooled `ReusableGet` transport. Declared lazily on
    # first `send`/`fetch`/`cancel`/`wait_for_action_server`; `send`/`fetch`/`cancel`
    # `call!` over the pool (the wires also serve as the routing-match probes).
    @atomic _match::Union{NamedTuple{(:send_goal, :get_result, :cancel_goal), NTuple{3, _ClientWire}}, Nothing}
    const _match_lock::ReentrantLock
end

function ActionClient(node::Node, name::AbstractString, ::Type{A};
                      qos::QosProfile = default_qos(),
                      warmup::Union{Symbol, WarmupMode, Nothing}=nothing,
                      warmup_sync::Union{Bool, Nothing}=nothing) where {A}
    support = ActionTypeSupport(A)
    G = goal_type(support); R = result_type(support); F = feedback_type(support)
    fqn = resolve_name(node, name; kind=:service)
    gid = ROSZenoh.entity_gid(node.entity.z_id, next_entity_id!(node.context))
    stis = _action_service_tis(A, support)
    _ti(kind) = stis === nothing ? type_info(A) : getfield(stis, kind)   # per-kind service TypeInfo
    keys = (send_goal   = _service_key(node, qos, _send_goal_topic(fqn),   _ti(:send_goal)),
            get_result  = _service_key(node, qos, _get_result_topic(fqn),  _ti(:get_result)),
            cancel_goal = _service_key(node, qos, _cancel_goal_topic(fqn), _ti(:cancel_goal)))
    client = ActionClient{A, G, R, F}(node, fqn, support, qos, stis, keys,
                                      gid, 0, true, nothing, ReentrantLock())
    _warmup!(_resolve_warmup(node, warmup, warmup_sync), () -> _warm_client(client))
    return client
end

# Warm the request-wrapper encode paths only — the control-plane reply decode is
# infrequent and left to JIT. Guarded so a missing wrapper accessor never breaks
# client construction.
function _warm_client(c::ActionClient{A, G, R, F}) where {A, G, R, F}
    try
        precompile(encode, (_action_wrapper(A, "_SendGoal_Request"),))
        precompile(encode, (_action_wrapper(A, "_GetResult_Request"),))
    catch
    end
    nothing
end

# The rmw_zenoh per-request attachment for an action service `get`, as the `Vector{UInt8}`
# `call!` aliases zero-copy. A native peer echoes it into its reply and panics if absent, so
# every get carries one; the per-request `seq` advances here.
_request_attachment(client::ActionClient) =
    ROSZenoh.attachment_bytes((@atomic client._seq += 1), _now_ns(client.node), client._gid)

# Issue one action service request (`kind`) over its pooled `ReusableGet`, retrying the
# discovery window while the route is unmatched (an unmatched get completes with no reply,
# so `call!` returns `nothing` promptly — empty ≠ rejected). `bytes` is the request CDR,
# aliased zero-copy by `call!`; the rmw_zenoh attachment (which a native peer echoes back
# and panics if absent) rides each get as a fresh `Vector`. The reply, if any, is handed to
# `decode(h)` BEFORE the pooled `rg` is released — the borrowed holder is invalid after the
# next `call!`; `decode(nothing)` covers the no-reply case. Returns whatever `decode` returns.
function _action_call(decode::F, client::ActionClient, kind::Symbol,
                      bytes::Vector{UInt8}; retries::Int = 40) where {F}
    ctx = client.node.context
    w = getfield(_ensure_action_match!(client), kind)
    rg = _acquire_rg!(w)
    try
        for _ in 1:retries                                    # ~retries × 50ms discovery backoffs
            h = Zenoh.call!(rg; payload = bytes, attachment = _request_attachment(client))
            h === nothing || return decode(h)                 # decode before the `finally` releases rg
            is_shutdown(ctx) && break
            sleep(0.05)
        end
        return decode(nothing)
    finally
        _release_rg!(w, rg)
    end
end

Base.isopen(c::ActionClient) = @atomic c.open
function Base.close(c::ActionClient)
    (@atomicswap c.open = false) || return nothing
    ws = @atomicswap c._match = nothing             # reap the lazy call/match wires
    ws === nothing || for w in (ws.send_goal, ws.get_result, ws.cancel_goal)
        (try; close(w); catch; end)                 # closes the pooled ReusableGets, then the Querier
    end
    nothing
end
Base.show(io::IO, c::ActionClient{A}) where {A} =
    print(io, "ActionClient(", c.name, ", ", nameof(A),
          isopen(c) ? "" : ", closed", ")")

# Per-kind `get` timeout baked into each call Querier (ms): `send`/`cancel` settle on a
# prompt accept/ack; `get_result` blocks until the goal settles, so it gets the long bound.
_action_call_timeout(kind::Symbol) = kind === :get_result ? _GET_RESULT_TIMEOUT_MS : 5000

# Lazily declare the three call/match wires (Queriers on the send_goal/get_result/cancel_goal
# keyexprs, each with a pooled `ReusableGet` transport) on first `send`/`fetch`/`cancel`/await.
# The send_goal + get_result wires double as the routing-match probes. A client that never
# touches its server declares none.
function _ensure_action_match!(client::ActionClient)
    ws = @atomic client._match
    ws === nothing || return ws
    @lock client._match_lock begin
        ws = @atomic client._match
        ws === nothing || return ws
        ctx = client.node.context
        mk(kind) = begin
            tk = getfield(client._service_keys, kind)        # cached at construction
            w  = _ClientWire(Querier(ctx.session, Keyexpr(tk); target = :best_matching,
                                     timeout_ms = _action_call_timeout(kind)),
                             nothing, ReentrantLock())
            _ensure_wire_listener!(w, ctx)
            w
        end
        wires = (send_goal = mk(:send_goal), get_result = mk(:get_result), cancel_goal = mk(:cancel_goal))
        @atomic client._match = wires
        return wires
    end
end

"""
    action_server_matched(client::ActionClient) -> Bool

True when the client is routing-matched to an action server right now: both its
`send_goal` and `get_result` Queriers have matched a server's queryables. When
true, [`send`](@ref) and `fetch` reach the server without discovery backoff;
until then they retry on discovery. `false` for a closed client.

The routing-plane signal behind [`wait_for_action_server`](@ref). The first call
lazily declares the two match probes (Queriers) on the client; a client that
never awaits its server declares none.
"""
function action_server_matched(client::ActionClient)
    isopen(client) || return false
    ws = _ensure_action_match!(client)
    _wire_matched(ws.send_goal) && _wire_matched(ws.get_result)
end

"""
    ClientGoal{A, G, R, F}

A client-side handle for a dispatched goal: iterable over feedback,
`fetch`-able for the result, `cancel`-able, and `state`-queryable. The result is
held in a write-once slot the framework fills from the server's `get_result`
reply. Returned by [`send`](@ref) once the server has accepted or rejected.
"""
mutable struct ClientGoal{A, G, R, F}
    const client::ActionClient{A, G, R, F}
    const id::GoalId
    @atomic _state::Symbol               # :accepted/:rejected/:canceling/terminal
    # Lazily-opened feedback subscription (a SubscriptionHandle); nothing until
    # `feedback(goal)` is first iterated.
    _feedback_sub::Any
    # A goal settles exactly once, so its result is a single write-once slot. One
    # framework `get_result` task (started lazily by `fetch` or `feedback`) fills
    # `_outcome` once — `Some(result)` on success, an `Exception` on failure — and
    # notifies `_settled`. `fetch` returns/raises it (idempotent across repeat calls);
    # `feedback` ends its stream when it fills.
    @atomic _outcome::Union{Some{R}, Exception, Nothing}
    const _settled::Threads.Condition
    @atomic _result_started::Bool
end

goal_id(g::ClientGoal) = g.id
state(g::ClientGoal) = @atomic g._state
Base.show(io::IO, g::ClientGoal{A}) where {A} =
    print(io, "ClientGoal(", nameof(A), " ", (@atomic g._state), ")")

"""
    send(client, goal) -> ClientGoal

Dispatch `goal` (a `Goal` struct for the action) on `client` and return a
client-side goal handle once the server replies accepted or rejected. Blocks the
calling task on the `send_goal` service reply.

The client mints the 16-byte goal id (the correlation token reused for feedback,
`get_result`, and cancel). Because a queryable can lag the liveliness token a
peer discovers it by, `send` retries the one-shot `get` over roughly a two-second
window (50 ms backoffs) while the route stays unmatched; the first reply that
lands is authoritative. Inspect the outcome with [`state`](@ref) (`:accepted` or
`:rejected`), iterate [`feedback`](@ref), and block on `fetch` for the result.

`:rejected` covers every no-acceptance outcome alike:

- an `on_goal` rejection,
- an error reply (server inactive),
- no server discovered within the window, and
- a Context drain mid-window.

A caller that must tell them apart checks [`action_server_matched`](@ref) or
`is_shutdown` separately.

Throws:

- `ArgumentError` if `client` is already closed.
- [`NodeInactiveError`](@ref) if the client's node is a managed node that is not
  [`Active`](@ref) (probe [`isactive`](@ref)`(node)` first).

```julia
goal = send(client, Fibonacci_Goal(; order = 10))
state(goal) === :accepted || error("rejected")
result = fetch(goal)
```
"""
function send(client::ActionClient{A, G, R, F}, goal::G) where {A, G, R, F}
    isopen(client) || throw(ArgumentError("send on a closed ActionClient"))
    # Outbound gate: a client on a non-Active managed node must not address a peer
    # (entry-only — an in-flight call finishes). Unmanaged nodes are always active.
    isactive(client.node) ||
        throw(NodeInactiveError("send on a client of an inactive node; probe isactive(node) first"))
    id = _new_goal_id()
    # The client mints the goal id (the correlation token reused on feedback /
    # get_result / cancel) into `<A>_SendGoal_Request{goal_id, goal}`. An accepted reply
    # ⇒ :accepted; a not-ok reply or no reply within the discovery window ⇒ :rejected.
    req = _send_goal_request_type(A)(; goal_id = _to_uuid(id), goal = goal)
    accepted = _action_call(client, :send_goal, _encode_to_vector(req)) do h
        h !== nothing && Zenoh.is_ok(h) &&
            decode_owned(Zenoh.sample(h), _send_goal_response_type(A)).accepted
    end
    g = ClientGoal{A, G, R, F}(client, id, accepted ? :accepted : :rejected,
                               nothing, nothing, Threads.Condition(), false)
    return g
end

"""
    feedback(goal) -> iterable

Stream the goal's feedback messages, each a `Feedback` struct. Lazily opens
a subscription on the action's feedback topic on first iteration; the stream ends
when the goal reaches a terminal state. The topic carries `<A>_FeedbackMessage`
(goal_id + feedback), so the stream is filtered to *this* goal's id.
"""
function feedback(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    # A bounded Channel (sized to the feedback QoS depth) fed by a Subscription that
    # decodes `<A>_FeedbackMessage` and forwards this goal's `Feedback`. Drop-oldest on
    # enqueue (KEEP_LAST ring) so a slow or early-breaking consumer never blocks the
    # subscription callback task.
    client = g.client
    ch = Channel{F}(max(1, _fifo_capacity(client.qos)))
    sub = _make_subscription(client.node, _feedback_topic(client.name),
                             _feedback_message_type(A); qos=client.qos) do msg
        _from_uuid(msg.goal_id) == g.id && isopen(ch) && _put_drop_oldest!(ch, msg.feedback)
    end
    g._feedback_sub = sub
    # The framework drives `get_result` itself, so the stream ends on settle even with
    # no `fetch` call: the closer blocks on the result slot, then tears the sub down.
    _ensure_result!(g)
    errormonitor(Threads.@spawn begin
        try
            @lock g._settled begin
                while (@atomic g._outcome) === nothing
                    wait(g._settled)
                end
            end
        finally
            close(sub)
            isopen(ch) && close(ch)
        end
    end)
    return ch
end

_is_terminal_client_state(s::Symbol) = s in (:succeeded, :canceled, :aborted, :rejected)

# Exactly one `get_result` task per goal fills the write-once result slot. Both
# `fetch` and `feedback` call this; the atomic gate ensures only the first spawns.
function _ensure_result!(g::ClientGoal)
    (@atomicswap g._result_started = true) && return nothing
    errormonitor(Threads.@spawn _drive_result!(g))
    nothing
end

# Fill the result slot exactly once and wake everyone waiting on `_settled`. A rejected
# goal has no result; anything else issues `get_result`. Stores `Some(result)` on
# success or the raised `Exception` (which `fetch` re-raises).
function _drive_result!(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    outcome::Union{Some{R}, Exception} = try
        (@atomic g._state) === :rejected &&
            throw(ArgumentError("fetch: goal was rejected by the server"))
        Some(_get_result_once(g))
    catch err
        err isa Exception ? err : ErrorException(sprint(showerror, err))
    end
    @lock g._settled begin
        @atomic g._outcome = outcome
        notify(g._settled)
    end
    nothing
end

# One `get_result` round-trip → the `Result`, or throws. The queryable can lag
# discovery, so a get to an unmatched route returns no reply; retry until one lands.
# Once matched the server holds the reply until the goal settles, so the matched
# attempt blocks to the timeout for the real result.
function _get_result_once(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    client = g.client
    # Outbound gate (entry-only): no get_result query from a non-Active managed node.
    isactive(client.node) ||
        throw(NodeInactiveError("fetch on a client of an inactive node; probe isactive(node) first"))
    req = _get_result_request_type(A)(; goal_id = _to_uuid(g.id))
    # Once matched the server holds the reply until the goal settles (the wire's long
    # `get_result` timeout); an unmatched route returns no reply, so retry discovery.
    result_state, got = _action_call(client, :get_result, _encode_to_vector(req)) do h
        (h !== nothing && Zenoh.is_ok(h)) || return (:aborted, nothing)
        resp = decode_owned(Zenoh.sample(h), _get_result_response_type(A))
        (_client_state_from_status(resp.status), resp.result)
    end
    got === nothing &&
        throw(ErrorException("fetch: no result for goal (server error, evicted \
                              cache, or timed out after $(_GET_RESULT_TIMEOUT_MS)ms)"))
    @atomic g._state = result_state
    return got
end

"""
    fetch(goal) -> result

Block until the goal settles and return its `Result`. Backed by a write-once
slot filled from one `get_result` request — single-fill, like the server-side
[`respond!`](@ref): the first `fetch`/`feedback` starts it, every `fetch` returns
that result. Raises:

- an error if the goal was rejected.
- an error if the server reported a failure.
- a timed-out-result error if the result never arrived (a Context drain ends the
  `get_result` retry early, surfacing here).
- [`NodeInactiveError`](@ref) if the client's node is a managed node that is not
  [`Active`](@ref) when the `get_result` query starts (probe
  [`isactive`](@ref)`(node)` first).
"""
function Base.fetch(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    _ensure_result!(g)
    @lock g._settled begin
        while (@atomic g._outcome) === nothing
            wait(g._settled)
        end
    end
    o = @atomic g._outcome
    o isa Exception && throw(o)
    return something(o)::R
end

# get_result wait bound (ms): exceeds the server's result-cache TTL so a real result
# is always seen, and stays finite so a never-coming reply can't wedge `fetch`.
const _GET_RESULT_TIMEOUT_MS = Int(_RESULT_CACHE_TTL_NS ÷ 1_000_000) + 60_000

_client_state_from_status(b::Integer) =
    _state_symbol(GoalState(Int8(b)))

"""
    cancel(goal)

Request cancellation of a dispatched goal: issue a `cancel_goal` request to the
server, which runs its `on_cancel` and, on accept, moves the goal to `CANCELING`
so the handler's [`checkpoint`](@ref)/[`feedback!`](@ref) throw
[`Cancelled`](@ref). Returns `nothing` once the server replies (a one-shot `get`
with a 5-second timeout); the goal's terminal state is observed through `fetch`.

The request targets this goal's id (a zeroed id would be the server-side
cancel-all sentinel). The handle's local state moves to `:canceling` once the
reply lands, unless the goal has already settled — a terminal `state(goal)` is
kept; read the true terminal outcome through `fetch`, which is unaffected.

Throws [`NodeInactiveError`](@ref) if the client's node is a managed node that is
not [`Active`](@ref) (probe [`isactive`](@ref)`(node)` first).
"""
function cancel(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    client = g.client
    # Outbound gate (entry-only): no cancel_goal query from a non-Active managed node.
    isactive(client.node) ||
        throw(NodeInactiveError("cancel on a client of an inactive node; probe isactive(node) first"))
    # Request `action_msgs/CancelGoal_Request{goal_info{goal_id, stamp}}` targeting
    # this goal (a zeroed goal_id would be the server's cancel-all sentinel). Best-effort,
    # single-shot (`retries=1`): the reply is unused — we just need the ack to ride the wire.
    req = _CancelGoal_Request(; goal_info = _GoalInfo(; goal_id = _to_uuid(g.id),
                                                      stamp = to_msg(_Time, Dates.now(client.node))))
    _action_call((_) -> nothing, client, :cancel_goal, _encode_to_vector(req); retries = 1)
    _is_terminal_client_state(@atomic g._state) || @atomic g._state = :canceling
    nothing
end

# The Zenoh data-route keyexpr for a client-side service call: reproduce the key the
# server's queryable declared via a transient `EndpointEntity` of the matching kind,
# since the topic key is a pure function of topic + type + hash (not the client id).
# The service-call keyexpr string for one of the action's three services. Pure in
# (node, qos, topic, ti); called once per client per kind at construction, the results
# cached in `_service_keys` (see the struct). Takes `node`/`qos` rather than the client
# so it can run before the client is built.
function _service_key(node::Node, qos::QosProfile, topic::AbstractString, ti::TypeInfo)
    e = ROSZenoh.EndpointEntity(; id=0, node=node.entity, kind=Service,
                                topic=String(topic), type_info=ti, qos=qos)
    return topic_keyexpr(node.context.format, e)
end

