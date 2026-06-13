# ── ActionClient ─────────────────────────────────────────────────────────────
# `send` issues a send_goal request and returns a client-side goal handle that is
# iterable over feedback, `fetch`-able for the result, and `cancel`-able. The
# three services are reached with Zenoh `get` (one per call); feedback is a
# Subscription on the feedback topic filtered by goal id.

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
    # rmw_zenoh request attachment: the three action services are `get`s, and a
    # real rmw_zenoh queryable reads the request's `(sequence_number, source_timestamp,
    # source_gid)` attachment to stamp its reply — hiroz `.unwrap()`s it, so a request
    # without one panics the peer. Stable per-client gid + a per-request seq.
    const _gid::NTuple{16, UInt8}
    @atomic _seq::Int64
    @atomic open::Bool
    # Lazy routing-match probes (`_ClientWire`s, service.jl) for `send_goal` and
    # `get_result`, declared on first `wait_for_action_server`/`action_server_matched`
    # and `nothing` until then. `send`/`fetch` use one-shot gets and never read these.
    @atomic _match::Union{Vector{_ClientWire}, Nothing}
    const _match_lock::ReentrantLock
end

function ActionClient(node::Node, name::AbstractString, ::Type{A};
                      qos::QosProfile = default_qos()) where {A}
    support = ActionTypeSupport(A)
    G = goal_type(support); R = result_type(support); F = feedback_type(support)
    fqn = resolve_name(node, name; kind=:service)
    gid = ROSZenoh.entity_gid(node.entity.z_id, next_entity_id!(node.context))
    ActionClient{A, G, R, F}(node, fqn, support, qos, gid, 0, true, nothing, ReentrantLock())
end

# The rmw_zenoh per-request attachment for an action service `get`. Mirrors the
# `ServiceClient` path so a request reaching a C++/Rust peer carries the metadata it
# echoes back into the reply (without it, hiroz's `query.attachment().unwrap()` panics).
_request_attachment(client::ActionClient) =
    encode_attachment((@atomic client._seq += 1), _now_ns(client.node), client._gid)

# The action sub-service `TypeInfo` for a client call (`:send_goal` / `:get_result`
# / `:cancel_goal`), or the action-typed info as a fallback — the client side of
# `_action_service_tis`, keeping the client's `get` keyexpr matched to the server.
function _client_service_ti(client::ActionClient{A}, kind::Symbol) where {A}
    tis = _action_service_tis(A, client.support)
    tis === nothing ? type_info(A) : getfield(tis, kind)
end

Base.isopen(c::ActionClient) = @atomic c.open
function Base.close(c::ActionClient)
    (@atomicswap c.open = false) || return nothing
    ws = @atomicswap c._match = nothing             # reap the lazy match probes, if any
    ws === nothing || for w in ws; (try; close(w); catch; end); end
    nothing
end
Base.show(io::IO, c::ActionClient{A}) where {A} =
    print(io, "ActionClient(", c.name, ", ", nameof(A),
          isopen(c) ? "" : ", closed", ")")

# Lazily declare matching probes (Queriers on the keyexprs `send`/`fetch` query) on
# first await of the server, reusing the shared `_ClientWire` machinery (service.jl).
# Probe both `send_goal` and `get_result`: `send` retries discovery itself, but
# `fetch`'s `get_result` is a one-shot get with a long timeout, so awaiting its match
# is what spares `fetch` from blocking on an unmatched service. A client that never
# awaits declares no probes.
function _ensure_action_match!(client::ActionClient)
    ws = @atomic client._match
    ws === nothing || return ws
    @lock client._match_lock begin
        ws = @atomic client._match
        ws === nothing || return ws
        ctx = client.node.context
        wires = _ClientWire[]
        for (topic_of, kind) in ((_send_goal_topic, :send_goal),
                                 (_get_result_topic, :get_result))
            tk = _service_key(client, topic_of(client.name), _client_service_ti(client, kind))
            w  = _ClientWire(Querier(ctx.session, Keyexpr(tk); target = :best_matching),
                             nothing, ReentrantLock())
            _ensure_wire_listener!(w, ctx)
            push!(wires, w)
        end
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
    all(_wire_matched, _ensure_action_match!(client))
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

`:rejected` covers every no-acceptance outcome alike — an `on_goal` rejection, an
error reply (server inactive), no server discovered within the window, and a
Context drain mid-window — so a caller that must tell them apart checks
[`action_server_matched`](@ref) or `is_shutdown` separately.

Throws `ArgumentError` if `client` is already closed.

```julia
goal = send(client, Fibonacci_Goal(; order = 10))
state(goal) === :accepted || error("rejected")
result = fetch(goal)
```
"""
function send(client::ActionClient{A, G, R, F}, goal::G) where {A, G, R, F}
    isopen(client) || throw(ArgumentError("send on a closed ActionClient"))
    id = _new_goal_id()
    ctx = client.node.context
    tk = _service_key(client, _send_goal_topic(client.name), _client_service_ti(client, :send_goal))

    # The request is `<A>_SendGoal_Request{goal_id, goal}`; we mint the id (the
    # correlation token reused on feedback / get_result / cancel). The reply is the
    # `<A>_SendGoal_Response{accepted, stamp}`.
    req = _send_goal_request_type(A)(; goal_id = _to_uuid(id), goal = goal)
    # The send_goal queryable can lag the liveliness token a peer discovers it by, so a
    # one-shot `get` to a not-yet-route-matched queryable returns no reply — an empty
    # result here means undiscovered, not rejected. Retry until a reply arrives (then it
    # is authoritative) or the window elapses (no server ⇒ rejected); a retried `get`
    # only re-fires while the route stays empty.
    accepted = false
    got_reply = false
    for _ in 1:40                                          # up to ~2s of 50ms backoffs
        for r in Base.get(ctx.session, Keyexpr(tk), ""; payload=encode(req),
                          attachment=_request_attachment(client), timeout_ms=5000)
            got_reply = true
            if Zenoh.is_ok(r)
                resp = decode_owned(Zenoh.sample(r), _send_goal_response_type(A))
                accepted = resp.accepted
            end
            break                   # a service has a single reply
        end
        (got_reply || is_shutdown(ctx)) && break
        sleep(0.05)
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
    # A bounded Channel fed by a Subscription on the feedback topic. The subscription
    # decodes `<A>_FeedbackMessage`, drops messages for other goals, and forwards the
    # unwrapped `Feedback`; the stream ends when the goal's result slot fills.
    ch = Channel{F}(32)
    client = g.client
    sub = _make_subscription(client.node, _feedback_topic(client.name),
                             _feedback_message_type(A); qos=client.qos) do msg
        _from_uuid(msg.goal_id) == g.id && isopen(ch) && put!(ch, msg.feedback)
    end
    g._feedback_sub = sub
    # The framework drives `get_result` (the single settlement signal), so the stream
    # ends on settle even without a `fetch` call. The closer blocks on the result slot,
    # then tears the subscription down.
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

# Start (once) the single `get_result` task that fills the goal's write-once result
# slot. Idempotent — `fetch` and `feedback` both call it; only the first spawns.
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

# One `get_result` round-trip → the `Result`, or throws. Like `send_goal`, the
# queryable can lag discovery, so a get to an unmatched route returns no reply; retry
# until one lands. Once matched the server holds the reply until the goal settles, so
# the matched attempt blocks to the timeout for the real result; retries only re-fire
# while the route is still empty.
function _get_result_once(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    client = g.client
    ctx = client.node.context
    tk = _service_key(client, _get_result_topic(client.name), _client_service_ti(client, :get_result))
    req = _get_result_request_type(A)(; goal_id = _to_uuid(g.id))
    local got::Union{R, Nothing} = nothing
    local result_state::Symbol = :aborted
    got_reply = false
    for _ in 1:40                                          # ~2s of 50ms backoffs to match
        for r in Base.get(ctx.session, Keyexpr(tk), "";
                          payload=encode(req), attachment=_request_attachment(client),
                          timeout_ms=_GET_RESULT_TIMEOUT_MS)
            got_reply = true
            if Zenoh.is_ok(r)
                resp = decode_owned(Zenoh.sample(r), _get_result_response_type(A))
                result_state = _client_state_from_status(resp.status)
                got = resp.result
            end
            break
        end
        (got_reply || is_shutdown(ctx)) && break
        sleep(0.05)
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
that result. Raises an error if the goal was rejected, the server reported a
failure, or the result never arrived (a Context drain ends the `get_result` retry
early, surfacing as a timed-out-result error).
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

# get_result wait bound (ms): longer than the server's result-cache TTL so a real
# result is always seen, finite so a never-coming reply can't wedge `fetch`.
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
"""
function cancel(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    client = g.client
    ctx = client.node.context
    tk = _service_key(client, _cancel_goal_topic(client.name), _client_service_ti(client, :cancel_goal))
    # Request `action_msgs/CancelGoal_Request{goal_info{goal_id, stamp}}` targeting
    # this goal (a zeroed goal_id would be the server's cancel-all sentinel).
    req = _CancelGoal_Request(; goal_info = _GoalInfo(; goal_id = _to_uuid(g.id),
                                                      stamp = to_msg(_Time, Dates.now(client.node))))
    for r in Base.get(ctx.session, Keyexpr(tk), ""; payload=encode(req),
                      attachment=_request_attachment(client), timeout_ms=5000)
        break
    end
    _is_terminal_client_state(@atomic g._state) || @atomic g._state = :canceling
    nothing
end

# The Zenoh data-route keyexpr for a client-side service call: build the wire key
# the server's queryable declared. We construct a transient `EndpointEntity` of
# the matching kind so `topic_keyexpr` produces the same key — the client's own
# id doesn't enter the topic key (only the topic + type + hash do).
function _service_key(client::ActionClient, topic::AbstractString, ti::TypeInfo)
    node = client.node
    e = ROSZenoh.EndpointEntity(; id=0, node=node.entity, kind=Service,
                                topic=String(topic), type_info=ti, qos=client.qos)
    return topic_keyexpr(node.context.format, e)
end

