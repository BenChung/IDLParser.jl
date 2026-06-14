# ── ActionServer ─────────────────────────────────────────────────────────────
# The long-lived server: three Zenoh queryables (send_goal / cancel_goal /
# get_result), feedback + status publishers, a goal table (result cache + live
# goals), and the user callbacks. Each sub-endpoint is a generic `Entity`; its
# queryable/publisher route hangs off `entity.wire`.

"""
    ActionServer(node, name, A; on_goal=accept-all, on_cancel=accept-all, on_accepted, qos=default_qos())
    ActionServer(node, name, A; concurrency=Serial(), on_goal=…, on_cancel=…) do goal … end

A ROS 2 action server for action type `A`, declared on `node` at the resolved
name `name`. Owns the three action-protocol services (`send_goal`, `cancel_goal`,
`get_result`), the `feedback` and `status` topics, the goal table (live goals
plus the settled-result cache), and the user callbacks. The three sub-messages of
`A` — `Goal` (`G`), `Result` (`R`), `Feedback` (`F`) — drive the handler
signatures.

Two construction forms share one constructor:

- High-level (`do`-block): the body is the goal's execution. The framework spawns
  it wrapped in the cancellation + fail-safe settlement machinery, scheduled per
  [`Concurrency`](@ref). [`Serial`](@ref) (the default) holds goal bodies on the
  server's `serial_lock` so they run one at a time in acceptance order;
  [`Parallel`](@ref)`(n)` runs each body on its own OS thread, at most `n` in flight.
- Low-level: supply `on_accepted=callback`; you own when, whether, and where each
  accepted goal runs (the `GoalHandle` is handed to you). Pair this with
  [`SingleFlight`](@ref) or your own orchestrator.

Pass either a `do`-block body or `on_accepted=`, never both; a server with
neither raises `ArgumentError`. `on_goal(request)` returns
[`accept`](@ref)/[`reject`](@ref)/[`defer`](@ref) (default accept-all);
`on_cancel(goal)` returns `accept`/`reject` (default accept).

The three services key off rmw_zenoh SERVICE-level type names and RIHS01 hashes —
`<pkg>/action/<A>_SendGoal`, `…_GetResult`, and `action_msgs/srv/CancelGoal` fill
the type+hash segments of each data keyexpr — matching a real ROS 2 peer;
`feedback` and `status` carry their own wire types (`<A>_FeedbackMessage`,
`action_msgs/GoalStatusArray`). Construction requires `A` to be generated with
`@ros_msgs` so the protocol-wrapper structs exist; a bare `@ros_msg` raises
`ArgumentError`.

`close`-able and idempotent. Closing:

- awaits outstanding goal tasks (including detached post-cancel cleanup) up to the
  Context drain timeout before withdrawing each sub-endpoint's liveliness token and
  Zenoh route;
- leaves a goal task that overruns the timeout running detached (close warns and
  proceeds).

The server also dies with its node.

```julia
server = ActionServer(node, "fibonacci", Fibonacci) do goal
    seq = Int32[0, 1]
    for _ in 1:goal.request.order
        feedback!(goal, Fibonacci_Feedback(; partial_sequence = seq))
        push!(seq, seq[end] + seq[end-1])
    end
    Fibonacci_Result(; sequence = seq)
end
```

Follows the ROS 2 actions concept:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html
"""
mutable struct ActionServer{A, G, R, F}
    const node::Node
    const name::String                   # resolved action FQN
    const support::ActionTypeSupport{A, G, R, F}
    # User callbacks. `on_goal(request)::GoalResponse`; `on_cancel(goal)::GoalResponse`;
    # `on_accepted(goal)` schedules/executes (the high-level form fills this in).
    const on_goal::Function
    const on_cancel::Function
    const on_accepted::Function
    # The five sub-endpoints (Entity handles). Services carry their Queryable in
    # `.wire`; the topics carry their `Zenoh.Publisher` route.
    const send_goal_ent::Entity
    const cancel_goal_ent::Entity
    const get_result_ent::Entity
    const feedback_ent::Entity
    const status_ent::Entity
    # Goal table: id → GoalHandle, holding live + settled goals (the result cache).
    # A TTL sweep on each `_register_goal!` bounds the table and the per-transition
    # `_publish_status` cost that scales with it.
    const goals::Dict{GoalId, GoalHandle{A, G, R, F}}
    const lock::ReentrantLock
    # Serializes goal bodies under `concurrency = Serial()`. Dedicated to goal bodies
    # so the node-wide lock stays free while a long mission runs.
    const serial_lock::ReentrantLock
    # Per-endpoint attachment sequence_numbers, one per published topic.
    @atomic feedback_seq::Int64
    @atomic status_seq::Int64
    @atomic open::Bool
end

function _make_action_server(node::Node, name::AbstractString, ::Type{A};
                             on_goal::Function = _default_on_goal,
                             on_cancel::Function = _default_on_cancel,
                             on_accepted::Union{Function, Nothing} = nothing,
                             concurrency::Concurrency = Serial(),
                             body::Union{Function, Nothing} = nothing,
                             qos::QosProfile = default_qos(),
                             warmup::Union{Symbol, Nothing} = nothing,
                             warmup_sync::Union{Bool, Nothing} = nothing) where {A}
    support = ActionTypeSupport(A)
    G = goal_type(support); R = result_type(support); F = feedback_type(support)
    fqn = resolve_name(node, name; kind=:service)

    # Resolve the execution callback. A `do`-body is wrapped into an `on_accepted`
    # that schedules per `concurrency`; a bare `on_accepted=` is used as given; one
    # of the two is required.
    accepted = if body !== nothing
        on_accepted === nothing ||
            throw(ArgumentError("ActionServer: pass either a `do`-block body or \
                                 `on_accepted=`, not both"))
        _high_level_on_accepted(body, concurrency)
    elseif on_accepted !== nothing
        on_accepted
    else
        throw(ArgumentError("ActionServer requires execution: a `do goal … end` \
                             body or an `on_accepted=` callback"))
    end

    # Five sub-endpoints. Each of the three services keys its data keyexpr on its own
    # SERVICE-level type + RIHS01 (`<pkg>/action/<A>_SendGoal`, `…_GetResult`,
    # `action_msgs/srv/CancelGoal`), which is what routes against a native peer; the
    # action type's own hash routes only Julia-to-Julia and is the path taken when the
    # service hashes can't be synthesized (Goal/Result not registered with their TDs).
    stis = _action_service_tis(A, support)
    sg_ti = stis === nothing ? type_info(A) : stis.send_goal
    cg_ti = stis === nothing ? type_info(A) : stis.cancel_goal
    gr_ti = stis === nothing ? type_info(A) : stis.get_result
    send_goal_ent   = make_entity(node, Service,      _send_goal_topic(fqn),   sg_ti; qos=qos)
    cancel_goal_ent = make_entity(node, Service,      _cancel_goal_topic(fqn), cg_ti; qos=qos)
    get_result_ent  = make_entity(node, Service,      _get_result_topic(fqn),  gr_ti; qos=qos)
    # Feedback/status carry their own wire types (FeedbackMessage / GoalStatusArray):
    # the data keyexpr embeds type+hash, so a publisher must match what its subscriber
    # declares.
    feedback_ent    = make_entity(node, Publisher,    _feedback_topic(fqn),
                                  type_info_of(_feedback_message_type(A)); qos=qos)
    status_ent      = make_entity(node, Publisher,    _status_topic(fqn),
                                  type_info_of(_GoalStatusArray); qos=qos)

    server = ActionServer{A, G, R, F}(node, fqn, support, on_goal, on_cancel, accepted,
                                      send_goal_ent, cancel_goal_ent, get_result_ent,
                                      feedback_ent, status_ent,
                                      Dict{GoalId, GoalHandle{A, G, R, F}}(),
                                      ReentrantLock(), ReentrantLock(), 0, 0, true)

    _wire_action_server!(server)

    # Warm goal-decode → execution callable → result/feedback encode. Precompile-only:
    # a goal body is a long-running mission over a live handle, so fabricating one to
    # run is neither safe nor meaningful. `exec` is the do-block body or `on_accepted`.
    pol = _resolve_warmup(node, warmup, warmup_sync)
    exec = body !== nothing ? body : accepted
    _warmup!(pol, () -> _warm_action(G, R, F, exec, GoalHandle{A, G, R, F}))

    return server
end

_default_on_goal(_request)  = accept()
_default_on_cancel(_goal)   = accept()

# Declare the data routes: a Queryable per service, a Zenoh.Publisher per topic.
# Each queryable's callback runs on its own Julia task (the Zenoh callback
# trampoline), so a blocking accept decision never stalls the I/O thread.
function _wire_action_server!(server::ActionServer)
    sess = server.node.context.session

    # Feedback + status publishers (the shared Entity data-route declaration).
    declare_publisher!(server.feedback_ent)
    declare_publisher!(server.status_ent)

    _declare_queryable!(server.send_goal_ent,   q -> _handle_send_goal(server, q))
    _declare_queryable!(server.cancel_goal_ent, q -> _handle_cancel_goal(server, q))
    _declare_queryable!(server.get_result_ent,  q -> _handle_get_result(server, q))
    server
end

# Declare a callback-form Queryable on a Service `Entity` and stash it in `.wire` so
# it shares the entity's close lifecycle. Each Zenoh callback runs on its own Julia
# task. A handler throw is logged and swallowed at this dispatch leaf — only
# ShutdownException propagates — so one bad request can't kill the queryable.
function _declare_queryable!(e::Entity, handler)
    ctx = e.node.context
    tk = topic_keyexpr(ctx.format, e.endpoint)
    qable = Queryable(ctx.session, Keyexpr(tk)) do q
        try
            handler(q)
        catch err
            err isa ShutdownException && return
            @error "action service handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    e.wire = qable
    return qable
end

Base.isopen(server::ActionServer) = @atomic server.open
Base.show(io::IO, server::ActionServer{A}) where {A} =
    print(io, "ActionServer(", server.name, ", ", nameof(A),
          isopen(server) ? "" : ", closed", ")")

"This server's five sub-endpoint handles, one per [`Entity`](@ref)."
entities(server::ActionServer) = (server.send_goal_ent, server.cancel_goal_ent,
                                  server.get_result_ent, server.feedback_ent,
                                  server.status_ent)

"""
    close(server::ActionServer)

Undeclare the action server: close its five sub-endpoints (each withdraws its
liveliness token + Zenoh route), after awaiting outstanding goal tasks so a
settle-then-detached-cleanup goal finishes rather than being dropped mid-flight.
Idempotent.
"""
function Base.close(server::ActionServer)
    # Single-winner latch: a Context drain hook and an explicit close routinely race.
    (@atomicswap server.open = false) || return nothing
    # Await outstanding goal tasks (incl. detached post-cancel cleanup) up to the
    # Context drain timeout, so a goal mid-mission settles rather than being dropped.
    _await_goals(server)
    for e in entities(server)
        try
            close(e)
        catch err
            @error "close(ActionServer): closing sub-endpoint failed" exception=(err, catch_backtrace())
        end
    end
    nothing
end

# Snapshot live goal tasks and join them under the Context drain bound. The bound is
# what keeps a wedged goal from blocking close forever; an overrunning goal is left
# running detached, since Julia tasks can't be hard-killed.
function _await_goals(server::ActionServer)
    tasks = @lock server.lock Task[g._task for g in values(server.goals)
                                   if g._task !== nothing && !istaskdone(g._task)]
    isempty(tasks) && return nothing
    secs = server.node.context.drain_timeout
    done = Base.timedwait(Float64(secs); pollint=0.02) do
        all(istaskdone, tasks)
    end
    done === :ok ||
        @warn "close(ActionServer): goal tasks exceeded drain timeout; continuing" n=count(!istaskdone, tasks)
    nothing
end

"""
    cancel_all!(server::ActionServer; timeout = drain_timeout)

Cooperatively cancel every live goal: transition each `ACCEPTED`/`EXECUTING` goal
to `CANCELING` (arming the [`checkpoint`](@ref)/[`feedback!`](@ref) `Cancelled`
throw) and wait up to `timeout` seconds for the bodies to settle. The administrative
counterpart of a client `cancel` — it skips `on_cancel` since the server is being
brought down. A goal body that ignores cancellation past the budget is left running
detached with an `@warn` (same ceiling as `close`: Julia tasks can't be hard-killed).
Drives [`deactivate!`](@ref) of a managed action server (nav2 `terminate_all`).
"""
function cancel_all!(server::ActionServer{A, G, R, F};
                     timeout::Real = server.node.context.drain_timeout) where {A, G, R, F}
    # Snapshot under `server.lock`, then `_transition!` (takes `g.lock`) outside it:
    # server.lock must never be held across a `g.lock` acquire (same shape as
    # `_handle_cancel_goal`).
    live = @lock server.lock GoalHandle{A, G, R, F}[g for g in values(server.goals)
        if (@atomic g.status) in (GOAL_ACCEPTED, GOAL_EXECUTING)]
    canceling = GoalHandle{A, G, R, F}[g for g in live if _transition!(g, GOAL_CANCELING)]
    isempty(canceling) && return nothing
    _publish_status(server)
    # Bounded join on settlement: the body sees `Cancelled` at its next checkpoint and
    # fills the result cell (settles CANCELED). A body with no checkpoint can't settle
    # in time — leave it running, like `close`'s detached-goal ceiling.
    done = Base.timedwait(Float64(timeout); pollint=0.02) do
        all(g -> isfilled(g.cell), canceling)
    end
    done === :ok ||
        @warn "cancel_all!: goals ignored cancellation past the budget; continuing" server=server.name n=count(g -> !isfilled(g.cell), canceling)
    nothing
end

# ── send_goal service ─────────────────────────────────────────────────────────
# Decode the goal request (goal_id + goal), run `on_goal`, and reply
# accepted/rejected. On accept (not defer) we fire `on_accepted` so execution
# starts. The reply is the `SendGoal_Response` (accepted::Bool + stamp).

function _handle_send_goal(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
    # Dispatch gate: an inactive managed node's action services error-reply.
    isactive(server.send_goal_ent) || return _reply_inactive(q)
    id, goal_req = _decode_send_goal(server, q)

    # Idempotent re-send: a client retries `send_goal` on discovery lag, so a known
    # goal id is a duplicate. Re-acknowledge it accepted WITHOUT re-running `on_goal`
    # or spawning a second body — re-registering would orphan the result cell an
    # in-flight `get_result` waits on, deadlocking the client's `fetch`.
    if @lock server.lock haskey(server.goals, id)
        _reply_send_goal(server, q, true)
        return nothing
    end

    decision = _with_node_logger(() -> server.on_goal(goal_req), server.node)
    accepted = !(decision isa Reject)

    if accepted
        g, fresh = _register_goal!(server, id, goal_req)
        # `accept` starts execution now; `defer` accepts but leaves the goal ACCEPTED
        # for the owner to `execute` later. Only a freshly-registered goal starts — a
        # request that raced a duplicate reuses the already-running one.
        fresh && !(decision isa Defer) && _start_goal!(server, g)
    end
    _reply_send_goal(server, q, accepted)
    nothing
end

# How long a settled goal's result stays cached for a late `get_result` (wall ns):
# long enough that a client's `fetch` always finds the result, short enough that the
# table stays bounded under a high goal rate. A fixed bound in place of rclcpp's
# per-goal `result_timeout`.
const _RESULT_CACHE_TTL_NS = Int64(15 * 60) * 1_000_000_000   # 15 minutes

# Evict terminal goals settled longer ago than the cache TTL. Eviction keys on the
# TTL alone — a repeated fetch must still replay the cached result — so the `fetched`
# flag only prunes the status array, never deletes from the cache. Live goals
# (settled_at == 0) stay, as does a goal whose task is still running (detached
# post-cancel cleanup) so the drain join can still reach it. Caller holds `server.lock`.
function _evict_terminal_goals!(server::ActionServer)
    now = _now_ns(server.node)
    for (id, g) in server.goals
        settled = @atomic g.settled_at
        settled == 0 && continue                       # still live
        t = g._task
        (t === nothing || istaskdone(t)) || continue   # cleanup still in flight
        now - settled > _RESULT_CACHE_TTL_NS && delete!(server.goals, id)
    end
    nothing
end

# Create + register a GoalHandle with its result cell. The cell's `deliver` closure
# caches the result for `get_result`, publishes the terminal status, and transitions
# the goal — the cell runs it exactly once on the first terminal fill.
function _register_goal!(server::ActionServer{A, G, R, F}, id::GoalId, req::G) where {A, G, R, F}
    # The deliver closure reaches the goal via `g`, so the cell's `.handle` field is
    # typed `Nothing` to break the cell↔handle cycle (the cell is `const` on the
    # handle, so it must exist before `g`).
    local g::GoalHandle{A, G, R, F}
    deliver = (status::SettlementStatus, payload) -> _deliver_result!(server, g, status, payload)
    cell = ResultCell{Nothing, R}(nothing, deliver)
    g = GoalHandle{A, G, R, F}(server, id, req, cell,
                               _now_ns(server.node), ReentrantLock(),
                               GOAL_ACCEPTED, nothing, 0, false, nothing)
    # Idempotent insert: a known id reuses the existing goal rather than orphaning its
    # result cell and spawning a duplicate body. Returns `(goal, fresh)`; `fresh=false`
    # ⇒ a duplicate or racing request already registered it.
    g_out, fresh = @lock server.lock begin
        existing = get(server.goals, id, nothing)
        if existing !== nothing
            (existing, false)
        else
            _evict_terminal_goals!(server)   # reclaim stale cache before inserting
            server.goals[id] = g
            (g, true)
        end
    end
    fresh && _publish_status(server)          # ACCEPTED appears on the status topic
    return (g_out, fresh)
end

# Result-cache + status delivery, invoked once by the cell's first terminal fill,
# under the held cell lock before `filled` flips. Cache the result payload (the cell
# doesn't retain it) and latch the terminal status under `g.lock`, so a concurrent
# `_transition!` can't clobber the terminal write — terminal always wins. Lock order
# is cell.lock → g.lock; `_transition!` takes g.lock alone, so it never inverts.
function _deliver_result!(server::ActionServer{A, G, R, F}, g::GoalHandle{A, G, R, F},
                          status::SettlementStatus, payload) where {A, G, R, F}
    @lock g.lock begin
        # The settled payload is the goal's `Result` on a success/cancel/abort with
        # a real value; a force-aborted goal delivers `nothing` (no defaultable
        # result) — leave `g.result` nothing and let `_reply_result` zero-fill.
        g.result = payload isa R ? payload : nothing
        @atomic g.status = _terminal_state(status)   # terminal latch wins the race
        @atomic g.settled_at = _now_ns(server.node)   # arm the cache TTL clock
    end
    _publish_status(server)
    nothing
end

# ── cancel_goal service ───────────────────────────────────────────────────────
# Decode the cancel request (a goal_info: goal_id + stamp; zero id ⇒ cancel-all),
# run `on_cancel` per matched goal, and on accept move it to CANCELING. This arms
# the `checkpoint`/`feedback!` cancellation throw; execution keeps running until the
# handler observes the token.

function _handle_cancel_goal(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
    isactive(server.cancel_goal_ent) || return _reply_inactive(q)   # managed-node dispatch gate
    target = _decode_cancel(server, q)            # GoalId, or nothing ⇒ cancel-all

    canceling = GoalHandle{A, G, R, F}[]
    @lock server.lock begin
        for g in values(server.goals)
            (target === nothing || g.id == target) || continue
            push!(canceling, g)
        end
    end

    accepted = GoalHandle{A, G, R, F}[]
    for g in canceling
        # Only a live (accepted/executing) goal can be canceled.
        (@atomic g.status) in (GOAL_ACCEPTED, GOAL_EXECUTING) || continue
        _with_node_logger(() -> server.on_cancel(g), server.node) isa Reject && continue
        if _transition!(g, GOAL_CANCELING)
            push!(accepted, g)
        end
    end
    !isempty(accepted) && _publish_status(server)
    _reply_cancel(server, q, accepted)
    nothing
end

# ── get_result service ────────────────────────────────────────────────────────
# The client's `fetch` blocks on a `get_result` request. The reply lands only once
# the goal is terminal, so the queryable handler (one task per request) waits on the
# result cell, then replies with the cached status + result. An unknown goal id is an
# error reply.

function _handle_get_result(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
    isactive(server.get_result_ent) || return _reply_inactive(q)   # managed-node dispatch gate
    id = _decode_get_result(server, q)
    g = @lock server.lock get(server.goals, id, nothing)
    if g === nothing
        reply_err(q, _result_unknown_payload(server))
        return nothing
    end
    # Block this request task until the goal settles, waking the instant the cell
    # fills. The shutdown probe bails the wait so a drain can't hang the request — a
    # goal that settles as we drain still wins (`wait_settled` checks `filled` first).
    if !wait_settled(g.cell, () -> is_shutdown(server.node.context))
        return _reply_inactive(q)
    end
    _reply_result(server, q, g)
    nothing
end

# ── high-level on_accepted: the do-block sugar ──────────────────────────────────
# Spawn the body wrapped in the cancellation + fail-safe settlement machinery, on its
# own task so the send_goal handler replies accepted promptly. Scheduling follows
# `concurrency`: `Serial()` bodies are sticky and serialize on the server's dedicated
# `serial_lock` (one goal at a time, order-of-acceptance preserved); `Parallel(n)`
# bodies run on spawned threads, at most `n` in flight (`Inf` = unbounded).

function _high_level_on_accepted(body::Function, concurrency::Concurrency)
    # One semaphore per server, shared by every goal body, so `Parallel(n)` caps
    # in-flight bodies.
    sem = concurrency isa Parallel && isfinite(concurrency.n) ?
          Base.Semaphore(Int(concurrency.n)) : nothing
    return function (g::GoalHandle)
        run = () -> _run_goal_body(g, body, concurrency, sem)
        # The body is always its own task so `on_accepted` (running on the send_goal
        # request task) returns promptly to reply accepted.
        g._task = _spawn_body(concurrency, run)
        nothing
    end
end

# Body-task placement: sticky for `Serial()` (stay on the node's single
# cooperative thread), spawned for `Parallel`.
function _spawn_body(c::Concurrency, run)
    if c isa Serial
        t = Task(run); t.sticky = true; schedule(t); return t
    else
        return Threads.@spawn run()
    end
end

# Execute one goal body under the settlement wrapper. `Serial()` serializes on the
# dedicated `serial_lock`, leaving the node-wide lock free during a long mission;
# `Parallel(n)` admits at most `n` bodies at once via `sem` (`nothing` = unbounded).
function _run_goal_body(g::GoalHandle{A, G, R, F}, body::Function, concurrency::Concurrency,
                        sem::Union{Base.Semaphore, Nothing} = nothing) where {A, G, R, F}
    _transition!(g, GOAL_EXECUTING)
    server = g.server
    _publish_status(server)
    # Run the goal body under the node logger so a plain `@info` inside it routes
    # to the server node's /rosout.
    runner = () -> settle_handler!(g.cell, () -> _with_node_logger(() -> body(g), server.node);
                                   success_status = succeeded,
                                   default_result = () -> _zero_result(R),
                                   log_id = g.id)
    if concurrency isa Serial
        @lock server.serial_lock runner()
    elseif sem !== nothing
        Base.acquire(runner, sem)
    else
        runner()
    end
    nothing
end

# ── execute(goal): the low-level / deferred run entry ───────────────────────────

"""
    execute(goal) do goal … end
    execute(goal, body)

Run `body(goal)` as this goal's execution, wrapped in the cancellation +
settlement machinery — the low-level and `defer` counterpart of the high-level
`do`-block server. Spawns the body on its own OS-thread task (tracked so
`close(server)` joins it on drain); the task transitions the goal to `EXECUTING`
and publishes the status change before running the body. Returns `nothing`
immediately; the body runs concurrently.

Use it from an `on_accepted` callback that chose to defer, or from an
orchestrator such as [`SingleFlight`](@ref). Each call spawns a fresh task, so
two `execute` calls on different goals run simultaneously — the caller owns any
one-at-a-time policy.

The body's exit maps to a goal terminal status: normal return settles `succeeded`,
a thrown [`Cancelled`](@ref) settles `canceled`, any other throw (or a body that
exits without responding) settles `aborted` and logs. [`respond!`](@ref) owns the
exactly-once settlement that guarantees this mapping fires exactly once.
"""
function execute(body::Function, g::GoalHandle{A, G, R, F}) where {A, G, R, F}
    g._task = Threads.@spawn _run_goal_body(g, body, Parallel(Inf))
    nothing
end
execute(g::GoalHandle, body::Function) = execute(body, g)

# Start an accepted goal via the server's `on_accepted`. Guarded so a throwing
# `on_accepted` can't take down the send_goal handler.
function _start_goal!(server::ActionServer, g::GoalHandle)
    try
        server.on_accepted(g)
    catch err
        @error "on_accepted threw; aborting goal" goal=g.id exception=(err, catch_backtrace())
        # The goal was accepted but never ran — fail-safe settle so the client's
        # get_result doesn't hang.
        isfilled(g.cell) || force_abort!(g.cell)
    end
    nothing
end

# A zero/default-constructed result for the synthesized cancel/abort reply and for
# `_reply_result`'s fallback when a goal settled without a real result. Built
# positionally from each field's exact type so it works regardless of keyword
# defaults; a generated message is always so buildable. Returns `nothing` if even the
# positional build fails (a genuinely un-constructable R) so callers stay fail-safe.
function _zero_result(::Type{R}) where {R}
    try
        return R(map(_zero_result_field, fieldtypes(R))...)
    catch
        return nothing
    end
end

# One field's zero value: numbers→0, strings→"", enums→first instance, fixed
# arrays→zeroed SArray (per-element for non-numeric), sequences→empty Vector,
# nested messages→recurse. Standalone so the action path never depends on the
# warm-up machinery.
function _zero_result_field(::Type{S}) where {S}
    if S <: Number
        return zero(S)
    elseif S <: AbstractString
        return convert(S, "")
    elseif S <: Enum
        return first(instances(S))
    elseif S <: StaticArray
        return eltype(S) <: Number ? zero(S) :
               S(ntuple(_ -> _zero_result_field(eltype(S)), length(S)))
    elseif S <: AbstractVector
        return S(undef, 0)
    else
        return S(map(_zero_result_field, fieldtypes(S))...)   # nested message
    end
end

# ── feedback / status publication ───────────────────────────────────────────────
# Feedback rides the `_action/feedback` topic as a `<A>_FeedbackMessage` (goal_id
# + feedback); status rides `_action/status` as an `action_msgs/GoalStatusArray`.
# Both `put` through the topic Entity's Zenoh publisher with the per-message
# attachment.

const _GoalInfo        = Interfaces.action_msgs.msg.GoalInfo
const _GoalStatus      = Interfaces.action_msgs.msg.GoalStatus
const _GoalStatusArray = Interfaces.action_msgs.msg.GoalStatusArray

# GoalState's Int8 values are the action_msgs/GoalStatus STATUS_* codes verbatim
# (UNKNOWN=0 … ABORTED=6), so the enum reinterprets straight to the wire byte.
_goal_status_byte(s::GoalState) = Int8(s)

function _publish_feedback(server::ActionServer{A, G, R, F}, id::GoalId, fb::F) where {A, G, R, F}
    e = server.feedback_ent
    isopen(e) || return nothing
    isactive(e) || return nothing                 # gated (inactive) managed node ⇒ drop
    route = e._route
    route === nothing && return nothing
    msg = _feedback_message_type(A)(; goal_id = _to_uuid(id), feedback = fb)
    seq = (@atomic server.feedback_seq += 1)
    attach = encode_attachment(seq, _now_ns(server.node), gid(e))
    put(route::ZPublisher, encode(msg); attachment=attach)
    nothing
end

# Publish the current goal-status snapshot as an `action_msgs/GoalStatusArray`, one
# `GoalStatus` per tracked goal. Fetched terminal goals are pruned (the client already
# holds their result). Status also reaches clients via get_result, so a closed or
# un-routed publisher is a quiet no-op.
function _publish_status(server::ActionServer)
    e = server.status_ent
    isopen(e) || return nothing
    isactive(e) || return nothing                 # gated (inactive) managed node ⇒ drop
    route = e._route
    route === nothing && return nothing
    stamp = to_msg(_Time, Dates.now(server.node))
    statuses = @lock server.lock _GoalStatus[
        _GoalStatus(; goal_info = _GoalInfo(; goal_id = _to_uuid(g.id), stamp = stamp),
                    status = _goal_status_byte(@atomic g.status))
        for g in values(server.goals) if !(@atomic g.fetched)]
    msg = _GoalStatusArray(; status_list = statuses)
    seq = (@atomic server.status_seq += 1)
    attach = encode_attachment(seq, _now_ns(server.node), gid(e))
    put(route::ZPublisher, encode(msg); attachment=attach)
    nothing
end

# ── send_goal / cancel / get_result wire framing ────────────────────────────────
# Bridge the Zenoh `Query`/`reply` to the action protocol wrapper messages: the
# per-action `<A>_SendGoal_Request/_Response` + `<A>_GetResult_Request/_Response`
# (generated siblings of `A`) and `action_msgs/CancelGoal_Request/_Response`. Each
# decodes the request payload and replies the encoded wrapper, so the framing is
# byte-exact with rmw_zenoh / cross-vendor peers.

const _CancelGoal_Request  = Interfaces.action_msgs.srv.CancelGoal_Request
const _CancelGoal_Response = Interfaces.action_msgs.srv.CancelGoal_Response

# Decode the request payload as `T` (owned copy; the query buffer is borrowed).
# A request always carries a CDR body, even an empty one, so a missing payload is
# a peer protocol error.
function _decode_query(q::Query, ::Type{T}) where {T}
    pay = payload(q)
    pay === nothing && throw(ArgumentError("action request carried no payload"))
    return decode_owned(Zenoh.as_memory(pay, UInt8), T)
end

# Decode `<A>_SendGoal_Request{goal_id::UUID, goal::G}` → (GoalId, G). The client-minted
# goal_id is the correlation token across the three services + feedback topic.
function _decode_send_goal(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    req = _decode_query(q, _send_goal_request_type(A))
    return (_from_uuid(req.goal_id), req.goal)
end

# Decode `action_msgs/CancelGoal_Request{goal_info{goal_id, stamp}}` → a target
# GoalId, or `nothing` for cancel-all (the all-zero UUID sentinel).
function _decode_cancel(server::ActionServer, q::Query)
    req = _decode_query(q, _CancelGoal_Request)
    id = _from_uuid(req.goal_info.goal_id)
    return _is_zero_uuid(id) ? nothing : id
end

# Decode `<A>_GetResult_Request{goal_id::UUID}` → the GoalId to wait on.
function _decode_get_result(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    req = _decode_query(q, _get_result_request_type(A))
    return _from_uuid(req.goal_id)
end

# Reply `<A>_SendGoal_Response{accepted::Bool, stamp}` — the accept decision +
# the acceptance wall-clock stamp.
function _reply_send_goal(server::ActionServer{A, G, R, F}, q::Query, accepted::Bool) where {A, G, R, F}
    resp = _send_goal_response_type(A)(; accepted = accepted,
                                       stamp = to_msg(_Time, Dates.now(server.node)))
    reply(q, encode(resp))
    nothing
end

# Reply `action_msgs/CancelGoal_Response{return_code::Int8, goals_canceling}`.
# `return_code` 0 (`ERROR_NONE`) when at least one goal moved to CANCELING, else 1
# (`ERROR_REJECTED`); `goals_canceling` lists the accepted goals' GoalInfo.
function _reply_cancel(server::ActionServer, q::Query, accepted::AbstractVector)
    stamp = to_msg(_Time, Dates.now(server.node))
    canceling = _GoalInfo[_GoalInfo(; goal_id = _to_uuid(g.id), stamp = stamp) for g in accepted]
    resp = _CancelGoal_Response(; return_code = Int8(isempty(accepted) ? 1 : 0),
                                goals_canceling = canceling)
    reply(q, encode(resp))
    nothing
end

# Reply `<A>_GetResult_Response{status::Int8, result::R}` — the terminal status byte
# and the cached result (`g.result`). A force-aborted goal has no cached result, so
# zero-fill `R` to keep the reply well-formed and unblock the client. An
# un-constructable `R` (zero-builder returns `nothing`) replies the status as a query
# error rather than throwing inside the queryable — a swallowed throw sends no reply
# and hangs the client; the error still carries the status byte so `fetch` unblocks.
# Marking the goal fetched lets `_publish_status` prune it and eviction reclaim it.
function _reply_result(server::ActionServer{A, G, R, F}, q::Query, g::GoalHandle{A, G, R, F}) where {A, G, R, F}
    st = _terminal_state(outcome(g.cell)::SettlementStatus)
    result = g.result === nothing ? _zero_result(R) : g.result
    if result === nothing
        reply_err(q, string("action result unavailable (status ", _goal_status_byte(st), ")"))
    else
        resp = _get_result_response_type(A)(; status = _goal_status_byte(st), result = result)
        reply(q, encode(resp))
    end
    @atomic g.fetched = true
    nothing
end

_reply_inactive(q::Query) = (reply_err(q, "action server inactive"); nothing)
_result_unknown_payload(::ActionServer) = "unknown goal id"

# ── small helpers ──────────────────────────────────────────────────────────────

_now_ns(node) = nanoseconds(Dates.now(node, System()))

