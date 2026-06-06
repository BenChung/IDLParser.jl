# ── ActionServer (§9) ──────────────────────────────────────────────────────────
# The long-lived server: three Zenoh queryables (send_goal / cancel_goal /
# get_result), a feedback publisher, a status publisher, a goal table (the result
# cache + live goals), and the user callbacks. Each sub-endpoint is a generic
# `Entity` (node.jl) so it gets a liveliness token + graph injection + close-on-
# node-close for free; the queryable/publisher routes hang off `entity.wire`.

"""
    ActionServer{A, G, R, F}

A ROS 2 action server for action type `A` (§9). Owns the three protocol services
(`send_goal`/`cancel_goal`/`get_result`), the `feedback` + `status` topics, the
goal table (live goals + the result cache), and the user callbacks
(`on_goal`/`on_cancel`/`on_accepted`). `close`-able; dies with its node, joining
outstanding goal tasks (incl. detached post-cancel cleanup) up to the Context
drain timeout (§14).

The goal/feedback/result lifecycle and cancellation model follow the upstream ROS 2
action concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html

Construct via [`ActionServer`](@ref)'s constructor — the low-level three-callback
form or the high-level `do`-block sugar.
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
    # Goal table: id → GoalHandle. Holds live + settled goals (the result cache).
    # Terminal goals are evicted once fetched or past the cache TTL
    # (`_evict_terminal_goals!`, swept on each `_register_goal!`) so the table — and
    # the per-transition `_publish_status` cost that scales with it — stays bounded.
    const goals::Dict{GoalId, GoalHandle{A, G, R, F}}
    const lock::ReentrantLock
    # Serializes goal-body execution under `concurrency = Serial()` (one goal at a
    # time, §4). Dedicated to goal bodies so the node-wide lock (guarding cross-
    # entity state) stays free while a long mission runs.
    const serial_lock::ReentrantLock
    @atomic open::Bool
end

"""
    ActionServer(node, name, A; on_goal=…, on_cancel=…, on_accepted, qos=default_qos())
    ActionServer(node, name, A; concurrency=Serial(), on_goal=…, on_cancel=…) do goal … end

Declare an action server for action type `A` on `name` (§9). Two forms share one
constructor:

- **Low-level** — supply `on_accepted` (and optionally `on_goal`/`on_cancel`);
  you own when/whether/where each accepted goal runs (the `GoalHandle` is yours).
- **High-level** (`do`-block) — the body *is* the execution; the framework
  spawns it wrapped in the `Cancelled` + fail-safe settlement machinery, one at a
  time (`concurrency = Serial()`, default) or up to `n` concurrent
  (`Parallel(n)`, §4).

`on_goal(request)` returns [`accept`](@ref)/[`reject`](@ref)/[`defer`](@ref)
(default accept-all); `on_cancel(goal)` returns `accept`/`reject` (default
accept). The five sub-endpoints are declared on `node` and reaped with it.
"""
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

    # Resolve the execution callback: the high-level `do`-body wins, wrapped into
    # an `on_accepted` that schedules per `concurrency`; otherwise the low-level
    # `on_accepted` (required if no body).
    accepted = if body !== nothing
        on_accepted === nothing ||
            throw(ArgumentError("ActionServer: pass either a `do`-block body or \
                                 `on_accepted=`, not both"))
        _high_level_on_accepted(body, concurrency)
    elseif on_accepted !== nothing
        on_accepted
    else
        throw(ArgumentError("ActionServer requires execution: a `do goal … end` \
                             body or an `on_accepted=` callback (§9)"))
    end

    # Five sub-endpoints. Each of the three services keys off its own SERVICE-level
    # type (`pkg::action::dds_::<A>_SendGoal_`/`_GetResult_`, `action_msgs::srv::dds_::
    # CancelGoal_`) + the service RIHS01, matching rmw_zenoh — the action type's own
    # info would never match a peer. Falls back to the action-typed info when the
    # service hashes can't be synthesized (Goal/Result not registered with their TDs).
    stis = _action_service_tis(A, support)
    sg_ti = stis === nothing ? type_info(A) : stis.send_goal
    cg_ti = stis === nothing ? type_info(A) : stis.cancel_goal
    gr_ti = stis === nothing ? type_info(A) : stis.get_result
    send_goal_ent   = make_entity(node, Service,      _send_goal_topic(fqn),   sg_ti; qos=qos)
    cancel_goal_ent = make_entity(node, Service,      _cancel_goal_topic(fqn), cg_ti; qos=qos)
    get_result_ent  = make_entity(node, Service,      _get_result_topic(fqn),  gr_ti; qos=qos)
    # Feedback/status carry their OWN wire types (FeedbackMessage / GoalStatusArray),
    # not the action type — the data keyexpr embeds type+hash, so a publisher must match
    # what its subscriber declares (the §11 per-section refinement, applied here for the
    # two topics that have live subscribers; the services stay action-typed for now).
    feedback_ent    = make_entity(node, Publisher,    _feedback_topic(fqn),
                                  type_info_of(_feedback_message_type(A)); qos=qos)
    status_ent      = make_entity(node, Publisher,    _status_topic(fqn),
                                  type_info_of(_GoalStatusArray); qos=qos)

    server = ActionServer{A, G, R, F}(node, fqn, support, on_goal, on_cancel, accepted,
                                      send_goal_ent, cancel_goal_ent, get_result_ent,
                                      feedback_ent, status_ent,
                                      Dict{GoalId, GoalHandle{A, G, R, F}}(),
                                      ReentrantLock(), ReentrantLock(), true)

    _wire_action_server!(server)

    # Precompile goal-decode → per-goal execution callable → result/feedback encode.
    # Precompile-only: a fabricated long-running goal body over a live GoalHandle
    # isn't safe to run at warm-up. `exec` is the do-block body (high-level) or the
    # `on_accepted` callback (low-level).
    pol = _resolve_warmup(node, warmup, warmup_sync)
    exec = body !== nothing ? body : accepted
    _warmup!(pol, () -> _warm_action(G, R, F, exec, GoalHandle{A, G, R, F}))

    return server
end

_default_on_goal(_request)  = accept()
_default_on_cancel(_goal)   = accept()

# Declare the data routes: a Queryable per service, a Zenoh.Publisher per topic.
# Each queryable's callback runs on its own Julia task (Zenoh trampoline, §2.3),
# so a blocking accept decision never stalls the I/O thread.
function _wire_action_server!(server::ActionServer)
    sess = server.node.context.session

    # Feedback + status publishers (the data-route declaration node.jl provides).
    declare_publisher!(server.feedback_ent)
    declare_publisher!(server.status_ent)

    _declare_queryable!(server.send_goal_ent,   q -> _handle_send_goal(server, q))
    _declare_queryable!(server.cancel_goal_ent, q -> _handle_cancel_goal(server, q))
    _declare_queryable!(server.get_result_ent,  q -> _handle_get_result(server, q))
    server
end

# Declare a callback-form Queryable on a Service `Entity` and stash it in `.wire`
# so it shares the entity's close lifecycle (node.jl closes `entity.wire`). The
# topic keyexpr is the service's data route (`topic_keyexpr`, §2.2). A handler
# throw is logged, never fatal — one bad request can't kill the queryable.
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

"The underlying generic [`Entity`](@ref)s (one per sub-endpoint, §6)."
entities(server::ActionServer) = (server.send_goal_ent, server.cancel_goal_ent,
                                  server.get_result_ent, server.feedback_ent,
                                  server.status_ent)

"""
    close(server::ActionServer)

Undeclare the action server (§9/§14): close its five sub-endpoints (each
withdraws its liveliness token + Zenoh route), after awaiting outstanding goal
tasks so a settle-then-detached-cleanup goal (DESIGN §drain) finishes rather than
being dropped mid-flight. Idempotent.
"""
function Base.close(server::ActionServer)
    (@atomicswap server.open = false) || return nothing
    # Await outstanding goal tasks (incl. detached post-cancel cleanup) up to the
    # Context drain timeout, so `close(server)` mid-"descend and disarm" doesn't
    # drop the vehicle (DESIGN §drain).
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

# Snapshot live goal tasks and join them with the Context drain bound. A goal that
# overruns is left running detached (we don't hard-kill cleanup); the timeout just
# stops us waiting forever.
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

# ── send_goal service (§9) ─────────────────────────────────────────────────────
# Decode the goal request (goal_id + goal), run `on_goal`, and reply
# accepted/rejected. On accept (not defer) we fire `on_accepted` so execution
# starts. The reply is the `SendGoal_Response` (accepted::Bool + stamp).

function _handle_send_goal(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
    id, goal_req = _decode_send_goal(server, q)

    # Idempotent re-send: a client retries `send_goal` when its reply is lost/slow, so a
    # goal id we already know is a duplicate. Re-acknowledge it (accepted) WITHOUT running
    # `on_goal` again or spawning a second execution — re-registering would orphan the
    # result cell an in-flight `get_result` waits on, deadlocking the client's `fetch`.
    if @lock server.lock haskey(server.goals, id)
        _reply_send_goal(server, q, true)
        return nothing
    end

    decision = _with_node_logger(() -> server.on_goal(goal_req), server.node)
    accepted = !(decision isa Reject)

    if accepted
        g, fresh = _register_goal!(server, id, goal_req)
        # `defer` accepts but leaves the goal ACCEPTED for the owner to `execute` later
        # (the queue path); `accept` starts now. Only a freshly-registered goal starts —
        # a request that raced a duplicate reuses the already-running one.
        fresh && !(decision isa Defer) && _start_goal!(server, g)
    end
    _reply_send_goal(server, q, accepted)
    nothing
end

# How long a settled goal's result stays cached for a late `get_result` before it
# is evicted (wall ns). rclcpp keys this off the goal's `result_timeout`; we use a
# fixed bound — long enough that a client's `fetch` always finds the result, short
# enough that the table can't grow without limit under a high goal rate.
const _RESULT_CACHE_TTL_NS = Int64(15 * 60) * 1_000_000_000   # 15 minutes

# Evict terminal goals whose result is no longer recoverable: settled longer ago
# than the cache TTL. Eviction keys on the TTL alone — a repeated fetch must still
# replay the cached result — so the `fetched` flag only prunes the status array.
# Live goals (settled_at == 0) stay, as does a goal whose task is still running (a
# detached post-cancel cleanup) so the drain join (`_await_goals`) can still reach it.
# Caller holds `server.lock`. Bounds the table size and `_publish_status`'s
# table-scaled cost, matching rclcpp.
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

# Create + register a GoalHandle with its result cell. The cell's `deliver`
# closure caches the result for `get_result`, publishes the terminal status, and
# transitions the goal — settlement.jl runs it exactly once on the first terminal
# fill.
function _register_goal!(server::ActionServer{A, G, R, F}, id::GoalId, req::G) where {A, G, R, F}
    # The deliver closure reaches the goal via `g`, so the cell's own `.handle`
    # field is unused here — type it `Nothing` to break the cell↔handle cycle
    # (the cell is `const` on the handle, so it must exist before `g`).
    local g::GoalHandle{A, G, R, F}
    deliver = (status::SettlementStatus, payload) -> _deliver_result!(server, g, status, payload)
    cell = ResultCell{Nothing, R}(nothing, deliver)
    g = GoalHandle{A, G, R, F}(server, id, req, cell,
                               _now_ns(server.node), ReentrantLock(),
                               GOAL_ACCEPTED, nothing, 0, false, nothing)
    # Idempotent insert: a client retries `send_goal` when its reply is lost or lags
    # discovery, so the same id can arrive more than once. A known id reuses the
    # existing goal; re-registering would orphan its result cell (the one a held
    # `get_result` blocks on) and spawn a duplicate body, deadlocking `fetch`. Returns
    # `(goal, fresh)`; `fresh=false` ⇒ a duplicate/racing request already registered it.
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

# The result-cache + status delivery, invoked once by the cell's first terminal
# fill (settlement.jl, under the cell lock, *before* `filled` flips). Cache the
# result payload (the cell doesn't retain it) and latch the terminal status, both
# under `g.lock` so the terminal write can't be clobbered by a concurrent
# `_transition!` (the only other `g.status` writer) — terminal always wins. A
# waiting `get_result` replays the cached result; a late fetch finds it in
# `g.result`. Lock order is cell.lock → g.lock (we run under the held cell lock);
# `_transition!` takes g.lock alone, so the order never inverts.
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

# ── cancel_goal service (§9) ───────────────────────────────────────────────────
# Decode the cancel request (a goal_info: goal_id + stamp; zero id ⇒ cancel-all),
# run `on_cancel` per matched goal, and on accept move it to CANCELING. This arms
# the `checkpoint`/`feedback!` cancellation throw; execution keeps running until the
# handler observes the token (§9).

function _handle_cancel_goal(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
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

# ── get_result service (§9) ────────────────────────────────────────────────────
# The client's `fetch` blocks on a `get_result` request. The reply lands only once
# the goal is terminal, so the queryable handler waits on the result cell
# (a task per request, the Zenoh trampoline gives us that, §2.3) then replies with
# the cached status + result. An unknown goal id is an error reply.

function _handle_get_result(server::ActionServer{A, G, R, F}, q::Query) where {A, G, R, F}
    isopen(server) || return _reply_inactive(q)
    id = _decode_get_result(server, q)
    g = @lock server.lock get(server.goals, id, nothing)
    if g === nothing
        reply_err(q, _result_unknown_payload(server))
        return nothing
    end
    # Block this request task until the goal settles, waking the instant the cell
    # fills (settlement.jl notifies on `respond!`/`fill!`/`force_abort!`). Bail on
    # shutdown so a drain can't hang the request — a goal that settles as we drain
    # still wins (wait_settled checks `filled` first).
    if !wait_settled(g.cell, () -> is_shutdown(server.node.context))
        return _reply_inactive(q)
    end
    _reply_result(server, q, g)
    nothing
end

# ── high-level on_accepted: the do-block sugar (§9) ─────────────────────────────
# Spawn the body wrapped in `settle_handler!` (settlement.jl): a normal return →
# SUCCEEDED, a thrown `Cancelled` → CANCELED, anything else → ABORTED + log, and a
# never-responded exit → ABORTED. Scheduling is `concurrency` (§4): each body runs
# on its own task (so the send_goal handler can reply accepted promptly); `Serial()`
# bodies are sticky and serialize on the server's dedicated `serial_lock` (one goal
# at a time, order-of-acceptance preserved), `Parallel` bodies run free.

function _high_level_on_accepted(body::Function, concurrency::Concurrency)
    return function (g::GoalHandle)
        run = () -> _run_goal_body(g, body, concurrency)
        # The body is always its own task so `on_accepted` (which runs on the
        # send_goal request task) returns promptly to reply accepted. Under
        # `Serial()` the task is sticky, staying on the node's cooperative thread
        # like the single-threaded default; `Parallel` bodies run on spawned threads.
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

# Execute one goal body under the settlement wrapper. `Serial()` serializes bodies
# on the server's dedicated `serial_lock` (§4 one-at-a-time), leaving the node-wide
# lock free during a long mission; `Parallel` runs free.
function _run_goal_body(g::GoalHandle{A, G, R, F}, body::Function, concurrency::Concurrency) where {A, G, R, F}
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
    else
        runner()
    end
    nothing
end

# ── execute(goal): the low-level / deferred run entry (§9) ──────────────────────

"""
    execute(goal) do goal … end
    execute(goal, body)

Run `body(goal)` as this goal's execution, wrapped in the `Cancelled` + fail-safe
settlement machinery (§9) — the low-level/`defer` counterpart of the high-level
`do`-block. Spawns the body on its own task (tracked for the server drain) and
transitions the goal to `EXECUTING`. Use it from an `on_accepted` that deferred,
or from an orchestrator.
"""
function execute(body::Function, g::GoalHandle{A, G, R, F}) where {A, G, R, F}
    g._task = Threads.@spawn _run_goal_body(g, body, Parallel(Inf))
    nothing
end
execute(g::GoalHandle, body::Function) = execute(body, g)

# Start an accepted goal via the server's `on_accepted` (the low-level path runs
# user scheduling; the high-level path spawns the wrapped body). Guarded so a
# throwing `on_accepted` can't take down the send_goal handler.
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

# A zero/default-constructed result for the synthesized cancel/abort reply
# (settlement.jl's `default_result` thunk) and for `_reply_result`'s fallback when
# a goal settled without a real result (force_abort! / non-defaultable type).
# Built positionally from each field's exact type so it works whether or not the
# struct has keyword defaults; a generated message is always so buildable. Returns
# `nothing` if even the positional build fails (a genuinely un-constructable R), so
# the callers stay fail-safe rather than throwing — `force_abort!` and
# `_reply_result`'s error reply are the backstops.
function _zero_result(::Type{R}) where {R}
    try
        return R(map(_zero_result_field, fieldtypes(R))...)
    catch
        return nothing
    end
end

# One field's zero value: numbers→0, strings→"", enums→first instance, fixed
# arrays→zeroed SArray (per-element for non-numeric), sequences→empty Vector,
# nested messages→recurse. Mirrors warmup's `_default_field` but stands alone so
# the action path never depends on the warm-up machinery.
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

# ── feedback / status publication (§9) ──────────────────────────────────────────
# Feedback rides the `_action/feedback` topic as a `<A>_FeedbackMessage` (goal_id
# + feedback); status rides `_action/status` as an `action_msgs/GoalStatusArray`.
# Both `put` through the topic Entity's Zenoh publisher with the per-message
# attachment (§3.4).

const _GoalInfo        = Interfaces.action_msgs.msg.GoalInfo
const _GoalStatus      = Interfaces.action_msgs.msg.GoalStatus
const _GoalStatusArray = Interfaces.action_msgs.msg.GoalStatusArray

# GoalState's Int8 values are the action_msgs/GoalStatus STATUS_* codes verbatim
# (UNKNOWN=0 … ABORTED=6), so the enum reinterprets straight to the wire byte.
_goal_status_byte(s::GoalState) = Int8(s)

function _publish_feedback(server::ActionServer{A, G, R, F}, id::GoalId, fb::F) where {A, G, R, F}
    e = server.feedback_ent
    isopen(e) || return nothing
    route = e._route
    route === nothing && return nothing
    msg = _feedback_message_type(A)(; goal_id = _to_uuid(id), feedback = fb)
    attach = encode_attachment(0, _now_ns(server.node), gid(e))
    put(route::ZPublisher, encode(msg); attachment=attach)
    nothing
end

# Publish the current goal-status snapshot as an `action_msgs/GoalStatusArray`
# (one `GoalStatus{goal_info{goal_id, stamp}, status}` per tracked goal). Already-
# fetched terminal goals are excluded — the client has their result and rclcpp
# prunes them, so re-publishing them every transition is pure cost. Status also
# reaches clients via get_result, so a closed/un-routed publisher is a quiet no-op
# rather than an error.
function _publish_status(server::ActionServer)
    e = server.status_ent
    isopen(e) || return nothing
    route = e._route
    route === nothing && return nothing
    stamp = to_msg(_Time, Dates.now(server.node))
    statuses = @lock server.lock _GoalStatus[
        _GoalStatus(; goal_info = _GoalInfo(; goal_id = _to_uuid(g.id), stamp = stamp),
                    status = _goal_status_byte(@atomic g.status))
        for g in values(server.goals) if !(@atomic g.fetched)]
    msg = _GoalStatusArray(; status_list = statuses)
    attach = encode_attachment(0, _now_ns(server.node), gid(e))
    put(route::ZPublisher, encode(msg); attachment=attach)
    nothing
end

# ── send_goal / cancel / get_result wire framing (§9) ───────────────────────────
# Bridge the Zenoh `Query`/`reply` to the action protocol wrapper messages: the
# per-action `<A>_SendGoal_Request/_Response` + `<A>_GetResult_Request/_Response`
# (generated siblings of `A`) and `action_msgs/CancelGoal_Request/_Response`. Each
# decodes the request payload (mirroring service.jl) and replies the encoded
# wrapper, so the framing is byte-exact with rmw_zenoh / cross-vendor peers.

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

# Decode `<A>_SendGoal_Request{goal_id::UUID, goal::G}` → (GoalId, G). The client
# mints the goal_id, so we adopt it (the correlation token across the three
# services + feedback topic).
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

# Reply `<A>_GetResult_Response{status::Int8, result::R}` — the terminal status
# byte and the cached result (`g.result`, captured by `_deliver_result!`). A
# force-aborted goal has no cached result; zero-fill `R` so the reply is
# well-formed and the client's `fetch` unblocks with the status. If `R` is
# genuinely un-constructable (zero-builder returns `nothing`) we honor the
# force_abort! contract by replying the status as a query error instead of throwing
# inside the queryable (a throw is swallowed → no reply → the client hangs); the
# error still carries the status byte so `fetch` unblocks. Marking the goal fetched
# lets `_publish_status` prune it and eviction reclaim it.
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

