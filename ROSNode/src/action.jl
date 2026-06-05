# §9 Actions. Two layers over the ROS2 action wire protocol — three services
# (`send_goal`, `cancel_goal`, `get_result`) plus two topics (`feedback`,
# `status`), all derived from the action type's sub-messages:
#
#   <action>/_action/send_goal     (service)  goal_id + goal   → accepted + stamp
#   <action>/_action/cancel_goal   (service)  goal_info        → return_code + …
#   <action>/_action/get_result    (service)  goal_id          → status + result
#   <action>/_action/feedback      (topic)    goal_id + feedback
#   <action>/_action/status        (topic)    GoalStatusArray
#
# The framework owns the goal state machine, the result cache (so a late
# `get_result` is answered), status publication, and the fail-safe settlement
# backstop (settlement.jl); *you* own execution & scheduling. The high-level
# `do`-block is sugar — an `on_accepted` that spawns the body wrapped in the
# `Cancelled` + settlement machinery, scheduled by `concurrency` (§4).
#
# The settlement core (write-once `ResultCell`, `respond!`/`fill!`/`force_abort!`,
# `settle_handler!`) is shared with services verbatim — a `GoalHandle` *is* the
# cell's handle. The status tokens (`succeeded`/`canceled`/`aborted`/`feedback`)
# come from core.jl.

using Zenoh: Zenoh, Keyexpr, Query, Queryable, Querier, reply, reply_err,
             Publisher as ZPublisher, put, payload, attachment
import Zenoh
using ROSZenoh: ROSZenoh, Service, EndpointKind, default_qos, topic_keyexpr
using Dates: Dates
using StaticArrays: StaticArray   # fixed arrays (`T[N]`) generate as SArray; the
                                  # result zero-builder defaults them per-element

# `fetch` extends `Base.fetch` (await a result) — not re-exported, to avoid the
# `using ROSNode` + `Base.fetch` clash; reached as `fetch(goal)` since we add the
# method to Base. Likewise `pause` (Base) is a `SingleFlight` method, not exported.
export ActionServer, ActionClient, GoalHandle,
       feedback!, checkpoint, iscancelled, succeed, abort, execute,
       send, cancel, state, action_server_matched,
       accept, reject, defer,
       SingleFlight

# ── action type support (§9/§11) ────────────────────────────────────────────
# The ROS2 action protocol is built from sub-messages of the action definition:
# the user-facing `Goal`/`Result`/`Feedback`, plus the protocol wrappers
# (`SendGoal_Request/_Response`, `GetResult_Request/_Response`, the
# `FeedbackMessage` goal_id+feedback pair, `action_msgs/CancelGoal` and
# `GoalStatusArray`). `@ros_msg` on a `.action` today generates only the three
# bare sub-types (`<A>_Goal`/`_Result`/`_Feedback`, §ROSMessages); the protocol
# wrappers + `action_msgs`/`unique_identifier_msgs` types are a typesupport gap.
#
# `ActionTypeSupport` carries the three sub-types so the server/client speak in
# them and centralizes the topic-suffix naming. The protocol wrappers
# (`<A>_SendGoal_Request/_Response`, `<A>_GetResult_Request/_Response`,
# `<A>_FeedbackMessage`) are generated as siblings of `A` (§ROSMessages
# `action_protocol_decls`) and resolved reflectively off `A` (`_action_wrapper`),
# so the wire framing is built through real ctors rather than byte seams.

"""
    ActionTypeSupport{A, G, R, F}

Resolves the sub-message types of an action type `A` into its `Goal` (`G`),
`Result` (`R`), and `Feedback` (`F`) structs (§9). Built from the action type
argument to `ActionServer`/`ActionClient`; the three types drive the data-route
key expressions and the handler signatures. The protocol wrappers
(`SendGoal`/`GetResult` request/response, `FeedbackMessage`) are resolved off `A`
on demand via [`_action_wrapper`](@ref) — they're generated siblings of `A`.
"""
struct ActionTypeSupport{A, G, R, F} end

goal_type(::ActionTypeSupport{A, G, R, F})     where {A, G, R, F} = G
result_type(::ActionTypeSupport{A, G, R, F})   where {A, G, R, F} = R
feedback_type(::ActionTypeSupport{A, G, R, F}) where {A, G, R, F} = F
action_type(::ActionTypeSupport{A})            where {A}          = A

# The five protocol-wrapper structs rosidl derives from a `.action` are emitted
# as siblings of `A` in its `.action` module (`<A>_SendGoal_Request`, etc. —
# §ROSMessages `action_protocol_decls`). Resolve them by suffix off `A`'s name,
# the same reflection `ActionTypeSupport` does for the `_Goal`/`_Result`/`_Feedback`
# triple; a missing wrapper means the action was generated without the protocol
# set (an `@ros_msg` on the bare `.action` text, not `@ros_msgs`).
function _action_wrapper(::Type{A}, suffix::AbstractString) where {A}
    m = parentmodule(A)
    s = Symbol(nameof(A), suffix)
    isdefined(m, s) ||
        throw(ArgumentError("$(A): missing protocol wrapper `$(s)` in $(m) — \
                             generate the action with `@ros_msgs` so the \
                             SendGoal/GetResult/FeedbackMessage wrappers exist"))
    return getfield(m, s)::Type
end

_send_goal_request_type(::Type{A})  where {A} = _action_wrapper(A, "_SendGoal_Request")
_send_goal_response_type(::Type{A}) where {A} = _action_wrapper(A, "_SendGoal_Response")
_get_result_request_type(::Type{A}) where {A} = _action_wrapper(A, "_GetResult_Request")
_get_result_response_type(::Type{A})where {A} = _action_wrapper(A, "_GetResult_Response")
_feedback_message_type(::Type{A})   where {A} = _action_wrapper(A, "_FeedbackMessage")

# Reflectively resolve `<A>_Goal`/`_Result`/`_Feedback` from `A`'s defining
# module (the `<pkg>.action` submodule, §ROSMessages codegen). `A` is named by
# its bare action name; the triple lives beside it. A missing sub-type is the
# "this isn't an action type" mistake, named rather than left as an `UndefVar`.
function ActionTypeSupport(::Type{A}) where {A}
    m = parentmodule(A)
    base = string(nameof(A))
    sub(suffix) = begin
        s = Symbol(base, suffix)
        isdefined(m, s) ||
            throw(ArgumentError("$(A) does not look like an action type: \
                                 expected a sibling `$(s)` in $(m) \
                                 (a generated `.action` produces `<Name>_Goal`/\
                                 `_Result`/`_Feedback`)"))
        getfield(m, s)
    end
    ActionTypeSupport{A, sub("_Goal"), sub("_Result"), sub("_Feedback")}()
end

# The five action sub-endpoint key suffixes, under rmw_zenoh's `<topic>/_action/*`
# convention. The action `name` is the resolved FQN; ROSZenoh's keyexpr builder
# then maps each `(suffix-topic, kind)` to the wire key.
_send_goal_topic(name)   = string(name, "/_action/send_goal")
_cancel_goal_topic(name) = string(name, "/_action/cancel_goal")
_get_result_topic(name)  = string(name, "/_action/get_result")

# rmw_zenoh action-protocol service keyexpr identity (§9). The three services key
# off SERVICE-level type hashes computed from the FIXED action-protocol type
# descriptions (ROSMessages.{send_goal,get_result,cancel_goal}_service_rihs01) — the
# same form a real ROS2 peer keys them on. send_goal/get_result depend on the
# action's Goal/Result type descriptions; cancel_goal is the shared, constant
# `action_msgs/srv/CancelGoal` (its keyexpr name is `srv`, though the hash is over
# the `action`-path names — see ROSMessages). Returns a NamedTuple of the three
# `TypeInfo`s, or `nothing` (→ caller falls back to the action-typed info) when the
# Goal/Result types aren't registered with their wire descriptions.
function _action_service_tis(::Type{A}, support::ActionTypeSupport) where {A}
    ge = _entry_of(goal_type(support))
    re = _entry_of(result_type(support))
    (ge === nothing || re === nothing || ge.td === nothing || re.td === nothing) &&
        return nothing
    goal_main = ge.td.type_description
    pkg, _, gbare = split_ros_name(goal_main.type_name)   # ("pkg", "action", "Name_Goal")
    aname = endswith(gbare, "_Goal") ? gbare[1:end-length("_Goal")] : gbare
    hsg = type_hash_from_rihs_string(ROSMessages.send_goal_service_rihs01(pkg, aname, goal_main))
    hgr = type_hash_from_rihs_string(ROSMessages.get_result_service_rihs01(pkg, aname, re.td.type_description))
    hcg = type_hash_from_rihs_string(ROSMessages.cancel_goal_service_rihs01())
    (hsg === nothing || hgr === nothing || hcg === nothing) && return nothing
    return (send_goal   = TypeInfo(string(pkg, "/action/", aname, "_SendGoal"), hsg),
            get_result  = TypeInfo(string(pkg, "/action/", aname, "_GetResult"), hgr),
            cancel_goal = TypeInfo("action_msgs/srv/CancelGoal", hcg))
end
_feedback_topic(name)    = string(name, "/_action/feedback")
_status_topic(name)      = string(name, "/_action/status")

# ── goal id ──────────────────────────────────────────────────────────────────
# A goal is identified by a 16-byte `unique_identifier_msgs/UUID`. We mint a
# random one server-side acceptance / client-side send; the bytes are the cache
# key and the correlation token across the three services + feedback topic.

const GoalId = NTuple{16, UInt8}

_new_goal_id()::GoalId = ntuple(_ -> rand(UInt8), 16)

# The wire `unique_identifier_msgs/UUID` is a `@cdr_fixed` struct with a single
# `uuid::SVector{16,UInt8}` field; its kw-ctor accepts a plain NTuple (it
# `convert`s). Reading back, `.uuid` indexes 1-based. These are the only two
# crossings between the internal `GoalId` tuple and the wire UUID.
const _UUID = Interfaces.unique_identifier_msgs.msg.UUID
const _Time = Interfaces.builtin_interfaces.msg.Time

_to_uuid(id::GoalId) = _UUID(; uuid = id)
_from_uuid(u)::GoalId = ntuple(i -> u.uuid[i], 16)

# A zeroed UUID (16 NUL bytes) is the action_msgs "cancel-all" sentinel.
_is_zero_uuid(id::GoalId) = all(==(0x00), id)

# ── goal state machine (§9) ───────────────────────────────────────────────────
# rclcpp's status enum (action_msgs/GoalStatus): the values are wire-stable —
# they ride the `status` topic and the `get_result` reply. Modeled as a sealed
# `@enum` so the state transitions are exhaustive and an unknown code is caught.

"""
    GoalState

The ROS2 goal lifecycle (`action_msgs/msg/GoalStatus`): `ACCEPTED` → `EXECUTING`
→ {`SUCCEEDED`, `CANCELED`, `ABORTED`}, with `CANCELING` between executing and
canceled. Integer values match the wire enum so they publish directly to the
`status` topic. `:unknown`/`:rejected` are local-only (never published as a live
goal).
"""
@enum GoalState::Int8 begin
    GOAL_UNKNOWN   = 0
    GOAL_ACCEPTED  = 1
    GOAL_EXECUTING = 2
    GOAL_CANCELING = 3
    GOAL_SUCCEEDED = 4
    GOAL_CANCELED  = 5
    GOAL_ABORTED   = 6
end

# Public state symbols (the client/server `state(goal)` surface, §9). Kept as
# Symbols at the API edge — they read naturally (`state(goal) === :executing`)
# and don't leak the wire enum.
_state_symbol(s::GoalState) =
    s === GOAL_ACCEPTED  ? :accepted  :
    s === GOAL_EXECUTING ? :executing :
    s === GOAL_CANCELING ? :canceling :
    s === GOAL_SUCCEEDED ? :succeeded :
    s === GOAL_CANCELED  ? :canceled  :
    s === GOAL_ABORTED   ? :aborted   : :unknown

# Map a terminal settlement token (core.jl) onto its goal status. `feedback` is
# not terminal and never reaches here.
_terminal_state(::Succeeded) = GOAL_SUCCEEDED
_terminal_state(::Canceled)  = GOAL_CANCELED
_terminal_state(::Aborted)   = GOAL_ABORTED

# Whether a goal status is terminal (a result is cached, no further transitions).
_is_terminal_state(s::GoalState) =
    s === GOAL_SUCCEEDED || s === GOAL_CANCELED || s === GOAL_ABORTED

# ── on_goal decisions (§9) ─────────────────────────────────────────────────────
# `on_goal` returns one of three tokens. `accept` starts execution immediately
# (via `on_accepted`); `defer` accepts but leaves the goal in ACCEPTED for the
# owner to `execute` later (the queue/orchestrator path); `reject` declines.

"`on_goal` decision tokens — `accept()`/`reject()`/`defer()` (§9)."
abstract type GoalResponse end
struct Accept <: GoalResponse end
struct Reject <: GoalResponse end
struct Defer  <: GoalResponse end

"Accept a goal and begin executing it now (fires `on_accepted`)."
accept() = Accept()
"Reject a goal — the client's `send` returns a rejected handle."
reject() = Reject()
"Accept a goal but defer execution — stays `ACCEPTED` until `execute(goal)` (the queue path, §9)."
defer()  = Defer()

# `on_cancel` returns accept/reject — reuse the same tokens (accept arms the
# cancellation token and moves to CANCELING; reject leaves the goal running).

# ── GoalHandle (§9) ────────────────────────────────────────────────────────────
# The per-goal object: the thing you `feedback!`, settle, and observe
# cancellation on. Holds the goal id, the goal request, the live state, the
# write-once result cell (settlement.jl), and a back-reference to the server for
# feedback publication + status updates. Two lifetimes: the server is long-lived;
# a GoalHandle lives from acceptance until its result is fetched (or the cache
# evicts it).

"""
    GoalHandle{A, G, R, F}

A single accepted action goal (§9). Carries the 16-byte goal id, the decoded
goal request (`G`), the live [`GoalState`](@ref), and the write-once
[`ResultCell`](@ref) the handler settles. Cancellation is *structured*:
[`feedback!`](@ref)/[`checkpoint`](@ref) throw [`Cancelled`](@ref) once the goal
is `CANCELING`, so the classic "stuck in CANCELING" bug can't happen on the
default path.

Settle it with [`succeed`](@ref)/[`abort`](@ref)/`canceled` via
[`respond!`](@ref), or just `return`/throw from the handler (fail-safe
settlement, §8/§9). The server owns the result cache and status publication; the
handle is the user-facing verb surface.
"""
mutable struct GoalHandle{A, G, R, F}
    const server::Any                    # back-ref to the ActionServer (duck-typed)
    const id::GoalId
    const request::G
    const cell::ResultCell               # ResultCell{GoalHandle, R}; concrete elided
    const accepted_at::Int64             # acceptance stamp (wall ns, §3.4/§7)
    const lock::ReentrantLock
    @atomic status::GoalState
    # The settled result, cached for `get_result` replay (a late or repeated
    # fetch is answered without re-running the goal). The cell delivers the
    # payload through `deliver` and does *not* retain it, so we capture it here on
    # the first terminal fill. `nothing` until settled; written under `g.lock`
    # before the cell flips `filled` (single-writer, in `_deliver_result!`), so a
    # waiter waking on `wait_settled` always sees the result by the time it reads it.
    result::Union{R, Nothing}
    # Terminal stamp (wall ns) — when the goal settled; `0` while live. Drives
    # TTL eviction of the result cache (the goal table holds settled goals only
    # long enough for a late `get_result`, §9).
    @atomic settled_at::Int64
    # Set once a `get_result` has replayed this goal's result; lets `_publish_status`
    # drop it from the status array (rclcpp prunes fetched terminal goals) and lets
    # eviction reclaim it eagerly rather than waiting out the full TTL.
    @atomic fetched::Bool
    # The execution task (set when scheduled); tracked so `close(server)` joins
    # outstanding goals incl. detached post-cancel cleanup (§14 / DESIGN §drain).
    _task::Union{Task, Nothing}
end

"The goal's 16-byte `unique_identifier_msgs/UUID` (§9)."
goal_id(g::GoalHandle) = g.id

"""
    state(goal) -> Symbol

The goal's lifecycle state (§9): `:accepted`, `:executing`, `:canceling`,
`:succeeded`, `:canceled`, `:aborted`, or `:unknown`. The same accessor serves
the server `GoalHandle` and the client-side goal handle.
"""
state(g::GoalHandle) = _state_symbol(@atomic g.status)

Base.show(io::IO, g::GoalHandle{A}) where {A} =
    print(io, "GoalHandle(", nameof(A), " ", _state_symbol(@atomic g.status), ")")

# Transition guard: only advance from a non-terminal state, atomically. The
# terminal latch is owned by settlement (`_deliver_result!`), which writes under
# the *same* `g.lock`; so a transition can never clobber a settled status — it
# either observes the terminal state here and bails, or runs first and is then
# overwritten by the terminal latch. Either way terminal wins.
function _transition!(g::GoalHandle, to::GoalState)
    @lock g.lock begin
        cur = @atomic g.status
        _is_terminal_state(cur) && return false
        @atomic g.status = to
        return true
    end
end

# ── cancellation checkpoints (§9) ──────────────────────────────────────────────
# The structured-cancellation surface. `iscancelled` is the predicate;
# `checkpoint` throws `Cancelled` at a yield point if canceling; `feedback!`
# publishes a feedback message *and* checkpoints (so a normal progress-report loop
# is also a cancellation-observation loop with no extra code).

"""
    iscancelled(goal) -> Bool

True once the goal has been moved to `CANCELING` by an accepted cancel request
(§9). Cheap atomic read — the non-throwing form of [`checkpoint`](@ref) for code
that wants to branch rather than unwind.
"""
iscancelled(g::GoalHandle) = (@atomic g.status) === GOAL_CANCELING

"""
    checkpoint(goal)

A cancellation yield point (§9): throws [`Cancelled`](@ref) if the goal is
`CANCELING`, otherwise returns. Place it in long loops so cancellation unwinds
structurally — the fail-safe settlement then maps the `Cancelled` to a `CANCELED`
result. After a goal is already settled (e.g. detached post-cancel cleanup ran
`respond!`), `checkpoint` is a no-op so the cleanup runs to completion.
"""
function checkpoint(g::GoalHandle)
    # Once settled, the cancel signal is spent — detached cleanup must not keep
    # throwing (DESIGN: "checkpoint() now a no-op" after the cleanup respond!).
    isfilled(g.cell) && return nothing
    iscancelled(g) && throw(Cancelled())
    nothing
end

# ── feedback (§9) ──────────────────────────────────────────────────────────────

"""
    feedback!(goal, fb)

Publish one feedback message `fb` (a `Feedback` struct for the action) on the
action's feedback topic, *and* checkpoint cancellation (§9): if the goal is
`CANCELING` this throws [`Cancelled`](@ref) before publishing, so a feedback loop
doubles as a cancellation-observation loop. Sugar for
`respond!(goal, feedback, fb)`.

A feedback publish after the goal is settled is dropped (the client has the
result; the stream is closed).
"""
function feedback!(g::GoalHandle{A, G, R, F}, fb::F) where {A, G, R, F}
    checkpoint(g)
    isfilled(g.cell) && return nothing
    _publish_feedback(g.server, g.id, fb)
    nothing
end

# `respond!(goal, feedback, fb)` spelling — feedback is a stream verb, routed
# here rather than to the cell (settlement.jl rejects `feedback` as non-terminal).
function respond!(g::GoalHandle{A, G, R, F}, ::Feedback, fb::F) where {A, G, R, F}
    feedback!(g, fb)
end

# ── settle verbs (§9) ──────────────────────────────────────────────────────────
# `respond!` is the authoritative settle (settlement.jl); `succeed`/`abort` read
# better at the call site (DESIGN §respond!), same write underneath. `canceled`
# is spelled via the token directly (`respond!(goal, canceled, r)`) — no
# dedicated verb, mirroring services' `failed`.

"""
    respond!(goal, status, payload) -> Bool

Settle the goal's write-once result cell with a terminal `status`
([`succeeded`](@ref)/[`canceled`](@ref)/[`aborted`](@ref)) and the result
`payload` (§9). The first terminal `respond!` wins, runs the result-cache
delivery + status publication, and releases the client's `get_result`; a second
explicit terminal `respond!` is the one hard error (settlement.jl). Returns
`true` if this call filled the cell.

`respond!(goal, feedback, fb)` is the stream form (`feedback!` sugar), dispatched
separately — it never touches the cell.
"""
function respond!(g::GoalHandle{A, G, R, F}, status::SettlementStatus, payload) where {A, G, R, F}
    respond!(g.cell, status, payload)
end

"""
    succeed(goal, result)

Settle the goal as `SUCCEEDED` with `result` — `respond!(goal, succeeded, result)`
(§9). The common explicit terminal verb when the handler computes its result
before its final expression.
"""
succeed(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, succeeded, result)

"""
    abort(goal, result)

Settle the goal as `ABORTED` with `result` — `respond!(goal, aborted, result)`
(§9). The explicit failure verb; a thrown (non-`Cancelled`) exception reaches the
same outcome via fail-safe settlement.
"""
abort(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, aborted, result)

# ── ActionServer (§9) ──────────────────────────────────────────────────────────
# The long-lived server: three Zenoh queryables (send_goal / cancel_goal /
# get_result), a feedback publisher, a status publisher, a goal table (the result
# cache + live goals), and the user callbacks. Each sub-endpoint is a generic
# `Entity` (node.jl) so it gets a liveliness token + graph injection + close-on-
# node-close for free; the queryable/publisher routes hang off `entity.wire`.

"""
    ActionServer{A, G, R, F}

A ROS2 action server for action type `A` (§9). Owns the three protocol services
(`send_goal`/`cancel_goal`/`get_result`), the `feedback` + `status` topics, the
goal table (live goals + the result cache), and the user callbacks
(`on_goal`/`on_cancel`/`on_accepted`). `close`-able; dies with its node, joining
outstanding goal tasks (incl. detached post-cancel cleanup) up to the Context
drain timeout (§14).

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
    # time, §4) — a *dedicated* lock, not the node-wide one (which guards cross-
    # entity state and must stay free while a long mission runs).
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

    # §D8: precompile goal-decode → per-goal execution callable → result/feedback
    # encode. Precompile-only (no :execute — a fabricated long-running goal body
    # over a live GoalHandle isn't safe to run at warm-up). `exec` is the do-block
    # body (high-level) or the `on_accepted` callback (low-level).
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

    decision = _with_node_logger(() -> server.on_goal(goal_req), server.node)  # D7
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

# Evict terminal goals once their result is no longer recoverable: settled longer ago
# than the cache TTL (a late OR repeated `get_result` past the window). NOT on
# `fetched` — a repeated fetch must still replay the cached result; that flag only
# prunes the status array. Live goals (settled_at == 0) are never touched, nor is
# a goal whose task is still running (a detached post-cancel cleanup), so the drain
# join (`_await_goals`) can still reach it. Caller holds `server.lock`. Bounds both
# the table size and `_publish_status`'s cost, which scales with the table (rclcpp
# prunes terminal goals the same way).
function _evict_terminal_goals!(server::ActionServer)
    now = _now_ns(server.node)
    for (id, g) in server.goals
        settled = @atomic g.settled_at
        settled == 0 && continue                       # still live
        t = g._task
        (t === nothing || istaskdone(t)) || continue   # cleanup still in flight
        # Evict purely by the cache TTL — a late OR repeated `get_result` must still
        # replay a cached result within the window, so do NOT evict on `fetched` (that
        # flag only prunes the status array). rclcpp's result_timeout behaves the same.
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
    # discovery, so the same id can arrive more than once. Re-registering would orphan
    # the original goal's result cell — the one a held `get_result` blocks on — and spawn
    # a duplicate body, deadlocking `fetch`. A known id reuses the existing goal. Returns
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
# run `on_cancel` per matched goal, and on accept move it to CANCELING (arming the
# `checkpoint`/`feedback!` cancellation throw — it does *not* stop execution; the
# handler observes the token, §9).

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
        _with_node_logger(() -> server.on_cancel(g), server.node) isa Reject && continue  # D7
        if _transition!(g, GOAL_CANCELING)
            push!(accepted, g)
        end
    end
    !isempty(accepted) && _publish_status(server)
    _reply_cancel(server, q, accepted)
    nothing
end

# ── get_result service (§9) ────────────────────────────────────────────────────
# The client's `fetch` blocks on a `get_result` request. We must reply only once
# the goal is terminal — so the queryable handler *waits* on the result cell
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
        # `Serial()` the task is sticky — it stays on the node's cooperative thread
        # (D3), consistent with the single-threaded default; `Parallel` bodies run
        # on spawned threads.
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
# on the server's dedicated `serial_lock` (§4 one-at-a-time — *not* the node-wide
# lock, which must stay free during a long mission); `Parallel` runs free.
function _run_goal_body(g::GoalHandle{A, G, R, F}, body::Function, concurrency::Concurrency) where {A, G, R, F}
    _transition!(g, GOAL_EXECUTING)
    server = g.server
    _publish_status(server)
    # D7: run the goal body under the node logger so a plain `@info` inside it
    # routes to the server node's /rosout.
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
# A request always carries a CDR body, even an empty one — a missing payload is a
# protocol error from the peer.
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

# ── ActionClient (§9) ──────────────────────────────────────────────────────────
# `send` issues a send_goal request and returns a client-side goal handle that is
# iterable over feedback, `fetch`-able for the result, and `cancel`-able. The
# three services are reached with Zenoh `get` (one per call); feedback is a
# Subscription on the feedback topic filtered by goal id.

"""
    ActionClient{A, G, R, F}

A ROS2 action client for action type `A` (§9). `close`-able; dies with its node.
Use [`send`](@ref) to dispatch a goal, then iterate [`feedback`](@ref), block on
[`fetch`](@ref) for the result, and [`cancel`](@ref) to request cancellation.
"""
mutable struct ActionClient{A, G, R, F}
    const node::Node
    const name::String                   # resolved action FQN
    const support::ActionTypeSupport{A, G, R, F}
    const qos::QosProfile
    @atomic open::Bool
    # Lazy routing-match probes (`_ClientWire`s, service.jl) on the services the
    # send→fetch flow uses — `send_goal` and `get_result` — declared on the first
    # `wait_for_action_server`/`action_server_matched`, `nothing` until then (a client
    # that never awaits declares no extra Queriers). `send`/`fetch` use one-shot gets
    # and never touch these.
    @atomic _match::Union{Vector{_ClientWire}, Nothing}
    const _match_lock::ReentrantLock
end

"""
    ActionClient(node, name, A; qos=default_qos()) -> ActionClient

Declare an action client for action type `A` on `name` (§9). The three service
requests are issued on demand via Zenoh `get`; feedback is subscribed per-goal.
"""
function ActionClient(node::Node, name::AbstractString, ::Type{A};
                      qos::QosProfile = default_qos()) where {A}
    support = ActionTypeSupport(A)
    G = goal_type(support); R = result_type(support); F = feedback_type(support)
    fqn = resolve_name(node, name; kind=:service)
    ActionClient{A, G, R, F}(node, fqn, support, qos, true, nothing, ReentrantLock())
end

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

# Lazily declare the matching probes (Queriers on the same keyexprs `send`/`fetch`
# query) the first time the server is awaited; reuses the shared `_ClientWire`
# machinery (service.jl). We probe BOTH `send_goal` and `get_result`: `send` retries
# discovery on its own, but `fetch`'s `get_result` is a one-shot get with a long
# timeout, so awaiting its match is what keeps `fetch` from blocking on an unmatched
# service. `send`/`fetch` themselves don't need these, so a client that never awaits
# pays nothing.
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

True iff the client is routing-matched to an action server right now — its
`send_goal` *and* `get_result` Queriers both matched the server's queryables (§9).
That's the guarantee both [`send`](@ref) and [`fetch`](@ref) will reach the server
immediately rather than block/retry on discovery. The routing-plane signal behind
[`wait_for_action_server`](@ref); lazily declares the probes on first use. `false`
for a closed client.
"""
function action_server_matched(client::ActionClient)
    isopen(client) || return false
    all(_wire_matched, _ensure_action_match!(client))
end

"""
    ClientGoal{A, G, R, F}

A client-side handle for a dispatched goal (§9): iterable over feedback,
`fetch`-able for the result, `cancel`-able, and `state`-queryable. Returned by
[`send`](@ref) once the server has accepted or rejected.
"""
mutable struct ClientGoal{A, G, R, F}
    const client::ActionClient{A, G, R, F}
    const id::GoalId
    @atomic _state::Symbol               # :accepted/:rejected/:executing/terminal
    # Lazily-opened feedback subscription (a SubscriptionHandle); nothing until
    # `feedback(goal)` is first iterated.
    _feedback_sub::Any
    # A goal settles exactly once, so its result is a single write-once slot. One
    # framework `get_result` task (started lazily by `fetch` or `feedback`) fills
    # `_outcome` once — `Some(result)` on success, an `Exception` on failure — and
    # notifies `_settled`. `fetch` returns/raises it (idempotent across repeat calls);
    # `feedback` ends its stream when it fills, so the caller never spawns a fetch.
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

Dispatch `goal` (a `Goal` struct for the action) on `client` and return a handle
once the server replies accepted/rejected (§9). Blocks this task on the
`send_goal` service reply (raises `ShutdownException` if the Context drains).
Inspect [`state`](@ref); iterate [`feedback`](@ref); block on [`fetch`](@ref).
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
    # one-shot `get` to a not-yet-route-matched queryable returns NO reply — which must
    # not be read as a reject. Retry on an empty result until a reply arrives (then it is
    # authoritative) or the window elapses (no server ⇒ rejected). A retried `get`
    # re-sends only when no reply came back — i.e. the server never received it.
    accepted = false
    got_reply = false
    for _ in 1:40                                          # up to ~2s of 50ms backoffs
        for r in Base.get(ctx.session, Keyexpr(tk), ""; payload=encode(req), timeout_ms=5000)
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

Stream the goal's feedback messages (§9), each a `Feedback` struct. Lazily opens
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
    # ends on settle without the caller spawning a fetch. The closer waits on the
    # result slot — no polling — then tears the subscription down.
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
# queryable can lag discovery — a one-shot get to an unmatched queryable returns NO
# reply — so retry until a reply lands (mirroring `send`). Once matched the server
# holds the reply until the goal settles, so the matched attempt blocks (to the
# timeout) for the real result; retries only re-fire while the route is still empty.
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
                          payload=encode(req), timeout_ms=_GET_RESULT_TIMEOUT_MS)
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

Block until the goal settles and return its `Result` (§9). The goal settles exactly
once, so the result lives in a single write-once slot the framework fills via one
`get_result` request — the first `fetch`/`feedback` starts it, and every `fetch`
returns that same result. Raises [`ShutdownException`](@ref) on drain, or an error if
the goal was rejected or the server reported a failure.
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

Request cancellation of the goal (§9): issues a `cancel_goal` request to the
server, which runs its `on_cancel` and (on accept) moves the goal to `CANCELING`
so its handler's [`checkpoint`](@ref)/[`feedback!`](@ref) throw [`Cancelled`](@ref).
Returns once the server replies; the goal's terminal state is observed via
[`fetch`](@ref).
"""
function cancel(g::ClientGoal{A, G, R, F}) where {A, G, R, F}
    client = g.client
    ctx = client.node.context
    tk = _service_key(client, _cancel_goal_topic(client.name), _client_service_ti(client, :cancel_goal))
    # Request `action_msgs/CancelGoal_Request{goal_info{goal_id, stamp}}` targeting
    # this goal (a zeroed goal_id would be the server's cancel-all sentinel).
    req = _CancelGoal_Request(; goal_info = _GoalInfo(; goal_id = _to_uuid(g.id),
                                                      stamp = to_msg(_Time, Dates.now(client.node))))
    for r in Base.get(ctx.session, Keyexpr(tk), ""; payload=encode(req), timeout_ms=5000)
        break
    end
    @atomic g._state = :canceling
    nothing
end

# The Zenoh data-route keyexpr for a client-side service call: build the wire key
# the server's queryable declared. We construct a transient `EndpointEntity` of
# the matching kind so `topic_keyexpr` produces the same key — the client's own
# id doesn't enter the topic key (only the topic + type + hash do, §2.2).
function _service_key(client::ActionClient, topic::AbstractString, ti::TypeInfo)
    node = client.node
    e = ROSZenoh.EndpointEntity(; id=0, node=node.entity, kind=Service,
                                topic=String(topic), type_info=ti, qos=client.qos)
    return topic_keyexpr(node.context.format, e)
end

# ── SingleFlight orchestrator (§9, optional helper) ─────────────────────────────
# An orchestrator over the low-level API (NOT an `ActionServer` feature, §9): one
# active goal + a bounded queue, the common one-actuator pattern. Built on
# `on_accepted = g -> submit!(sched, g)` + `execute!(sched) do goal … end`. Pause
# reuses `checkpoint` (the orchestrator flips a flag the body's `checkpoint`
# blocks on). Shipped as an optional helper, deliberately minimal — richer
# scheduling is the next rung (§15.2 deferred).

"""
    SingleFlight(; queue=4)

An optional action orchestrator (§9): runs one accepted goal at a time with a
bounded `queue` of pending goals. Wire it via `on_accepted = g -> submit!(sched, g)`
on an [`ActionServer`](@ref) (which should `defer` in `on_goal`), then drive
execution with `execute!(sched) do goal … end`. Not a server feature — *a* policy,
not *the* server.

This is the minimal shape; `pause`/`resume`/`queued_goals` are sketched as the
orchestrator's own surface (DESIGN §orchestration).
"""
mutable struct SingleFlight
    const queue::Channel{Any}            # pending GoalHandles, bounded
    @atomic paused::Bool
    const lock::ReentrantLock
    @atomic _active::Any                 # the running GoalHandle, or nothing
end

SingleFlight(; queue::Integer = 4) =
    SingleFlight(Channel{Any}(max(1, Int(queue))), false, ReentrantLock(), nothing)

"""
    submit!(sched::SingleFlight, goal)

Enqueue an accepted (deferred) goal for the orchestrator to run (§9). The
`on_accepted` wiring (`g -> submit!(sched, g)`). Blocks if the queue is full
(backpressure); the goal stays `ACCEPTED` until [`execute!`](@ref) picks it up.
"""
function submit!(sched::SingleFlight, goal::GoalHandle)
    put!(sched.queue, goal)
    nothing
end

"""
    execute!(body, sched::SingleFlight)
    execute!(sched::SingleFlight) do goal … end

Run the orchestrator loop: pull one goal at a time off the queue and `execute` it
with `body(goal)` (the same `Cancelled` + settlement wrapper, §9), waiting for
each to finish before the next (single-flight). Honors [`pause`](@ref) at the
dispatch boundary — while paused the loop holds the next goal in the queue rather
than starting it. Spawned on its own task; returns the task.

(DESIGN sketches a richer pause that also blocks a *running* goal's `checkpoint`;
that needs the orchestrator's flag threaded through the `GoalHandle`, a later
rung — this minimal helper gates between goals.)
"""
function execute!(body::Function, sched::SingleFlight)
    Threads.@spawn begin
        for goal in sched.queue
            # Pause gate: hold the next goal while paused (cooperative, polled).
            while (@atomic sched.paused)
                Base.sleep(0.02)
            end
            @atomic sched._active = goal
            try
                execute(goal::GoalHandle, body)
                # Single-flight: wait for the goal's task before the next.
                t = goal._task
                t === nothing || wait(t)
            catch err
                @error "SingleFlight.execute! goal failed" exception=(err, catch_backtrace())
            finally
                @atomic sched._active = nothing
            end
        end
    end
end
execute!(sched::SingleFlight, body::Function) = execute!(body, sched)

# `pause`/`resume` are the orchestrator's own surface (not exported — `pause`
# shadows the unexported `Base.pause` CPU hint otherwise; reach via `ROSNode.pause`).
"Pause the orchestrator: queued goals wait, and a running goal's `checkpoint` blocks (§9)."
function pause(sched::SingleFlight; reason::AbstractString = "")
    @atomic sched.paused = true
    isempty(reason) || @info "SingleFlight paused" reason
    nothing
end

"Resume a paused orchestrator (§9)."
resume(sched::SingleFlight) = (@atomic sched.paused = false; nothing)

"The orchestrator's currently-running goal, or `nothing` (introspection, §9)."
active_goal(sched::SingleFlight) = @atomic sched._active

# ── enum-instance call-methods: the §6 constructor spelling for ActionServer ────
# `ActionServer` is a plain type here (not an `EndpointKind` instance — an action
# spans five endpoints). Constructors route to `_make_action_server`: the
# do-block form takes the body first (the `Timer(f, …)` precedent, §6), the
# low-level form takes only kwargs.

# Low-level: ActionServer(node, name, A; on_goal, on_cancel, on_accepted, …).
ActionServer(node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; kwargs...)

# High-level do-block: ActionServer(node, name, A; concurrency, …) do goal … end.
ActionServer(body::Function, node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; body=body, kwargs...)
