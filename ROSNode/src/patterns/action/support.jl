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

