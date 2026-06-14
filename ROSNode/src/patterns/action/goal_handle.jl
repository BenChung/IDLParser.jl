# ── GoalHandle ───────────────────────────────────────────────────────────────
# The per-goal object you `feedback!`, settle, and observe cancellation on. It
# holds a back-reference to the long-lived server; a GoalHandle itself lives from
# acceptance until the result-cache TTL evicts it after settlement.

"""
    GoalHandle{A, G, R, F}

A single accepted action goal, server-side. Carries:

- the 16-byte `unique_identifier_msgs/UUID` goal id
- the decoded goal request, a `Goal` struct reachable as `goal.request`
- the live goal state ([`state`](@ref))
- the write-once result cell ([`respond!`](@ref)) the handler settles

This is the user-facing verb surface for one goal; the server owns the result
cache and `status` publication.

Cancellation is structured. Once an accepted cancel moves the goal to
`CANCELING`, [`feedback!`](@ref) and [`checkpoint`](@ref) throw
[`Cancelled`](@ref) at the next yield point, so a goal in `CANCELING` unwinds to
a `CANCELED` result on the default path with no manual polling. This domain's
handler-exit status mapping:

- `return` a `Result` ⇒ `SUCCEEDED`
- throw [`Cancelled`](@ref) ⇒ `CANCELED`
- throw anything else (or fall through with no settle) ⇒ `ABORTED`

Settle explicitly with [`succeed`](@ref)/[`abort`](@ref) (or
`respond!(goal, canceled, result)`); the exactly-once handler-exit contract is
owned by [`respond!`](@ref).

Constructed by the framework on goal acceptance, never directly. A handle lives
from acceptance until the result-cache TTL evicts it after settlement.

Follows the ROS 2 action goal lifecycle:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html
"""
mutable struct GoalHandle{A, G, R, F}
    const server::Any                    # back-ref to the ActionServer (duck-typed)
    const id::GoalId
    const request::G
    const cell::ResultCell               # ResultCell{GoalHandle, R}; concrete elided
    const accepted_at::Int64             # acceptance stamp (wall ns)
    const lock::ReentrantLock
    @atomic status::GoalState
    # The settled result, cached so a late or repeated `get_result` is answered
    # without re-running the goal (the cell delivers the payload without retaining it).
    # Written once under `g.lock` before the cell flips `filled`, so a waiter waking
    # on `wait_settled` always sees the result.
    result::Union{R, Nothing}
    # Terminal stamp (wall ns), `0` while live. Drives TTL eviction of the result cache.
    @atomic settled_at::Int64
    # Set once `get_result` has replayed this result; `_publish_status` then drops the
    # goal from the status array. Eviction keys on the TTL alone — a repeated fetch
    # must still replay — so this flag never gates cache deletion.
    @atomic fetched::Bool
    # The execution task, tracked so `close(server)` joins outstanding goals including
    # detached post-cancel cleanup.
    _task::Union{Task, Nothing}
end

"The goal's 16-byte `unique_identifier_msgs/UUID`."
goal_id(g::GoalHandle) = g.id

"""
    state(goal) -> Symbol

The goal's current lifecycle state as a `Symbol`. The same accessor reads a
server-side [`GoalHandle`](@ref) and a client-side dispatched-goal handle, with
the symbol set differing by side:

| Side               | States                                                                                                |
| ------------------ | ----------------------------------------------------------------------------------------------------- |
| Server (and shared) | `:accepted`, `:executing`, `:canceling`, `:succeeded`, `:canceled`, `:aborted`, `:unknown` (`action_msgs/msg/GoalStatus`) |
| Client only        | adds `:rejected` — the symbol the handle [`send`](@ref) returns when the server declines               |

`:rejected` is a client-local symbol the server-side handle never reports: a
rejected goal is never registered.

The Symbol surface keeps the wire `GoalStatus` enum internal and reads naturally
at a call site (`state(goal) === :executing`).

This is the action-goal `state`, distinct from the same-named accessor for
managed-node lifecycle: a managed node's lifecycle state is [`state`](@ref) on a
[`LifecycleNode`](@ref).
"""
state(g::GoalHandle) = _state_symbol(@atomic g.status)

Base.show(io::IO, g::GoalHandle{A}) where {A} =
    print(io, "GoalHandle(", nameof(A), " ", _state_symbol(@atomic g.status), ")")

# Advance the status from a non-terminal state under `g.lock`. The terminal latch
# (`_deliver_result!`) takes the same lock, so terminal always wins: a transition
# either sees the terminal state and bails or is overwritten by the latch.
function _transition!(g::GoalHandle, to::GoalState)
    @lock g.lock begin
        cur = @atomic g.status
        _is_terminal_state(cur) && return false
        @atomic g.status = to
        return true
    end
end

# ── cancellation checkpoints ───────────────────────────────────────────────────
# The structured-cancellation surface. `iscancelled` is the predicate;
# `checkpoint` throws `Cancelled` at a yield point if canceling; `feedback!`
# publishes a feedback message *and* checkpoints (so a normal progress-report loop
# is also a cancellation-observation loop with no extra code).

"""
    iscancelled(goal) -> Bool

True once an accepted cancel request has moved the goal to `CANCELING`. A cheap,
lock-free atomic read — the non-throwing counterpart of [`checkpoint`](@ref), for
a handler that branches on cancellation.

Reads the live status only: a goal that has already settled as `CANCELED` reads
`false` here (it is no longer in `CANCELING`).
"""
iscancelled(g::GoalHandle) = (@atomic g.status) === GOAL_CANCELING

"""
    checkpoint(goal)

A cancellation yield point: throw [`Cancelled`](@ref) when the goal is
`CANCELING`, otherwise return `nothing`. Place it inside long-running loops so an
accepted cancel request unwinds the handler structurally — the fail-safe
settlement then maps the `Cancelled` to a `CANCELED` result.

Once the goal has already settled (for example, detached post-cancel cleanup has
run `respond!`), `checkpoint` is a no-op so that cleanup runs to completion. The
non-throwing form is [`iscancelled`](@ref), a Bool read for handlers that branch
on cancellation.

Returns `nothing`.
"""
function checkpoint(g::GoalHandle)
    # Once settled, the cancel signal is spent — a no-op lets post-cancel cleanup finish.
    isfilled(g.cell) && return nothing
    iscancelled(g) && throw(Cancelled())
    nothing
end

# ── feedback ─────────────────────────────────────────────────────────────────

"""
    feedback!(goal, fb)

Publish one feedback message `fb` (a `Feedback` struct for the action) on the
action's `feedback` topic, after checkpointing cancellation. A `CANCELING` goal
throws [`Cancelled`](@ref) before anything is published, so a progress-report
loop doubles as a cancellation-observation loop with no extra code. Equivalent to
`respond!(goal, feedback, fb)`.

A feedback publish after the goal has settled is dropped (the client already
holds the result and the feedback stream is closed). The message rides the
`feedback` topic as a `<A>_FeedbackMessage` (goal_id + feedback) so a client
filters it to the matching goal.

Returns `nothing`.
"""
function feedback!(g::GoalHandle{A, G, R, F}, fb::F) where {A, G, R, F}
    checkpoint(g)
    isfilled(g.cell) && return nothing
    _publish_feedback(g.server, g.id, fb)
    nothing
end

# `respond!(goal, feedback, fb)` spelling — feedback is a stream verb, routed here
# rather than to the cell, which only accepts terminal settlement tokens.
function respond!(g::GoalHandle{A, G, R, F}, ::Feedback, fb::F) where {A, G, R, F}
    feedback!(g, fb)
end

# ── settle verbs ───────────────────────────────────────────────────────────────
# `respond!` is the authoritative settle; `succeed`/`abort` read better at the
# call site, same write underneath. `canceled` is spelled via the token directly
# (`respond!(goal, canceled, r)`) — no dedicated verb, mirroring services'
# `failed`.

"""
    respond!(goal, status, payload) -> Bool

Settle the goal with a terminal `status` and the result `payload`. The action
status set:

- [`succeeded`](@ref) — the goal completed
- [`canceled`](@ref) — an accepted cancel unwound the goal
- [`aborted`](@ref) — the goal failed

Filling the goal's write-once result `ResultCell` is the shared single-fill
contract of [`respond!`](@ref). One fill drives:

- the first-wins latch
- cache delivery
- `status` publication
- `get_result` release

Returns `true` if this call filled the cell.

`respond!(goal, feedback, fb)` is the stream form (`feedback!` sugar), dispatched
separately — it never touches the cell. The service tokens
([`success`](@ref)/[`failure`](@ref)) have no goal status and raise
`ArgumentError`.
"""
function respond!(g::GoalHandle{A, G, R, F}, status::SettlementStatus, payload) where {A, G, R, F}
    respond!(g.cell, status, payload)
end

# Service tokens are terminal for the cell but have no goal-status mapping
# (`_terminal_state`); rejecting here keeps them from latching the cell with a
# status `get_result` can never publish.
respond!(::GoalHandle, status::Union{Success, Failure}, _) =
    throw(ArgumentError("respond!: $(status) is a service token; settle a goal \
                         with succeeded/canceled/aborted"))

"""
    succeed(goal, result)

Settle the goal as `SUCCEEDED` with `result` (a `Result` struct for the action) —
shorthand for `respond!(goal, succeeded, result)`. Use it when the handler
computes its result before its final expression; a plain `return result` from a
`do`-block body reaches the same outcome.

Settlement is single-fill via [`respond!`](@ref); returns `true` if this call
filled the cell.
"""
succeed(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, succeeded, result)

"""
    abort(goal, result)

Settle the goal as `ABORTED` with `result` (a `Result` struct for the action) —
shorthand for `respond!(goal, aborted, result)`. The explicit failure verb; a
thrown non-`Cancelled` exception from the handler reaches the same `ABORTED`
outcome through fail-safe settlement.

Settlement is single-fill via [`respond!`](@ref); returns `true` if this call
filled the cell.
"""
abort(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, aborted, result)

