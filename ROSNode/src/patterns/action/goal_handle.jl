# ── GoalHandle (§9) ────────────────────────────────────────────────────────────
# The per-goal object: the thing you `feedback!`, settle, and observe
# cancellation on. Holds the goal id, the goal request, the live state, the
# write-once result cell (settlement.jl), and a back-reference to the server for
# feedback publication + status updates. Two lifetimes: the server is long-lived;
# a GoalHandle lives from acceptance until its result is fetched (or the cache
# evicts it).

"""
    GoalHandle{A, G, R, F}

A single accepted action goal, server-side. Carries the 16-byte
`unique_identifier_msgs/UUID` goal id, the decoded goal request (a `Goal` struct,
reachable as `goal.request`), the live goal state ([`state`](@ref)), and the
write-once result cell the handler settles. This is the user-facing verb surface
for one goal; the server owns the result cache and `status` publication.

Cancellation is structured. Once an accepted cancel moves the goal to
`CANCELING`, [`feedback!`](@ref) and [`checkpoint`](@ref) throw
[`Cancelled`](@ref) at the next yield point, so a goal in `CANCELING` unwinds to
a `CANCELED` result on the default path with no manual polling. Settle explicitly
with [`succeed`](@ref)/[`abort`](@ref) (or `respond!(goal, canceled, result)`),
or just `return` a `Result` (settles `SUCCEEDED`) or throw (settles `ABORTED`) —
the fail-safe wrapper guarantees the cell is filled exactly once.

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
    const accepted_at::Int64             # acceptance stamp (wall ns, §3.4/§7)
    const lock::ReentrantLock
    @atomic status::GoalState
    # The settled result, cached for `get_result` replay so a late or repeated
    # fetch is answered without re-running the goal. The cell delivers the payload
    # through `deliver` without retaining it, so the first terminal fill captures
    # it here. `nothing` until settled; written under `g.lock` before the cell
    # flips `filled` (single-writer, in `_deliver_result!`), so a waiter waking on
    # `wait_settled` always sees the result by the time it reads it.
    result::Union{R, Nothing}
    # Terminal stamp (wall ns) — when the goal settled; `0` while live. Drives
    # TTL eviction of the result cache (the goal table holds settled goals only
    # long enough for a late `get_result`, §9).
    @atomic settled_at::Int64
    # Set once a `get_result` has replayed this goal's result; `_publish_status`
    # then drops it from the status array (rclcpp prunes fetched terminal goals).
    # Eviction keys on the TTL alone — a repeated fetch must still replay.
    @atomic fetched::Bool
    # The execution task (set when scheduled); tracked so `close(server)` joins
    # outstanding goals incl. detached post-cancel cleanup (§14 / DESIGN §drain).
    _task::Union{Task, Nothing}
end

"The goal's 16-byte `unique_identifier_msgs/UUID` (§9)."
goal_id(g::GoalHandle) = g.id

"""
    state(goal) -> Symbol

The goal's current lifecycle state as a `Symbol`: `:accepted`, `:executing`,
`:canceling`, `:succeeded`, `:canceled`, `:aborted`, or `:unknown`
(`action_msgs/msg/GoalStatus`). The same accessor reads a server-side
[`GoalHandle`](@ref) and a client-side dispatched-goal handle; the client side
adds `:rejected` (the handle [`send`](@ref) returns when the server declines), a
local symbol the server-side handle never reports — a rejected goal is never
registered.

The Symbol surface keeps the wire `GoalStatus` enum internal and reads naturally
at a call site (`state(goal) === :executing`).
"""
state(g::GoalHandle) = _state_symbol(@atomic g.status)

Base.show(io::IO, g::GoalHandle{A}) where {A} =
    print(io, "GoalHandle(", nameof(A), " ", _state_symbol(@atomic g.status), ")")

# Transition guard: advance the status only from a non-terminal state, under
# `g.lock`. Settlement's terminal latch (`_deliver_result!`) takes the same lock,
# so terminal always wins — a transition either sees the terminal state and bails
# or is overwritten by the latch.
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
    # Once settled, the cancel signal is spent: a no-op here lets detached
    # post-cancel cleanup run to completion.
    isfilled(g.cell) && return nothing
    iscancelled(g) && throw(Cancelled())
    nothing
end

# ── feedback (§9) ──────────────────────────────────────────────────────────────

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

Settle the goal as `SUCCEEDED` with `result` (a `Result` struct for the action) —
shorthand for `respond!(goal, succeeded, result)`. Use it when the handler
computes its result before its final expression; a plain `return result` from a
`do`-block body reaches the same outcome.

The first terminal settle wins, runs the result-cache delivery and `status`
publication, and releases the client's blocked `get_result`. Returns `true`; a
second explicit terminal settle raises `ArgumentError` (the double-settle guard).
"""
succeed(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, succeeded, result)

"""
    abort(goal, result)

Settle the goal as `ABORTED` with `result` (a `Result` struct for the action) —
shorthand for `respond!(goal, aborted, result)`. The explicit failure verb; a
thrown non-`Cancelled` exception from the handler reaches the same `ABORTED`
outcome through fail-safe settlement.

The first terminal settle wins. Returns `true`; a second explicit terminal settle
raises `ArgumentError` (the double-settle guard).
"""
abort(g::GoalHandle{A, G, R, F}, result::R) where {A, G, R, F} = respond!(g, aborted, result)

