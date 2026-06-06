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
[`ResultCell`](@ref) the handler settles. Cancellation is structured:
[`feedback!`](@ref)/[`checkpoint`](@ref) throw [`Cancelled`](@ref) once the goal
is `CANCELING`, so a goal in `CANCELING` always unwinds to a `CANCELED` result on
the default path.

Settle it with [`succeed`](@ref)/[`abort`](@ref)/`canceled` via
[`respond!`](@ref), or just `return`/throw from the handler (fail-safe
settlement, §8/§9). The server owns the result cache and status publication; the
handle is the user-facing verb surface.

See the ROS 2 action concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html
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
    # Once settled, the cancel signal is spent: a no-op here lets detached
    # post-cancel cleanup run to completion instead of re-throwing Cancelled.
    isfilled(g.cell) && return nothing
    iscancelled(g) && throw(Cancelled())
    nothing
end

# ── feedback (§9) ──────────────────────────────────────────────────────────────

"""
    feedback!(goal, fb)

Publish one feedback message `fb` (a `Feedback` struct for the action) on the
action's feedback topic, then checkpoint cancellation (§9): a `CANCELING` goal
throws [`Cancelled`](@ref) before publishing, so a feedback loop doubles as a
cancellation-observation loop. Sugar for `respond!(goal, feedback, fb)`.

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

