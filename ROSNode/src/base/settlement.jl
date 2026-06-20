# The write-once result cell and fail-safe settlement for services and actions.
#
# A client always blocks on the result, so every handler exit must settle: the
# cell is filled exactly once before the task is reaped. This file is
# the mechanism — generic over the handle (Service request / action GoalHandle)
# and the result type — so both reuse one settlement core. The status tokens
# (`success`/`failure`, `succeeded`/`canceled`/`aborted`, `feedback`) come from
# core.jl; this layer turns a token + payload into a single authoritative write.
#
# Wiring the actual reply/result-cache delivery (Zenoh `reply` / `reply_err`,
# status publication) is the Service/Action layer's job: it constructs the cell
# with a `deliver` closure that this file invokes exactly once on first fill.

# ── the cell ──────────────────────────────────────────────────────────────

"""
    ResultCell{H, R}

Write-once settlement slot shared by services and actions. `H` is the
handle type (a service request or an action `GoalHandle`) carried so the deliver
closure can reach it; `R` is the user result type.

This is the result-delivery contract behind both layers: a ROS 2 service returns
exactly one response per request, and a ROS 2 action delivers one final result per
goal.

- Services: https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html
- Actions:  https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html

Filled exactly once: the first terminal write wins and runs `deliver`, and the
status it was filled with is observable via [`outcome`](@ref). Three paths fill
the cell (first wins):

- explicit [`respond!`](@ref) — the authoritative write.
- the handler's return value — fills only if still empty.
- the fail-safe wrapper — fills only if still empty.

A second *explicit* terminal `respond!` after the cell is filled is the one hard
error; every other later attempt is silently ignored ([`isfilled`](@ref) guards
the return-value path).

The lock makes concurrent settlement safe: a detached cleanup task can race the
fail-safe wrapper, and only one wins.

The `detached` flag (set by the service `detach!`) tells [`settle_handler!`](@ref)'s
fail-safe `finally` to *not* force-abort a still-empty cell at handler return —
settlement ownership has transferred to a spawned task. `respond!`/`fill!`/
`_settle!`/`wait_settled` stay detach-agnostic; only the at-return backstop reads it.
"""
mutable struct ResultCell{H, R}
    const handle::H
    # deliver(status::SettlementStatus, payload) — hands the settled value to the
    # wire (reply-ok / query-error / result cache). Runs once, under the lock,
    # holding the first terminal write. Provided by the owning Service/Action.
    const deliver
    const lock::ReentrantLock
    # Settlement signal, built on `lock`. A waiter (the action `get_result` request
    # task) parks here and is woken the instant the cell fills, instead of
    # polling. The settle paths already hold `lock`, so they `notify` it directly.
    const cond::Threads.Condition
    @atomic filled::Bool
    status::Union{SettlementStatus, Nothing}   # the terminal token that filled it
    # Set by `detach!` (service.jl) to transfer settlement ownership off the
    # handler frame: a detached still-empty cell is not force-aborted at handler
    # return — a spawned task settles it later, bounded by the detach deadline.
    @atomic detached::Bool
end

function ResultCell{H, R}(handle::H, deliver) where {H, R}
    lk = ReentrantLock()
    return ResultCell{H, R}(handle, deliver, lk, Threads.Condition(lk), false, nothing, false)
end

"""
    isfilled(cell::ResultCell) -> Bool

True once the cell holds a terminal outcome. The return-value fallback and the
fail-safe wrapper both gate on this so they never overwrite an explicit
[`respond!`](@ref). Reads the atomic flag — cheap and lock-free.
"""
isfilled(cell::ResultCell) = @atomic cell.filled

"""
    outcome(cell::ResultCell) -> Union{SettlementStatus, Nothing}

The token the cell was settled with (`nothing` while empty). Lets the state
machine map the settlement onto the goal status (SUCCEEDED/CANCELED/ABORTED) and
lets `feedback` be rejected where a terminal result is due.
"""
outcome(cell::ResultCell) = cell.status

# ── respond! — the authoritative settle verb ────────────────────────────────

"""
    respond!(cell, status::SettlementStatus, payload) -> Bool

The single settlement verb. The terminal tokens — service
[`success`](@ref)/[`failure`](@ref), action [`succeeded`](@ref)/[`canceled`](@ref)/
[`aborted`](@ref) — perform the *authoritative* write: the first one wins, runs
`deliver`, and latches the cell. Returns `true` if this call filled it.

Behavior on an already-full cell depends on the path:

- explicit second terminal `respond!` — the one hard error; it signals a
  double-settle bug.
- return-value and fail-safe paths — go through `fill!`, which silently ignores
  an already-full cell.

[`feedback`](@ref) is not terminal — it never touches the cell; the Action layer
dispatches it to the feedback stream before reaching settlement.
"""
function respond!(cell::ResultCell, status::SettlementStatus, payload)
    is_terminal(status) ||
        throw(ArgumentError("respond!: $(status) is not a terminal status; \
                             feedback is a stream, not a cell write"))
    @lock cell.lock begin
        if (@atomic cell.filled)
            throw(ArgumentError("respond!: result already settled as \
                                 $(cell.status); cannot settle twice"))
        end
        return _settle!(cell, status, payload)
    end
end

"""
    fill!(cell, status::SettlementStatus, payload) -> Bool

Fill the cell *only if still empty*; a no-op (returns `false`) once settled. This
is the non-authoritative path the framework uses — the return-value fallback and
the fail-safe wrapper — so a handler that already called [`respond!`](@ref) has
its return value ignored rather than triggering the double-settle error.

The empty-check and the write are one atomic section under the lock, so exactly
one of a return-value fill and a racing detached `respond!` delivers.
"""
function fill!(cell::ResultCell, status::SettlementStatus, payload)
    is_terminal(status) || return false
    @lock cell.lock begin
        (@atomic cell.filled) && return false
        return _settle!(cell, status, payload)
    end
end

# Deliver + latch, caller holds the lock and has checked emptiness. `deliver` runs
# before `filled` flips, so a `get_result` waiter waking on `wait_settled` never
# sees `filled=true` ahead of the cached result. The flip sits in a `finally` so a
# throwing `deliver` still latches (the half-delivered cell reads filled, the
# fail-safe `finally` leaves it alone, and the delivery error is the caller's to
# log). `notify` follows the flip so a woken waiter sees a fully-latched cell.
function _settle!(cell::ResultCell, status::SettlementStatus, payload)
    cell.status = status
    try
        cell.deliver(status, payload)
    finally
        @atomic cell.filled = true
        notify(cell.cond)
    end
    return true
end

# ── fail-safe settlement wrapper (the service/action fill-exactly-once invariant) ──

"""
    settle_handler!(cell, body; success_status, default_result, log_id=nothing)

Run a user handler `body()` and guarantee `cell` is filled exactly once before
the task is reaped. Collapses every handler exit into one settled cell:

| Handler exit                         | Cell empty ⇒ filled with        | Log              |
|--------------------------------------|---------------------------------|------------------|
| `return payload`                     | `success_status(payload)`       | —                |
| threw `Cancelled`                    | `canceled(default_result())`    | —                |
| threw anything else                  | `aborted` `(default_result())`  | `@error` + trace |
| returned/never responded, cell empty | `aborted` `(default_result())`  | `@error`         |
| cell already full                    | — (ignored)                     | —                |

`success_status` is the per-layer success token ([`success`](@ref) for a service,
[`succeeded`](@ref) for an action). `default_result` is a thunk producing the
typed payload for the synthesized error/cancel reply (the user result type's
zero/default). On the catch path it's wrapped so that a throwing `default_result`
can't defeat the guarantee — [`force_abort!`](@ref) is the infallible backstop.

Layered so the guarantee holds even if recovery itself fails: `try` settles the
normal exit, `catch` synthesizes the error/cancel outcome, and an infallible
`finally` force-aborts a still-empty cell.

Returns the cell's final [`outcome`](@ref).
"""
function settle_handler!(cell::ResultCell, body;
                         success_status::SettlementStatus,
                         default_result,
                         log_id = nothing)
    try
        v = body()
        # `respond!` is authoritative: only fill from the return value if the
        # handler didn't already settle. A detached handler hands settlement to a
        # spawned task, so its return value (typically `nothing`) is ignored — the
        # detach deadline/drain is the backstop, not the at-return fill.
        isfilled(cell) || (@atomic cell.detached) || fill!(cell, success_status, v)
    catch e
        if !isfilled(cell)
            status = e isa Cancelled ? canceled : aborted
            # default_result may itself throw (non-defaultable type / buggy
            # thunk); never let that defeat the fill — force_abort! covers it.
            try
                fill!(cell, status, default_result())
            catch fe
                # Sole log on the Cancelled path (the outer @error skips it):
                # without this a Cancelled goal force-aborts with no trace.
                @error "fail-safe settlement fill failed" exception=(fe, catch_backtrace()) status handle=log_id
            end
        end
        # Cancelled is the cooperative cancel signal, not an error; anything
        # else is a genuine handler failure worth surfacing with a backtrace.
        e isa Cancelled ||
            @error "handler failed; settled fail-safe" exception=(e, catch_backtrace()) handle=log_id
    finally
        # Infallible last resort: a non-defaultable result type or a delivery
        # error above can leave the cell empty — force a bare abort so the client
        # cannot hang. Must never throw. A detached cell skips this: ownership has
        # transferred to a spawned task, bounded instead by the detach deadline.
        isfilled(cell) || (@atomic cell.detached) || force_abort!(cell)
    end
    return outcome(cell)
end

"""
    force_abort!(cell::ResultCell) -> Bool

Infallible backstop: settle a still-empty cell as [`aborted`](@ref) without ever
constructing a user result. Used by [`settle_handler!`](@ref)'s `finally` when
even the synthesized default failed (a non-defaultable result type, a throwing
`default_result`, or a delivery error on the normal path).

It must not throw, so it swallows any delivery error. Two outcomes:

- latches the cell as aborted with a `nothing` payload, which the Service/Action
  `deliver` closure recognizes as "send the status enum + zero-filled result bytes
  directly, not via the message constructor".
- a delivery that still fails is logged, not raised — at that point the cell is
  latched and the client's own result timeout is the remaining guard.
"""
function force_abort!(cell::ResultCell)
    @lock cell.lock begin
        (@atomic cell.filled) && return false
        cell.status = aborted
        @atomic cell.filled = true
        notify(cell.cond)                    # wake waiters even on the backstop path
        try
            cell.deliver(aborted, nothing)   # nothing ⇒ zero-filled bytes, no ctor
        catch e
            @error "force_abort! delivery failed; cell latched, relying on \
                    client result timeout" exception=(e, catch_backtrace())
        end
        return true
    end
end

# ── waiting on settlement ────────────────────────────────────────────────────

"""
    wait_settled(cell::ResultCell, should_abandon; recheck=0.25) -> Bool

Block the calling task until `cell` is filled, returning `true` once it is. Parks
on the cell's settlement condition, so it wakes the *instant* `respond!`/`fill!`/
`force_abort!` latches the cell — no poll latency on the action `get_result` path.

`should_abandon()` is the cooperative bail-out, re-checked at most every `recheck`
seconds:

| `should_abandon()` | Effect                                        |
|--------------------|-----------------------------------------------|
| `true`             | abandon the wait; `wait_settled` returns `false` |
| `false`            | keep waiting                                  |

Pass `() -> is_shutdown(ctx)` so a drain can't wedge a request task here.
`Threads.Condition` has no timed wait, so we arm a one-shot `Base.Timer` each
iteration to re-notify ourselves — that bound also self-heals any missed `notify`.
A full cell takes precedence over abandonment, so a goal that settles as we
drain still replies its result.
"""
function wait_settled(cell::ResultCell, should_abandon; recheck::Real = 0.25)
    isfilled(cell) && return true
    @lock cell.cond begin
        while !(@atomic cell.filled)
            should_abandon() && return false
            t = Base.Timer(_ -> (@lock cell.cond notify(cell.cond)), recheck)
            try
                wait(cell.cond)
            finally
                close(t)
            end
        end
        return true
    end
end
