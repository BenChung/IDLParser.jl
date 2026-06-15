# Shared seams: the vocabulary every other component depends on. Included
# first, precompiles alone, dependency-light (ROSZenoh/Zenoh/Base only).
# Re-exports the wire-model identity types from ROSZenoh so callers spell
# `QosProfile`/`TypeInfo`/… as `ROSNode.X` without reaching through the stack.

# ── re-exports from the layer below ──────────────────────────────────────
# These are the ROSZenoh seams ROSNode's public surface speaks in directly:
# QoS policy, type identity, and the endpoint-kind tag (inferred from the
# message type at entity construction). `EndpointKind`'s instances
# (Publisher/Subscription/Service/Client) come along so dispatch on kind reads
# naturally here too.
using ROSZenoh: QosProfile, TypeInfo, EndpointKind, default_qos,
                Publisher, Subscription, Service, Client
# Borrowed-payload escape error: the borrowed-view path lets it surface to user
# code, so it belongs in ROSNode's exported vocabulary.
using Zenoh: BorrowError

export QosProfile, TypeInfo, EndpointKind, default_qos,
       Publisher, Subscription, Service, Client,
       BorrowError,
       ShutdownException, Cancelled,
       SettlementStatus, success, failure, failed,
       succeeded, canceled, aborted, feedback,
       Concurrency, Serial, Parallel,
       ViewMode, Owned, Checked, Unchecked,
       MatchPolicy, ExactMatch, WeakMatch,
       WarmupPolicy, WarmupMode, Precompile, Execute, NoWarmup,
       is_warming, @effectful

# ── exceptions ───────────────────────────────────────────────────────────

"""
    ShutdownException()

Raised into every blocked wait — clock sleeps, `call`, `wait_for_service`, graph
waits — when the owning [`Context`](@ref) begins to drain. Catching it is the
cooperative signal to unwind that wait cleanly and let the Context tear down; the
drain path treats it as an expected shutdown and logs it as such. A subscription
dispatch loop that sees it returns quietly, treating it as teardown rather than a
handler failure.

ROSNode's analogue of the rclpy/rclcpp shutdown signal that interrupts blocking
calls once `rclpy.shutdown()` / context invalidation occurs.
"""
struct ShutdownException <: Exception end

Base.showerror(io::IO, ::ShutdownException) =
    print(io, "ShutdownException: context is shutting down")

"""
    Cancelled()

Thrown by the action cancellation checkpoints — `checkpoint(goal)` and
`feedback!(goal, …)` — when the goal has moved to the `CANCELING` action goal
state. A goal handler that lets it propagate settles cleanly through the
fail-safe settlement:

  - `Cancelled` — fills the result cell with `canceled(default_result())`, a
    `CANCELED` result.
  - any other exception — maps to `ABORTED`.

Cancellation then unwinds structurally through the handler's call
stack, with no manual status polling.

Models the ROS 2 action goal-state path `{ACCEPTED, EXECUTING}` → `CANCELING` →
`CANCELED`.
"""
struct Cancelled <: Exception end

Base.showerror(io::IO, ::Cancelled) =
    print(io, "Cancelled: goal was canceled")

# ── settlement status tokens ─────────────────────────────────────────────
# Tokens passed to `respond!` to select an outcome, as singletons of a sealed
# abstract type (the clock-source pattern). `respond!` dispatches on the abstract
# type and uses `is_terminal` to reject `feedback` where a result is due.
#
#   service:  return resp                  ⇒ success
#             respond!(req, failure, msg)   ⇒ query error reply (client `call` raises)
#   action:   return result                ⇒ succeeded
#             respond!(goal, canceled, r)   ⇒ CANCELED
#             respond!(goal, aborted,  r)   ⇒ ABORTED
#             respond!(goal, feedback, fb)  ⇒ one feedback message (a stream, not the cell)
#
# The distinct service (`success`/`failure`) and action (`succeeded`/`aborted`)
# spellings keep each call site self-describing.

"""
    SettlementStatus

Sealed abstract supertype of the outcome tokens passed to `respond!` to settle a
service request or an action goal. The singleton instances split by call site:

| Token | Call site | Terminal? |
|-------|-----------|-----------|
| [`success`](@ref) | service | yes |
| [`failure`](@ref) (alias [`failed`](@ref)) | service | yes |
| [`succeeded`](@ref) | action | yes |
| [`canceled`](@ref) | action | yes |
| [`aborted`](@ref) | action | yes |
| [`feedback`](@ref) | action | no — a stream verb, not a terminal cell write |

`respond!` dispatches on this abstract type, so it accepts any token; the
non-terminal [`feedback`](@ref) passed where a result is due raises an
`ArgumentError` — only terminal tokens fill the result cell. The distinct service and action
spellings document each call site rather than enforce a boundary.
"""
abstract type SettlementStatus end

"Service handler succeeded — the response is delivered as a reply-ok."
struct Success     <: SettlementStatus end
"Service handler failed — surfaces as a Zenoh query error reply so the client's `call` raises."
struct Failure     <: SettlementStatus end
"Action goal ran to completion — fills the result cell as SUCCEEDED."
struct Succeeded   <: SettlementStatus end
"Action goal was canceled — fills the result cell as CANCELED."
struct Canceled    <: SettlementStatus end
"Action goal aborted — fills the result cell as ABORTED (also the fail-safe outcome)."
struct Aborted     <: SettlementStatus end
"Action feedback — a stream verb, not a terminal cell write."
struct Feedback    <: SettlementStatus end

"""
    success :: Success

Service-handler success token. `respond!(req, success, resp)` settles the
in-flight request by delivering `resp` as a Zenoh reply-ok, the normal successful
path of a ROS 2 service. Returning a response value from the handler reaches the
same outcome — `settle_handler!` fills the result cell with `success` when the
handler returns without having already settled. A terminal token (fills the
write-once result cell).
"""
const success   = Success()

"""
    failure :: Failure

Service-handler failure token (spelled [`failed`](@ref) at some call sites — the
same singleton). `respond!(req, failure, msg)` settles the request as a Zenoh
query *error* reply carrying `msg`, so the client's `call` raises `ServiceError`.
A terminal token. The service counterpart of an action goal's [`aborted`](@ref),
kept a distinct spelling so the service and action call sites read for their own
lifecycle.
"""
const failure   = Failure()

"""
    succeeded :: Succeeded

Action goal success token. `respond!(goal, succeeded, result)` — or returning the
result from the goal handler — fills the write-once result cell as the SUCCEEDED
action goal state, completing the goal normally. A terminal token. The action
counterpart of a service's [`success`](@ref).
"""
const succeeded = Succeeded()

"""
    canceled :: Canceled

Action goal cancellation token. `respond!(goal, canceled, result)` fills the
result cell as the CANCELED action goal state. The fail-safe settlement also
synthesizes this outcome when a goal handler unwinds via a thrown
[`Cancelled`](@ref). A terminal token.
"""
const canceled  = Canceled()

"""
    aborted :: Aborted

Action goal abort token. `respond!(goal, aborted, result)` fills the result cell
as the ABORTED action goal state, the explicit failure verb for a goal. Also the
fail-safe outcome: a goal handler that throws any exception other than
[`Cancelled`](@ref), or that returns leaving the result cell empty, is settled as
`aborted` so the client never hangs. A terminal token.
"""
const aborted   = Aborted()

"""
    feedback :: Feedback

Action feedback token — a stream verb. `respond!(goal, feedback, fb)` publishes
one feedback message on the goal's feedback topic (the sugar behind
`feedback!(goal, fb)`) and leaves the goal running. It never fills the result
cell, so the settlement layer rejects it where a terminal result is due
(feedback is not a terminal token).

Mirrors ROS 2 action feedback, the progress stream published while a goal
executes.
"""
const feedback  = Feedback()

"""
    failed :: Failure

Alias for [`failure`](@ref) — the identical singleton, spelled to read naturally
at the service call site `respond!(req, failed, msg)`. Settles the request as a
Zenoh query error reply, so the client's `call` raises `ServiceError`. A terminal
token.
"""
const failed = failure

Base.show(io::IO, ::Success)   = print(io, "success")
Base.show(io::IO, ::Failure)   = print(io, "failure")
Base.show(io::IO, ::Succeeded) = print(io, "succeeded")
Base.show(io::IO, ::Canceled)  = print(io, "canceled")
Base.show(io::IO, ::Aborted)   = print(io, "aborted")
Base.show(io::IO, ::Feedback)  = print(io, "feedback")

# Terminal tokens fill the write-once result cell; `feedback` is a stream verb.
# Service and action settlement uses this to reject `feedback` where a result is due.
is_terminal(::SettlementStatus) = true
is_terminal(::Feedback)         = false

# ── concurrency policy ───────────────────────────────────────────────────
# How an endpoint dispatches its handlers, as a sealed type (the SettlementStatus
# / ClockSource precedent). The default `Serial()` runs handlers on one sticky
# task: they interleave only at yield points, so node-local handler state needs no
# locks/atomics (rclcpp single-threaded-executor semantics). `Parallel(n)` opts a
# specific endpoint into OS-thread parallelism.

"""
    Concurrency

Sealed abstract supertype of the per-endpoint handler-dispatch policy:
[`Serial`](@ref) (one sticky cooperative task, the default) or [`Parallel`](@ref)
(up to `n` handlers on OS threads). Passed as the `concurrency=` keyword when
declaring a subscription (or other handler endpoint). Mirrors the rclcpp executor
choice between a single-threaded and a multi-threaded executor, scoped to a single
endpoint.

See https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Executors.html
"""
abstract type Concurrency end

"""
    Serial()

The default concurrency policy: one sticky worker task runs the endpoint's
handlers one at a time, in arrival order, on a single OS thread (sticky, so it
stays put even under `julia -t N`). Handlers run one at a time, so node-local
handler state needs no locks or atomics; a blocking call inside a handler yields,
letting the others make progress. The single-threaded-executor model from rclcpp.
"""
struct Serial <: Concurrency end

"""
    Parallel(n)

Dispatch up to `n` handlers concurrently on OS threads; completion order is
independent of arrival order. `n` persistent non-sticky worker tasks pull from the
subscriber, so `n` caps the in-flight handlers (and, under [`Checked`](@ref) /
[`Unchecked`](@ref) views, the pinned payloads); actual parallelism is bounded by
`min(n, Threads.nthreads())`. Opt a specific endpoint into this once its handler is
thread-safe — the multi-threaded-executor model from rclcpp.

`Parallel(Inf)` spawns one task per message with no cap — an explicit foot-gun,
never a default. `n` must be `>= 1`; `Parallel(n)` with `n < 1` throws
`ArgumentError`.

```julia
Subscription(node, "/scan", LaserScan; concurrency=Parallel(4)) do msg
    process(msg)   # up to 4 of these run at once, order independent of arrival
end
```
"""
struct Parallel <: Concurrency
    n::Float64
    function Parallel(n::Real)
        n >= 1 || throw(ArgumentError("Parallel(n): n ≥ 1 required, got $n"))
        new(Float64(n))
    end
end

Base.show(io::IO, ::Serial)    = print(io, "Serial()")
Base.show(io::IO, c::Parallel) = print(io, "Parallel(", isfinite(c.n) ? Int(c.n) : "Inf", ")")

# ── view-borrow mode (consulted when `view=true`) ────────────────────────────
# Three points on the safety↔speed curve for how a `view` handler borrows the
# payload, as flag structs dispatched on (like `Concurrency`) so the choice is
# type-stable and self-describing at the call site.

"""
    ViewMode

Sealed abstract supertype controlling how a subscription delivers each message to
its handler — the single `view=` knob, on a safety-versus-speed curve:

  - [`Owned`](@ref) (default; `view=false`) — a fully-owned message: storable,
    forwardable, and safe to spawn beyond the handler's return, with no lifetime
    caveats.
  - [`Checked`](@ref) (`view=true`) — a zero-copy `CDRView` aliasing the payload,
    with a runtime escape guard: a view used after the handler returns throws
    `BorrowError`.
  - [`Unchecked`](@ref) — the same zero-copy view with the guard removed: fastest
    and zero-allocation; an escaping view is undefined behavior.

`Checked` and `Unchecked` borrow over the *same* representation, so validating
under `Checked` exercises exactly what `Unchecked` runs.
"""
abstract type ViewMode end

"""
    Owned()

Default [`ViewMode`](@ref): the handler receives a fully-owned message decoded out
of the payload, with every field materialized. The message is free to
store, forward, or pass to a spawned task beyond the handler's return — no borrow
lifetime applies. `view=false` is shorthand for `Owned()`.
"""
struct Owned <: ViewMode end

"""
    Checked()

Zero-copy [`ViewMode`](@ref) with a runtime escape guard: the handler receives a
`CDRView` aliasing the payload, invalidated the instant the handler returns. A
`CDRView` or `CDRString` that escaped the handler throws `BorrowError` on
its next access. The guard costs one allocation and borrows over the same
representation [`Unchecked`](@ref) ships, so it validates exactly the code the
unchecked path will run. `view=true` is shorthand for `Checked()`. The intended
workflow: run under `Checked()`, confirm no `BorrowError`, then switch to
`Unchecked()`.
"""
struct Checked <: ViewMode end

"""
    Unchecked()

The production fast-path [`ViewMode`](@ref): a zero-copy `CDRView` over a bare
isbits `PayloadView`, zero-allocation and with no escape checking. A `CDRView` or
`CDRString` that outlives the handler is undefined behavior — it reads freed
memory. Validate the same handler under [`Checked`](@ref) first; once it shows no
`BorrowError`, switch to `Unchecked()` for the zero-alloc tier.
"""
struct Unchecked <: ViewMode end

Base.show(io::IO, ::Owned)     = print(io, "Owned()")
Base.show(io::IO, ::Checked)   = print(io, "Checked()")
Base.show(io::IO, ::Unchecked) = print(io, "Unchecked()")

# `view=` accepts the `ViewMode` structs or the Bool shorthand.
_view_mode(v::ViewMode) = v
_view_mode(v::Bool) = v ? Checked() : Owned()
# Whether a mode delivers a borrowed view (vs an owned message) — drives the
# holder-lifetime choice in the consumers.
_is_view(::Owned) = false
_is_view(::ViewMode) = true

# ── subscription match policy (the `match=` knob) ────────────────────────────

"""
    MatchPolicy

Sealed abstract supertype for how a subscription admits publishers of a topic — the
`match=` knob:

  - [`ExactMatch`](@ref) (default; `:exact`) — admit only publishers whose type hash
    matches the subscription's exactly.
  - [`WeakMatch`](@ref) (`:weak`) — also admit a publisher advertising the topic
    under a different (or absent) type hash, decoding best-effort.
"""
abstract type MatchPolicy end

"""
    ExactMatch()

Default [`MatchPolicy`](@ref): admit only publishers whose type hash matches the
subscription's. `:exact` is shorthand for `ExactMatch()`.
"""
struct ExactMatch <: MatchPolicy end

"""
    WeakMatch()

[`MatchPolicy`](@ref) that also admits a publisher advertising the topic under a
different or absent type hash, decoding best-effort. `:weak` is shorthand for
`WeakMatch()`.
"""
struct WeakMatch <: MatchPolicy end

Base.show(io::IO, ::ExactMatch) = print(io, "ExactMatch()")
Base.show(io::IO, ::WeakMatch)  = print(io, "WeakMatch()")

# `match=` accepts the MatchPolicy structs or the Symbol shorthand.
_match_policy(m::MatchPolicy) = m
function _match_policy(m::Symbol)
    m === :exact && return ExactMatch()
    m === :weak  && return WeakMatch()
    throw(ArgumentError("Subscription `match` must be :exact or :weak (or a `MatchPolicy`), got :$m"))
end
_is_weak(m::MatchPolicy) = m isa WeakMatch

# ── small shared helpers ─────────────────────────────────────────────────

"""
    @unreachable(msg)

Marks a branch the type system should have excluded; throws if ever hit. Keeps
exhaustive `if`/`elseif` chains over the status tokens honest without a silent
fallthrough.
"""
macro unreachable(msg)
    :(error(string("unreachable: ", $(esc(msg)))))
end

# ── warm-up / precompilation ───────────────────────────────────────────────
# The dispatch chain specializes on the message type, so the first `publish(::T)`
# or first handler-on-`::T` JITs the whole chain — a startup spike. The warm-up
# pass precompiles that chain at entity construction. `is_warming()` + `@effectful`
# let a handler stub its non-ROS effects during `:execute` warm-up (and package
# precompilation) while both branches still compile: the flag is a runtime value,
# so the live branch codegens even though the stub runs.

"Runtime `:execute` warm-up scope. `true` only on a task the framework is warming."
const _WARMUP = Base.ScopedValues.ScopedValue(false)

"""
    is_warming() -> Bool

`true` while the framework is running a handler purely to warm its code path —
either `:execute` warm-up (the `_WARMUP` scope) or package precompilation
(`jl_generating_output`). Lets handler code skip real side effects while the
framework still *compiles* the handler at full native depth. Use
[`@effectful`](@ref) for the common skip-this-effect-while-warming pattern.

Returns a plain runtime `Bool` by design: both branches of a guard compile, and
only the live one runs, so warm-up exercises the stub yet still codegens the real
branch. Must stay non-foldable (no `@pure`/`:foldable`), or a branch is DCE'd and
warm-up compiles the wrong specialization.
"""
@inline is_warming()::Bool =
    _WARMUP[] || (ccall(:jl_generating_output, Cint, ()) != 0)

"""
    @effectful expr

Run `expr` for its side effects on the live path, skipping it while the framework
is warming the handler ([`is_warming`](@ref)). The skipped branch still
**compiles** — the guard is a runtime value, so both arms codegen — so warm-up
exercises the exact specialization the real run will use. Returns `expr`'s value
at runtime, `nothing` while warming. When the warm-up path itself needs a usable
value, write `is_warming() ? stub : real` directly.

Wrap a non-ROS effect (hardware I/O, external calls) so it runs only on the live
path; ROS effects like `publish`/`call` are null-routed by the framework during
warm-up and need no wrapping.

    Subscription(node, "/cmd", Twist) do msg
        cmd = plan(msg)                  # compiled either way
        @effectful arm_thrusters!(cmd)   # non-ROS effect: skipped while warming, still compiled
        publish(throttle_pub, derive(cmd))   # ROS effect: framework null-routes during warm-up
    end
"""
macro effectful(expr)
    quote
        ROSNode.is_warming() ? nothing : $(esc(expr))
    end
end

# ── warm-up mode (the `mode` of a `WarmupPolicy`) ────────────────────────────
# Three depths, as flag structs dispatched on (like `ViewMode`/`Concurrency`) so the
# choice is type-stable and self-describing; `:precompile`/`:execute`/`:off` stay
# accepted as shorthands.

"""
    WarmupMode

Sealed abstract supertype selecting how an entity's dispatch chain is warmed — the
`mode` of a [`WarmupPolicy`](@ref), on a depth-versus-cost curve:

  - [`Precompile`](@ref) (default; `:precompile`) — a side-effect-free
    `precompile`-anchor of the dispatch chain.
  - [`Execute`](@ref) (`:execute`) — additionally run the handler once on a sample
    message at full native depth, side effects suppressed.
  - [`NoWarmup`](@ref) (`:off`) — no warm-up.
"""
abstract type WarmupMode end

"""
    Precompile()

Default [`WarmupMode`](@ref): a side-effect-free `precompile`-anchor of the dispatch
chain — caches the inference tree and codegens the named frame and its inlined
callees, needing no message instance. `:precompile` is shorthand for `Precompile()`.
"""
struct Precompile <: WarmupMode end

"""
    Execute()

[`WarmupMode`](@ref) that additionally runs the handler once on a synthesized sample
message under the warm-up scope ([`is_warming`](@ref) true), reaching full native
depth including the handler body; outbound ROS effects null-route and
[`@effectful`](@ref) blocks skip. `:execute` is shorthand for `Execute()`.
"""
struct Execute <: WarmupMode end

"""
    NoWarmup()

[`WarmupMode`](@ref) that disables warm-up: the first message JITs the dispatch
chain. `:off` is shorthand for `NoWarmup()`.
"""
struct NoWarmup <: WarmupMode end

Base.show(io::IO, ::Precompile) = print(io, "Precompile()")
Base.show(io::IO, ::Execute)    = print(io, "Execute()")
Base.show(io::IO, ::NoWarmup)   = print(io, "NoWarmup()")

# `mode`/`warmup=` accept the WarmupMode structs or the Symbol shorthand.
_warmup_mode(m::WarmupMode) = m
function _warmup_mode(m::Symbol)
    m === :precompile && return Precompile()
    m === :execute    && return Execute()
    m === :off        && return NoWarmup()
    throw(ArgumentError("warmup mode must be :precompile, :execute, or :off (or a `WarmupMode`), got :$m"))
end

"""
    WarmupPolicy(mode, sync)

Per-entity warm-up policy. `mode` is a [`WarmupMode`](@ref) (or its `:precompile` /
`:execute` / `:off` shorthand) selecting how the dispatch chain is warmed; `sync`
selects when:

  - `false` (default) — warms on a background task; zero construction latency.
  - `true` — blocks the constructor until warm; first message guaranteed compiled
    (hard real-time).
"""
struct WarmupPolicy
    mode::WarmupMode
    sync::Bool
end
WarmupPolicy(mode, sync::Bool) = WarmupPolicy(_warmup_mode(mode), sync)

const _DEFAULT_WARMUP = WarmupPolicy(Precompile(), false)
