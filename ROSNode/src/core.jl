# Shared seams: the vocabulary every other component depends on. Included
# first, precompiles alone, dependency-light (ROSZenoh/Zenoh/Base only).
# Re-exports the wire-model identity types from ROSZenoh so callers spell
# `QosProfile`/`TypeInfo`/… as `ROSNode.X` without reaching through the stack.

# ── re-exports from the layer below ──────────────────────────────────────
# These are the ROSZenoh seams ROSNode's public surface speaks in directly:
# QoS policy, type identity, and the endpoint-kind tag (inferred from the
# message type at entity construction, §6). `EndpointKind`'s instances
# (Publisher/Subscription/Service/Client) come along so dispatch on kind reads
# naturally here too.
using ROSZenoh: QosProfile, TypeInfo, EndpointKind, default_qos,
                Publisher, Subscription, Service, Client
# Borrowed-payload escape error: the view path (§3.2) lets it surface to user
# code, so it belongs in ROSNode's exported vocabulary.
using Zenoh: BorrowError

export QosProfile, TypeInfo, EndpointKind, default_qos,
       Publisher, Subscription, Service, Client,
       BorrowError,
       ShutdownException, Cancelled,
       SettlementStatus, success, failure, failed,
       succeeded, canceled, aborted, feedback,
       Concurrency, Serial, Parallel

# ── exceptions ───────────────────────────────────────────────────────────

"""
    ShutdownException()

Raised into every blocked wait (clock sleeps, `call`, `wait_for_service`, graph
waits, …) when the owning Context drains (§14). Catching it is the cooperative
signal to unwind — it is not an error, so the drain path logs it as such.
"""
struct ShutdownException <: Exception end

Base.showerror(io::IO, ::ShutdownException) =
    print(io, "ShutdownException: context is shutting down")

"""
    Cancelled()

Thrown by the action cancellation checkpoints (`checkpoint(goal)` /
`feedback!`, §9) when a goal is transitioning to CANCELING. The framework's
fail-safe settlement maps it to a CANCELED result rather than ABORTED, so a
handler that lets it propagate settles cleanly without manual polling.
"""
struct Cancelled <: Exception end

Base.showerror(io::IO, ::Cancelled) =
    print(io, "Cancelled: goal was canceled")

# ── settlement status tokens ─────────────────────────────────────────────
# The write-once-cell verbs of services (§8) and actions (§9). They are *tags*
# passed to `respond!`/`fill!` to select an outcome — modeled as singletons of
# a sealed abstract type (the ClockSource §7 precedent) so dispatch on the
# token is a compile-time choice and an unknown token is a `MethodError`, not a
# silently-wrong branch.
#
#   service:  return resp                  ⇒ success
#             respond!(req, failure, msg)   ⇒ query error reply (client `call` raises)
#   action:   return result                ⇒ succeeded
#             respond!(goal, canceled, r)   ⇒ CANCELED
#             respond!(goal, aborted,  r)   ⇒ ABORTED
#             respond!(goal, feedback, fb)  ⇒ one feedback message (a stream, not the cell)
#
# `success`/`succeeded` and `failure`/`aborted` read differently at the two
# call sites by design (a service has no goal lifecycle); they are distinct
# tokens so a mis-paired use (e.g. `feedback` on a service) is a MethodError.

abstract type SettlementStatus end

"Service handler succeeded — the response is delivered as a reply-ok."
struct Success     <: SettlementStatus end
"Service handler failed — surfaces as a Zenoh query error reply so the client's `call` raises (§8)."
struct Failure     <: SettlementStatus end
"Action goal ran to completion — fills the result cell as SUCCEEDED (§9)."
struct Succeeded   <: SettlementStatus end
"Action goal was canceled — fills the result cell as CANCELED."
struct Canceled    <: SettlementStatus end
"Action goal aborted — fills the result cell as ABORTED (also the fail-safe outcome)."
struct Aborted     <: SettlementStatus end
"Action feedback — a stream verb, not a terminal cell write."
struct Feedback    <: SettlementStatus end

const success   = Success()
const failure   = Failure()
const succeeded = Succeeded()
const canceled  = Canceled()
const aborted   = Aborted()
const feedback  = Feedback()

# `failed` reads better than `failure` at the service `respond!(req, failed, …)`
# call site (DESIGN §Services); same token, two spellings.
const failed = failure

Base.show(io::IO, ::Success)   = print(io, "success")
Base.show(io::IO, ::Failure)   = print(io, "failure")
Base.show(io::IO, ::Succeeded) = print(io, "succeeded")
Base.show(io::IO, ::Canceled)  = print(io, "canceled")
Base.show(io::IO, ::Aborted)   = print(io, "aborted")
Base.show(io::IO, ::Feedback)  = print(io, "feedback")

# Terminal tokens fill the write-once cell; `feedback` is a stream and never
# does. Used by §8/§9 settlement to reject a `feedback` where a result is due.
is_terminal(::SettlementStatus) = true
is_terminal(::Feedback)         = false

# ── concurrency policy (§4, D2/D3) ───────────────────────────────────────
# How an endpoint dispatches its handlers, as a sealed type (the SettlementStatus
# / ClockSource precedent). **The default is single-threaded cooperative**:
# `Serial()` runs handlers on one *sticky* task, so they interleave only at yield
# points and never run simultaneously — node-local handler state needs no
# locks/atomics (rclcpp single-threaded-executor semantics, D3). A user opts a
# specific endpoint into OS-thread parallelism with `Parallel(n)`.

abstract type Concurrency end

"""
    Serial()

One sticky worker, order preserved — the default. Handlers run cooperatively on a
single OS thread (sticky, so they don't migrate even under `julia -t N`): never
simultaneous, so no data races and no locks needed; blocking calls yield, so a
blocking handler doesn't wedge the others.
"""
struct Serial <: Concurrency end

"""
    Parallel(n)

Up to `n` handlers in flight on OS threads (`Threads.@spawn`), order **not**
preserved. `n` caps in-flight handlers (and, with `view=true`, pinned payloads);
actual parallelism is `min(n, nthreads)`. `Parallel(Inf)` is unbounded spawn — an
explicit foot-gun, never a default. Opt in per endpoint once its handler is
thread-safe.
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
