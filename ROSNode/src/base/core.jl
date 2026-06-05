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
       Concurrency, Serial, Parallel,
       ViewMode, Owned, Checked, Unchecked,
       is_warming, @effectful

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

# ── view-borrow mode (only consulted when `view=true`) ───────────────────────
# Three points on the safety↔speed curve for how a `view` handler borrows the
# payload, as flag structs (dispatched on, like `Concurrency`) rather than a
# Bool/Symbol — so the choice is type-stable and self-describing at the call site.

"""
    ViewMode

How a subscription delivers each message to its handler — the single `view=` knob:

  - [`Owned`](@ref) (default) — materialize a fully-owned message; storable,
    forwardable, spawnable, no lifetime caveats.
  - [`Checked`](@ref) — zero-copy `CDRView` aliasing the payload, with a runtime
    escape guard (a view used after the handler returns throws `BorrowError`).
  - [`Unchecked`](@ref) — the same zero-copy view with the guard removed: fastest,
    zero-allocation, but an escaping view is undefined behaviour.

`Checked` and `Unchecked` borrow over the *same* representation, so `Checked`
validates exactly what `Unchecked` ships. `view=true`/`view=false` are accepted as
shorthand for `Checked()`/`Owned()`.
"""
abstract type ViewMode end

"""
    Owned()

Default delivery: the handler gets a fully-owned message (every field copied out
of the payload), so it can be stored, forwarded, or spawned with no lifetime
caveats (§3.1). `view=false` is shorthand for `Owned()`.
"""
struct Owned <: ViewMode end

"""
    Checked()

Zero-copy delivery with a runtime escape guard: the handler gets a `CDRView`
aliasing the payload, invalidated the instant it returns, so a `CDRView`/`CDRString`
that escaped throws `BorrowError` on next access instead of reading freed memory.
Allocates the guard (not zero-alloc), over the same representation [`Unchecked`](@ref)
ships — so it validates the exact code you'll run unchecked. `view=true` is
shorthand for `Checked()`. Run under `Checked()`, confirm no `BorrowError`, then
switch to `Unchecked()`.
"""
struct Checked <: ViewMode end

"""
    Unchecked()

The production fast path: zero-copy `CDRView` over a bare isbits `PayloadView` —
**zero-allocation**, no escape checking. An escaping `CDRView`/`CDRString` is
undefined behaviour; validate with [`Checked`](@ref) first.
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

# ── warm-up / precompilation (§D8) ─────────────────────────────────────────
# The dispatch chain is specialized on the message type, so the *first*
# `publish(::T)` / first handler-on-`::T` JITs the whole chain — a startup spike.
# D8 precompiles that chain at entity construction (warmup.jl). `is_warming()` +
# `@effectful` let user handlers stub their non-ROS effects during the `:execute`
# warm-up (and during package precompilation) *without* specializing or folding
# the wrong branch — it must be a runtime value flag, so both branches compile and
# only one executes.

"Runtime `:execute` warm-up scope. `true` only on a task the framework is warming."
const _WARMUP = Base.ScopedValues.ScopedValue(false)

"""
    is_warming() -> Bool

`true` while the framework is running a handler purely to warm its code path —
either D8 `:execute` warm-up (the `_WARMUP` scope) or package precompilation
(`jl_generating_output`). Lets handler code skip real side effects while still
*compiling* them. A runtime value (not a type/`Val`) on purpose: both branches of
a guard compile, only the live one runs — so warm-up exercises the stub yet still
codegens the real branch. Must stay non-foldable (no `@pure`/`:foldable`), else a
branch is DCE'd and the warm-up compiles the wrong specialization.
"""
@inline is_warming()::Bool =
    _WARMUP[] || (ccall(:jl_generating_output, Cint, ()) != 0)

"""
    @effectful expr

Run `expr` for its side effects only when *not* warming. While warming
(`is_warming()`), `expr` is skipped but still **compiled** (the guard is a runtime
value flag, so its branch codegens). Returns `expr`'s value at runtime, `nothing`
while warming. For a value the warm-up path itself needs, write
`is_warming() ? stub : real` directly instead.

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

"""
    WarmupPolicy(mode, sync)

Per-entity warm-up policy (§D8). `mode` is `:precompile` (default — a
side-effect-free `precompile`-anchor of the dispatch chain), `:execute` (run the
handler once on a sample message with side effects suppressed, reaching full
native depth), or `:off` (no warm-up). `sync=false` (default) warms on a
background task — zero construction latency; `sync=true` blocks the constructor
until warm (first message guaranteed compiled — hard real-time).
"""
struct WarmupPolicy
    mode::Symbol
    sync::Bool
    function WarmupPolicy(mode::Symbol, sync::Bool)
        mode in (:precompile, :execute, :off) ||
            throw(ArgumentError("warmup mode must be :precompile, :execute, or :off, got :$mode"))
        new(mode, sync)
    end
end

const _DEFAULT_WARMUP = WarmupPolicy(:precompile, false)
