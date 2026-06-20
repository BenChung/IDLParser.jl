# Time & clocks, mirroring rclcpp's three-clock model. The clock source is a
# singleton-type discriminator that doubles as the `RTime` tag, so combining two
# clocks is a compile-time MethodError.
#
# Depends only on `Dates` + Base. The Node type is included later, so node
# accessors are duck-typed (`::Any`) against the node's clock surface (see
# `_node_clock`). Sim time (`/clock`, `use_sim_time`) is a Context-level concern;
# the ROS-clock paths read system time and carry TODO(graph) markers for the
# eventual `/clock` wiring.

using Dates: Dates, Period, Nanosecond

# ── clock sources ─────────────────────────────────────────────────────────
# Singleton types give type-stable selection, and the same type tags `RTime{C}`.

"""
    ClockSource

Sealed tag for the three rclcpp clocks. Instances (`System()`, `Steady()`,
`ROS()`) select a clock at dispatch time and tag the `RTime` they produce.
"""
abstract type ClockSource end

"Wall clock (`CLOCK_REALTIME`); can jump (NTP). For human-facing timestamps."
struct System <: ClockSource end
"Monotonic clock (`CLOCK_MONOTONIC`); never goes backward. For durations/timeouts."
struct Steady <: ClockSource end
"""
The ROS clock: system time by default, or simulated time driven by `/clock`
when a node sets `use_sim_time`. Backs `now(node)` and message stamps.
"""
struct ROS <: ClockSource end

# ── value types ─────────────────────────────────────────────────────────────

"""
    RTime{C<:ClockSource}

A clock-tagged instant: an `Int64` nanosecond count (rclcpp's
`rcl_time_point_value_t`). The tag `C` makes cross-clock arithmetic a
`MethodError` (`RTime{Steady} - RTime{System}` has no method), enforcing rclcpp's
"clock types differ" rule at dispatch time. This is the in-process value type,
distinct from the wire `builtin_interfaces/Time` struct; convert with
`to_msg`/`rtime`. The name avoids the `Dates.Time` clash.

See https://design.ros2.org/articles/clock_and_time.html
"""
struct RTime{C<:ClockSource}
    ns::Int64
end

"The `ClockSource` instance tagging this instant."
source(::RTime{C}) where {C} = C()
"Nanoseconds since the clock's epoch."
nanoseconds(t::RTime) = t.ns

"""
    Duration(ns)

A clock-agnostic span in nanoseconds (signed `Int64`; differencing two instants
may be negative). rclcpp's `rcl_duration_value_t`, untagged so a span measured on
one clock can be applied to another. Distinct from `ROSZenoh.Duration`, the
unsigned QoS-policy type.
"""
struct Duration
    ns::Int64
end
"Construct from a `Dates.Period` (`Duration(Dates.Millisecond(20))`)."
Duration(p::Period) = Duration(Dates.tons(p))

"Nanoseconds in this span."
nanoseconds(d::Duration) = d.ns
"Seconds (lossy `Float64`) — convenience for human-facing logging/rates."
seconds(d::Duration) = d.ns / 1e9

# Anything period-like (an `RTime` delta or a `Dates.Period`) coerces to ns.
_to_ns(d::Duration) = d.ns
_to_ns(p::Period)   = Dates.tons(p)
_to_ns(n::Integer)  = Int64(n)

# ── arithmetic (within a clock only) ────────────────────────────────────────
# Only same-tag instants combine; there is no `RTime{A} ⊕ RTime{B}` method, so
# mixing clocks is a dispatch error.

# instant − instant → span
Base.:-(a::RTime{C}, b::RTime{C}) where {C} = Duration(a.ns - b.ns)

# instant ± span → instant (span may be a `Duration` or any `Dates.Period`)
Base.:+(t::RTime{C}, d::Duration) where {C} = RTime{C}(t.ns + d.ns)
Base.:-(t::RTime{C}, d::Duration) where {C} = RTime{C}(t.ns - d.ns)
Base.:+(d::Duration, t::RTime{C}) where {C} = t + d
Base.:+(t::RTime{C}, p::Period) where {C}   = RTime{C}(t.ns + Dates.tons(p))
Base.:-(t::RTime{C}, p::Period) where {C}   = RTime{C}(t.ns - Dates.tons(p))
Base.:+(p::Period, t::RTime{C}) where {C}   = t + p

# span algebra (clock-agnostic)
Base.:+(a::Duration, b::Duration) = Duration(a.ns + b.ns)
Base.:-(a::Duration, b::Duration) = Duration(a.ns - b.ns)
Base.:-(d::Duration)              = Duration(-d.ns)
Base.:*(d::Duration, k::Real)     = Duration(round(Int64, d.ns * k))
Base.:*(k::Real, d::Duration)     = d * k
Base.:/(d::Duration, k::Real)     = Duration(round(Int64, d.ns / k))
Base.:+(a::Duration, p::Period)   = Duration(a.ns + Dates.tons(p))
Base.:-(a::Duration, p::Period)   = Duration(a.ns - Dates.tons(p))

# comparisons — same clock for instants, free for spans
Base.:(==)(a::RTime{C}, b::RTime{C}) where {C} = a.ns == b.ns
Base.isless(a::RTime{C}, b::RTime{C}) where {C} = a.ns < b.ns
Base.:(==)(a::Duration, b::Duration)  = a.ns == b.ns
Base.isless(a::Duration, b::Duration) = a.ns < b.ns
Base.:(==)(a::Duration, p::Period)  = a.ns == Dates.tons(p)
Base.isless(a::Duration, p::Period) = a.ns < Dates.tons(p)
Base.isless(p::Period, a::Duration) = Dates.tons(p) < a.ns

Base.zero(::Type{Duration}) = Duration(0)
Base.zero(::Duration)       = Duration(0)
Base.hash(t::RTime{C}, h::UInt) where {C} = hash(C, hash(t.ns, h))
Base.hash(d::Duration, h::UInt) = hash(d.ns, hash(:RNDuration, h))

# render spans through Dates' canonical breakdown so logs read naturally
Base.show(io::IO, d::Duration) =
    print(io, "Duration(", Dates.canonicalize(Dates.CompoundPeriod(Nanosecond(d.ns))), ")")
Base.show(io::IO, t::RTime{C}) where {C} = print(io, "RTime{", nameof(C), "}(", t.ns, "ns)")

# ── wire convert: ↔ builtin_interfaces/Time & /Duration ─────────────────────
# The generated message structs (`builtin_interfaces.msg.Time`/`.Duration`,
# fields `sec::Int32` + `nanosec::UInt32`) come from ROSMessages and aren't in
# scope here. Convert duck-typed via field access so `Header(stamp = now(node))`
# works through the generated kw-ctor's `convert` without a static dependency.

const _NS_PER_S = 1_000_000_000

# split signed ns into ROS's (sec::Int32, nanosec∈[0,1e9)::UInt32) form
function _sec_nanosec(ns::Integer)
    s, n = divrem(Int64(ns), _NS_PER_S)
    if n < 0           # ROS keeps nanosec non-negative; borrow a second
        s -= 1
        n += _NS_PER_S
    end
    (Int32(s), UInt32(n))
end

_join_ns(sec::Integer, nanosec::Integer) = Int64(sec) * _NS_PER_S + Int64(nanosec)

"""
    to_msg(T, t::RTime)  /  to_msg(T, d::Duration)

Build a wire `builtin_interfaces` `Time`/`Duration` message of type `T` from a
clock instant or span. `T` is the generated kw-struct (passed in so this file
needn't depend on ROSMessages); it must accept `sec`/`nanosec` keywords.
"""
function to_msg(::Type{T}, t::RTime) where {T}
    sec, nanosec = _sec_nanosec(t.ns)
    T(; sec, nanosec)
end
function to_msg(::Type{T}, d::Duration) where {T}
    sec, nanosec = _sec_nanosec(d.ns)
    T(; sec, nanosec)
end

"""
    rtime(C, msg)  /  duration(msg)

Read a `builtin_interfaces/Time` (→ `RTime{C}`) or `/Duration` (→ `Duration`)
message back into the value types. `msg` is duck-typed on `sec`/`nanosec`.
"""
rtime(::Type{C}, msg) where {C<:ClockSource} = RTime{C}(_join_ns(msg.sec, msg.nanosec))
rtime(::C, msg) where {C<:ClockSource}       = rtime(C, msg)
duration(msg) = Duration(_join_ns(msg.sec, msg.nanosec))

# `header.stamp = now(node)` converts at the boundary via the generated kw-ctor's
# `convert(FieldType, value)`. The `Base.convert` methods belong with the
# concrete generated `builtin_interfaces` `Time`/`Duration` structs (owned by the
# message-integration layer): an unconstrained `convert(::Type{T}, ::RTime)`
# would claim every target and collide with Base methods for `Any`/`Nothing`/etc.
#
# TODO(messages): register the narrow methods against the concrete types, e.g.
#   Base.convert(::Type{builtin_interfaces.msg.Time}, t::RTime)        = to_msg(builtin_interfaces.msg.Time, t)
#   Base.convert(::Type{builtin_interfaces.msg.Duration}, d::Duration) = to_msg(builtin_interfaces.msg.Duration, d)
# Meanwhile callers use `to_msg(T, …)` / `rtime` / `duration` explicitly.

# ── jump callbacks ──────────────────────────────────────────────────────────
# Let timers recompute their next fire across a clock discontinuity (sim
# activate/deactivate, NTP step, runtime `use_sim_time` toggle).

"Why a clock jumped — handed to a `JumpCallback`."
@enum JumpKind sim_activated sim_deactivated time_forward time_backward

"""
    TimeJump(kind, delta)

A clock discontinuity event: `kind::JumpKind` and the `delta::Duration` applied
(zero for a pure source switch).
"""
struct TimeJump
    kind::JumpKind
    delta::Duration
end

"""
    JumpCallback(f; min_forward=nothing, min_backward=nothing, on_clock_change=true)

A registerable jump handler. `f(::TimeJump)` fires on any of:

- `min_forward` — the clock leaps forward by ≥ this threshold (`Duration`/`Period`/`nothing`).
- `min_backward` — the clock leaps backward by ≥ this threshold (`Duration`/`Period`/`nothing`).
- `on_clock_change` — a sim activate/deactivate, when `true`.
"""
struct JumpCallback
    f::Function
    min_forward::Union{Duration,Nothing}
    min_backward::Union{Duration,Nothing}
    on_clock_change::Bool
end

function JumpCallback(f::Function; min_forward=nothing, min_backward=nothing,
                      on_clock_change::Bool=true)
    JumpCallback(f,
        min_forward  === nothing ? nothing : Duration(_to_ns(min_forward)),
        min_backward === nothing ? nothing : Duration(_to_ns(min_backward)),
        on_clock_change)
end

# whether a jump passes a callback's thresholds (used by the jump dispatch)
function _triggers(cb::JumpCallback, j::TimeJump)
    (cb.on_clock_change && (j.kind === sim_activated || j.kind === sim_deactivated)) && return true
    if j.delta.ns >= 0
        return cb.min_forward !== nothing && j.delta.ns >= cb.min_forward.ns
    else
        return cb.min_backward !== nothing && -j.delta.ns >= cb.min_backward.ns
    end
end

# ── Clock{C}: a holdable handle bound to a node ─────────────────────────────
# Holds the node (for sim-time routing + shutdown interruption) and the source
# tag. Returned by `clock(node, …)` so callers can keep one for sleeps and jump
# registration without re-deriving it each `now`.

"""
    Clock{C<:ClockSource}

A node-bound clock handle. Read it with `now(clock)`, wait on it with
`sleep`/`sleep_until`, and register `JumpCallback`s against it. Obtain via
`clock(node)` / `clock(node, Steady())`.
"""
struct Clock{C<:ClockSource}
    node::Any              # duck-typed; the Node type lands later
    source::C
    jumps::Vector{JumpCallback}
end

Clock(node, src::C) where {C<:ClockSource} = Clock{C}(node, src, JumpCallback[])

source(::Clock{C}) where {C} = C()
Base.show(io::IO, ::Clock{C}) where {C} = print(io, "Clock{", nameof(C), "}")

# Duck-typed node→clock accessor, defined permissively so `now(node)` compiles
# against any node shape the later layers settle on.
# TODO(graph): route through `node.clock(C())` once Node exists, so the ROS
# clock can pick up the node's `use_sim_time` + Context `/clock` source.
function _node_clock(node, src::C) where {C<:ClockSource}
    # One stable handle per (node, source), cached in `node.clocks`, so that
    # `register!(clock(node, ROS()), cb)`, `now(node, ROS())`, and the sim-time jump
    # dispatch all reach the same `Clock{C}`. A context-less mock (tests) has no
    # `clocks` table, so synthesize a fresh handle for the caller to hold.
    if hasproperty(node, :clocks)
        return get!(() -> Clock(node, src), node.clocks, C)::Clock{C}
    end
    Clock(node, src)
end

"""
    clock(node) -> Clock{ROS}
    clock(node, source::ClockSource) -> Clock{C}

The node's clock for `source` (default `ROS()`), suitable to hold for repeated
reads, sleeps, and jump-callback registration.
"""
clock(node) = clock(node, ROS())
clock(node, src::ClockSource) = _node_clock(node, src)

"""
    register!(clock, cb::JumpCallback) -> JumpCallback

Register a jump handler on `clock`. Returns `cb` so it can later be removed.
"""
function register!(c::Clock, cb::JumpCallback)
    push!(c.jumps, cb)
    cb
end
register!(f::Function, c::Clock; kwargs...) = register!(c, JumpCallback(f; kwargs...))

# ── now() ───────────────────────────────────────────────────────────────────
# Raw clock reads. Steady uses Julia's monotonic `time_ns`; System uses wall
# `time()`. The ROS clock's sim-time routing is in `_read_ns(::Clock{ROS})` below.

_read_ns(::Steady) = Int64(Base.time_ns() & typemax(Int64))   # monotonic, ns
_read_ns(::System) = round(Int64, time() * 1e9)               # wall, ns since unix epoch

# ROS clock: sim time from the owning Context's `/clock` source when this clock's
# node opted into `use_sim_time`, else system time. The common (non-sim) path is
# lock-free: `sim_time_ns === nothing` short-circuits before the per-node opt-in check
# (which takes `_sim_lock`). The Context's own ROS clock (`c.node === ctx`) is never a
# sim user, so it reads system time.
function _read_ns(c::Clock{ROS})
    ctx = _clock_ctx(c.node)
    ctx === nothing && return _read_ns(System())
    s = @atomic ctx.sim_time_ns
    s === nothing && return _read_ns(System())          # sim inactive (common) — lock-free
    (c.node !== ctx && _is_sim_user(ctx, c.node)) ? s : _read_ns(System())
end
# A bare `ROS()` source (no node/Context to reach) is system time.
_read_ns(::ROS) = _read_ns(System())

"""
    now(clock::Clock{C}) -> RTime{C}

Read a held clock. For `Clock{ROS}` this honors the node's sim-time routing: the
Context's `/clock`-driven time when the node opted into `use_sim_time`, else system
time.
"""
Dates.now(c::Clock{C}) where {C} =
    C === ROS ? RTime{ROS}(_read_ns(c)) : RTime{C}(_read_ns(C()))

"""
    now(node) -> RTime{ROS}
    now(node, source::ClockSource) -> RTime{C}

The common stamping path (`header.stamp = now(node)`). `now(node, Steady())` /
`now(node, System())` select an explicit clock; `now(node, src)` is sugar for
`now(clock(node, src))`.
"""
Dates.now(node, src::ClockSource) = Dates.now(clock(node, src))

# Bare `now(node)` defaults to the ROS clock. `node::Any` is broad, but the
# `Clock`-specific method above is more specific so `now(::Clock)` still wins;
# `Dates.now()` (no args, wall `DateTime`) is unaffected (different arity).
Dates.now(node::Any) = Dates.now(node, ROS())

# ── sleep / sleep_until ─────────────────────────────────────────────────────
# Clock-aware and interruptible on context shutdown. Steady/System sleep the wall
# remainder; ROS sleeps wall time today and tracks sim later.

# How long (wall ns) to actually block to realize a span on this clock. For
# Steady/System that's just the span; for ROS-under-sim it differs (TODO).
_wall_ns(::Clock, d::Duration) = d.ns

"""
    sleep(clock::Clock, dur)
    sleep(node, dur)

Sleep `dur` (a `Duration` or `Dates.Period`) on `clock`'s timeline. Blocks the
wall span (the ROS clock sleeps wall time, not simulated `/clock` time) and is
interruptible on `close(ctx)` (raises `ShutdownException`). A `node` first
argument uses its ROS clock.
"""
function Base.sleep(c::Clock, dur)
    ns = _wall_ns(c, Duration(_to_ns(dur)))
    ns <= 0 && return nothing
    _interruptible_sleep(c, ns)
end
# Node form (anything that isn't already a Clock/source). The ROS clock sleep.
Base.sleep(node::Any, dur) = Base.sleep(clock(node, ROS()), dur)

"""
    sleep_until(clock::Clock{C}, t::RTime{C})
    sleep_until(node, t::RTime{C})

Sleep until `t`. The instant's clock tag selects the timeline: a `node` first
argument is resolved to *its* clock for `C`. So the tag on the instant picks the
node's clock:

- `sleep_until(node, now(node))` — the ROS clock.
- `sleep_until(node, now(node, Steady()))` — the steady clock.
"""
function sleep_until(c::Clock{C}, t::RTime{C}) where {C}
    rem = t - Dates.now(c)        # Duration
    rem.ns <= 0 && return nothing
    Base.sleep(c, rem)
end
# Node form: route to the node's clock for the instant's tag `C`. Excludes
# `Clock` (the method above is strictly more specific for a `Clock` first arg).
sleep_until(node, t::RTime{C}) where {C} = sleep_until(clock(node, C()), t)

# Recover the owning Context from a clock's node for shutdown interruption.
# `Clock.node` is duck-typed: a Context (context-level clock), a Node (`.context`),
# or — in tests — something context-less. Return `nothing` in the last case so the
# sleep falls back to an uninterruptible wall sleep rather than throwing.
function _clock_ctx(node)
    node isa Context && return node
    hasproperty(node, :context) && node.context isa Context && return node.context
    nothing
end

# The blocking wall sleep, interruptible on Context shutdown. Parks on the
# Context's `_shutdown_wake` condition, which the drain notifies first; a one-shot
# `Base.Timer` bounds the wait to the requested span (`Threads.Condition` has no
# timed wait). Re-checks `is_shutdown` under the condition lock — the
# check-before-park / re-check-after-notify ordering every interruptible wait
# follows, so a drain firing between the check and the `wait` is never lost — then
# raises `ShutdownException`. A clock with no resolvable Context sleeps the wall span.
function _interruptible_sleep(c::Clock, ns::Integer)
    ctx = _clock_ctx(c.node)
    if ctx === nothing
        Base.sleep(ns / 1e9)
        return nothing
    end
    is_shutdown(ctx) && throw(ShutdownException())
    @lock ctx._shutdown_wake begin
        is_shutdown(ctx) && throw(ShutdownException())
        t = Base.Timer(ns / 1e9) do _
            @lock ctx._shutdown_wake notify(ctx._shutdown_wake)
        end
        try
            wait(ctx._shutdown_wake)
        finally
            close(t)
        end
        is_shutdown(ctx) && throw(ShutdownException())
    end
    nothing
end

# ── Timer ───────────────────────────────────────────────────────────────────
# `Timer(node, period; clock=ROS()) do … end` fires every period on the chosen
# clock. The constructor takes the function first, returns a `close`-able handle
# that stays open until closed, and dies with the node.

"""
    Timer(f, node, period; clock=ROS())
    Timer(node, period; clock=ROS()) do … end

Fire `f()` every `period` (a `Duration` or `Dates.Period`) on `clock`. The
backing implementation depends on the clock:

- `Steady()`/`System()` — a wall-driven `Base.Timer` task.
- `ROS()` — a wall-driven `Base.Timer`; it fires on wall time, not simulated `/clock` time.

`close(timer)` stops it; it also dies with the node.
"""
mutable struct Timer{C<:ClockSource}
    const clk::Clock{C}
    const period::Duration
    const f::Function
    @atomic open::Bool
    impl::Union{Base.Timer,Nothing}   # the backing wall timer (Steady/System)
end

function Timer(f::Function, node, period; clock::ClockSource=ROS())
    c = ROSNode.clock(node, clock)
    t = Timer{typeof(clock)}(c, Duration(_to_ns(period)), f, true, nothing)
    _start!(t)
    t
end
Timer(node, period; clock::ClockSource=ROS()) =
    error("Timer requires a callback: `Timer(node, period; clock) do … end`")

# The dispatch gate for a timer tick: a Timer on a non-Active managed node must
# not fire. The gate keys on the owning node (`clk.node`); `isactive(::Node)` folds
# in the LifecycleNode lookup (and is always `true` for an unmanaged node). A
# context-level clock has no node to gate, so it always ticks. `isactive`/`Node`
# land in later-included files — late-bound, fine at tick time.
_timer_active(node) = node isa Node ? isactive(node) : true

# One tick: the gate (live + Active) then the guarded user callback. A named top-level function so
# `precompile(_fire!, (Timer{C},))` bakes the gate + dispatch site ahead of the first fire; the
# `_ -> _fire!(t)` handler is a trivial forward. `t.f()` is a dynamic call (`f::Function`), so its
# concrete target compiles at first fire — but that target is the already-anchored warmed reaction.
function _fire!(t::Timer)
    (@atomic t.open) || return nothing
    _timer_active(t.clk.node) || return nothing
    try
        t.f()
    catch err
        err isa ShutdownException && return nothing
        @error "ROSNode.Timer callback threw" exception=(err, catch_backtrace())
    end
    return nothing
end

# Wall-clock timers (Steady/System): a `Base.Timer` re-arming each period. The
# handler runs the user `f` guarding against its throws so one bad tick can't
# kill the timer task.
function _start!(t::Timer{C}) where {C<:Union{Steady,System}}
    # no-op while armed: replacing a live impl orphans it, double-firing each period
    t.impl !== nothing && isopen(t.impl) && return t
    secs = t.period.ns / 1e9
    t.impl = Base.Timer(_ -> _fire!(t), secs; interval=secs)
    t
end

# ROS-clock timer. Non-sim: identical to a wall timer (ROS == system). Sim: must
# fire when `/clock` crosses the next deadline, driven by the Context time
# source. TODO(graph): subscribe to the node's ROS-clock advances; for now run
# it as a wall timer so non-sim deployments work.
function _start!(t::Timer{ROS})
    # no-op while armed: replacing a live impl orphans it, double-firing each period
    t.impl !== nothing && isopen(t.impl) && return t
    secs = t.period.ns / 1e9
    t.impl = Base.Timer(_ -> _fire!(t), secs; interval=secs)
    t
end

Base.isopen(t::Timer) = @atomic t.open
function Base.close(t::Timer)
    (@atomicswap t.open = false) || return nothing
    t.impl === nothing || close(t.impl)
    nothing
end
Base.show(io::IO, t::Timer{C}) where {C} =
    print(io, "Timer{", nameof(C), "}(", Dates.canonicalize(Dates.CompoundPeriod(Nanosecond(t.period.ns))),
          isopen(t) ? "" : ", closed", ")")

# ── Rate ──────────────────────────────────────────────────────────────────
# `Rate(node, hz)` + `sleep(rate)` — hold a loop frequency on the node's clock,
# sleeping the remainder of each period (sim-aware via the clock, interruptible).

"""
    Rate(node, hz; clock=ROS())

A loop-frequency governor. Call `sleep(rate)` at the bottom of a loop to sleep
the remainder of each `1/hz` period on the node's clock. Anchoring to the last
wake absorbs per-iteration drift, holding the target frequency (rclcpp `WallRate`
semantics).
"""
mutable struct Rate{C<:ClockSource}
    const clk::Clock{C}
    const period::Duration
    last::RTime{C}
end

function Rate(node, hz::Real; clock::ClockSource=ROS())
    hz > 0 || throw(ArgumentError("Rate frequency must be positive, got $hz"))
    c = ROSNode.clock(node, clock)
    Rate(c, Duration(round(Int64, _NS_PER_S / hz)), Dates.now(c))
end

"""
    sleep(rate::Rate)

Sleep until the next period boundary relative to the previous wake. Returns
`false` if the loop overran the period (no sleep happened), `true` otherwise.
Interruptible on shutdown like the other clock waits.
"""
function Base.sleep(r::Rate{C}) where {C}
    target = r.last + r.period
    n = Dates.now(r.clk)
    if n >= target            # overran: reset the anchor, don't accumulate debt
        r.last = n
        return false
    end
    sleep_until(r.clk, target)
    r.last = target
    true
end

Base.show(io::IO, r::Rate{C}) where {C} =
    print(io, "Rate{", nameof(C), "}(", round(_NS_PER_S / r.period.ns; digits=3), " Hz)")
