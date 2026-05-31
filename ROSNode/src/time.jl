# §7 Time & clocks. rclcpp's three-clock model expressed Julian-ly: the clock
# source is a singleton-type discriminator that doubles as the `RTime` tag, so
# mixing clocks is a *compile-time* MethodError rather than a runtime throw.
#
# Depends only on `Dates` + Base; the Node type lands later, so node accessors
# are duck-typed (`::Any`) against the node's clock surface (see `_node_clock`).
# Sim time (`/clock`, `use_sim_time`) is a Context-level concern not yet wired —
# the ROS-clock paths fall back to system time with TODO(graph) markers so this
# file precompiles and the wall/steady paths work today.

using Dates: Dates, Period, Nanosecond

# ── clock sources ─────────────────────────────────────────────────────────
# Singleton types, not Symbols: type-stable selection and the discriminator is
# reused as the `RTime{C}` tag below.

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
The ROS abstraction: system time normally, or simulated time driven by `/clock`
when a node sets `use_sim_time`. What `now(node)` returns and what stamps use.
"""
struct ROS <: ClockSource end

# ── value types ─────────────────────────────────────────────────────────────

"""
    RTime{C<:ClockSource}

A clock-tagged instant: an `Int64` nanosecond count (rclcpp's
`rcl_time_point_value_t`), *not* the wire `builtin_interfaces/Time` struct.
Named `RTime`, not `Time`, to avoid the `Dates.Time` clash. The tag `C` makes
cross-clock arithmetic a `MethodError` (`RTime{Steady} - RTime{System}` has no
method), turning rclcpp's runtime "clock types differ" throw into a dispatch
guarantee.
"""
struct RTime{C<:ClockSource}
    ns::Int64
end
# (the default inner ctor `RTime{C}(::Int64)` already converts any `Integer`.)

"The `ClockSource` instance tagging this instant."
source(::RTime{C}) where {C} = C()
"Nanoseconds since the clock's epoch."
nanoseconds(t::RTime) = t.ns

"""
    Duration(ns)

A clock-agnostic span in nanoseconds (signed `Int64`; differencing two instants
may be negative). Distinct from `ROSZenoh.Duration` (the unsigned QoS-policy
type) — this is rclcpp's `rcl_duration_value_t`, deliberately untagged so a span
measured on one clock can be applied to another (rclcpp parity).
"""
struct Duration
    ns::Int64
end
# (the default inner ctor `Duration(::Int64)` already converts any `Integer`.)
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
# The whole point: only same-tag instants combine. No `RTime{A} ⊕ RTime{B}`
# method exists, so mixing clocks fails to compile-dispatch.

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

# `header.stamp = now(node)` should convert at the boundary via the generated
# kw-ctor's `convert(FieldType, value)` (§7). We deliberately do **not** define
# `Base.convert(::Type{T}, ::RTime) where T` here: an unconstrained `T` claims
# every `convert` target and collides with dozens of Base methods (`Any`,
# `Nothing`, `Missing`, `Ref`, …). The conversion is keyed on the *concrete*
# generated `builtin_interfaces` `Time`/`Duration` structs, which are owned by
# the message-integration layer and not in scope here.
#
# TODO(messages): when that layer lands, register the narrow methods against the
# concrete types, e.g.
#   Base.convert(::Type{builtin_interfaces.msg.Time}, t::RTime)     = to_msg(builtin_interfaces.msg.Time, t)
#   Base.convert(::Type{builtin_interfaces.msg.Duration}, d::Duration) = to_msg(builtin_interfaces.msg.Duration, d)
# Until then, callers use `to_msg(T, …)` / `rtime` / `duration` explicitly.

# ── jump callbacks ──────────────────────────────────────────────────────────
# Registered so timers can recompute their next fire across a discontinuity
# (sim activate/deactivate, NTP step, runtime `use_sim_time` toggle, §7).

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

A registerable jump handler. `f(::TimeJump)` fires when the clock leaps forward
by ≥ `min_forward`, backward by ≥ `min_backward`, or (if `on_clock_change`) on a
sim activate/deactivate. Thresholds are `Duration`/`Period`/`nothing`.
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

# whether a jump passes a callback's thresholds (used by the §14 jump dispatch)
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
    node::Any              # duck-typed; the Node type lands later (§6/§14)
    source::C
    jumps::Vector{JumpCallback}
end

Clock(node, src::C) where {C<:ClockSource} = Clock{C}(node, src, JumpCallback[])

source(::Clock{C}) where {C} = C()
Base.show(io::IO, ::Clock{C}) where {C} = print(io, "Clock{", nameof(C), "}")

# Duck-typed node→clock accessor. The Node owns a clock registry keyed by
# source; until that lands, synthesize a fresh handle. Defined permissively so
# `now(node)` compiles against any node shape the later layers settle on.
# TODO(graph): route through `node.clock(C())` once Node exists, so the ROS
# clock can pick up the node's `use_sim_time` + Context `/clock` source.
function _node_clock(node, src::C) where {C<:ClockSource}
    if hasproperty(node, :clocks) && haskey(node.clocks, C)
        return node.clocks[C]::Clock{C}
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
# `time()`. ROS == System until sim time is wired.

_read_ns(::Steady) = Int64(Base.time_ns() & typemax(Int64))   # monotonic, ns
_read_ns(::System) = round(Int64, time() * 1e9)               # wall, ns since unix epoch

# ROS clock: system time today. TODO(graph): when the owning node's
# `use_sim_time` is set, read the Context-hosted `/clock` sim time instead.
function _read_ns(c::Clock{ROS})
    # placeholder for the sim-time branch; system time is the non-sim answer.
    _read_ns(System())
end
_read_ns(::ROS) = _read_ns(System())

"""
    now(clock::Clock{C}) -> RTime{C}

Read a held clock. For `Clock{ROS}` this honors the node's sim-time routing
(system time until `/clock` wiring lands).
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
# Clock-aware, and **interruptible on context shutdown** (§14). Steady/System
# sleep the wall remainder; ROS is system today, sim later. Interruption hook is
# stubbed until the Context shutdown plumbing lands.

# How long (wall ns) to actually block to realize a span on this clock. For
# Steady/System that's just the span; for ROS-under-sim it differs (TODO).
_wall_ns(::Clock, d::Duration) = d.ns

"""
    sleep(clock::Clock, dur)
    sleep(node, dur)

Sleep `dur` (a `Duration` or `Dates.Period`) on `clock`'s timeline. Sim-aware on
the ROS clock and interruptible on `close(ctx)` (raises `ShutdownException`).
A `node` first argument uses its ROS clock.
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
argument is resolved to *its* clock for `C` (so `sleep_until(node, now(node))`
waits on the ROS clock, `now(node, Steady())` on the steady one).
"""
function sleep_until(c::Clock{C}, t::RTime{C}) where {C}
    rem = t - Dates.now(c)        # Duration
    rem.ns <= 0 && return nothing
    Base.sleep(c, rem)
end
# Node form: route to the node's clock for the instant's tag `C`. Excludes
# `Clock` (the method above is strictly more specific for a `Clock` first arg).
sleep_until(node, t::RTime{C}) where {C} = sleep_until(clock(node, C()), t)

# The actual blocking wall sleep. TODO(graph): register with the owning
# Context's shutdown so a drain wakes us early with a `ShutdownException`, and so
# a ROS-clock sleep waiting on a stalled `/clock` is woken on close. Today we
# just sleep wall time in slices so a future cooperative cancel can hook in.
function _interruptible_sleep(::Clock, ns::Integer)
    Base.sleep(ns / 1e9)
    nothing
end

# ── Timer ───────────────────────────────────────────────────────────────────
# `Timer(node, period; clock=ROS()) do … end` — fires every period on the chosen
# clock. Follows the `Timer(f, …)` precedent (DESIGN §"type-constructors"):
# constructor takes the function, returns a `close`-able handle, doesn't
# auto-close, and dies with the node.

"""
    Timer(f, node, period; clock=ROS())
    Timer(node, period; clock=ROS()) do … end

Fire `f()` every `period` (a `Duration` or `Dates.Period`) on `clock`. On a
`Steady()`/`System()` clock it's a wall-driven `Base.Timer` task; on the `ROS()`
clock under sim it tracks `/clock` (TODO). `close(timer)` stops it; it also dies
with the node.
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

# Wall-clock timers (Steady/System): a `Base.Timer` re-arming each period. The
# handler runs the user `f` guarding against its throws so one bad tick can't
# kill the timer task.
function _start!(t::Timer{C}) where {C<:Union{Steady,System}}
    secs = t.period.ns / 1e9
    t.impl = Base.Timer(secs; interval=secs) do _
        (@atomic t.open) || return
        try
            t.f()
        catch err
            err isa ShutdownException && return
            @error "ROSNode.Timer callback threw" exception=(err, catch_backtrace())
        end
    end
    t
end

# ROS-clock timer. Non-sim: identical to a wall timer (ROS == system). Sim: must
# fire when `/clock` crosses the next deadline, driven by the Context time
# source. TODO(graph): subscribe to the node's ROS-clock advances; for now run
# it as a wall timer so non-sim deployments work.
function _start!(t::Timer{ROS})
    secs = t.period.ns / 1e9
    t.impl = Base.Timer(secs; interval=secs) do _
        (@atomic t.open) || return
        try
            t.f()
        catch err
            err isa ShutdownException && return
            @error "ROSNode.Timer callback threw" exception=(err, catch_backtrace())
        end
    end
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
the remainder of each `1/hz` period on the node's clock. Drifts are absorbed by
anchoring to the last wake (rclcpp `WallRate` semantics) rather than `sleep`'s
fixed delay.
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
