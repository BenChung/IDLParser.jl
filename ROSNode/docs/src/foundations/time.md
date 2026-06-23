# Time & Clocks

ROSNode reads time through clocks, mirroring rclcpp's three-clock model: a wall clock, a monotonic clock, and a ROS clock that can follow simulated `/clock` time. Each read returns a clock-tagged instant you can stamp onto a message, difference into a span, or wait against. See the upstream [Clock and Time](https://design.ros2.org/articles/clock_and_time.html) design article for the model this follows.

## Reaching the API

The clock API is reached by importing the names you use, not by `using ROSNode` alone:

```julia
using ROSNode: clock, Rate, sleep_until, RTime, Duration,
               System, Steady, ROS, nanoseconds, seconds,
               JumpCallback, register!
using Dates: now                 # `now(node)` adds a method to `Dates.now`
# `Timer` is reached qualified — `ROSNode.Timer` — to leave `Base.Timer` in scope.
```

Two names extend the standard library rather than living in ROSNode: `now` adds methods to `Dates.now`, and `sleep` adds methods to `Base.sleep`. `now` needs `using Dates: now` (or call it qualified, `Dates.now(node)`); `sleep` is always in scope from `Base`.

## The three clocks

A `ClockSource` selects which clock a read uses. The three instances are singletons:

| Source | Backing | Monotonic | Can jump | Use for |
|--------|---------|-----------|----------|---------|
| `System()` | wall clock (`CLOCK_REALTIME`) | no | yes (NTP) | human-facing timestamps |
| `Steady()` | monotonic clock (`CLOCK_MONOTONIC`) | yes | no | durations, timeouts |
| `ROS()` | system time, or `/clock` under `use_sim_time` | no | yes | message stamps, the default |

`now(node)` reads the ROS clock; pass a source to pick another.

## Reading the time

`now(node)` is the common path and returns an `RTime{ROS}`. `clock(node, source)` returns a held `Clock` handle to read repeatedly, sleep against, and register jump callbacks on.

```julia
node = Node(ctx, "stamper")

t    = now(node)               # RTime{ROS} — the default clock
mono = now(node, Steady())     # RTime{Steady} — monotonic, for durations
wall = now(node, System())     # RTime{System} — wall clock

clk  = clock(node, Steady())   # a handle to hold
now(clk)                       # read it; equivalent to now(node, Steady())
```

## Instants and spans

Two value types carry time in-process:

- `RTime{C}` — an instant, tagged with its clock source `C`.
- `Duration` — an untagged span in nanoseconds; differencing two instants yields one.

Arithmetic stays on a clock: differencing two instants gives a span, and adding a span (a `Duration` or any `Dates.Period`) gives an instant back.

```julia
using Dates: Millisecond

a  = now(node)
b  = now(node)
dt = b - a                       # instant − instant → Duration
nanoseconds(dt)                  # Int64 nanoseconds
seconds(dt)                      # Float64 seconds, for logging

later = now(node) + Millisecond(20)   # instant + Period → instant
```

Each instant carries its clock as a type tag, so the compiler rejects mixing two clocks — there is no method to subtract a `System` instant from a `Steady` one:

```julia
now(node, Steady()) - now(node, System())   # MethodError: the clocks differ
```

## Stamping messages

A `builtin_interfaces/Time` or `/Duration` field accepts an `RTime`/`Duration` directly, so a header stamps from `now(node)`:

```julia
@ros_import "std_msgs/msg/Header" from="interfaces"

h = std_msgs.msg.Header(stamp = now(node), frame_id = "base_link")
```

For explicit conversion — building a wire message by hand, or reading a stamp back off the wire — use `to_msg`, `rtime`, and `duration`:

```julia
using ROSNode: to_msg, rtime, duration
const Time = ROSNode.Interfaces.builtin_interfaces.msg.Time

wire = to_msg(Time, now(node))        # RTime    → builtin_interfaces/Time
back = rtime(ROS(), wire)             # Time      → RTime{ROS}
span = duration(some_duration_msg)    # /Duration → Duration
```

!!! note "`Duration` names two things"
    `ROSNode.Duration` is the in-process time span here. `ROSZenoh.Duration` is the unsigned QoS-policy lifespan. Import whichever one you mean, or qualify it.

## Sleeping and rate-limiting

`sleep` and `sleep_until` wait on a clock's timeline and wake early when the Context shuts down (raising `ShutdownException`), so a sleeping loop stops cleanly on Ctrl-C.

```julia
sleep(node, Millisecond(500))            # sleep a span on the node's ROS clock
sleep_until(node, now(node) + Duration(Millisecond(500)))   # sleep to an instant
```

`Rate(node, hz)` holds a loop frequency. Call `sleep(rate)` at the bottom of the loop to sleep the remainder of each period; it anchors to the previous wake, so per-iteration work doesn't accumulate drift (rclcpp `WallRate` semantics). It returns `false` when the loop overran the period and no sleep happened.

```julia
rate = Rate(node, 10)                    # 10 Hz
while isopen(ctx)
    publish(pub, reading())
    sleep(rate)                          # hold 10 Hz
end
```

## Timers

`ROSNode.Timer` fires a callback every period on a chosen clock. It re-arms until closed, dies with the node, and on a managed [`LifecycleNode`](@ref) fires only while the node is `Active`. A throwing tick routes through the Context's reaction-error policy (see [Reaction errors](runtime-model.md#Reaction-errors)).

```julia
t = ROSNode.Timer(node, Millisecond(100)) do
    publish(pub, reading())
end
# … later …
close(t)
```

Inside a component, declare timers with [`every`](@ref) rather than constructing `Timer` directly: `every` ties the timer to the component lifecycle and can bind its period to a parameter. `Timer` is the primitive `every` builds on — reach for it directly outside the component model. See [Components](../composition/components.md).

## Simulated time

A node follows simulated time by enabling the well-known `use_sim_time` parameter. The Context subscribes to `/clock`, and `now(node, ROS())` then returns the latest published `/clock` value; clearing the parameter reverts the ROS clock to system time.

```julia
@parameters struct SimParams
    use_sim_time::Bool = false
end

node = Node(ctx, "simnode", SimParams)
node.parameters.use_sim_time = true      # route the ROS clock to /clock
```

A `JumpCallback` registered on a clock fires across discontinuities — sim activate/deactivate, a forward or backward step past a threshold:

```julia
clk = clock(node, ROS())
register!(clk; min_forward = Millisecond(1)) do jump
    @info "clock jumped" kind = jump.kind delta = jump.delta
end
```

!!! note "Simulated time governs reads, not scheduling"
    `use_sim_time` reroutes clock *reads*: `now(node, ROS())` returns `/clock` time. Timers, `sleep`, and `Rate` schedule on wall time — a `Timer{ROS}` fires on its wall period regardless of how `/clock` advances.

## API reference

```@meta
CurrentModule = ROSNode
```

### Clocks and sources

```@docs
ClockSource
System
Steady
ROS
clock
Clock
source
```

### Reading time

```@docs
Dates.now(::ROSNode.Clock)
Dates.now(::Any, ::ROSNode.ClockSource)
```

### Instants and spans

```@docs
RTime
Duration
nanoseconds
seconds
```

### Wire conversion

```@docs
to_msg
rtime
```

### Waiting

```@docs
Base.sleep(::ROSNode.Clock, ::Any)
sleep_until
Rate
Base.sleep(::ROSNode.Rate)
```

### Timers

```@docs
Timer
```

### Simulated time

```@docs
JumpCallback
register!
TimeJump
JumpKind
```
```