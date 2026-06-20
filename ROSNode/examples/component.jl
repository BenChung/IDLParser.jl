# Components — authoring a node as a collection of mixins (see DESIGN-COMPONENTS.md).
#
#     julia --project=. ROSNode/examples/component.jl
#
# Self-contained: the node and a ground-station client share one process / Context, so
# no router is needed (add `peers=["tcp/localhost:7447"]` to `@context` + start `zenohd`
# to reach nodes on other hosts).
#
# Where the other examples wire a node imperatively, here the node *is* a collection of
# `@mixin`s — each a cohesive chunk of private state + the entities authored onto it +
# its own lifecycle. A `Sensor` mixin publishes telemetry on a timer and *provides* a
# battery reading; a `Guard` mixin *requires* that reading (dependency injection) and
# serves a "safe to fly?" query. `@node` assembles them into one node (Guard ▸ Sensor,
# toposorted), and `run` brings it up. Several such nodes compose into one process with
# `container` (noted at the bottom).
#
# Self-contained: the message + service types are authored in Julia, so this runs
# against just a router — no sourced ROS2 install.

using ROSNode

# An authored message — a plain struct that *is* the ROS type (cf. authored_pubsub.jl).
module Msgs
    using ROSNode
    @ros_package "drone_msgs"
    @ros_message struct Telemetry
        battery::Float64
        altitude::Float64
    end
end

module Drone
    using ROSNode
    using ..Msgs: Telemetry
    @ros_package "drone"          # names the inline-authored service type below
    # Extend (not shadow) the framework generics this module defines methods on. A bare
    # `configure(...) = …` under plain `using` would define a *new* local function the
    # framework never calls; `import` makes these methods extend ROSNode's. (Reactions
    # authored by `@hears`/`@serves`/`@every` are fresh functions, so they need no import.)
    import ROSNode: configure, requires, construct

    # An interface (§4.1): a NAME for the generic functions a provider must define.
    # Provision is declared with `@provides`, not subtyping; consumers ask by interface.
    @interface BatterySource  battery(_)::Float64

    # ── Sensor: publishes telemetry on a timer; provides the battery reading ───────
    @mixin struct Sensor
        level::Float64 = 100.0                              # private state (simulated battery %)
    end
    @param     Sensor rate::Int64 = 5 ∈ 1..50              # Hz — read live; driveable by `ros2 param`
    @publishes Sensor telemetry :: Telemetry on "~/telemetry"   # node-private ⇒ /vehicle/telemetry
    battery(s::Sensor) = s.level                            # satisfy the BatterySource contract
    @provides  Sensor BatterySource
    # Reactions and lifecycle hooks are node-first: `(node, m, …)`. The accessors take both —
    # `parameters(node, m)` / `entities(node, m)` — reading the node's typed carriers at the
    # member's path (a constant on `m`'s type), so the body stays fully typed.
    configure(node, s::Sensor) = @info "Sensor up" rate = parameters(node, s).rate
    @every :rate function tick(node, s::Sensor)             # fires at `rate` Hz, only while Active
        s.level = max(0.0, s.level - 1.0)                   # drain a little each tick
        publish(entities(node, s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
    end

    # ── Guard: depends on a BatterySource; serves "safe to fly?" ───────────────────
    # The provider's concrete type is fixed per composition (a real Sensor here, a mock
    # in a test rig), so it lands in a type parameter — reactions read it type-stably.
    struct NullBattery end                                  # null-object provider for standalone loads
    battery(::NullBattery) = 0.0
    # A parametric mixin writes its own `Name` param + the `<: Component{Name}` clause (the macro
    # injects neither when the struct already has type params). `Name` is the member's path; `B` is
    # the injected provider's concrete type.
    @mixin struct Guard{Name, B} <: Component{Name}
        battery_src::B                                      # the injected sibling provider
    end
    requires(::Type{Guard}) = (BatterySource,)              # need a BatterySource …
    # `construct` threads the member name as `Val{Name}` and produces the concrete instantiation.
    construct(::Type{Guard}, node, ::Val{Name}, src) where {Name} =                # … injected ⇒ Guard{name,Sensor}
        Guard{Name, typeof(src)}(battery_src = src)
    construct(::Type{Guard}, node, ::Val{Name}) where {Name} =                     # standalone ⇒ Guard{name,NullBattery}
        Guard{Name, NullBattery}(battery_src = NullBattery())
    @param Guard min_battery::Float64 = 20.0
    # Inline-authoring `@serves`: node-first, then the request fields after `g`; the `@NamedTuple`
    # return is the response — the macro generates the `srv` type.
    @serves "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)                          # reads the Sensor through the interface
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
end
using .Drone

# Assemble the node from two mixins. Guard requires BatterySource, Sensor provides it,
# so the framework injects the Sensor into the Guard (§4.2). The string member names
# ("sensor", "guard") are each mixin's namespace within the node — written out, so the
# parameter/entity prefixes are explicit.
@node Vehicle = ["sensor" => Drone.Sensor, "guard" => Drone.Guard]

@context() do ctx     # one process, one session — no router needed (add `peers=…` to reach other hosts)
    # A ground station — a plain node — watching telemetry. Set up first, so it's ready
    # when the vehicle starts publishing.
    ground = Node(ctx, "ground")
    Subscription(ground, "/vehicle/telemetry", Msgs.Telemetry) do msg
        @info "telemetry" battery = round(msg.battery; digits = 1) altitude = msg.altitude
    end

    # Bring the node up on this Context. `block=false` returns so we can drive it here;
    # drop `ctx`/`block` to run it standalone in its own process. `overrides` sets each
    # mixin's parameters by their local names.
    vehicle = run(Vehicle; ctx = ctx, name = "vehicle",
                  overrides = (rate = 5, min_battery = 90.0), block = false)
    sleep(1.2)                                              # telemetry streams in at 5 Hz

    # Query the guard service (its request/response types were authored inline).
    SafeReq = Drone.drone.srv.safe_Request
    guard = ServiceClient(ground, "/vehicle/safe_to_fly", SafeReq)
    wait_for_service(guard; timeout = 5)
    resp = call(guard, SafeReq(target_altitude = 50.0))
    @info "safe to fly?" ok = resp.ok battery = round(resp.battery; digits = 1) threshold = 90.0

    close(guard)
    close(vehicle)
end

# ── Deploy-time composition (the second scale) ──────────────────────────────────
# The same `Vehicle` runs standalone *or* shares a process with other nodes — decided
# at deploy time, not in the node's code:
#
#     container("fleet") do c
#         add!(c, Vehicle;     name = "vehicle")
#         add!(c, GroundStation; name = "ground")     # another @node / @mixin
#     end
#
# Inside a container the nodes share one session/discovery/registry and (intra-process)
# a direct in-process delivery path; `run(Vehicle)` instead gives it its own process.
