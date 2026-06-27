# Components, the `@schema` way — the same Vehicle node as `component.jl`, but authoring each
# component's `member_schema` with the `@schema` macro instead of the raw value combinators.
#
#     julia --project=ROSNode ROSNode/examples/schema.jl   # from the workspace root
#
# `@schema Base [Params] begin … end` is the member_schema authoring EDSL — the same port directives as
# `@component` (`@publishes`/`@hears`/`@serves`/`@runs`/`@every`/`@uses`), plus `@param`/`@provides`/
# `@requires` for parameters and DI and `@ctor` for a custom constructor. It emits the
# `member_schema(::Type{Base}) = component(Base, …)` (and a `@parameters` struct or inline handler defs if
# you write them inline), and defines no state struct — you write the `mutable struct`, the
# reaction/lifecycle methods, and the `@parameters` separately, exactly as in `component.jl`. That
# decoupling keeps the representation power the raw combinators have but `@component`'s all-in-one form
# gives up: a parametric DI-consumer struct, externally-defined handlers, a custom ctor.
#
# Three authoring tiers, one scenario — compare side by side:
#   • component.jl        raw combinators: `member_schema(::Type{S}) = component(S, P, publishes(…), …)`
#   • component_macro.jl  `@component mutable struct …` — struct + handlers + schema in one block
#   • schema.jl (here)    `@schema S P begin … end`     — the schema EDSL over a hand-written struct
#
# Self-contained: the node and a ground-station client share one process / Context, so no router is
# needed; the message + service types are authored in Julia, so it needs no sourced ROS2 install either.

using ROSNode

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
    @ros_package "drone"
    # `configure` (a lifecycle hook) is extended below, so it must be imported to add a method rather than
    # shadow the framework generic. `@schema` emits `member_schema` as `ROSNode.member_schema` for you, so
    # — unlike `component.jl` — that import is unnecessary. (`battery`/`tick`/`safe` are fresh names.)
    import ROSNode: configure

    @interface BatterySource  battery(_)::Float64

    # ── Sensor: publishes telemetry on a timer; provides the battery reading ───────
    # Struct, ctor, the contract impl, the lifecycle hook, and the timer handler are ordinary
    # definitions; `@schema` only wires the ports. `@every tick at :rate` drives the timer rate live
    # from the `rate` parameter (the `at`-rate form; a number would be a fixed Hz).
    @parameters struct SensorParams
        rate::Int64 = 5 ∈ 1..50                             # Hz — read live; driveable by `ros2 param`
    end
    mutable struct Sensor{Name} <: Component{Name}
        level::Float64                                      # private state (simulated battery %)
    end
    Sensor{Name}() where {Name} = Sensor{Name}(100.0)
    battery(s::Sensor) = s.level                            # satisfy the BatterySource contract
    configure(node, s::Sensor) = @info "Sensor up" rate = parameters(node, s).rate
    function tick(node, s::Sensor)                          # fires at `rate` Hz, only while Active
        s.level = max(0.0, s.level - 1.0)
        publish(entities(node, s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
    end
    @schema Sensor SensorParams begin
        @publishes telemetry::Telemetry on "~/telemetry"   # node-private ⇒ /vehicle/telemetry
        @every     tick at :rate                           # bind the period to the `rate` parameter
        @provides  BatterySource
    end

    # ── Guard: depends on a BatterySource; serves "safe to fly?" ───────────────────
    # A DI consumer. The injected provider's concrete type lands in the type parameter `B` (type-stable
    # reads). For this holder shape — the free type parameters past `Name` are exactly the injected deps,
    # in order — the DEFAULT constructor builds `Guard{Name, typeof(src)}(src)` from the type itself, so
    # `@requires` alone is enough: no `@ctor`, no hand-written `make_guard`. (Reach for `@ctor` only when
    # construction is non-trivial — named fields, extra non-dep state, or a different parameter order.)
    mutable struct Guard{Name, B} <: Component{Name}
        battery_src::B                                      # the injected sibling provider
    end
    @parameters struct GuardParams
        min_battery::Float64 = 20.0
    end
    @service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
    @schema Guard GuardParams begin
        @serves   safe                                     # the pre-authored @service handler, by reference
        @requires BatterySource                            # DI evidence — the default ctor injects the sibling
    end
end
using .Drone

# Compose the two components — identical to component.jl; `@schema` changed only how each `member_schema`
# is authored, not how they compose. Guard requires BatterySource, Sensor provides it ⇒ the Sensor is
# injected into the Guard (DI-toposorted). `name="Vehicle"` registers the kind.
const Vehicle = node("sensor" => Drone.Sensor, "guard" => Drone.Guard; name = "Vehicle")

@context() do ctx     # one process, one session — no router needed (add `peers=…` to reach other hosts)
    ground = Node(ctx, "ground")
    Subscription(ground, "/vehicle/telemetry", Msgs.Telemetry) do msg
        @info "telemetry" battery = round(msg.battery; digits = 1) altitude = msg.altitude
    end

    vehicle = run(Vehicle; ctx = ctx, name = "vehicle",
                  overrides = (rate = 5, min_battery = 90.0), block = false)
    sleep(1.2)                                              # telemetry streams in at 5 Hz

    SafeReq = Drone.drone.srv.safe_Request
    guard = ServiceClient(ground, "/vehicle/safe_to_fly", SafeReq)
    wait_for_service(guard; timeout = 5)
    resp = call(guard, SafeReq(target_altitude = 50.0))
    @info "safe to fly?" ok = resp.ok battery = round(resp.battery; digits = 1) threshold = 90.0

    close(guard)
    close(vehicle)
end
