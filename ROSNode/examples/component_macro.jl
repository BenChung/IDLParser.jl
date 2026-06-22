# Components, the `@component` way — the same Vehicle node as `component.jl`, but authoring its
# components with the `@component` macro instead of the raw value combinators.
#
#     julia --project=. ROSNode/examples/component_macro.jl
#
# `@component mutable struct … end` is the concise authoring tier: it EMITS the value API
# (`component(…)` + `member_schema` + the struct + ctor + reaction/hook methods) in ONE expression,
# so a struct edit re-runs the whole block and re-keys everything together (Revise-friendlier than the
# separate-definitions form). It is sugar over `component`/`node` — the raw combinators remain the
# primitive, and the two coexist: here the *provider* `Sensor` is authored with `@component`, while the
# *DI consumer* `Guard` (which `requires` an injected sibling + a custom ctor) stays on the raw API —
# `@component` v1 covers the DI-free common case (`@requires` is a planned extension, and errors clearly
# for now). Compare side-by-side with `component.jl`, which authors both the long way.
#
# Self-contained: the node and a ground-station client share one process / Context, so no router is
# needed; the message + service types are authored in Julia, so this runs against just a router — no
# sourced ROS2 install.

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
    # Only the raw-API `Guard` below needs `member_schema` IMPORTED to extend it; the `@component`
    # `Sensor` needs no `import` at all — the macro emits its `member_schema` + `configure` as
    # `ROSNode.…` (extending the framework generics) for you. (`battery` is this module's own
    # `@interface` generic, so its `Sensor` method extends it without an import.)
    import ROSNode: member_schema

    @interface BatterySource  battery(_)::Float64

    # ── Sensor (@component): publishes telemetry on a timer; provides the battery reading ──────────
    # One block authors the struct, the zero-arg ctor (from the inline `level` default), the
    # `@parameters struct SensorParams` (from `@param`), the `tick` timer + handler, the `configure`
    # hook, and `member_schema(::Type{Sensor})` on the bare base. Reactions/hooks are node-first
    # `(node, m, …)`; a bare `m` is annotated `m::Sensor` for you, so the body stays fully typed.
    @component mutable struct Sensor{Name} <: Component{Name}
        level::Float64 = 100.0                              # private state (simulated battery %)
        @param rate::Int64 = 5 ∈ 1..50                      # Hz — read live; driveable by `ros2 param`
        @provides BatterySource                             # this component satisfies the BatterySource contract
        @publishes telemetry::Telemetry on "~/telemetry"    # node-private ⇒ /vehicle/telemetry
        @every :rate function tick(node, m)                 # fires at the live `rate` Hz, only while Active
            m.level = max(0.0, m.level - 1.0)               # drain a little each tick
            publish(entities(node, m).telemetry, Telemetry(battery = m.level, altitude = 12.0))
        end
        configure(node, m) = @info "Sensor up" rate = parameters(node, m).rate
    end
    # The BatterySource contract impl. Single-arg (not node-first), so it lives OUTSIDE the @component
    # block — a struct edit would orphan it (the documented limit for externally-defined methods).
    battery(s::Sensor) = s.level

    # ── Guard (raw value API): depends on a BatterySource; serves "safe to fly?" ───────────────────
    # A DI CONSUMER — `requires` an injected sibling, stored in the type parameter `B`, built by a
    # custom ctor that threads the resolved dep. `@component` v1 does not author this yet (`@requires`
    # is reserved), so it uses the raw `component(…; requires, ctor)` API — which composes seamlessly
    # with the `@component`'d Sensor in the same `node(…)`.
    mutable struct Guard{Name, B} <: Component{Name}
        battery_src::B                                      # the injected sibling provider
    end
    make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
    @parameters struct GuardParams
        min_battery::Float64 = 20.0
    end
    # `@service` authors the service TYPE + handler IMPL + descriptor in lockstep (same as component.jl).
    @service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
    member_schema(::Type{Guard}) = component(Guard, GuardParams, safe;
        requires = (BatterySource,), ctor = make_guard)
end
using .Drone

# Compose the two components into one node — identical to component.jl; `@component` changed only how
# `Sensor` is authored, not how it composes. Guard requires BatterySource, Sensor provides it ⇒ the
# Sensor is injected into the Guard (DI-toposorted). `name="Vehicle"` registers the kind.
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

# Deploy-time composition is unchanged from component.jl: the registered `Vehicle` kind runs standalone
# (`run(Vehicle)`) or shares a process via `container(…) do c; add!(c, Vehicle; name="vehicle"); end`.
