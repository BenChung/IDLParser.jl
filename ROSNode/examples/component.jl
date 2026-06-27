# Components — authoring a node as a typed `node(…)` schema of member components (the functor
# API; see PLAN-NODE-PLAN.md for the design, DESIGN-COMPONENTS.md for the underlying concepts).
#
#     julia --project=ROSNode ROSNode/examples/component.jl   # from the workspace root
#
# Self-contained: the node and a ground-station client share one process / Context, so
# no router is needed (add `peers=["tcp/localhost:7447"]` to `@context` + start `zenohd`
# to reach nodes on other hosts).
#
# Where the other examples wire a node imperatively, here a node is assembled from member
# *components* by VALUE COMBINATORS: `publishes`/`every`/`serves` author the entities,
# `component(State, Params, ports…; requires/provides/ctor)` ties them to a state type through
# the `member_schema` trait, and `node("name" => State, …)` composes the members (DI-toposorted)
# into one runnable schema value. A `Sensor` component publishes telemetry on a timer and
# *provides* a battery reading; a `Guard` component *requires* that reading (dependency
# injection) and serves a "safe to fly?" query. `run(schema)` brings the node up; several such
# schemas compose into one process with `container` (noted at the bottom).
#
# Self-contained: the message + service types are authored in Julia, so this runs against
# just a router — no sourced ROS2 install.

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
    @ros_package "drone"          # names the authored service type below
    # Extend (not shadow) the framework generics this module defines methods on: `configure`
    # (a lifecycle hook) and `member_schema` (the trait carrying each component's schema). A
    # bare `configure(...) = …` under plain `using` would define a *new* local function the
    # framework never calls; `import` makes these extend ROSNode's. (Reaction handlers such as
    # `tick`/`safe` are fresh functions, so they need no import.)
    import ROSNode: configure, member_schema

    # An interface (§4.1): a NAME for the generic functions a provider must define. Provision
    # is declared by listing the interface in `provides=`, not by subtyping; a consumer asks
    # for it in `requires=`.
    @interface BatterySource  battery(_)::Float64

    # ── Sensor: publishes telemetry on a timer; provides the battery reading ───────
    @parameters struct SensorParams
        rate::Int64 = 5 ∈ 1..50                             # Hz — read live; driveable by `ros2 param`
    end
    mutable struct Sensor{Name} <: Component{Name}
        level::Float64                                      # private state (simulated battery %)
    end
    Sensor{Name}() where {Name} = Sensor{Name}(100.0)       # the no-DI default ctor (DefaultCtor calls S{Name}())
    battery(s::Sensor) = s.level                            # satisfy the BatterySource contract
    # Reactions and lifecycle hooks are node-first: `(node, m, …)`. The accessors take both —
    # `parameters(node, m)` / `entities(node, m)` — reading the node's typed carriers at the
    # member's path (a constant on `m`'s type), so the body stays fully typed.
    configure(node, s::Sensor) = @info "Sensor up" rate = parameters(node, s).rate
    function tick(node, s::Sensor)                          # fires at `rate` Hz, only while Active
        s.level = max(0.0, s.level - 1.0)                   # drain a little each tick
        publish(entities(node, s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
    end
    # The component's schema: state type + param schema + the authored ports + the DI evidence.
    # `every(:tick, :rate, tick)` drives the timer rate live from the `rate` parameter.
    member_schema(::Type{Sensor}) = component(Sensor, SensorParams,
        publishes(:telemetry, Telemetry; on = "~/telemetry"),   # node-private ⇒ /vehicle/telemetry
        every(:tick, :rate, tick);
        provides = (BatterySource,))

    # ── Guard: depends on a BatterySource; serves "safe to fly?" ───────────────────
    # The provider's concrete type is fixed per composition (a real Sensor here, a mock in a
    # test rig), so it lands in the type parameter `B` — the reaction reads it type-stably.
    mutable struct Guard{Name, B} <: Component{Name}
        battery_src::B                                      # the injected sibling provider
    end
    @parameters struct GuardParams
        min_battery::Float64 = 20.0
    end
    # `@service` authors the ROS service TYPE + the handler IMPL + a `safe` descriptor in lockstep:
    # the args after `node, g` are the request fields, the `@NamedTuple` return is the response (it
    # generates `drone/srv/safe_{Request,Response}` and wraps the returned tuple). The body reads the
    # injected Sensor through the BatterySource interface. (Drop to `serves(:n, safe_Request, h; on)`
    # for a different handler, or `serves(:other, safe; on)` to reuse this under another name.)
    @service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
    # No `ctor=` needed: the holder shape (the free type parameters past `Name` are exactly the injected
    # deps, in order) is built by the DEFAULT `construct`, which calls the type itself —
    # `Guard{name, typeof(src)}(src)`, fixing `Guard{name, Sensor}`. The single `BatterySource` dependency
    # arrives as `src`. (Pass `ctor=` only for a custom shape: named fields, extra state, a reordered param.)
    member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))
    # (A standalone load with no Sensor sibling would use a no-`requires` schema + a 0-dep ctor
    #  injecting a null provider; this composition always supplies a real Sensor.)
end
using .Drone

# Assemble the node from two components. Guard requires BatterySource, Sensor provides it, so
# the framework injects the Sensor into the Guard (§4.2). The string member names ("sensor",
# "guard") are each component's namespace within the node — written out, so the parameter/entity
# prefixes are explicit. `name="Vehicle"` registers the kind in the process-global registry, so
# a container can `load_node` it by name (see the bottom).
const Vehicle = node("sensor" => Drone.Sensor, "guard" => Drone.Guard; name = "Vehicle")

@context() do ctx     # one process, one session — no router needed (add `peers=…` to reach other hosts)
    # A ground station — a plain node — watching telemetry. Set up first, so it's ready
    # when the vehicle starts publishing.
    ground = Node(ctx, "ground")
    Subscription(ground, "/vehicle/telemetry", Msgs.Telemetry) do msg
        @info "telemetry" battery = round(msg.battery; digits = 1) altitude = msg.altitude
    end

    # Bring the node up on this Context. `block=false` returns so we can drive it here; drop
    # `ctx`/`block` to run it standalone in its own process. `overrides` sets each component's
    # parameters by their local names.
    vehicle = run(Vehicle; ctx = ctx, name = "vehicle",
                  overrides = (rate = 5, min_battery = 90.0), block = false)
    sleep(1.2)                                              # telemetry streams in at 5 Hz

    # Query the guard service (its request/response types were authored by `@service safe`).
    SafeReq = Drone.drone.srv.safe_Request
    guard = ServiceClient(ground, "/vehicle/safe_to_fly", SafeReq)
    wait_for_service(guard; timeout = 5)
    resp = call(guard, SafeReq(target_altitude = 50.0))
    @info "safe to fly?" ok = resp.ok battery = round(resp.battery; digits = 1) threshold = 90.0

    close(guard)
    close(vehicle)
end

# ── Deploy-time composition (the second scale) ──────────────────────────────────
# The same `Vehicle` schema runs standalone *or* shares a process with other nodes — decided at
# deploy time, not in the node's code. Because `node(…; name="Vehicle")` registered the kind, a
# container can load it by name (`ros2 component load`) or programmatically:
#
#     container("fleet") do c
#         add!(c, Vehicle;       name = "vehicle")
#         add!(c, GroundStation; name = "ground")     # another node(…) schema
#     end
#
# Inside a container the nodes share one session/discovery/registry and (intra-process) a
# direct in-process delivery path; `run(Vehicle)` instead gives it its own process.
