# Components

A component is a node authored as a collection of `@mixin`s — each a cohesive chunk of private state, the entities authored onto it, and its own lifecycle. Where the other pages wire a node imperatively (open a context, create a `Node`, attach a publisher here, a service there), a component assembles the node from these chunks and runs it.

This page assembles a `Vehicle` node from two mixins. A `Sensor` publishes telemetry on a timer and provides a battery reading; a `Guard` requires that reading and serves a "safe to fly?" query. `@node` composes them; `run` brings the node up. The full example lives in `examples/component.jl`.

## The mixin — state plus the entities authored onto it

`@mixin` declares one chunk. The struct holds private state; entities attach to the mixin type itself, keeping the struct state-only:

```julia
@mixin struct Sensor
    level::Float64 = 100.0          # private state (simulated battery %)
end
```

A `@param` attaches a live parameter, read through `parameters(s)` and driveable from the [parameter](../communication/parameters.md) services. The `∈ 1..50` constraint reuses the parameter schema grammar:

```julia
@param Sensor rate::Int64 = 5 ∈ 1..50      # Hz — read live via parameters(s).rate
```

A `@publishes` declares a publisher on the mixin. The `~/` prefix makes the topic node-private, so `~/telemetry` resolves against the node's name to `/vehicle/telemetry`. Drive the publisher through `entities(s)` — see [Topics](../communication/topics.md) for the publish surface:

```julia
@publishes Sensor telemetry :: Telemetry on "~/telemetry"   # node-private ⇒ /vehicle/telemetry
```

`parameters(s)` and `entities(s)` are the two reflective accessors: `parameters(s)` returns the current, type-stable parameter snapshot, and `entities(s)` returns the materialised entity handles. Both are typed by the mixin type, so `parameters(s).rate` and `entities(s).telemetry` are ordinary typed field loads.

A `@every` declares a timer. `:rate` binds its frequency to the `rate` parameter, in Hz. The timer fires only while the node is Active:

```julia
@every :rate function tick(s::Sensor)
    s.level = max(0.0, s.level - 1.0)       # drain a little each tick
    publish(entities(s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
end
```

## Inline-authored entities

A `@serves` authors a service straight from a function signature. The arguments after the mixin are the request fields; the `@NamedTuple` return type is the response. The macro generates the `srv` type from that signature. See [Services](../communication/services.md) for the service model:

```julia
@serves "~/safe_to_fly" function safe(g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
    b = battery(g.battery_src)              # reads the Sensor through the interface
    (ok = b >= parameters(g).min_battery && target_altitude <= 100.0, battery = b)
end
```

The leading string `"~/safe_to_fly"` names the service; the handler keeps the function name `safe`.

## Dependency injection between mixins

`Guard` needs a battery reading that a sibling owns. It asks for that capability by interface, and the framework injects a sibling that provides it.

An `@interface` names a capability — the generic functions a provider must define:

```julia
@interface BatterySource  battery(_)::Float64
```

`Sensor` provides it by backing the contract method and declaring the evidence with `@provides`:

```julia
battery(s::Sensor) = s.level                # satisfy the BatterySource contract
@provides Sensor BatterySource
```

`Guard` declares the need with `requires`, and receives the resolved provider in its `construct` method. The injected provider lands in a state field:

```julia
@mixin struct Guard
    battery_src::Any = nothing              # the injected sibling provider
end
requires(::Type{Guard}) = (BatterySource,)
construct(::Type{Guard}, node, src) = Guard(battery_src = src)
@param Guard min_battery::Float64 = 20.0
```

`construct` dispatches on the mixin being built. The dependency arrives untyped and is used through its interface method — `battery(g.battery_src)` in the service handler above.

`requires`, `construct`, and the lifecycle hooks extend framework generics. A component module writes `import ROSNode: configure, requires, construct` so its method definitions extend ROSNode's. A bare definition under plain `using` defines a fresh local function the framework never calls. Reactions authored by `@hears`/`@serves`/`@every`, and `@param`/`@provides`, are macro-emitted and need no import.

`configure` is a lifecycle hook — a plain method run at startup:

```julia
configure(s::Sensor) = @info "Sensor up" rate = parameters(s).rate
```

## Assembling and running the node

`@node` assembles the node from a list of members. Each `"name" => Mixin` pair gives the mixin a namespace within the node:

```julia
@node Vehicle = ["sensor" => Drone.Sensor, "guard" => Drone.Guard]
```

`Guard` requires `BatterySource` and `Sensor` provides it, so the framework toposorts the members by dependency and injects the `Sensor` into the `Guard`. A dependency configures before its dependent and tears down after.

`run` brings the node up. `block = false` returns so you can drive the node from the calling code; passing `ctx` runs it on an existing context, and dropping `ctx` runs it standalone in its own process. `overrides` sets each mixin's parameters by their local names:

```julia
@context() do ctx
    ground = Node(ctx, "ground")
    Subscription(ground, "/vehicle/telemetry", Msgs.Telemetry) do msg
        @info "telemetry" battery = round(msg.battery; digits = 1) altitude = msg.altitude
    end

    vehicle = run(Vehicle; ctx = ctx, name = "vehicle",
                  overrides = (rate = 5, min_battery = 90.0), block = false)
    sleep(1.2)                              # telemetry streams in at 5 Hz

    SafeReq = Drone.drone.srv.safe_Request
    guard = ServiceClient(ground, "/vehicle/safe_to_fly", SafeReq)
    wait_for_service(guard; timeout = 5)
    resp = call(guard, SafeReq(target_altitude = 50.0))
    @info "safe to fly?" ok = resp.ok battery = round(resp.battery; digits = 1)

    close(guard)
    close(vehicle)
end
```

The ground station is a plain node subscribing to `/vehicle/telemetry`. The vehicle publishes telemetry at 5 Hz, and the guard answers the `~/safe_to_fly` query its `@serves` authored.

## Two scales of composition

A `@node` runs standalone — `run(Vehicle; name = "vehicle")` gives it its own process with its own session, discovery, and registry.

Several nodes share one process through a `container`. Each `add!` instantiates a node on the container's single context, so the nodes share one session, discovery, and registry, plus a direct in-process delivery path between them:

```julia
container("fleet") do c
    add!(c, Vehicle;       name = "vehicle")
    add!(c, GroundStation; name = "ground")     # another @node
end
```

The same `Vehicle` runs either way — standalone or composed. Deploy-time wiring picks the scale, leaving the node's code unchanged.

## Other reactions and ports

The example uses `@publishes`, `@serves`, and `@every`. The mixin surface has three more verified members, each mirroring its primitive layer:

- `@hears` declares a subscription reaction — a handler dispatched on the mixin for each message on a topic.
- `@uses` declares a client port — a [service](../communication/services.md) or action client the mixin drives from its reactions.
- `@runs` inline-authors an action server from a function signature, the way `@serves` authors a service.

## See also

- [Parameters](../communication/parameters.md) — the live parameter model `@param` builds on.
- [Interoperating with ROS 2](../interop/ros2.md) — driving a component's parameters, topics, and services from `ros2` tooling.
