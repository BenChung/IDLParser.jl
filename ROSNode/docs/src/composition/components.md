# Components

A component is a node authored as a collection of `@mixin`s — each a cohesive chunk of private state, the entities authored onto it, and its own lifecycle. Where the other pages wire a node imperatively (open a context, create a `Node`, attach a publisher here, a service there), a component assembles the node from these chunks and runs it.

This page assembles a `Vehicle` node from two mixins. A `Sensor` publishes telemetry on a timer and provides a battery reading; a `Guard` requires that reading and serves a "safe to fly?" query. `@node` composes them; `run` brings the node up. The full example lives in `examples/component.jl`.

!!! warning "`import` the framework generics — a bare `using` silently shadows them"
    The lifecycle hooks (`configure`, `activate`, `deactivate`, `cleanup`, `on_error`) and the assembly generics (`construct`, `requires`, `provides`) are **ROSNode functions you add methods to**. Bring them in with `import`, not plain `using`:

    ```julia
    using ROSNode
    import ROSNode: configure, cleanup, construct, requires   # the generics you extend
    ```

    Under a bare `using ROSNode`, a definition like `configure(m::MyMixin) = …` creates a **new local `configure`** that shadows ROSNode's. Your method is never called: the framework dispatches its own generic, runs the default no-op, and there is **no error or warning** — the node simply comes up without your setup. (Equivalently, qualify the name at the definition: `ROSNode.configure(m::MyMixin) = …`.)

    Members authored by the macros — `@hears`/`@serves`/`@every`/`@runs`/`@uses` reactions and `@param`/`@provides`/`@interface` — are macro-emitted and need no import.

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

`Guard` declares the need with `requires`, and receives the resolved provider in its `construct` method. The injected provider lands in a type-parameter field — its concrete type is fixed per composition (a real `Sensor` here, a mock in a test rig), so reactions read it type-stably:

```julia
@mixin struct Guard{B}
    battery_src::B                          # the injected sibling provider
end
requires(::Type{Guard}) = (BatterySource,)
construct(::Type{Guard}, node, src) = Guard(battery_src = src)   # injected ⇒ Guard{Sensor}
@param Guard min_battery::Float64 = 20.0
```

`construct` dispatches on the mixin being built — the bare base `Guard`, which covers every `Guard{B}`; reactions annotate the base the same way (`safe(g::Guard, …)`). The dependency is used through its interface method — `battery(g.battery_src)` in the service handler above. Omitting a zero-dep `construct` makes the dependency required: the mixin loads only composed in a `@node`. Providing one makes it optional — `construct(::Type{Guard}, node) = Guard(battery_src = NullBattery())` picks a null-object default so `run(Guard)` works standalone.

`requires` and `construct` are two of the ROSNode generics you extend, so they need the `import` (or a qualified `ROSNode.construct(…) = …`) from the warning at the top of the page — a bare `using`-shadowed definition is silently never called.

`configure` is a lifecycle hook — a plain method run at startup; the lifecycle section below covers the full set:

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

## Lifecycle — acquiring and releasing state

A mixin whose state wraps an external resource — a device, a file, a connection — authors the setup and the matching release as lifecycle hooks. The five hooks are plain methods on the mixin type, each defaulting to a no-op:

- `configure(m)` acquires resources into the mixin's state.
- `activate(m)` / `deactivate(m)` mark the working edge. A managed node gates dispatch automatically (below), so most mixins leave both as no-ops.
- `cleanup(m)` releases what `configure` acquired.
- `on_error(m)` recovers after a hook throws on a managed transition.

The framework brackets the hooks with its own bookkeeping: a member's entities materialise immediately before its `configure` runs, its timers (created paused) start at `activate`, and its entities close immediately after `cleanup`. The hooks cover only the state the framework can't see:

```julia
import ROSNode: configure, cleanup

@mixin struct Recorder
    io::Any = nothing                       # configure opens it, cleanup closes it
end
@param Recorder path::String = "flight.log"

configure(r::Recorder) = r.io = open(parameters(r).path, "a")
cleanup(r::Recorder)   = r.io === nothing || close(r.io)

@hears "/vehicle/telemetry" function record(r::Recorder, msg::Telemetry)
    println(r.io, msg.battery)
end
```

A managed node's dispatch gate (below) holds `record` until `Active`: `configure` has already opened `r.io` when the first message lands. An unmanaged node delivers from the moment the subscription materialises, and a message can reach `record` before `configure` assigns `r.io`. Guard the field in the handler, or run the node managed, when that window matters.

Every node shares this vocabulary; `managed` chooses who drives it:

- **Unmanaged (default).** `run` / `add!` brings the node straight up: construction runs every member's `configure`, then every member's `activate`. `cleanup` runs at teardown — an explicit `close(node)`, a container's `unload_node` (containers are the second scale of composition, below), or the Context drain when the node's Context closes.
- **Managed (`managed = true`).** The node declares the lifecycle control surface (the five `lifecycle_msgs` services plus the `~/transition_event` topic) and starts `Unconfigured`. The hooks run at the real transitions — `autostart = true` runs `configure!` then `activate!` during construction — and an external orchestrator (`ros2 lifecycle set …`) or in-process calls drive them:

```julia
vehicle = run(Vehicle; ctx = ctx, name = "vehicle", managed = true, block = false)
ln = ROSNode.lifecycle(vehicle)
configure!(ln)      # → Inactive: entities materialise, each member's configure runs
activate!(ln)       # → Active: timers start, dispatch gating lifts
deactivate!(ln)     # → Inactive: members deactivate in reverse order, gating drops back
cleanup!(ln)        # → Unconfigured: each cleanup runs, entities close — a configure! starts fresh
```

Those calls drive one state machine — the same one a `ros2 lifecycle` orchestrator drives over `~/change_state`. Each transition runs the members' hooks under the [settlement three-way](../communication/services.md), so it can land its target, decline back to its origin, or divert into error processing. Step through it:

```@raw html
<div class="rosnode-statechart" data-machine="lifecycle"></div>
```

While a managed node is in any state other than `Active`, its publishers, subscriptions, timers, and services are gated at dispatch: publishers drop, subscriptions and timers don't fire, services error-reply. Action servers are gated the same way: goal, cancel, and result requests error-reply, and feedback/status publications drop. `deactivate` goes one step further and cooperatively cancels any goals still in flight — each executing body sees the cancel signal and settles `CANCELED`, and a body that ignores it is left running detached after a bounded wait — so a node bounced to `Inactive` does not keep working against state the orchestrator may have invalidated. Outbound client ports gate too: a `call`/`send` or goal dispatch from a client on a node that is not `Active` raises a catchable error, so probe `isactive(node)` first. The control surface stays live throughout, as do the parameter services and `~/get_type_description` — an orchestrator drives transitions and tunes parameters on an inactive node. `activate`/`deactivate` therefore carry only work beyond that automatic gating — pre-rolling a device, flushing a buffer.

In a multi-mixin node the hooks fan out in dependency order: `configure`, `activate`, and `on_error` run providers first, `deactivate` and `cleanup` in reverse. In `Vehicle`, the `Sensor` configures before the `Guard` it was injected into, and cleans up after it.

A hook signals failure two ways. Returning the lifecycle `failure` token rolls the transition back to its pre-transition state: the fan-out stops at the failing member, the members that already ran the forward hook unwind in reverse dependency order (a failed `activate!` deactivates them and lands `Inactive`; a failed `configure!` cleans them up, closes their ports, and lands `Unconfigured`), and the transition reports `:failure` — error processing is not entered. Throwing instead diverts to error processing. On a managed node a throw enters it:

```@raw html
<div class="rosnode-statechart" data-machine="recovery"></div>
```

Across a node's members, `on_error` runs in dependency order; the node recovers to `Unconfigured` only when every member returns cleanly, and a throwing `on_error` (logged, with the remaining members still run) drops it to `Finalized`. After the `on_error` fan-out, error processing runs the guarded member-cleanup fan-out — each member's `cleanup` hook plus its port close — so recovery to `Unconfigured` is a true reset rather than a latch over leftover state: a later `configure!` reacquires from scratch with no stale entities still dispatching and no double-acquire. An *unmanaged* node has no error processing: `run` / `add!` tears the partial node down — every member that reached `configure` cleans up against its partial state, entities close, and nothing is left on the Context — then the throw propagates.

`cleanup` runs at most once per `configure`. Teardown has several triggers — explicit `close`, `unload_node`, the shutdown transition, the Context drain — and a node can see more than one; the member's materialised entities are the guard, so the first trigger runs `cleanup` and later ones are no-ops. The framework logs a throwing `cleanup` and teardown continues: the member's entities still close and the remaining members still clean up. After `cleanup` the entities drop; the managed `cleanup!` transition runs the same guarded step, so a re-`configure!` rematerialises them.

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

## API reference

```@meta
CurrentModule = ROSNode
```

### Mixins and ports

```@docs
Component
@mixin
@param
@publishes
@uses
@every
@hears
@serves
@runs
@interface
@provides
provides
requires
construct
entities
parameters
```

### Lifecycle hooks

```@docs
configure
activate
deactivate
cleanup
on_error
```

### Assembling and running

```@docs
@node
Container
container
add!
ros_init!
```

### Node-kind registry

```@docs
register_node_kind!
node_kind
node_kinds
load_node
unload_node
list_nodes
```
