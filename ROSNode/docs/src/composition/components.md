# Components

A component is a node authored as a collection of cohesive chunks — each a typed struct of private state, the entities authored onto it, and its own lifecycle. Where the other pages wire a node imperatively (open a context, create a `Node`, attach a publisher here, a service there), a component assembles the node from these chunks and runs it.

The assembly is a **schema value**. The value combinators `publishes`/`every`/`serves`/`hears`/`runs`/`uses` author the entities; `component(State, Params, ports…; provides=/requires=/ctor=)` ties them to a state type through the `member_schema` trait; and `node("name" => State, …)` composes the members — resolving dependency injection once — into one runnable `NodeSchema`. `run(schema)` brings the node up.

This page assembles a `Vehicle` node from two components. A `Sensor` publishes telemetry on a timer and provides a battery reading; a `Guard` requires that reading and serves a "safe to fly?" query. `node` composes them; `run` brings the node up. The full example lives in `examples/component.jl`, and `examples/component_macro.jl` authors the same node with the [`@component`](@ref) macro.

!!! warning "`import` the framework generics — a bare `using` silently shadows them"
    The lifecycle hooks (`configure`, `activate`, `deactivate`, `cleanup`, `on_error`) and the `member_schema` trait are **ROSNode functions you add methods to**. Bring them in with `import`, not plain `using`:

    ```julia
    using ROSNode
    import ROSNode: configure, cleanup, member_schema   # the generics you extend
    ```

    Under a bare `using ROSNode`, a definition like `configure(node, m::Sensor) = …` creates a **new local `configure`** that shadows ROSNode's. Your method is never called: the framework dispatches its own generic, runs the default no-op, and there is **no error or warning** — the node simply comes up without your setup. (Equivalently, qualify the name at the definition: `ROSNode.configure(node, m::Sensor) = …`.)

    Reaction handlers (`tick`, `safe`) are fresh functions, so they need no import. The [`@component`](@ref) macro emits its `member_schema` and lifecycle methods as `ROSNode.…` for you, so a component authored entirely with the macro needs no import either.

## The component — state plus the entities authored onto it

The components and their authored types live in a module that names the ROS package the inline-authored service belongs to; the message type is authored in its own package module. `run` and the ground-station client see them through `Drone.*` and `Msgs.Telemetry`:

```julia
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
    @ros_package "drone"            # names the inline-authored service type below
    import ROSNode: configure, member_schema   # the generics this module extends
    # ── components authored below ──
end
```

A component's state is a `mutable struct` parameterised by its member-path `Name` and subtyping `Component{Name}`. The struct holds only private state; the entities ride the schema, not the struct. A zero-argument ctor `S{Name}()` builds it with no dependencies — the default the framework calls when nothing is injected:

```julia
mutable struct Sensor{Name} <: Component{Name}
    level::Float64                          # private state (simulated battery %)
end
Sensor{Name}() where {Name} = Sensor{Name}(100.0)
```

A `@parameters` struct declares the component's live parameters, read through `parameters(node, s)` and driveable from the [parameter](../communication/parameters.md) services. The `∈ 1..50` constraint reuses the parameter schema grammar:

```julia
@parameters struct SensorParams
    rate::Int64 = 5 ∈ 1..50                 # Hz — read live via parameters(node, s).rate
end
```

Reactions and lifecycle hooks are **node-first**: they take `(node, m, …)`, where `m` is the component instance. The two reflective accessors take both — `parameters(node, s)` returns the current, type-stable parameter snapshot, and `entities(node, s)` returns the materialised entity handles. The member's path is a constant on its type, so `parameters(node, s).rate` and `entities(node, s).telemetry` are ordinary typed field loads off the node's carriers.

`configure` is a lifecycle hook — a plain method run at startup; the [Component Lifecycle](lifecycle.md) page covers the full set. A reaction handler is an ordinary function dispatched on the component base:

```julia
configure(node, s::Sensor) = @info "Sensor up" rate = parameters(node, s).rate

function tick(node, s::Sensor)              # fires at `rate` Hz, only while Active
    s.level = max(0.0, s.level - 1.0)       # drain a little each tick
    publish(entities(node, s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
end
```

## The schema — `member_schema` and the value combinators

`member_schema(::Type{S})` is the trait carrying the component's schema. It is defined on the **bare base** `S` (never a `S{Name}` instantiation), and `component(State, Params, ports…)` ties the state and parameter types to the authored ports:

- `publishes(:name, T; on)` — a publisher port carrying message type `T`.
- `hears(:name, T, handler; on)` — a subscription dispatching `handler` per message.
- `serves(:name, ReqType, handler; on)` — a service over a request type.
- `every(:name, rate, handler)` — a timer; `rate` is a frequency in Hz or a parameter `Symbol` to bind it live.
- `runs(:name, Action, exec; on)` — an action server over a pre-authored action type.
- [`uses(:name, marker; on)`](@ref uses) — a persistent service/action **client** port.

A port's `on` clause sets its wire name; with none, the wire defaults to the port/reaction name. The `~/` prefix makes a topic node-private, so `~/telemetry` resolves against the node's name to `/vehicle/telemetry`. `every(:tick, :rate, tick)` binds the timer's frequency to the `rate` parameter:

```julia
member_schema(::Type{Sensor}) = component(Sensor, SensorParams,
    publishes(:telemetry, Telemetry; on = "~/telemetry"),   # node-private ⇒ /vehicle/telemetry
    every(:tick, :rate, tick);
    provides = (BatterySource,))
```

The `provides = (BatterySource,)` keyword is the dependency-injection evidence covered below.

## Inline-authored entities

A bare `serves`/`hears` wires a **pre-authored** request or message type to a handler. [`@service`](@ref) instead authors the ROS service **type** and the handler in one place: the arguments after `(node, m)` are the request fields, and the `@NamedTuple` return type is the response. The macro generates `f_Request`/`f_Response` from that signature and registers them. See [Services](../communication/services.md) for the service model:

```julia
@service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
    b = battery(g.battery_src)              # reads the Sensor through the interface
    (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
end
```

The leading string `"~/safe_to_fly"` names the service wire; the handler keeps the function name `safe`. The `safe` marker drops straight into `component(…)` as a port — `component` converts a bare `@service`/`@action` handler into its descriptor automatically:

```julia
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe;
    requires = (BatterySource,), ctor = make_guard)
```

[`@action`](@ref) authors an action server the same way, deriving the Goal/Result/Feedback types from the signature and a `FeedbackSink` parameter. Drop down a tier with `serves(:other, safe; on)` to reuse the same handler under another name, or `serves(:n, safe_Request, h; on)` to wire a different handler to the authored type.

## Dependency injection between components

`Guard` needs a battery reading that a sibling owns. It asks for that capability by interface, and the framework injects a sibling that provides it.

An [`@interface`](@ref) names a capability — the generic functions a provider must define:

```julia
@interface BatterySource  battery(_)::Float64
```

`Sensor` provides it by backing the contract method and listing the interface in its `provides=`:

```julia
battery(s::Sensor) = s.level                # satisfy the BatterySource contract
# … provides = (BatterySource,) in Sensor's component(…) above
```

Provision is Holy-trait evidence — a `provides=` listing (equivalently a [`@provides`](@ref) / [`provides`](@ref) declaration), resolved against need rather than by subtyping. A consumer declares the matching need in `requires=` and receives the resolved provider through its constructor. `Guard` holds that provider in a type parameter so reads of it are type-stable, and supplies a constructor that places its own member name:

```julia
mutable struct Guard{Name, B} <: Component{Name}
    battery_src::B                          # the injected sibling provider
end
@parameters struct GuardParams
    min_battery::Float64 = 20.0
end
make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
```

The `requires = (BatterySource,)` plus `ctor = make_guard` in `Guard`'s `member_schema` above wire the need to the constructor. The single dependency arrives as `src`, fixing the member's type to `Guard{:guard, Sensor{:sensor}}`; the handler reads it through the interface — `battery(g.battery_src)` in the `safe` service. [Parametric Components](parametric.md) covers the constructor contract in full, injecting a mock provider in tests, and running a dependent component standalone.

## The `@component` macro — the same component in one block

[`@component`](@ref) is the concise authoring tier: it emits the whole value API — the struct, the zero-arg ctor, the `@parameters` struct, the reaction/hook methods, and `member_schema` — from one `mutable struct` block. A struct-body edit re-runs the whole block and re-keys everything together (Revise-friendlier than the separate definitions). It is sugar over `component`/`node`; the raw combinators remain the primitive, and the two coexist in one `node`.

The block uses inline directives:

- `field = default` — a struct field with the default the zero-arg ctor uses.
- `@param x::T = d ∈ lo..hi` — a parameter; the macro collects these into an emitted `@parameters struct`.
- `@provides Iface` — interface(s) the component provides.
- `@publishes out::T on "~/wire"` — a publisher port (the wire clause **trails** the declaration).
- `@hears function h(node, m, msg::T) … end` — a subscription port plus its handler (inline-only).
- `@every :rate function tick(node, m) … end` — a timer port plus its handler (`rate` = Hz or a parameter `Symbol`; inline-only).
- `@service "~/s" function f(node, m, x::X)::@NamedTuple{…} … end` — inline service authoring.
- `configure(node, m) = …` — lifecycle hooks live in the block too.

A bare `m` argument is annotated `m::Sensor` for you, so reaction bodies stay fully typed. The handler directives `@hears`/`@service`/`@action` take a **leading** `"wire"` string, while `@publishes` takes the **trailing** `on "wire"` form — the split follows the name source.

```julia
@component mutable struct Sensor{Name} <: Component{Name}
    level::Float64 = 100.0                              # private state (simulated battery %)
    @param rate::Int64 = 5 ∈ 1..50                      # Hz — read live; driveable by `ros2 param`
    @provides BatterySource                             # satisfies the BatterySource contract
    @publishes telemetry::Telemetry on "~/telemetry"    # node-private ⇒ /vehicle/telemetry
    @every :rate function tick(node, m)                 # fires at the live `rate` Hz, only while Active
        m.level = max(0.0, m.level - 1.0)               # drain a little each tick
        publish(entities(node, m).telemetry, Telemetry(battery = m.level, altitude = 12.0))
    end
    configure(node, m) = @info "Sensor up" rate = parameters(node, m).rate
end
# A non-node-first interface impl lives OUTSIDE the block (a struct edit would orphan it):
battery(s::Sensor) = s.level
```

`@component` v1 covers the DI-free common case plus `@provides`. A DI **consumer** (`@requires` + an injected ctor, see [Parametric Components](parametric.md)) or a client port stays on the raw `component(M, …; requires=(I,), ctor=f)` / `uses(:n, marker)` API — `@requires`/`@uses` in the block error clearly, pointing at the raw form. The two compose seamlessly in the same `node`: the `Sensor` above and a raw-API `Guard` assemble into one node unchanged.

## Assembling and running the node

[`node`](@ref) assembles the node kind from a list of members. Each `"name" => State` pair gives the component a namespace within the node, and DI is resolved once, here:

```julia
const Vehicle = node("sensor" => Drone.Sensor, "guard" => Drone.Guard; name = "Vehicle")
```

`Guard` requires `BatterySource` and `Sensor` provides it, so the framework toposorts the members by dependency and injects the `Sensor` into the `Guard`. A dependency configures before its dependent and tears down after. `name = "Vehicle"` registers the kind in the process-global registry, so a container can load it by name (see [Containers & Dynamic Composition](containers.md)); the resolved member order and dependency edges are frozen into the returned `NodeSchema`'s type, so construction is type-stable.

`run` brings the node up. `block = false` returns so you can drive the node from the calling code; passing `ctx` runs it on an existing context, and dropping `ctx` runs it standalone in its own process. `overrides` sets each member's parameters by their local names:

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

The ground station is a plain node subscribing to `/vehicle/telemetry`. The vehicle publishes telemetry at 5 Hz, and the guard answers the `~/safe_to_fly` query its `@service` authored.

## Inspecting the resolved wiring

A port's **wire name** defaults to the name you wrote:

- `hears`, `serves`, `every`, `runs` — the reaction's name.
- `publishes`, `uses` — the declared port name.

An `on "topic"` clause or a [`node`](@ref) `remap` overrides that default. The wire name then resolves against the node's namespace to the topic on the wire — a relative `foo` on node `/vehicle` lands on `/foo`:

```
hears(:foo, …)   →   wire name  foo   →   topic  /foo
```

`describe_wiring` prints each member's ports and the fully-qualified ROS name each resolves to — the quick way to confirm two ports share a name before chasing a silent non-delivery:

```julia
describe_wiring(vehicle)
```

```
wiring of /vehicle — 2 member(s)
  sensor :: Sensor
    telemetry         pub   ~/telemetry            → /vehicle/telemetry
    tick              timer
  guard :: Guard
    safe              srv   ~/safe_to_fly          → /vehicle/safe_to_fly
```

The middle column is each port's authored wire name (after any `remap`), the arrow its resolved name. A name resolves against the node by the [standard ROS rules](https://design.ros2.org/articles/topic_and_service_names.html):

- a relative name (`foo`) resolves under the node's namespace — `/foo`;
- a private name (`~/foo`) resolves under the node's own name — `/vehicle/foo`;
- an absolute name (`/foo`) stands as written.

The resolved name is what the entity uses on the wire as a Zenoh key expression; see [Addressing & Key Expressions](../foundations/addressing.md) for the keyexpr it becomes.

Two ports connect only when they resolve to the same name. A `hears(:foo, …)` (relative `foo`) and a `publishes(:p, …; on = "~/foo")` (private) therefore land on different topics — `describe_wiring` shows the split as `→ /foo` against `→ /vehicle/foo`. A `node` call also errors if two same-channel outputs collide on one name unless one was explicitly remapped — `remap(Sensor, :telemetry => "…")` resolves the clash.

## Lifecycle

Every component has a lifecycle: the `configure`/`activate`/`deactivate`/`cleanup`/`on_error` hooks, run **unmanaged** (brought straight up at `run`) or **managed** (driven through the `lifecycle_msgs` control surface, with the `isactive` gate holding dispatch until `Active`). A component that wraps an external resource authors its setup in `configure` and the matching release in `cleanup`. [Component Lifecycle](lifecycle.md) is the full treatment: the hook set, the managed state machine, the dispatch gate, and failure/recovery.

## Composition at two scales

`run(Vehicle; name = "vehicle")` gives the node its own process, with its own session, discovery, and registry. A `container` instead runs several nodes in one process — sharing one session, discovery, and a direct in-process delivery path between them — and loads a registered kind by name (`load_node`, the `ros2 component load` path). The same schema runs either way; deploy-time wiring picks the scale. [Containers & Dynamic Composition](containers.md) covers containers, by-name loading, and inspecting a composed node.

## See also

- [Parametric Components](parametric.md) — type parameters beyond `Name`, the dependency-injection constructor contract, mocks, and standalone runs.
- [Component Lifecycle](lifecycle.md) — the hook set, managed nodes, and the dispatch gate.
- [Containers & Dynamic Composition](containers.md) — running several nodes in one process and loading a kind by name.
- [Parameters](../communication/parameters.md) — the live parameter model `@parameters` builds on.
- [Precompilation & Warm-up](../advanced/precompilation.md) — baking a component's first-`run` path with `precompile_node`.
- [Interoperating with ROS 2](../interop/ros2.md) — driving a component's parameters, topics, and services from `ros2` tooling.

## API reference

```@meta
CurrentModule = ROSNode
```

### Authoring a component

```@docs
Component
@component
member_schema
```

### Ports and entities

```@docs
uses
@service
@action
entities
parameters
```

### Interfaces and dependency injection

```@docs
@interface
@provides
provides
requires
```

The dependency-injection constructor ([`construct`](@ref)) is documented under [Parametric Components](parametric.md); the lifecycle hooks under [Component Lifecycle](lifecycle.md); containers, the node-kind registry, and schema introspection under [Containers & Dynamic Composition](containers.md).

### Assembling a node

```@docs
node
describe_wiring
```
