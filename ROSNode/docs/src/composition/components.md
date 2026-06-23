# Components

A component is a node authored as a collection of cohesive chunks — each a typed struct of private state, the entities authored onto it, and its own lifecycle. A component assembles the node from these chunks and runs it, where the other pages wire a node imperatively — a context, a `Node`, and each endpoint attached by hand.

Components can be built in two ways:

- [The @component macro](@ref) with **[`@component`](@ref)** — one `mutable struct` block declares the state, parameters, ports, reactions, and lifecycle hooks together.
- [The member schema](@ref) with **[`component`](@ref) and the value combinators** — write the pieces separately: `publishes`/`hears`/`serves`/`runs`/`every`/`uses` author the entities, and `component(State, Params, ports…)` binds them to a state type through the `member_schema` trait. This is the primitive `@component` expands to; reach for it directly for dynamic or programmatic assembly.

Either way, `node("name" => State, …)` composes the components with dependency injection into one runnable `NodeSchema` and `run(schema)` brings the node up.

This page builds a `Vehicle` node from two explicit combinator-built components, then recaps it as one [`@component`](@ref) block. The `Vehicle` is built from two parts:

* A `Sensor` that publishes telemetry on a timer and provides a battery reading, and
* a `Guard` which enforces a sensed minimum battery level and that serves a "safe to fly?" query. 

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
    @ros_package "drone"
    import ROSNode: configure, member_schema
end
```

A component splits its data into two independent stores, and each datum goes to the one matching who may read it:

- **Private state** — the fields of the component's `mutable struct`. Internal to the component; touched only by its own reactions and hooks.
- **Public parameters** — a separate `@parameters` struct. Read live and retuned from outside through the [parameter](../communication/parameters.md) services (`ros2 param get`/`set`).

The two are independent, so a component carries whatever mix it needs — both, one, or neither. The `Sensor` carries both: a private `level` and a public `rate`.

The state struct is parameterised by its member-path `Name` and subtypes `Component{Name}`; the entities ride the schema, not the struct. A zero-argument ctor `S{Name}()` builds it with no dependencies — the default the framework calls for a component with no `requires`:

```julia
mutable struct Sensor{Name} <: Component{Name}
    level::Float64                          # simulated
end
Sensor{Name}() where {Name} = Sensor{Name}(100.0)
```

The `@parameters` struct holds the public side, read through `parameters(node, s)`. The `∈ 1..50` constraint reuses the [parameter schema grammar](../communication/parameters.md):

```julia
@parameters struct SensorParams
    rate::Int64 = 5 ∈ 1..50                 # Hz
end
```

`component(State, Params, …)` (below) binds both stores onto the component; `component(State, …)` with the `Params` argument dropped is a component with private state and no public parameters.

Reactions and lifecycle hooks are **node-first**: they take `(node, m, …)`, where `m` is the component instance. The two reflective accessors take both — `parameters(node, s)` returns the current, type-stable parameter snapshot, and `entities(node, s)` returns the materialised entity handles. The member's path is a constant on its type, so `parameters(node, s).rate` and `entities(node, s).telemetry` are ordinary typed field loads off the node's carriers.

`configure` is a lifecycle hook — a plain method run at startup; the [Component Lifecycle](lifecycle.md) page covers the full set. A reaction handler is an ordinary function dispatched on the component base:

```julia
configure(node, s::Sensor) = @info "Sensor up" rate = parameters(node, s).rate

function tick(node, s::Sensor)              # fires at `rate` Hz, only while Active
    s.level = max(0.0, s.level - 1.0)
    publish(entities(node, s).telemetry, Telemetry(battery = s.level, altitude = 12.0))
end
```

## The member schema

[`member_schema`](@ref)`(::Type{S})` is the trait carrying the component's schema. It is defined on the **bare base** `S` (never a `S{Name}` instantiation), and [`component`](@ref)`(State, Params, ports…)` ties the state and parameter types to the authored ports. Each value combinator builds one port from a `name` plus its specifics; the full signatures and options live in [Schema combinators](@ref):

- [`publishes`](@ref)`(:telemetry, Telemetry; on)` — a publisher carrying a message type.
- [`hears`](@ref)`(:odom, Odometry, on_odom; on)` — a subscription running a handler per message.
- [`serves`](@ref)`(:safe, SafeReq, safe; on)` — a service-server answering a request.
- [`runs`](@ref)`(:dock, Dock, dock!; on)` — an action-server running each accepted goal.
- [`every`](@ref)`(:tick, :rate, tick)` — a timer firing a reaction (a frequency in Hz, or a parameter to bind it live).
- [`uses`](@ref)`(:cmd, CmdReq; on)` — a persistent service/action **client** port.
- [`remap`](@ref)`(Sensor, :telemetry => "…")` — override the names a member's ports resolve to, at `node(…)`.

`every(:tick, :rate, tick)` binds the timer's frequency to the `rate` parameter:

```julia
member_schema(::Type{Sensor}) = component(Sensor, SensorParams,
    publishes(:telemetry, Telemetry; on = "~/telemetry"),   # node-private ⇒ /vehicle/telemetry
    every(:tick, :rate, tick);
    provides = (BatterySource,))
```

The `provides = (BatterySource,)` keyword is the dependency-injection evidence covered below.

### Port names and topics

A combinator's first argument, `name`, is the port's **identity** — its key in `entities(node, m)`, and (for the reaction combinators) the handler's name. That same `name` is also, by default, the **topic** the port uses (its service or action name, for `serves`/`runs`/`uses`). It resolves through a short chain:

1. the default is the port's own `name`, taken as a relative name;
2. an `on = "/some/name"` clause overrides that default;
3. a `node` [`remap`](@ref) overrides it again;
4. the chosen name resolves against the node's namespace to its fully-qualified form — see [Inspecting the resolved wiring](@ref).

Timers are the exception: they address no topic and fire locally.

### Inspecting a schema

A schema is a plain value — build one and look at it before running anything. A `member_schema`, and a `node(…)` schema, print their structure and each port's authored name:

```@example schema
using ROSNode
import ROSNode: member_schema
const Time = ROSNode.Interfaces.builtin_interfaces.msg.Time

mutable struct Beacon{Name} <: Component{Name}
    seq::Int
end
Beacon{Name}() where {Name} = Beacon{Name}(0)

pulse(node, b::Beacon) = nothing
on_clock(node, b::Beacon, msg::Time) = nothing

member_schema(::Type{Beacon}) = component(Beacon,
    publishes(:beat, Time; on = "~/beat"),         # private
    hears(:clock, Time, on_clock; on = "/clock"),  # absolute
    every(:pulse, 2.0, pulse))

member_schema(Beacon)
```

Composing it into a node shows the resolved DI order and each member's ports:

```@example schema
rig = node("beacon" => Beacon; name = "Rig")
```

[`describe_wiring`](@ref) goes one step further: on a *built* node it appends the fully-qualified ROS name each port resolves to (here `~/beat` private → `/rig/beat`, `/clock` absolute → `/clock`):

```@example schema
cn = run(rig; name = "rig", localhost_only = true, block = false)
describe_wiring(cn)
close(cn)
```

## Services and actions

Services and actions are the **imperative** ports: a request earns a response, a goal runs to a result. Each needs a typed request/response (or goal/result/feedback) contract, and you create one two ways:

- **Author it inline** — [`@service`](@ref) / [`@action`](@ref) define the contract *and* the handler in one place. The arguments after `(node, m)` are the request (or goal) fields; the `@NamedTuple` return is the response (or result).
- **Wire a pre-authored type** — `serves(:name, ReqType, handler)` / `runs(:name, Action, exec)` bind a handler to a service or action type you already have, imported from another package or authored elsewhere.

Inline authoring keeps the whole port in one definition:

```julia
@service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
    b = battery(g.battery_src)
    (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
end
```

The leading `"~/safe_to_fly"` sets the service name; the handler keeps its function name `safe`, which drops straight into `component(…)` as a port:

```julia
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe;
    requires = (BatterySource,), ctor = make_guard)
```

[`@action`](@ref) authors an action server the same way, with a feedback stream alongside the goal and result. Both macros generate and register the wire types for you, and either handler rebinds under another name — the [`@service`](@ref)/[`@action`](@ref) docstrings cover those forms, and [Services](../communication/services.md) / [Actions](../communication/actions.md) the request/response and goal/result/feedback models.

## Dependency injection between components

`Guard` needs a battery reading that a sibling owns. It asks for that capability by interface, and the framework injects a sibling that provides it.

An [`@interface`](@ref) names a capability — the generic functions a provider must define:

```julia
@interface BatterySource  battery(_)::Float64
```

`Sensor` provides it by backing the contract method and listing the interface in its `provides=`:

```julia
battery(s::Sensor) = s.level
```

Provision is **Holy-trait evidence**: a component declares the interfaces it provides, resolved against a consumer's `requires` rather than by subtyping. Declare it two ways:

- **on the `component(…)` call** — the `provides = (BatterySource,)` keyword;
- **on the type** — a [`@provides`](@ref) declaration (`@provides Sensor BatterySource`), sugar for a [`provides`](@ref)`(::Type{Sensor})` method.

The keyword takes precedence; the method is the fallback, used when the keyword is omitted. A consumer declares its matching need with `requires=` (or, the same two ways, a [`requires`](@ref)`(::Type{M})` method) and receives the resolved provider through its constructor. `Guard` holds that provider in a type parameter so reads of it are type-stable, and supplies a constructor that places its own member name:

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

## The @component macro

[`@component`](@ref) is the concise authoring tier: it emits the whole value API — the struct, the zero-arg ctor, the `@parameters` struct, the reaction/hook methods, and `member_schema` — from one `mutable struct` block. A struct-body edit re-runs the whole block and re-keys everything together (Revise-friendlier than the separate definitions). It is sugar over `component`/`node`; the raw combinators remain the primitive, and the two coexist in one `node`.

The block uses inline directives:

- `field = default` — a **private** state field (a plain `mutable struct` field), with the default the zero-arg ctor uses.
- `@param x::T = d ∈ lo..hi` — a **public** parameter; the macro collects these into an emitted `@parameters struct`. Mix the two freely: a block carries any number of each.
- `@provides Iface` — interface(s) the component provides.
- `@requires src::Marker` — an injected dependency, where `Marker` is a concrete component type or an `@interface`. The macro adds the type parameter that holds the resolved sibling and the constructor that places it, so the field reads type-stably and you write no parametric struct or ctor by hand.
- `@publishes out::T on "~/topic"` — a publisher port (the `on` clause **trails** the declaration).
- `@hears function h(node, m, msg::T) … end` — a subscription port plus its handler (inline-only).
- `@every :rate function tick(node, m) … end` — a timer port plus its handler (`rate` = Hz or a parameter `Symbol`; inline-only).
- `@service "~/s" function f(node, m, x::X)::@NamedTuple{…} … end` — inline service authoring.
- `configure(node, m) = …` — lifecycle hooks live in the block too.

A bare `m` argument is annotated `m::Sensor` for you, so reaction bodies stay fully typed. The handler directives `@hears`/`@service`/`@action` take a **leading** name string, while `@publishes` takes the **trailing** `on "…"` form — the split follows the name source.

```julia
@component mutable struct Sensor{Name} <: Component{Name}
    level::Float64 = 100.0                              # simulated battery %
    @param rate::Int64 = 5 ∈ 1..50                      # Hz
    @provides BatterySource
    @publishes telemetry::Telemetry on "~/telemetry"
    @every :rate function tick(node, m)                 # fires at the live `rate` Hz, only while Active
        m.level = max(0.0, m.level - 1.0)
        publish(entities(node, m).telemetry, Telemetry(battery = m.level, altitude = 12.0))
    end
    configure(node, m) = @info "Sensor up" rate = parameters(node, m).rate
end
# A non-node-first interface impl lives OUTSIDE the block (a struct edit would orphan it):
battery(s::Sensor) = s.level
```

A DI **consumer** declares its dependencies with `@requires` — the `Guard` from [Dependency injection between components](@ref), authored without the hand-written parametric struct or constructor:

```julia
@component mutable struct Guard{Name} <: Component{Name}
    @requires battery_src::BatterySource        # inject the sibling that provides it
    @param min_battery::Float64 = 20.0
    @service "~/safe_to_fly" function safe(node, g, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)               # type-stable: battery_src is the resolved sibling's type
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
end
```

`@requires battery_src::Marker` adds the type parameter that holds the resolved sibling and the `construct` that places it; `Marker` is an `@interface` (as here) or a concrete component type. A **client** port (`uses`) still uses the raw value API — `@uses` in the block errors clearly, pointing at it. The macro and the raw combinators compose in one `node`: the `@component`-authored `Sensor` and `Guard` above assemble unchanged.

## Assembling and running the node

[`node`](@ref) assembles the node kind from a list of members. Each `"name" => State` pair gives the component a namespace within the node, and the framework resolves DI once, here:

```julia
const Vehicle = node("sensor" => Drone.Sensor, "guard" => Drone.Guard; name = "Vehicle")
```

`Guard` requires `BatterySource` and `Sensor` provides it, so the `node` call:

- toposorts the members by dependency and injects the `Sensor` into the `Guard`;
- orders lifecycle so a dependency configures before its dependent and tears down after;
- registers the kind under `name = "Vehicle"` in the process-global registry, so a container can load it by name (see [Containers & Dynamic Composition](containers.md));
- freezes the member order and dependency edges into the returned `NodeSchema`'s type, so construction is type-stable.

`run` brings the node up. Its options:

- `block = false` — returns immediately so the caller can drive the node.
- `ctx = ctx` — runs the node on an existing context; omit `ctx` to run it standalone in its own process.
- `overrides = (…,)` — sets each member's parameters by their local names.

```julia
@context() do ctx
    ground = Node(ctx, "ground")
    Subscription(ground, "/vehicle/telemetry", Msgs.Telemetry) do msg
        @info "telemetry" battery = round(msg.battery; digits = 1) altitude = msg.altitude
    end

    vehicle = run(Vehicle; ctx = ctx, name = "vehicle",
                  overrides = (rate = 5, min_battery = 90.0), block = false)
    sleep(1.2)                              # let a few telemetry messages arrive before the query

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

The last step of the chain in [Port names and topics](@ref) resolves a port's name against the node's namespace, by the [standard ROS rules](https://design.ros2.org/articles/topic_and_service_names.html):

- a relative name (`foo`) resolves under the node's namespace — `/foo`;
- a private name (`~/foo`) resolves under the node's own name — `/vehicle/foo`;
- an absolute name (`/foo`) stands as written.

[`describe_wiring`](@ref) on a built node prints each port's authored name and the resolved name beside it — the [Inspecting a schema](@ref) demo runs it live. It is the quick way to confirm two ports share a name before chasing a silent non-delivery. The resolved name is what the entity uses as a Zenoh key expression; see [Addressing & Key Expressions](../foundations/addressing.md).

Two ports connect only when they resolve to the same name:

- **Mismatched names silently miss.** A `hears(:foo, …)` (relative `foo` → `/foo`) and a `publishes(:p, …; on = "~/foo")` (private → `/vehicle/foo`) land on different topics, so they never connect.
- **Colliding names error.** A `node` call errors when two same-channel outputs resolve to one name, unless one was explicitly remapped — [`remap`](@ref) resolves the clash.

## Lifecycle

Every component has a lifecycle: setup in `configure`, the matching release in `cleanup`, and `activate`/`deactivate`/`on_error` for the rest. A component runs unmanaged or managed. [Component Lifecycle](lifecycle.md) is the full treatment: the hook set, the managed state machine, the dispatch gate, and failure/recovery.

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

### Schema combinators

```@docs
component
publishes
hears
serves
runs
every
uses
remap
```

### Ports and entities

```@docs
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
