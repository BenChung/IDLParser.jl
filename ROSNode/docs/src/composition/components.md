# Components

A component is a node authored as a collection of cohesive chunks — each a typed struct of private state, the entities authored onto it, and its own lifecycle. Where the [communication](../communication/topics.md) pages wire a node by hand — open a context, create a `Node`, attach each endpoint — a component *declares* those chunks once, and the framework assembles them into a runnable node with dependency injection between them.

This page builds a `Vehicle` node from two components:

- a `Sensor` that publishes telemetry on a timer and provides a battery reading, and
- a `Guard` that requires that reading and serves a "safe to fly?" query.

## Three forms, one model

A component is a **value**: its ports, parameters, and dependency wiring form a schema tied to a state type by the [`member_schema`](@ref) trait. Three authoring forms produce that same value, so they interoperate in one node — pick by how much you want bundled:

| Form | You write | You get | Reach for it when |
|---|---|---|---|
| [`@component`](@ref) | one `mutable struct` block | the struct, its `@parameters`, the handlers, and `member_schema` | authoring a node as cohesive chunks — the common case |
| [`@schema`](@ref) | a directive block over a struct you write | `member_schema` (not the struct) | the struct or handlers must be your own — a parametric dependency consumer, a shared or externally-defined handler, a custom constructor |
| [`component`](@ref) + combinators | each port as a value, assembled by hand | a `MemberSchema` value | building or transforming schemas dynamically or programmatically |

All three share one vocabulary and one assembly: [`node`](@ref)`("name" => S, …)` composes any mix of them — toposorting dependencies — into a `NodeSchema`, and `run` brings it up. The macros are sugar over the combinators: `@schema`'s `@publishes`/`@hears`/… *are* the [`publishes`](@ref)/[`hears`](@ref)/… combinators with the port name lifted out, and `@component` adds the state struct on top.

The rest of this page leads with `@component` (the whole node in one block), drops to `@schema` (the schema over a struct you write), then to the combinators (the value primitive), and finally covers what all three share — [Dependency injection](@ref), [Port names and topics](@ref), [Services and actions](@ref), [Assembling and running the node](@ref), and the [lifecycle](lifecycle.md).

## The shared model

Every component splits its data by who may read it, and carries whatever mix it needs — both, one, or neither:

- **Private state** — the fields of the component's `mutable struct`, touched only by its own reactions and hooks.
- **Public parameters** — a separate [`@parameters`](../communication/parameters.md) struct, read live and retuned from outside through the parameter services (`ros2 param get`/`set`).

The state struct carries its member path as the type parameter `Name` and subtypes [`Component`](@ref)`{Name}`; the entity handles ride the schema, not the struct.

### The port vocabulary

A component's entities are authored as **ports** — each named, each carrying its message or service types. One vocabulary spells them across all three forms: a combinator, the matching macro directive, and the same name in schema introspection.

| Port | Combinator | Macro directive | Runs |
|---|---|---|---|
| publisher | [`publishes`](@ref)`(:n, T)` | `@publishes n::T` | `publish(entities(node, m).n, msg)` |
| subscription | [`hears`](@ref)`(:n, T, h)` | `@hears h::T` | `h(node, m, msg)` per message |
| service | [`serves`](@ref)`(:n, Req, h)` | `@serves h` | answers each request (handler contract under [Services and actions](@ref)) |
| action | [`runs`](@ref)`(:n, A, exec)` | `@runs exec` | runs each accepted goal (handler contract under [Services and actions](@ref)) |
| timer | [`every`](@ref)`(:n, rate, h)` | `@every h at rate` | `h(node, m)` at `rate` (Hz, or a `:param` to bind it live) |
| client | [`uses`](@ref)`(:n, Marker)` | `@uses n::Marker` | holds a persistent service/action client |

### Reactions and accessors

Reactions and lifecycle hooks are **node-first** — `(node, m, …)`, where `m` is the component instance. Two accessors read the node's carriers at the member's path (a constant on `m`'s type), so each is a type-stable field load:

- [`parameters`](@ref)`(node, m)` — the current parameter snapshot.
- [`entities`](@ref)`(node, m)` — the materialised entity handles.

The [`member_schema`](@ref)`(::Type{S})` trait carries a component's schema, defined on the **bare base** `S` (never an `S{Name}` instantiation). `@component` and `@schema` emit it for you; the combinator form writes it by hand.

!!! warning "`import` the framework generics — a bare `using` silently shadows them"
    The lifecycle hooks (`configure`, `activate`, `deactivate`, `cleanup`, `on_error`) and the `member_schema` trait are **ROSNode functions you add methods to**. Bring in the ones you define with `import`, not plain `using`:

    ```julia
    using ROSNode
    import ROSNode: configure, cleanup, member_schema   # the generics you extend
    ```

    Under a bare `using ROSNode`, a definition like `configure(node, m::Sensor) = …` creates a **new local `configure`** that shadows ROSNode's. Your method never runs: the framework dispatches its own generic, finds the default no-op, and emits no error or warning — the node simply comes up without your setup. (Equivalently, qualify the name at the definition: `ROSNode.configure(node, m::Sensor) = …`.)

    [`@component`](@ref) emits its `member_schema` *and* lifecycle hooks as `ROSNode.…`, so a component authored entirely with it needs no `import`. [`@schema`](@ref) emits `member_schema` for you; `import` only the lifecycle hooks you write alongside it. Reaction handlers (`tick`, `safe`) are fresh functions and need no `import`.

The components and their authored types live in a module that names the ROS package the inline-authored service belongs to; the message type is authored in its own package module:

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
    @ros_package "drone"             # names the inline-authored service type below
end
```

## The @component macro

[`@component`](@ref) authors a whole component in one `mutable struct` block — the state, parameters, ports, reactions, and lifecycle hooks together. It emits the struct, its constructor (a zero-argument one from the field defaults, or — for a `@requires` consumer — the injecting [`construct`](@ref) method), the [`@parameters`](@ref) struct, the reaction/hook methods, and [`member_schema`](@ref). A struct-body edit re-runs the block and re-keys everything together, so the pieces never drift apart.

The block reads as inline directives:

- `field = default` — a **private** state field, with the default the zero-argument constructor uses.
- `@param x::T = d ∈ lo..hi` — a **public** parameter; the macro collects these into the emitted `@parameters` struct.
- `@provides I [I2 …]` — interface(s) the component provides.
- `@requires field::Marker` — an injected dependency; the macro adds the type parameter that holds the resolved sibling and the constructor that places it (see [Dependency injection](@ref)).
- `@publishes name::T [on "wire"]` — a publisher port.
- `@hears [name =>] handler[::T] [on "wire"]` — a subscription; `handler` is an inline `function f(node, m::Sensor, msg::T) … end` or a reference `f::T` to a handler in scope.
- `@serves [name =>] handler [on "wire"]` — a service; `handler` is an inline [`@service`](@ref) definition, a reference to a pre-authored `@service` handler, or `f::Req` binding a raw handler to a request type.
- `@runs [name =>] exec [on "wire"]` — an action, the [`@action`](@ref) analog of `@serves`.
- `@every [name =>] handler at rate` (or `@every rate function f(node, m::Sensor) … end` for an inline handler) — a timer; `rate` is a frequency in Hz or a parameter `Symbol`.
- `@uses name::Marker [on "wire"]` — a persistent service/action client port.
- `configure(node, m::Sensor) = …` — lifecycle hooks live in the block too.

You type the reaction's second argument with the component yourself — `m::Sensor`, under any name — and the macro emits the handler verbatim, so dispatch is explicit in the source. Typing it dispatches the reaction on the component and keeps `entities`/`parameters` type-stable; a bare `m` is an ordinary `::Any` catch-all the macro leaves untouched (the same dispatch rules as any Julia method). A port name defaults to its handler's name; `name => handler` overrides it. A wire defaults to the port name; a trailing `on "wire"` (or, on a handler directive, a leading `"wire"` string) overrides it.

The `Sensor` carries both stores — a private `level` and a public `rate` — publishes telemetry on a timer bound to that `rate`, and provides the battery reading:

```julia
@component mutable struct Sensor{Name} <: Component{Name}
    level::Float64 = 100.0                              # private state (simulated battery %)
    @param rate::Int64 = 5 ∈ 1..50                      # public — Hz, retunable via `ros2 param`
    @provides BatterySource
    @publishes telemetry::Telemetry on "~/telemetry"    # node-private ⇒ /vehicle/telemetry
    @every :rate function tick(node, m::Sensor)         # fires at the live `rate` Hz, only while Active
        m.level = max(0.0, m.level - 1.0)
        publish(entities(node, m).telemetry, Telemetry(battery = m.level, altitude = 12.0))
    end
    configure(node, m::Sensor) = @info "Sensor up" rate = parameters(node, m).rate
end
battery(s::Sensor) = s.level     # the BatterySource impl — single-arg, so it lives OUTSIDE the block
```

A dependency **consumer** declares its needs with `@requires`. The macro writes the parametric struct and the injecting constructor, so the `Guard` is a single block too — its `battery_src` field reads type-stably as the resolved sibling:

```julia
@component mutable struct Guard{Name} <: Component{Name}
    @requires battery_src::BatterySource                # inject the sibling that provides it
    @param min_battery::Float64 = 20.0
    @service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)                      # type-stable: battery_src is the resolved sibling's type
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
end
```

## The @schema macro

[`@schema`](@ref) authors a component's [`member_schema`](@ref) with the same port directives — you write the state `mutable struct` and the lifecycle hooks as ordinary definitions, and `@schema` wires them. It emits the `member_schema` (plus a [`@parameters`](@ref) struct if you declare parameters inline, and any handler you define inline in a directive). That decoupling buys back the representation power `@component`'s single block gives up: a parametric struct, externally-defined or shared handlers, and a custom constructor.

`@schema Base [Params] begin … end` expands to `member_schema(::Type{Base}) = component(Base, [Params,] ports…; provides, requires, ctor)`. Reference an existing [`@parameters`](@ref) type as `Base P`, or declare parameters inline with `@param` (the two are mutually exclusive). The port directives, name override, and wire are exactly those of [`@component`](@ref); the binding directives differ to match the hand-written struct:

- `@requires Marker [Marker2 …]` — dependency evidence only (bare marker types); the injected field lives on your struct.
- `@ctor f` — the injecting constructor `f(node, ::Val{Name}, deps…)`. For the holder shape — the free type parameters past `Name` are exactly the injected deps, in order — the default constructor builds `Base{Name, typeof.(deps)…}(deps…)` from the type itself, so **`@ctor` is needed only for a custom shape** (see [Dependency injection](@ref)).

The `Guard` written this way keeps its own parametric struct, its `@parameters`, and its [`@service`](@ref) handler; `@schema` only wires them. Its holder shape needs no constructor:

```julia
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
    @serves   safe                                      # the pre-authored @service handler, by reference
    @requires BatterySource                             # the default ctor injects it: Guard{Name, typeof(src)}(src)
end
```

The `Sensor` authors the same way — a `mutable struct Sensor{Name}`, a `tick(node, s::Sensor)` reaction, then a block wiring them with `@publishes`, `@every tick at :rate`, and `@provides`.

## The value combinators

The combinators are the value primitive both macros target. [`component`](@ref)`(State, [Params,] ports…; provides, requires, ctor)` ties a state type and an optional parameter type to the ports built by [`publishes`](@ref)/[`hears`](@ref)/[`serves`](@ref)/[`runs`](@ref)/[`every`](@ref)/[`uses`](@ref), and you assign it to [`member_schema`](@ref) on the bare base. Reach for this directly to build or transform schemas at run time — a list of ports computed from configuration, a member chosen dynamically.

Over the same `Sensor` and `Guard` structs and handlers, `member_schema` written by hand is what `@schema` emits:

```julia
member_schema(::Type{Sensor}) = component(Sensor, SensorParams,
    publishes(:telemetry, Telemetry; on = "~/telemetry"),
    every(:tick, :rate, tick);
    provides = (BatterySource,))

member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))
```

Each combinator's first argument, `name`, is the port's identity — its key in `entities(node, m)` and, by default, the topic it addresses. [`remap`](@ref)`(Sensor, :telemetry => "…")` overrides a member's port names at the `node(…)` call; [`serves`](@ref)`(:other, safe)` and [`runs`](@ref)`(:other, ex)` rebind a pre-authored handler under another name.

## Dependency injection

`Guard` needs a battery reading a sibling owns. It asks for that capability by interface, and the framework injects a sibling that provides it — resolved once, when [`node`](@ref) assembles the kind.

An [`@interface`](@ref) names a capability — the generic functions a provider must define:

```julia
@interface BatterySource  battery(_)::Float64
```

Provision and need are **Holy-trait evidence**, matched at assembly rather than by subtyping. A provider declares the interfaces it satisfies, a consumer the ones it needs, each declared whichever way fits the form:

| | Provider declares | Consumer declares |
|---|---|---|
| `@component` | `@provides BatterySource` | `@requires field::Marker` (emits the field + constructor) |
| `@schema` | `@provides BatterySource` | `@requires Marker` + the field on your struct |
| combinators | `provides = (BatterySource,)` keyword, or [`@provides`](@ref)`/`[`provides`](@ref) on the type | `requires = (Marker,)` keyword, or [`requires`](@ref) on the type |

`Sensor` backs the contract and provides the interface; an explicit keyword takes precedence over the type-level declaration:

```julia
battery(s::Sensor) = s.level
```

The consumer receives the resolved provider through its constructor. `Guard` holds it in the type parameter `B`, so reads stay type-stable, and the assembler fixes the member's type to `Guard{:guard, Sensor{:sensor}}`. For that **holder shape** — the free parameters past `Name` are exactly the injected deps — the default [`construct`](@ref) builds the component from its own type, so no constructor is written. Override it for any other shape (named fields, extra state, a different parameter order) with a [`construct`](@ref) method, the `ctor =` keyword, or `@schema`'s `@ctor`. [Parametric Components](parametric.md) covers the constructor contract, mocks, and standalone runs.

## Port names and topics

A port's `name` is its identity *and*, by default, the topic it addresses (its service or action name, for `serves`/`runs`/`uses`). The name resolves through a short chain:

1. the default is the port's own `name`, taken as a relative name;
2. an `on = "/some/name"` clause (or `@publishes … on "…"`) overrides that default;
3. a [`remap`](@ref) at the `node(…)` call overrides it again;
4. the chosen name resolves against the node's namespace by the [standard ROS rules](https://design.ros2.org/articles/topic_and_service_names.html):
   - a relative name (`foo`) resolves under the node's namespace — `/foo`;
   - a private name (`~/foo`) resolves under the node's own name — `/vehicle/foo`;
   - an absolute name (`/foo`) stands as written.

Timers are the exception: they address no topic and fire locally. Two ports connect only when they resolve to the same name — a mismatched pair silently misses, and two same-channel outputs on one name error when the node starts (at `run`) unless one was [`remap`](@ref)-ed. [Inspecting the resolved wiring](@ref) shows how to confirm a wiring before chasing a silent non-delivery.

## Services and actions

A service answers a request with a response; an action runs a goal to a result alongside a feedback stream. Each needs a typed contract, created two ways:

- **Author it inline** — [`@service`](@ref) / [`@action`](@ref) define the contract *and* the handler in one place. The arguments after `(node, m)` are the request (or goal) fields; the `@NamedTuple` return is the response (or result). The macro generates and registers the wire types, and the handler keeps its function name, so it drops straight into a port (`@serves safe`, `serves(safe)`, or a bare `safe` in `component(…)`).
- **Wire a pre-authored type** — `serves(:name, ReqType, handler)` / `runs(:name, Action, exec)` bind a handler to a service or action type imported from another package or authored elsewhere.

The handler's call contract follows how the type was supplied:

- an inline [`@service`](@ref)/[`@action`](@ref) handler, or one referenced by name (`@serves safe`, `serves(safe)`), receives the request (or goal) fields **splatted** — `safe(node, m, target_altitude)` — paired with the `@NamedTuple` it returns;
- a pre-authored type wired with `serves(:n, Req, h)` / `runs(:n, A, exec)` receives the request **object** / a goal handle — `h(node, m, req)` / `exec(node, m, goal)`.

```julia
@service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
    b = battery(g.battery_src)
    (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
end
```

The leading `"~/safe_to_fly"` sets the service name. [Services](../communication/services.md) and [Actions](../communication/actions.md) cover the request/response and goal/result/feedback models; the [`@service`](@ref)/[`@action`](@ref) docstrings cover rebinding a handler under another name.

## Assembling and running the node

[`node`](@ref) assembles the kind from a list of members. Each `"name" => S` pair gives a component its namespace within the node, accepting any of the three forms — a state type whose `member_schema` is defined, or a `component(…)`/`@schema` schema directly:

```julia
const Vehicle = node("sensor" => Drone.Sensor, "guard" => Drone.Guard; name = "Vehicle")
```

`Guard` requires `BatterySource` and `Sensor` provides it, so the `node` call:

- toposorts the members and injects the `Sensor` into the `Guard`;
- orders lifecycle so a dependency configures before its dependent and tears down after;
- registers the kind under `name = "Vehicle"`, so a container can load it by name (see [Containers & Dynamic Composition](containers.md));
- freezes the member order and dependency edges into the returned schema's type, so construction is type-stable.

`run` brings the node up:

- `block = false` returns immediately so the caller can drive the node;
- `ctx = ctx` runs it on an existing context (omit `ctx` to run standalone in its own process);
- `overrides = (…,)` sets each member's parameters by their local names.

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

The ground station is a plain node subscribing to `/vehicle/telemetry`. The vehicle publishes telemetry at 5 Hz, and the guard answers the `~/safe_to_fly` query its `@service` authored. `run(S)` also promotes a single component to its own node directly — `run(Sensor; name = "sensor")` — with un-prefixed parameters.

## Inspecting the resolved wiring

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

Composing it into a node shows the resolved dependency order and each member's ports:

```@example schema
rig = node("beacon" => Beacon; name = "Rig")
```

[`describe_wiring`](@ref) goes one step further: on a *built* node it appends the fully-qualified ROS name each port resolves to (here `~/beat` private → `/rig/beat`, `/clock` absolute → `/clock`):

```@example schema
cn = run(rig; name = "rig", localhost_only = true, block = false)
describe_wiring(cn)
close(cn)
```

It is the quick way to confirm two ports share a name before chasing a silent non-delivery. The resolved name is what the entity uses as a Zenoh key expression; see [Addressing & Key Expressions](../foundations/addressing.md).

## Lifecycle

Every component has a lifecycle: setup in `configure`, the matching release in `cleanup`, and `activate`/`deactivate`/`on_error` for the rest. A component runs unmanaged (autostarted) or managed (driven through the lifecycle state machine, with a dispatch gate that silences its entities until the node is active). [Component Lifecycle](lifecycle.md) is the full treatment — the hook set, the managed state machine, the gate, and failure/recovery.

## Composition at two scales

`run(Vehicle; name = "vehicle")` gives the node its own process, with its own session, discovery, and registry. A `container` instead runs several nodes in one process — sharing one session, discovery, and a direct in-process delivery path between them — and loads a registered kind by name (the `ros2 component load` path). The same schema runs either way; deploy-time wiring picks the scale. [Containers & Dynamic Composition](containers.md) covers containers, by-name loading, and inspecting a composed node.

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
@schema
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
