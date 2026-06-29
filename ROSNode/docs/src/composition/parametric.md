# Parametric Components

A component carries its member path as the type parameter `Name`. A **parametric** component adds further type parameters — most often to hold an injected dependency in a field whose type is fixed per composition, so reactions read it through a direct, type-stable field load.

[Components](components.md) injects a `Sensor` into a `Guard` through the `BatterySource` interface. This page is the contract behind that injection: how the constructor places `Name` and the resolved dependencies, how the same component takes a real provider in production and a mock in a test, and how to run a dependent component on its own.

## A dependency held in a type parameter

The injected provider's concrete type varies by composition — a real `Sensor` in the vehicle, a mock in a test rig. A free type parameter `B` fixes that type per composition, so `battery(g.battery_src)` resolves to a direct call:

```julia
mutable struct Guard{Name, B} <: Component{Name}
    battery_src::B
end
```

The assembler fills `B` from whatever sibling satisfies the `requires`. In `Vehicle` that is the `Sensor`, so the member's type is `Guard{:guard, Sensor{:sensor}}` — the provider's own `Name` rides along, naming its path within the node.

## The `construct` contract

`Name` is a type parameter and the injected provider fills `B`, so construction must place both. Assembly builds the component by **calling its own constructor** with these arguments, in order, expecting a `Guard{Name, …}` back:

```
Guard(node, ::Val{Name}, deps...) where {Name} -> Guard{Name, …}
```

- `node` — the node-core handle;
- `::Val{Name}` — the member's path, its `Symbol` key in `node("name" => Guard, …)`, lifted to `Val{Name}`;
- `deps...` — exactly one resolved provider per `requires` entry, in `requires` order.

A `Guard` whose only fields are its injected deps is a **pure holder**: the default builds the field form `Guard{Name, typeof(src)}(src)` from the type, so it needs no constructor at all:

```julia
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))
```

When the default doesn't fit — deps stored in named fields, non-dep fields to set, a different parameter order, or construction that needs `node` or `Name` — write the constructor yourself, most cleanly as an **inner constructor matching the signature**. Assembly calls it automatically, so `ctor=` is elided:

```julia
mutable struct Guard{Name, B} <: Component{Name}
    battery_src::B
    Guard(node, ::Val{Name}, src) where {Name} = new{Name, typeof(src)}(src)   # found and called by assembly
end
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))   # no ctor=
```

Defining that inner constructor replaces Julia's default field constructor — which is what you want here, since every `Guard` now goes through your `new`. Two other surfaces build the same way: a [`construct`](@ref) method on the bare type (also no `ctor=`), or — to wire an external callable, or to override either of the above — `member_schema`'s `ctor =`:

```julia
construct(::Type{Guard}, node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)   # no ctor= needed
# …or pass an external callable explicitly (and it wins over an inner ctor / construct method):
make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,), ctor = make_guard)
```

The [`@component`](@ref) macro's `@requires battery_src::BatterySource` directive emits the parametric struct and a `construct` method for you (see [The @component macro](@ref)) — the concise form when the component is authored as one block. A component whose every parameter past `Name` is defaulted needs none of this: its constructor is the default `M{Name}()`.

An injected provider arrives **constructed but not yet configured**: the framework configures providers ahead of their consumers (see [Component Lifecycle](lifecycle.md)). Store it from the constructor, then read it through its interface from `configure` onward.

## Injecting a mock in tests

The provider rides `B`, so the consumer's code is identical whichever provider it gets. A test composes a mock that satisfies the interface, and the same `Guard` reads it type-stably:

```julia
mutable struct MockBattery{Name} <: Component{Name} end
battery(::MockBattery) = 42.0
member_schema(::Type{MockBattery}) = component(MockBattery; provides = (BatterySource,))

rig = node("batt" => MockBattery, "guard" => Guard)     # guard member is Guard{:guard, MockBattery{:batt}}
```

## Running a dependent component standalone

`run(S)` promotes a single component to its own node with un-prefixed parameters, constructing it through its zero-dependency constructor:

- a component declaring no `requires` — constructed and run;
- a `requires`-bearing component — rejected (no sibling can satisfy the dependency), with the reason named.

A component you want to run both ways gets a zero-`requires` variant whose constructor injects a **null-object** stand-in — a provider that answers the interface with a safe default:

```julia
struct NullBattery end
battery(::NullBattery) = 100.0                          # always "full": the standalone default

mutable struct SoloGuard{Name} <: Component{Name}
    battery_src::NullBattery
end
SoloGuard{Name}() where {Name} = SoloGuard{Name}(NullBattery())
member_schema(::Type{SoloGuard}) = component(SoloGuard, GuardParams, safe)

run(SoloGuard; name = "guard")                          # un-prefixed params: ros2 param get /guard min_battery
```

The composed `Guard` and the standalone `SoloGuard` share their reactions and parameters; the provider wiring is all that differs.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
construct
```
