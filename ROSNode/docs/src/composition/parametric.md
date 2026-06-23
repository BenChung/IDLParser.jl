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

`Name` is a type parameter and the injected provider fills `B`, so the framework needs a constructor that places both. It calls `construct` with the node-core handle, the member name as `Val{Name}`, and one dependency per declared `requires`, in `requires` order, and builds `Guard{Name, B}` from them.

A `Guard` whose only fields are its injected deps is a **pure holder**, and the default [`construct`](@ref) builds it from the type — `Guard{Name, typeof(src)}(src)` — so it needs no constructor at all:

```julia
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))
```

The default fits when the free type parameters past `Name` are exactly the deps, in order, and the struct's fields are those deps. Override it for any other shape — deps stored in named fields, non-dep fields to set, a different parameter order — as either surface: a [`construct`](@ref)`(::Type{Guard}, …)` method on the bare type, or the `ctor =` keyword on `component` (the keyword wins if both are given):

```julia
construct(::Type{Guard}, node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))

make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,), ctor = make_guard)
```

The [`@component`](@ref) macro's `@requires battery_src::BatterySource` directive emits the parametric struct and this constructor for you (see [The @component macro](@ref)) — the concise form when the component is authored as one block. A component whose every parameter past `Name` is defaulted needs none of this: its constructor is the default `M{Name}()`.

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
