# Parametric Components

A component carries its member path as the type parameter `Name`. A **parametric** component adds further type parameters — most often to hold an injected dependency in a field whose type is fixed per composition, so reactions read it through a direct, type-stable field load.

[Components](components.md) injects a `Sensor` into a `Guard` through the `BatterySource` interface. This page is the contract behind that injection: how the constructor places `Name` and the resolved dependencies, how the same component takes a real provider in production and a mock in a test, and how to run a dependent component on its own.

## A dependency held in a type parameter

The injected provider's concrete type varies by composition — a real `Sensor` in the vehicle, a mock in a test rig. A free type parameter `B` fixes that type per composition, so `battery(g.battery_src)` resolves to a direct call:

```julia
mutable struct Guard{Name, B} <: Component{Name}
    battery_src::B                          # the injected sibling provider
end
```

The assembler fills `B` from whatever sibling satisfies the `requires`. In `Vehicle` that is the `Sensor`, so the member's type is `Guard{:guard, Sensor{:sensor}}` — the provider's own `Name` rides along, naming its path within the node.

## The `construct` contract

`Name` is a type parameter, so a parametric component supplies its own constructor: `Guard{:guard}()` would still be missing `B`. The constructor places `Name` and stores the resolved dependencies. The framework calls it with the node-core handle, the member name as `Val{Name}`, and one dependency per declared `requires`, in `requires` order:

```julia
make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
```

Declare it as either surface — the `ctor =` keyword on `component`, or a [`construct`](@ref)`(::Type{Guard}, …)` method on the bare type (the keyword wins when a component carries both):

```julia
# as a keyword:
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe;
    requires = (BatterySource,), ctor = make_guard)

# or as the trait method (same signature, no `ctor =`):
construct(::Type{Guard}, node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,))
```

A `requires` backed by neither a `ctor` nor a matching `construct` method is rejected at authoring time, since the default zero-dependency constructor has no slot for the injected provider. A component whose every parameter past `Name` is defaulted already has its constructor — the default `M{Name}()` — and supplies none of the above.

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

`run(S)` promotes a single component to its own node with un-prefixed parameters, constructing it through its zero-dependency constructor. That covers a component declaring no `requires`. A `requires`-bearing component has no sibling to satisfy the dependency on its own, so `run` rejects it and names that reason.

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
