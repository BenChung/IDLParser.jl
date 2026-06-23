# Precompilation & Warm-up

```@meta
CurrentModule = ROSNode
```

The first message on a topic, the first request to a service, or the first goal to an action compiles the whole type-specialized chain — decode, your handler, and everything the handler calls. That one-time JIT cost lands as a startup spike, or as a stall on the first live message. Two mechanisms compile that chain ahead of the first live message: **warm-up** compiles it at endpoint construction, and the **node bake** folds a component's first-`run` path into its package's precompiled image.

## Two tiers

| Tier | Runs at | Covers | How |
|---|---|---|---|
| Warm-up | endpoint construction | each endpoint's decode→handler chain, recompiled each process | opt-in per node via `warmup = :precompile`/`:execute`; node default `:off` |
| Node bake | the consuming package's precompile | a component's typed carriers, lifecycle fan-out, and reaction/serve bodies, baked into its pkgimage | one line: [`precompile_node`](@ref) in a `@compile_workload` |

The two compose: warm-up can be enabled on any node (opt-in, off by default), and the bake additionally makes a precompiled component start from an already-compiled image, so even its first process is hot.

## Warm-up

Every endpoint warms its own dispatch chain at construction under a [`WarmupPolicy`](@ref) — a **mode** paired with a **sync** flag.

The mode chooses how deep warming reaches, cheapest first:

- `:precompile` `precompile`-anchors the decode→handler chain: it caches the inference tree and codegens the type-specialized frame, side-effect-free and with no message instance.
- `:execute` additionally runs the handler once on a synthesized sample message, reaching full native depth. Outbound ROS operations (`publish`/`call`) null-route, and side effects marked with [`@effectful`](@ref) are skipped, so warming emits no real traffic and touches no external state.
- `:off` (the node default) skips warm-up.

The sync flag chooses when warming happens:

- `warmup_sync = false` (the default) warms on a background task — zero construction latency.
- `warmup_sync = true` blocks the constructor until the chain is compiled — for hard real-time bring-up, where the first message must already be hot.

Set the policy as the node default that every endpoint inherits, or override it per endpoint:

```julia
node = Node(ctx, "vehicle"; warmup = :precompile, warmup_sync = true)

Subscription(node, "/cmd_vel", geometry_msgs.msg.Twist;
             warmup = :execute, warmup_sync = true) do msg
    drive(msg)
end
```

A functor node passes the same `warmup`/`warmup_sync` to `run` — `run(Vehicle; warmup = :precompile, warmup_sync = true)` — and every member endpoint inherits it.

A handler written for live data may reject a synthesized sample, so an `:execute` warm-up tolerates one:

- it runs the handler under a warm scope where [`is_warming`](@ref) is true, and
- a throw degrades that handler to compile-only with a warning.

Pass `warmup_sample` a representative message when the default builder can't synthesize a realistic one.

Warm-up reaches your handler only when the chain is monomorphic — a static, typed subscription. A dynamic subscription resolves its type per sample, so it warms the codec and replays a discovered-type manifest at startup instead; see [Runtime Type Discovery](discovery.md).

## The node bake

A node kind declared in a precompiled package bakes its first-`run` path into that package's image with [`precompile_node`](@ref), so a later `run` skips first-touch codegen entirely. Place it inside a PrecompileTools `@compile_workload`, after the `node(…)` schema and every reaction handler are defined:

```julia
module Vehicle
using ROSNode, PrecompileTools
import ROSNode: member_schema
@ros_import "std_msgs/msg/Bool"

mutable struct Drive{Name} <: Component{Name} end
Drive{Name}() where {Name} = Drive{Name}()
@parameters struct DriveParams
    max_speed::Float64 = 2.0
end
brake(node, d::Drive, msg::std_msgs.msg.Bool) = msg.data && stop!(d)
member_schema(::Type{Drive}) = component(Drive, DriveParams,
    hears(:brake, std_msgs.msg.Bool, brake))

const Rig = node("drive" => Drive; name = "Rig")

# Re-register the const schema at LOAD — node(…; name=…) defers the registry mutation
# under precompilation, so a precompiled package must re-register from __init__.
__init__() = register_node_kind!("Rig", Rig)

@compile_workload precompile_node(Rig)
end
```

`precompile_node(Rig)` combines `precompile(run, (typeof(Rig),))` with a walk over the schema's frozen descriptors. It bakes, into the consuming package's precompile image:

- the node's `run` specialization, the `@generated` carrier builders, and the lifecycle fan-out (construct → materialise → configure → activate → teardown), anchored on the node's concrete `ComponentNode` type;
- each port's codec (encode/decode), wire resolution, and per-handle close;
- the spawned consume/serve/dispatch bodies — the subscription receive loop, the service request→handler→response serve tree, and the intra-process delivery leg — keyed on the member's concrete type and each reaction's handler.

Prefer the **value** form `precompile_node(Rig)` over the type form `precompile_node(typeof(Rig))`: the value form anchors reaction bodies from the node's frozen handlers, so inline `component(…)` members and rebound/remapped handlers bake correctly. The type form is best-effort: with no handler values it (1) falls back to a guarded `member_schema(base)` lookup, and (2) skips an inline `component(…)` member instead of aborting the precompile.

The bake reaches a member whose ports have statically derivable handle types. Action ports are anchored too, but an action whose handle materialises only at `run` keeps the generic accessor and warms at first `run` instead.

`precompile_node` is opt-in and side-effect-free: it runs only `precompile` and codegen, never your reactions, and fires only at precompile. A node without it still works — the same specializations generate lazily on the first `run`.

### Registering schemas at load

`node(…; name = …)` registers the kind in the process-global registry so a container can `load_node` it by name. Under precompilation that registry mutation is **discarded** (it would not survive the package cache), so a precompiled package re-registers its `const` schemas at load. The minimal form is an explicit `__init__`:

```julia
__init__() = register_node_kind!("Rig", Rig)
# or, for several kinds:  register_node_kinds!(Rig, Counter, …)
```

For the common case [`@register_nodes`](@ref) handles it — it rosters the kinds and auto-installs an `__init__` (the same hook that absorbs `@ros_message`/`@service` types) when the module has none:

```julia
@register_nodes Rig
```

In the REPL, scripts, and runtime, registration is immediate and needs no `__init__`.

## See also

- [Components](../composition/components.md) — authoring nodes as components and composing them with `node`.
- [Message Delivery](delivery.md) — the view modes and concurrency a warmed handler runs under.
- [Runtime Type Discovery](discovery.md) — warm-up and manifest replay for dynamic subscriptions.
- Julia manual: [Module initialization and precompilation](https://docs.julialang.org/en/v1/manual/modules/#Module-initialization-and-precompilation).
- [PrecompileTools.jl](https://julialang.github.io/PrecompileTools.jl/stable/) — the workload mechanism `precompile_node` builds on.

## API reference

```@docs
WarmupPolicy
WarmupMode
Precompile
Execute
NoWarmup
precompile_node
```
