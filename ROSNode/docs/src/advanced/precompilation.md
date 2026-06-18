# Precompilation & Warm-up

```@meta
CurrentModule = ROSNode
```

The first message on a topic, the first request to a service, or the first goal to an action compiles the whole type-specialized chain — decode, your handler, and everything the handler calls. That one-time JIT cost lands as a startup spike, or as a stall on the first live message. Two mechanisms move it off the hot path: **warm-up** compiles the chain at endpoint construction, and the **node bake** folds a component package's handlers into its precompiled image.

## Two tiers

| Tier | Runs at | Covers | How |
|---|---|---|---|
| Warm-up | endpoint construction | each endpoint's decode→handler chain, recompiled each process | automatic — node default `:precompile` |
| Node bake | the consuming package's precompile | a component's typed accessors + reaction specializations, baked into its pkgimage | one line: [`@precompile_nodes`](@ref) |

The two compose: warm-up applies to every node, and the bake additionally makes a precompiled component start from an already-compiled image, so even its first process is hot.

## Warm-up

Every endpoint warms its own dispatch chain at construction under a [`WarmupPolicy`](@ref) — a **mode** paired with a **sync** flag.

The mode chooses how deep warming reaches, cheapest first:

- `:precompile` (the default) `precompile`-anchors the decode→handler chain: it caches the inference tree and codegens the type-specialized frame, side-effect-free and with no message instance.
- `:execute` additionally runs the handler once on a synthesized sample message, reaching full native depth. Outbound ROS operations (`publish`/`call`) null-route, and side effects marked with [`@effectful`](@ref) are skipped, so warming emits no real traffic and touches no external state.
- `:off` skips warm-up.

The sync flag chooses when warming happens:

- `warmup_sync = false` (the default) warms on a background task — zero construction latency.
- `warmup_sync = true` blocks the constructor until the chain is compiled — for hard real-time bring-up, where the first message must already be hot.

Set the policy as the node default that every endpoint inherits, or override it per endpoint:

```julia
node = Node(ctx, "vehicle"; warmup = :precompile, warmup_sync = true)   # node default

# per-endpoint override; :execute runs the handler once on a fabricated Twist
Subscription(node, "/cmd_vel", geometry_msgs.msg.Twist;
             warmup = :execute, warmup_sync = true) do msg
    drive(msg)
end
```

A handler written for live data may reject a synthesized sample, so an `:execute` warm-up tolerates one:

- it runs the handler under a warm scope where [`is_warming`](@ref) is true, and
- a throw degrades that handler to compile-only with a warning.

Pass `warmup_sample` a representative message when the default builder can't synthesize a realistic one.

Warm-up reaches your handler only when the chain is monomorphic — a static, typed subscription. A dynamic subscription resolves its type per sample, so it warms the codec and replays a discovered-type manifest at startup instead; see [Runtime Type Discovery](discovery.md).

## The node bake

A component declared in a precompiled package bakes its per-mixin machinery into that package's image with [`@precompile_nodes`](@ref), so a later `run` skips first-touch codegen entirely. Place it as the last statement of the module, after every `@mixin`/`@param`/`@publishes`/… and reaction handler:

```julia
module Vehicle
using ROSNode
@ros_import "std_msgs/msg/Bool"

@mixin struct Drive end
@param Drive max_speed::Float64 = 2.0
@hears function brake(d::Drive, msg::std_msgs.msg.Bool)
    msg.data && stop!(d)
end

@node Rig = ["drive" => Drive]

@precompile_nodes      # bake Drive's accessors + handlers into Vehicle's image
end
```

It bakes, into the consuming package's precompile image:

- each mixin's typed `parameters(m)` and `entities(m)` accessors, and
- its reaction handlers, compiled against those accessors, together with the codec and dispatch frames they specialize.

The bake reaches a mixin whose ports all have a statically derivable handle type. A mixin with an action server or client keeps the generic accessor and warms at first `run` instead, since its handle type materializes only then.

`@precompile_nodes` is opt-in and side-effect-free: it runs only `precompile` and codegen, never your reactions, and fires only at precompile. A component without it still works — the same accessors and specializations generate lazily on the first `run`.

## See also

- [Components](../composition/components.md) — authoring nodes as mixins.
- [Message Delivery](delivery.md) — the view modes and concurrency a warmed handler runs under.
- [Runtime Type Discovery](discovery.md) — warm-up and manifest replay for dynamic subscriptions.
- Julia manual: [Module initialization and precompilation](https://docs.julialang.org/en/v1/manual/modules/#Module-initialization-and-precompilation).
- [PrecompileTools.jl](https://julialang.github.io/PrecompileTools.jl/stable/) — the workload mechanism `@precompile_nodes` builds on.

## API reference

```@docs
WarmupPolicy
WarmupMode
Precompile
Execute
NoWarmup
@precompile_nodes
```
