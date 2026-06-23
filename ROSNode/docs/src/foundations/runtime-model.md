# The Runtime Model

Every ROSNode program follows one lifecycle skeleton: open a `Context`, group entities under a `Node`, then `spin`. This page covers that skeleton and the discovery options that shape it.

## The skeleton

Open a Context, create a Node, build entities under it, and spin. Here is a publisher reduced to that shape:

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)

    publish(pub, std_msgs.msg.String(data = "hello world"))
    spin(ctx; handle_signals = true)
end
```

Those four pieces — Context, Node, the entities, and spin — appear in every program. Topics, services, and actions each fill in the entity step with their own constructors.

The `from="interfaces"` argument tells `@ros_import` where to find the `std_msgs/msg/String` definition: it adds a local source root holding the `.msg` files. Drop it to resolve a name from the vendored tree and a sourced ROS 2 environment. See [Interface Types](interface-types.md) for how `@ros_import` and `from=` resolve names.

## Contexts

A `Context` owns the Zenoh session, discovery, and shutdown. The `@context do ctx … end` form, on exit:

- drains in-flight work,
- closes the Zenoh session,
- and binds the calling module as the type-resolution home, so wire types resolve against that module's imported structs.

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
end
```

The resolution-home binding is what lets a type-less subscription decode a message into the right Julia struct; see [Runtime Type Discovery](../advanced/discovery.md) for how that resolution proceeds.

## Nodes

A `Node(ctx, "name")` groups the entities you create under it: publishers, subscriptions, servers, and clients. The name is the node's identity on the graph, and every entity built against the node inherits it.

```julia
node = Node(ctx, "talker")
```

## spin

`spin(ctx)` parks the main task to keep the process alive while the scheduler delivers messages on their own tasks. The scheduler is the executor: each handler runs on its own task, so a publish loop and a subscription callback make progress independently.

```julia
spin(ctx; handle_signals = true)
```

`handle_signals = true` turns Ctrl-C into a graceful drain: ROSNode departs liveliness cleanly and closes the session, and background loops observe `isopen(ctx)` flip to `false` so they can stop. Plain `spin(ctx)` parks the task and leaves signal handling to Julia's default SIGINT path.

### Reaction errors

A fire-and-forget reaction — a subscription handler or an [`every`](@ref)/timer callback — runs with no caller to return an error to. When one throws an exception it did not catch, the Context's `on_reaction_error` policy decides what happens:

- [`ShutdownOnError`](@ref ROSNode.ShutdownOnError) (`:shutdown`, the **default**) logs the exception once and gracefully drains the Context — the same drain Ctrl-C runs — so `spin` returns and the process exits cleanly. This stops the failing reaction rather than looping on it: a handler that throws on every message, or a timer that throws every tick, would otherwise flood the log fast enough to starve the interrupt and leave the process effectively unkillable.
- [`ContinueOnError`](@ref ROSNode.ContinueOnError) (`:continue`) logs the exception (rate-limited so it cannot flood) and keeps the reaction running — for a node that must survive a bad message or tick.

Set it on the Context (`Context(; on_reaction_error = :continue)`) or pass it to `run` (`run(schema; on_reaction_error = :continue)`). Services and actions are unaffected: a throwing request or goal handler replies with an error, it does not bring the node down. A message *decode* failure — malformed or off-type wire data, not your code — is always logged (rate-limited) and dropped, never a shutdown, so a misbehaving peer cannot take the node down.

## Discovery options

ROSNode mirrors `rmw_zenoh`, whose discovery is router-based. The `peers` keyword on `@context` selects the router to connect to:

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    # ...
end
```

Environment discovery is the default: without `peers`, ROSNode scouts via multicast, and the `localhost_only` / `domain_id` knobs shape its reach:

| Knob | Effect | Default |
|------|--------|---------|
| `peers` | Sets the routers/peers to connect to (Zenoh `connect/endpoints`). | none (environment discovery) |
| `localhost_only` | `true` disables multicast scouting and, absent explicit `peers`, connects to the loopback router `tcp/localhost:7447`. | `false` (reaches peers on other hosts) |
| `domain_id` | Partitions discovery so contexts on different domains stay isolated. | `ROS_DOMAIN_ID`, else `0` |

ROSNode stamps `domain_id` into every topic keyexpr and the liveliness subscription, so contexts on different domains never meet — see [Addressing & Key Expressions](addressing.md) for how names become keyexprs.

```julia
@context(localhost_only = true, domain_id = 0) do ctx
    # ...
end
```

See [Getting Started](../getting-started.md) for the `zenohd` command that brings up the router these options point at.

## Next

- [Interface Types](interface-types.md)

Once the skeleton is in place, fill the entity step with a [topic](../communication/topics.md).

## API reference

```@meta
CurrentModule = ROSNode
```

### Context

```@docs
@context
Context
spin
session
registry
graph
local_graph
describe_graph
node_endpoint_descs
resolve_name
on_shutdown
deregister_on_shutdown!
request_shutdown
is_shutdown
next_entity_id!
EndpointDesc
EndpointInfo
NodeInfo
GraphIndex
PublisherKind
SubscriptionKind
```

### Node

```@docs
Node
Entity
dispose
```

### Settlement and shutdown

```@docs
ShutdownException
Cancelled
SettlementStatus
success
failure
failed
succeeded
canceled
aborted
feedback
ReactionErrorPolicy
ShutdownOnError
ContinueOnError
```

### Lifecycle nodes

```@docs
LifecycleNode
LifecycleState
Unconfigured
Inactive
Active
Finalized
TransitionResult
isactive
inner_node
configure!
activate!
deactivate!
cleanup!
shutdown!
```
