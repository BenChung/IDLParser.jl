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

## Context

A `Context` owns the Zenoh session, discovery, and shutdown. The `@context do ctx … end` form drains in-flight work and closes the session on exit, and binds the calling module as the type-resolution home so wire types resolve against that module's imported structs.

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    # everything lives here
end
```

The resolution-home binding is what lets a type-less subscription decode a message into the right Julia struct; see [Runtime Type Discovery](../advanced/discovery.md) for how that resolution proceeds.

## Node

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

## Discovery options

ROSNode mirrors `rmw_zenoh`, whose discovery is router-based. The `peers` keyword on `@context` selects the router to connect to:

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    # ...
end
```

Omit `peers` (and set `localhost_only` / `domain_id` as needed) to use environment discovery. `localhost_only = true` confines discovery to the configured router; omit it to reach peers on other hosts. `domain_id` partitions discovery so contexts on different domains stay isolated.

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
resolve_name
on_shutdown
request_shutdown
is_shutdown
next_entity_id!
EndpointInfo
NodeInfo
GraphIndex
EndpointKind
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
