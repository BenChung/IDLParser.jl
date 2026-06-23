# Topics

A topic is a named channel. Publishers send messages on it, and subscribers receive every message that arrives.

This page builds a talker that publishes a `std_msgs/String` on `/chatter` and a listener that prints each one, using an imported message type for both halves.

## Imported interfaces

`@ros_import` bakes a standard interface by name and lands it at `std_msgs.msg.String` in the calling module. The `from=` root names where the macro reads the `.msg` source. Resolve `std_msgs` one of three ways:

- run from the repository's `examples/` directory, where `interfaces/std_msgs/msg/String.msg` ships (`from="interfaces"`);
- point `from=` at your own interface tree;
- source a ROS 2 environment, which resolves `std_msgs` without `from=`.

The talker opens a context, creates a node, and publishes a fresh `String` each second:

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)

    for i in 0:typemax(Int)
        isopen(ctx) || break
        publish(pub, std_msgs.msg.String(data = "hello world $i"))
        sleep(1.0)
    end
end
```

The listener runs as a separate process. The do-block handler runs once per message, receiving an owned, decoded `std_msgs.msg.String`:

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "listener")

    sub = Subscription(node, "/chatter") do msg
        @info "heard" msg.data
    end

    spin(ctx; handle_signals = true)
end
```

The type-less `Subscription(node, "/chatter")` resolves each sample's concrete type at runtime against the home module's imports, which `@context` binds from the calling module. This single `@ros_import "std_msgs/msg/String"` therefore serves both the publisher's explicit type and the subscriber's resolution. See [Interface Types](../foundations/interface-types.md) for how that resolution works, and [Runtime Type Discovery](../advanced/discovery.md) for a recorder that handles any type off the wire.

Concurrency has two modes: the default `concurrency = Serial()` delivers messages in order on one task, while `Parallel(n)` fans out across threads. See [Message Delivery](../advanced/delivery.md) for view modes and concurrency in depth.

`spin` parks the main task to keep the process alive while the scheduler delivers each message on its own task, and `handle_signals = true` turns Ctrl-C into a graceful drain.

`Publisher` and `Subscription` are singleton instances of distinct endpoint kinds. Each accepts a `qos` and the `warmup` family plus kind-specific options; see [`PublisherKind`](@ref ROSNode.PublisherKind) and [`SubscriptionKind`](@ref ROSNode.SubscriptionKind) for the full argument lists.

To define your own message type directly in Julia, see [Authoring Interfaces in Julia](../advanced/authoring.md). Both halves are wire-compatible with ROS 2 nodes — see [Interoperating with ROS 2](../interop/ros2.md) to echo `/chatter` from a sourced ROS 2 environment, and [Getting Started](../getting-started.md) to start the router these examples point at.

## Next

[Services](services.md) extend the topic model to request/response calls.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
PublisherHandle
publish
SubscriptionHandle
DynamicSubscriptionHandle
```
