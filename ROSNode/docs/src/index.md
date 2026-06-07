# ROSNode.jl

ROSNode is a pure-Julia ROS 2 client library built on Zenoh. It mirrors `rmw_zenoh` on the wire, so its nodes interoperate with C++ and Python ROS 2 nodes sharing the same router and `ROS_DOMAIN_ID`.

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)
    publish(pub, std_msgs.msg.String(data = "hello world"))
end
```

ROSNode also resolves message types at runtime. A type-less subscription discovers the concrete wire type from the publisher and decodes it on the fly, handing the handler a real typed struct — with no `@ros_import`:

```julia
using ROSNode

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "listener")

    # No @ros_import: the message type is resolved at runtime from discovery.
    Subscription(node, "/chatter") do msg
        @info "heard" msg.data
    end

    spin(ctx; handle_signals = true)
end
```

This powers generic tooling — recorders, bridges, introspection — over types it never imported. `@ros_cache` persists each discovered type to warm it at startup on the next run. See [Interface Types](interfaces.md).

## What it provides

- **Nodes** group communication entities under a name.
- **Topics** carry messages from publishers to subscribers.
- **Services** answer a request with a response.
- **Actions** run long-running goals that stream feedback and return a result.
- **Parameters** expose named, typed values that peers read and set.

## Defining interface types

Two paths produce the same wire types:

1. Import an existing ROS interface by name with `@ros_import`.
2. Author one in Julia with `@ros_message`, `@ros_service`, or `@ros_action`.

Both carry the same RIHS01 identity, so an imported type and its authored twin are wire-compatible. See [Interface Types](interfaces.md).

## Runtime model

A `Context` owns the Zenoh session, discovery, and shutdown; a `Node` groups the entities you create against it; `spin(ctx; handle_signals = true)` parks the main task and runs until shutdown, turning Ctrl-C into a graceful drain. Julia's scheduler is the executor, so handlers run on tasks. See [Getting Started](getting-started.md) for the full setup.

Typed subscriptions select a view mode — `Owned` copies the payload into Julia memory, `Checked` exposes a zero-copy view guarded against escape, `Unchecked` exposes the view directly for maximum throughput. Callbacks run with `Serial()` concurrency by default; `Parallel(n)` fans delivery across `n` tasks.

## Tutorials

- [Getting Started](getting-started.md) — start a router, open a context, run an example.
- [Interface Types](interfaces.md) — import or author message, service, and action types.
- [Publisher and Subscriber](tutorials/pubsub.md) — publish and subscribe on a topic.
- [Service and Client](tutorials/service.md) — serve and call a request/response service.
- [Action Server and Client](tutorials/action.md) — run an action server and client with feedback.

## Mapping to the ROS 2 tutorials

| ROSNode page | ROS 2 rolling beginner tutorial |
| --- | --- |
| `tutorials/pubsub.md` | Writing a simple publisher and subscriber (C++/Python) |
| `tutorials/service.md` | Writing a simple service and client (C++/Python) |
| `tutorials/action.md` | Writing an action server and client |
| `interfaces.md` | Creating custom msg and srv files + Implementing custom interfaces |
| `getting-started.md` | Configuring environment / Creating a workspace / Creating a package / Using colcon |
| `index.md` | (overview of the client library) |
