# ROSNode.jl

ROSNode is a pure-Julia ROS 2 client library built on Zenoh. It mirrors `rmw_zenoh` on the wire, so its nodes interoperate with C++ and Python ROS 2 nodes sharing the same router and `ROS_DOMAIN_ID`.

The `peers` endpoint points at a running Zenoh router (`zenohd`); start one first. `from="interfaces"` adds a local directory of interface files as a search root, so `std_msgs/msg/String` resolves from `interfaces/std_msgs/msg/String.msg` without a sourced ROS environment. This snippet shows the API shape; for a runnable talker see `examples/publisher.jl`.

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)
    publish(pub, std_msgs.msg.String(data = "hello world"))
end
```

ROSNode also resolves message types at runtime. A type-less subscription reads each sample's `(name, hash)` and resolves the concrete wire type — from the registry, the project cache, ament, or a wire query, in that order — then decodes it on the fly, handing the handler a real typed struct, with no `@ros_import`:

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

This powers generic tooling — recorders, bridges, introspection — over types it never imported.

## What it provides

- **Nodes** group communication entities under a name.
- **Topics** carry messages from publishers to subscribers.
- **Services** answer a request with a response.
- **Actions** run long-running goals that stream feedback and return a result.
- **Parameters** expose named, typed values that peers read and set.

## The guide

Start with [Getting Started](getting-started.md): start a router, open a context, run an example. From there the guide builds from concepts to practice.

**Foundations**

- [The Runtime Model](foundations/runtime-model.md) — how a context, node, and `spin` keep a process alive while the scheduler delivers messages.
- [Interface Types](foundations/interface-types.md) — the typed messages every topic, service, and action carries, and the RIHS01 identity that makes them wire-compatible.

**Communication**

- [Topics](communication/topics.md) — publish and subscribe on a named channel.
- [Services](communication/services.md) — answer a request with a response.
- [Actions](communication/actions.md) — run a long-running goal that streams feedback and returns a result.
- [Parameters](communication/parameters.md) — expose named, typed values that peers read and set.

**Going Further**

- [Authoring Interfaces in Julia](advanced/authoring.md) — define message, service, and action types in Julia source with `@ros_message`, `@ros_service`, and `@ros_action`.
- [Runtime Type Discovery](advanced/discovery.md) — resolve message types off the wire and persist them with `@ros_cache`.
- [Message Delivery](advanced/delivery.md) — choose a view mode and concurrency for how samples reach a handler.

**Composition**

- [Components](composition/components.md) — author a node as a collection of mixins, and compose nodes into a process.

**Interop**

- [Interoperating with ROS 2](interop/ros2.md) — exchange topics, services, actions, and parameters with C++ and Python ROS 2 nodes.

## The module

```@docs
ROSNode.ROSNode
```
