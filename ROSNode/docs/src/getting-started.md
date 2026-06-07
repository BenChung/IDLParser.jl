# Getting Started

ROSNode brings ROS 2 publish/subscribe, services, and actions to Julia over Zenoh, mirroring `rmw_zenoh` on the wire. This page sets up the package, starts a router, walks the Context / Node / spin lifecycle, and runs the bundled examples.

## Adding ROSNode

ROSNode ships as part of this workspace. Activate the workspace project and load the package:

```julia
using ROSNode
```

Run any example with the workspace project active so ROSNode and its dependencies resolve:

```sh
julia --project=. ROSNode/examples/publisher.jl
```

## Starting a router

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is **router-based**. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router. Omit `peers` (and set `localhost_only` / `domain_id` as needed) to use environment discovery. `localhost_only = true` confines discovery to the configured router; omit it to reach peers on other hosts.

## The Context / Node / spin model

Every program follows one lifecycle skeleton: open a Context, group entities under a Node, then spin. Here is a publisher reduced to that shape.

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

- **Context** owns the Zenoh session, discovery, and shutdown; the `@context do ctx … end` form drains in-flight work, closes the session on exit, and binds this module as the type-resolution home.
- **Node** groups the entities you create under it: publishers, subscriptions, servers, and clients.
- **spin** parks the main task and runs until Ctrl-C requests shutdown. `handle_signals = true` turns Ctrl-C into a graceful drain that flips `isopen(ctx)` for background loops to observe.

## Running the examples

Run a router, then start the two halves in separate Julia processes pointed at the same router:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/publisher.jl     # in one terminal
julia --project=. ROSNode/examples/subscriber.jl    # in another
```

Examples that bundle both halves (`service.jl`, `action.jl`) take a role argument to split across two processes — the `ROLE` env var or the first CLI arg, defaulting to `both`:

```sh
julia --project=. ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
julia --project=. ROSNode/examples/service.jl client    # call only (needs a server)
```

`service.jl`, `action.jl`, and `parameters.jl` each run both halves in one self-contained process; point them at the router and run.

## Interoperating with ROS 2

ROSNode mirrors `rmw_zenoh`: with `rmw_zenoh` selected and a matching router plus `ROS_DOMAIN_ID`, ROSNode entities talk to real ROS 2 C++/Python nodes over the wire:

```sh
ros2 topic echo /chatter std_msgs/msg/String
ros2 service call /add_two_ints example_interfaces/srv/AddTwoInts "{a: 2, b: 40}"
ros2 action send_goal /fibonacci example_interfaces/action/Fibonacci "{order: 8}"
```

`@ros_import` statically generates the interface type by name from ROSNode's vendored tree or, inside a sourced ROS 2 environment, from `AMENT_PREFIX_PATH`. Authored types (`@ros_message` / `@ros_service` / `@ros_action`) carry the same RIHS01 identity as the equivalent `.msg` / `.srv` / `.action`, which makes both wire-compatible with ROS 2.

## Next

- [Interface Types](interfaces.md)
- [Publisher and Subscriber](tutorials/pubsub.md)
- [Service and Client](tutorials/service.md)
- [Actions](tutorials/action.md)
