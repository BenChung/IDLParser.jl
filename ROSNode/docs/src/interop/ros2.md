# Interoperating with ROS 2

ROSNode mirrors `rmw_zenoh` on the wire. With `rmw_zenoh` selected and a matching router plus `ROS_DOMAIN_ID`, ROSNode entities exchange messages with real C++/Python ROS 2 nodes.

## Wire identity (RIHS01)

A shared RIHS01 hash is what lets a ROSNode entity and a stock C++/Python endpoint speak the same wire type — see [Interface Types](../foundations/interface-types.md) for how the hash is derived and matched across imported, authored, and runtime-discovered types.

## Talking to ROS 2 from the command line

The `ros2` CLI reaches ROSNode entities the same way it reaches any node, once the ROS 2 side joins the same Zenoh fabric and domain:

1. Select the Zenoh RMW: `export RMW_IMPLEMENTATION=rmw_zenoh_cpp`.
2. Start its router so both sides have a meeting point: `ros2 run rmw_zenoh_cpp rmw_zenohd`.
3. Match `ROS_DOMAIN_ID` on both sides (default `0`).

A domain or RMW mismatch produces silence — no traffic and no diagnostic.

Topics — echo what a `Publisher` sends, or feed a `Subscription` ([Topics](../communication/topics.md)):

```sh
ros2 topic echo /chatter std_msgs/msg/String
ros2 topic pub  /chatter std_msgs/msg/String "{data: hi}"
```

Services — call a `Service` server ([Services](../communication/services.md)):

```sh
ros2 service call /add_two_ints example_interfaces/srv/AddTwoInts "{a: 2, b: 40}"
```

Actions — send a goal to an `ActionServer` ([Actions](../communication/actions.md)):

```sh
ros2 action send_goal /fibonacci action_tutorials_interfaces/action/Fibonacci "{order: 8}"
```

Parameters — read and write a parameterized node's live parameters ([Parameters](../communication/parameters.md)):

```sh
ros2 param get /client_demo_server max_speed
ros2 param set /client_demo_server max_speed 80
```

Lifecycle — drive a managed component's transitions ([Components](../composition/components.md)):

```sh
ros2 lifecycle set /vehicle configure
ros2 lifecycle get /vehicle
```

## Where imported types come from

`@ros_import` statically generates an interface by name from ROSNode's vendored tree or, inside a sourced ROS 2 environment, from `AMENT_PREFIX_PATH`. The `from=` argument adds local source roots that are searched first, so your own packages resolve ahead of — and can shadow — the vendored and ament trees:

```julia
@ros_import "std_msgs/msg/String" from="interfaces"
```

See [Interface Types](../foundations/interface-types.md) for the full import surface.

## Reaching peers on other hosts

The default `Context` already reaches peers on other hosts: `localhost_only` is `false`, so multicast scouting stays on. Add `peers=["tcp/host:7447"]` to dial a router directly. See [Getting Started](../getting-started.md) for the router command and the discovery options.

## See also

- [Topics](../communication/topics.md)
- [Services](../communication/services.md)
- [Actions](../communication/actions.md)
- [Parameters](../communication/parameters.md)
