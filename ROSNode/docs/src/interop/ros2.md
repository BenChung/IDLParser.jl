# Interoperating with ROS 2

ROSNode mirrors `rmw_zenoh` on the wire. With `rmw_zenoh` selected and a matching router plus `ROS_DOMAIN_ID`, ROSNode entities exchange messages with real C++/Python ROS 2 nodes.

## Wire identity (RIHS01)

A ROS interface's wire type is its name plus its RIHS01 hash, derived from the field layout. Two definitions sharing both the interface name and the RIHS01 hash are the same wire type. This identity lets imported, authored, and runtime-discovered Julia types interoperate with each other and with ROS 2: an authored interface carries the same RIHS01 as the equivalent `.msg`/`.srv`/`.action`, so it speaks for its imported twin and for a stock C++/Python endpoint alike.

See [Interface Types](../foundations/interface-types.md) for how imported and discovered types resolve, and [Authoring Interfaces in Julia](../advanced/authoring.md) for the authored forms.

## Talking to ROS 2 from the command line

With a router running and `rmw_zenoh` selected, the `ros2` CLI reaches ROSNode entities the same way it reaches any node.

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

`@ros_import` statically generates an interface by name from ROSNode's vendored tree or, inside a sourced ROS 2 environment, from `AMENT_PREFIX_PATH`. The `from=` argument adds local source roots, so your own packages resolve alongside the vendored tree:

```julia
@ros_import "std_msgs/msg/String" from="interfaces"
```

See [Interface Types](../foundations/interface-types.md) for the full import surface.

## Reaching peers on other hosts

`peers` selects the router; omit `localhost_only` so discovery reaches peers on other hosts. See [Getting Started](../getting-started.md) for the router command and the discovery options.

## See also

- [Topics](../communication/topics.md)
- [Services](../communication/services.md)
- [Actions](../communication/actions.md)
- [Parameters](../communication/parameters.md)
