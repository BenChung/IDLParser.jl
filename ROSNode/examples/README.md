# ROSNode examples

Five minimal, runnable examples covering the core communication patterns:

| File             | Pattern                  | Type used                              |
| ---------------- | ------------------------ | -------------------------------------- |
| `publisher.jl`   | Publish                  | `std_msgs/msg/String`                  |
| `subscriber.jl`  | Subscribe                | `std_msgs/msg/String`                  |
| `service.jl`     | Service server + client  | `example_interfaces/srv/AddTwoInts`    |
| `action.jl`      | Action server + client   | `example_interfaces/action/Fibonacci`  |
| `warming.jl`     | Dynamic type + warm-up   | BYO `sensor_demo/msg/Reading`          |

## Running

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is
**router-based**. Start a router once, then run any example against it:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/publisher.jl     # in one terminal
julia --project=. ROSNode/examples/subscriber.jl    # in another
```

`service.jl` and `action.jl` each run both halves in one process, so they're
self-contained — just point them at the router.

The `peers = ["tcp/localhost:7447"]` argument to `Context` selects that router;
drop it (and set `localhost_only` / `domain_id` as needed) to use environment
discovery instead.

## Interop with ROS2

With `rmw_zenoh` selected and a matching router + `ROS_DOMAIN_ID`, these talk to
real ROS2 nodes:

```sh
ros2 topic echo /chatter std_msgs/msg/String
ros2 service call /add_two_ints example_interfaces/srv/AddTwoInts "{a: 2, b: 40}"
ros2 action send_goal /fibonacci example_interfaces/action/Fibonacci "{order: 8}"
```

## Where types come from

`@ros_import "pkg/qual/Name"` statically generates an interface type by name,
resolving it from ROSNode's vendored tree or, inside a sourced ROS2 environment,
from `AMENT_PREFIX_PATH`. Generated types land at `pkg.qual.Name` in the calling
module (e.g. `std_msgs.msg.String`), and a fully-qualified import also binds the
bare leaf — `@ros_import "std_msgs/msg/String"` gives `String` directly (use
`… as Alias` to avoid a name clash). `std_msgs` and `example_interfaces` are not
vendored, so those examples need a sourced ROS2 install on `AMENT_PREFIX_PATH`.

Conventions worth knowing (the ROS/wire type names are always `Name_Request`,
`Name_Goal`, … — the dotted forms below are Julia-side aliases that read better):

- A **service** generates a `Name` tag type exposing `Name.Request` / `Name.Response`.
  Reference the service by its `.Request` type; the response is resolved
  automatically — e.g. `Service(node, "/add", AddTwoInts.Request) do req … end`.
- An **action** generates a `Name` tag type that both *is* the action handle (pass
  it to `ActionServer`/`ActionClient`) and exposes `Name.Goal` / `Name.Feedback` /
  `Name.Result` — e.g. `ActionServer(node, "/fib", Fibonacci) do goal … end` and
  `send(client, Fibonacci.Goal(order = 8))`.

**Your own (BYO) interfaces.** Point `@ros_import` at a local source tree with
`from=` and reference your types by name like any other — they go through the same
registration path (RIHS01, keyexpr-only resolution, the §13 server), which raw
`@ros_msgs`/`@ros_msg` skip:

```julia
# layout: interfaces/robot_msgs/msg/Widget.msg  (parent-of-packages, the ROS way)
@ros_import from="interfaces" "robot_msgs/msg/Widget" "robot_msgs/srv/DoThing"
pub = Publisher(node, "/widgets", robot_msgs.msg.Widget)
```

`from=` takes one dir or a vector (`from=["a", "b"]`); relative roots resolve against
the source file. Per name the search order is `from`-roots → vendored → ament, so a
BYO package can supply or shadow types. Use raw `@ros_msgs` only if you specifically
*don't* want registration.

## Notes

- The `Context(; …) do ctx … end` form brackets startup and shutdown: on exit it
  drains in-flight work, undeclares entities, and closes the session.
- Julia's scheduler **is** the executor — there is no `spin()`. Handlers run on
  tasks; a blocking handler yields rather than stalling delivery. Keep the process
  alive (e.g. a `sleep` loop) for as long as you want to receive messages.
- Every entity (`Publisher`, `Subscription`, `ServiceClient`, servers, …) is
  `close`-able and is closed automatically when its node/context drains.
