# ROSNode examples

Minimal, runnable examples covering the core communication patterns. The first group
imports interfaces by name (`@ros_import`); the second authors them in Julia (`@ros_message`
/ `@ros_service` / `@ros_action`) — same wire types, defined from a struct or function.

| File             | Pattern                       | Type used                              |
| ---------------- | ----------------------------- | -------------------------------------- |
| `publisher.jl`   | Publish                       | `std_msgs/msg/String`                  |
| `subscriber.jl`  | Subscribe                     | `std_msgs/msg/String`                  |
| `service.jl`     | Service server + client       | `example_interfaces/srv/AddTwoInts`    |
| `action.jl`      | Action server + client        | `action_tutorials_interfaces/action/Fibonacci` |
| `parameters.jl`  | Parameter server + client     | `rcl_interfaces` (vendored)            |
| `warming.jl`     | Dynamic type + warm-up        | BYO `sensor_demo/msg/Reading`          |

**Authored types** — define the interface in Julia instead of importing it:

| File                     | Pattern                       | Authored as                                          |
| ------------------------ | ----------------------------- | ---------------------------------------------------- |
| `authored_pubsub.jl`     | Publish + subscribe           | `@ros_message struct` → `demo_msgs/msg/Chatter`      |
| `authored_pubsub_std.jl` | Publish + subscribe (interop) | `@ros_message "std_msgs/msg/String" Chatter` (rename onto the stock type) |
| `authored_service.jl`    | Service server + client       | `@ros_service function` → `example_interfaces/srv/AddTwoInts`   |
| `authored_action.jl`     | Action server + client        | `@ros_action function` → `action_tutorials_interfaces/action/Fibonacci` |

**Components** — author a whole *node* as a collection of mixins (see `DESIGN-COMPONENTS.md`):

| File           | Pattern                            | Shows                                                                                   |
| -------------- | ---------------------------------- | --------------------------------------------------------------------------------------- |
| `component.jl` | Node as a collection of `@mixin`s  | `@param` / `@publishes` / `@every` / inline `@serves`, DI via `@interface`/`@provides`/`requires`/`construct`, `@node` + `run` |

`component.jl` is self-contained (it authors its own types and runs the node plus a
client in one process), so it needs no router — point `@context` at one only to reach
other hosts.

## Running

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is
**router-based**. Start a router once, then run any example against it:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/publisher.jl     # in one terminal
julia --project=. ROSNode/examples/subscriber.jl    # in another
```

`service.jl`, `action.jl`, and `parameters.jl` each run both halves in one process,
so they're self-contained — just point them at the router. `parameters.jl` needs no
sourced ROS2 at all (its `rcl_interfaces` types are vendored).

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router;
drop it (and set `localhost_only` / `domain_id` as needed) to use environment
discovery instead.

## Interop with ROS2

With `rmw_zenoh` selected and a matching router + `ROS_DOMAIN_ID`, these talk to
real ROS2 nodes:

```sh
ros2 topic echo /chatter std_msgs/msg/String
ros2 service call /add_two_ints example_interfaces/srv/AddTwoInts "{a: 2, b: 40}"
ros2 action send_goal /fibonacci action_tutorials_interfaces/action/Fibonacci "{order: 8}"
ros2 param get /client_demo_server max_speed     # ↔ parameters.jl server
ros2 param set /client_demo_server max_speed 80
```

## Where types come from

`@ros_import "pkg/qual/Name"` statically generates an interface type by name,
resolving it from ROSNode's vendored tree or, inside a sourced ROS2 environment,
from `AMENT_PREFIX_PATH`. Generated types land at `pkg.qual.Name` in the calling
module (e.g. `std_msgs.msg.String`), and a fully-qualified import also binds the
bare leaf — `@ros_import "std_msgs/msg/String"` gives `String` directly (use
`… as Alias` to avoid a name clash). `std_msgs` and `example_interfaces` are not
vendored, so those examples need a sourced ROS2 install on `AMENT_PREFIX_PATH`.
(`parameters.jl` is the exception — its `rcl_interfaces` types are vendored, so it
runs against just a router.)

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

## Authoring interfaces in Julia

The reverse of `@ros_import`: when *you* own the interface, write it as a Julia type and
let ROSNode derive the ROS interface from it. `@ros_package "pkg"` sets the package; then

- **`@ros_message struct Name … end`** registers a message (`pkg/msg/Name`). Struct fields
  map to ROS fields by the fixed type table; nested authored structs compose (a field whose
  type is another `@ros_message` becomes a nested ref, hashed into the parent's RIHS).
- **`@ros_service function Name(a, b)::@NamedTuple{…}`** registers a service: the parameters
  are the request fields, the `@NamedTuple` return is the response, and the body is the
  handler (throw to fail the call). Serve it with `Service(node, ke, Name)` and call it with
  `call(client; a = …, b = …)`, which returns the response NamedTuple.
- **`@ros_action function Name(goalfields…, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…}`**
  registers an action: goal fields are parameters, the `FeedbackSink` parameter publishes
  feedback (and checkpoints cancellation) when called, and the `@NamedTuple` return is the
  result. `send(client; goalfields…)`, iterate `feedback(gh)` (NamedTuples), `fetch(gh)`.

Authored types carry the same RIHS01 as the equivalent `.msg`/`.srv`/`.action`, so the
authored examples are wire-compatible with their imported twins **and** with ROS2 — run
`authored_service.jl server` against `service.jl client` (or a C++/Python node), or point
`ros2 action send_goal /fibonacci …` at `authored_action.jl server`. Override the package
or rename a struct onto an existing identity with the explicit form,
`@ros_message "std_msgs/msg/String" MyStruct`.

## Notes

- The `@context(; …) do ctx … end` form brackets startup and shutdown: on exit it
  drains in-flight work, undeclares entities, and closes the session. `@context` also
  binds the calling module as the Context's type-resolution home (sugar for
  `Context(; home=@__MODULE__, …)`), so type-less subscriptions resolve wire types to
  this module's `@ros_import` structs; use a plain `Context(; …)` for content-canonical
  resolution only.
- Julia's scheduler is the executor: handlers run on their own tasks, and a blocking
  handler yields rather than stalling delivery. `spin(ctx; handle_signals = true)`
  parks the main task to keep the process alive while the scheduler delivers messages,
  and turns Ctrl-C into a graceful drain (clean liveliness departure + session close).
  The self-contained examples that drive both halves in one process instead run to
  completion and let the `@context` do-block drain on exit.
- Every entity (`Publisher`, `Subscription`, `ServiceClient`, servers, …) is
  `close`-able and is closed automatically when its node/context drains.
