# Interface Types

Every topic, service, and action carries a typed message. ROSNode brings those types in two ways: import an existing interface by name, or author one directly in Julia. Both forms produce fully registered ROS types — real RIHS01, keyexpr resolution, wire compatibility — and interoperate with each other and with C++/Python ROS 2 nodes.

## Importing existing interfaces

`@ros_import` bakes an existing interface by its fully-qualified name. The `from="…"` argument adds a local source root, so your own `.msg`/`.srv`/`.action` definitions resolve alongside the vendored tree and a sourced ROS 2 environment.

```julia
@ros_import "std_msgs/msg/String" from="interfaces"
@ros_import "example_interfaces/srv/AddTwoInts" from="interfaces"
@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"
```

The binding follows the interface kind:

- A **message** binds the bare leaf type, e.g. `std_msgs.msg.String`. Construct it by that name: `std_msgs.msg.String(data = "hello")`.
- A **service** binds the bare namespace, exposing `.Request` and `.Response`. Reference it by `.Request`; ROSNode resolves the Response automatically.
- An **action** binds the bare namespace, which serves as the action handle and exposes `.Goal`, `.Result`, and `.Feedback`.

```julia
# message: bare leaf type
pub = Publisher(node, "/chatter", std_msgs.msg.String)

# service: reference by .Request
srv = Service(node, "/add_two_ints", AddTwoInts.Request) do req
    AddTwoInts.Response(sum = req.a + req.b)
end

# action: bare namespace is the handle; sections by short name
server = ActionServer(node, "/fibonacci", Fibonacci) do goal
    feedback!(goal, Fibonacci.Feedback(partial_sequence = seq))
    Fibonacci.Result(sequence = seq)
end
```

## Authoring interfaces in Julia

`@ros_package` names the ROS package the types belong to, giving each one its wire identity. `@ros_message`, `@ros_service`, and `@ros_action` then define the interface entirely in Julia source. Authored types compose — a struct referencing another authored type by its Julia type resolves and hashes the nested ref exactly as a `.msg` nested field would.

A message is a struct. `@ros_message` turns each one into `<pkg>/msg/<Name>`:

```julia
module Msgs
    using ROSNode
    @ros_package "demo_msgs"
    @ros_message struct Header;  stamp::Float64; frame::String;          end
    @ros_message struct Chatter; header::Header; data::String; seq::Int32; end   # nested authored ref
end
```

A service is a function: the parameters are the request fields, the `@NamedTuple` return type is the response. The body is the handler; throw to fail the call:

```julia
module Srv
    using ROSNode
    @ros_package "example_interfaces"
    @ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b,)
    end
end
```

An action is a function with a `FeedbackSink` parameter: the goal fields are parameters, the `FeedbackSink{@NamedTuple{…}}` parameter is the feedback channel, the `@NamedTuple` return type is the result. Calling the sink publishes Feedback and checkpoints cancellation; returning the result settles SUCCEEDED:

```julia
module Act
    using ROSNode
    @ros_package "action_tutorials_interfaces"
    @ros_action function Fibonacci(order::Int32,
            fb::FeedbackSink{@NamedTuple{partial_sequence::Vector{Int32}}})::@NamedTuple{sequence::Vector{Int32}}
        seq = Int32[0, 1]
        for _ in 1:order
            push!(seq, seq[end] + seq[end-1])
            fb((partial_sequence = copy(seq),))   # publish feedback + cancellation yield point
        end
        (sequence = seq,)
    end
end
```

Pass the authored function itself as the marker to the server and client constructors, e.g. `Service(node, "/add_two_ints", Srv.AddTwoInts)` and `ActionServer(node, "/fibonacci", Act.Fibonacci)`.

### Mapping onto an existing identity

The annotate form `@ros_message "pkg/msg/Name" T` keeps your Julia name `T` and gives it the wire type — and matching RIHS01 — of `pkg/msg/Name`. A one-field struct matches `std_msgs/msg/String` (`string data`), deriving the identical hash, so it interoperates with the stock `/chatter` endpoint. The annotate form registers the struct as written, so mark it `@kwdef` yourself for keyword construction:

```julia
module Msgs
    using ROSNode
    Base.@kwdef struct Chatter; data::String; end
    @ros_message "std_msgs/msg/String" Chatter      # Julia `Chatter` ⇔ ROS std_msgs/msg/String
end
```

## Discovering types at runtime

A type-less `Subscription` resolves its message type at runtime from discovery, so tooling subscribes to a topic whose type it learns over the wire. The `msg` is the real decoded value, with type-stable field access:

```julia
sub = Subscription(node, "/chatter") do msg
    @info "heard" typeof(msg.data) msg.data
end
```

`@ros_cache` makes that discovery stick across runs: it persists each discovered type description in a project-local, RIHS01-keyed store under `<project>/ros_typesupport`, records a per-node warm-up manifest, and bakes wire-discovered types into static ones at precompile.

```julia
@ros_cache
```

The dynamic subscription records every type it handles into the manifest. The next run replays that manifest at startup, warming each recorded type's codec before the first message arrives:

```julia
sub = Subscription(node, "/readings"; warmup = :precompile) do msg
    @info "recorded" sensor = msg.sensor_id value = msg.value
end
```

The framework logs a one-time hint naming the discovered type and the static spelling to graduate to — `Subscription(node, "/readings", Reading)` — which drops the per-message `invokelatest` hop and unlocks the zero-copy `view = true` fast path. Pass `warmup_sync = true` to block construction until the codec is warm.

## Wire identity (RIHS01)

A ROS interface's wire type is identified by its name plus its RIHS01 hash, derived from the field layout. Two definitions sharing both the interface name and the RIHS01 hash are the same wire type. This identity is what lets authored and imported Julia types interoperate: an authored `@ros_message`/`@ros_service`/`@ros_action` carries the same RIHS01 as the equivalent `.msg`/`.srv`/`.action`, so it is wire-compatible with its imported twin and with C++/Python ROS 2 nodes.

With `rmw_zenoh` selected and a matching router plus `ROS_DOMAIN_ID`, ROSNode entities exchange messages with real ROS 2 nodes:

```sh
ros2 topic echo /chatter std_msgs/msg/String
ros2 service call /add_two_ints example_interfaces/srv/AddTwoInts "{a: 2, b: 40}"
ros2 action send_goal /fibonacci action_tutorials_interfaces/action/Fibonacci "{order: 8}"
```

See [Getting started](getting-started.md) for the router setup, and the [publisher and subscriber](tutorials/pubsub.md), [service and client](tutorials/service.md), and [action server and client](tutorials/action.md) tutorials for the end-to-end walkthroughs.
