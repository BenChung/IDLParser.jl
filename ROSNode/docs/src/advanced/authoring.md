# Authoring Interfaces in Julia

A hand-written Julia definition becomes a fully registered ROS type. `@ros_message`, `@ros_service`, and `@ros_action` derive the ROS interface — real RIHS01 identity, keyexpr resolution, wire compatibility — straight from the Julia type you write.

`@ros_package` names the package the types belong to, giving each one its wire identity. The macros derive the rest from what you write.

## Messages

A message is a struct. `@ros_message` turns each one into `<pkg>/msg/<Name>`, landing it at `<pkg>.msg.<Name>` with the bare name leaf-bound. Authored types compose: a struct that references another authored type by its Julia type resolves and hashes the nested ref into the parent's RIHS exactly as a `.msg` nested field would:

```julia
module Msgs
    using ROSNode
    @ros_package "demo_msgs"
    @ros_message struct Header;  stamp::Float64; frame::String;          end
    @ros_message struct Chatter; header::Header; data::String; seq::Int32; end
end
```

`Chatter` registers as `demo_msgs/msg/Chatter`. Publish and subscribe it by its Julia name; passing the type explicitly resolves it straight from the registry. The endpoint snippets below run inside a `node` from [The Runtime Model](../foundations/runtime-model.md):

```julia
pub = Publisher(node, "/chatter", Msgs.Chatter)
publish(pub, Msgs.Chatter(
    header = Msgs.Header(stamp = 1.0, frame = "demo"),
    data   = "hello world",
    seq    = Int32(0)))

sub = Subscription(node, "/chatter", Msgs.Chatter) do msg
    @info "heard" msg.data seq = msg.seq frame = msg.header.frame
end
```

### Mapping onto an existing identity

The annotate form `@ros_message "pkg/msg/Name" T` keeps your Julia name `T` and gives it the wire type — and matching RIHS01 — of `pkg/msg/Name`, so `T` interoperates with the stock endpoint for that type. A one-field struct whose `data::String` matches the single `string data` field of `std_msgs/msg/String` derives the identical hash:

```julia
module Msgs
    using ROSNode
    Base.@kwdef struct Chatter; data::String; end
    @ros_message "std_msgs/msg/String" Chatter
end
```

The annotate form registers the struct as written, so mark it `Base.@kwdef` yourself for keyword construction.

## Services

A service is a function: the parameters are the request fields, the `@NamedTuple` return type is the response, and the body is the handler. Throw to fail the call (the client's `call` then raises):

```julia
module Srv
    using ROSNode
    @ros_package "example_interfaces"
    @ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b,)
    end
end
```

Pass the authored function itself as the marker; ROSNode wires the request decode → handler → response encode from the registered service type. The client calls with the request fields as keyword arguments and receives the response as a NamedTuple:

```julia
srv = Service(node, "/add_two_ints", Srv.AddTwoInts)

client = ServiceClient(node, "/add_two_ints", Srv.AddTwoInts)
if wait_for_service(client; timeout = 5)
    resp = call(client; a = Int64(2), b = Int64(40))
    @info "response" sum = resp.sum
end
```

## Actions

An action is a function with a `FeedbackSink` parameter: the goal fields are parameters, the `FeedbackSink{@NamedTuple{…}}` parameter is the feedback channel, and the `@NamedTuple` return type is the result. Calling the sink publishes a Feedback message and checkpoints cancellation. How the body ends drives the [goal settlement three-way](../communication/actions.md#Goal-states):

- return the result → `:succeeded`,
- let a cancellation checkpoint in the sink throw → `:canceled`,
- throw anything else → `:aborted`.

```julia
module Act
    using ROSNode
    @ros_package "action_tutorials_interfaces"
    @ros_action function Fibonacci(order::Int32,
            fb::FeedbackSink{@NamedTuple{partial_sequence::Vector{Int32}}})::@NamedTuple{sequence::Vector{Int32}}
        seq = Int32[0, 1]
        for _ in 1:order
            push!(seq, seq[end] + seq[end-1])
            fb((partial_sequence = copy(seq),))
        end
        (sequence = seq,)
    end
end
```

Pass the authored function as the marker; ROSNode runs its body once per accepted goal and drives the accept/feedback/result protocol around it. The client sends a goal with its fields as keyword arguments and reads feedback as NamedTuples carrying `partial_sequence`:

```julia
server = ActionServer(node, "/fibonacci", Act.Fibonacci)

client = ActionClient(node, "/fibonacci", Act.Fibonacci)
if wait_for_action_server(client; timeout = 5)
    gh = send(client; order = Int32(8))       # blocks until the server accepts or rejects
    for fb in feedback(gh)                     # yields each Feedback message; empty if the goal was rejected
        @info "feedback" fb.partial_sequence
    end
    if state(gh) != :rejected
        result = fetch(gh)                    # raises on a rejected goal
        @info "result" result.sequence
    end
end
```

## Authoring or importing

An authored `@ros_message`/`@ros_service`/`@ros_action` carries the same RIHS01 as the equivalent `.msg`/`.srv`/`.action`, so it is wire-compatible with its imported twin and with ROS 2 nodes (see [Interoperating with ROS 2](../interop/ros2.md)). Author a type to keep its definition in Julia source alongside the code that uses it; [import](../foundations/interface-types.md) one to reuse a definition that already ships in a `.msg`/`.srv`/`.action` tree.

## See also

- [Interface Types](../foundations/interface-types.md) — importing existing interfaces and how wire identity works.
- [Runtime Type Discovery](discovery.md) — handling types you learn off the wire.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
@ros_package
@ros_message
@ros_service
@ros_action
FeedbackSink
goal_handle
```
