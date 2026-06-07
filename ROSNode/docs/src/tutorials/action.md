# Creating an Action Server and Client

An action runs a long goal that streams feedback while it works and ends in a result. The client tracks progress as feedback arrives and can cancel the goal mid-flight. This page tracks the ROS 2 "Writing an action server and client" tutorial using the Fibonacci action.

## Imported interface

`@ros_import` of a fully-qualified action binds the bare `Fibonacci` namespace: a tag type that serves as the action handle and exposes the sections by short name (`Fibonacci.Goal`, `Fibonacci.Feedback`, `Fibonacci.Result`).

The server's handler runs once per accepted goal. `goal.request` is the decoded Goal, `feedback!` publishes a Feedback message and checkpoints cancellation, and returning a Result settles the goal SUCCEEDED.

```julia
using ROSNode

@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")

    server = ActionServer(node, "/fibonacci", Fibonacci) do goal
        seq = Int32[0, 1]
        for i in 1:goal.request.order
            push!(seq, seq[end] + seq[end-1])
            feedback!(goal, Fibonacci.Feedback(partial_sequence = copy(seq)))
            sleep(0.3)  # also a cancellation checkpoint
        end
        Fibonacci.Result(sequence = seq)
    end

    spin(ctx; handle_signals = true)
end
```

The client sends a goal, streams feedback until the goal settles, then reads the cached result. `send` blocks until the server accepts or rejects and returns a goal handle.

```julia
using ROSNode

@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")
    client = ActionClient(node, "/fibonacci", Fibonacci)

    if wait_for_action_server(client; timeout = 5)
        gh = send(client, Fibonacci.Goal(order = Int32(8)))
        @info "goal" state = state(gh)   # :accepted (or :rejected)

        for fb in feedback(gh)           # loop ends when the goal settles
            @info "feedback" fb.partial_sequence
        end

        result = fetch(gh)               # cached result after settle
        @info "result" sequence = result.sequence
        @info "final state" state = state(gh)   # :succeeded
    end
    close(client)
end
```

## Authoring the action in Julia

`@ros_action` defines an action as a Julia function. Goal fields become parameters, a `FeedbackSink{@NamedTuple{…}}` parameter is the feedback channel, and the `@NamedTuple` return type is the result. `@ros_package "action_tutorials_interfaces"` gives the function the wire identity `action_tutorials_interfaces/action/Fibonacci`.

This schema names its feedback field `partial_sequence`. Calling the feedback sink `fb((partial_sequence = …,))` publishes a Feedback message and checkpoints cancellation; returning the result NamedTuple settles SUCCEEDED.

```julia
module Act
    using ROSNode
    @ros_package "action_tutorials_interfaces"
    @ros_action function Fibonacci(order::Int32,
            fb::FeedbackSink{@NamedTuple{partial_sequence::Vector{Int32}}})::@NamedTuple{sequence::Vector{Int32}}
        seq = Int32[0, 1]
        for _ in 1:order
            push!(seq, seq[end] + seq[end-1])
            fb((partial_sequence = copy(seq),))   # publish feedback + cancellation checkpoint
            sleep(0.3)
        end
        (sequence = seq,)
    end
end
```

Pass the authored function as the marker. ROSNode runs its body per goal and drives the accept/feedback/result protocol around it.

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")
    server = ActionServer(node, "/fibonacci", Act.Fibonacci)
    spin(ctx; handle_signals = true)
end
```

The client passes goal fields as keyword arguments and receives feedback as NamedTuples carrying `partial_sequence`.

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")
    client = ActionClient(node, "/fibonacci", Act.Fibonacci)

    if wait_for_action_server(client; timeout = 5)
        gh = send(client; order = Int32(8))   # goal fields as kwargs
        @info "goal" state = state(gh)

        for fb in feedback(gh)
            @info "feedback" fb.partial_sequence
        end

        result = fetch(gh)                    # NamedTuple result
        @info "result" result.sequence
    end
    close(client)
end
```

Both examples target `action_tutorials_interfaces/action/Fibonacci` with identical fields: a `partial_sequence` feedback array and a `sequence` result. A type's RIHS01 identity follows its name and field structure, so the imported and authored forms share one wire identity and interoperate with C++/Rust/Python nodes.

## Discovering the action at runtime

Inside a `@context` do-block, the calling module is the type-resolution home, so ROSNode resolves an action's sections from discovery when given the bare name. See [Interface Types](../interfaces.md) for how runtime resolution and authored types share one wire identity.

## Running it and cancelling a goal

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is router-based. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router. For environment discovery, omit `peers` and set `localhost_only` / `domain_id` as needed. `localhost_only = true` confines discovery to the configured router; omit it to reach peers on other hosts.

`action.jl` runs both halves in one process, so it is self-contained — just point it at the router. The `ROLE` env var or first CLI arg (defaulting to `both`) splits the halves across two processes:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/action.jl server    # serve only (Ctrl-C to stop)
julia --project=. ROSNode/examples/action.jl client    # send a goal (needs a server)
```

With `rmw_zenoh` selected and a matching router plus `ROS_DOMAIN_ID`, the server answers a real ROS 2 client over the wire:

```sh
ros2 action send_goal /fibonacci action_tutorials_interfaces/action/Fibonacci "{order: 8}"
```

To cancel a running goal, call `cancel(gh)` on the client. The server's next `feedback!` (imported) or `fb(…)` (authored) checkpoint throws `Cancelled`, and the goal settles `:canceled`.

See [Interface Types](../interfaces.md) for how `@ros_import`, authored types, and runtime discovery resolve to one wire identity.
