# Actions

An action runs a long goal that streams feedback while it works and ends in a result. The client tracks progress as feedback arrives and can cancel the goal mid-flight.

This page serves and calls the Fibonacci action using an imported interface. [Authoring an action in Julia](../advanced/authoring.md) and [resolving its type at runtime](../advanced/discovery.md) build on the same flow.

## Imported interface

`@ros_import` of a fully-qualified action binds the bare `Fibonacci` namespace: a tag type that serves as the action handle and exposes the sections by short name (`Fibonacci.Goal`, `Fibonacci.Feedback`, `Fibonacci.Result`).

The server's handler runs once per accepted goal. `goal.request` is the decoded Goal, `feedback!` publishes a Feedback message and checkpoints cancellation, and returning a Result settles the goal `:succeeded`.

```julia
using ROSNode

@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")

    server = ActionServer(node, "/fibonacci", Fibonacci) do goal
        seq = Int32[0, 1]
        for i in 1:goal.request.order
            push!(seq, seq[end] + seq[end-1])
            feedback!(goal, Fibonacci.Feedback(partial_sequence = copy(seq)))  # publishes feedback + checkpoints cancellation
            sleep(0.3)  # simulate work
        end
        Fibonacci.Result(sequence = seq)
    end

    spin(ctx; handle_signals = true)
end
```

The feedback field is `partial_sequence`, matching `action_tutorials_interfaces`. The action is wire-compatible with ROS 2 (see [Interoperating with ROS 2](../interop/ros2.md)).

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

`wait_for_action_server` returns `true` once a server route-matches within the timeout, gating `send` on a reachable server. The feedback loop drives the result protocol behind the scenes, so iterating it is the whole client lifecycle.

## Goal states

`state(gh)` reports where a goal sits in its lifecycle. Step a goal through it — click a transition to advance, any state to read it:

```@raw html
<div class="rosnode-statechart" data-machine="goal"></div>
```

How the handler ends the goal is the [settlement three-way](services.md) again, specialized: returning a Result settles `:succeeded`, a checkpoint raising [`Cancelled`](@ref) settles `:canceled`, and any other throw settles `:aborted`. `:rejected` is a client-side handle state — the server declined the goal with [`reject`](@ref) and never registered it.

## Cancelling a goal

Call `cancel(gh)` on the client to cancel a running goal. The server's next `feedback!` checkpoint throws `Cancelled`, the handler unwinds, and the goal settles `:canceled`. Every `feedback!` call doubles as a cancellation checkpoint, so a handler that publishes feedback as it works yields cancellation points for free.

```julia
gh = send(client, Fibonacci.Goal(order = Int32(8)))
cancel(gh)                       # request cancellation
state(gh)                        # :canceling, then :canceled once it settles
```

## Next

[Parameters](parameters.md) configure a node's behavior over the same wire. See also [Services](services.md) for request/response calls and [Interface Types](../foundations/interface-types.md) for how imported and authored action types share one wire identity.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
ActionServer
ActionClient
GoalHandle
send
state
fetch
feedback!
checkpoint
iscancelled
succeed
abort
execute
cancel
accept
reject
defer
action_server_matched
SingleFlight
ActionTypeSupport
GoalState
NodeInactiveError
```
