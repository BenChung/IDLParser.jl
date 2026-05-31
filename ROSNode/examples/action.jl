# Action server + client in one process: example_interfaces/action/Fibonacci.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/action.jl
#
# Fibonacci is the canonical demo action: a Goal `order`, streamed Feedback
# `partial_sequence`, and a final Result `sequence`. Interoperates with the ROS2
# `ros2 action send_goal /fibonacci example_interfaces/action/Fibonacci "{order: 10}"`.

using ROSNode

# `@ros_import` of a fully-qualified action binds the bare `Fibonacci` namespace — a
# tag type that both *is* the action handle (pass it to the constructors) and exposes
# the sections by short name: `Fibonacci.Goal`, `Fibonacci.Feedback`, `Fibonacci.Result`
# (plus the protocol wrappers the runtime uses internally). Wire names are unchanged
# (`Fibonacci_Goal`, …).
@ros_import "example_interfaces/action/Fibonacci"

Context(; peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "fibonacci")

    # ── Server (high-level do-block) ──────────────────────────────────────────
    # The handler runs once per accepted goal. `goal` is a GoalHandle: `goal.request`
    # is the decoded Goal. `feedback!` publishes a Feedback message AND checkpoints
    # cancellation — if a client cancels, it throws `Cancelled` here and the goal
    # settles CANCELED automatically. Returning a Result settles the goal SUCCEEDED.
    server = ActionServer(node, "/fibonacci", Fibonacci) do goal
        seq = Int32[0, 1]
        for i in 1:goal.request.order
            push!(seq, seq[end] + seq[end-1])
            feedback!(goal, Fibonacci.Feedback(partial_sequence = copy(seq)))
            sleep(0.3)  # pretend the step is expensive; also a cancellation yield point
        end
        @info "goal complete" seq
        Fibonacci.Result(sequence = seq)
    end

    # ── Client ────────────────────────────────────────────────────────────────
    client = ActionClient(node, "/fibonacci", Fibonacci)
    sleep(0.3)  # let discovery match before sending

    # `send` blocks until the server accepts/rejects, then returns a handle.
    gh = send(client, Fibonacci.Goal(order = Int32(8)))
    @info "goal" state = state(gh)   # :accepted (or :rejected)

    # Iterate feedback as it streams; the loop ends when the goal reaches a
    # terminal state.
    for fb in feedback(gh)
        @info "feedback" fb.partial_sequence
    end

    # `fetch` blocks until the goal settles and returns the Result.
    result = fetch(gh)
    @info "result" sequence = result.sequence
    @info "final state" state = state(gh)   # :succeeded

    # To cancel a running goal instead: `cancel(gh)` — the server's `feedback!`/
    # `checkpoint` then throws `Cancelled` and the goal settles :canceled.

    close(client)
    close(server)
end
