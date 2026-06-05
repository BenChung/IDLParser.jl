# Minimal subscriber: prints every std_msgs/String received on /chatter.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/subscriber.jl
#
# Pair it with `publisher.jl` (or `ros2 topic pub /chatter std_msgs/msg/String …`).

using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

Context(; peers = ["tcp/localhost:7447"], home=@__MODULE__) do ctx
    node = Node(ctx, "listener")

    # The do-block handler runs once per message. `msg` is an owned, decoded
    # `std_msgs.msg.String` — safe to keep or hand to another task. The default
    # `concurrency = Serial()` delivers in order on one task (no locking needed);
    # pass `Parallel(n)` to fan out across threads.
    sub = Subscription(node, "/chatter") do msg
        @info "heard" typeof(msg.data) msg.data
    end

    # Julia's scheduler is the executor — no explicit spin() needed. Just keep the
    # process alive while the subscription's task delivers messages.
    @info "listening on /chatter — Ctrl-C to stop"
    while true
        sleep(1.0)
    end
end

# A type-less subscription is also available: `Subscription(node, "/chatter") do msg … end`
# resolves the concrete type at runtime from discovery, so no `@ros_import` is
# needed — handy for tooling that doesn't know the type ahead of time.
