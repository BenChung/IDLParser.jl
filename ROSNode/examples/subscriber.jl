# Minimal subscriber: prints every std_msgs/String received on /chatter.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/subscriber.jl
#
# Pair it with `publisher.jl` (or `ros2 topic pub /chatter std_msgs/msg/String …`).

using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

# `@context` binds this module as the Context's resolution home (sugar for
# `Context(; home=@__MODULE__, …)`), so the type-less `Subscription` below resolves to
# this module's `@ros_import`ed `String`.
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "listener")

    # The do-block handler runs once per message. `msg` is an owned, decoded
    # `std_msgs.msg.String` — safe to keep or hand to another task. The default
    # `concurrency = Serial()` delivers in order on one task (no locking needed);
    # pass `Parallel(n)` to fan out across threads.
    sub = Subscription(node, "/chatter") do msg
        @info "heard" typeof(msg.data) msg.data
    end

    # Messages are delivered on the scheduler's own tasks — `spin` is not an
    # rclcpp-style executor pump, it just parks the main task to keep the process
    # alive. `handle_signals=true` turns Ctrl-C into a graceful drain (clean liveliness
    # departure + session close); without it, Julia's default SIGINT path force-exits
    # and dumps a native backtrace of every thread (including libzenohc's tokio pool).
    @info "listening on /chatter — Ctrl-C to stop"
    spin(ctx; handle_signals = true)
end

# A type-less subscription is also available: `Subscription(node, "/chatter") do msg … end`
# resolves the concrete type at runtime from discovery, so no `@ros_import` is
# needed — handy for tooling that doesn't know the type ahead of time.
