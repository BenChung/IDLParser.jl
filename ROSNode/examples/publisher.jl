# Minimal publisher: publishes a std_msgs/String on /chatter once a second.
#
# Run a Zenoh router first (rmw_zenoh's discovery is router-based), then:
#     zenohd -l tcp/localhost:7447 &
#     julia --project=ROSNode ROSNode/examples/publisher.jl   # from the workspace root
#
# Pair it with `subscriber.jl`, or with a real ROS2 `ros2 topic echo /chatter`
# (with rmw_zenoh and a matching router/domain).

using ROSNode

# Bake the standard type by name. `@ros_import` resolves it from ROSNode's
# vendored tree or, inside a sourced ROS2 env, from AMENT_PREFIX_PATH — and lands
# it at `std_msgs.msg.String` in this module. (For your own interfaces, add a local
# source root: `@ros_import from="interfaces" "my_pkg/msg/MyType"`.)
@ros_import "std_msgs/msg/String" from="interfaces"

# `@context` opens the Context — owner of the Zenoh session, discovery, and shutdown —
# binding this module as its type-resolution home (sugar for `Context(; home=@__MODULE__, …)`).
# The do-block form drains and closes everything on exit; `peers` points at the router.
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)

    # `spin` parks the main task; the publish loop runs on a background task so the two
    # coexist. `handle_signals=true` makes Ctrl-C a graceful drain — the loop sees
    # `isopen(ctx)` flip and stops — instead of Julia's default SIGINT force-exit that
    # dumps a native backtrace of every thread (including libzenohc's tokio pool).
    @info "publishing on /chatter — Ctrl-C to stop"
    Threads.@spawn for i in 0:typemax(Int)
        isopen(ctx) || break
        msg = std_msgs.msg.String(data = "hello world $i")
        publish(pub, msg)
        @info "published" msg.data
        sleep(1.0)
    end
    spin(ctx; handle_signals = true)
end
