# Authored type that *is* std_msgs/msg/String on the wire — interoperates with the
# classic /chatter endpoint.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/authored_pubsub_std.jl
#
# Same shape as authored_pubsub.jl, but instead of minting a new `demo_msgs` type we RENAME
# a Julia struct onto an existing ROS identity. The annotate form `@ros_message "pkg/msg/Name" T`
# keeps the Julia name `T` but gives it the wire type — and matching RIHS01 — of `pkg/msg/Name`.
# Since `std_msgs/msg/String` is just `string data`, the derived hash equals the published one,
# so this publisher/subscriber is wire-compatible with the stock examples and real ROS2:
#
#   • this publisher  ↔  classic `subscriber.jl`  (or `ros2 topic echo /chatter std_msgs/msg/String`)
#   • classic `publisher.jl`  ↔  this subscriber   (or `ros2 topic pub /chatter std_msgs/msg/String "{data: hi}"`)

using ROSNode

# The struct can't be named `String` (clashes with Base), so we name it `Chatter` and bind it to
# the ROS identity with the annotate form. `data::String` matches the single `string data` field
# of std_msgs/msg/String, so the authored RIHS01 is identical to the published type's. The annotate
# form renames an *existing* struct as-is (unlike `@ros_message struct …`, which auto-adds keyword
# construction), so we mark it `@kwdef` ourselves to publish with `Chatter(data = …)`.
module Msgs
    using ROSNode
    Base.@kwdef struct Chatter; data::String; end
    @ros_message "std_msgs/msg/String" Chatter      # Julia `Chatter` ⇔ ROS std_msgs/msg/String
end

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "authored_talker")

    # Pub + sub by the Julia name; on the wire it's std_msgs/msg/String, so a stock `subscriber.jl`
    # (or `ros2 topic echo /chatter std_msgs/msg/String`) sees these messages too.
    sub = Subscription(node, "/chatter", Msgs.Chatter) do msg
        @info "heard" msg.data
    end
    #pub = Publisher(node, "/chatter", Msgs.Chatter)
    #sleep(0.4)   # let discovery match sub ↔ pub before the first publish

    #for i in 0:4
    #    publish(pub, Msgs.Chatter(data = "hello world $i"))
    #    sleep(0.5)
    #end
    spin(ctx; handle_signals = true)
end
