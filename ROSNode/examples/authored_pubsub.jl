# Authored-type pub/sub: a hand-written Julia struct *is* the ROS message.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/authored_pubsub.jl
#
# The authored counterpart to publisher.jl + subscriber.jl. Instead of `@ros_import`ing
# an existing interface, we DEFINE one in Julia: a plain struct marked `@ros_message`
# becomes a fully registered ROS type — real RIHS01, keyexpr resolution, the works —
# with no `.msg` file and no codegen step. Pub and sub run in one process here; split
# them into two (or talk to a ROS2 peer that has the matching definition) and they
# interoperate like any other type.

using ROSNode

# `@ros_package` names the ROS package these types belong to; `@ros_message` turns each
# struct into `demo_msgs/msg/<Name>` (lives at `demo_msgs.msg.<Name>`, bare name leaf-bound).
# Authored types compose: `Chatter` references `Header` by its Julia type, and the nested
# ref is resolved + hashed into `Chatter`'s RIHS exactly as a `.msg` nested field would be.
module Msgs
    using ROSNode
    @ros_package "demo_msgs"
    @ros_message struct Header;  stamp::Float64; frame::String;        end
    @ros_message struct Chatter; header::Header; data::String; seq::Int32; end   # nested authored ref
end

# To instead be wire-compatible with an *existing* ROS2 type (so `ros2 topic echo` and
# real nodes interoperate), rename a struct onto that identity with the annotate form —
# the Julia name stays yours, the ROS identity is the explicit one:
#     struct Chatter; data::String; end
#     @ros_message "std_msgs/msg/String" Chatter      # Chatter ⇔ std_msgs/msg/String

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "authored_talker")

    # Subscribe + publish the authored type by its Julia name — `@context` made this module
    # the resolution home, but passing the type explicitly resolves it straight from the registry.
    sub = Subscription(node, "/chatter", Msgs.Chatter) do msg
        @info "heard" msg.data seq = msg.seq frame = msg.header.frame
    end
    pub = Publisher(node, "/chatter", Msgs.Chatter)
    sleep(0.4)   # let discovery match sub ↔ pub before the first publish

    for i in 0:4
        publish(pub, Msgs.Chatter(
            header = Msgs.Header(stamp = 1.0 * i, frame = "demo"),
            data   = "hello world $i",
            seq    = Int32(i)))
        sleep(0.5)
    end
    sleep(0.3)   # let the subscriber drain the last sample before ctx closes
end
