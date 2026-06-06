# §15.1 intra-process short-circuit. Same-Context publisher → subscriber delivery
# that skips the CDR serialize, the Zenoh hop, and the decode. Wired but OFF by
# default, so this suite opts in (set_intra_process!(true)) and restores the default.
#
# The definitive observable is object identity on a Vector field: a `view=true`
# (shared) intra-process delivery hands the subscriber the SAME `samples` Vector the
# publisher built (===); a Zenoh-decoded copy is always a fresh Vector. So `===`
# proves the direct zero-copy path was taken (not the loopback), and exactly-once
# delivery proves the loopback is suppressed (no double-delivery). Widget carries a
# `float64[] samples` field, which is what makes this check possible.

using ROSNode
using ROSNode: Context, Node, Publisher, Subscription, publish, set_intra_process!
using Test

module _IpcTypes
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/msg/Widget"
end
const Widget = _IpcTypes.robot_msgs.msg.Widget

const _IPCEP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
               get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_ipcctx(f::Function) = Context(f; peers = [_IPCEP], localhost_only = true)

@testset "intra-process short-circuit (§15.1)" begin
    prev = set_intra_process!(true)
    try
        @testset "direct zero-copy delivery (view=true): same object, no Zenoh hop" begin
            _ipcctx() do ctx
                node = Node(ctx, "ipc_view")
                got  = Channel{Any}(8)
                # view=true shares the publisher's object read-only (true zero-copy).
                sub = Subscription(node, "/ipc", Widget; view = true) do m
                    put!(got, m)
                end
                pub = Publisher(node, "/ipc", Widget)
                sleep(0.4)                                    # let routes match
                samples = [1.0, 2.0, 3.0]
                publish(pub, Widget(label = "x", count = Int32(1), samples = samples))
                timedwait(() -> isready(got), 5.0; pollint = 0.02)
                @test isready(got)
                m = take!(got)
                @test m.samples === samples                   # SAME Vector ⇒ direct zero-copy (a Zenoh decode is a fresh Vector)
                sleep(0.2)
                @test !isready(got)                           # exactly once: loopback suppressed, no double-delivery
            end
        end

        @testset "owned delivery (default), all messages exactly once" begin
            _ipcctx() do ctx
                node = Node(ctx, "ipc_owned")
                got  = Channel{Any}(64)
                sub = Subscription(node, "/ipc2", Widget) do m   # Owned() default ⇒ an independent copy
                    put!(got, m)
                end
                pub = Publisher(node, "/ipc2", Widget)
                sleep(0.4)
                N = 10
                for i in 1:N
                    publish(pub, Widget(label = "m", count = Int32(i), samples = Float64[i]))
                    sleep(0.005)
                end
                recv = Int[]
                timedwait(5.0; pollint = 0.02) do
                    while isready(got); push!(recv, Int(take!(got).count)); end
                    length(recv) >= N
                end
                @test length(recv) == N                       # all delivered, exactly once (no loopback double)
                @test sort(recv) == collect(1:N)              # Serial ⇒ order preserved
            end
        end

        @testset "disabled: rides the Zenoh loopback (a decoded copy)" begin
            set_intra_process!(false)                         # sub below opens with allowed_origin = ANY
            _ipcctx() do ctx
                node = Node(ctx, "ipc_off")
                got  = Channel{Any}(8)
                sub = Subscription(node, "/ipc3", Widget) do m   # owned
                    put!(got, m)
                end
                pub = Publisher(node, "/ipc3", Widget)
                sleep(0.4)
                samples = [9.0]
                publish(pub, Widget(label = "z", count = Int32(7), samples = samples))
                timedwait(() -> isready(got), 5.0; pollint = 0.02)
                @test isready(got)
                m = take!(got)
                @test Int(m.count) == 7                       # delivered over Zenoh
                @test m.samples == samples                    # value equal,
                @test m.samples !== samples                   # but a decoded copy — NOT the shared object
            end
            set_intra_process!(true)
        end
    finally
        set_intra_process!(prev)                              # restore the suite default (off)
    end
end
