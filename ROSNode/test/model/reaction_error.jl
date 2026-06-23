# Reaction-error policy — how a node responds when a fire-and-forget reaction (a subscription
# handler or an `every`/timer callback) throws an uncaught exception. Default `ShutdownOnError`
# logs once and gracefully drains the Context (so a fast failing reaction can't flood the log and
# starve Ctrl-C); `ContinueOnError` logs (rate-limited) and keeps the reaction running. Services
# and actions are unaffected (they reply with an error). Every "did it drain?" / "did it stay up?"
# assertion is a bounded `timedwait`, so a regression fails the test instead of wedging the suite.
#
# Live against the per-run private router (see runtests.jl): a Context opens a real Zenoh session,
# so we connect ONLY to that endpoint (multicast off).

using ROSNode
using ROSNode: Context, Node, Publisher, Subscription, publish, spin,
               is_shutdown, request_shutdown, ShutdownOnError, ContinueOnError, Duration
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
const _Time = ROSNode.Interfaces.builtin_interfaces.msg.Time

_rectx(; on_reaction_error = :shutdown) =
    Context(; peers = [_EP], localhost_only = true, drain_timeout = 1.0,
              on_reaction_error = on_reaction_error)

_drains_within(ctx, secs) = Base.timedwait(() -> is_shutdown(ctx), Float64(secs); pollint = 0.02) === :ok

@testset "reaction error: policy plumbing" begin
    @test ROSNode._reaction_error_policy(:shutdown) === ShutdownOnError()
    @test ROSNode._reaction_error_policy(:continue) === ContinueOnError()
    @test ROSNode._reaction_error_policy(ContinueOnError()) === ContinueOnError()
    @test_throws ArgumentError ROSNode._reaction_error_policy(:nope)

    ctx = _rectx()
    @test ctx.reaction_error === ShutdownOnError()            # the default
    close(ctx)
    ctx2 = _rectx(; on_reaction_error = :continue)
    @test ctx2.reaction_error === ContinueOnError()
    close(ctx2)
end

@testset "reaction error: timer drains by default" begin
    ctx = _rectx()
    n = Node(ctx, "tdrain")
    fired = Ref(0)
    t = ROSNode.Timer(n, Duration(50_000_000)) do      # 50 ms; throws every tick
        fired[] += 1
        error("boom-timer")
    end
    spinner = @async try; spin(ctx); catch; end
    @test _drains_within(ctx, 5.0)                     # one throw → graceful drain
    @test fired[] >= 1
    @test istaskdone(spinner) || (sleep(0.2); istaskdone(spinner))   # spin returned on drain
    close(ctx)
end

@testset "reaction error: timer keeps ticking on :continue" begin
    ctx = _rectx(; on_reaction_error = :continue)
    n = Node(ctx, "tcont")
    fired = Ref(0)
    t = ROSNode.Timer(n, Duration(50_000_000)) do
        fired[] += 1
        error("boom-timer-c")
    end
    sleep(0.6)
    @test !is_shutdown(ctx)                            # stayed up despite throwing
    @test fired[] >= 2                                 # …and kept firing
    close(t); close(ctx)
end

@testset "reaction error: subscription handler drains by default" begin
    ctx = _rectx()
    nsub = Node(ctx, "rsub")
    got = Ref(0)
    Subscription(nsub, "/reacterr/boom", _Time) do msg
        got[] += 1
        error("boom-handler")
    end
    npub = Node(ctx, "rpub")
    p = Publisher(npub, "/reacterr/boom", _Time)
    sleep(0.6)                                         # discovery match
    spinner = @async try; spin(ctx); catch; end
    for _ in 1:40                                      # publish until the throw drains the node
        try; publish(p, _Time(sec = Int32(0), nanosec = UInt32(0))); catch; end
        is_shutdown(ctx) && break
        sleep(0.05)
    end
    @test _drains_within(ctx, 5.0)
    @test got[] >= 1
    close(ctx)
end

@testset "reaction error: subscription handler survives on :continue" begin
    ctx = _rectx(; on_reaction_error = :continue)
    nsub = Node(ctx, "rsubc")
    got = Ref(0)
    Subscription(nsub, "/reacterr/boomc", _Time) do msg
        got[] += 1
        error("boom-handler-c")
    end
    npub = Node(ctx, "rpubc")
    p = Publisher(npub, "/reacterr/boomc", _Time)
    sleep(0.6)
    for _ in 1:10
        try; publish(p, _Time(sec = Int32(0), nanosec = UInt32(0))); catch; end
        sleep(0.05)
    end
    sleep(0.3)
    @test !is_shutdown(ctx)                            # node stayed up
    @test got[] >= 2                                   # …and kept delivering through the throws
    close(ctx)
end
