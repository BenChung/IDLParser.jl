# §D8 — precompilation / warm-up. Pure-logic tests for the detector/macro/policy and
# the default-message builder, plus live tests that the per-entity warm-up neither
# breaks delivery (`:precompile`) nor leaks side effects (`:execute`).
#
# Standalone (outside the suite): run a router on :7447 (or set ROS_TEST_EP), then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/warmup.jl

using ROSNode
using ROSNode: Context, Node, Publisher, Subscription, publish, is_warming,
               WarmupPolicy, WireKeyValue
using Test
import Zenoh

const _WUEP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
              get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_wuctx(f::Function) = Context(f; peers = [_WUEP], localhost_only = true)

# Block up to `secs` for the next value on `ch`; `nothing` on timeout.
function _wurecv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint = 0.02) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

const _Time = ROSNode.Interfaces.builtin_interfaces.msg.Time   # all-fixed (@cdr_fixed)

@testset "D8 warm-up" begin

    # ── the detector + @effectful (no session) ────────────────────────────────
    @testset "is_warming / @effectful" begin
        @test is_warming() == false                       # plain runtime: not warming
        # The scope flips it true (and propagates to the value the block reads).
        @test Base.ScopedValues.with(() -> is_warming(), ROSNode._WARMUP => true) == true
        @test is_warming() == false                       # scope exited

        # @effectful runs + returns the value when not warming…
        r = Ref(0)
        @test (@effectful (r[] += 1)) == 1
        @test r[] == 1
        # …and is skipped (returns nothing) while warming, but the surrounding code
        # still ran (the guard is a runtime value, not a fold).
        ran = Ref(false)
        out = Base.ScopedValues.with(ROSNode._WARMUP => true) do
            ran[] = true
            @effectful (r[] += 1)
        end
        @test ran[] == true
        @test out === nothing
        @test r[] == 1                                    # effect suppressed: not incremented
    end

    @testset "WarmupPolicy validation" begin
        @test WarmupPolicy(:precompile, false).mode === ROSNode.Precompile()   # symbol shorthand
        @test WarmupPolicy(ROSNode.Execute(), true).mode === ROSNode.Execute() # WarmupMode form
        @test WarmupPolicy(:execute, true).sync == true
        @test_throws ArgumentError WarmupPolicy(:nonsense, false)
    end

    # ── default-message builder + codec round-trip (no session) ───────────────
    @testset "_default_msg builds + round-trips" begin
        # Fixed (@cdr_fixed) path.
        t = ROSNode._default_msg(_Time)
        @test t.sec == 0 && t.nanosec == 0
        # String (@kwdef / read_view) path.
        kv = ROSNode._default_msg(WireKeyValue)
        @test kv.key == "" && kv.value == ""
        # Both survive an encode → decode round-trip through the codec.
        for (T, m) in ((_Time, t), (WireKeyValue, WireKeyValue(key = "k", value = "v")))
            back = ROSNode.decode(Zenoh.as_memory(ROSNode.encode(m), UInt8), T)
            @test back == m
        end
    end

    # ── live: :precompile is delivery-transparent (regression) ────────────────
    @testset ":precompile does not break delivery" begin
        _wuctx() do ctx
            node = Node(ctx, "warm_pre")                  # default warmup = :precompile
            got  = Channel{Any}(8)
            sub  = Subscription(node, "/wp", WireKeyValue) do m
                put!(got, m)
            end
            pub  = Publisher(node, "/wp", WireKeyValue)
            sleep(0.3)                                     # let the route match
            publish(pub, WireKeyValue(key = "a", value = "b"))
            m = _wurecv(got, 5.0)
            @test m isa WireKeyValue && m.value == "b"
            close(sub); close(pub)
        end
    end

    # ── live: :execute runs the body but suppresses effects ───────────────────
    @testset ":execute warms the body, suppresses @effectful" begin
        _wuctx() do ctx
            node = Node(ctx, "warm_exec")
            body_ran = Ref(0)                              # unconditional: proves the body ran
            effect   = Ref(0)                              # @effectful: must NOT fire while warming
            # sync so the warm-up handler call completes before the constructor returns.
            sub = Subscription(node, "/we", WireKeyValue;
                               warmup = :execute, warmup_sync = true) do m
                body_ran[] += 1
                @effectful (effect[] += 1)
            end
            # After sync :execute warm-up: body ran once on the default sample, effect
            # was suppressed (but its branch still compiled).
            @test body_ran[] == 1
            @test effect[]   == 0

            # A real message fires both (the same chain, now warm).
            pub = Publisher(node, "/we", WireKeyValue)
            sleep(0.3)
            publish(pub, WireKeyValue(key = "k", value = "v"))
            @test timedwait(() -> body_ran[] >= 2, 5.0; pollint = 0.02) === :ok
            @test body_ran[] == 2
            @test effect[]   == 1                          # real delivery is not warming
            close(sub); close(pub)
        end
    end

    # ── live: :execute publisher warm-up null-routes the wire put ─────────────
    @testset ":execute publisher null-routes the warm put" begin
        _wuctx() do ctx
            node = Node(ctx, "warm_pub")
            got  = Channel{Any}(8)
            # Subscriber first, so it's matched before the publisher's warm-up fires.
            sub  = Subscription(node, "/wpub", WireKeyValue) do m
                put!(got, m)
            end
            sleep(0.3)
            # The :execute warm-up publishes a default sample once — but `_WARMUP[]`
            # null-routes the actual `put`, so nothing is delivered.
            pub = Publisher(node, "/wpub", WireKeyValue; warmup = :execute, warmup_sync = true)
            @test _wurecv(got, 0.5) === nothing            # warm publish did not go on the wire
            # A real publish does deliver.
            publish(pub, WireKeyValue(key = "k", value = "v"))
            @test _wurecv(got, 5.0) !== nothing
            close(sub); close(pub)
        end
    end
end
