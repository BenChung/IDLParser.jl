# §7 RTime/Duration arithmetic + the cross-clock dispatch guarantee. All pure —
# no session/router. The point of the clock-tag is that mixing clocks is a
# *compile-time* MethodError, so we assert `!applicable(...)` rather than catching.

using ROSNode: RTime, Duration, System, Steady, ROS, ClockSource,
               nanoseconds, seconds, source, to_msg, rtime, duration,
               Clock, Rate, JumpCallback, TimeJump, register!, sim_activated, time_forward,
               time_backward
using Dates: Dates, Nanosecond, Millisecond, Second

# A builtin_interfaces/Time|Duration stand-in: the real generated structs aren't
# in scope here, so to_msg/rtime/duration are duck-typed on `sec`/`nanosec`.
struct WireTime
    sec::Int32
    nanosec::UInt32
end
WireTime(; sec, nanosec) = WireTime(Int32(sec), UInt32(nanosec))

# A node stand-in for the clock surface (Clock holds `node::Any`).
struct ClockMockNode
    clocks::Dict{DataType, Any}
end
ClockMockNode() = ClockMockNode(Dict{DataType, Any}())

@testset "time" begin
    @testset "instant − instant → span (same clock)" begin
        a = RTime{System}(1_000)
        b = RTime{System}(250)
        @test (a - b) === Duration(750)
        @test nanoseconds(a - b) == 750
        @test (b - a) == Duration(-750)        # spans can be negative
    end

    @testset "instant ± span stays on the clock" begin
        t = RTime{Steady}(100)
        @test t + Duration(50) === RTime{Steady}(150)
        @test t - Duration(50) === RTime{Steady}(50)
        @test Duration(50) + t === RTime{Steady}(150)   # commute
        # Dates.Period interop
        @test t + Millisecond(1) === RTime{Steady}(100 + 1_000_000)
        @test Millisecond(1) + t === RTime{Steady}(100 + 1_000_000)
    end

    @testset "cross-clock mixing has no method (compile-time guard)" begin
        a = RTime{System}(1)
        b = RTime{Steady}(1)
        c = RTime{ROS}(1)
        @test !applicable(-, a, b)
        @test !applicable(-, a, c)
        @test !applicable(-, b, c)
        @test !applicable(+, a, b)
        @test !applicable(isless, a, b)
        # `==` is exempt: it inherits Base's universal `Any,Any` fallback (a `false`
        # result, not a MethodError), so equality across clocks is merely always-false.
        @test (a == b) === false
        # same clock *is* applicable
        @test applicable(-, a, RTime{System}(2))
    end

    @testset "span algebra is clock-agnostic" begin
        @test Duration(3) + Duration(4) == Duration(7)
        @test Duration(10) - Duration(4) == Duration(6)
        @test -Duration(5) == Duration(-5)
        @test Duration(10) * 2 == Duration(20)
        @test 2 * Duration(10) == Duration(20)
        @test Duration(10) / 4 == Duration(round(Int64, 10 / 4))   # rounds
        @test Duration(0) == zero(Duration)
        @test zero(Duration(99)) == Duration(0)
        @test Duration(Millisecond(20)) == Duration(20_000_000)
    end

    @testset "comparisons + Period interop" begin
        @test RTime{System}(1) < RTime{System}(2)
        @test RTime{System}(2) == RTime{System}(2)
        @test Duration(5) < Duration(6)
        @test Duration(1_000_000) == Millisecond(1)
        @test Millisecond(1) > Duration(999_999)
        @test seconds(Duration(1_500_000_000)) ≈ 1.5
    end

    @testset "source tag + hashing" begin
        @test source(RTime{ROS}(0)) === ROS()
        @test source(RTime{Steady}(0)) === Steady()
        # the tag participates in the hash: same ns, different clock ⇒ differs
        @test hash(RTime{System}(5)) != hash(RTime{Steady}(5))
        @test hash(RTime{System}(5)) == hash(RTime{System}(5))
    end

    @testset "wire convert ↔ builtin_interfaces (sec/nanosec)" begin
        # exact round-trip on a positive instant
        t = RTime{ROS}(1_700_000_000_123_456_789)
        m = to_msg(WireTime, t)
        @test m.sec == 1_700_000_000
        @test m.nanosec == 123_456_789
        @test rtime(ROS, m) == t
        @test rtime(ROS(), m) == t

        # negative ns borrows a second so nanosec stays in [0, 1e9)
        d = Duration(-1)
        md = to_msg(WireTime, d)
        @test md.sec == -1
        @test md.nanosec == 999_999_999
        @test duration(md) == d           # round-trips back to -1 ns
    end

    @testset "Clock read on a mock node" begin
        node = ClockMockNode()
        cs = ROSNode.clock(node, Steady())
        @test cs isa Clock{Steady}
        t1 = Dates.now(cs)
        @test t1 isa RTime{Steady}
        t2 = Dates.now(cs)
        @test t2 >= t1                    # steady never goes backward
        # default clock is ROS
        @test ROSNode.clock(node) isa Clock{ROS}
        @test Dates.now(node, System()) isa RTime{System}
    end

    @testset "Rate frequency + sleep returns overrun flag" begin
        node = ClockMockNode()
        r = Rate(node, 1000.0; clock=Steady())   # 1ms period
        @test_throws ArgumentError Rate(node, 0.0)
        @test_throws ArgumentError Rate(node, -5.0)
        # `sleep(rate)` returns the overrun flag (Bool); the actual value is
        # timing-dependent (true if it slept, false if the loop already overran the
        # period), so we assert only the type, not wall timing.
        @test Base.sleep(r) isa Bool
    end

    @testset "JumpCallback threshold predicate" begin
        fired = Ref(0)
        cb = JumpCallback(_ -> (fired[] += 1);
                          min_forward=Millisecond(10), min_backward=Millisecond(5),
                          on_clock_change=true)
        # a sim activation always triggers (on_clock_change)
        @test ROSNode._triggers(cb, TimeJump(sim_activated, Duration(0)))
        # forward below threshold: no
        @test !ROSNode._triggers(cb, TimeJump(time_forward, Duration(1_000_000)))   # 1ms < 10ms
        # forward at/above threshold: yes
        @test ROSNode._triggers(cb, TimeJump(time_forward, Duration(10_000_000)))
        # backward at/above its threshold: yes
        @test ROSNode._triggers(cb, TimeJump(time_backward, Duration(-5_000_000)))
        # backward below threshold: no
        @test !ROSNode._triggers(cb, TimeJump(time_backward, Duration(-1_000_000)))
    end

    # The firing contract (the audit gap): a registered callback's `f` actually runs
    # when the jump dispatch driver fans to the clock the user holds, gated by
    # `_triggers`. Pure: drive `_fire_jumps_to!` directly on a mock node (no Context).
    @testset "JumpCallback fires through the dispatch driver" begin
        node = ClockMockNode()
        c = ROSNode.clock(node, ROS())            # cached in node.clocks[ROS] (stable handle)
        fired = Ref(0)
        register!(c, JumpCallback(_ -> (fired[] += 1);
                                  min_forward = Millisecond(10), on_clock_change = true))
        ROSNode._fire_jumps_to!(node, TimeJump(sim_activated, Duration(0)))
        @test fired[] == 1                        # source switch fires (on_clock_change)
        ROSNode._fire_jumps_to!(node, TimeJump(time_forward, Duration(1_000_000)))   # 1ms < 10ms
        @test fired[] == 1                        # below threshold: no fire
        ROSNode._fire_jumps_to!(node, TimeJump(time_forward, Duration(10_000_000)))  # 10ms
        @test fired[] == 2                        # at/above threshold: fires
    end
end
