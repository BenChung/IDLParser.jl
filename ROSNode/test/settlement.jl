# §8/§9 the write-once result cell + fail-safe settlement. Pure: a ResultCell is
# generic over the handle and result type with a `deliver` closure we supply, so
# every path (return / respond! / throw / non-defaultable result) is exercisable
# without any wire. The invariant under test: the cell is filled exactly once and
# a client can never hang.

using ROSNode: ResultCell, respond!, fill!, isfilled, outcome, settle_handler!,
               force_abort!, success, failure, succeeded, canceled, aborted,
               feedback, Cancelled, is_terminal, wait_settled

# A recording deliver closure: captures (status, payload) per call so we can
# assert exactly-once delivery and the settled outcome.
mutable struct Recorder
    calls::Vector{Tuple{Any, Any}}
end
Recorder() = Recorder(Tuple{Any, Any}[])
recorder_deliver(rec::Recorder) = (status, payload) -> push!(rec.calls, (status, payload))

# A minimal handle stand-in (services pass the request, actions a GoalHandle).
struct FakeHandle end

make_cell(rec::Recorder) =
    ResultCell{FakeHandle, Int}(FakeHandle(), recorder_deliver(rec))

@testset "settlement" begin
    @testset "respond! fills once and delivers" begin
        rec = Recorder()
        cell = make_cell(rec)
        @test !isfilled(cell)
        @test outcome(cell) === nothing
        @test respond!(cell, success, 42) === true
        @test isfilled(cell)
        @test outcome(cell) === success
        @test rec.calls == [(success, 42)]
    end

    @testset "explicit double respond! is the one hard error" begin
        rec = Recorder()
        cell = make_cell(rec)
        respond!(cell, succeeded, 1)
        @test_throws ArgumentError respond!(cell, succeeded, 2)
        @test rec.calls == [(succeeded, 1)]          # second never delivered
        @test outcome(cell) === succeeded
    end

    @testset "feedback is not terminal and is rejected by respond!" begin
        @test is_terminal(success)
        @test is_terminal(aborted)
        @test !is_terminal(feedback)
        rec = Recorder()
        cell = make_cell(rec)
        @test_throws ArgumentError respond!(cell, feedback, 0)
        @test !isfilled(cell)
        @test isempty(rec.calls)
    end

    @testset "fill! is non-authoritative: ignored once full" begin
        rec = Recorder()
        cell = make_cell(rec)
        @test fill!(cell, succeeded, 7) === true
        @test fill!(cell, aborted, 9) === false      # ignored, not an error
        @test outcome(cell) === succeeded
        @test rec.calls == [(succeeded, 7)]
        # feedback through fill! is a no-op (non-terminal)
        @test fill!(cell, feedback, 0) === false
    end

    @testset "settle_handler!: normal return ⇒ success" begin
        rec = Recorder()
        cell = make_cell(rec)
        out = settle_handler!(cell, () -> 99;
                              success_status=succeeded, default_result=() -> 0)
        @test out === succeeded
        @test rec.calls == [(succeeded, 99)]
    end

    @testset "settle_handler!: Cancelled ⇒ canceled (no error log path)" begin
        rec = Recorder()
        cell = make_cell(rec)
        out = settle_handler!(cell, () -> throw(Cancelled());
                              success_status=succeeded, default_result=() -> -1)
        @test out === canceled
        @test rec.calls == [(canceled, -1)]
    end

    @testset "settle_handler!: arbitrary throw ⇒ aborted (fail-safe)" begin
        rec = Recorder()
        cell = make_cell(rec)
        out = settle_handler!(cell, () -> error("boom");
                              success_status=succeeded, default_result=() -> -2)
        @test out === aborted
        @test rec.calls == [(aborted, -2)]
    end

    @testset "settle_handler!: handler already responded ⇒ return value ignored" begin
        rec = Recorder()
        cell = make_cell(rec)
        out = settle_handler!(cell, () -> (respond!(cell, canceled, 5); 99);
                              success_status=succeeded, default_result=() -> 0)
        @test out === canceled                       # explicit respond! wins
        @test rec.calls == [(canceled, 5)]           # return value (99) not delivered
    end

    @testset "force_abort! backstop: non-defaultable result can't hang a caller" begin
        rec = Recorder()
        cell = make_cell(rec)
        # default_result itself throws → catch-path fill fails → finally force_abort!
        out = settle_handler!(cell, () -> error("boom");
                              success_status=succeeded,
                              default_result=() -> error("no default"))
        @test out === aborted
        @test isfilled(cell)
        @test rec.calls == [(aborted, nothing)]      # nothing payload ⇒ zero-filled bytes
    end

    @testset "force_abort! is infallible even if deliver throws" begin
        # deliver throws on the abort path; force_abort! must latch + swallow.
        cell = ResultCell{FakeHandle, Int}(FakeHandle(),
            (status, payload) -> error("delivery exploded"))
        # The swallowed delivery error is logged (with a backtrace) by design;
        # assert it here so the expected error is captured, not leaked to stderr.
        @test (@test_logs (:error, r"force_abort! delivery failed") force_abort!(cell)) === true
        @test isfilled(cell)
        @test outcome(cell) === aborted
        # a second force_abort! is a no-op
        @test force_abort!(cell) === false
    end

    @testset "wait_settled: returns immediately if already filled" begin
        rec = Recorder()
        cell = make_cell(rec)
        respond!(cell, succeeded, 1)
        @test wait_settled(cell, () -> false) === true
    end

    @testset "wait_settled: wakes the instant the cell fills" begin
        rec = Recorder()
        cell = make_cell(rec)
        waiter = Threads.@spawn wait_settled(cell, () -> false)
        # Settle from another task; the waiter must wake via notify, not the timer.
        Threads.@spawn (sleep(0.05); respond!(cell, succeeded, 7))
        @test fetch(waiter) === true
        @test outcome(cell) === succeeded
    end

    @testset "wait_settled: abandons when should_abandon fires" begin
        rec = Recorder()
        cell = make_cell(rec)                            # never filled
        abandon = Threads.Atomic{Bool}(false)
        waiter = Threads.@spawn wait_settled(cell, () -> abandon[]; recheck=0.02)
        Threads.@spawn (sleep(0.05); abandon[] = true)
        @test fetch(waiter) === false                    # bailed, did not hang
        @test !isfilled(cell)
    end

    @testset "wait_settled: a full cell wins over abandonment" begin
        rec = Recorder()
        cell = make_cell(rec)
        respond!(cell, aborted, 3)
        @test wait_settled(cell, () -> true) === true    # filled checked first
    end

    @testset "concurrent settle: exactly one winner" begin
        rec = Recorder()
        cell = make_cell(rec)
        # Race many fill! tasks; only the first delivers.
        @sync for i in 1:50
            Threads.@spawn fill!(cell, succeeded, i)
        end
        @test isfilled(cell)
        @test length(rec.calls) == 1                 # delivered exactly once
        @test rec.calls[1][1] === succeeded
    end
end
