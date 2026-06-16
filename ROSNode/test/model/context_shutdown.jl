# §14 — Context drain / shutdown root. The drain is the RAII/shutdown invariant
# ("a drain that started always completes, so a blocked main task can never
# hang") and the one path every trigger (close / request_shutdown / signal)
# funnels through. These tests pin that contract: hooks run in order and exactly
# once, a slow hook is bounded (the drain proceeds), concurrent requesters are
# idempotent, registered resources undeclare in reverse, and `wait`/`spin` return
# promptly once drained. Every "did it finish?" assertion is a bounded `timedwait`
# so a regression that hangs the drain FAILS the test instead of wedging the suite.
#
# Live against the per-run private router (see runtests.jl): a Context opens a real
# Zenoh session, so we connect ONLY to that endpoint (multicast off).

using ROSNode
using ROSNode: Context, on_shutdown, request_shutdown, is_shutdown, spin,
               register_resource!
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
# A short drain_timeout keeps the "slow hook" case fast; entities here are trivial
# (no routes), so teardown is well under it.
_sdctx(; drain_timeout = 1.0) =
    Context(; peers = [_EP], localhost_only = true, drain_timeout = drain_timeout)

# Run `body()` on its own task and assert it finishes within `secs` — the bounded
# form of "the drain completed". A hang (the bug this whole file guards) trips the
# `@test`, it does not wedge the run.
function _finishes_within(body::Function, secs::Real)
    t = @async body()
    Base.timedwait(() -> istaskdone(t), Float64(secs); pollint = 0.02) === :ok
end

# A close-able stub for the reverse-order undeclare test: records its label into a
# shared sink when `close`d. `register_resource!` is duck-typed on `close(r)`, so a
# bare handle is enough — no real Node/entity construction needed.
mutable struct _CloseRec
    label::Symbol
    sink::Vector{Symbol}
    closed::Bool
end
_CloseRec(label, sink) = _CloseRec(label, sink, false)
function Base.close(r::_CloseRec)
    r.closed = true
    push!(r.sink, r.label)
    nothing
end

@testset "Context drain / shutdown (§14)" begin

    # (1) close(ctx) runs every registered on_shutdown hook, in registration order,
    # before returning — and the Context lands in the terminal shutdown state.
    @testset "on_shutdown hooks run in order" begin
        ctx = _sdctx()
        order = Int[]
        for i in 1:4
            on_shutdown(() -> push!(order, i), ctx)
        end
        @test _finishes_within(5.0) do
            close(ctx)
        end
        @test order == [1, 2, 3, 4]
        @test is_shutdown(ctx)
    end

    # (2) A hook that overruns drain_timeout must NOT stall the drain: it is bounded
    # (left running detached) and the drain still completes. Asserted by a timedwait
    # that is far shorter than the hook's sleep — if the drain waited on the hook,
    # close would blow past it and the test fails.
    @testset "a slow on_shutdown hook is bounded; drain still completes" begin
        ctx = _sdctx(drain_timeout = 0.3)
        ran = Threads.Atomic{Bool}(false)
        on_shutdown(ctx) do
            sleep(5.0)                 # >> drain_timeout
            ran[] = true
        end
        fast = Threads.Atomic{Bool}(false)
        on_shutdown(() -> (fast[] = true), ctx)  # the next hook still gets to run
        # close must return well under the slow hook's 5s sleep (timeout bounds it).
        @test _finishes_within(2.0) do
            close(ctx)
        end
        @test is_shutdown(ctx)
        @test fast[]                   # bounding one hook didn't skip the rest
    end

    # (3) request_shutdown is idempotent under concurrent callers: many tasks race
    # to initiate, exactly one wins (returns true), no caller errors, and the hooks
    # fire exactly once. Then wait(ctx) completes.
    @testset "request_shutdown is idempotent under concurrent callers" begin
        ctx = _sdctx()
        hook_runs = Threads.Atomic{Int}(0)
        on_shutdown(() -> Threads.atomic_add!(hook_runs, 1), ctx)

        results = Vector{Bool}(undef, 16)
        tasks = map(1:16) do i
            @async (results[i] = request_shutdown(ctx))
        end
        for t in tasks
            @test _finishes_within(5.0) do
                wait(t)                # no caller throws
            end
        end
        @test count(identity, results) == 1     # exactly one initiator

        @test _finishes_within(5.0) do
            wait(ctx)                  # everyone converges on the same drain
        end
        @test is_shutdown(ctx)
        @test hook_runs[] == 1         # hooks fire once, not once-per-caller
    end

    # (4) Registered resources undeclare in REVERSE registration order (step 4 of the
    # drain) so dependents close before their dependencies.
    @testset "resources undeclare in reverse order" begin
        ctx = _sdctx()
        closed = Symbol[]
        recs = [_CloseRec(s, closed) for s in (:a, :b, :c, :d)]
        for r in recs
            register_resource!(ctx, r)
        end
        @test _finishes_within(5.0) do
            close(ctx)
        end
        @test closed == [:d, :c, :b, :a]
        @test all(r -> r.closed, recs)
    end

    # (5) wait(ctx) and spin(ctx) (no signal handling) park only until the drain is
    # done — both return promptly once it has. We drain first, then assert each
    # returns immediately; a second close is a no-op park that also returns at once.
    @testset "wait/spin return promptly after the drain" begin
        ctx = _sdctx()
        close(ctx)                     # drive the drain to completion first
        @test is_shutdown(ctx)
        @test _finishes_within(2.0) do
            wait(ctx)                  # already drained ⇒ immediate
        end
        @test _finishes_within(2.0) do
            spin(ctx)                  # parks on wait(ctx); already drained ⇒ immediate
        end
        @test _finishes_within(2.0) do
            close(ctx)                 # second close is a no-op park
        end
    end

    # (6) Interrupt → graceful drain. `_park_until_drained` catches an `InterruptException`
    # delivered to the spin task and drives request_shutdown (the interactive Ctrl-C path;
    # deployed, a real SIGINT instead exits and drains via the atexit hook). We simulate the
    # delivery with `schedule(t, InterruptException(); error=true)` so the graceful path is
    # tested deterministically — no interactive signal needed. (The second-interrupt
    # `exit(130)` path is not unit-tested here: it ends the process.)
    @testset "handle_signals: Ctrl-C drives a graceful drain" begin
        ctx = _sdctx()
        spun = Ref(false)
        t = @async (spin(ctx; handle_signals = true); spun[] = true)
        sleep(0.3)                       # let spin install handlers and park on the drain
        @test !istaskdone(t)             # parked — not returned early nor thrown
        schedule(t, InterruptException(); error = true)   # the "Ctrl-C"
        @test Base.timedwait(() -> istaskdone(t), 5.0) === :ok
        @test is_shutdown(ctx)           # the interrupt became a graceful drain
        @test spun[]                     # spin returned normally; no exception escaped
    end

end
