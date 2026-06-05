# §4 concurrency — the persistent worker-pool consumer. Exercises the paths the
# rest of the suite (Serial + owned, the defaults) never hits: Parallel(n) with
# both view and owned decode, and Parallel(Inf). Each verifies every published
# message is delivered exactly once (order not guaranteed under Parallel).

using ROSNode
using ROSNode: Context, Node, Publisher, Subscription, publish, WireKeyValue,
               Serial, Parallel
using Test

const _PEP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
             get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_pctx(f::Function) = Context(f; peers = [_PEP], localhost_only = true)

# Run a round-trip under `concurrency`/`view`: publish 1..N, collect the handler's
# `value` field (thread-safe Channel — handlers run on multiple OS threads), and
# return the sorted integers received.
function _roundtrip(topic, concurrency, view; N = 30)
    _pctx() do ctx
        node = Node(ctx, "par_" * replace(topic, "/" => "_"))
        got = Channel{Int}(4N)                 # never blocks: ≤ N puts
        sub = Subscription(node, topic, WireKeyValue;
                           concurrency = concurrency, view = view) do m
            put!(got, parse(Int, String(m.value)))   # String(...) materializes the view
        end
        pub = Publisher(node, topic, WireKeyValue)
        sleep(0.4)                              # let the route match
        for i in 1:N
            publish(pub, WireKeyValue(key = "k", value = string(i)))
            sleep(0.002)                        # keep workers ahead of the ring (no drops)
        end
        recv = Int[]
        timedwait(5.0; pollint = 0.02) do
            while isready(got); push!(recv, take!(got)); end
            length(recv) >= N
        end
        return sort(recv)
    end
end

@testset "Parallel dispatch (worker pool)" begin
    N = 30
    @testset "Parallel(2) view" begin
        @test _roundtrip("/par/v", Parallel(2), true; N = N) == collect(1:N)
    end
    @testset "Parallel(2) owned" begin
        @test _roundtrip("/par/o", Parallel(2), false; N = N) == collect(1:N)
    end
    @testset "Parallel(Inf) view" begin
        # Unbounded spawn path; just confirm every message lands (order/dup-safe).
        @test _roundtrip("/par/inf", Parallel(Inf), true; N = N) == collect(1:N)
    end
    @testset "Serial still ordered after refactor" begin
        # Serial = 1 sticky worker: delivery order must match publish order.
        _pctx() do ctx
            node = Node(ctx, "par_serial")
            got = Channel{Int}(4N)
            sub = Subscription(node, "/par/serial", WireKeyValue;
                               concurrency = Serial(), view = false) do m
                put!(got, parse(Int, String(m.value)))
            end
            pub = Publisher(node, "/par/serial", WireKeyValue)
            sleep(0.4)
            for i in 1:N
                publish(pub, WireKeyValue(key = "k", value = string(i)))
                sleep(0.002)
            end
            recv = Int[]
            timedwait(5.0; pollint = 0.02) do
                while isready(got); push!(recv, take!(got)); end
                length(recv) >= N
            end
            @test recv == collect(1:N)          # ordered, no sort
        end
    end
end
