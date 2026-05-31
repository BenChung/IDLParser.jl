# D4 — transient_local (latched/cached delivery) + durability-aware lifecycle
# re-latch. Live tests against the private router (see runtests.jl): a latched
# publisher caches its last samples and a late-joining subscriber recovers them via
# Zenoh advanced pub/sub (history query). On a managed node the re-latch re-runs that
# query on Inactive→Active so the node never comes up Active missing latched state,
# novelty-gated so an unchanged value isn't replayed.
#
# Standalone (outside the suite): run a router on :7447 (or set ROS_TEST_EP), then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/transient_local.jl

using ROSNode
using ROSNode: Context, Node, Publisher, Subscription, publish, QosProfile,
               LifecycleNode, configure!, activate!, deactivate!, inner_node,
               WireKeyValue
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_tlctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# transient_local QoS (reliable, KeepLast(depth)) — the latched-state profile.
_tl(; depth = 10) = QosProfile(durability = :transient_local, reliability = :reliable, depth = depth)

# Block up to `secs` for the next value on `ch`; `nothing` on timeout (no message).
function _tlrecv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint = 0.02) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

_tlstep(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

@testset "D4 transient_local (Zenoh session)" begin

    # (1) The core interop proof: a value published BEFORE a subscriber exists is
    # delivered to that subscriber on join, via the publisher's cache + the
    # subscriber's history query. No lifecycle involved.
    @testset "latched value reaches a late subscriber" begin
        _tlstep("latched: open ctx + latched publisher")
        _tlctx() do ctx
            node = Node(ctx, "latched")
            pub  = Publisher(node, "/map", WireKeyValue; qos = _tl())
            publish(pub, WireKeyValue(key = "map", value = "v1"))
            sleep(0.5)                                  # let the cache settle
            _tlstep("latched: late subscriber joins")
            got = Channel{Any}(8)
            sub = Subscription(node, "/map", WireKeyValue; qos = _tl()) do m
                put!(got, m)
            end
            m = _tlrecv(got, 5.0)                         # history query recovers it
            @test m isa WireKeyValue
            @test m !== nothing && m.value == "v1"
            close(sub); close(pub)
        end
    end

    # (7) Volatile regression: WITHOUT transient_local, a late subscriber does NOT
    # receive a value published before it joined — but a live publish after it joins
    # still arrives. Confirms volatile routes to the plain (non-advanced) path.
    @testset "volatile does not latch (regression)" begin
        _tlstep("volatile: open ctx + volatile publisher")
        _tlctx() do ctx
            node = Node(ctx, "vol")
            pub  = Publisher(node, "/live", WireKeyValue)          # default = volatile
            publish(pub, WireKeyValue(key = "live", value = "old"))
            sleep(0.5)
            got = Channel{Any}(8)
            sub = Subscription(node, "/live", WireKeyValue) do m
                put!(got, m)
            end
            sleep(0.5)
            @test _tlrecv(got, 0.5) === nothing                      # no latch: stale value not replayed
            _tlstep("volatile: live publish after subscribe")
            publish(pub, WireKeyValue(key = "live", value = "new"))
            m = _tlrecv(got, 5.0)
            @test m isa WireKeyValue && m !== nothing && m.value == "new"
            close(sub); close(pub)
        end
    end

    # (3) The D4 bug fixed: a managed node with a transient_local subscription must
    # come up Active holding latched state, even though the latched sample arrived
    # (and was gate-dropped) while it was Inactive. The re-latch on activate recovers
    # it. Also asserts exactly-once (no double-delivery from the redundant re-query).
    @testset "lifecycle: first activation delivers latched state" begin
        _tlstep("lifecycle-first: latched publisher + managed node (Inactive)")
        _tlctx() do ctx
            pubnode = Node(ctx, "pub3")
            pub = Publisher(pubnode, "/state3", WireKeyValue; qos = _tl())
            publish(pub, WireKeyValue(key = "s", value = "v1"))
            sleep(0.4)

            got = Channel{Any}(8)
            ln  = LifecycleNode(ctx, "managed3")
            configure!(ln)                                   # Unconfigured → Inactive
            Subscription(inner_node(ln), "/state3", WireKeyValue; qos = _tl()) do m
                put!(got, m)
            end
            sleep(0.6)
            @test _tlrecv(got, 0.4) === nothing                # gated while Inactive
            _tlstep("lifecycle-first: activate ⇒ re-latch")
            activate!(ln)                                    # Inactive → Active ⇒ re-latch
            m = _tlrecv(got, 5.0)
            @test m isa WireKeyValue && m !== nothing && m.value == "v1"
            @test _tlrecv(got, 0.5) === nothing                # exactly once (novelty-gated)
            close(ln); close(pub); close(pubnode)
        end
    end

    # (4) & (5) Reactivation across a deactivate cycle: an UNCHANGED latched value is
    # not replayed (novelty gate), but a value UPDATED while inactive IS delivered.
    @testset "lifecycle: reactivation dedups unchanged, delivers updated" begin
        _tlstep("lifecycle-recycle: latched publisher + managed node")
        _tlctx() do ctx
            pubnode = Node(ctx, "pub4")
            pub = Publisher(pubnode, "/state4", WireKeyValue; qos = _tl())
            publish(pub, WireKeyValue(key = "s", value = "v1"))
            sleep(0.4)

            got = Channel{Any}(8)
            ln  = LifecycleNode(ctx, "managed4")
            configure!(ln)
            Subscription(inner_node(ln), "/state4", WireKeyValue; qos = _tl()) do m
                put!(got, m)
            end
            activate!(ln)
            @test (_tlrecv(got, 5.0)).value == "v1"            # first activation delivers
            @test _tlrecv(got, 0.4) === nothing

            _tlstep("lifecycle-recycle: deactivate, reactivate UNCHANGED ⇒ suppressed")
            deactivate!(ln)
            sleep(0.3)
            activate!(ln)
            @test _tlrecv(got, 1.0) === nothing                # unchanged ⇒ no replay

            _tlstep("lifecycle-recycle: update while inactive, reactivate ⇒ delivered")
            deactivate!(ln)
            sleep(0.2)
            publish(pub, WireKeyValue(key = "s", value = "v2"))   # updated while Inactive
            sleep(0.4)
            activate!(ln)
            m = _tlrecv(got, 5.0)
            @test m isa WireKeyValue && m !== nothing && m.value == "v2"  # genuine update fires
            close(ln); close(pub); close(pubnode)
        end
    end

    # (6) Escape hatch: `force_relatch=true` re-delivers on every activation
    # regardless of sequence (for idempotent handlers that rebuild from latched state).
    @testset "lifecycle: force_relatch redelivers unchanged" begin
        _tlstep("lifecycle-force: latched publisher + force_relatch sub")
        _tlctx() do ctx
            pubnode = Node(ctx, "pub6")
            pub = Publisher(pubnode, "/state6", WireKeyValue; qos = _tl())
            publish(pub, WireKeyValue(key = "s", value = "v1"))
            sleep(0.4)

            got = Channel{Any}(8)
            ln  = LifecycleNode(ctx, "managed6")
            configure!(ln)
            Subscription(inner_node(ln), "/state6", WireKeyValue; qos = _tl(),
                         force_relatch = true) do m
                put!(got, m)
            end
            activate!(ln)
            @test (_tlrecv(got, 5.0)).value == "v1"
            _tlstep("lifecycle-force: reactivate ⇒ redelivered despite unchanged")
            deactivate!(ln)
            sleep(0.3)
            activate!(ln)
            m = _tlrecv(got, 5.0)
            @test m isa WireKeyValue && m !== nothing && m.value == "v1"   # forced replay
            close(ln); close(pub); close(pubnode)
        end
    end

end
