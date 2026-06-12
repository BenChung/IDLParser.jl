# Opt-in live verification — not run by `runtests.jl` (which is hermetic). Needs a
# reachable Zenoh router. Start one on an isolated port (so it can't disturb a real
# deployment on the default 7447), then run this against it:
#
#   zenohd -l tcp/127.0.0.1:7455 --no-multicast-scouting &
#   ROSREACH_TEST_ROUTER=tcp/127.0.0.1:7455 julia --project=ROSReach test/live_smoke.jl
#   kill %1   # stop only the router you started
#
# It declares a real rmw_zenoh publisher liveliness token on session A and asserts
# a ROSReach index on session B (both via the router) discovers it, retains A's
# z_id (the join key), sees the router in its transport view, and diagnoses reach.
using Test
using Zenoh, ROSZenoh, ROSReach
using ROSZenoh: RmwZenoh, NodeEntity, EndpointEntity, ZenohId, TypeInfo, TypeHash,
                liveliness_keyexpr

const ROUTER = get(ENV, "ROSREACH_TEST_ROUTER", "tcp/127.0.0.1:7455")
const RIHS = TypeHash(0x01, ntuple(i -> UInt8(i), 32))

mkcfg() = begin
    c = Zenoh.Config()
    c["connect/endpoints"] = [ROUTER]
    c["scouting/multicast/enabled"] = false
    c
end

@testset "ROSReach live (router=$ROUTER)" begin
    sA = Base.open(mkcfg())
    sB = Base.open(mkcfg())
    sleep(1.5)
    zidA = ROSReach._render_zid(Zenoh.to_le_bytes(Zenoh.zid(sA)))

    node = NodeEntity(0, ZenohId(zidA), 1, "talker", "/", "")
    ep = EndpointEntity(; id=2, node=node, kind=ROSZenoh.Publisher, topic="/chatter",
                        type_info=TypeInfo("std_msgs/msg/String", RIHS))
    tok = Zenoh.LivelinessToken(sA, Zenoh.Keyexpr(liveliness_keyexpr(RmwZenoh(), ep)))
    idx = ROSReach.open_index(sB; history=true)

    try
        found = nothing
        for _ in 1:60
            eps = ROSReach.snapshot_endpoints(idx)
            i = findfirst(e -> occursin("chatter", e.topic), eps)
            i === nothing || (found = eps[i]; break)
            sleep(0.2)
        end
        @test found !== nothing
        if found !== nothing
            @test found.z_id == zidA            # the join key survived the wire
            @test found.is_local == false
            @test found.type.name == "std_msgs/msg/String"
        end
        @test any(n -> n.name == "talker", ROSReach.node_names(idx))

        t = ROSReach.transport_topology(sB)
        @test t.self == ROSReach.local_zid(sB)
        @test !isempty(t.routers)               # the router we connected through

        r = ROSReach.diagnose(idx, sB, "/talker")
        @test r.reachable === true
        @test any(l -> l.name === :graph && l.ok === true, r.layers)

        rd = ROSReach.diagnose(idx, sB, "tcp/127.0.0.1:1"; active=true, timeout_ms=600)
        @test rd.reachable === false
        @test any(l -> l.name === :tcp && l.ok === false, rd.layers)
    finally
        close(idx); close(tok); close(sA); close(sB)
    end
end
