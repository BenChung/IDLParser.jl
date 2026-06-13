using Test
using ROSReach
using ROSReach: _render_zid, _classify, _locator_hostport, _type_compat, _endpoint,
                _is_zero_hash
import ROSZenoh
import Zenoh
using ROSZenoh: NodeEntity, EndpointEntity, ZenohId, TypeInfo, TypeHash, QosProfile

const RIHS_A = TypeHash(0x01, ntuple(i -> UInt8(i), 32))
const RIHS_B = TypeHash(0x01, ntuple(i -> UInt8(i + 1), 32))
const RIHS_ZERO = TypeHash()   # all-zero placeholder

# Build an Endpoint directly (no session needed) for query/edge logic tests.
mkep(; zid="aaaa", node="n", ns="/", kind=Publisher, topic="/t", type=nothing,
     qos=QosProfile(), is_local=false) =
    ROSReach.Endpoint(zid, 0, 1, 1, node, ns, kind, topic, type, qos, is_local)

@testset "ROSReach" begin

    @testset "z_id renderer" begin
        # be (reversed le) = [0x00×14, 0x12, 0x34] ⇒ leading zeros elided ⇒ "1234".
        le = (0x34, 0x12, ntuple(_ -> 0x00, 14)...)
        @test _render_zid(le) == "1234"
        @test _render_zid(ntuple(_ -> 0x00, 16)) == "0"
        # A full-length id keeps all bytes.
        full = ntuple(i -> UInt8(i), 16)
        @test length(_render_zid(full)) == 32
    end

    @testset "endpoint join key + is_local" begin
        node = NodeEntity(0, ZenohId("abcd"), 7, "talker", "/", "")
        e = EndpointEntity(; id=9, node=node, kind=ROSZenoh.Publisher,
                           topic="/chatter", type_info=TypeInfo("std_msgs/msg/String", RIHS_A))
        ep_remote = _endpoint(e, "ffff")
        @test ep_remote.z_id == "abcd"
        @test ep_remote.node_id == 7
        @test ep_remote.entity_id == 9
        @test ep_remote.is_local == false
        ep_local = _endpoint(e, "abcd")
        @test ep_local.is_local == true
        # ros2dds (node === nothing): no join key, not local.
        e2 = EndpointEntity(; id=1, node=nothing, kind=ROSZenoh.Subscription, topic="/x")
        ep_dds = _endpoint(e2, "abcd")
        @test ep_dds.z_id == "" && ep_dds.is_local == false
    end

    @testset "target classification" begin
        @test _classify("/talker") === :node
        @test _classify("talker") === :node
        @test _classify("tcp/192.168.1.5:7447") === :locator
        @test _classify("udp/239.0.0.1:7447") === :locator
        @test _classify("abcd1234ef") === :zid
        @test _classify("deadbeef") === :zid
    end

    @testset "locator host:port parse" begin
        @test _locator_hostport("tcp/1.2.3.4:7447") == ("1.2.3.4", 7447)
        @test _locator_hostport("tcp/localhost:7447#iface=lo") == ("localhost", 7447)
        @test _locator_hostport("tcp/[::1]:7447") == ("::1", 7447)
        @test _locator_hostport("garbage") === nothing
    end

    @testset "type compatibility flags" begin
        @test _is_zero_hash(RIHS_ZERO)
        @test !_is_zero_hash(RIHS_A)
        pa = mkep(type=TypeInfo("T", RIHS_A))
        sa = mkep(kind=Subscription, type=TypeInfo("T", RIHS_A))
        @test _type_compat(pa, sa) == (true, true)            # match, verified
        sb = mkep(kind=Subscription, type=TypeInfo("T", RIHS_B))
        @test _type_compat(pa, sb) == (false, true)           # mismatch, verified
        # missing type ⇒ can't refute, not verified
        s_none = mkep(kind=Subscription, type=nothing)
        @test _type_compat(pa, s_none) == (true, false)
        # zero-hash placeholder ⇒ comparison untrustworthy
        pz = mkep(type=TypeInfo("T", RIHS_ZERO))
        sz = mkep(kind=Subscription, type=TypeInfo("T", RIHS_ZERO))
        @test _type_compat(pz, sz) == (true, false)
    end

    @testset "wiring warnings (near-miss names)" begin
        # Classic `~/` slip: @hears subscribes /foo, the publisher resolved ~/foo → /talker/foo.
        eps = [mkep(node="talker", topic="/talker/foo", kind=Publisher),
               mkep(node="listener", topic="/foo", kind=Subscription)]
        w = ROSReach._wiring_warnings(eps)
        @test length(w) == 1
        @test w[1].consumer.topic == "/foo" && w[1].producer.topic == "/talker/foo"
        @test w[1].consumer.kind === Subscription && w[1].producer.kind === Publisher
        @test occursin("namespaced form", w[1].note)
        # The service/client dual: a client on /svc with no server, server on /node/svc.
        ws = ROSReach._wiring_warnings(
            [mkep(node="server", topic="/node/svc", kind=Service),
             mkep(node="caller", topic="/svc", kind=Client)])
        @test length(ws) == 1
        @test ws[1].consumer.kind === Client && ws[1].producer.kind === Service
        # Connected on the exact name ⇒ silent, even with a look-alike pub also present.
        @test isempty(ROSReach._wiring_warnings(
            [mkep(node="t", topic="/chatter", kind=Publisher),
             mkep(node="t", topic="/x/chatter", kind=Publisher),
             mkep(node="l", topic="/chatter", kind=Subscription)]))
        # A dangling sub with no look-alike ⇒ silent (not a generic "no publisher" report).
        @test isempty(ROSReach._wiring_warnings(
            [mkep(node="t", topic="/other", kind=Publisher),
             mkep(node="l", topic="/bar", kind=Subscription)]))
        # Different basename ⇒ not confusable, silent.
        @test isempty(ROSReach._wiring_warnings(
            [mkep(node="t", topic="/foo_raw", kind=Publisher),
             mkep(node="l", topic="/foo", kind=Subscription)]))
        # A sub and a client never cross-pair (different channels), even same basename.
        @test isempty(ROSReach._wiring_warnings(
            [mkep(node="t", topic="/a/foo", kind=Service),
             mkep(node="l", topic="/foo", kind=Subscription)]))
    end

    # The rest exercise the real liveliness/transport path; they need a Zenoh
    # session, which opens fine offline (peer mode, no router required).
    local s
    session_ok = try
        s = Base.open(Zenoh.Config())
        true
    catch err
        @warn "could not open a Zenoh session; skipping session-backed tests" exception=err
        false
    end

    if session_ok
        idx = open_index(s; history=false)
        try
            # Inject endpoints directly into the index to test the query layer
            # without depending on live discovery timing.
            p = mkep(zid="aaaa", node="talker", topic="/chatter", kind=Publisher,
                     type=TypeInfo("std_msgs/msg/String", RIHS_A))
            sub = mkep(zid="bbbb", node="listener", topic="/chatter", kind=Subscription,
                       type=TypeInfo("std_msgs/msg/String", RIHS_A))
            bad = mkep(zid="cccc", node="weird", topic="/chatter", kind=Subscription,
                       type=TypeInfo("std_msgs/msg/Int32", RIHS_B))
            @lock idx.lock begin
                idx.endpoints["k1"] = p
                idx.endpoints["k2"] = sub
                idx.endpoints["k3"] = bad
            end

            @testset "queries" begin
                @test count_publishers(idx, "/chatter") == 1
                @test count_subscribers(idx, "/chatter") == 2
                @test Set(n.name for n in node_names(idx)) == Set(["talker", "listener", "weird"])
                @test session_of(idx, "talker") == "aaaa"
                @test length(endpoints_on_session(idx, "aaaa")) == 1
                tnt = topic_names_and_types(idx)
                @test length(tnt["/chatter"]) == 2   # String + Int32 ⇒ mismatch on the wire
            end

            @testset "topic_edges + honesty flags" begin
                edges = topic_edges(idx; topic="/chatter")
                @test length(edges) == 2              # 1 pub × 2 subs
                good = first(e for e in edges if e.sub.node_name == "listener")
                @test good.type_ok && good.type_verified
                bad_edge = first(e for e in edges if e.sub.node_name == "weird")
                @test !bad_edge.type_ok && bad_edge.type_verified
            end

            @testset "transport view" begin
                t = transport_topology(s)
                @test t.self == local_zid(s)
                @test t.mode in (:router, :peer, :unknown)
                @test local_zid(idx) == local_zid(s)
            end

            @testset "reach diagnosis" begin
                # Unknown node ⇒ resolve fails, definitively unreachable.
                r1 = diagnose(idx, s, "/nope"; active=false)
                @test r1.reachable === false
                @test first(r1.layers).name === :resolve && first(r1.layers).ok === false
                @test !isempty(r1.caveats)
                # A known session, passive: resolves + present in graph.
                r2 = diagnose(idx, s, "aaaa"; active=false, timeout_ms=600)
                @test r2.resolved_zid == "aaaa"
                @test any(l -> l.name === :graph && l.ok === true, r2.layers)
                # Active probe to a closed port ⇒ TCP layer fails.
                r3 = diagnose(idx, s, "tcp/127.0.0.1:1"; active=true, timeout_ms=500)
                @test any(l -> l.name === :tcp && l.ok === false, r3.layers)
                @test r3.reachable === false
            end

            @testset "rendering smoke" begin
                io = IOBuffer()
                graph_report(io, idx, s)
                out = String(take!(io))
                @test occursin("ROS graph", out) && occursin("/chatter", out)
                io2 = IOBuffer()
                to_dot(io2, idx, s)
                dot = String(take!(io2))
                @test occursin("digraph", dot) && occursin("/chatter", dot)
            end
        finally
            close(idx)
            close(s)
        end
    end

    @testset "isolation: no ROSNode dependency" begin
        proj = read(joinpath(pkgdir(ROSReach), "Project.toml"), String)
        deps = split(proj, "[deps]")[2]
        deps = split(deps, "[")[1]      # only the [deps] block
        @test !occursin("ROSNode", deps)
    end
end

# Opt-in live verification over a real router (see test/live_smoke.jl). Runs only
# when ROSREACH_TEST_ROUTER is set, so the default suite stays hermetic.
if get(ENV, "ROSREACH_TEST_ROUTER", "") != ""
    include("live_smoke.jl")
end
