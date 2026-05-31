# Keyexpr + attachment round-trips through ROSZenoh, exercised via the ROSNode
# wrappers (serialization.jl) and the wire-model builders ROSNode uses internally.
# All pure encode/decode — no Zenoh router needed (Zenoh.serialize/deserialize and
# the formatter parsers operate on bytes/strings directly).

using ROSNode: encode_attachment, decode_attachment, type_info, ros_type_name
using ROSZenoh: ZenohId, entity_gid, NodeEntity, EndpointEntity,
                Publisher, Subscription, Service, Client,
                TypeInfo, TypeHash, QosProfile, RmwZenoh,
                liveliness_keyexpr, parse_liveliness,
                topic_keyexpr, parse_topic_keyexpr

const _ZID = ZenohId("1234567890abcdef1234567890abcdef")

@testset "ROSZenoh round-trips" begin
    @testset "attachment triple round-trips (32-byte wire, fixed gid)" begin
        gid = entity_gid(_ZID, 42)
        att = encode_attachment(99, 1_700_000_000_000_000_000, gid)
        @test length(att) == 32                      # 8 + 8 + 16, gid unprefixed
        seq, ts, g = decode_attachment(att)
        @test seq == 99
        @test ts == 1_700_000_000_000_000_000
        @test g === gid
        @test g isa NTuple{16, UInt8}
    end

    @testset "attachment edge values round-trip" begin
        gid = entity_gid(_ZID, 0)
        for (seq, ts) in ((0, 0), (typemax(Int64), typemax(Int64)), (1, -1))
            s, t, g = decode_attachment(encode_attachment(seq, ts, gid))
            @test (s, t, g) == (Int64(seq), Int64(ts), gid)
        end
    end

    @testset "liveliness keyexpr round-trips for every endpoint kind" begin
        f = RmwZenoh()
        node = NodeEntity(0, _ZID, 0, "talker", "/robot", "")
        for kind in (Publisher, Subscription, Service, Client)
            e = EndpointEntity(id=11, node=node, kind=kind, topic="/chatter",
                               type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
            parsed = parse_liveliness(f, liveliness_keyexpr(f, e))
            @test parsed isa EndpointEntity
            @test parsed.kind == kind
            @test parsed.topic == "/chatter"
            @test parsed.type_info == e.type_info
            @test parsed.qos == e.qos
            @test parsed.node.name == "talker"
            @test parsed.node.namespace == "/robot"
        end
    end

    @testset "liveliness carries the full QoS profile" begin
        f = RmwZenoh()
        node = NodeEntity(0, _ZID, 0, "n", "/", "")
        q = QosProfile(reliability=:best_effort, durability=:transient_local,
                       history=:keep_last, depth=7)
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="x",
                           type_info=TypeInfo("a/msg/B", TypeHash()), qos=q)
        @test parse_liveliness(f, liveliness_keyexpr(f, e)).qos == q
    end

    @testset "topic keyexpr round-trips type identity" begin
        f = RmwZenoh()
        node = NodeEntity(7, _ZID, 0, "n", "/", "")
        ti = TypeInfo("sensor_msgs/msg/Imu", TypeHash())
        e = EndpointEntity(id=2, node=node, kind=Publisher, topic="/imu/data",
                           type_info=ti)
        parsed = parse_topic_keyexpr(f, topic_keyexpr(f, e))
        @test parsed.domain_id == 7
        @test occursin("imu", parsed.topic)         # topic survives (slash form is formatter's)
        @test parsed.type_info == ti
    end

    @testset "node token round-trips node identity (no endpoint)" begin
        f = RmwZenoh()
        original = NodeEntity(0, _ZID, 15, "my_node", "/robot/sensors", "")
        parsed = parse_liveliness(f, liveliness_keyexpr(f, original))
        @test parsed isa NodeEntity
        @test parsed.name == "my_node"
        @test parsed.namespace == "/robot/sensors"
        @test parsed.id == 15
    end

    @testset "ros_type_name derives the qualified name from module nesting" begin
        # A hand-defined type at top level has no package context → bare name.
        @test ros_type_name(Int) == "Int64"
        # type_info pairs the name with the Humble-zero placeholder hash today.
        ti = type_info(Int)
        @test ti isa TypeInfo
        @test ti.hash == TypeHash()
    end
end
