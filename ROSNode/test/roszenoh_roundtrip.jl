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
    @testset "attachment triple round-trips (33-byte wire, fixed gid)" begin
        gid = entity_gid(_ZID, 42)
        att = encode_attachment(99, 1_700_000_000_000_000_000, gid)
        @test length(att) == 33                      # 8 + 8 + VarInt(16) + 16 (length-prefixed gid)
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

    @testset "topic + liveliness keyexpr use the DDS type name (rmw_zenoh wire form)" begin
        f = RmwZenoh()
        node = NodeEntity(0, _ZID, 0, "talker", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="/chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        ke = topic_keyexpr(f, e)
        # The type segment is the DDS name `std_msgs::msg::dds_::String_` — a SINGLE
        # component — NOT the ROS `std_msgs/msg/String` (3 components). A real peer's
        # keyexpr won't match unless we emit this form.
        @test occursin("std_msgs::msg::dds_::String_", ke)
        @test !occursin("std_msgs/msg/String", ke)
        @test length(split(ke, '/')) == 4                   # domain / chatter / type / hash
        @test occursin("std_msgs::msg::dds_::String_", liveliness_keyexpr(f, e))

        # Parse a REAL rmw_zenoh data keyexpr (what a C++/Rust talker emits) back to our
        # ROS-form TypeInfo — the exact shape the dynamic-sub path failed on.
        real = "0/chatter/std_msgs::msg::dds_::String_/" *
               "RIHS01_df668c740482bbd48fb39d76a70dfd4bd59db1288021743503259e948f6b1a18"
        p = parse_topic_keyexpr(f, real)
        @test p.domain_id == 0
        @test p.topic == "chatter"
        @test p.type_info.name == "std_msgs/msg/String"     # DDS -> ROS conversion

        # Multi-level topic: the slash-preserving topic still anchors on the fixed ends.
        p2 = parse_topic_keyexpr(f, "5/imu/data/sensor_msgs::msg::dds_::Imu_/" *
                                     "RIHS01_" * repeat("ab", 32))
        @test p2.domain_id == 5
        @test p2.topic == "imu/data"
        @test p2.type_info.name == "sensor_msgs/msg/Imu"
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
