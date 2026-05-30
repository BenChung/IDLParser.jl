@testset "round-trip" begin
    zid = ZenohId("1234567890abcdef1234567890abcdef")

    @testset "RmwZenoh endpoint round-trips for every kind" begin
        f = RmwZenoh()
        node = NodeEntity(0, zid, 0, "test_node", "/my_ns", "")
        for kind in (Publisher, Subscription, Service, Client)
            e = EndpointEntity(id=12, node=node, kind=kind,
                               topic="/topic/name",
                               type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
            ke = liveliness_keyexpr(f, e)
            parsed = parse_liveliness(f, ke)
            @test parsed isa EndpointEntity
            @test parsed.id == e.id
            @test parsed.kind == e.kind
            @test parsed.topic == e.topic
            @test parsed.type_info == e.type_info
            @test parsed.qos == e.qos
            @test parsed.node !== nothing
            @test parsed.node.name == "test_node"
            @test parsed.node.namespace == "/my_ns"
            @test parsed.node.domain_id == 0
            @test parsed.node.id == 0
        end
    end

    @testset "RmwZenoh empty type_info round-trips" begin
        f = RmwZenoh()
        node = NodeEntity(0, zid, 0, "test", "/", "")
        e = EndpointEntity(id=14, node=node, kind=Publisher, topic="test")
        parsed = parse_liveliness(f, liveliness_keyexpr(f, e))
        @test parsed.type_info === nothing
    end

    @testset "RmwZenoh node round-trip" begin
        f = RmwZenoh()
        original = NodeEntity(0, zid, 15, "my_node", "/robot/sensors", "")
        parsed = parse_liveliness(f, liveliness_keyexpr(f, original))
        @test parsed isa NodeEntity
        @test parsed.id == 15
        @test parsed.name == "my_node"
        @test parsed.namespace == "/robot/sensors"
        @test parsed.domain_id == 0
    end

    @testset "RmwZenoh non-default QoS round-trips" begin
        f = RmwZenoh()
        node = NodeEntity(0, zid, 0, "n", "/ns", "")
        for q in (QosProfile(reliability=:best_effort),
                  QosProfile(durability=:transient_local),
                  QosProfile(history=:keep_all, depth=0),
                  QosProfile(reliability=:best_effort, durability=:transient_local,
                             history=:keep_last, depth=7))
            e = EndpointEntity(id=1, node=node, kind=Publisher, topic="x",
                               type_info=TypeInfo("a/msg/B", TypeHash()), qos=q)
            parsed = parse_liveliness(f, liveliness_keyexpr(f, e))
            @test parsed.qos == q
        end
    end

    @testset "Ros2DDS round-trips topic with internal slashes" begin
        f = Ros2DDS()
        node = NodeEntity(0, zid, 1, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="/chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        parsed = parse_liveliness(f, liveliness_keyexpr(f, e))
        @test parsed isa EndpointEntity
        @test parsed.topic == "/chatter"
        @test parsed.kind == Publisher
        @test parsed.type_info !== nothing
        @test parsed.type_info.name == "std_msgs/msg/String"
        # ros2dds carries no node identity in the token
        @test parsed.node === nothing
    end

    @testset "Ros2DDS service round-trip has no QoS suffix" begin
        f = Ros2DDS()
        node = NodeEntity(0, zid, 1, "n", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Service, topic="/svc",
                           type_info=TypeInfo("pkg/srv/X", TypeHash()))
        parsed = parse_liveliness(f, liveliness_keyexpr(f, e))
        @test parsed.kind == Service
        @test parsed.topic == "/svc"
    end
end
