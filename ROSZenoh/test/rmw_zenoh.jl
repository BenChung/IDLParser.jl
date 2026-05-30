@testset "RmwZenoh" begin
    f = RmwZenoh()
    zid_str = "1234567890abcdef1234567890abcdef"
    zid = ZenohId(zid_str)

    # 64 zeros for the default TypeHash() rendering
    zerohash = "RIHS01_" * "0"^64

    @testset "mangle/demangle" begin
        @test mangle(f, "/chatter") == "%chatter"
        @test mangle(f, "std_msgs/msg/String") == "std_msgs%msg%String"
        @test demangle(f, "std_msgs%msg%String") == "std_msgs/msg/String"
        @test demangle(f, mangle(f, "/a/b/c")) == "/a/b/c"
    end

    @testset "QoS default encoding" begin
        @test encode_qos(f, default_qos()) == "::,10:,:,:,,"
        ok, q = decode_qos(f, "::,10:,:,:,,")
        @test ok == false
        @test q == default_qos()
    end

    @testset "QoS reliable + transient_local + depth 5" begin
        q = QosProfile(reliability=:reliable, durability=:transient_local,
                       history=:keep_last, depth=5)
        s = encode_qos(f, q)
        # reliable is default → empty, transient_local → "1", depth 5 non-default → "1,5"
        @test s == ":1:1,5:,:,:,,"
        _, q2 = decode_qos(f, s)
        @test q2 == q
    end

    @testset "QoS best_effort + keep_all" begin
        q = QosProfile(reliability=:best_effort, durability=:volatile,
                       history=:keep_all, depth=0)
        s = encode_qos(f, q)
        @test s == "2::2,:,:,:,,"
        _, q2 = decode_qos(f, s)
        @test q2 == q
    end

    @testset "QoS deadline / lifespan" begin
        q = QosProfile(deadline=Duration(1, 500), lifespan=Duration(0, 250))
        s = encode_qos(f, q)
        # deadline 1,500 ; lifespan sec=0 omitted → ",250" ; liveliness default ",,"
        @test s == "::,10:1,500:,250:,,"
        _, q2 = decode_qos(f, s)
        @test q2 == q
        @test q2.deadline == Duration(1, 500)
        @test q2.lifespan == Duration(0, 250)
    end

    @testset "QoS liveliness manual_by_topic + lease" begin
        q = QosProfile(liveliness=:manual_by_topic, liveliness_lease=Duration(2, 0))
        s = encode_qos(f, q)
        # liveliness kind "3", lease sec=2, nsec=0 omitted → "3,2,"
        @test s == "::,10:,:,:3,2,"
        _, q2 = decode_qos(f, s)
        @test q2 == q
        @test q2.liveliness == :manual_by_topic
        @test q2.liveliness_lease == Duration(2, 0)
    end

    @testset "QoS — tolerates rmw_zenoh omit-on-default forms" begin
        # rmw_zenoh omits the depth at the default (10); we accept it as 10.
        _, q = decode_qos(f, "::,:,:,:,,")
        @test q == default_qos()
        # liveliness kind "1" is Automatic (the RMW default enum value).
        _, q2 = decode_qos(f, "::,10:,:,:1,,")
        @test q2.liveliness == :automatic
        # Older peers that omit the trailing fields entirely.
        _, q3 = decode_qos(f, "::,10")
        @test q3 == default_qos()
    end

    @testset "topic_keyexpr — simple" begin
        node = NodeEntity(0, zid, 0, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher,
                           topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        @test topic_keyexpr(f, e) == "0/chatter/std_msgs/msg/String/$zerohash"
    end

    @testset "topic_keyexpr — preserves internal slashes" begin
        node = NodeEntity(0, zid, 0, "talker", "/", "")
        e = EndpointEntity(id=10, node=node, kind=Service,
                           topic="/talker/get_type_description",
                           type_info=TypeInfo("type_description_interfaces::srv::dds_::GetTypeDescription_",
                                              TypeHash()))
        ke = topic_keyexpr(f, e)
        @test startswith(ke, "0/talker/get_type_description/")
        @test !occursin('%', ke)
    end

    @testset "topic_keyexpr — strips leading and trailing slashes" begin
        node = NodeEntity(0, zid, 0, "test", "/", "")
        e = EndpointEntity(id=4, node=node, kind=Service, topic="/my_service/",
                           type_info=TypeInfo("std_srvs/srv/Trigger", TypeHash()))
        ke = topic_keyexpr(f, e)
        @test startswith(ke, "0/my_service/")
        @test !occursin("//", ke)
    end

    @testset "topic_keyexpr — empty type_info" begin
        node = NodeEntity(0, zid, 0, "test", "/", "")
        e = EndpointEntity(id=5, node=node, kind=Publisher, topic="chatter")
        ke = topic_keyexpr(f, e)
        @test ke == "0/chatter/EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH"
    end

    @testset "topic_keyexpr — type name demangling" begin
        # Type names with % are demangled to / in the topic KE.
        node = NodeEntity(0, zid, 0, "test", "/", "")
        e = EndpointEntity(id=6, node=node, kind=Publisher, topic="chatter",
                           type_info=TypeInfo("std_msgs%msg%String", TypeHash()))
        ke = topic_keyexpr(f, e)
        @test occursin("std_msgs/msg/String", ke)
    end

    @testset "parse_topic_keyexpr — simple" begin
        r = parse_topic_keyexpr(f, "0/chatter/std_msgs/msg/String/$zerohash")
        @test r.domain_id == 0
        @test r.topic == "chatter"
        @test r.type_info.name == "std_msgs/msg/String"
        @test r.type_info.hash == TypeHash()
    end

    @testset "parse_topic_keyexpr — topic with internal slashes" begin
        r = parse_topic_keyexpr(f, "7/talker/get_type_description/std_srvs/srv/Trigger/$zerohash")
        @test r.domain_id == 7
        @test r.topic == "talker/get_type_description"
        @test r.type_info.name == "std_srvs/srv/Trigger"
    end

    @testset "parse_topic_keyexpr — empty type" begin
        r = parse_topic_keyexpr(f, "0/chatter/EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH")
        @test r.domain_id == 0
        @test r.topic == "chatter"
        @test r.type_info === nothing
    end

    @testset "parse_topic_keyexpr — round-trips topic_keyexpr" begin
        node = NodeEntity(3, zid, 0, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher,
                           topic="my/nested/topic",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        r = parse_topic_keyexpr(f, topic_keyexpr(f, e))
        @test r.domain_id == 3
        @test r.topic == "my/nested/topic"
        @test r.type_info == e.type_info
    end

    @testset "parse_topic_keyexpr — rejects bad domain and hash" begin
        @test_throws ArgumentError parse_topic_keyexpr(f, "x/chatter/std_msgs/msg/String/$zerohash")
        @test_throws ArgumentError parse_topic_keyexpr(f, "0/chatter/std_msgs/msg/String/notahash")
    end

    @testset "liveliness_keyexpr — publisher byte-exact" begin
        node = NodeEntity(0, zid, 0, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher,
                           topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        ke = liveliness_keyexpr(f, e)
        expected = "@ros2_lv/0/$zid_str/0/1/MP/%/%/test_node/chatter/std_msgs%msg%String/$zerohash/::,10:,:,:,,"
        @test ke == expected
    end

    @testset "liveliness_keyexpr — endpoint kind codes" begin
        node = NodeEntity(0, zid, 0, "test_node", "/", "")
        for (kind, code) in [(Publisher, "MP"), (Subscription, "MS"),
                             (Service, "SS"), (Client, "SC")]
            e = EndpointEntity(id=1, node=node, kind=kind, topic="t",
                               type_info=TypeInfo("p/msg/T", TypeHash()))
            @test occursin("/$code/", liveliness_keyexpr(f, e))
        end
    end

    @testset "liveliness_keyexpr — multi-segment namespace mangled" begin
        node = NodeEntity(0, zid, 0, "sensor_node", "/robot/sensors", "")
        e = EndpointEntity(id=7, node=node, kind=Publisher, topic="/data/temperature",
                           type_info=TypeInfo("sensor_msgs/msg/Temperature", TypeHash()))
        ke = liveliness_keyexpr(f, e)
        @test occursin("%robot%sensors", ke)
        @test occursin("%data%temperature", ke)
    end

    @testset "liveliness_keyexpr — empty namespace -> %" begin
        node = NodeEntity(0, zid, 0, "test_node", "", "")
        e = EndpointEntity(id=8, node=node, kind=Subscription, topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        ke = liveliness_keyexpr(f, e)
        # Positions: @ros2_lv/0/<zid>/0/8/MS/<enclave>/<ns>/<name>/...
        parts = split(ke, '/')
        @test parts[7] == "%"   # enclave (always %)
        @test parts[8] == "%"   # empty namespace
    end

    @testset "liveliness_keyexpr — root namespace -> %" begin
        node = NodeEntity(0, zid, 0, "test_node", "/", "")
        e = EndpointEntity(id=9, node=node, kind=Publisher, topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        parts = split(liveliness_keyexpr(f, e), '/')
        @test parts[8] == "%"
    end

    @testset "node liveliness byte-exact" begin
        node = NodeEntity(0, zid, 15, "my_node", "/robot/sensors", "")
        ke = liveliness_keyexpr(f, node)
        @test ke == "@ros2_lv/0/$zid_str/15/15/NN/%/%robot%sensors/my_node"
    end

    @testset "missing node errors" begin
        e = EndpointEntity(id=1, node=nothing, kind=Publisher, topic="t")
        @test_throws ArgumentError topic_keyexpr(f, e)
        @test_throws ArgumentError liveliness_keyexpr(f, e)
    end
end
