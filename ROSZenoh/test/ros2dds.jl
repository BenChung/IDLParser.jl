@testset "Ros2DDS" begin
    f = Ros2DDS()
    zid_str = "1234567890abcdef1234567890abcdef"
    zid = ZenohId(zid_str)

    @testset "mangle/demangle" begin
        @test mangle(f, "/chatter") == "§chatter"
        @test mangle(f, "std_msgs/msg/String") == "std_msgs§msg§String"
        @test demangle(f, "std_msgs§msg§String") == "std_msgs/msg/String"
    end

    @testset "QoS default keyless=false" begin
        s = encode_qos(f, default_qos(); keyless=false)
        @test s == "K:1:0:0,10"
        ok, q = decode_qos(f, s)
        @test ok == false
        @test q == default_qos()
    end

    @testset "QoS default keyless=true" begin
        s = encode_qos(f, default_qos(); keyless=true)
        @test s == ":1:0:0,10"
        ok, q = decode_qos(f, s)
        @test ok == true
        @test q == default_qos()
    end

    @testset "QoS reliable + transient_local" begin
        q = QosProfile(reliability=:reliable, durability=:transient_local,
                       history=:keep_last, depth=10)
        @test encode_qos(f, q) == "K:1:1:0,10"
        _, q2 = decode_qos(f, "K:1:1:0,10")
        @test q2 == q
    end

    @testset "QoS best_effort + keep_all" begin
        q = QosProfile(reliability=:best_effort, durability=:volatile,
                       history=:keep_all, depth=0)
        @test encode_qos(f, q) == "K:0:0:1,0"
    end

    @testset "topic_keyexpr — simple" begin
        node = NodeEntity(0, zid, 1, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        @test topic_keyexpr(f, e) == "chatter"
    end

    @testset "topic_keyexpr — strips leading slash" begin
        node = NodeEntity(0, zid, 1, "test", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="/chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        @test topic_keyexpr(f, e) == "chatter"
    end

    @testset "liveliness_keyexpr — publisher byte-exact" begin
        node = NodeEntity(0, zid, 1, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        ke = liveliness_keyexpr(f, e)
        expected = "@/$zid_str/@ros2_lv/MP/chatter/std_msgs§msg§String/K:1:0:0,10"
        @test ke == expected
    end

    @testset "liveliness_keyexpr — service has no QoS" begin
        node = NodeEntity(0, zid, 1, "test_node", "/", "")
        e = EndpointEntity(id=1, node=node, kind=Service, topic="add_two_ints",
                           type_info=TypeInfo("example_interfaces/srv/AddTwoInts", TypeHash()))
        ke = liveliness_keyexpr(f, e)
        @test ke == "@/$zid_str/@ros2_lv/SS/add_two_ints/example_interfaces§srv§AddTwoInts"
        # No trailing ':' from a QoS string
        @test !occursin(':', split(ke, '/')[end])
    end

    @testset "liveliness_keyexpr — kind codes" begin
        node = NodeEntity(0, zid, 1, "test_node", "/", "")
        for (kind, code) in [(Publisher, "MP"), (Subscription, "MS"),
                             (Service, "SS"), (Client, "SC")]
            e = EndpointEntity(id=1, node=node, kind=kind, topic="t",
                               type_info=TypeInfo("p/msg/T", TypeHash()))
            @test occursin("/$code/", liveliness_keyexpr(f, e))
        end
    end

    @testset "no node liveliness without a caller-supplied token" begin
        node = NodeEntity(0, zid, 1, "n", "/", "")
        @test_throws ArgumentError liveliness_keyexpr(f, node)
    end

    @testset "node liveliness — caller supplies the token" begin
        node = NodeEntity(0, zid, 1, "n", "/", "")
        custom = "@/$zid_str/my_higher_level/NN/n"
        @test liveliness_keyexpr(f, node; token=custom) == custom
    end

    @testset "parse_liveliness — node kwarg pass-through" begin
        node = NodeEntity(0, zid, 7, "test_node", "/ns", "")
        e = EndpointEntity(id=1, node=node, kind=Publisher, topic="/chatter",
                           type_info=TypeInfo("std_msgs/msg/String", TypeHash()))
        ke = liveliness_keyexpr(f, e)

        # Without `node=`, parse leaves it nothing.
        @test parse_liveliness(f, ke).node === nothing

        # With `node=`, the parsed entity carries the caller-supplied node.
        parsed = parse_liveliness(f, ke; node=node)
        @test parsed.node === node
        @test parsed.topic == "/chatter"
        @test parsed.kind == Publisher
    end
end
