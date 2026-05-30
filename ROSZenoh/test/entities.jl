@testset "entities" begin
    @testset "ZenohId" begin
        @test string(ZenohId("1234567890abcdef1234567890abcdef")) == "1234567890abcdef1234567890abcdef"
        @test string(ZenohId("9aed1ea85b72095f6dbc9ee90dabd56")) == "9aed1ea85b72095f6dbc9ee90dabd56"
        @test_throws ArgumentError ZenohId("DEADBEEF")  # uppercase rejected
        @test_throws ArgumentError ZenohId("")
        @test_throws ArgumentError ZenohId("z" * "0"^31)  # non-hex char
        @test_throws ArgumentError ZenohId("0"^33)  # too long
    end

    @testset "TypeHash" begin
        z = TypeHash()
        @test z.version == 0x01
        @test all(==(0x00), z.value)
        @test to_rihs_string(z) ==
              "RIHS01_0000000000000000000000000000000000000000000000000000000000000000"

        h = TypeHash(0x01, ntuple(_ -> 0xab, 32))
        @test to_rihs_string(h) == "RIHS01_" * repeat("ab", 32)

        # non-version-1
        h2 = TypeHash(0x02, ntuple(_ -> 0x00, 32))
        @test startswith(to_rihs_string(h2), "RIHS02_")

        # parse round-trip
        @test type_hash_from_rihs_string(to_rihs_string(h)) == h
        @test type_hash_from_rihs_string("TypeHashNotSupported") == z
        @test type_hash_from_rihs_string("RIHS01_short") === nothing
        @test type_hash_from_rihs_string("not a hash") === nothing
    end

    @testset "QosProfile defaults" begin
        q = default_qos()
        @test q.reliability == :reliable
        @test q.durability == :volatile
        @test q.history == :keep_last
        @test q.depth == 10
        @test_throws ArgumentError QosProfile(reliability=:bogus)
    end

    @testset "EndpointKind codes" begin
        @test ROSZenoh.kind_code(Publisher) == "MP"
        @test ROSZenoh.kind_code(Subscription) == "MS"
        @test ROSZenoh.kind_code(Service) == "SS"
        @test ROSZenoh.kind_code(Client) == "SC"
        @test ROSZenoh.parse_kind_code("MP") == Publisher
        @test ROSZenoh.parse_kind_code("XX") === nothing
    end
end
