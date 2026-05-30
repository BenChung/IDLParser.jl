using ROSZenoh: to_le_bytes, entity_gid
using SHA: sha256

@testset "Attachment" begin
    zid = ZenohId("1234567890abcdef1234567890abcdef")

    @testset "to_le_bytes — full 16-byte zid reverses display order" begin
        disp = hex2bytes("1234567890abcdef1234567890abcdef")
        @test collect(to_le_bytes(zid)) == reverse(disp)
    end

    @testset "to_le_bytes — short zid reverses and tail-pads with zeros" begin
        # "0102" is the big-endian display; LE storage is [0x02,0x01,0,…].
        le = to_le_bytes(ZenohId("0102"))
        @test le[1] == 0x02
        @test le[2] == 0x01
        @test all(==(0x00), le[3:16])
    end

    @testset "entity_gid — matches sha256(zid_le ‖ id_le)[1:16]" begin
        id = 7
        id_le = UInt8[(UInt64(id) >> (8 * (i - 1))) & 0xff for i in 1:8]
        expected = NTuple{16,UInt8}(@view sha256(vcat(collect(to_le_bytes(zid)), id_le))[1:16])
        @test entity_gid(zid, id) == expected
    end

    @testset "entity_gid — raw-LE form agrees with ZenohId form" begin
        # The NTuple form is what ROSNode feeds from a live session zid.
        @test entity_gid(to_le_bytes(zid), 9) == entity_gid(zid, 9)
    end

    @testset "entity_gid — deterministic, 16 bytes, id-sensitive" begin
        g = entity_gid(zid, 0)
        @test g isa NTuple{16,UInt8}
        @test entity_gid(zid, 0) == g          # deterministic
        @test entity_gid(zid, 1) != g          # entity id changes the gid
        @test entity_gid(ZenohId("abcd"), 0) != g  # zid changes the gid
    end

    @testset "entity_gid — entity/node convenience methods" begin
        node = NodeEntity(0, zid, 3, "n", "/", "")
        e = EndpointEntity(id=5, node=node, kind=Publisher, topic="t")
        @test entity_gid(node) == entity_gid(zid, 3)
        @test entity_gid(e) == entity_gid(zid, 5)
        orphan = EndpointEntity(id=5, kind=Publisher, topic="t")
        @test_throws ArgumentError entity_gid(orphan)
    end

    @testset "encode/decode round-trip through the serializer" begin
        gid = entity_gid(zid, 42)
        payload = encode_attachment(99, 1_700_000_000_000_000_000, gid)
        seq, ts, g = decode_attachment(payload)
        @test seq == 99
        @test ts == 1_700_000_000_000_000_000
        @test g == gid
    end

    @testset "encode_attachment — 32-byte wire size (8+8+16, gid unprefixed)" begin
        payload = encode_attachment(0, 0, entity_gid(zid, 0))
        @test length(payload) == 32
    end
end
