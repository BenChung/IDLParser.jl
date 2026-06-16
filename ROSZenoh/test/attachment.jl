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

    @testset "to_le_bytes — odd-length hex (elided high nibble)" begin
        # Zenoh elides a leading zero nibble, so the high byte can render as one
        # char: "102" is big-endian 0x01,0x02 → LE [0x02,0x01,0,…].
        le = to_le_bytes(ZenohId("102"))
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

    @testset "encode_attachment — 33-byte wire size (8 + 8 + VarInt(16) + 16)" begin
        # The gid is a length-prefixed `[u8;16]` (zenoh-ext fixed-array form), so
        # the payload is 33 bytes: 8 (seq) + 8 (ts) + 1 (VarInt 16) + 16 (gid).
        payload = encode_attachment(0, 0, entity_gid(zid, 0))
        bytes = collect(ROSZenoh.Zenoh.as_memory(payload, UInt8))
        @test length(bytes) == 33
        @test bytes[17] == 0x10   # VarInt(16) prefix precedes the gid bytes
    end

    @testset "encode_attachment — byte-identical to the generic serializer" begin
        # The hand-rolled single-pass encoder must match `Zenoh.serialize` of the
        # `(i64, i64, Vector{UInt8})` tuple byte-for-byte, or a C++/Rust rmw_zenoh
        # peer mis-reads the attachment. Cover the int64 and gid edge cases.
        refbytes(s, t, g) = collect(ROSZenoh.Zenoh.as_memory(
            ROSZenoh.Zenoh.serialize((Int64(s), Int64(t), collect(g))), UInt8))
        ourbytes(s, t, g) = collect(ROSZenoh.Zenoh.as_memory(
            encode_attachment(s, t, g), UInt8))

        gid0 = ntuple(_ -> 0x00, 16)
        gidF = ntuple(_ -> 0xff, 16)
        gidI = ntuple(i -> UInt8(i), 16)
        seqs = Int64[0, 1, -1, 42, 256, typemax(Int64), typemin(Int64),
                     0x0102030405060708 % Int64]
        tss  = Int64[0, 1, -1, 1_700_000_000_000_000_000, typemax(Int64), typemin(Int64)]
        for s in seqs, t in tss, g in (gid0, gidF, gidI)
            @test ourbytes(s, t, g) == refbytes(s, t, g)
        end
    end

    @testset "encode_attachment — round-trips through decode_attachment" begin
        gidI = ntuple(i -> UInt8(i), 16)
        for s in (Int64(0), Int64(-1), typemax(Int64), typemin(Int64)),
            t in (Int64(0), typemax(Int64), Int64(1_700_000_000_000_000_000)),
            g in (ntuple(_ -> 0x00, 16), ntuple(_ -> 0xff, 16), gidI)
            @test decode_attachment(encode_attachment(s, t, g)) == (s, t, g)
        end
    end
end
