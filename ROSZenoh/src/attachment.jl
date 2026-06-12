# Per-message attachment — rmw_zenoh's per-sample metadata that rides in the
# Zenoh `put`/`reply` attachment, a separate channel from liveliness tokens.
#
# Wire shape (matches hiroz `Attachment` / zenoh-cpp rmw_zenoh, byte-for-byte):
#   sequence_number :: i64       (8 bytes LE)
#   source_timestamp:: i64       (8 bytes LE, Unix nanoseconds)
#   source_gid      :: [u8; 16]  (VarInt(16) length prefix + 16 bytes)
# = 8 + 8 + 1 + 16 = 33 bytes. zenoh-ext's `Serialize for [T; N]` routes through
# the same length-prefixed slice path as `Vec<u8>` (`serialize_slice` writes
# `VarInt(len)` first), and its `Deserialize` rejects with `ZDeserializeError`
# unless that prefix == N. So the gid MUST carry the VarInt(16) prefix — we
# serialize it as a `Vector{UInt8}` (the length-prefixed path) and rebuild the
# `NTuple{16,UInt8}` on decode. An unprefixed 16 raw bytes round-trips only
# Julia↔Julia (symmetric); a Rust/C++ peer reads the first gid byte as the length
# and errors.

# ---- gid derivation -------------------------------------------------------
#
# hiroz: `sha256(z_id.to_le_bytes() ++ entity_id.to_le_bytes())[..16]`, where
# `z_id.to_le_bytes()` is zenoh's full 16-byte little-endian session id and
# `entity_id` is a `usize` (8-byte LE).

"""
    to_le_bytes(z::ZenohId) -> NTuple{16, UInt8}

The full 16-byte little-endian session id, matching `zenoh::ZenohId::to_le_bytes`.
Zenoh's *string* form renders the significant bytes big-endian (most-significant
first) with leading zero bytes elided, so we parse the hex, reverse it back to
little-endian storage order, and zero-pad the unused high bytes (which sit at the
tail of the LE array).
"""
function to_le_bytes(z::ZenohId)
    # Zenoh elides a leading zero *nibble* too, so the high byte may render as a
    # single hex char — left-pad to even length before parsing.
    hex = isodd(length(z.hex)) ? "0" * z.hex : z.hex
    disp = hex2bytes(hex)          # display order: disp[1] is most-significant
    le = zeros(UInt8, 16)
    n = length(disp)
    @inbounds for i in 1:n
        le[i] = disp[n - i + 1]    # reverse to little-endian
    end
    return NTuple{16, UInt8}(le)
end

# usize → 8-byte little-endian, independent of host endianness.
_u64_le(x::Integer) = UInt8[(UInt64(x) >> (8 * (i - 1))) & 0xff for i in 1:8]

"""
    entity_gid(z_id_le::NTuple{16,UInt8}, id::Integer) -> NTuple{16, UInt8}
    entity_gid(z_id::ZenohId, id::Integer) -> NTuple{16, UInt8}

The 16-byte `source_gid` for an entity: `sha256(z_id_le ‖ id_le)[1:16]`, where
`z_id_le` is the 16-byte LE session id and `id_le` the 8-byte LE entity id. Matches
hiroz `endpoint_gid` byte-for-byte.

Prefer the `NTuple` form fed from a live session — `entity_gid(Zenoh.to_le_bytes(
Zenoh.zid(session)), id)` — which uses the raw session bytes directly. The
`ZenohId` form is for the string-only case (e.g. a zid recovered from a liveliness
keyexpr); it reconstructs the LE bytes via [`to_le_bytes`](@ref).
"""
function entity_gid(z_id_le::NTuple{16, UInt8}, id::Integer)
    hash = sha256(vcat(collect(z_id_le), _u64_le(id)))
    return NTuple{16, UInt8}(@view hash[1:16])
end
entity_gid(z_id::ZenohId, id::Integer) = entity_gid(to_le_bytes(z_id), id)

"""
    entity_gid(n::NodeEntity) -> NTuple{16, UInt8}
    entity_gid(e::EndpointEntity) -> NTuple{16, UInt8}

Convenience methods using the entity's own `z_id`/`id`. The endpoint form
requires a node (the `z_id` source); it throws `ArgumentError` otherwise.
"""
entity_gid(n::NodeEntity) = entity_gid(n.z_id, n.id)
function entity_gid(e::EndpointEntity)
    e.node === nothing &&
        throw(ArgumentError("entity_gid requires a node for the zenoh id"))
    return entity_gid(e.node.z_id, e.id)
end

# ---- encode / decode ------------------------------------------------------

"""
    encode_attachment(seq, ts, gid::NTuple{16,UInt8}) -> ZBytes

Serialize the `(sequence_number, source_timestamp, source_gid)` attachment into a
payload for `put`/`reply`. This is rmw_zenoh's per-sample metadata; matching its
wire format byte-for-byte is what lets a Julia peer interop with C++/Rust rmw_zenoh
nodes (see [RMW / middleware vendors](https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Different-Middleware-Vendors.html)).
`seq` and `ts` are coerced to `Int64`; `gid` is the fixed-width `NTuple{16,UInt8}`,
emitted as a length-prefixed `[u8;16]` to match zenoh-ext's fixed-array encoding
(the VarInt(16) prefix — see file header).
"""
encode_attachment(seq::Integer, ts::Integer, gid::NTuple{16, UInt8}) =
    Zenoh.serialize((Int64(seq), Int64(ts), collect(gid)))

"""
    decode_attachment(sample) -> (seq::Int64, ts::Int64, gid::NTuple{16,UInt8})

Read an attachment back from a received `Sample` (its put attachment) or raw
attachment `ZBytes`. The inverse of [`encode_attachment`](@ref); the gid arrives
length-prefixed (`[u8;16]`) and is rebuilt into the fixed-width
`NTuple{16,UInt8}`. Returns `nothing` for a sample that carries no attachment.
"""
# Deserializing a Sample directly reads its PAYLOAD (Zenoh's
# `ZDeserializer(s::Sample) = ZDeserializer(payload(s))`) — pull the put
# attachment explicitly.
function decode_attachment(sample::Zenoh.AbstractSample)
    att = Zenoh.attachment(sample)
    att === nothing && return nothing
    return decode_attachment(att)
end
function decode_attachment(bytes)
    seq, ts, gid = Zenoh.deserialize(Tuple{Int64, Int64, Vector{UInt8}}, bytes)
    length(gid) == 16 ||
        throw(ArgumentError("attachment gid is $(length(gid)) bytes, expected 16"))
    return (seq, ts, NTuple{16, UInt8}(gid))
end
