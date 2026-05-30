# Per-message attachment — rmw_zenoh's per-sample metadata that rides in the
# Zenoh `put`/`reply` attachment, a separate channel from liveliness tokens.
#
# Wire shape (matches hiroz `Attachment`, byte-for-byte):
#   sequence_number :: i64
#   source_timestamp:: i64   (Unix nanoseconds)
#   source_gid      :: [u8; 16]
# = 8 + 8 + 16 = 32 bytes. The gid is the FIXED `[u8; 16]` form (no length
# prefix), so it must stay `NTuple{16,UInt8}` through the serializer — a
# `Vector{UInt8}` would be length-prefixed and break parity.

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
    disp = hex2bytes(z.hex)        # display order: disp[1] is most-significant
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
payload for `put`/`reply`. `seq` and `ts` are coerced to `Int64`; `gid` must be
the fixed-width `NTuple{16,UInt8}` (the parity pivot — see file header).
"""
encode_attachment(seq::Integer, ts::Integer, gid::NTuple{16, UInt8}) =
    Zenoh.serialize((Int64(seq), Int64(ts), gid))

"""
    decode_attachment(sample) -> (seq::Int64, ts::Int64, gid::NTuple{16,UInt8})

Read an attachment back from a received `Sample` (or `ZBytes`). The inverse of
[`encode_attachment`](@ref).
"""
decode_attachment(sample) =
    Zenoh.deserialize(Tuple{Int64, Int64, NTuple{16, UInt8}}, sample)
