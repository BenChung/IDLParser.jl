# Entity types mirroring `hiroz-protocol::entity` and `::qos`. These are plain
# records — sum types aren't needed because every field is always present (the
# Rust `Option` cases stay as `Union{T, Nothing}`, and a few enums become
# `@enum`s or `Symbol`s where Symbol reads more naturally in Julia).

# Liveliness placeholders. RmwZenoh-specific values are duplicated here as
# consts rather than hidden in `rmw_zenoh.jl` because parse_liveliness needs
# to distinguish "placeholder" from "real content" on read.
const EMPTY_PLACEHOLDER  = "%"
const EMPTY_TOPIC_TYPE   = "EMPTY_TOPIC_TYPE"
const EMPTY_TOPIC_HASH   = "EMPTY_TOPIC_HASH"

"""
    ZenohId(hex::AbstractString)

The Zenoh session identifier as it appears in a key expression. Zenoh stores
this as a 16-byte little-endian id and renders it as a lowercase hex string
with leading zero-bytes elided, so length is variable (≤32 chars). We keep
the canonical hex string rather than the raw bytes because the key-expression
output is always the hex form anyway.
"""
struct ZenohId
    hex::String
    function ZenohId(s::AbstractString)
        all(c -> c in "0123456789abcdef", s) ||
            throw(ArgumentError("ZenohId must be lowercase hex: $(repr(s))"))
        isempty(s) && throw(ArgumentError("ZenohId may not be empty"))
        length(s) <= 32 || throw(ArgumentError("ZenohId may not exceed 32 hex chars"))
        new(String(s))
    end
end

Base.string(z::ZenohId) = z.hex
Base.print(io::IO, z::ZenohId) = print(io, z.hex)

"""
    TypeHash(version::UInt8, value::NTuple{32, UInt8})

A ROS2 type hash. `version` is the RIHS version byte (currently `0x01`).
`zero()` returns the Humble-era placeholder (version 1, all zeros) used when
no real hash is known.
"""
struct TypeHash
    version::UInt8
    value::NTuple{32, UInt8}
end

TypeHash() = TypeHash(0x01, ntuple(_ -> 0x00, 32))

const _TYPE_HASH_NOT_SUPPORTED = "TypeHashNotSupported"

"""
    to_rihs_string(h::TypeHash) -> String

`"RIHS01_<64-hex>"` for version 1; otherwise `"RIHS<vv>_<64-hex>"` with the
version as a 2-char lowercase hex.
"""
function to_rihs_string(h::TypeHash)
    hex = bytes2hex(collect(h.value))
    h.version == 0x01 ? string("RIHS01_", hex) :
                        string("RIHS", lpad(string(h.version, base=16), 2, '0'), "_", hex)
end

"""
    type_hash_from_rihs_string(s) -> Union{TypeHash, Nothing}

Inverse of `to_rihs_string`. Accepts the Humble-era `"TypeHashNotSupported"`
sentinel by returning a zero TypeHash. Returns `nothing` on malformed input.
"""
function type_hash_from_rihs_string(s::AbstractString)
    s == _TYPE_HASH_NOT_SUPPORTED && return TypeHash()
    startswith(s, "RIHS") || return nothing
    length(s) == 4 + 2 + 1 + 64 || return nothing
    s[7] == '_' || return nothing
    vstr = SubString(s, 5, 6)
    hexstr = SubString(s, 8, 71)
    version = tryparse(UInt8, vstr; base=16)
    version === nothing && return nothing
    bytes = try
        hex2bytes(hexstr)
    catch
        return nothing
    end
    length(bytes) == 32 || return nothing
    TypeHash(version, NTuple{32, UInt8}(bytes))
end

"""
    TypeInfo(name::String, hash::TypeHash)

A ROS2 type identity: the fully-qualified ROS2 type name (e.g.
`"std_msgs/msg/String"`) together with its RIHS01 hash.
"""
struct TypeInfo
    name::String
    hash::TypeHash
end

TypeInfo(name::AbstractString, hash::TypeHash) = TypeInfo(String(name), hash)

# Endpoint kind. Two-letter codes (`MP`, `MS`, `SS`, `SC`) are the on-wire
# encoding shared by both formatters; `NN` is for nodes (handled separately
# at parse time — we don't put Node in this enum).
@enum EndpointKind Publisher Subscription Service Client

const _KIND_CODE = Dict(
    Publisher    => "MP",
    Subscription => "MS",
    Service      => "SS",
    Client       => "SC",
)
const _CODE_KIND = Dict(v => k for (k, v) in _KIND_CODE)

kind_code(k::EndpointKind) = _KIND_CODE[k]
parse_kind_code(s::AbstractString) = get(_CODE_KIND, String(s), nothing)

"""
    QosProfile(; reliability=:reliable, durability=:volatile,
                 history=:keep_last, depth=10)

DDS-style QoS profile attached to publishers and subscriptions. The fields
mirror ROS2 RMW values; `keyless` is **not** a QoS attribute here — it's a
runtime flag passed alongside the profile to `encode_qos`/`decode_qos`
because rmw_zenoh ignores it but ros2dds prepends it to the encoding.

`history == :keep_all` ignores `depth`.
"""
struct QosProfile
    reliability::Symbol   # :reliable | :best_effort
    durability::Symbol    # :volatile | :transient_local
    history::Symbol       # :keep_last | :keep_all
    depth::Int
end

function QosProfile(; reliability::Symbol=:reliable,
                      durability::Symbol=:volatile,
                      history::Symbol=:keep_last,
                      depth::Integer=10)
    reliability in (:reliable, :best_effort) ||
        throw(ArgumentError("reliability must be :reliable or :best_effort, got :$reliability"))
    durability in (:volatile, :transient_local) ||
        throw(ArgumentError("durability must be :volatile or :transient_local, got :$durability"))
    history in (:keep_last, :keep_all) ||
        throw(ArgumentError("history must be :keep_last or :keep_all, got :$history"))
    QosProfile(reliability, durability, history, Int(depth))
end

# rmw_zenoh and ros2dds both default to Reliable + Volatile + KeepLast(10).
default_qos() = QosProfile()

"""
    NodeEntity(domain_id, z_id, id, name, namespace, enclave)

A ROS2 node. `namespace` and `enclave` may be empty strings (rendered as the
`%` placeholder in liveliness tokens). The enclave field is preserved on the
Julia side but is **not** round-trippable through liveliness encoding —
rmw_zenoh always writes `%` in the enclave slot.
"""
struct NodeEntity
    domain_id::Int
    z_id::ZenohId
    id::Int
    name::String
    namespace::String
    enclave::String
end

NodeEntity(domain_id::Integer, z_id::ZenohId, id::Integer,
           name::AbstractString, namespace::AbstractString, enclave::AbstractString) =
    NodeEntity(Int(domain_id), z_id, Int(id), String(name), String(namespace), String(enclave))

"""
    EndpointEntity(id, node, kind, topic, type_info, qos)

A publisher/subscription/service/client owned by a node (or with `node ===
nothing` for the rare case where an endpoint is observed without its node —
ros2dds liveliness tokens carry no node identity, so parsing always yields
`node === nothing` there).
"""
struct EndpointEntity
    id::Int
    node::Union{NodeEntity, Nothing}
    kind::EndpointKind
    topic::String
    type_info::Union{TypeInfo, Nothing}
    qos::QosProfile
end

EndpointEntity(; id::Integer,
                 node::Union{NodeEntity, Nothing}=nothing,
                 kind::EndpointKind,
                 topic::AbstractString,
                 type_info::Union{TypeInfo, Nothing}=nothing,
                 qos::QosProfile=default_qos()) =
    EndpointEntity(Int(id), node, kind, String(topic), type_info, qos)
