"""
Key-expression and liveliness-token formatting for the two Zenoh-based ROS 2
middleware conventions: `RmwZenoh` (the native `rmw_zenoh_cpp` RMW) and
`Ros2DDS` (the `zenoh-plugin-ros2dds` DDS bridge). Each formatter is a
zero-sized singleton; the API is a set of generic functions specialized on the
formatter type.

See the ROS 2 middleware-vendor concept:
https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Different-Middleware-Vendors.html
"""
abstract type KeyExprFormat end

"""
    RmwZenoh()

Default formatter — compatible with `rmw_zenoh_cpp` (the ROS2 native Zenoh
RMW). Topic key expressions preserve internal slashes; liveliness tokens
mangle `/` → `%`. QoS encoding uses the rmw_zenoh six-field colon scheme.
"""
struct RmwZenoh <: KeyExprFormat end

"""
    Ros2DDS()

Compatible with `zenoh-plugin-ros2dds`. Topic key expressions are bare topic
names (no type/hash); liveliness uses `@/<zid>/@ros2_lv/...` and the section
sign `§` (U+00A7) as the slash-escape. QoS encoding has a `keyless` prefix
field and is only emitted on pub/sub.
"""
struct Ros2DDS <: KeyExprFormat end

# Per-formatter escape character substituted for '/' when mangling liveliness tokens.
escape_char(::RmwZenoh) = '%'
escape_char(::Ros2DDS)  = '§'

admin_space(::KeyExprFormat) = "@ros2_lv"

"""
    mangle(::KeyExprFormat, s) -> String

Replace every `/` in `s` with the formatter's escape character.
"""
mangle(f::KeyExprFormat, s::AbstractString) = replace(String(s), '/' => escape_char(f))

"""
    demangle(::KeyExprFormat, s) -> String

Inverse of `mangle`.
"""
demangle(f::KeyExprFormat, s::AbstractString) = replace(String(s), escape_char(f) => '/')

# Strip at most one leading and one trailing '/'; runs of slashes survive (unlike strip(s, '/')).
function _strip_one_slash(s::AbstractString)
    s = startswith(s, '/') ? SubString(s, nextind(s, 1)) : SubString(s, 1)
    s = endswith(s, '/')   ? SubString(s, 1, prevind(s, lastindex(s)))  : s
    String(s)
end

_strip_trailing_slash(s::AbstractString) =
    endswith(s, '/') ? String(SubString(s, 1, prevind(s, lastindex(s)))) : String(s)

# ----- Generic API (dispatched on the formatter singleton) ------------------

"""
    topic_keyexpr(format, endpoint) -> String

The Zenoh key expression used by `endpoint`'s topic for data publication /
subscription. Format-specific; see `RmwZenoh` and `Ros2DDS` for shape.
"""
function topic_keyexpr end

"""
    parse_topic_keyexpr(format, ke::AbstractString) -> (; domain_id, topic, type_info)

Reverse of `topic_keyexpr`: recover the `(type_name, RIHS01 hash)` — together
with the domain and topic — from a received data key expression. Returns a
NamedTuple `(domain_id::Union{Int,Nothing}, topic::String,
type_info::Union{TypeInfo,Nothing})`; `type_info` is `nothing` when the key
carries the empty-type placeholder (or, under `Ros2DDS`, no type at all).

Format-specific; see `RmwZenoh` and `Ros2DDS`. May throw `ArgumentError` on a
key that doesn't match the expected shape.
"""
function parse_topic_keyexpr end

"""
    liveliness_keyexpr(format, entity) -> String

The liveliness token broadcast for `entity` (an `EndpointEntity` or a
`NodeEntity`).
"""
function liveliness_keyexpr end

"""
    parse_liveliness(format, ke::AbstractString; kwargs...) -> Union{NodeEntity, EndpointEntity}

Reverse of `liveliness_keyexpr`. May throw `ArgumentError` if the input
doesn't match the expected shape. Round-trip is exact for endpoints under
both formatters except for fields the encoding doesn't carry (e.g. node
enclave under rmw_zenoh; node identity under ros2dds).

The ros2dds method additionally accepts a `node::NodeEntity` keyword so the
caller can attach a node from a higher-level source (since the token itself
carries none).
"""
function parse_liveliness end

"""
    encode_qos(format, qos::QosProfile; keyless::Bool=false) -> String
    decode_qos(format, s::AbstractString) -> Tuple{Bool, QosProfile}

Encode/decode the QoS suffix that appears in liveliness tokens for
publishers and subscriptions. `keyless` is a topic-level attribute (not part
of the profile itself); rmw_zenoh ignores it, ros2dds prepends it.
`decode_qos` returns `(keyless, profile)` to make the asymmetry explicit.
"""
function encode_qos end
function decode_qos end
