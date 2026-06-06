# The message ↔ ZBytes bridge: the one place ROSNode turns a generated message
# struct into a Zenoh payload and back. Encode sizes the buffer exactly then writes
# once; decode reads straight out of the (copied or borrowed) payload memory, so
# neither direction allocates an intermediate full-payload `Vector`. The wire body
# is the 4-byte CDR encapsulation header (`CDR_LE`) followed by the CDR payload;
# rmw_zenoh per-message metadata rides separately in the Zenoh attachment (below).

using CDRSerialization: CDRReader, CDRWriter, CDRSizeCalculator, read_view,
                        iscompact, materialize, CDRView, CDR_LE
using Zenoh: ZBytes, Sample, AbstractSample, payload, as_memory
import ROSZenoh
# `TypeInfo` is already in scope (core.jl re-exports it); `TypeHash` is not.
using ROSZenoh: TypeHash

# `read`/`write`/`position` are Base; `addValue!` is CDRSerialization's own
# (unexported) — reach it through the module.
import CDRSerialization

# ── encode: message → ZBytes ─────────────────────────────────────────────

"""
    encode(msg) -> ZBytes

Serialize a generated ROS message `msg` to a Zenoh payload. One exact-size
allocation (the `CDRSizeCalculator` pass gives the precise byte count, preamble
included), one serialization pass with `CDRWriter`, then the buffer is wrapped
as a `ZBytes` that `put`/`reply` borrows: Zenoh pins the buffer until its deleter
fires, so the transport reuses our heap allocation directly. Compact
(`@cdr1_compat`) messages take `write`'s single-store fast path internally.

`write` is the exact-buffer entry point: it `ensureroom`s the precise per-field
size, matching the tightly-sized buffer this path allocates. (`write_all!` budgets
padding conservatively upfront for growable `IOBuffer`s and would overrun the
exact `Memory` sized here.)

The SHM-move publish path (`zref(session, WireOf{T})`, §3.3) is a transport-layer
choice made by the publisher, not here; this is the heap-borrow form.
"""
function encode(msg)
    # Borrow the heap buffer (copy=false): Zenoh keeps it alive for transmission.
    return ZBytes(_encode_to_vector(msg))
end

# Size-then-write a message into an exact `Vector{UInt8}` (4-byte CDR_LE preamble +
# payload). Shared by `encode` (which wraps it as a borrowed `ZBytes`) and `as` (which
# decodes it straight into a sibling Julia type).
function _encode_to_vector(msg)
    calc = CDRSizeCalculator()
    CDRSerialization.addValue!(calc, msg)
    buf = Vector{UInt8}(undef, position(calc))
    write(CDRWriter(buf), msg)      # CDRWriter emits the CDR_LE preamble into buf[1:4]
    return buf
end

"""
    as(x, ::Type{T}) -> T

Boundary cast: re-materialize `x` as the layout-compatible ROS type `T`. For two
distinct Julia structs that share one wire type (equal RIHS01 ⇒ identical CDR form
and field layout — e.g. two modules' aliases of `sensor_msgs/msg/Image`), this hands
`x`'s value across the nominal-type boundary via the exact wire codec, correct by the
same round-trip invariant the wire relies on. Returns `x` unchanged when
`typeof(x) === T`. A genuine layout mismatch surfaces as a decode error rather than a
silent reinterpretation.

Use it when a value built as one alias reaches code expecting another — e.g. a handler
on a Context whose `home` resolves the wire type to a different struct than the
handler's body dispatches on: `f(as(msg, ThatType))`. A `CDRView` re-tags zero-copy
through `CDRSerialization.retag`.
"""
as(x, ::Type{T}) where {T} = typeof(x) === T ? x : decode(_encode_to_vector(x), T)
export as

# ── decode: ZBytes / Sample → message ────────────────────────────────────

"""
    decode(sample::Sample, ::Type{T}; view=false) -> T | CDRView{T}

Materialize a `T` from a received `Sample`. Owned by default (`view=false`):
every field is copied out, so the result is storable / forwardable / spawnable
with no lifetime caveats (§3.1, correctness first). `view=true` returns a
`CDRView{T}` whose variable-length fields alias the payload bytes — only valid
while the backing memory is live (§3.2); the subscription dispatcher runs the
view handler inside `with_memory` and hands us the borrowed memory through the
`DenseVector{UInt8}` overload below.

The owned form copies the payload once (`as_memory`) so the decoded message
outlives the sample. Both forms parse the 4-byte CDR preamble via `CDRReader`.
"""
function decode(sample::AbstractSample, ::Type{T}; view::Bool=false) where {T}
    # Owned: copy the payload into freshly-owned `Memory{UInt8}` so the decode
    # (and any aliasing views inside it) outlive the sample.
    mem = as_memory(payload(sample), UInt8)
    return decode(mem, T; view=view)
end

"""
    decode(mem::DenseVector{UInt8}, ::Type{T}; view=false) -> T | CDRView{T}

Decode `T` from a contiguous CDR payload (preamble included). The dispatcher's
view path calls this with payload-aliasing memory (`unsafe_memory` of a
`Borrowed`); the owned path with a copied `Memory`. `view=true` zero-copies the
`Vector`-of-POD / string fields as `CDRArray` / `CDRString` aliases of `mem`;
`view=false` (the default) materializes a fully-owned `T`.

Compact (`@cdr1_compat`) messages decode identically either way — `read` takes
the single-load fast path (`iscompact`), and the value is plain bits with no
aliasing, so a "view" of one is already escapable.
"""
function decode(mem::DenseVector{UInt8}, ::Type{T}; view::Bool=false) where {T}
    # Runtime `view::Bool` widens the return to `Union{T, CDRView{T}}`, forcing a
    # boxed result and dynamic dispatch. Prefer `decode_view`/`decode_owned` on hot
    # paths; each is type-stable.
    return view ? decode_view(mem, T) : decode_owned(mem, T)
end

"""
    decode_owned(mem::DenseVector{UInt8}, ::Type{T}) -> T
    decode_owned(sample::AbstractSample, ::Type{T}) -> T

Type-stable owned decode: always materializes a fully-owned `T` (one field walk),
storable / forwardable / spawnable with no lifetime caveats (§3.1). The sample
overload copies the payload into freshly-owned `Memory{UInt8}` first, so the
decode outlives the sample.
"""
@inline decode_owned(mem::DenseVector{UInt8}, ::Type{T}) where {T} = read(_ros_reader(mem), T)
@inline decode_owned(sample::AbstractSample, ::Type{T}) where {T} =
    decode_owned(as_memory(payload(sample), UInt8), T)

"""
    decode_view(mem::DenseVector{UInt8}, ::Type{T}) -> CDRView{T} | T

Type-stable view decode: returns a `CDRView{T}` whose variable-length fields alias
`mem` (valid only while the backing memory is live, §3.2), or — for a compact
(`@cdr1_compat`) `T` — an owned `T`, since a "view" of plain bits is already
escapable. `iscompact` is `@generated` and constant-folds per concrete `T`, so the
`?:` dead-branches away and each specialization has a single concrete return type
(no `Union`/`Any` box — contrast the `view::Bool`-keyword `decode`).
"""
@inline function decode_view(mem::DenseVector{UInt8}, ::Type{T}) where {T}
    r = _ros_reader(mem)
    return iscompact(r, T) ? read(r, T) : read_view(r, T)
end

# ROS2/rmw_zenoh serializes with `CDR_LE` (CDR v1, little-endian). Passing the
# encapsulation as a `Val` gives a concrete `CDRReader{B,false,true}` whose
# `IsCDR2`/`LE` params are known at compile time, so `decode_view`/`decode_owned`
# infer a concrete return with no dynamic dispatch. The preamble is still consumed
# and validated; a non-`CDR_LE` payload throws rather than mis-decoding.
@inline _ros_reader(mem::DenseVector{UInt8}) = CDRReader(mem, Val(CDR_LE))

"""
    decode_owned(view::CDRView{T}) -> T

Copy a `CDRView` (from `decode(...; view=true)`) out into its fully-owned form —
the escape hatch when a view must outlive its backing payload (§3.2). A no-op for
fields already owned; copies `CDRArray`/`CDRString` aliases into `Vector`/`String`.
"""
decode_owned(view::CDRView) = materialize(view)

# ── per-message attachment (§3.4) ────────────────────────────────────────
# The `(sequence_number::Int64, source_timestamp::Int64, source_gid::NTuple{16,
# UInt8})` triple that rides every `put`/request/reply, byte-for-byte with hiroz.
# Encode/decode, the fixed `[u8;16]` gid form, and `gid` derivation (`entity_gid`)
# live in ROSZenoh (`attachment.jl`); these wrappers thread them through so ROSNode
# callers spell the wire metadata in one vocabulary. The publisher/service layer
# supplies the gid from its `EndpointEntity`.

"""
    encode_attachment(seq, ts, gid::NTuple{16,UInt8}) -> ZBytes

Serialize the per-message metadata triple into a `put`/`reply` attachment.
Wraps [`ROSZenoh.encode_attachment`](@ref); `seq`/`ts` coerce to `Int64`, `gid`
stays the fixed-width `NTuple{16,UInt8}` (the parity pivot — a `Vector` would be
length-prefixed and break byte-compatibility).
"""
encode_attachment(seq::Integer, ts::Integer, gid::NTuple{16,UInt8}) =
    ROSZenoh.encode_attachment(seq, ts, gid)

"""
    decode_attachment(sample) -> (seq::Int64, ts::Int64, gid::NTuple{16,UInt8})

Read the metadata triple back from a received `Sample` (or `ZBytes`). The inverse
of [`encode_attachment`](@ref); wraps [`ROSZenoh.decode_attachment`](@ref).
"""
decode_attachment(sample) = ROSZenoh.decode_attachment(sample)

# ── type identity for a message type (§2.1) ──────────────────────────────

"""
    type_info(::Type{T}) -> TypeInfo

The `TypeInfo` (qualified ROS2 name + RIHS01 hash) for a generated message type,
used to build the data-route and liveliness key expressions. The name and hash
identify the .msg interface on the wire — see
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html. The name is
recovered reflectively from the type's module path: a generated type lives at
`<package>.<qualifier>.<Name>` (e.g. `std_msgs.msg.String`), which maps to the
ROS2 name `"<package>/<qualifier>/<Name>"`.

The RIHS01 hash is a per-type value computed from the struct AST, which the
generated Julia type does not carry — it belongs to the type registry, which
`eval`s codegen from the IL and can stash the `type_info_from_struct` result keyed
by `(name, hash)`. Until that registry lands, the hash is the zero placeholder
(`TypeHash()`, the Humble sentinel): correct for keyexpr structure, but not for
cross-version hash matching. The registry specializes this method per registered
type with the real hash, so static specialized types are fast and dynamic ones go
through `invokelatest`.
"""
function type_info(::Type{T}) where {T}
    return TypeInfo(ros_type_name(T), TypeHash())
end

"""
    ros_type_name(::Type{T}) -> String

The fully-qualified ROS2 type name for a generated message type, derived from its
module nesting: `parentmodule` is the `msg`/`srv`/`action` submodule and its
parent is the package, so `std_msgs.msg.String` ⇒ `"std_msgs/msg/String"`. Falls
back to the bare type name when the type is not nested under the
package/qualifier modules (e.g. a hand-defined type at top level).
"""
function ros_type_name(::Type{T}) where {T}
    name = string(nameof(T))
    qualifier_mod = parentmodule(T)
    qualifier = string(nameof(qualifier_mod))
    package_mod = parentmodule(qualifier_mod)
    # A generated type sits two modules deep (`package.qualifier.Name`); anything
    # shallower (e.g. defined directly in a module) has no ROS package context.
    (package_mod === qualifier_mod || qualifier_mod === Main) && return name
    package = string(nameof(package_mod))
    return string(package, "/", qualifier, "/", name)
end
