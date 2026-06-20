# The message ↔ ZBytes bridge: the one place ROSNode turns a generated message
# struct into a Zenoh payload and back. Encode sizes the buffer exactly then writes
# once; decode reads straight out of the (copied or borrowed) payload memory, so
# neither direction allocates an intermediate full-payload `Vector`. The wire body
# is the 4-byte CDR encapsulation header (`CDR_LE`) followed by the CDR payload;
# rmw_zenoh per-message metadata rides separately in the Zenoh attachment (below).

using CDRSerialization: CDRReader, CDRWriter, CDRSizeCalculator, read_view,
                        iscompact, materialize, CDRView, CDR_LE
using Zenoh: Zenoh, ZBytes, Sample, AbstractSample, payload, as_memory,
             OwnedZBytes, reusable_copy_bytes, copy_bytes!
import ROSZenoh
# core.jl already re-exports `TypeInfo`; only `TypeHash` needs importing here.
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

The SHM-move publish path (`zref(session, WireOf{T})`) is a transport-layer
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
    # Declare the wire kind (mirrors `_ros_reader`'s `CDRReader(mem, Val(CDR_LE))`) so the
    # writer type is concrete here and `write` dispatches statically — a runtime-kind
    # `CDRWriter(buf)` is abstract and would dispatch + box on every encode.
    write(CDRWriter(buf, Val(CDR_LE)), msg)   # also emits the CDR_LE preamble into buf[1:4]
    return buf
end

# ── ReusableEncoder: the shared allocation-free outbound encode core ──────────
#
# The one buffer-reuse engine behind every outbound message — publisher `put`, service
# `reply`, and service/action request `call!`. Holds a growable CDR buffer + a reused
# `CDRWriter` (preamble written once at construction) + a reused `CDRSizeCalculator` + a
# reused 33-byte attachment buffer, guarded by `lock` for single-writer safety. The
# patterns differ only in transport discipline *after* the encode: the move-consume
# transports (`put`/`reply`, which `_move` the payload) copy the bytes into a held
# `OwnedZBytes` via `Zenoh.copy_bytes!`; the borrow-alias transport (`call!`) passes the
# reused `Vector`s straight in (it aliases them zero-copy for the call's duration).
const _ENC_WRITER = typeof(CDRWriter(Vector{UInt8}(undef, 4), Val(CDR_LE)))
const _ENC_DYN_INIT = 64   # initial growable-buffer capacity; grows to high-water on demand

mutable struct ReusableEncoder
    const lock::Base.ReentrantLock
    const msgbuf::Vector{UInt8}
    const writer::_ENC_WRITER
    const sizecalc::CDRSizeCalculator
    # `growable=false` (compact, `iscompact` fixed wire size): `msgbuf` is pre-sized exactly,
    # a publish just re-seeks + writes — no size pass. `true` (dynamic, or a compact `T` whose
    # default couldn't be sized): size with `sizecalc`, `resize!` `msgbuf` to fit, then write.
    const growable::Bool
    const attbuf::Vector{UInt8}
end

function ReusableEncoder(::Type{T}) where {T}
    msgbuf = UInt8[]; growable = true
    if iscompact(T)
        try
            msgbuf = Vector{UInt8}(undef, length(_encode_to_vector(_default_msg(T))))
            growable = false
        catch err
            @debug "ReusableEncoder: compact fast path unavailable, using growable encode" type=T exception=err
            msgbuf = Vector{UInt8}(undef, _ENC_DYN_INIT); growable = true
        end
    else
        msgbuf = Vector{UInt8}(undef, _ENC_DYN_INIT)
    end
    return ReusableEncoder(Base.ReentrantLock(), msgbuf,
                           CDRWriter(msgbuf, Val(CDR_LE)),   # emits the CDR_LE preamble into msgbuf[1:4]
                           CDRSizeCalculator(), growable, Vector{UInt8}(undef, 33))
end

# Serialize `msg` into the reused buffer (its length set to the exact wire size `n`);
# returns `n` (preamble + body). The exact length lets a borrow-alias caller pass `msgbuf`
# straight to `call!`. Caller holds `enc.lock`.
@inline function encode_into!(enc::ReusableEncoder, msg)
    if enc.growable
        c = enc.sizecalc
        c.offset = 4                                   # reset the accumulator past the preamble
        CDRSerialization.addValue!(c, msg)
        n = position(c)
        n == length(enc.msgbuf) || resize!(enc.msgbuf, n)
    end
    cw = enc.writer
    seek(cw.buf, 4)                                    # preamble untouched
    write(cw, msg)
    return position(cw.buf)
end

# Fill the reused 33-byte attachment buffer with the rmw_zenoh `(seq, ts, gid)` metadata.
@inline fill_attachment!(enc::ReusableEncoder, seq::Integer, ts::Integer, g::NTuple{16, UInt8}) =
    (ROSZenoh._fill_attachment!(enc.attbuf, seq, ts, g); enc.attbuf)

# ── MoveOutbound: the shared move-consume outbound path (publisher `put` + service `reply`) ──
#
# A `ReusableEncoder` plus the two held `OwnedZBytes` the move-consume transports re-arm and
# `_move` each send (both `put` and `reply` consume their payload + attachment ZBytes). `arm!`
# is the **common code path** the publisher and the service reply share: under `out.enc.lock`,
# encode the message + attachment into the reused buffers and `copy_bytes!` them into the held
# boxes, leaving `payload_zb`/`att_zb` ready to `_move` into the transport. The caller supplies
# only the transport verb — `put(route, …)` vs `reply(query, …)` — and holds `out.enc.lock`
# across it (the boxes are shared state the transport consumes; the lock also serializes
# concurrent `Parallel` service replies, the analog of the publisher's single-writer guard).
mutable struct MoveOutbound
    const enc::ReusableEncoder
    const payload_zb::OwnedZBytes
    const att_zb::OwnedZBytes
end
MoveOutbound(::Type{T}) where {T} =
    MoveOutbound(ReusableEncoder(T), reusable_copy_bytes(), reusable_copy_bytes())

# Encode `msg` + the `(seq, ts, gid)` attachment into the held boxes (caller holds `out.enc.lock`).
# After this the boxes hold the wire bytes, ready for the transport to `_move` (or `close` to
# re-gravestone). `copy_bytes!` copies into Zenoh-owned storage in place — no per-send alloc.
@inline function arm!(out::MoveOutbound, msg, seq::Integer, ts::Integer, g::NTuple{16, UInt8})
    enc = out.enc
    n = encode_into!(enc, msg)
    copy_bytes!(out.payload_zb, enc.msgbuf, n)
    fill_attachment!(enc, seq, ts, g)
    copy_bytes!(out.att_zb, enc.attbuf, 33)
    return nothing
end

# Drop the held boxes (gravestone) under the lock. Idempotent — `close(::OwnedZBytes)` is
# gravestone-safe, so a post-send (already-moved) box and a second `close` are both no-ops.
function Base.close(out::MoveOutbound)
    @lock out.enc.lock begin
        close(out.payload_zb)
        close(out.att_zb)
    end
    return nothing
end

"""
    as(x, ::Type{T}) -> T

Cast a ROS message value `x` to the layout-compatible message type `T` by
round-tripping it through the CDR wire codec. Two distinct Julia structs that share
one wire type carry equal RIHS01 type hashes, hence identical CDR field layout — for
example two modules' separate aliases of `sensor_msgs/msg/Image`. `as` encodes `x` to
its CDR bytes and decodes those bytes as `T`, handing the value across the
nominal-type boundary with the same round-trip invariant the transport relies on.

- `typeof(x) === T` — returns `x` itself (identity short-circuit).
- otherwise — a fully-owned `T`; the decode copies every field out (see `decode_owned`).

Layout compatibility is the caller's contract: pass only equal-RIHS01 types. The
decode is positional, so a mismatched `T` fails one of two ways:

- overruns the payload — an `EOFError`/`BoundsError`.
- decodes silently into garbage — when its fields happen to fit the bytes.

Use it when a value built as one alias reaches code expecting another. Type
resolution warns when two distinct Julia structs settle on one RIHS01 wire type at a
common dependent; handing such a value across that boundary is the canonical case:
`f(as(msg, ThatType))`.

A borrowed `CDRView` (from `decode(...; view=true)`) takes its own method:

- it stores its fields in one NamedTuple, so it cannot walk the encoder directly — it is copied out of its buffer first (`decode_owned`) and the owned value cast.
- `as` on a view therefore always returns an owned `T`, including when `T` is the view's own tag.
- for a zero-copy cast of a view to a sibling type use `CDRSerialization.retag`.

```julia
# Illustrative: ImageA and ImageB stand for two modules' aliases of
# sensor_msgs/msg/Image (equal RIHS01).
img_b = as(img_a, ImageB)   # owned ImageB carrying img_a's field values
as(img_a, ImageA)           # returns img_a itself (identity short-circuit)
```
"""
as(x, ::Type{T}) where {T} = typeof(x) === T ? x : decode(_encode_to_vector(x), T)
# A view's single-NamedTuple layout can't walk the @generated encoder (its buffer is
# not reachable either — no struct-start offset, and a nested view's buffer is the
# outer message's bytes); copy it out, then cast.
as(v::CDRView, ::Type{T}) where {T} = as(decode_owned(v), T)
export as

# ── decode: ZBytes / Sample → message ────────────────────────────────────

"""
    decode(sample::Sample, ::Type{T}; view=false) -> T | CDRView{T}

Materialize a `T` from a received `Sample`. The `view` keyword selects copy
semantics (default owned, for correctness first):

| `view`    | result       | lifetime                                  |
|-----------|--------------|-------------------------------------------|
| `false`   | owned `T`    | every field copied out — storable / forwardable / spawnable with no caveats |
| `true`    | `CDRView{T}` | variable-length fields alias the payload bytes — valid only while the backing memory is live |

The subscription dispatcher runs the view handler inside `with_memory` and hands
us the borrowed memory through the `DenseVector{UInt8}` overload below.

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
`Borrowed`); the owned path with a copied `Memory`. The `view` keyword selects
the return:

- `true` — zero-copies the `Vector`-of-POD / string fields as `CDRArray` / `CDRString` aliases of `mem`.
- `false` (the default) — materializes a fully-owned `T`.

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
storable / forwardable / spawnable with no lifetime caveats. The sample
overload copies the payload into freshly-owned `Memory{UInt8}` first, so the
decode outlives the sample.
"""
@inline decode_owned(mem::DenseVector{UInt8}, ::Type{T}) where {T} = read(_ros_reader(mem), T)
@inline decode_owned(sample::AbstractSample, ::Type{T}) where {T} =
    decode_owned(as_memory(payload(sample), UInt8), T)

"""
    decode_view(mem::DenseVector{UInt8}, ::Type{T}) -> CDRView{T} | T

Type-stable view decode: returns a `CDRView{T}` whose variable-length fields alias
`mem` (valid only while the backing memory is live), or — for a compact
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
the escape hatch when a view must outlive its backing payload. A no-op for
fields already owned; copies `CDRArray`/`CDRString` aliases into `Vector`/`String`.
"""
decode_owned(view::CDRView) = materialize(view)

# ── per-message attachment ───────────────────────────────────────────────
# The `(sequence_number::Int64, source_timestamp::Int64, source_gid::NTuple{16,
# UInt8})` triple that rides every `put`/request/reply, byte-exact against native
# rmw_zenoh peers. Encode/decode, the fixed `[u8;16]` gid form, and `gid`
# derivation live in ROSZenoh; these wrappers thread them through so ROSNode
# callers spell the wire metadata in one vocabulary. The publisher/service layer
# supplies the gid from its `EndpointEntity`.

"""
    encode_attachment(seq, ts, gid::NTuple{16,UInt8}) -> ZBytes

Serialize the per-message metadata triple into a `put`/`reply` attachment.
Wraps `ROSZenoh.encode_attachment`; `seq`/`ts` coerce to `Int64`, `gid`
stays the fixed-width `NTuple{16,UInt8}` so the encoding is byte-exact against
native rmw_zenoh peers — a `Vector` gid gets length-prefixed and fails their
deserialize.
"""
encode_attachment(seq::Integer, ts::Integer, gid::NTuple{16,UInt8}) =
    ROSZenoh.encode_attachment(seq, ts, gid)

"""
    decode_attachment(sample) -> (seq::Int64, ts::Int64, gid::NTuple{16,UInt8})

Read the metadata triple back from a received `Sample` (or `ZBytes`). The inverse
of [`encode_attachment`](@ref); wraps `ROSZenoh.decode_attachment`.
"""
decode_attachment(sample) = ROSZenoh.decode_attachment(sample)

# ── type identity for a message type ─────────────────────────────────────

"""
    type_info(::Type{T}) -> TypeInfo

The `TypeInfo` — the (qualified name, RIHS01) wire identity — for a generated
message type, used to build the data-route and liveliness key expressions. The
name and hash identify the .msg interface on the wire — see
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html. The name is
recovered reflectively from the type's module path: a generated type lives at
`<package>.<qualifier>.<Name>` (e.g. `std_msgs.msg.String`), which maps to the
ROS2 name `"<package>/<qualifier>/<Name>"`.

The RIHS01 hash is a per-type value computed from the struct AST, which the
generated Julia type does not carry. This shapes the hash returned here:

- this reflective method returns the zero placeholder (`TypeHash()`, the Humble sentinel), since the AST is unavailable.
- the placeholder is sufficient for keyexpr structure.
- for cross-version hash matching, use a registered type's verified hash — carried in its [`RegistryEntry`](@ref) and recovered with `type_info_of`.
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
