# The message ↔ ZBytes bridge (§2.1/§3): the one place ROSNode turns a generated
# message struct into a Zenoh payload and back. Both directions avoid an
# intermediate full-payload `Vector` — encode sizes exactly then writes once,
# decode reads straight out of the (copied or borrowed) payload memory. The wire
# body is the 4-byte CDR encapsulation header (`CDR_LE`) + CDR payload; rmw_zenoh
# per-message metadata rides separately in the Zenoh attachment (below).

using CDRSerialization: CDRReader, CDRWriter, CDRSizeCalculator, read_view,
                        iscompact, materialize, CDRView
using Zenoh: ZBytes, Sample, payload, as_memory
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
as a `ZBytes` that `put`/`reply` *borrows* (no copy into the transport — Zenoh
pins the buffer until its deleter fires, §3.3). Compact (`@cdr1_compat`) messages
take `write`'s single-store fast path internally.

`write` (not `write_all!`) is the exact-buffer entry point: it `ensureroom`s the
*precise* per-field size, whereas `write_all!`'s upfront conservative budget
over-estimates padding and overruns a tightly-sized buffer (it targets growable
`IOBuffer`s, not the exact `Memory` we size here).

The SHM-move publish path (`zref(session, WireOf{T})`, §3.3) is a transport-layer
choice made by the publisher, not here; this is the heap-borrow form.
"""
function encode(msg)
    calc = CDRSizeCalculator()
    CDRSerialization.addValue!(calc, msg)
    nbytes = position(calc)

    buf = Vector{UInt8}(undef, nbytes)
    w = CDRWriter(buf)              # emits the CDR_LE preamble into buf[1:4]
    write(w, msg)

    # Borrow the heap buffer (copy=false): Zenoh keeps it alive for transmission.
    return ZBytes(buf)
end

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
function decode(sample::Sample, ::Type{T}; view::Bool=false) where {T}
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
    r = CDRReader(mem)
    if view && !iscompact(r, T)
        # Zero-copy: variable-length fields alias `mem`; lifetime is the caller's.
        return read_view(r, T)
    end
    # Owned (or compact, where view≡owned): full materialization, one field walk.
    return read(r, T)
end

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
# The encode/decode and the fixed `[u8;16]` gid form live in ROSZenoh
# (`attachment.jl`); we thread them through so ROSNode callers spell the wire
# metadata in one vocabulary. `gid` derivation (`entity_gid`) is also ROSZenoh's —
# the publisher/service layer supplies the gid from its `EndpointEntity`.

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
used to build the data-route and liveliness key expressions (§2.2). The name is
recovered reflectively from the type's module path: a generated type lives at
`<package>.<qualifier>.<Name>` (e.g. `std_msgs.msg.String`), which maps to the
ROS2 name `"<package>/<qualifier>/<Name>"`.

TODO(typesupport §11): the RIHS01 hash is a per-type *fact computed from the
struct AST*, which the generated Julia type does not carry — it belongs to the
type registry, which `eval`s codegen from the IL and can stash the
`type_info_from_struct` result keyed by `(name, hash)`. Until that registry
lands, the hash is the zero placeholder (`TypeHash()`, the Humble sentinel),
which is correct enough for keyexpr *structure* but not for cross-version hash
matching. The registry specializes this method per registered type with the real
hash; static types so specialized are fast, dynamic ones reached via
`invokelatest`.
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
