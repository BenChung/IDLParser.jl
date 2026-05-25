# RIHS01 — ROS IDL Hash Standard 01.
#
# SHA-256 over a JSON encoding of a `TypeDescription` (the message's
# field-by-field shape) with its referenced types. The JSON shape and spacing
# mirrors what ROS2's tooling emits (Python's `json.dumps()` default: spaces
# after `:` and `,`). The same byte-for-byte JSON across implementations means
# the same hash everywhere — that's the point.
#
# Spec & reference impl: ROS 2 `type_description_interfaces` + the Rust port
# at https://github.com/ZettaScaleLabs/hiroz/blob/main/crates/hiroz-schema/src/hash.rs

using SHA
using Moshi.Match: @match

# ---- TypeId constants ------------------------------------------------------
# From `type_description_interfaces/msg/FieldType`. The primitive IDs live in
# 1..22; the array / bounded-sequence / unbounded-sequence variants are the
# primitive ID plus a fixed offset of 48 / 96 / 144 respectively.

const TYPE_ID_NESTED_TYPE      = 0x01
const TYPE_ID_INT8             = 0x02
const TYPE_ID_UINT8            = 0x03
const TYPE_ID_INT16            = 0x04
const TYPE_ID_UINT16           = 0x05
const TYPE_ID_INT32            = 0x06
const TYPE_ID_UINT32           = 0x07
const TYPE_ID_INT64            = 0x08
const TYPE_ID_UINT64           = 0x09
const TYPE_ID_FLOAT            = 0x0A
const TYPE_ID_DOUBLE           = 0x0B
const TYPE_ID_LONG_DOUBLE      = 0x0C
const TYPE_ID_CHAR             = 0x0D
const TYPE_ID_WCHAR            = 0x0E
const TYPE_ID_BOOLEAN          = 0x0F
const TYPE_ID_BYTE             = 0x10
const TYPE_ID_STRING           = 0x11
const TYPE_ID_WSTRING          = 0x12
const TYPE_ID_FIXED_STRING     = 0x13
const TYPE_ID_FIXED_WSTRING    = 0x14
const TYPE_ID_BOUNDED_STRING   = 0x15
const TYPE_ID_BOUNDED_WSTRING  = 0x16

const ARRAY_OFFSET             = 0x30  # 48
const BOUNDED_SEQUENCE_OFFSET  = 0x60  # 96
const UNBOUNDED_SEQUENCE_OFFSET = 0x90 # 144

# ---- Data structures (mirroring the Rust schema's hash-version layout) -----

struct FieldTypeDescription
    type_id::UInt8
    capacity::UInt64
    string_capacity::UInt64
    nested_type_name::String
end
FieldTypeDescription(type_id::Integer) =
    FieldTypeDescription(UInt8(type_id), UInt64(0), UInt64(0), "")

struct FieldDescription
    name::String
    field_type::FieldTypeDescription
end

struct TypeDescription
    type_name::String
    fields::Vector{FieldDescription}
end

struct TypeDescriptionMsg
    type_description::TypeDescription
    referenced_type_descriptions::Vector{TypeDescription}
end

# ---- JSON serialization ----------------------------------------------------
#
# We write JSON manually rather than depending on a JSON library — the shape
# is fixed, and we need exact control over key order and `: ` / `, ` spacing
# to match the reference output byte-for-byte. Diverging by one space changes
# the hash.

function _json_string(io::IO, s::AbstractString)
    write(io, '"')
    for c in s
        if c == '"'
            write(io, "\\\"")
        elseif c == '\\'
            write(io, "\\\\")
        elseif c == '\b'
            write(io, "\\b")
        elseif c == '\f'
            write(io, "\\f")
        elseif c == '\n'
            write(io, "\\n")
        elseif c == '\r'
            write(io, "\\r")
        elseif c == '\t'
            write(io, "\\t")
        elseif UInt32(c) < 0x20
            write(io, "\\u", lpad(string(UInt32(c), base=16), 4, '0'))
        else
            write(io, c)
        end
    end
    write(io, '"')
end

function _json_field_type(io::IO, ft::FieldTypeDescription)
    write(io, "{\"type_id\": ", string(Int(ft.type_id)),
              ", \"capacity\": ", string(ft.capacity),
              ", \"string_capacity\": ", string(ft.string_capacity),
              ", \"nested_type_name\": ")
    _json_string(io, ft.nested_type_name)
    write(io, '}')
end

function _json_field(io::IO, f::FieldDescription)
    write(io, "{\"name\": ")
    _json_string(io, f.name)
    write(io, ", \"type\": ")
    _json_field_type(io, f.field_type)
    write(io, '}')
end

function _json_type_desc(io::IO, td::TypeDescription)
    write(io, "{\"type_name\": ")
    _json_string(io, td.type_name)
    write(io, ", \"fields\": [")
    for (i, f) in enumerate(td.fields)
        i > 1 && write(io, ", ")
        _json_field(io, f)
    end
    write(io, "]}")
end

function _json_msg(io::IO, msg::TypeDescriptionMsg)
    write(io, "{\"type_description\": ")
    _json_type_desc(io, msg.type_description)
    write(io, ", \"referenced_type_descriptions\": [")
    for (i, td) in enumerate(msg.referenced_type_descriptions)
        i > 1 && write(io, ", ")
        _json_type_desc(io, td)
    end
    write(io, "]}")
end

"""
    to_ros2_json(msg::TypeDescriptionMsg) -> String

Serialize a TypeDescriptionMsg into the JSON form that RIHS01 hashes over.
"""
function to_ros2_json(msg::TypeDescriptionMsg)
    io = IOBuffer()
    _json_msg(io, msg)
    String(take!(io))
end

# ---- Hashing ---------------------------------------------------------------

"""
    calculate_rihs01_hash(msg::TypeDescriptionMsg) -> String

Return the RIHS01 hash string (`"RIHS01_<64 hex>"`) for the given message.
The caller is responsible for sorting `referenced_type_descriptions` if
cross-implementation compatibility matters — the hash function does not sort.
"""
function calculate_rihs01_hash(msg::TypeDescriptionMsg)
    return "RIHS01_" * bytes2hex(sha256(to_ros2_json(msg)))
end

# ---- AST → TypeDescription mapping -----------------------------------------

# Resolve a scoped name into the ROS2 fully-qualified form. Relative names
# (empty path) are expanded against `package`, joined by `/` like
# `geometry_msgs/msg/Point`. The middle `msg` segment is part of the rosidl
# convention; for service / action references the caller can pass a custom
# `qualifier`.
function _qualified_ros2_name(name::Parse.ScopedName.Type,
                              package::AbstractString,
                              qualifier::AbstractString)
    @match name begin
        Parse.ScopedName.Name(path, n, _) => begin
            isempty(path) || return join([String.(path); String(n)], "/")
            isempty(package) && return String(n)
            return string(package, '/', qualifier, '/', String(n))
        end
    end
end

# Helper: pull the integer out of a `Lit(Intg(n))` ConstExpr, erroring out on
# any other shape (e.g. a non-constant-folded length expression).
_intg_value(expr) = @match expr begin
    Parse.ConstExpr.Lit(Parse.Literal.Intg(n)) => Int(n)
    _ => error("expected integer-literal length, got $expr")
end

# (primitive_type_id, string_capacity, nested_type_name) for a "leaf" type —
# one that's not itself an array or sequence wrapper. Errors on unsupported
# IDL forms (any, value-base, fixed-point, map).
#
# Moshi `@match` treats bare identifiers (including `nothing`) as variable
# bindings, not literals — so we bind the inner length and discriminate with
# `len === nothing` in the body.
function _leaf_type_info(ts::Parse.TypeSpec.Type,
                         package::AbstractString,
                         qualifier::AbstractString)
    @match ts begin
        Parse.TypeSpec.TBool() => (TYPE_ID_BOOLEAN, UInt64(0), "")
        Parse.TypeSpec.TOctet() => (TYPE_ID_BYTE, UInt64(0), "")
        Parse.TypeSpec.TInt(8)  => (TYPE_ID_INT8,  UInt64(0), "")
        Parse.TypeSpec.TInt(16) => (TYPE_ID_INT16, UInt64(0), "")
        Parse.TypeSpec.TInt(32) => (TYPE_ID_INT32, UInt64(0), "")
        Parse.TypeSpec.TInt(64) => (TYPE_ID_INT64, UInt64(0), "")
        Parse.TypeSpec.TUInt(8)  => (TYPE_ID_UINT8,  UInt64(0), "")
        Parse.TypeSpec.TUInt(16) => (TYPE_ID_UINT16, UInt64(0), "")
        Parse.TypeSpec.TUInt(32) => (TYPE_ID_UINT32, UInt64(0), "")
        Parse.TypeSpec.TUInt(64) => (TYPE_ID_UINT64, UInt64(0), "")
        Parse.TypeSpec.TFloat(32)  => (TYPE_ID_FLOAT,       UInt64(0), "")
        Parse.TypeSpec.TFloat(64)  => (TYPE_ID_DOUBLE,      UInt64(0), "")
        Parse.TypeSpec.TFloat(128) => (TYPE_ID_LONG_DOUBLE, UInt64(0), "")
        Parse.TypeSpec.TChar()  => (TYPE_ID_CHAR,  UInt64(0), "")
        Parse.TypeSpec.TWChar() => (TYPE_ID_WCHAR, UInt64(0), "")
        Parse.TypeSpec.TString(len) =>
            len === nothing ?
                (TYPE_ID_STRING, UInt64(0), "") :
                (TYPE_ID_BOUNDED_STRING, UInt64(_intg_value(len)), "")
        Parse.TypeSpec.TWString(len) =>
            len === nothing ?
                (TYPE_ID_WSTRING, UInt64(0), "") :
                (TYPE_ID_BOUNDED_WSTRING, UInt64(_intg_value(len)), "")
        Parse.TypeSpec.TRef(scoped) =>
            (TYPE_ID_NESTED_TYPE, UInt64(0),
             _qualified_ros2_name(scoped, package, qualifier))
        _ => error("unsupported TypeSpec for RIHS01: $ts")
    end
end

# Map a (TypeSpec, Declarator) pair into a FieldTypeDescription. The
# Declarator distinguishes static arrays (DArray, type_id += ARRAY_OFFSET)
# from scalars; a TypeSpec.TSeq adds the bounded/unbounded-sequence offset
# to the *element* type's ID.
function _to_field_type(ts::Parse.TypeSpec.Type,
                        decl::Parse.Declarator.Type,
                        package::AbstractString,
                        qualifier::AbstractString)
    static_capacity = @match decl begin
        Parse.Declarator.DArray(_, dims) => begin
            length(dims) == 1 ||
                error("RIHS01 only supports 1-D arrays; got $(length(dims)) dims")
            @match dims[1] begin
                Parse.ConstExpr.Lit(Parse.Literal.Intg(n)) => UInt64(n)
                _ => error("non-literal array dim is not RIHS01-representable: $(dims[1])")
            end
        end
        Parse.Declarator.DIdent(_) => UInt64(0)
    end

    # Unwrap an outer TSeq to find the element type plus its bound (if any).
    seq_offset, seq_capacity, inner_ts = @match ts begin
        Parse.TypeSpec.TSeq(elt, len) =>
            len === nothing ?
                (UNBOUNDED_SEQUENCE_OFFSET, UInt64(0), elt) :
                (BOUNDED_SEQUENCE_OFFSET, UInt64(_intg_value(len)), elt)
        _ => (UInt8(0), UInt64(0), ts)
    end

    prim_id, string_cap, nested = _leaf_type_info(inner_ts, package, qualifier)

    if seq_offset != 0 && static_capacity != 0
        # ROS2's flat type-id encoding can't represent "static array of
        # sequence-of-T" — that requires a nested wrapper type. Reject
        # rather than emit a wrong hash silently.
        error("RIHS01 cannot encode a static array of a sequence")
    end

    type_id = if static_capacity != 0
        prim_id + ARRAY_OFFSET
    elseif seq_offset != 0
        prim_id + seq_offset
    else
        prim_id
    end

    capacity = static_capacity != 0 ? static_capacity : seq_capacity
    return FieldTypeDescription(type_id, capacity, string_cap, nested)
end

# Strip leading Annotated wrappers (e.g. `@default(...)`) — defaults are
# excluded from the hash per the RIHS01 spec.
function _unwrap_annotated(node)
    while node isa Parse.Annotated.Type
        node = @match node begin
            Parse.Annotated.Annotation(_, _, inner) => inner
        end
    end
    return node
end

"""
    type_description_from_struct(struct_ast, name; package="", qualifier="msg") -> TypeDescription

Build a RIHS01 `TypeDescription` from a parsed struct AST. `name` is the
fully-qualified ROS2 name (`"std_msgs/msg/String"`) — if you pass a bare name
and a `package`, it'll be assembled as `"\$package/\$qualifier/\$name"`.
"""
function type_description_from_struct(struct_ast::Parse.TypeDecl.Type,
                                      name::AbstractString;
                                      package::AbstractString="",
                                      qualifier::AbstractString="msg")
    members = @match struct_ast begin
        Parse.TypeDecl.StructDecl(_, _, ms) => ms
        _ => error("type_description_from_struct expects a StructDecl, got $(typeof(struct_ast))")
    end

    full_name = if occursin('/', name) || isempty(package)
        String(name)
    else
        string(package, '/', qualifier, '/', name)
    end

    fields = FieldDescription[]
    for m in members
        pair = _unwrap_annotated(m)
        ts, decls = pair
        length(decls) == 1 ||
            error("ROS2 fields have a single declarator each; got $(length(decls))")
        decl = decls[1]
        field_name = @match decl begin
            Parse.Declarator.DArray(n, _) => String(n)
            Parse.Declarator.DIdent(n)   => String(n)
        end
        ft = _to_field_type(ts, decl, package, qualifier)
        push!(fields, FieldDescription(field_name, ft))
    end
    return TypeDescription(full_name, fields)
end

"""
    rihs01_hash(struct_ast, name; package="", qualifier="msg", references=TypeDescription[]) -> String

High-level convenience: build the TypeDescriptionMsg and return its RIHS01
hash string. `references` is a vector of pre-built `TypeDescription`s for
every nested type the message refers to; the caller is responsible for
sorting them (the spec says the hash does not auto-sort).
"""
function rihs01_hash(struct_ast::Parse.TypeDecl.Type,
                     name::AbstractString;
                     package::AbstractString="",
                     qualifier::AbstractString="msg",
                     references::AbstractVector{TypeDescription}=TypeDescription[])
    td = type_description_from_struct(struct_ast, name; package=package, qualifier=qualifier)
    return calculate_rihs01_hash(TypeDescriptionMsg(td, collect(references)))
end
