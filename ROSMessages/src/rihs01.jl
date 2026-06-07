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
# Hand-written so key order and `: ` / `, ` spacing match the reference output
# byte-for-byte — a one-space divergence changes the hash.

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
The hash covers `referenced_type_descriptions` in the order given; sort them
first (by `type_name`) to match other ROS 2 implementations.
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
        # The flat type-id encoding has no "static array of sequence-of-T";
        # that shape needs a nested wrapper type.
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

# ---- service-level TypeDescription (rmw_zenoh service keyexpr hash) ---------
#
# rmw_zenoh names a service/client keyexpr by the *service* type (`pkg::srv::dds_::
# Base_`), not the request message — and the RIHS01 in that keyexpr is the service
# type hash. rosidl computes it over a synthesized umbrella: a `pkg/srv/Base`
# type whose three fields reference the request, response, and a synthesized
# `pkg/srv/Base_Event` message (`info` = service_msgs/ServiceEventInfo, plus
# bounded-1 sequences of the request and response). The hash is over that umbrella
# plus the name-sorted, deduplicated closure of all three.

"""
    service_type_description(service_name, request, response, service_event_info) -> TypeDescriptionMsg

Build the ROS2 *service-level* `TypeDescriptionMsg` whose RIHS01 matches the
service type hash rmw_zenoh puts in the service/client keyexpr. `service_name` is
the fully-qualified `pkg/srv/Base`; `request`/`response` are the request/response
`TypeDescriptionMsg`s (main + their own referenced closures, as the registry
stores them); `service_event_info` is `service_msgs/msg/ServiceEventInfo`'s
`TypeDescriptionMsg` (its closure supplies `builtin_interfaces/msg/Time`).
"""
function service_type_description(service_name::AbstractString,
                                  request::TypeDescriptionMsg,
                                  response::TypeDescriptionMsg,
                                  service_event_info::TypeDescriptionMsg)
    req_name   = request.type_description.type_name
    resp_name  = response.type_description.type_name
    sei_name   = service_event_info.type_description.type_name
    event_name = string(service_name, "_Event")

    nested(name) =
        FieldTypeDescription(TYPE_ID_NESTED_TYPE, UInt64(0), UInt64(0), String(name))
    # `<=1` bounded sequence of a nested type, the rosidl event-message shape.
    bseq1(name) =
        FieldTypeDescription(TYPE_ID_NESTED_TYPE + BOUNDED_SEQUENCE_OFFSET,
                             UInt64(1), UInt64(0), String(name))

    event = TypeDescription(event_name, FieldDescription[
        FieldDescription("info",     nested(sei_name)),
        FieldDescription("request",  bseq1(req_name)),
        FieldDescription("response", bseq1(resp_name)),
    ])
    main = TypeDescription(service_name, FieldDescription[
        FieldDescription("request_message",  nested(req_name)),
        FieldDescription("response_message", nested(resp_name)),
        FieldDescription("event_message",    nested(event_name)),
    ])

    # Deduplicate by type_name (each main + its refs), add the synthesized event,
    # then sort — RIHS01's cross-implementation-stable reference order.
    pool = Dict{String, TypeDescription}()
    for tdm in (request, response, service_event_info)
        pool[tdm.type_description.type_name] = tdm.type_description
        for r in tdm.referenced_type_descriptions
            pool[r.type_name] = r
        end
    end
    pool[event.type_name] = event
    refs = sort!(collect(values(pool)); by = t -> t.type_name)
    return TypeDescriptionMsg(main, refs)
end

"""
    service_rihs01(service_name, request, response, service_event_info) -> String

The `"RIHS01_<hex>"` service type hash — [`service_type_description`](@ref) fed to
[`calculate_rihs01_hash`](@ref).
"""
service_rihs01(service_name::AbstractString, request::TypeDescriptionMsg,
               response::TypeDescriptionMsg, service_event_info::TypeDescriptionMsg) =
    calculate_rihs01_hash(service_type_description(service_name, request, response,
                                                   service_event_info))

# ---- action-protocol service hashes (rmw_zenoh / hiroz) ---------------------
#
# A ROS2 action is three services on `<topic>/_action/{send_goal,get_result,
# cancel_goal}`. rmw_zenoh keys each off a SERVICE type hash computed from fixed,
# hardcoded request/response shapes under the `action` qualifier (the `is_action`
# path), matching the reference rmw_zenoh/hiroz codegen. The closures replicate
# that codegen exactly so our keyexpr matches a real ROS2 peer: SendGoal/GetResult
# carry the action Goal/Result main TD only (no transitive deps), and CancelGoal
# omits the `goals_canceling` field and the UUID dep entirely.
#
# The keyexpr *type name* differs from the hashed name for cancel_goal: it's
# published as `action_msgs/srv/CancelGoal` but the hash is computed over the
# `action`-path names below. Callers build the TypeInfo name separately.

# Fixed dependency type descriptions (standard ROS2 types, byte-exact encodings).
const _AP_UUID = TypeDescription("unique_identifier_msgs/msg/UUID",
    [FieldDescription("uuid", FieldTypeDescription(TYPE_ID_UINT8 + ARRAY_OFFSET, UInt64(16), UInt64(0), ""))])
const _AP_TIME = TypeDescription("builtin_interfaces/msg/Time",
    [FieldDescription("sec", FieldTypeDescription(TYPE_ID_INT32)),
     FieldDescription("nanosec", FieldTypeDescription(TYPE_ID_UINT32))])
const _AP_GOALINFO = TypeDescription("action_msgs/msg/GoalInfo",
    [FieldDescription("goal_id", FieldTypeDescription(TYPE_ID_NESTED_TYPE, UInt64(0), UInt64(0), "unique_identifier_msgs/msg/UUID")),
     FieldDescription("stamp", FieldTypeDescription(TYPE_ID_NESTED_TYPE, UInt64(0), UInt64(0), "builtin_interfaces/msg/Time"))])
const _AP_SEI = TypeDescription("service_msgs/msg/ServiceEventInfo",
    [FieldDescription("event_type", FieldTypeDescription(TYPE_ID_UINT8)),
     FieldDescription("stamp", FieldTypeDescription(TYPE_ID_NESTED_TYPE, UInt64(0), UInt64(0), "builtin_interfaces/msg/Time")),
     FieldDescription("client_gid", FieldTypeDescription(TYPE_ID_UINT8 + ARRAY_OFFSET, UInt64(16), UInt64(0), "")),
     FieldDescription("sequence_number", FieldTypeDescription(TYPE_ID_INT64))])
const _AP_SEI_MSG = TypeDescriptionMsg(_AP_SEI, TypeDescription[_AP_TIME])

_ap_nested(name) = FieldTypeDescription(TYPE_ID_NESTED_TYPE, UInt64(0), UInt64(0), String(name))

"""
    send_goal_service_rihs01(package, action_name, goal_td::TypeDescription) -> String

The action `send_goal` service RIHS01. `goal_td` is the action's `<Name>_Goal`
type description (main only — its transitive deps are excluded from the closure).
"""
function send_goal_service_rihs01(package::AbstractString, action_name::AbstractString,
                                  goal_td::TypeDescription)
    base = string(package, "/action/", action_name, "_SendGoal")
    req = TypeDescriptionMsg(
        TypeDescription(string(base, "_Request"),
            [FieldDescription("goal_id", _ap_nested("unique_identifier_msgs/msg/UUID")),
             FieldDescription("goal", _ap_nested(string(package, "/action/", action_name, "_Goal")))]),
        TypeDescription[_AP_UUID, goal_td])
    resp = TypeDescriptionMsg(
        TypeDescription(string(base, "_Response"),
            [FieldDescription("accepted", FieldTypeDescription(TYPE_ID_BOOLEAN)),
             FieldDescription("stamp", _ap_nested("builtin_interfaces/msg/Time"))]),
        TypeDescription[])
    return service_rihs01(base, req, resp, _AP_SEI_MSG)
end

"""
    get_result_service_rihs01(package, action_name, result_td::TypeDescription) -> String

The action `get_result` service RIHS01. `result_td` is the action's `<Name>_Result`
type description (main only).
"""
function get_result_service_rihs01(package::AbstractString, action_name::AbstractString,
                                   result_td::TypeDescription)
    base = string(package, "/action/", action_name, "_GetResult")
    req = TypeDescriptionMsg(
        TypeDescription(string(base, "_Request"),
            [FieldDescription("goal_id", _ap_nested("unique_identifier_msgs/msg/UUID"))]),
        TypeDescription[_AP_UUID])
    resp = TypeDescriptionMsg(
        TypeDescription(string(base, "_Response"),
            [FieldDescription("status", FieldTypeDescription(TYPE_ID_INT8)),
             FieldDescription("result", _ap_nested(string(package, "/action/", action_name, "_Result")))]),
        TypeDescription[result_td])
    return service_rihs01(base, req, resp, _AP_SEI_MSG)
end

"""
    cancel_goal_service_rihs01() -> String

The action `cancel_goal` (`action_msgs/srv/CancelGoal`) service RIHS01 — a constant
shared by every action. Matches the reference impl's incomplete description (the
response carries only `return_code`; the UUID dep is omitted).
"""
function cancel_goal_service_rihs01()
    base = "action_msgs/action/CancelGoal"
    req = TypeDescriptionMsg(
        TypeDescription(string(base, "_Request"),
            [FieldDescription("goal_info", _ap_nested("action_msgs/msg/GoalInfo"))]),
        TypeDescription[_AP_GOALINFO])
    resp = TypeDescriptionMsg(
        TypeDescription(string(base, "_Response"),
            [FieldDescription("return_code", FieldTypeDescription(TYPE_ID_INT8))]),
        TypeDescription[])
    return service_rihs01(base, req, resp, _AP_SEI_MSG)
end

# ---- TypeDescription → IL (inverse of the AST→TypeDescription encoding) -----
#
# Lifts the type-id-encoded `TypeDescription` back into the interface IL. The
# round trip is lossy in the ways the encoding itself is: constants and field
# defaults are dropped by RIHS01, `wchar` collapses to `char` (the IL has no
# wide-char base), and the package qualifier survives only inside nested-type
# refs — the lifted `RMessage` carries the bare struct name like the rest of
# the IL. The `fixed`-string ids never arise from a ROS2 interface and error.

# Parse a fully-qualified nested type name (`geometry_msgs/msg/Point`, or a
# bare `Point` relative ref) into an `RRef`, mirroring `_ref_to_base` in
# `ros2.jl`: a `pkg/<qual>/Name` keeps both the owning package and the qualifier
# (so `.../action/Name_Goal` survives), a bare name is a relative ref.
function _nested_name_to_ref(qualified::AbstractString)
    parts = split(qualified, '/')
    name = String(parts[end])
    path = parts[1:end-1]
    if isempty(path)
        return IL.RBase.RRef(nothing, name, "msg")
    elseif length(path) >= 2
        return IL.RBase.RRef(String(path[end-1]), name, String(path[end]))
    else
        return IL.RBase.RRef(String(path[1]), name, "msg")
    end
end

# Split a field's `type_id` into its primitive id and array modifier. The
# offsets don't overlap (primitives are 1..22), so a single descending cascade
# recovers which wrapper, if any, was applied; `capacity` is the element count
# for static arrays and bounded sequences and is ignored otherwise.
function _decode_array(type_id::UInt8, capacity::UInt64)
    if type_id > UNBOUNDED_SEQUENCE_OFFSET
        return (type_id - UNBOUNDED_SEQUENCE_OFFSET, IL.ArraySpec.AUnbounded())
    elseif type_id > BOUNDED_SEQUENCE_OFFSET
        return (type_id - BOUNDED_SEQUENCE_OFFSET, IL.ArraySpec.ABounded(Int(capacity)))
    elseif type_id > ARRAY_OFFSET
        return (type_id - ARRAY_OFFSET, IL.ArraySpec.AStatic(Int(capacity)))
    else
        return (type_id, IL.ArraySpec.AScalar())
    end
end

# Map a primitive type id (the array/sequence offset already stripped) back to
# an IL base type. `string_capacity` supplies the bound for bounded strings;
# `nested_type_name` the ref target for a nested type.
function _base_from_type_id(prim_id::UInt8, string_capacity::UInt64,
                            nested_type_name::AbstractString)
    if prim_id == TYPE_ID_NESTED_TYPE
        return _nested_name_to_ref(nested_type_name)
    elseif prim_id == TYPE_ID_INT8;   return IL.RBase.RInt(8)
    elseif prim_id == TYPE_ID_INT16;  return IL.RBase.RInt(16)
    elseif prim_id == TYPE_ID_INT32;  return IL.RBase.RInt(32)
    elseif prim_id == TYPE_ID_INT64;  return IL.RBase.RInt(64)
    elseif prim_id == TYPE_ID_UINT8;  return IL.RBase.RUInt(8)
    elseif prim_id == TYPE_ID_UINT16; return IL.RBase.RUInt(16)
    elseif prim_id == TYPE_ID_UINT32; return IL.RBase.RUInt(32)
    elseif prim_id == TYPE_ID_UINT64; return IL.RBase.RUInt(64)
    elseif prim_id == TYPE_ID_FLOAT;       return IL.RBase.RFloat(32)
    elseif prim_id == TYPE_ID_DOUBLE;      return IL.RBase.RFloat(64)
    elseif prim_id == TYPE_ID_LONG_DOUBLE; return IL.RBase.RFloat(128)
    elseif prim_id == TYPE_ID_CHAR;  return IL.RBase.RChar()
    # The IL has no wide-char base; `wchar` collapses to `char` as it does in
    # `ros2.jl`'s `_ts_to_base`.
    elseif prim_id == TYPE_ID_WCHAR;   return IL.RBase.RChar()
    elseif prim_id == TYPE_ID_BOOLEAN; return IL.RBase.RBool()
    elseif prim_id == TYPE_ID_BYTE;    return IL.RBase.RByte()
    elseif prim_id == TYPE_ID_STRING;          return IL.RBase.RStr(nothing)
    elseif prim_id == TYPE_ID_WSTRING;         return IL.RBase.RWStr(nothing)
    elseif prim_id == TYPE_ID_BOUNDED_STRING;  return IL.RBase.RStr(Int(string_capacity))
    elseif prim_id == TYPE_ID_BOUNDED_WSTRING; return IL.RBase.RWStr(Int(string_capacity))
    else
        error("unsupported RIHS01 type_id for IL lift: $(Int(prim_id))")
    end
end

function _lift_field_description(fd::FieldDescription)
    ft = fd.field_type
    prim_id, array = _decode_array(ft.type_id, ft.capacity)
    base = _base_from_type_id(prim_id, ft.string_capacity, ft.nested_type_name)
    return IL.RField(IL.RType(base, array), Symbol(fd.name), nothing)
end

"""
    lift(td::TypeDescription) -> IL.RMessage
    lift(msg::TypeDescriptionMsg) -> IL.RMessage

Reconstruct the interface IL from a RIHS01 `TypeDescription` — the inverse of
[`type_description_from_struct`](@ref). The `RMessage` name is the final
segment of the fully-qualified `type_name` (`std_msgs/msg/String` → `String`).
RIHS01 excludes constants and field defaults, so the result carries neither.
For a `TypeDescriptionMsg` the referenced descriptions are dropped: nested
types already survive as refs inside the main type's fields.
"""
function lift(td::TypeDescription)
    name = Symbol(split(td.type_name, '/')[end])
    fields = IL.RField[_lift_field_description(f) for f in td.fields]
    return IL.RMessage(name, IL.RConstant[], fields)
end

lift(msg::TypeDescriptionMsg) = lift(msg.type_description)
