# §10 Parameters. Declared parameters are typed and baked into the node: a
# `@parameters` macro generates an immutable struct (type-stable reads) plus a
# `descriptors(::Type)` method, and the live value sits behind an atomic `Ref{P}`
# inside a `ParameterServer{P}`. Mutation is transactional — `transaction(server)
# do p … end` builds a mutable draft, validates the whole candidate, swaps once,
# and fires one `/parameter_events` batch. Single-statement sugar (`server.x = v`,
# `setproperties!(server, …)`) routes through the same commit path. Undeclared
# params live in a separate `dynamic_parameters` dict, gated by `allow_undeclared`.
#
# The six standard parameter services + `/parameter_events` are generic over `P`
# via reflection (`fieldnames`/`fieldtypes` + `descriptors()`); they reflect over
# the union of declared fields and the dynamic dict so a ROS client sees one flat
# namespace. The reflection handlers return plain Julia values; the wire binding
# (`wire_parameter_services!`) marshals them to/from the `rcl_interfaces` generated
# request/response types over the §8 Service layer.

import Dates

export @parameters, ParameterServer, ParameterClient, ParameterDescriptor, ParameterType,
       descriptors, validate, transaction, setproperties, setproperties!,
       dynamic_parameters, declared_names, parameter_names, parameter, set_parameter!,
       get_parameters, get_parameter_types, set_parameters, set_parameters_atomically,
       list_parameters, describe_parameters,
       on_parameter_event, readonly, ParameterRejection

# ── legal value types (§10) ───────────────────────────────────────────────────
# The closed set ROS2's `ParameterValue` tagged union can carry: bool, int64,
# float64, string, byte arrays, and `Vector` of {bool,int64,float64,string}.
# `Symbol` is accepted as sugar over string-with-choices. An illegal type is
# rejected by `parameter_type` (below), which the generated `descriptors` body
# calls — so the rejection lands when descriptors are first built, not at expansion.

const _SCALAR_PARAM_TYPES = (Bool, Int64, Float64, String, Symbol)
const _ARRAY_ELT_TYPES    = (Bool, Int64, Float64, String, UInt8)

# Supertype of every node-level parameter surface: the single-schema
# `ParameterServer{P}` (server.jl) and the multi-schema `CompositeParameterServer`
# façade (composite.jl, §4.4). `wire_parameter_services!` and `node.parameters`
# are written against this so a composed node exposes one member-prefixed `ros2
# param` namespace over its members' servers.
abstract type AbstractParameterServer end

"""
    ParameterType

The `rcl_interfaces/msg/ParameterType` tag enum (§10), `UInt8`-backed so it
marshals straight onto the wire `type` byte. `PARAMETER_NOT_SET` (0) is the
unset/unknown sentinel — never stored for a declared field, and what an unknown
parameter name or unrecognized wire tag reads back as. The remaining arms mirror
ROS 2's `ParameterValue` union: `PARAMETER_BOOL`, `PARAMETER_INTEGER`,
`PARAMETER_DOUBLE`, `PARAMETER_STRING`, `PARAMETER_BYTE_ARRAY`, and the four
`*_ARRAY` arms.

[`get_parameter_types`](@ref) returns these, and a [`ParameterDescriptor`](@ref)
carries one as its `ptype`.

See the ROS 2 parameter concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html
"""
@enum ParameterType::UInt8 begin
    PARAMETER_NOT_SET       = 0
    PARAMETER_BOOL          = 1
    PARAMETER_INTEGER       = 2
    PARAMETER_DOUBLE        = 3
    PARAMETER_STRING        = 4
    PARAMETER_BYTE_ARRAY    = 5
    PARAMETER_BOOL_ARRAY    = 6
    PARAMETER_INTEGER_ARRAY = 7
    PARAMETER_DOUBLE_ARRAY  = 8
    PARAMETER_STRING_ARRAY  = 9
end

# A wire `type` byte → `ParameterType`, guarding an out-of-range tag back to
# NOT_SET (a peer sending an unknown arm reads as unset, never throws).
_ptype(t::Integer) = (0 <= t <= 9) ? ParameterType(t) : PARAMETER_NOT_SET
_ptype(t::ParameterType) = t

# The Julia value type behind a `ParameterType` arm — the inverse of
# `parameter_type`, used to fill in a client-side `ParameterDescriptor.type` and to
# coerce a dynamically-typed remote value.
function _param_julia_type(t::ParameterType)
    t === PARAMETER_BOOL          && return Bool
    t === PARAMETER_INTEGER       && return Int64
    t === PARAMETER_DOUBLE        && return Float64
    t === PARAMETER_STRING        && return String
    t === PARAMETER_BYTE_ARRAY    && return Vector{UInt8}
    t === PARAMETER_BOOL_ARRAY    && return Vector{Bool}
    t === PARAMETER_INTEGER_ARRAY && return Vector{Int64}
    t === PARAMETER_DOUBLE_ARRAY  && return Vector{Float64}
    t === PARAMETER_STRING_ARRAY  && return Vector{String}
    return Nothing
end

"""
    parameter_type(::Type) -> ParameterType

The `rcl_interfaces/msg/ParameterType` tag for a declared field type. `Symbol`
maps to STRING (string-with-choices sugar). Throws `ArgumentError` for anything
outside the legal value set; the generated [`descriptors`](@ref) body calls
this, so an illegal field type surfaces when descriptors are first built.
"""
function parameter_type(::Type{T}) where {T}
    T === Bool            && return PARAMETER_BOOL
    T === Int64           && return PARAMETER_INTEGER
    T === Float64         && return PARAMETER_DOUBLE
    (T === String || T === Symbol) && return PARAMETER_STRING
    T === Vector{UInt8}   && return PARAMETER_BYTE_ARRAY
    T === Vector{Bool}    && return PARAMETER_BOOL_ARRAY
    T === Vector{Int64}   && return PARAMETER_INTEGER_ARRAY
    T === Vector{Float64} && return PARAMETER_DOUBLE_ARRAY
    T === Vector{String}  && return PARAMETER_STRING_ARRAY
    throw(ArgumentError("not a legal parameter type: $(T)"))
end

# Boolean form of the legal-type check. Currently unused — type validation runs
# through `parameter_type`'s throw inside the generated `descriptors` body.
_is_legal_param_type(::Type{T}) where {T} =
    (T in _SCALAR_PARAM_TYPES) ||
    (T <: Vector && eltype(T) in _ARRAY_ELT_TYPES)

# ── descriptors (§10) ───────────────────────────────────────────────────────────

"""
    ParameterDescriptor

Per-field metadata for one declared parameter (ROS 2
`rcl_interfaces/msg/ParameterDescriptor`, §10): its `name::Symbol`, Julia `type`,
[`ParameterType`](@ref) tag `ptype`, human `description`, optional `constraint`,
`read_only` flag, and schema `default`. Built by [`@parameters`](@ref) and
reflected over by the parameter services (`describe_parameters`,
`get_parameter_types`, …).

`constraint` is one of: `nothing` (no bound), a numeric `(lo, hi)` range tuple,
or a tuple of allowed values (the `∈ choices` form). The tuple is untagged, so a
choice set of exactly two numbers is indistinguishable from a range and is
validated as one. On the wire the structured constraint travels only as the
human `additional_constraints` string and no default travels at all, so a
descriptor decoded by a remote [`ParameterClient`](@ref) carries both
`constraint === nothing` and `default === nothing`.
"""
struct ParameterDescriptor
    name::Symbol
    type::Type
    ptype::ParameterType
    description::String
    constraint::Any        # nothing | (lo, hi)::Tuple | choices::Tuple
    read_only::Bool
    default::Any
end

Base.show(io::IO, d::ParameterDescriptor) =
    print(io, "ParameterDescriptor(", d.name, "::", d.type,
          d.read_only ? ", readonly" : "", ")")

# Validate a candidate value against a descriptor's constraint. Returns `nothing`
# on success, or a human reason string on violation (the wire `reason` field).
function _check_constraint(d::ParameterDescriptor, value)
    c = d.constraint
    c === nothing && return nothing
    if c isa Tuple && length(c) == 2 && c[1] isa Number && c[2] isa Number
        lo, hi = c
        (value < lo || value > hi) &&
            return "value $(value) out of range [$(lo), $(hi)]"
    elseif c isa Tuple
        value in c ||
            return "value $(repr(value)) not in allowed set $(c)"
    end
    return nothing
end

# ── readonly marker (§10) ───────────────────────────────────────────────────────
# `field = default |> readonly` tags a field read-only. The macro detects the pipe
# syntactically at parse time (`_parse_param_field`) and strips it, so the stored
# default is the bare value. `readonly`/`_ReadOnly` exist only as a harmless
# runtime identity so a stray `readonly(x)` outside the DSL still evaluates.

struct _ReadOnly
    value::Any
end

"""
    readonly(default)

Mark a [`@parameters`](@ref) field read-only inside the schema DSL:
`field::T = default |> readonly` (§10). Read-only blocks runtime sets — a
runtime set raises [`ParameterRejection`](@ref) internally and returns a
rejected `SetParametersResult` on the wire — while still permitting a startup
override (CLI/launch/YAML), which goes through the constructor rather than a
transaction. The marker is detected and stripped at macro expansion, so the
stored default is the bare value; a stray `readonly(x)` evaluated outside the
DSL just wraps `x` in an inert holder.
"""
readonly(x) = _ReadOnly(x)

