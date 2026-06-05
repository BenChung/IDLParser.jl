# §10 Parameters. Declared parameters are typed and baked into the node: a
# `@parameters` macro generates an immutable struct (type-stable reads) plus a
# `descriptors(::Type)` method, and the live value sits behind an atomic `Ref{P}`
# inside a `ParameterServer{P}`. Mutation is transactional — `transaction(server)
# do p … end` builds a mutable draft, validates the *whole* candidate, swaps once,
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

export @parameters, ParameterServer, ParameterDescriptor,
       descriptors, validate, transaction, setproperties, setproperties!,
       dynamic_parameters, declared_names, parameter_names, parameter, set_parameter!,
       on_parameter_event, readonly, ParameterRejection

# ── legal value types (§10) ───────────────────────────────────────────────────
# The closed set ROS2's `ParameterValue` tagged union can carry: bool, int64,
# float64, string, byte arrays, and `Vector` of {bool,int64,float64,string}.
# `Symbol` is accepted as sugar over string-with-choices. Anything else is
# rejected at macro expansion (below) rather than silently coerced on the wire.

const _SCALAR_PARAM_TYPES = (Bool, Int64, Float64, String, Symbol)
const _ARRAY_ELT_TYPES    = (Bool, Int64, Float64, String, UInt8)

# Map a Julia field type to the ROS2 `ParameterType` tag (rcl_interfaces values).
# 0 = NOT_SET; we never store NOT_SET for a declared field.
const PARAMETER_NOT_SET        = UInt8(0)
const PARAMETER_BOOL           = UInt8(1)
const PARAMETER_INTEGER        = UInt8(2)
const PARAMETER_DOUBLE         = UInt8(3)
const PARAMETER_STRING         = UInt8(4)
const PARAMETER_BYTE_ARRAY     = UInt8(5)
const PARAMETER_BOOL_ARRAY     = UInt8(6)
const PARAMETER_INTEGER_ARRAY  = UInt8(7)
const PARAMETER_DOUBLE_ARRAY   = UInt8(8)
const PARAMETER_STRING_ARRAY   = UInt8(9)

"""
    parameter_type(::Type) -> UInt8

The `rcl_interfaces/msg/ParameterType` tag for a declared field type. `Symbol`
maps to STRING (it is string-with-choices sugar). Throws for anything outside the
legal value set — the same check the macro applies at expansion time.
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

# Predicate form used by the macro to validate field types up front.
_is_legal_param_type(::Type{T}) where {T} =
    (T in _SCALAR_PARAM_TYPES) ||
    (T <: Vector && eltype(T) in _ARRAY_ELT_TYPES)

# ── descriptors (§10) ───────────────────────────────────────────────────────────
# One per declared field. `constraint` is `nothing` (no bound), a 2-tuple
# `(lo, hi)` numeric range, or a tuple of allowed values (the `∈ choices` form);
# `validate_value` checks a candidate field value against it. `read_only` blocks
# *runtime* sets (a startup override is still allowed, §10).

"""
    ParameterDescriptor

Per-field metadata for a declared parameter (§10): its `name`, Julia `type`, ROS2
`ptype` tag, human `description`, optional `constraint`, `read_only` flag, and the
schema `default`. Built by [`@parameters`](@ref); reflected over by the six
parameter services (`describe`/`get_types`/…). `constraint` is `nothing`, a
numeric `(lo, hi)` range, or a tuple of allowed values (`∈ choices`).
"""
struct ParameterDescriptor
    name::Symbol
    type::Type
    ptype::UInt8
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
# *syntactically* at parse time (`_parse_param_field`) and strips it — the default
# is never wrapped at runtime. `readonly`/`_ReadOnly` exist only as a harmless
# runtime identity so a stray `readonly(x)` outside the DSL doesn't error.

struct _ReadOnly
    value::Any
end

"""
    readonly(default)

Mark a `@parameters` field read-only: `field::T = default |> readonly`. Read-only
blocks *runtime* sets (an exception internally; a rejected `SetParametersResult`
on the wire), but a startup override (CLI/launch/YAML) is still allowed (§10).
The marker is consumed at macro expansion, so the stored default is the bare value.
"""
readonly(x) = _ReadOnly(x)

