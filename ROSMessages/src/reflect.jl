# Julia type → IL. The one genuinely new ROSMessages translation: everything
# downstream of the IL (lower → TypeDescription → RIHS01, generation, unparse) is
# already implemented and golden-tested. Symmetric with `lift`/`lower`/`unparse`.
#
# This module has no registry and no `ros_type_name` (that lives in ROSNode), so
# nested-struct ROS names come from a `name_of` callback. The default replicates the
# `<pkg>.<qualifier>.Name` module nesting but *throws* off-shape rather than guessing —
# a wrong RRef would silently produce a wrong RIHS.

using StaticArrays: StaticArray

# Fully-qualified ROS name of a *nested* struct from its module nesting
# (`Pkg.qual.Name` → "Pkg/qual/Name"). Throws when not `<pkg>.<qualifier>.Name`-deep
# (unlike `ros_type_name`'s silent bare-name fallback) — the ROSNode-supplied `name_of`
# resolves through the registry and likewise errors on an unregistered nested type.
function _default_name_of(@nospecialize(S))
    q = parentmodule(S)          # qualifier module (msg/srv/action)
    p = parentmodule(q)          # package module
    (q === Main || p === Main || p === q) && error(
        "il reflection: $(S) is not nested as `<pkg>.<qualifier>.Name` (module $(q)); \
         pass an explicit `name_of` or register the type first")
    return string(nameof(p), '/', nameof(q), '/', nameof(S))
end

# Element (base) type map — the left column of the v1 table, sans array modifier.
# A `UInt8` maps to `uint8` (never `byte`); `Char`/`Dict`/abstract/etc. have no ROS
# image and error. A concrete nested struct becomes a fully-packaged `RRef` so the
# RIHS is correct regardless of where it's lowered.
function _rbase(@nospecialize(E::Type); name_of)
    E === Bool    && return IL.RBase.RBool()
    E === Int8    && return IL.RBase.RInt(8)
    E === Int16   && return IL.RBase.RInt(16)
    E === Int32   && return IL.RBase.RInt(32)
    E === Int64   && return IL.RBase.RInt(64)
    E === UInt8   && return IL.RBase.RUInt(8)
    E === UInt16  && return IL.RBase.RUInt(16)
    E === UInt32  && return IL.RBase.RUInt(32)
    E === UInt64  && return IL.RBase.RUInt(64)
    E === Float32 && return IL.RBase.RFloat(32)
    E === Float64 && return IL.RBase.RFloat(64)
    E === String  && return IL.RBase.RStr(nothing)
    (E <: AbstractArray || E <: Tuple) && error(
        "il reflection: element type $(E) is itself an array/tuple — nested arrays are not a ROS type")
    if isconcretetype(E) && isstructtype(E)
        fqn = name_of(E)
        parts = split(fqn, '/')
        length(parts) >= 2 || error(
            "il reflection: name_of($(E)) must return a qualified ROS name like \"pkg/msg/Name\", got \"$(fqn)\"")
        # Keep the qualifier so a ref to a non-msg type (e.g. an action wrapper's
        # `_Goal` section) lowers to `pkg/<qual>/Name` rather than a hardcoded `msg`.
        qual = length(parts) >= 3 ? String(parts[end-1]) : "msg"
        return IL.RBase.RRef(String(parts[1]), String(parts[end]), qual)
    end
    error("il reflection: unsupported field type $(E) (Char/Dict/abstract/non-concrete have no ROS mapping)")
end

# Full field type: base + array modifier. `Vector{E}` → unbounded; an `SVector`/
# `SArray`/`NTuple{N,E}` → static `[N]`; anything else is a scalar. The element map
# and the array-wrapper map compose, so arrays-of-messages fall out for free.
function _rtype(@nospecialize(T::Type); name_of)
    if T <: StaticArray                       # SVector{N,E}, SArray{Tuple{N},E}
        return IL.RType(_rbase(eltype(T); name_of=name_of), IL.ArraySpec.AStatic(length(T)))
    elseif T <: AbstractVector                # Vector{E} (dynamic)
        return IL.RType(_rbase(eltype(T); name_of=name_of), IL.ArraySpec.AUnbounded())
    elseif T <: Tuple && isconcretetype(T)    # NTuple{N,E} — homogeneous only
        ets = fieldtypes(T)
        isempty(ets) && error("il reflection: empty tuple field has no ROS mapping")
        E = first(ets)
        all(==(E), ets) || error("il reflection: only homogeneous NTuple{N,E} is supported, got $(T)")
        return IL.RType(_rbase(E; name_of=name_of), IL.ArraySpec.AStatic(length(ets)))
    else
        return IL.RType(_rbase(T; name_of=name_of), IL.ArraySpec.AScalar())
    end
end

"""
    il_from_type(::Type{T}; name_of=_default_name_of) -> IL.RMessage

Reflect a concrete Julia struct into an `IL.RMessage` (fields in declaration order =
RIHS field order). The message name is the bare struct name; the package flows in later
through `lower(il; package=…)` and the `RRef`s `name_of` resolves. Errors on a
`@NamedTuple` (use [`il_from_fields`](@ref)) or a non-concrete/non-struct type.
"""
function il_from_type(::Type{T}; name_of=_default_name_of) where {T}
    nameof(T) === :NamedTuple &&
        error("il_from_type does not accept a @NamedTuple (its name is :NamedTuple); use il_from_fields")
    (isconcretetype(T) && isstructtype(T)) ||
        error("il_from_type expects a concrete struct type, got $(T)")
    fields = IL.RField[IL.RField(_rtype(ty; name_of=name_of), nm, nothing)
                       for (nm, ty) in zip(fieldnames(T), fieldtypes(T))]
    # An empty authored struct gets the rosidl synthetic member (shared convention),
    # so its RIHS + wire form match the parse path and a real ROS2 peer.
    return IL.RMessage(Symbol(nameof(T)), IL.RConstant[], IL.nonempty_fields(fields))
end

"""
    il_from_fields(name, pairs; name_of=_default_name_of) -> IL.RMessage

Build an `IL.RMessage` named `name` from an ordered list of `(field_name, field_type)`
pairs — the signature path for service/action sections, whose `@NamedTuple`s can't name
themselves. `name` must already be the suffixed section name (`"SetMode_Request"`,
`"FlyTo_Goal"`, …). Same per-field map as [`il_from_type`](@ref).
"""
function il_from_fields(name::AbstractString, pairs; name_of=_default_name_of)
    fields = IL.RField[IL.RField(_rtype(ty; name_of=name_of), Symbol(nm), nothing)
                       for (nm, ty) in pairs]
    # An empty section (e.g. an empty `@NamedTuple{}` service response) gets the rosidl
    # synthetic member (shared convention), matching the parse path and a real peer.
    return IL.RMessage(Symbol(name), IL.RConstant[], IL.nonempty_fields(fields))
end

"""
    nt_pairs(::Type{<:NamedTuple}) -> Vector{Tuple{Symbol,Type}}

The ordered `(name, type)` pairs of a `@NamedTuple` *type*, for feeding [`il_from_fields`](@ref).
"""
nt_pairs(::Type{NT}) where {NT<:NamedTuple} =
    Tuple{Symbol,Type}[(n, t) for (n, t) in zip(fieldnames(NT), fieldtypes(NT))]

"""
    il_service_from_fields(name, req_pairs, resp_pairs; name_of) -> IL.RService

Assemble an `IL.RService` named `name` with sections `<name>_Request` / `<name>_Response`
built from `(field_name, field_type)` pair lists (matching `service_il`'s naming).
"""
il_service_from_fields(name::AbstractString, req, resp; name_of=_default_name_of) =
    IL.RService(Symbol(name),
                il_from_fields(string(name, "_Request"),  req;  name_of=name_of),
                il_from_fields(string(name, "_Response"), resp; name_of=name_of))

"""
    il_action_from_fields(name, goal, result, feedback; name_of) -> IL.RAction

Assemble an `IL.RAction` named `name` with sections `<name>_Goal`/`_Result`/`_Feedback`
(matching `action_il`'s naming). The implicit protocol wrappers and namespace aliases are
*not* in the IL — the ROSNode generation path appends `action_protocol_decls`.
"""
il_action_from_fields(name::AbstractString, goal, result, feedback; name_of=_default_name_of) =
    IL.RAction(Symbol(name),
               il_from_fields(string(name, "_Goal"),     goal;     name_of=name_of),
               il_from_fields(string(name, "_Result"),   result;   name_of=name_of),
               il_from_fields(string(name, "_Feedback"), feedback; name_of=name_of))

"""
    il_field_diff(a::IL.RMessage, b::IL.RMessage) -> Vector{NamedTuple}

Node-free structural diff of two messages, by field name: `:removed` (in `a` not `b`),
`:added` (in `b` not `a`), `:changed` (same name, different `RType`). Each entry renders the
`RType`s via `IL.unparse` (`a`/`b` strings, `nothing` where absent). An empty diff ⇒
structurally identical ⇒ same RIHS. The overlap guard renders these into its error.
"""
function il_field_diff(a::IL.RMessage, b::IL.RMessage)
    bt = Dict(f.name => f.type for f in b.fields)
    at = Dict(f.name => f.type for f in a.fields)
    out = NamedTuple[]
    for f in a.fields
        if !haskey(bt, f.name)
            push!(out, (name=f.name, kind=:removed, a=IL.unparse(f.type), b=nothing))
        elseif IL.unparse(f.type) != IL.unparse(bt[f.name])
            push!(out, (name=f.name, kind=:changed, a=IL.unparse(f.type), b=IL.unparse(bt[f.name])))
        end
    end
    for f in b.fields
        haskey(at, f.name) || push!(out, (name=f.name, kind=:added, a=nothing, b=IL.unparse(f.type)))
    end
    return out
end

# ── client-side @NamedTuple ↔ struct (ONE level; embedded messages stay structs) ──

"""
    struct_from_nt(::Type{T}, nt::NamedTuple) -> T

Build `T` from a `@NamedTuple` by field *name* (order-independent), one level deep — an
embedded message field is passed as its struct, not a nested NamedTuple. Errors if a field
is missing.
"""
struct_from_nt(::Type{T}, nt::NamedTuple) where {T} =
    T((getfield(nt, n) for n in fieldnames(T))...)

"""
    nt_from_struct(x) -> NamedTuple

A decoded struct as a `@NamedTuple`, one level deep — embedded message fields stay as their
structs (they're named types; the `@NamedTuple` only names the extrinsically-unnamed section).
"""
nt_from_struct(x) =
    NamedTuple{fieldnames(typeof(x))}(ntuple(i -> getfield(x, i), fieldcount(typeof(x))))
