# Conformance: does a concrete decoded type satisfy a mapper's required shape?
# (DESIGN-MAPPING.md §8.4, §12.4). Two paths share the kind vocabulary:
#   • `fits(FieldShape, T)`  — fast reflection bool, used for StructMatch SELECTION.
#   • `conform(FieldShape, RMessage|T)` — the precise IL-vs-IL checker with diagnostics,
#     used to VALIDATE a bound mapper's `requires` (§12.4); pure, unit-testable offline.

using Moshi.Match: @match
using ROSMessages: IL, il_from_type

# ── fits: FieldShape vs a concrete Julia type ─────────────────────────────────
"""`true` iff every required field of `shape` is present in `T` with a compatible kind."""
function fits(shape::FieldShape, @nospecialize(T::Type))::Bool
    (isconcretetype(T) && isstructtype(T)) || return false
    for fr in shape.fields
        if !hasfield(T, fr.name)
            fr.optional || return false
            continue
        end
        _kind_fits(fr.kind, fieldtype(T, fr.name)) || return false
    end
    if !shape.allow_extra
        names = (fr.name for fr in shape.fields)
        all(n -> n in names, fieldnames(T)) || return false
    end
    return true
end

_kind_fits(k::KScalar, @nospecialize(FT)) = _scalar_fits(k.sk, FT)
_kind_fits(k::KNested, @nospecialize(FT)) = fits(k.shape, FT)
function _kind_fits(k::KArray, @nospecialize(FT))
    FT <: AbstractVector || return false
    return _kind_fits(k.elem, eltype(FT))     # exact fixed-length check deferred to RMessage conform
end

function _scalar_fits(sk::ScalarKind, @nospecialize(FT))
    sk === SAny    && return true
    sk === SBool   && return FT === Bool
    sk === SChar   && return FT === Char
    sk === SString && return FT <: AbstractString
    sk === SFloat  && return FT <: AbstractFloat
    sk === SInt    && return FT <: Signed
    sk === SUInt   && return FT <: Unsigned
    sk === SReal   && return FT <: Real && FT !== Bool
    return false
end

# ── requirement: derive the shape a BSpec implicitly demands ───────────────────
const _LeafReq = Tuple{Path,Kind}

"""The shape the checker validates: explicit for `BFn`, auto-derived for `BSpec` from
the field paths its emission reads (DESIGN-MAPPING.md §12.2.4)."""
requirement(::BFn) = nothing
requirement(b::BSpec) = _build_shape(_emit_reqs(b.emit))

# Each Source contributes leaf paths required to have `leafkind`.
_src_reqs(s::SrcPath,  leafkind::Kind) = _LeafReq[(s.path, leafkind)]
_src_reqs(s::SrcTuple, leafkind::Kind) = reduce(vcat, (_src_reqs(p, leafkind) for p in s.parts); init=_LeafReq[])
_src_reqs(s::SrcList,  leafkind::Kind) = reduce(vcat, (_src_reqs(p, leafkind) for p in s.items); init=_LeafReq[])
_src_reqs(::SrcLit,    ::Kind)         = _LeafReq[]

_rot_src(r::RQuat)      = r.src
_rot_src(r::RAxisAngle) = r.src
_rot_src(r::RMat3x3)    = r.src

_emit_reqs(e::EScalars)     = _src_reqs(e.value, KScalar(SReal))
_emit_reqs(e::EScalarGroup) = reduce(vcat, (_src_reqs(v, KScalar(SReal)) for (_, v) in e.series); init=_LeafReq[])
_emit_reqs(e::ETextLog)     = vcat(_src_reqs(e.text, KScalar(SString)),
                                   e.level === nothing ? _LeafReq[] : _src_reqs(e.level, KScalar(SString)))
_emit_reqs(e::EPoints2D)    = _pointset_reqs(e.positions)
_emit_reqs(e::EPoints3D)    = _pointset_reqs(e.positions)
_emit_reqs(e::ETransform3D) = vcat(
    e.translation === nothing ? _LeafReq[] : _src_reqs(e.translation, KScalar(SReal)),
    e.rotation    === nothing ? _LeafReq[] : _src_reqs(_rot_src(e.rotation), KScalar(SReal)),
    e.scale       === nothing ? _LeafReq[] : _src_reqs(e.scale, KScalar(SReal)))

_pointset_reqs(p::PFixed) = reduce(vcat, (_src_reqs(pt, KScalar(SReal)) for pt in p.points); init=_LeafReq[])
_pointset_reqs(p::PEach)  =
    _LeafReq[(p.array, KArray(KNested(_build_shape(_src_reqs(p.point, KScalar(SReal)))), ASeq()))]

# Merge (path, leafkind) pairs into a nested FieldShape tree.
function _build_shape(reqs::Vector{_LeafReq})
    leaves = Dict{Symbol,Kind}()
    nested = Dict{Symbol,Vector{_LeafReq}}()
    for (path, kind) in reqs
        if length(path) == 1
            leaves[path[1]] = kind
        else
            push!(get!(nested, path[1], _LeafReq[]), (path[2:end], kind))
        end
    end
    fields = FieldReq[]
    for (seg, k) in leaves
        push!(fields, FieldReq(seg, k, false))
    end
    for (seg, sub) in nested
        haskey(leaves, seg) && continue        # a leaf wins over an inferred nesting on the same name
        push!(fields, FieldReq(seg, KNested(_build_shape(sub)), false))
    end
    return FieldShape(fields, true)
end

# ── conform: precise IL-vs-IL check (DESIGN-MAPPING.md §12.4) ──────────────────
"""One conformance failure: the dotted field path, what the shape wanted, what the type has."""
struct Mismatch
    path::String
    expected::String
    actual::String
end
Base.show(io::IO, m::Mismatch) = print(io, m.path, ": expected ", m.expected, ", got ", m.actual)

struct ConformResult
    ok::Bool
    mismatches::Vector{Mismatch}
end
ConformResult(ms::Vector{Mismatch}) = ConformResult(isempty(ms), ms)

# A ScalarKind accepts these ROS bases (§12.4 table; RByte→uint, RWStr→string).
_base_ok(sk::ScalarKind, base::IL.RBase.Type) = sk === SAny || @match base begin
    IL.RBase.RBool()       => sk === SBool
    IL.RBase.RChar()       => sk === SChar
    IL.RBase.RByte()       => sk in (SUInt, SInt, SReal)
    IL.RBase.RInt(_)       => sk in (SInt, SReal)
    IL.RBase.RUInt(_)      => sk in (SUInt, SReal)
    IL.RBase.RFloat(_)     => sk in (SFloat, SReal)
    IL.RBase.RStr(_)       => sk === SString
    IL.RBase.RWStr(_)      => sk === SString
    IL.RBase.RRef(_, _, _) => false
end
_is_scalar(a::IL.ArraySpec.Type) = @match a begin (IL.ArraySpec.AScalar()) => true; _ => false end
_is_ref(base::IL.RBase.Type)     = @match base begin (IL.RBase.RRef(_, _, _)) => true; _ => false end
_bound_ok(b::ASeq, a::IL.ArraySpec.Type) =
    @match a begin (IL.ArraySpec.AUnbounded()) => true; (IL.ArraySpec.ABounded(_)) => true; _ => false end
_bound_ok(b::AFixed, a::IL.ArraySpec.Type) =
    @match a begin IL.ArraySpec.AStatic(n) => n == b.n; _ => false end
_il_str(x) = (io = IOBuffer(); IL.unparse(io, x); String(take!(io)))

"""
    conform(shape, t::RMessage, resolve) -> ConformResult

Does ROS type `t` satisfy `shape`? `resolve(rref::IL.RBase.Type) -> Union{RMessage,Nothing}`
supplies the nested message for a `KNested` field. Pure IL→IL — unit-testable offline.
"""
function conform(shape::FieldShape, t::IL.RMessage, resolve)::ConformResult
    out = Mismatch[]
    _conform!(out, "", shape, t, resolve)
    return ConformResult(out)
end

"""`conform` against a concrete Julia type (builds the `RMessage` + a nested-type resolver)."""
function conform(shape::FieldShape, @nospecialize(T::Type))::ConformResult
    (isconcretetype(T) && isstructtype(T)) ||
        return ConformResult(Mismatch[Mismatch("", "a message struct", string(T))])
    return conform(shape, il_from_type(T), _type_resolver(T))
end

function _conform!(out, prefix, s::FieldShape, t::IL.RMessage, resolve)
    byname = Dict(rf.name => rf for rf in t.fields)
    for fr in s.fields
        p = isempty(prefix) ? string(fr.name) : string(prefix, ".", fr.name)
        rf = get(byname, fr.name, nothing)
        if rf === nothing
            fr.optional || push!(out, Mismatch(p, "field", "missing"))
            continue
        end
        _check_kind!(out, p, fr.kind, rf.type, resolve)
    end
    if !s.allow_extra
        names = Set(fr.name for fr in s.fields)
        for rf in t.fields
            rf.name in names || push!(out, Mismatch(prefix, "no extra fields", "extra `$(rf.name)`"))
        end
    end
    return nothing
end

function _check_kind!(out, p, k::KScalar, rt::IL.RType, resolve)
    _is_scalar(rt.array) || (push!(out, Mismatch(p, "scalar", _il_str(rt.array))); return)
    _base_ok(k.sk, rt.base) || push!(out, Mismatch(p, lowercase(string(k.sk)[2:end]), _il_str(rt.base)))
    return nothing
end
function _check_kind!(out, p, k::KArray, rt::IL.RType, resolve)
    _bound_ok(k.bound, rt.array) || (push!(out, Mismatch(p, "array", _il_str(rt.array))); return)
    _check_kind!(out, p * "[]", k.elem, IL.RType(rt.base, IL.ArraySpec.AScalar()), resolve)
    return nothing
end
function _check_kind!(out, p, k::KNested, rt::IL.RType, resolve)
    _is_scalar(rt.array) || (push!(out, Mismatch(p, "nested message", _il_str(rt.array))); return)
    _is_ref(rt.base) || (push!(out, Mismatch(p, "nested message", _il_str(rt.base))); return)
    nt = resolve(rt.base)
    nt === nothing ? push!(out, Mismatch(p, "resolvable nested message", "unresolved `$(_il_str(rt.base))`")) :
                     _conform!(out, p, k.shape, nt, resolve)
    return nothing
end

# Build a RRef → nested-RMessage resolver by reflecting `T`'s nested struct fields.
function _type_resolver(@nospecialize(T))
    d = Dict{IL.RBase.Type,IL.RMessage}()
    _walk_resolver!(d, T)
    return rref -> get(d, rref, nothing)
end
function _walk_resolver!(d, @nospecialize(T))
    (isstructtype(T) && isconcretetype(T)) || return
    for rf in il_from_type(T).fields
        _is_ref(rf.type.base) || continue
        haskey(d, rf.type.base) && continue
        hasfield(T, rf.name) || continue
        FT = fieldtype(T, rf.name)
        ET = FT <: AbstractArray ? eltype(FT) : FT
        (isstructtype(ET) && isconcretetype(ET)) || continue
        d[rf.type.base] = il_from_type(ET)
        _walk_resolver!(d, ET)
    end
    return nothing
end
