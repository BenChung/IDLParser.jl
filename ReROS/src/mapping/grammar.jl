# Julia DSL surface → IL (DESIGN-MAPPING.md §12.1/§12.3). `shape(...)` builds a
# FieldShape; `archetype(...)` builds an ArchetypeIL. (TOML lowering: Phase 6.)

const _SCALAR_TOKENS = Dict(
    :bool => SBool, :int => SInt, :uint => SUInt, :float => SFloat,
    :real => SReal, :string => SString, :char => SChar, :any => SAny,
)

_scalar(tok::Symbol) = KScalar(get(() -> error("ReROS: unknown kind token :$tok"), _SCALAR_TOKENS, tok))

# Kind from a DSL value: a scalar token (Symbol), a nested FieldShape, or an array
# Expr `:(float[])` (sequence) / `:(float[3])` (fixed).
_kind(v::Symbol)     = _scalar(v)
_kind(v::FieldShape) = KNested(v)
_kind(v::Kind)       = v
function _kind(v::Expr)
    v.head === :ref || error("ReROS: bad kind expr $v (expected T[] or T[N])")
    elem = _kind(v.args[1])
    bound = length(v.args) == 1 ? ASeq() : AFixed(Int(v.args[2]))
    return KArray(elem, bound)
end

"""
    shape(; field = kind, ...) -> FieldShape

`kind` is a scalar token (`:float`/`:int`/`:uint`/`:real`/`:bool`/`:string`/`:char`/`:any`),
a nested `shape(...)`, or an array expr `:(float[])` / `:(float[3])`.
"""
function shape(; kwargs...)
    reqs = FieldReq[FieldReq(name, _kind(v), false) for (name, v) in kwargs]
    return FieldShape(reqs, true)
end

# ── field paths & sources ─────────────────────────────────────────────────────
parse_path(s::AbstractString) = Path(Symbol.(split(String(s), '.')))

_src(s::AbstractString)        = SrcPath(parse_path(s))
_src(v::Union{Bool,Integer})   = SrcLit(v isa Bool ? v : Int64(v))
_src(v::AbstractFloat)         = SrcLit(Float64(v))
_src(t::Tuple)                 = SrcTuple(Source[_src(x) for x in t])
_src(v::AbstractVector)        = SrcList(Source[_src(x) for x in v])
_src(s::Source)               = s

# ── archetype(...) → ArchetypeIL ──────────────────────────────────────────────
"""
    archetype(name; args...) -> ArchetypeIL

`name ∈ (:Scalars,:scalars,:TextLog,:Points2D,:Points3D,:Transform3D)`. Field-path
args are dotted strings; points are tuples of paths; `each=` iterates one array field.
"""
function archetype(name::Symbol; each=nothing, kwargs...)
    kw = Dict(kwargs)
    if name === :Scalars
        return EScalars(_src(kw[:value]))
    elseif name === :scalars
        return EScalarGroup(Pair{Symbol,Source}[k => _src(v) for (k, v) in kwargs])
    elseif name === :TextLog
        return ETextLog(_src(kw[:text]), haskey(kw, :level) ? _src(kw[:level]) : nothing)
    elseif name === :Points2D
        return EPoints2D(_pointset(each, kw, (:x, :y)))
    elseif name === :Points3D
        return EPoints3D(_pointset(each, kw, (:x, :y, :z)))
    elseif name === :Transform3D
        rot = haskey(kw, :quaternion) ? RQuat(_src(kw[:quaternion])) :
              haskey(kw, :rotation_axis_angle) ? RAxisAngle(_src(kw[:rotation_axis_angle])) :
              haskey(kw, :mat3x3) ? RMat3x3(_src(kw[:mat3x3])) : nothing
        return ETransform3D(haskey(kw, :translation) ? _src(kw[:translation]) : nothing,
                            rot, haskey(kw, :scale) ? _src(kw[:scale]) : nothing)
    else
        error("ReROS: unknown archetype :$name (declaratively-constructible: " *
              "Scalars, scalars, TextLog, Points2D, Points3D, Transform3D)")
    end
end

# Points: `each="arr"` + per-coord relative paths, or a fixed list under `positions`.
function _pointset(each, kw, coords::Tuple)
    if each !== nothing
        return PEach(parse_path(each), SrcTuple(Source[SrcPath(parse_path(kw[c])) for c in coords]))
    else
        return PFixed(Source[_src(p) for p in kw[:positions]])
    end
end
