# Declarative-spec application (DESIGN-MAPPING.md §13.4). This is the correct
# interpreter over the emission IL; the type-encoded `@generated` monomorphization
# (FieldPath{S} + a staged `apply!`) is the deferred perf optimization — it needs a
# live high-rate workload to profile, which the structural correctness here does not.

# ── extract a Source from a message value ─────────────────────────────────────
_extract(s::SrcPath, m)  = foldl((v, sym) -> getfield(v, sym), s.path; init=m)
_extract(s::SrcTuple, m) = Tuple(_extract(p, m) for p in s.parts)
_extract(s::SrcList, m)  = [_extract(p, m) for p in s.items]
_extract(s::SrcLit, _)   = s.value

_ntuplef32(t, ::Val{N}) where {N} = ntuple(i -> Float32(t[i]), N)

# Build a Vector{NTuple{N,Float32}} of points.
_points(p::PFixed, m, ::Val{N}) where {N} =
    NTuple{N,Float32}[_ntuplef32(_extract(pt, m), Val(N)) for pt in p.points]
function _points(p::PEach, m, ::Val{N}) where {N}
    arr = foldl((v, s) -> getfield(v, s), p.array; init=m)
    return NTuple{N,Float32}[_ntuplef32(_extract(p.point, el), Val(N)) for el in arr]
end

# ── apply a BSpec ─────────────────────────────────────────────────────────────
function _apply_spec(b::BSpec, sink::Sink, msg, ctx::MapContext)
    _emit_archetype(b.emit, b.subpath, sink, msg, ctx)
    b.also_structural && map_struct!(sink, msg, ctx)
    return nothing
end

_emit_archetype(e::EScalars, sub, sink, msg, ctx) =
    emit!(sink, ctx, sub, Rerun.Archetypes.Scalars([Float64(_extract(e.value, msg))]))

function _emit_archetype(e::EScalarGroup, sub, sink, msg, ctx)
    for (name, src) in e.series
        path = isempty(sub) ? string(name) : string(sub, '/', name)
        emit!(sink, ctx, path, Rerun.Archetypes.Scalars([Float64(_extract(src, msg))]))
    end
    return nothing
end

_emit_archetype(e::ETextLog, sub, sink, msg, ctx) =
    emit!(sink, ctx, sub, Rerun.Archetypes.TextLog([string(_extract(e.text, msg))]))

_emit_archetype(e::EPoints3D, sub, sink, msg, ctx) =
    emit!(sink, ctx, sub, Rerun.Archetypes.Points3D(_points(e.positions, msg, Val(3))))

_emit_archetype(e::EPoints2D, sub, sink, msg, ctx) =
    emit!(sink, ctx, sub, Rerun.Archetypes.Points2D(_points(e.positions, msg, Val(2))))

function _emit_archetype(e::ETransform3D, sub, sink, msg, ctx)
    kw = Pair{Symbol,Any}[]
    e.translation !== nothing && push!(kw, :translation => [_ntuplef32(_extract(e.translation, msg), Val(3))])
    if e.rotation isa RQuat
        push!(kw, :quaternion => [_ntuplef32(_extract(e.rotation.src, msg), Val(4))])
    elseif e.rotation isa RAxisAngle
        # rerun.components.RotationAxisAngle is an Arrow struct {axis: f32[3], angle: f32},
        # NOT a flat 4-tuple — a tuple has no `.axis`/`.angle` and crashes the writer.
        v = _ntuplef32(_extract(e.rotation.src, msg), Val(4))   # (x, y, z, angle)
        push!(kw, :rotation_axis_angle => [(axis = (v[1], v[2], v[3]), angle = v[4])])
    elseif e.rotation isa RMat3x3
        push!(kw, :mat3x3 => [_ntuplef32(_extract(e.rotation.src, msg), Val(9))])
    end
    e.scale !== nothing && push!(kw, :scale => [_ntuplef32(_extract(e.scale, msg), Val(3))])
    emit!(sink, ctx, sub, Rerun.Archetypes.Transform3D(; kw...))
    return nothing
end
