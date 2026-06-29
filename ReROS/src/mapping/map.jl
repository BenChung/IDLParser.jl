# The mapping engine (DESIGN-MAPPING.md §2, §3): map_into!/map_struct!, the ctx-form
# emit!, the always-on structural layer, and bound_mapper (select → validate → callable).

# ── MapContext ────────────────────────────────────────────────────────────────
struct MapOpts
    max_instances::Int        # array batch cap → cap+note beyond (§3)
    expand_sequences::Bool    # message arrays → per-element sub-entities instead of columnar
    skip_header::Bool         # skip :header in map_struct! (consumed for ros_time, §6)
    maxdepth::Int             # recursion guard
end
MapOpts(; max_instances::Integer=256, expand_sequences::Bool=false,
        skip_header::Bool=true, maxdepth::Integer=8) =
    MapOpts(Int(max_instances), expand_sequences, skip_header, Int(maxdepth))

struct MapContext
    topic::String
    base_path::String
    times::TimeStamps
    depth::Int
    opts::MapOpts
    manifest::MapperManifest
    mapper::Any               # per-topic override: :auto | :structural | a Function | "pkg/msg/Name"
end
MapContext(topic, base_path, times; opts::MapOpts=MapOpts(),
           manifest::MapperManifest=MapperManifest(), mapper=:auto) =
    MapContext(String(topic), String(base_path), times, 0, opts, manifest, mapper)

joinpath_entity(base::AbstractString, seg) = string(base, '/', seg)

# A child (a nested struct field) drops the per-topic mapper override: it applies only at
# the top level (the message the rule pointed at), never to nested fields.
@inline child(ctx::MapContext, name) =
    MapContext(ctx.topic, joinpath_entity(ctx.base_path, name), ctx.times, ctx.depth + 1,
               ctx.opts, ctx.manifest, :auto)

# ── emit! (ctx forms; the low-level path/stamp forms are in sink.jl) ───────────
emit!(sink::Sink, ctx::MapContext, payload::Rerun.Archetype; static::Bool=false) =
    _push!(sink, ctx.base_path, payload, ctx.times, static)

function emit!(sink::Sink, ctx::MapContext, subpath::AbstractString, payload::Rerun.Archetype;
               static::Bool=false)
    path = isempty(subpath) ? ctx.base_path : joinpath_entity(ctx.base_path, subpath)
    return _push!(sink, path, payload, ctx.times, static)
end

# Small archetype builders (every field is a component vector — §3).
_scalars(v::Real) = Rerun.Archetypes.Scalars([Float64(v)])

# ── ros_type_name (local; ROSNode.ros_type_name backs this in integration) ─────
# A generated/authored type sits at `package.qualifier.Name` (two modules deep).
function ros_type_name(@nospecialize(T::Type))
    name = string(nameof(T))
    qmod = parentmodule(T)
    q = string(nameof(qmod))
    pmod = parentmodule(qmod)
    (pmod === qmod || qmod === Main) && return name
    return string(nameof(pmod), "/", q, "/", name)
end

# ── map_into!: leaf & container methods (the always-on structural layer, §3) ───
map_into!(sink::Sink, x::Real, ctx::MapContext)           = (emit!(sink, ctx, _scalars(x)); nothing)
map_into!(sink::Sink, x::Bool, ctx::MapContext)           = (emit!(sink, ctx, _scalars(x)); nothing)   # 0/1
map_into!(sink::Sink, x::AbstractString, ctx::MapContext) = (emit!(sink, ctx, Rerun.Archetypes.TextLog([String(x)])); nothing)
map_into!(sink::Sink, x::Char, ctx::MapContext)           = (emit!(sink, ctx, Rerun.Archetypes.TextLog([string(x)])); nothing)

# byte[]/uint8[] → record size only (Tensor/Blob payload deferred; §11.1).
function map_into!(sink::Sink, x::AbstractVector{UInt8}, ctx::MapContext)
    emit!(sink, ctx, "nbytes", _scalars(length(x)))
    return nothing
end

# Numeric 1-D array → one batched Scalars (N instances), capped (§3, mapping-1).
function map_into!(sink::Sink, x::AbstractVector{<:Real}, ctx::MapContext)
    n = length(x)
    if n <= ctx.opts.max_instances
        emit!(sink, ctx, Rerun.Archetypes.Scalars(Float64.(vec(x))))
    else
        emit!(sink, ctx, Rerun.Archetypes.Scalars(Float64.(@view x[firstindex(x):firstindex(x)+ctx.opts.max_instances-1])))
        emit!(sink, ctx, "n_truncated", _scalars(n))
    end
    return nothing
end

# Array of nested messages → columnar per-numeric-leaf batched Scalars + count (§3).
function map_into!(sink::Sink, x::AbstractVector, ctx::MapContext)
    emit!(sink, ctx, "count", _scalars(length(x)))
    if ctx.opts.expand_sequences
        for (i, el) in enumerate(x); map_into!(sink, el, child(ctx, i)); end
    elseif !isempty(x) && length(x) <= ctx.opts.max_instances
        leaves = numeric_leaves(eltype(x))
        if isempty(leaves)
            # No numeric leaves (e.g. a string[]): expand per-element so values aren't dropped.
            for (i, el) in enumerate(x); map_into!(sink, el, child(ctx, i)); end
        else
            for (sub, path) in leaves
                emit!(sink, child(ctx, sub), Rerun.Archetypes.Scalars(Float64[_getpath(el, path) for el in x]))
            end
        end
    elseif length(x) > ctx.opts.max_instances
        emit!(sink, ctx, "n_truncated", _scalars(length(x)))
    end
    return nothing
end

# Struct / message fallback — resolve the bound mapper, then run it (§2.3).
function map_into!(sink::Sink, x, ctx::MapContext)
    return bound_mapper(ctx, typeof(x))(sink, x, ctx)
end

# Forced structural recursion — never consults the manifest (§2.3).
function map_struct!(sink::Sink, x, ctx::MapContext)
    ctx.depth >= ctx.opts.maxdepth && return nothing
    T = typeof(x)
    isempty(fieldnames(T)) && return note_unmapped!(sink, ctx, T)
    for n in fieldnames(T)
        n === :header && ctx.opts.skip_header && continue
        map_into!(sink, getfield(x, n), child(ctx, n))
    end
    return nothing
end

# ── structural helpers ─────────────────────────────────────────────────────────
_getpath(m, path::Vector{Symbol}) = Float64(foldl((v, s) -> getfield(v, s), path; init=m))

"""Ordered `(subpath, field-chain)` over the numeric leaves of element type `E`."""
function numeric_leaves(@nospecialize(E::Type))
    leaves = Tuple{String,Vector{Symbol}}[]
    _walk_numeric!(leaves, E, String[], Symbol[])
    return leaves
end
function _walk_numeric!(leaves, @nospecialize(E::Type), names::Vector{String}, path::Vector{Symbol})
    (isstructtype(E) && isconcretetype(E)) || return
    for n in fieldnames(E)
        FT = fieldtype(E, n)
        nn = vcat(names, string(n)); np = vcat(path, n)
        if FT <: Real && FT !== Bool
            push!(leaves, (join(nn, "/"), np))
        elseif isstructtype(FT) && isconcretetype(FT) && !(FT <: AbstractString) && !(FT <: AbstractArray)
            _walk_numeric!(leaves, FT, nn, np)
        end
    end
    return leaves
end

note_unmapped!(sink::Sink, ctx::MapContext, @nospecialize(T)) =
    (@warn "ReROS: no mapper and no fields; value dropped" type=T topic=ctx.topic maxlog=1; nothing)

"""Nanoseconds from a message's `header.stamp` (std_msgs/Header), or `nothing` (§6)."""
function stamp_of(msg)::Union{Int64,Nothing}
    T = typeof(msg)
    hasfield(T, :header) || return nothing
    h = getfield(msg, :header)
    hasfield(typeof(h), :stamp) || return nothing
    s = getfield(h, :stamp)
    (hasfield(typeof(s), :sec) && hasfield(typeof(s), :nanosec)) || return nothing
    return Int64(getfield(s, :sec)) * 1_000_000_000 + Int64(getfield(s, :nanosec))
end

# ── bound_mapper: select → validate → callable (§2.4 binding) ──────────────────
# Returns a `(sink, x, ctx) -> nothing` callable. At the top level it first honors a
# per-topic mapper override (from the matched `record` rule), then walks the manifest
# candidates most-specific-first, binding the first whose `requires` fits; on no fit it
# falls back to `map_struct!`. (Per-(topic,T) memoization belongs to the subscription;
# here it resolves fresh.)
function bound_mapper(ctx::MapContext, @nospecialize(T::Type))
    toplevel = ctx.depth == 0
    if toplevel && ctx.mapper !== :auto
        cb = _override_callable(ctx, T, ctx.mapper)
        cb === nothing || return cb           # nothing ⇒ override didn't resolve; fall through to :auto
    end
    typename = ros_type_name(T)
    for e in select_candidates(ctx.manifest, typename, ctx.topic, T; toplevel=toplevel)
        cb = _try_bind(e, T, typename)
        cb === nothing || return cb
    end
    return map_struct!
end

# Try to bind one entry: no `requires` → use it; else conform, returning the callable
# on a fit and `nothing` (so the caller tries the next candidate) on a miss or a throw.
function _try_bind(e::MapperEntry, @nospecialize(T::Type), typename)
    e.requires === nothing && return _entry_callable(e)
    r = try
        conform(e.requires, T)                # precise IL-vs-IL check (§12.4)
    catch err
        @warn "ReROS: conformance check errored; trying next candidate" type=typename exception=err maxlog=5
        return nothing
    end
    r.ok && return _entry_callable(e)
    @warn "ReROS: mapper does not fit type; trying next candidate" desc=e.desc type=typename mismatches=join(string.(r.mismatches), "; ") maxlog=5
    return nothing
end

# Resolve a per-topic mapper override to a callable, or `nothing` to fall back to :auto.
function _override_callable(ctx::MapContext, @nospecialize(T::Type), mapper)
    if mapper === :structural
        return map_struct!
    elseif mapper isa Function
        return mapper
    elseif mapper isa AbstractString
        e = select_entry(ctx.manifest, String(mapper), ctx.topic, T; toplevel=true)
        e === nothing && (@warn "ReROS: rule mapper name has no manifest entry; using :auto" name=mapper maxlog=5; return nothing)
        return _try_bind(e, T, String(mapper))
    end
    @warn "ReROS: unrecognized rule mapper; using :auto" mapper maxlog=5
    return nothing
end

_entry_callable(e::MapperEntry) = _body_callable(e.body)
_body_callable(b::BFn) = _fn_callable(b.ref)
_body_callable(b::BSpec) = (sink, x, ctx) -> _apply_spec(b, sink, x, ctx)   # _apply_spec in codegen.jl
_fn_callable(r::FByValue) = r.f
_fn_callable(r::FByName)  = error("ReROS: FByName resolution is added with the TOML loader (Phase 6): $(r.qualname)")
_fn_callable(r::FInline)  = error("ReROS: FInline eval is added with the TOML loader (Phase 6)")
