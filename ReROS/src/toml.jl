# TOML config loader (DESIGN.md §6.3, DESIGN-MAPPING.md §12.3). Maps a TOML file to a
# RecorderConfig. `fn`/`mapper_use` reference values in already-loaded modules (a name
# walk, no eval); `[mapper.julia]` / `mapper_include` execute Julia and require allow_julia.

import TOML

"""
    load_config(path; allow_julia=false) -> RecorderConfig

Load a recorder configuration from a TOML file. `allow_julia` (or `allow_julia = true`
in the file) is required for the `[mapper.julia]` inline-source and `mapper_include`
forms, which execute arbitrary Julia.
"""
load_config(path::AbstractString; allow_julia::Bool=false) =
    _config_from_toml(TOML.parsefile(String(path)), allow_julia, dirname(abspath(String(path))))

"""Load a config from a TOML string (`include` paths resolve relative to `basedir`)."""
load_config_string(s::AbstractString; allow_julia::Bool=false, basedir::AbstractString=pwd()) =
    _config_from_toml(TOML.parse(String(s)), allow_julia, String(basedir))

function _config_from_toml(t::AbstractDict, allow_julia::Bool, basedir::AbstractString)
    allow = allow_julia || get(t, "allow_julia", false) === true
    sinks = _sinks_from_toml(get(t, "sinks", Any[]))
    isempty(sinks) && push!(sinks, Save(get(t, "save", "recording.rrd")))

    record = Rule[]
    for r in get(t, "record", Any[])
        push!(record, rule(_selector_from_toml(r); data = get(r, "data", true),
                           connectivity = get(r, "connectivity", true), mapper = _mapper_opt(get(r, "mapper", :auto))))
    end
    for x in get(t, "exclude", Any[])
        push!(record, exclude(_selector_from_toml(x)))
    end

    return RecorderConfig(get(t, "app_id", "reros");
                          recording_id = get(t, "recording_id", nothing),
                          sinks = sinks, record = record,
                          mappers = _mappers_from_toml(t, allow, basedir))
end

_mapper_opt(v::AbstractString) = startswith(v, ":") ? Symbol(v[2:end]) : v
_mapper_opt(v) = v

# ── sinks ─────────────────────────────────────────────────────────────────────
function _sinks_from_toml(arr)
    out = SinkSpec[]
    for s in arr
        if s isa AbstractString
            s == "spawn" ? push!(out, Spawn()) : error("ReROS: unknown sink \"$s\" (expected \"spawn\")")
        elseif s isa AbstractDict
            haskey(s, "save") && push!(out, Save(s["save"]))
            haskey(s, "grpc") && push!(out, Grpc(url = s["grpc"]))
            get(s, "spawn", false) === true && push!(out, Spawn())
        else
            error("ReROS: bad sink entry $(s)")
        end
    end
    return out
end

# ── selectors ─────────────────────────────────────────────────────────────────
function _selector_from_toml(t::AbstractDict)
    sels = Selector[]
    haskey(t, "topic")     && push!(sels, topic(t["topic"]))
    haskey(t, "type")      && push!(sels, type(t["type"]))
    haskey(t, "namespace") && push!(sels, namespace(t["namespace"]))
    haskey(t, "node")      && push!(sels, node(t["node"]))
    haskey(t, "kind")      && push!(sels, kind(Symbol(t["kind"])))
    isempty(sels) && error("ReROS: a record/exclude entry needs a selector (topic/type/namespace/node/kind)")
    return reduce(&, sels)
end

# ── mappers ───────────────────────────────────────────────────────────────────
function _mappers_from_toml(t::AbstractDict, allow::Bool, basedir::AbstractString)
    man = MapperManifest()
    # Includes first: an included file may define the module a later `mapper_use` resolves.
    for f in get(t, "mapper_include", String[])
        allow || error("ReROS: mapper_include requires allow_julia=true (it runs Julia): $f")
        Base.include(Main, joinpath(basedir, f))
    end
    for u in get(t, "mapper_use", String[])
        v = _resolve_name(u)
        v isa MapperManifest || error("ReROS: mapper_use \"$u\" is not a MapperManifest")
        man = merge(man, v)
    end
    entries = MapperEntry[]
    for m in get(t, "mapper", Any[])
        push!(entries, _mapper_entry_from_toml(m, allow))
    end
    return merge(man, MapperManifest(entries))
end

function _mapper_entry_from_toml(m::AbstractDict, allow::Bool)
    matcher = haskey(m, "type")  ? NameMatch(m["type"]) :
              haskey(m, "topic") ? TopicMatch(m["topic"]) :
              haskey(m, "match") ? StructMatch(_shape_from_toml(m["match"])) :
              error("ReROS: [[mapper]] needs a selector (type/topic/match)")
    body = if haskey(m, "fn")
        BFn(FByValue(_resolve_name(m["fn"])))
    elseif haskey(m, "emit")
        _emit_from_toml(m["emit"])
    elseif haskey(m, "julia")
        allow || error("ReROS: [[mapper]] julia body requires allow_julia=true")
        BFn(FByValue(_eval_inline_mapper(m["julia"]["body"])))
    else
        error("ReROS: [[mapper]] needs fn, emit, or julia")
    end
    return MapperEntry(matcher, body; desc = get(m, "desc", ""))
end

# match table → FieldShape. Values are kind tokens ("float", "float[]", "float[3]", "real",
# …) or nested tables (nested message shapes).
_shape_from_toml(t::AbstractDict) =
    FieldShape(FieldReq[FieldReq(Symbol(k), _kind_from_toml(v), false) for (k, v) in t], true)
_kind_from_toml(v::AbstractDict) = KNested(_shape_from_toml(v))
function _kind_from_toml(v::AbstractString)
    m = match(r"^(\w+)(?:\[(\d*)\])?$", v)
    m === nothing && error("ReROS: bad kind token \"$v\"")
    base = _scalar(Symbol(m.captures[1]))
    m.captures[2] === nothing && return base
    return KArray(base, isempty(m.captures[2]) ? ASeq() : AFixed(parse(Int, m.captures[2])))
end

# [mapper.emit] table → a BSpec (declarative archetype), via the DSL builders.
function _emit_from_toml(e::AbstractDict)
    il = _emit_il(e)
    return BSpec(il; subpath = get(e, "subpath", ""), also_structural = get(e, "also_structural", false))
end
function _emit_il(e::AbstractDict)
    arch = Symbol(e["archetype"])
    if arch === :Transform3D
        kw = Pair{Symbol,Any}[]
        for k in (:translation, :quaternion, :rotation_axis_angle, :mat3x3, :scale)
            haskey(e, String(k)) && push!(kw, k => Tuple(e[String(k)]))
        end
        return archetype(:Transform3D; kw...)
    elseif arch === :Points3D || arch === :Points2D
        if haskey(e, "each")
            return archetype(arch; each = e["each"],
                             (Symbol(c) => e[c] for c in ("x", "y", "z") if haskey(e, c))...)
        end
        return archetype(arch; positions = [Tuple(p) for p in e["positions"]])
    elseif arch === :Scalars
        return archetype(:Scalars; value = e["value"])
    elseif arch === :scalars
        return archetype(:scalars; (Symbol(k) => v for (k, v) in e
                                    if k ∉ ("archetype", "subpath", "also_structural"))...)
    elseif arch === :TextLog
        return haskey(e, "level") ? archetype(:TextLog; text = e["text"], level = e["level"]) :
                                    archetype(:TextLog; text = e["text"])
    else
        error("ReROS: unknown [mapper.emit] archetype \"$arch\"")
    end
end

# Walk a module-qualified name from `Main` (a value lookup, no eval): "Pkg.MAPPERS"/"Mod.fn".
function _resolve_name(qual::AbstractString)
    v = Main
    for p in split(qual, '.')
        v = try
            getproperty(v, Symbol(p))
        catch
            error("ReROS: cannot resolve name \"$qual\" — `$p` is undefined; the module/value " *
                  "must already be loaded (e.g. `using ...`) before load_config")
        end
    end
    return v
end

# Inline Julia body → a (sink, msg, ctx) callable, eval'd in a fresh module that imports
# the mapper-authoring API. Gated by allow_julia (executes arbitrary code).
function _eval_inline_mapper(body::AbstractString)
    m = Module(:ReROSInlineMapper)
    Core.eval(m, :(using ReROS; using Rerun.Archetypes))
    return Core.eval(m, Meta.parse("(sink, msg, ctx) -> begin\n" * body * "\nend"))
end
