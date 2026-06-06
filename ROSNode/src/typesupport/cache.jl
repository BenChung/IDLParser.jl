# ── content-addressed cache (project-local, opt-in) ─────────────────────────
# A store keyed by RIHS01, holding the wire `TypeDescription` JSON (the form
# `calculate_rihs01_hash` hashes over, so self-validating on load). Discovery
# checks it before the network; a fetch writes it back. Because codegen is cheap, the
# cache holds the definition and regenerates the code on load.
#
# Location & opt-in: the cache is a project-local folder (in the active project's
# dir) of `.json` blobs keyed by RIHS01 hash, off by default. The user enables
# persistence by adding the [`@ros_cache`](@ref) macro to their module (which calls
# [`enable_project_cache!`](@ref) from its `__init__`), or by the
# `$ROS_TYPESUPPORT_CACHE` env override (ops/tests, force-enables with that dir).
# ROSNode is a library; discovered types are deployment-specific, so they belong in
# the importing project, owned by the user — not a package-global scratchspace.
# The JSON body is the exact `to_ros2_json` text plus the type name on a header line
# so a reload reconstructs the `TypeInfo` without re-parsing the name out of the blob.

# Cache opt-in state. `dir === nothing` ⇒ use the project-local default when enabled.
# `dirs` is the ordered read search-list (a project may register several `@ros_cache`
# dirs); `dir` is the single primary write dir (the deterministic min of `dirs`, or
# the last explicit `enable_project_cache!`). Reads consult every entry in `dirs`.
mutable struct _CacheState
    enabled::Bool
    dir::Union{String, Nothing}
    dirs::Vector{String}
end
const _CACHE = _CacheState(false, nothing, String[])

# The project-local default cache dir: `<active-project-dir>/ros_typesupport`. Falls
# back to the cwd when no project is active (a bare `julia` REPL).
function _default_project_cache_dir()
    proj = Base.active_project()
    base = proj === nothing ? pwd() : dirname(proj)
    return joinpath(base, "ros_typesupport")
end

"""
    enable_project_cache!(dir = <project>/ros_typesupport) -> String

Turn on project-local persistence of dynamically-discovered [type descriptions][1].
Discovery writes `.json` `TypeDescription` blobs (keyed by RIHS01) under `dir`, and
resolution reads a cached blob first, falling back to the wire on a miss. Off by
default: the [`@ros_cache`](@ref) macro calls this from its `__init__`, so a project
opts in by adding the macro. Returns the (created) directory.

[1]: https://design.ros2.org/articles/idl_interface_definition.html
"""
function enable_project_cache!(dir::AbstractString=_default_project_cache_dir())
    d = String(dir)
    _CACHE.enabled = true
    _CACHE.dir = d
    _CACHE.dirs = String[d]          # explicit opt-in: this dir is the read+write set
    isdir(d) || mkpath(d)
    return d
end

# Register a set of `@ros_cache` opt-in dirs deterministically. All become the read
# search-list; the lexicographically smallest is the single stable write dir. A
# `>1`-dir project is warned: discovery persists, but only to that one dir.
function _register_cache_dirs!(dirs)
    ds = sort!(unique(String[String(d) for d in dirs]))
    isempty(ds) && return _CACHE.dir
    _CACHE.enabled = true
    _CACHE.dirs = ds
    _CACHE.dir = ds[1]               # deterministic primary write dir
    for d in ds
        isdir(d) || mkpath(d)
    end
    length(ds) > 1 && @warn "typesupport: multiple @ros_cache directories registered; \
        reads search all of them, new discoveries persist only to $(ds[1])" dirs=ds maxlog=1
    return _CACHE.dir
end

"Disable project-local type-description persistence (the default state)."
disable_project_cache!() = (_CACHE.enabled = false; _CACHE.dirs = String[]; nothing)

# Is the on-disk cache active? The env override force-enables (ops/test escape hatch).
_cache_enabled() = haskey(ENV, "ROS_TYPESUPPORT_CACHE") || _CACHE.enabled

# Resolve the cache directory, creating it on demand. The env override wins; else the
# configured dir; else the project-local default. Only meaningful when enabled.
function _cache_dir()
    base = get(ENV, "ROS_TYPESUPPORT_CACHE", "")
    isempty(base) && (base = _CACHE.dir === nothing ? _default_project_cache_dir() : _CACHE.dir)
    isdir(base) || mkpath(base)
    return base
end

# The ordered read search-list. The env override (single dir) wins; else every
# registered `@ros_cache` dir (deterministic order); else the project-local default. A
# type cached under any one of them resolves — reads are not pinned to the write dir.
function _cache_read_dirs()
    base = get(ENV, "ROS_TYPESUPPORT_CACHE", "")
    isempty(base) || return String[base]
    isempty(_CACHE.dirs) || return _CACHE.dirs
    return String[_CACHE.dir === nothing ? _default_project_cache_dir() : _CACHE.dir]
end

# The content-addressed leaf for a hash: `<64-hex>.json`. The bare hex (not the
# `RIHS01_` prefix) names the file — content-addressed by the 32-byte digest.
_cache_leaf(info::TypeInfo) = bytes2hex(collect(info.hash.value)) * ".json"

# Cache file for a hash under the primary (write) dir.
_cache_path(info::TypeInfo) = joinpath(_cache_dir(), _cache_leaf(info))

# Persist a `TypeDescriptionMsg` to the cache (best-effort; a write failure is
# logged, never fatal — the type is already in the live registry). The first line
# is the qualified type name; the rest is the canonical `to_ros2_json`.
function _cache_store(info::TypeInfo, td::TypeDescriptionMsg)
    _cache_enabled() || return nothing       # off by default — no surprise files
    try
        path = _cache_path(info)
        open(path, "w") do io
            println(io, info.name)
            print(io, to_ros2_json(td))
        end
    catch err
        @warn "typesupport: cache write failed for $(info.name)" exception=err
    end
    nothing
end

# Load + self-validate a cached `TypeDescriptionMsg` for `info`, searching every
# registered cache dir in order (first hit wins). Recomputes the RIHS01 over the parsed
# blob and discards on mismatch (a stale/corrupt entry can never decode wrong).
# Returns `nothing` on miss, parse failure, or hash mismatch across all dirs.
function _cache_load(info::TypeInfo)
    _cache_enabled() || return nothing
    leaf = _cache_leaf(info)
    for dir in _cache_read_dirs()
        td = _cache_load_path(joinpath(dir, leaf), info)
        td === nothing || return td
    end
    return nothing
end

# Load + self-validate one cache file. `nothing` on miss / parse failure / hash mismatch;
# a mismatched blob is removed (it can never decode right).
function _cache_load_path(path::AbstractString, info::TypeInfo)
    isfile(path) || return nothing
    td = try
        content = read(path, String)
        nl = findfirst('\n', content)
        nl === nothing && return nothing
        json = content[nextind(content, nl):end]
        _parse_type_description_json(json)
    catch err
        @warn "typesupport: cache read failed for $(info.name)" exception=err
        return nothing
    end
    td === nothing && return nothing
    verify_type_description(td, info) || begin
        @warn "typesupport: cached type description for $(info.name) failed \
               revalidation; discarding"
        try; rm(path; force=true); catch; end
        return nothing
    end
    return td
end

# ── TypeDescription JSON (de)serialization ──────────────────────────────────
# The cache and the `:typedesc` export round-trip the wire blob through the exact
# JSON `to_ros2_json` emits. Serialization is ROSMessages' (`to_ros2_json`, the
# hashing form); the inverse parser lives here as a cache/persistence concern. It
# parses the fixed, hand-written shape directly to avoid a JSON dependency, matching
# the posture ROSMessages takes when writing it.

# Minimal recursive-descent parser for the exact JSON shape `to_ros2_json` emits:
# `{"type_description": <td>, "referenced_type_descriptions": [<td>...]}` where a
# <td> is `{"type_name": <str>, "fields": [{"name": <str>, "type": {...}}...]}`.
# Returns `nothing` on any structural surprise (treated as a cache miss upstream).
function _parse_type_description_json(json::AbstractString)
    val, _ = _json_value(json, firstindex(json))
    val isa Dict || return nothing
    main = _td_from_dict(get(val, "type_description", nothing))
    main === nothing && return nothing
    refsraw = get(val, "referenced_type_descriptions", nothing)
    refs = TypeDescription[]
    if refsraw isa Vector
        for r in refsraw
            td = _td_from_dict(r)
            td === nothing && return nothing
            push!(refs, td)
        end
    end
    return TypeDescriptionMsg(main, refs)
end

function _td_from_dict(d)
    d isa Dict || return nothing
    name = get(d, "type_name", nothing)
    name isa AbstractString || return nothing
    fieldsraw = get(d, "fields", nothing)
    fieldsraw isa Vector || return nothing
    fields = FieldDescription[]
    for f in fieldsraw
        f isa Dict || return nothing
        fname = get(f, "name", nothing)
        ftd = get(f, "type", nothing)
        (fname isa AbstractString && ftd isa Dict) || return nothing
        ft = FieldTypeDescription(
            UInt8(Int(get(ftd, "type_id", 0))),
            UInt64(Int(get(ftd, "capacity", 0))),
            UInt64(Int(get(ftd, "string_capacity", 0))),
            String(get(ftd, "nested_type_name", "")))
        push!(fields, FieldDescription(String(fname), ft))
    end
    return TypeDescription(String(name), fields)
end

# A tiny JSON value reader (objects, arrays, strings, ints) covering exactly the
# subset `to_ros2_json` produces — no floats, no booleans, no nesting beyond the
# TypeDescription shape. Returns `(value, next_index)`. Throws on malformed input,
# which the callers above catch and treat as a miss.
function _json_value(s::AbstractString, i::Int)
    i = _json_skip_ws(s, i)
    c = s[i]
    if c == '{'
        return _json_object(s, i)
    elseif c == '['
        return _json_array(s, i)
    elseif c == '"'
        return _json_string_lit(s, i)
    else
        return _json_number(s, i)
    end
end

_json_skip_ws(s, i) = (while i <= lastindex(s) && isspace(s[i]); i = nextind(s, i); end; i)

function _json_object(s::AbstractString, i::Int)
    d = Dict{String, Any}()
    i = nextind(s, i)                                   # past '{'
    i = _json_skip_ws(s, i)
    s[i] == '}' && return (d, nextind(s, i))
    while true
        i = _json_skip_ws(s, i)
        key, i = _json_string_lit(s, i)
        i = _json_skip_ws(s, i)
        s[i] == ':' || error("expected ':' in JSON object")
        i = nextind(s, i)
        val, i = _json_value(s, i)
        d[key] = val
        i = _json_skip_ws(s, i)
        c = s[i]; i = nextind(s, i)
        c == ',' && continue
        c == '}' && break
        error("expected ',' or '}' in JSON object")
    end
    return (d, i)
end

function _json_array(s::AbstractString, i::Int)
    a = Any[]
    i = nextind(s, i)                                   # past '['
    i = _json_skip_ws(s, i)
    s[i] == ']' && return (a, nextind(s, i))
    while true
        val, i = _json_value(s, i)
        push!(a, val)
        i = _json_skip_ws(s, i)
        c = s[i]; i = nextind(s, i)
        c == ',' && continue
        c == ']' && break
        error("expected ',' or ']' in JSON array")
    end
    return (a, i)
end

function _json_string_lit(s::AbstractString, i::Int)
    s[i] == '"' || error("expected string in JSON")
    i = nextind(s, i)
    io = IOBuffer()
    while true
        c = s[i]
        if c == '"'
            return (String(take!(io)), nextind(s, i))
        elseif c == '\\'
            i = nextind(s, i)
            e = s[i]
            write(io, e == 'n' ? '\n' : e == 't' ? '\t' : e == 'r' ? '\r' :
                      e == 'b' ? '\b' : e == 'f' ? '\f' : e)
            i = nextind(s, i)
        else
            write(io, c)
            i = nextind(s, i)
        end
    end
end

function _json_number(s::AbstractString, i::Int)
    start = i
    while i <= lastindex(s) && (isdigit(s[i]) || s[i] == '-' || s[i] == '+')
        i = nextind(s, i)
    end
    n = parse(Int, SubString(s, start, prevind(s, i)))
    return (n, i)
end

