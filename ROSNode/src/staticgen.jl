# §11/D5 — static type generation macros: `@ros_import` and `@ros_cache`.
#
# Both turn a *source* of type definitions into statically-compiled Julia types in
# the caller module (reusing ROSMessages' existing generation — the same pipeline
# `@ros_msg`/`@ros_msgs` run) and record `(type, TypeDescription-JSON)` pairs in the
# module-local `__ros_static_types__` global, which Context creation registers (real
# RIHS01, bound to the precompiled type) so keyexpr-only resolution and the §13
# server use them directly — no runtime codegen.
#
#   • `@ros_import "pkg" | "pkg/qual/Name" …` — resolve ament/vendored types *by
#     name* (with their transitive reference closure) and generate them. The static
#     counterpart to dynamic discovery; no JSON caching (ament/vendored is the
#     durable source).
#   • `@ros_cache [dir]` — bake the project-local discovered-type cache (`.json`
#     blobs) into static types, `include_dependency` the blobs (so a grown cache
#     re-precompiles), and turn on runtime persistence (so this run's new discoveries
#     are written for next time). The D5/D9 "converge to fast" opt-in.
#
# `flush_type_cache` graduates the cache out to a folder (default `.msg`).

import IDLParser
using ROSMessages: ROSMessages, IL

export @ros_import, @ros_cache, flush_type_cache

# `_STATIC_GLOBAL` / `_CACHE_MARKER` (the module-local globals the macros populate)
# are typesupport.jl's, already in module scope.

# ── name → source-file resolution (ament + vendored), with transitive closure ──

# Interface file extension for a qualifier.
_iface_ext(qual::AbstractString) = qual == "srv" ? ".srv" : qual == "action" ? ".action" : ".msg"

# Resolve one fully-qualified `(package, qualifier, bare)` to a source path, vendored
# first (ships with ROSNode) then ament (a sourced ROS2 env). `nothing` if absent.
function _resolve_one_iface(package::AbstractString, qualifier::AbstractString, bare::AbstractString)
    vend = joinpath(_VENDOR_DIR, package, qualifier, bare * _iface_ext(qualifier))
    isfile(vend) && return vend
    return _find_ament_file("$package/$qualifier/$bare")
end

# All `(qualifier, bare, path)` interface files of a package (vendored ∪ ament).
function _package_iface_files(package::AbstractString)
    out = Tuple{String, String, String}[]
    vdir = joinpath(_VENDOR_DIR, package)
    if isdir(vdir)
        for qual in ("msg", "srv", "action")
            qdir = joinpath(vdir, qual)
            isdir(qdir) || continue
            for f in sort!(readdir(qdir))
                e = splitext(f)
                e[2] in (".msg", ".srv", ".action") || continue
                push!(out, (qual, e[1], joinpath(qdir, f)))
            end
        end
    end
    if isempty(out)
        files = get(discover_ament_packages(), String(package), String[])
        for path in files
            pkg, qual, bare = split_ros_name(_path_to_ros_name(path))
            push!(out, (qual, bare, path))
        end
    end
    return out
end

# Recover a `<pkg>/<qual>/<Name>` name from an ament file path
# `…/share/<pkg>/<qual>/<Name>.<ext>`.
function _path_to_ros_name(path::AbstractString)
    parts = splitpath(path)
    bare = splitext(parts[end])[1]
    qual = length(parts) >= 2 ? parts[end-1] : "msg"
    pkg  = length(parts) >= 3 ? parts[end-2] : ""
    return "$pkg/$qual/$bare"
end

# Expand an `@ros_import` name to its initial `(package, qualifier, bare)` set: a
# bare `"pkg"` → every interface in the package; else the single named type.
function _expand_import_name(name::AbstractString)
    parts = split(name, '/')
    if length(parts) == 1
        return [(String(name), q, b) for (q, b, _) in _package_iface_files(String(name))]
    else
        pkg, qual, bare = split_ros_name(name)
        return [(pkg, qual, bare)]
    end
end

# Parse a resolved interface file to IL (kind by qualifier).
function _parse_interface(path::AbstractString, qualifier::AbstractString, bare::AbstractString)
    src = read(path, String)
    qualifier == "srv"    ? ROSMessages.service_il(src; name=bare) :
    qualifier == "action" ? ROSMessages.action_il(src; name=bare)  :
                            ROSMessages.message_il(src; name=bare)
end

# Resolve the requested names to the full transitive set of interface specs
# `(; package, qualifier, bare, path, il)` — following each interface's references
# (`referenced_refs`) to its dependencies so generation is closed.
function _resolve_import_closure(names::Vector{String})
    specs = NamedTuple[]
    seen = Set{String}()
    work = Tuple{String, String, String}[]
    for name in names, t in _expand_import_name(name)
        push!(work, t)
    end
    while !isempty(work)
        (pkg, qual, bare) = popfirst!(work)
        qn = "$pkg/$qual/$bare"
        qn in seen && continue
        push!(seen, qn)
        path = _resolve_one_iface(pkg, qual, bare)
        if path === nothing
            @warn "@ros_import: could not resolve $qn (not vendored or in AMENT_PREFIX_PATH)"
            continue
        end
        il = _parse_interface(path, qual, bare)
        push!(specs, (; package=pkg, qualifier=qual, bare=bare, path=path, il=il))
        for (rpkg, rname) in IL.referenced_refs(il)         # transitive deps (always msg)
            push!(work, (rpkg === nothing ? pkg : String(rpkg), "msg", String(rname)))
        end
    end
    return specs
end

# ── per-type TypeDescription (with closure), for the baked registration JSON ────

# The generated types of an IL interface as `(qualifier, bare, section_il)` — a
# message is one; a service/action is its sections (each its own generated struct).
_il_sections(m::IL.RMessage) = [("msg", string(m.name), m)]
_il_sections(s::IL.RService) =
    [("srv", string(s.request.name), s.request), ("srv", string(s.response.name), s.response)]
_il_sections(a::IL.RAction) =
    [("action", string(a.goal.name), a.goal), ("action", string(a.result.name), a.result),
     ("action", string(a.feedback.name), a.feedback)]

# Compute, for each top-level generated type across `specs` (each `(package, il)`),
# its canonical internal `TypeDescriptionMsg` (main + referenced closure, sorted) —
# the form `to_ros2_json` serializes for the baked `_static_type_json`.
function _static_type_descriptions(specs)
    mains = Tuple{String, String, String, TypeDescription}[]
    for (package, il) in specs
        for (qual, bare, secil) in _il_sections(il)
            ast = _scan_for_struct(lower(secil; package=package))
            ast === nothing && continue
            qn = "$package/$qual/$bare"
            # `qn` (fully-qualified) is the type's own name; `qualifier="msg"` only
            # governs its *relative* refs (a bare ref is a same-package msg).
            main = type_description_from_struct(ast, qn; package=package, qualifier="msg")
            push!(mains, (package, qual, bare, main))
        end
    end
    pool = Dict{String, TypeDescription}(m[4].type_name => m[4] for m in mains)
    return [(pkg, qual, bare, TypeDescriptionMsg(main, _collect_td_closure(main, pool)))
            for (pkg, qual, bare, main) in mains]
end

# ── emitting the static-type registration ───────────────────────────────────────
# Both macros emit (into the caller, escaped): a module-local accumulator global
# `__ros_static_types__` of `(type, TypeDescription-JSON)` pairs (created once,
# appended per call; baked into the module image), an optional `__ros_cache_dir__`
# opt-in marker, and a flush of those into ROSNode's `_STATIC_TYPES` singleton — at
# eval for script/REPL modules (guarded off during precompile) and via a generated
# `__init__` for precompiled packages (defined once; deferring to a user `__init__`,
# which should then call `ROSNode.absorb_static_types!(@__MODULE__)`). Context pulls
# from the singleton — no module/method-table scanning.

# `jsons` :: iterable of `(package, qualifier, bare, json::String)`. `cache_dir` is
# `nothing` (`@ros_import`) or the persistence dir string (`@ros_cache`, `""` ⇒ default).
function _static_register_stmts(jsons, cache_dir)
    g = _STATIC_GLOBAL
    cm = _CACHE_MARKER
    stmts = Any[]
    pairs = [:(($(Meta.parse("$pkg.$qual.$bare")), $(json))) for (pkg, qual, bare, json) in jsons]
    if !isempty(pairs)
        push!(stmts, :(if !$(Expr(:isdefined, g)); global $(g) = Tuple{Type, String}[]; end))
        push!(stmts, :(push!($(g), $(pairs...))))
    end
    cache_dir === nothing || push!(stmts, :(global $(cm) = $(cache_dir)))
    # eval-time flush (scripts/REPL); skipped while a package precompiles.
    push!(stmts, :(if ccall(:jl_generating_output, Cint, ()) == 0
                       ROSNode.absorb_static_types!(@__MODULE__)
                   end))
    # load-time flush for precompiled packages — define __init__ once, deferring to
    # any user-defined one.
    push!(stmts, :(if !$(Expr(:isdefined, :__init__))
                       function __init__()
                           ROSNode.absorb_static_types!(@__MODULE__)
                       end
                   end))
    return stmts
end

# ── @ros_import ─────────────────────────────────────────────────────────────────

"""
    @ros_import "pkg" | "pkg/qual/Name" ...

Statically generate ament/vendored ROS interface types *resolved by name* into the
calling module (the static counterpart to dynamic discovery). Each name is a package
(`"sensor_msgs"` — all its interfaces) or a fully-qualified type
(`"sensor_msgs/msg/Imu"`); the transitive reference closure (e.g. `std_msgs/Header`)
is pulled automatically. Sources are found in ROSNode's vendored dir and, when inside
a sourced ROS2 env, `AMENT_PREFIX_PATH`.

Generation reuses the same pipeline as [`@ros_msg`](@ref) (structs +
`include_dependency`); types land at `<pkg>.<qual>.<Name>`. They are additionally
**auto-registered** (real RIHS01) on Context creation, so a keyexpr-only
`Subscription(node, "/t")` uses the precompiled type with no runtime codegen, and the
§13 server can serve their descriptions. No JSON caching — ament/vendored is the
durable source.
"""
macro ros_import(names...)
    all(n -> n isa AbstractString, names) ||
        error("@ros_import takes string literals (package or fully-qualified type names)")
    specs = _resolve_import_closure(String[String(n) for n in names])
    isempty(specs) &&
        error("@ros_import: no sources resolved for $(names) — vendored, or source a ROS2 env")
    files = unique(String[s.path for s in specs])
    block = ROSMessages._expand_msg_files(files)             # structs + include_dependency
    tds = _static_type_descriptions([(s.package, s.il) for s in specs])
    jsons = [(pkg, qual, bare, to_ros2_json(tdmsg)) for (pkg, qual, bare, tdmsg) in tds]
    append!(block.args, _static_register_stmts(jsons, nothing))
    return esc(block)
end

# ── @ros_cache ──────────────────────────────────────────────────────────────────

# Read a cache blob (`<name>\n<to_ros2_json>`); return `(name, TypeDescriptionMsg)`.
function _load_cache_blob(path::AbstractString)
    content = read(path, String)
    nl = findfirst('\n', content)
    nl === nothing && return nothing
    name = strip(content[1:prevind(content, nl)])
    td = _parse_type_description_json(content[nextind(content, nl):end])
    td === nothing ? nothing : (String(name), td)
end

# Lift a TypeDescriptionMsg's main + referenced closure into `(package, IL)` specs
# (deduped by qualified name across blobs).
function _cache_specs(seen::Set{String}, td::TypeDescriptionMsg)
    out = Tuple{String, Any}[]
    for d in (td.type_description, td.referenced_type_descriptions...)
        d.type_name in seen && continue
        push!(seen, d.type_name)
        push!(out, (split_ros_name(d.type_name)[1], lift(d)))
    end
    return out
end

"""
    @ros_cache [dir]

Opt into project-local persistence + static baking of dynamically-discovered types
(D5/D9). At precompile it reads the project cache (`.json` `TypeDescription` blobs
under `dir`, default `<project>/ros_typesupport`), generates static types for them
into the calling module, `include_dependency`s the blobs (so a grown cache
re-precompiles + regenerates next load), and auto-registers them (so keyexpr-only
subs use the precompiled type — no runtime codegen). It also turns on runtime
persistence, so *this* run's newly-discovered types are written to the cache for next
time. Converges a deployment toward fast warm-up as discovery saturates.
"""
macro ros_cache(args...)
    dir = isempty(args) ? _default_project_cache_dir() :
          (args[1] isa AbstractString ? String(args[1]) :
           error("@ros_cache takes an optional string-literal directory"))

    blobs = isdir(dir) ? sort!(filter(f -> endswith(f, ".json"), readdir(dir; join=true))) : String[]
    seen = Set{String}()
    specs = Tuple{String, Any}[]
    type_jsons = Tuple{String, String, String, String}[]   # (pkg, qual, bare, blobjson)
    for b in blobs
        parsed = _load_cache_blob(b)
        parsed === nothing && continue
        (name, td) = parsed
        append!(specs, _cache_specs(seen, td))
        pkg, qual, bare = split_ros_name(name)
        type_jsons = push!(type_jsons, (pkg, qual, bare, to_ros2_json(td)))
    end

    code = isempty(specs) ? Any[] : _generate_exprs_multi(specs)
    block = Expr(:toplevel, code...)
    # Invalidate precompile when the cache dir or any blob changes (so new discoveries
    # are baked next load). Directory mtime catches added/removed blobs.
    push!(block.args, :($Base.include_dependency($dir)))
    for b in blobs
        push!(block.args, :($Base.include_dependency($b)))
    end
    # Bake registration for the cached types + the cache opt-in marker (`""` ⇒ the
    # project default dir, recomputed at load so it stays relocatable).
    append!(block.args, _static_register_stmts(type_jsons, isempty(args) ? "" : dir))
    return esc(block)
end

# Generate Julia code for several IL interfaces together (cross-refs resolve), the
# IL analogue of ROSMessages' file-based `_expand_msg_files`.
function _generate_exprs_multi(ils_with_pkg)
    decls = IDLParser.Parse.Decl[]
    for (pkg, il) in ils_with_pkg
        append!(decls, lower(il; package=pkg))
    end
    resolved = IDLParser.ConstResolution.resolve_constants(decls)
    return IDLParser.Generation.generate_code(resolved)
end

# ── flushing the cache out to a folder (explicit graduation) ────────────────────

"""
    flush_type_cache(to; dir = <project cache>, format = :msg) -> Vector{String}

Graduate the project-local discovered-type cache out to folder `to` (D5): write each
cached type in `format` (`:msg` default — a colcon-buildable interface package;
`:julia` for precompilable source; `:typedesc` for the raw wire blob). Returns the
paths written. The durable, user-owned export of what was learned by running against
the live graph (vs. the ephemeral cache).
"""
function flush_type_cache(to::AbstractString; dir::AbstractString=_cache_dir(),
                          format::Symbol=:msg)
    written = String[]
    isdir(dir) || return written
    for f in sort!(readdir(dir; join=true))
        endswith(f, ".json") || continue
        parsed = _load_cache_blob(f)
        parsed === nothing && continue
        (name, td) = parsed
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(td))
        hash === nothing && continue
        entry = RegistryEntry(TypeInfo(name, hash), lift(td); td=td, provenance=:cache)
        try
            append!(written, _export_one(entry, to, format))
        catch err
            @warn "flush_type_cache: export failed for $name" exception=err
        end
    end
    return written
end
