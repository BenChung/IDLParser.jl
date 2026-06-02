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

# Resolve one fully-qualified `(package, qualifier, bare)` to a source path: BYO
# `from=` roots first (parent-of-packages, `<root>/<pkg>/<qual>/<Name>.<ext>`), then
# ROSNode's vendored dir, then ament (a sourced ROS2 env). `nothing` if absent.
function _resolve_one_iface(package::AbstractString, qualifier::AbstractString, bare::AbstractString,
                            roots::Vector{String}=String[])
    for root in roots
        p = joinpath(root, package, qualifier, bare * _iface_ext(qualifier))
        isfile(p) && return p
    end
    vend = joinpath(_VENDOR_DIR, package, qualifier, bare * _iface_ext(qualifier))
    isfile(vend) && return vend
    return _find_ament_file("$package/$qualifier/$bare")
end

# All `(qualifier, bare, path)` interface files of a package: BYO `from=` roots ∪
# vendored (first source to declare a given `(qual, bare)` wins, BYO before vendored),
# falling back to ament only when neither has the package.
function _package_iface_files(package::AbstractString, roots::Vector{String}=String[])
    out = Tuple{String, String, String}[]
    seen = Set{Tuple{String, String}}()
    for base in (roots..., _VENDOR_DIR)
        pdir = joinpath(base, package)
        isdir(pdir) || continue
        for qual in ("msg", "srv", "action")
            qdir = joinpath(pdir, qual)
            isdir(qdir) || continue
            for f in sort!(readdir(qdir))
                e = splitext(f)
                e[2] in (".msg", ".srv", ".action") || continue
                (qual, e[1]) in seen && continue
                push!(seen, (qual, e[1]))
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
function _expand_import_name(name::AbstractString, roots::Vector{String}=String[])
    parts = split(name, '/')
    if length(parts) == 1
        return [(String(name), q, b) for (q, b, _) in _package_iface_files(String(name), roots)]
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
function _resolve_import_closure(names::Vector{String}, roots::Vector{String}=String[])
    specs = NamedTuple[]
    seen = Set{String}()
    work = Tuple{String, String, String}[]
    for name in names, t in _expand_import_name(name, roots)
        push!(work, t)
    end
    while !isempty(work)
        (pkg, qual, bare) = popfirst!(work)
        qn = "$pkg/$qual/$bare"
        qn in seen && continue
        push!(seen, qn)
        path = _resolve_one_iface(pkg, qual, bare, roots)
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

# Lift a `from=` argument value (a string literal or a `["a", "b"]` vector literal)
# to its list of root strings. Non-literals error — roots are resolved at macro time.
function _from_literals(val)
    if val isa AbstractString
        return String[String(val)]
    elseif val isa Expr && val.head === :vect && all(x -> x isa AbstractString, val.args)
        return String[String(x) for x in val.args]
    else
        error("@ros_import: `from=` takes a string literal or a vector of string \
               literals, e.g. `from=\"interfaces\"` or `from=[\"a\", \"b\"]`")
    end
end

# Emit, per provided hit, `module <pkg> module <qual> const <Name> = <T> end end`
# so the caller's `<pkg>.<qual>.<Name>` path *aliases* an already-compiled struct `T`
# (one wire type ⇒ one Julia struct, §11/D5) instead of minting a duplicate. `T` is
# the resolved type spliced in literally — ROSNode's vendored canonical copy, or a
# type a dependency already `@ros_import`ed — so it's an external reference (single
# copy, even across a precompile boundary). One module pair per (pkg, qual).
function _alias_block(hits::Vector{Tuple{String, String, String, Any}})
    bypkg = Dict{String, Dict{String, Vector{Tuple{String, Any}}}}()
    pkg_order = String[]
    seen = Set{Tuple{String, String, String}}()
    for (pkg, qual, bare, T) in hits
        (pkg, qual, bare) in seen && continue
        push!(seen, (pkg, qual, bare))
        haskey(bypkg, pkg) || (push!(pkg_order, pkg); bypkg[pkg] = Dict{String, Vector{Tuple{String, Any}}}())
        push!(get!(bypkg[pkg], qual, Tuple{String, Any}[]), (bare, T))
    end
    out = Any[]
    for pkg in pkg_order
        qualmods = Any[]
        for (qual, bares) in bypkg[pkg]
            consts = [Expr(:const, Expr(:(=), Symbol(bare), T)) for (bare, T) in bares]
            push!(qualmods, Expr(:module, true, Symbol(qual), Expr(:block, consts...)))
        end
        push!(out, Expr(:module, true, Symbol(pkg), Expr(:block, qualmods...)))
    end
    return out
end

# Resolve `names` to an import plan
# `(alias_hits, gen_paths, gen_jsons, alias_ifaces, byo_deps)`: provided sections to
# alias (`[(pkg, qual, bare, Type)]`) vs. files to generate locally and their
# registration JSON, plus the BYO (`from=`) source paths to register as precompile
# dependencies (aliased *and* generated — editing a BYO file must re-trigger even if
# it currently aliases away). Shared by the bare and `as` forms.
function _import_plan(names::Vector{String}, roots::Vector{String}=String[])
    specs = _resolve_import_closure(names, roots)
    isempty(specs) &&
        error("@ros_import: no sources resolved for $(names) — vendored, in a sourced \
               ROS2 env (AMENT_PREFIX_PATH), or under a `from=` root")
    tds = _static_type_descriptions([(s.package, s.il) for s in specs])
    hashof = Dict{String, Any}()
    jsonof = Dict{String, String}()
    for (pkg, qual, bare, tdmsg) in tds
        qn = "$pkg/$qual/$bare"
        h = type_hash_from_rihs_string(calculate_rihs01_hash(tdmsg))
        h === nothing && continue
        hashof[qn] = h
        jsonof[qn] = to_ros2_json(tdmsg)
    end
    prov(qn) = (h = get(hashof, qn, nothing); h === nothing ? nothing : provided_type(qn, h))
    secs(s) = [(s.package, qual, bare) for (qual, bare, _) in _il_sections(s.il)]
    # A msg/srv whose every section is already provided aliases; everything else
    # (incl. actions — their implicit protocol types aren't provided) generates.
    aliasable(s) = s.qualifier in ("msg", "srv") &&
                   all(((p, q, b),) -> prov("$p/$q/$b") !== nothing, secs(s))
    alias_hits   = Tuple{String, String, String, Any}[]
    alias_ifaces = Tuple{String, String, String}[]   # provided srv/action, for Foo.* namespacing
    gen_paths    = String[]
    gen_qns      = Set{String}()
    for s in specs
        if aliasable(s)
            for (p, q, b) in secs(s)
                push!(alias_hits, (p, q, b, prov("$p/$q/$b")))
            end
            s.qualifier in ("srv", "action") && push!(alias_ifaces, (s.package, s.qualifier, s.bare))
        else
            push!(gen_paths, s.path)
            for (p, q, b) in secs(s); push!(gen_qns, "$p/$q/$b"); end
        end
    end
    gen_jsons = [(p, q, b, jsonof["$p/$q/$b"]) for (p, q, b, _) in tds if "$p/$q/$b" in gen_qns]
    # Precompile deps for BYO sources: every resolved path under a `from=` root (so an
    # aliased-away BYO file still re-triggers on edit), plus the package dir of any
    # bare-package import that hit a root (so adding/removing a file regenerates).
    byo_deps = String[s.path for s in specs if _under_roots(s.path, roots)]
    for name in names, root in roots
        '/' in name && continue                      # bare package only
        pdir = joinpath(root, name)
        isdir(pdir) && push!(byo_deps, pdir)
    end
    return (alias_hits, unique(gen_paths), gen_jsons, alias_ifaces, unique(byo_deps))
end

# True if `path` lives under one of the BYO `from=` roots (vs. vendored/ament). The
# trailing separator stops a sibling dir (`/a/bc`) from matching root `/a/b`.
_under_roots(path::AbstractString, roots::Vector{String}) =
    any(r -> startswith(normpath(path), normpath(r) * "/"), roots)

# Map a module body's child `module` exprs by name. A module expr is
# `Expr(:module, std_imports, name, body)`; we key the `body`'s sub-`module`s.
_child_modules(body::Expr) =
    Dict{Symbol, Expr}(c.args[2] => c for c in body.args if c isa Expr && c.head === :module)

# Merge `src`'s body into `dst`'s body (both `module name (block …)`): same-named
# sub-modules merge recursively (e.g. two `module msg`s — alias consts + generated
# structs coexist); everything else from `src` is *prepended* so aliased sections
# precede generated ones that may reference them within the same package.
function _merge_module!(dst::Expr, src::Expr)
    dstbody, srcbody = dst.args[3], src.args[3]
    dstsubs = _child_modules(dstbody)
    bring = Any[]
    for c in srcbody.args
        if c isa Expr && c.head === :module && haskey(dstsubs, c.args[2])
            _merge_module!(dstsubs[c.args[2]], c)
        else
            push!(bring, c)
        end
    end
    prepend!(dstbody.args, bring)
    return dst
end

# Append a plan's code to `block`. Provided-type aliases for a package that is *also*
# generated are merged into that package's generated `module` (a second `module pkg`
# at the same scope would clobber the first, dropping the aliased sections); aliases
# for packages with no generated section stay standalone and are emitted *before* the
# generated modules, which reference them by path. A provided srv/action also gets its
# `Foo.Request`/`Foo.Goal` namespace (generated ones get it from `_expand_msg_files`).
function _plan_block!(block, plan)
    (alias_hits, gen_paths, gen_jsons, alias_ifaces, byo_deps) = plan
    aliases = _alias_block(alias_hits)
    ROSMessages._inject_namespaces!(aliases, alias_ifaces)
    if isempty(gen_paths)
        append!(block.args, aliases)
    else
        gen = ROSMessages._expand_msg_files(gen_paths)       # structs + include_dependency
        genmods = _child_modules(Expr(:block, gen.args...))  # generated top-level modules by pkg
        for a in aliases                                     # each is `module <pkg> …`
            g = get(genmods, a.args[2], nothing)
            g === nothing ? append!(block.args, [a]) : _merge_module!(g, a)
        end
        append!(block.args, gen.args)
        append!(block.args, _static_register_stmts(gen_jsons, nothing))
    end
    # Track BYO sources so edits/additions invalidate precompile (generated paths get
    # this from `_expand_msg_files`; aliased BYO files and bare-package dirs don't).
    for d in byo_deps
        push!(block.args, :($Base.include_dependency($d)))
    end
    return block
end

"""
    @ros_import "pkg" | "pkg/qual/Name" ...
    @ros_import "pkg/qual/Name" as Alias
    @ros_import from="dir" "pkg/qual/Name" ...

Statically generate ament/vendored ROS interface types *resolved by name* into the
calling module (the static counterpart to dynamic discovery). Each name is a package
(`"sensor_msgs"` — all its interfaces) or a fully-qualified type
(`"sensor_msgs/msg/Imu"`); the transitive reference closure (e.g. `std_msgs/Header`)
is pulled automatically. Sources are found in ROSNode's vendored dir and, when inside
a sourced ROS2 env, `AMENT_PREFIX_PATH`.

**BYO interfaces (`from=`).** Pass `from="dir"` (or `from=["a", "b"]`) to add local
search roots for your own `.msg`/`.srv`/`.action` files, then reference them by name
like any other type — they go through the *same* registration pipeline (RIHS01,
single-copy aliasing, the §13 server), unlike raw `@ros_msg`/`@ros_msgs` which only
emit structs. A root is a *parent-of-packages* dir laid out the ROS way,
`<root>/<pkg>/<qual>/<Name>.<ext>` (e.g. `interfaces/robot_msgs/msg/Widget.msg` →
`@ros_import from="interfaces" "robot_msgs/msg/Widget"`). Per name the search order is
`from`-roots (in order) → vendored → ament, so a BYO package can supply types a
sourced env lacks or shadow one it has; a dep not found in any root falls through the
same way. Relative roots resolve against the source file (like `@ros_msg`). The roots'
files are tracked as precompile dependencies, so edits re-trigger generation.

Generation reuses the same pipeline as [`@ros_msg`](@ref) (structs +
`include_dependency`); types land at `<pkg>.<qual>.<Name>`. They are additionally
**auto-registered** (real RIHS01) on Context creation, so a keyexpr-only
`Subscription(node, "/t")` uses the precompiled type with no runtime codegen, and the
§13 server can serve their descriptions. No JSON caching — ament/vendored is the
durable source.

**Binding (Julia-`import`-like).** A fully-qualified *message* type binds its bare
leaf, like `import Mod: Name` — `@ros_import "sensor_msgs/msg/Image"` gives `Image`
directly (and `sensor_msgs.msg.Image` too, since `sensor_msgs` is in scope). A bare
package binds the module, like `import Mod` — `@ros_import "sensor_msgs"` gives
`sensor_msgs` (use `sensor_msgs.msg.X`). Transitively-pulled deps are *not* leaf-bound
(no namespace flood). Two imports binding the same leaf error, pointing at `as`.

**Single-copy:** any type whose `(name, RIHS01)` is already provided by a loaded
module — the vendored canonical copy, or one a dependency `@ros_import`ed (see
[`provided_type`](@ref)) — is *aliased* to that one struct rather than re-generated.
So transitively-pulled commons are shared, a second import re-aliases instead of
redefining, and a type package C imports is reused by downstream A and B (provided C
is loaded when they expand the macro).

`… as Alias` binds the imported type to `Alias` instead of its `pkg.qual.Name` path —
for a short handle, or to disambiguate a name collision (two same-named but
different-RIHS01 versions). A *generated* `as` type is isolated in a hidden submodule
so its path can't clash with another import; a *provided* one binds straight through.
"""
macro ros_import(names...)
    # Split into bare imports, `"pkg/qual/Name" as Alias` triples, and `from=` roots.
    bare = String[]
    aliased = Tuple{String, Symbol}[]
    rootsraw = String[]
    i = 1
    while i <= length(names)
        n = names[i]
        if n isa Expr && n.head in (:(=), :kw) && n.args[1] === :from
            append!(rootsraw, _from_literals(n.args[2])); i += 1
        elseif i + 2 <= length(names) && names[i + 1] === :as
            (n isa AbstractString && names[i + 2] isa Symbol) ||
                error("@ros_import: the `as` form is `@ros_import \"pkg/qual/Name\" as Alias`")
            push!(aliased, (String(n), names[i + 2])); i += 3
        elseif n isa AbstractString
            push!(bare, String(n)); i += 1
        else
            error("@ros_import takes string literals (optionally `\"pkg/qual/Name\" as Alias` " *
                  "or `from=\"dir\"`)")
        end
    end
    (isempty(bare) && isempty(aliased)) && error("@ros_import: nothing to import")
    # Resolve `from=` roots against the source file (like `@ros_msg`), so a relative
    # root is relative to the file the macro is written in, not the cwd.
    roots = String[ROSMessages._resolve_macro_path(__source__, r) for r in rootsraw]

    block = Expr(:toplevel)
    isempty(bare) || _plan_block!(block, _import_plan(bare, roots))

    # Hybrid binding: an explicitly-listed fully-qualified *message* type also binds
    # its bare leaf name (like `import Mod: Name`) — so `@ros_import "pkg/msg/Image"`
    # gives both `Image` and `pkg.msg.Image`. A bare package binds only the module
    # (`import Mod`), and transitively-pulled deps are never leaf-bound (no namespace
    # flood). The leaf points at the already-emitted `pkg.msg.Name` path (an alias for
    # a provided type, the struct for a generated one), so identity is unchanged.
    leaves = Dict{Symbol, String}()
    for n in bare
        parts = split(n, '/')
        (length(parts) == 3 && parts[2] in ("msg", "srv", "action")) || continue
        pkg, qual, name = parts
        sym = Symbol(name)
        if haskey(leaves, sym)
            error("@ros_import: `$name` is imported from both `$(leaves[sym])` and " *
                  "`$n`; use `@ros_import \"$n\" as <Alias>` to disambiguate")
        end
        leaves[sym] = n
        # msg → the struct `pkg.msg.Name`; srv/action → the `pkg.qual.Name` namespace
        # module (so `Name.Request`/`Name.Goal`). Both already emitted above.
        target = Expr(:., Expr(:., Symbol(pkg), QuoteNode(Symbol(qual))), QuoteNode(sym))
        push!(block.args, Expr(:const, Expr(:(=), sym, target)))
    end

    for (tname, asym) in aliased
        count(==('/'), tname) == 2 ||
            error("@ros_import … as $(asym): need a fully-qualified \"pkg/qual/Name\", got \"$tname\"")
        pkg, qual, bare_name = split_ros_name(tname)
        qual == "msg" ||
            error("@ros_import … as $(asym): `as` aliasing is only for message types " *
                  "(\"pkg/msg/Name\"); a srv/action has no single struct to bind, got \"$tname\"")
        (alias_hits, gen_paths, _, _, _) = plan = _import_plan([tname], roots)
        if isempty(gen_paths)
            # Fully provided: bind `Alias` straight to the provided type (no submodule).
            j = findfirst(h -> (h[1], h[2], h[3]) == (pkg, qual, bare_name), alias_hits)
            j === nothing && error("@ros_import … as $(asym): could not resolve \"$tname\"")
            push!(block.args, Expr(:const, Expr(:(=), asym, alias_hits[j][4])))
        else
            # Generated: isolate the type (+ its closure) in a hidden submodule so its
            # `pkg.qual.Name` path can't clash with another import, then expose `Alias`.
            modname = Symbol("__ros_alias_", asym)
            inner = _plan_block!(Expr(:toplevel), plan)
            pushfirst!(inner.args, :(using ROSNode))   # the register stmts call ROSNode.…
            push!(block.args, Expr(:module, true, modname, Expr(:block, inner.args...)))
            target = Expr(:., Expr(:., Expr(:., modname, QuoteNode(Symbol(pkg))),
                                   QuoteNode(Symbol(qual))), QuoteNode(Symbol(bare_name)))
            push!(block.args, Expr(:const, Expr(:(=), asym, target)))
        end
    end
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
    # Ensure the cache dir exists before depending on it: `include_dependency` errors
    # on a missing path, and on the first run (empty cache) the dir does not exist yet.
    # Creating it here registers the dependency from run 1, so a cache *grown* this run
    # invalidates precompilation and re-bakes next load (the D5/D9 convergence).
    isdir(dir) || mkpath(dir)

    blobs = sort!(filter(f -> endswith(f, ".json"), readdir(dir; join=true)))
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
