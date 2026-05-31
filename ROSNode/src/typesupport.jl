# §11 Type support — the runtime type registry and its three acquisition paths.
# The IL is the hub: every way a type enters (static `@ros_msgs`, ament scan of an
# installed workspace, or dynamic GetTypeDescription over the wire) normalizes into
# `ROSMessages.IL`, and codegen/hashing/persistence all read from it.
#
# The registry (context.jl owns the storage + lock) is keyed by `(name, RIHS01)` so
# evolved versions of a type coexist — the hash is the decode-safety key, not just a
# version tag. This file owns the entry *shape* (`RegistryEntry`), the acquisition
# front-ends, the content-addressed on-disk cache, and `export_typesupport`.
#
# Runtime codegen runs the existing macro pipeline programmatically:
#   lift → lower → resolve_constants → generate_code → eval into a fresh module.
# Because a dynamic type is born at runtime (a newer world age than the compiled
# dispatcher), its decode/handler calls cross the boundary via `invokelatest` — the
# "static is fast, dynamic is correct-but-boxed" line. The programmatic eval entry
# is the one piece that genuinely depends on a clean codegen module target; where it
# isn't fully wired it is marked `# TODO(§11):` and does not break precompilation.

using ROSMessages: ROSMessages, IL, lift, lower, message_il,
                   TypeDescription, TypeDescriptionMsg, FieldDescription,
                   FieldTypeDescription,
                   type_description_from_struct, calculate_rihs01_hash,
                   to_ros2_json
import IDLParser
using ROSZenoh: ROSZenoh, TypeInfo, TypeHash, to_rihs_string,
                type_hash_from_rihs_string

# `register_type!` / `lookup_type` / `TypeRegistry` / `registry` are context.jl's;
# `type_info` / `ros_type_name` are serialization.jl's (this file specializes the
# former per registered type). All are already in the module's scope by include
# order, so no `import` is needed.

export TypeRegistry, RegistryEntry, register_type!, lookup_type,
       resolve_type, export_typesupport,
       ament_prefix_paths, discover_ament_packages, load_ament_type,
       enable_project_cache!, disable_project_cache!, absorb_static_types!,
       warm_codegen!

# ── registry entry shape (§11) ────────────────────────────────────────────
# The value stored in the Context's `(name, hash)`-keyed table. Holds the IL (the
# hub form), the wire `TypeDescription` blob (source of truth for persistence and
# the `~/get_type_description` server, §15), a provenance tag, and — once realized
# — the generated `Module` and the type within it reached via `invokelatest`.

"""
    RegistryEntry

A single type registry record (§11). Keyed in the [`TypeRegistry`](@ref) by
`(type_name, RIHS01-hash)`. Carries:

- `info` — the `TypeInfo` (qualified name + hash) this entry answers for.
- `il` — the `ROSMessages.IL` (`RMessage`/`RService`/`RAction`), the hub form
  every consumer reads from.
- `td` — the wire `TypeDescriptionMsg` (main + referenced closure), the source of
  truth for persistence and the type-description service; `nothing` for a
  statically-acquired type that never carried one.
- `provenance` — `:static` / `:ament` / `:wire` / `:cache`, where the type came
  from (orthogonal to how it is used).
- `mod` — the generated `Module` once codegen has run (lazy; `nothing` until
  realized), and `type` the concrete struct within it — reached via
  `Base.invokelatest` because it is born in a newer world age.
"""
mutable struct RegistryEntry
    const info::TypeInfo
    const il::Any                       # IL.RMessage | IL.RService | IL.RAction
    const td::Union{TypeDescriptionMsg, Nothing}
    const provenance::Symbol
    mod::Union{Module, Nothing}         # generated module, realized lazily
    type::Any                           # the concrete type within `mod`
end

RegistryEntry(info::TypeInfo, il; td=nothing, provenance::Symbol=:wire) =
    RegistryEntry(info, il, td, provenance, nothing, nothing)

Base.show(io::IO, e::RegistryEntry) =
    print(io, "RegistryEntry(", e.info.name, ", ", to_rihs_string(e.info.hash),
          ", ", e.provenance, e.mod === nothing ? "" : ", realized", ")")

# ── ROS2 name decomposition ───────────────────────────────────────────────
# A fully-qualified ROS2 name is `<package>/<qualifier>/<Name>` (qualifier ∈
# {msg, srv, action}). Codegen's `lower` wants the package; the cache and ament
# scan want the package + qualifier + bare name.

"""
    split_ros_name(name) -> (package, qualifier, bare)

Decompose a fully-qualified ROS2 type name `"<pkg>/<qual>/<Name>"` into its parts.
A two-segment `"<pkg>/<Name>"` is read as `qualifier == "msg"` (the common
abbreviation); a bare `"Name"` yields empty package/qualifier. The inverse of the
`<pkg>/<qual>/<Name>` assembly the codegen and keyexpr layers build.
"""
function split_ros_name(name::AbstractString)
    parts = split(name, '/')
    if length(parts) >= 3
        return (String(parts[1]), String(parts[end-1]), String(parts[end]))
    elseif length(parts) == 2
        return (String(parts[1]), "msg", String(parts[2]))
    else
        return ("", "", String(name))
    end
end

# ── codegen: IL → generated module via the macro pipeline (§11) ─────────────
# The dynamic-type birth path. `lower(il; package)` → `resolve_constants` →
# `generate_code` produces the same `Expr` vector `@ros_msgs` splices at macro
# time; we `eval` it into a fresh anonymous module so a runtime type lives in a
# clean namespace (no name clash with a statically-included one). The generated
# code defines nested `<package>.<qualifier>.<Name>` submodules, so the concrete
# type is fetched back out by walking that path.

# Run lower → resolve → generate over one IL interface and return the `Expr`s.
# `package` threads through `lower` so cross-package `RRef`s project correctly.
# `lower` yields a loosely-typed `Vector{Any}`; `resolve_constants` dispatches on
# `Vector{<:Parse.CanAnnotate{Parse.Decl}}`, so we retype into `Parse.Decl[]` (what
# the `@ros_msgs` macro feeds it) before the call.
function _generate_exprs(il, package::AbstractString)
    decls = IDLParser.Parse.Decl[]
    append!(decls, lower(il; package=package))
    resolved = IDLParser.ConstResolution.resolve_constants(decls)
    return IDLParser.Generation.generate_code(resolved)
end

# Eval an interface's generated code into a fresh module and return that module.
# The generated package module emits `import StaticArrays, CDRSerialization`
# (gen.jl) — both of which resolve only relative to the eval target's *root
# package* project. ROSNode's project lists CDRSerialization but not StaticArrays
# (it's transitive), so a module rooted under ROSNode can't `import StaticArrays`.
# CDRSerialization's project *does* list both, so we root the gensym'd target under
# `CDRSerialization`: the generated imports resolve, and the generated CDR macros
# (`@cdr1_compat`/`@cdr_compact`) are in scope. The module is gensym-named so it
# can't clash with a static include.
#
# TODO(§11): rooting under CDRSerialization means dynamic types accumulate as
# hidden submodules there rather than under ROSNode. Acceptable (they're
# gensym-isolated and reached via invokelatest), but a dedicated runtime-codegen
# root module with StaticArrays as a direct dep would localize them — pin once the
# codegen entry exposes an explicit target-module parameter.
function _eval_module(il, package::AbstractString)
    name = gensym(isempty(package) ? :ros_dynamic : Symbol(package))
    target = Core.eval(CDRSerialization, :(module $name end))
    exprs = _generate_exprs(il, package)
    for ex in exprs
        Core.eval(target, ex)
    end
    return target
end

# Fetch the concrete generated type out of an eval'd module by walking the
# `<package>.<qualifier>.<Name>` path the codegen builds. For a service/action the
# `bare` is a section name (`Foo_Request`, `Foo_Goal`, …) and the qualifier is
# `srv`/`action`; for a plain message it's `msg`. Returns `nothing` if the path
# isn't present (a partially-generated or unexpectedly-shaped module).
#
# The bindings are younger than the running world, so `isdefined`/`getfield` go
# through `invokelatest` (Julia 1.12+ rejects a prior-world binding access
# otherwise). This is the realize-time half of the §11 "dynamic = invokelatest"
# rule; the per-message decode/handler hop is the other half (the dispatch layer's).
function _fetch_generated_type(mod::Module, name::AbstractString)
    package, qualifier, bare = split_ros_name(name)
    cur = mod
    for seg in (package, qualifier)
        isempty(seg) && continue
        sym = Symbol(seg)
        Base.invokelatest(isdefined, cur, sym) || return nothing
        cur = Base.invokelatest(getfield, cur, sym)
        cur isa Module || return nothing
    end
    sym = Symbol(bare)
    Base.invokelatest(isdefined, cur, sym) || return nothing
    return Base.invokelatest(getfield, cur, sym)
end

"""
    realize!(entry::RegistryEntry) -> RegistryEntry

Run the codegen pipeline for `entry` (once) so its generated `Module`/`type` are
available. Idempotent — a second call is a no-op once `entry.mod` is set. The
generated type is reached by callers via `Base.invokelatest` (it is born in a
newer world age than the compiled dispatcher, §11).

A codegen/eval failure is logged and leaves the entry unrealized (`mod`/`type`
stay `nothing`) so a single bad type can't abort discovery; the subscription path
then falls back to raw bytes for that topic (DESIGN §until-ready).
"""
function realize!(entry::RegistryEntry)
    entry.mod === nothing || return entry
    package, _, _ = split_ros_name(entry.info.name)
    try
        mod = _eval_module(entry.il, package)
        entry.mod = mod
        # A message has one umbrella struct to bind as `entry.type`; a service /
        # action has *sections* (`Foo_Request`/`Foo_Goal`/…) and no umbrella type,
        # so leave `entry.type === nothing` and let the service/action layer fetch
        # its sections from `entry.mod` (no warning — that's the expected shape).
        if entry.il isa IL.RMessage
            entry.type = _fetch_generated_type(mod, entry.info.name)
            if entry.type === nothing
                @warn "typesupport: codegen produced no type for $(entry.info.name); \
                       delivered raw bytes for this topic"
            else
                _record_type_entry!(entry)     # type → entry, so type_info_of recovers the real hash
            end
        end
    catch err
        @error "typesupport: codegen/eval failed for $(entry.info.name)" exception=(err, catch_backtrace())
    end
    return entry
end

# ── codegen warm-up (D8-aligned; guards the dynamic-discovery GC stall) ───────
# The first runtime codegen in a process (`lower → generate_code → eval`) JITs the
# whole generation pipeline and allocates heavily. If that first codegen runs
# *concurrently with heavy Zenoh I/O* (a busy libzenohc thread that has entered
# Julia), its allocation can stall on GC waiting for that thread to reach a
# safepoint — a hang. So we warm the pipeline ONCE while idle (at keyexpr-only
# subscription creation, before data flows): the JIT is paid up front, and each
# per-sample `realize!` during live traffic is then fast and light, opening no GC
# window to stall on. Idempotent + best-effort.
const _CODEGEN_WARMED = Ref(false)
const _WARM_LOCK = ReentrantLock()

"""
    warm_codegen!() -> nothing

Pre-JIT the runtime type-generation pipeline (once per process) while idle, so a
later [`realize!`](@ref) during live Zenoh traffic is fast and can't stall on GC
against a busy transport thread. Called automatically when a keyexpr-only
`Subscription` is created; safe to call eagerly. Idempotent.
"""
function warm_codegen!()
    @lock _WARM_LOCK begin
        _CODEGEN_WARMED[] && return nothing
        _CODEGEN_WARMED[] = true
    end
    try
        _eval_module(message_il("uint8 _b\nstring _s\n"; name="_Warm"), "_ros_warmup")
    catch err
        @debug "typesupport: codegen warm-up failed" exception=(err, catch_backtrace())
    end
    return nothing
end

# ── dynamic acquisition: TypeDescription → registry entry (§11) ─────────────
# The over-the-wire path's back half. The wire side (a GetTypeDescription client
# call) yields a `TypeDescriptionMsg`; this verifies its hash against the wire
# `TypeInfo`, lifts main + referenced closure to IL, and builds the entry. The
# *front* half (issuing the GetTypeDescription `call` on endpoint sighting) is the
# §12/§15 discovery layer's — it has the `ServiceClient` machinery and the remote
# node identity; this is the codegen-facing seam it hands the result to.

"""
    verify_type_description(td::TypeDescriptionMsg, info::TypeInfo) -> Bool

The integrity gate (§11): recompute `calculate_rihs01_hash` over the *raw* received
`TypeDescriptionMsg` and check it equals `info`'s wire RIHS01. A mismatch means the
remote sent a definition that doesn't match the hash it advertised — never trust it
to decode. Hashing the raw `td` (not a lift→lower roundtrip) is deliberate: the
hash ignores constants/defaults that `lift` drops, so the gate is exact.
"""
function verify_type_description(td::TypeDescriptionMsg, info::TypeInfo)
    expected = to_rihs_string(info.hash)
    got = calculate_rihs01_hash(td)
    return got == expected
end

"""
    entry_from_type_description(td::TypeDescriptionMsg, info::TypeInfo;
                                provenance=:wire, verify=true) -> RegistryEntry

Build a [`RegistryEntry`](@ref) from a wire `TypeDescriptionMsg` (§11). With
`verify=true` (the default) the hash gate runs first and a mismatch throws — a
discovered type must hash-match the name it travels under. `lift` reconstructs the
main IL; nested types survive as `RRef`s in its fields, so the referenced closure
rides in `td` for codegen to resolve, not as separate entries.

The entry is *not* realized here (codegen is deferred to [`realize!`](@ref) /
first use) so registration is cheap and a codegen failure doesn't block discovery.
"""
function entry_from_type_description(td::TypeDescriptionMsg, info::TypeInfo;
                                     provenance::Symbol=:wire, verify::Bool=true)
    if verify && !verify_type_description(td, info)
        throw(ArgumentError("type description for $(info.name) failed the RIHS01 \
                             integrity gate (got $(calculate_rihs01_hash(td)), \
                             expected $(to_rihs_string(info.hash)))"))
    end
    il = lift(td)
    return RegistryEntry(info, il; td=td, provenance=provenance)
end

"""
    register_type_description!(reg, td::TypeDescriptionMsg, info; kwargs...) -> RegistryEntry

Build (with the hash gate) and register a wire type description under
`(info.name, info.hash)`, returning the entry (§11). The dynamic-discovery
landing point: a GetTypeDescription reply is verified, lifted, and registered in
one call.

**Caches only `:wire` provenance** (best-effort): the content-addressed JSON cache
exists solely to skip re-discovering *dynamically-discovered* types across runs.
Types acquired via ament or vendoring are statically loadable (`@ros_msgs`) and
resolve directly, so they are deliberately never written to the cache — even if a
caller passes `cache=true` with a non-`:wire` provenance.
"""
function register_type_description!(reg::TypeRegistry, td::TypeDescriptionMsg,
                                    info::TypeInfo; provenance::Symbol=:wire,
                                    cache::Bool=true)
    entry = entry_from_type_description(td, info; provenance=provenance)
    register_type!(reg, info, entry)
    # Only dynamically (wire) discovered types are persisted — see the docstring.
    cache && provenance === :wire && _cache_store(info, td)
    return entry
end

# ── ament / colcon acquisition (static, no wire) ────────────────────────────
# The "I'm in a sourced workspace" path: scan `AMENT_PREFIX_PATH` for installed
# interface packages and parse their `share/<pkg>/{msg,srv,action}/*` files
# straight to IL — same as feeding them to `@ros_msgs`, but resolved by
# package/type name at runtime with no codegen until first use.

"""
    ament_prefix_paths() -> Vector{String}

The `AMENT_PREFIX_PATH` entries (colon-separated, ROS's install-prefix search
path), filtered to existing directories. Empty when not in a sourced workspace.
"""
function ament_prefix_paths()
    raw = get(ENV, "AMENT_PREFIX_PATH", "")
    isempty(raw) && return String[]
    return String[p for p in split(raw, ':') if !isempty(p) && isdir(p)]
end

# An interface file under a prefix: `<prefix>/share/<pkg>/{msg,srv,action}/<X>.ext`.
const _IFACE_EXTS = (".msg", ".srv", ".action")

"""
    discover_ament_packages() -> Dict{String, Vector{String}}

Scan `AMENT_PREFIX_PATH` for installed interface packages, mapping each package
name to the absolute paths of its `.msg`/`.srv`/`.action` files (§11 ament
acquisition). A package is "interface" iff its `share/<pkg>` dir has at least one
of those files. Later prefixes don't override earlier ones — first-seen wins, the
ament overlay convention.
"""
function discover_ament_packages()
    out = Dict{String, Vector{String}}()
    for prefix in ament_prefix_paths()
        share = joinpath(prefix, "share")
        isdir(share) || continue
        for pkg in readdir(share)
            pkgdir = joinpath(share, pkg)
            isdir(pkgdir) || continue
            haskey(out, pkg) && continue          # first prefix wins (overlay)
            files = String[]
            for qual in ("msg", "srv", "action")
                qdir = joinpath(pkgdir, qual)
                isdir(qdir) || continue
                for f in sort!(readdir(qdir))
                    splitext(f)[2] in _IFACE_EXTS || continue
                    push!(files, joinpath(qdir, f))
                end
            end
            isempty(files) || (out[pkg] = files)
        end
    end
    return out
end

# Locate the interface file for a fully-qualified ROS2 name across the ament
# prefixes: `<prefix>/share/<pkg>/<qual>/<Name>.<ext>`. Returns the first match
# (overlay order) or `nothing`.
function _find_ament_file(name::AbstractString)
    package, qualifier, bare = split_ros_name(name)
    isempty(package) && return nothing
    ext = qualifier == "srv" ? ".srv" : qualifier == "action" ? ".action" : ".msg"
    for prefix in ament_prefix_paths()
        path = joinpath(prefix, "share", package, qualifier, bare * ext)
        isfile(path) && return path
    end
    return nothing
end

"""
    load_ament_type(reg, name; register=true) -> Union{RegistryEntry, Nothing}

Resolve a fully-qualified ROS2 type `name` against the installed ament workspace
(§11): find its `.msg`/`.srv`/`.action` file, parse it to IL, compute the RIHS01
hash from the parsed AST, and (with `register=true`) register the entry under
`(name, hash)`. Returns `nothing` when the type isn't installed.

The hash is computed here (the file carries the definition, not the hash) so an
ament-acquired type is keyed identically to a wire-discovered one — RIHS01 is the
common identity. Nested types are *not* recursively resolved here; codegen
resolves refs when the package's siblings are co-registered (the typical
`@ros_msgs`-over-a-package shape).
"""
function load_ament_type(reg::TypeRegistry, name::AbstractString; register::Bool=true)
    path = _find_ament_file(name)
    path === nothing && return nothing
    package, qualifier, bare = split_ros_name(name)

    src = read(path, String)
    il = if qualifier == "srv"
        ROSMessages.service_il(src; name=bare)
    elseif qualifier == "action"
        ROSMessages.action_il(src; name=bare)
    else
        ROSMessages.message_il(src; name=bare)
    end

    info = _il_type_info(il, package, qualifier, name)
    entry = RegistryEntry(info, il; provenance=:ament)
    register && register_type!(reg, info, entry)
    return entry
end

# RIHS01 for an IL interface: lower to decls, find the primary struct AST, and run
# ROSZenoh's `type_info_from_struct` (which threads it through
# `type_description_from_struct` + `calculate_rihs01_hash`). The struct-AST walk
# uses ROSMessages' own `lift(decls)`→struct-recovery seam rather than reaching
# into the Moshi `@data` variants directly (those need the `@match` macro, which
# isn't a ROSNode dependency).
#
# TODO(§11): nested-ref `references` collection for byte-parity hashing of types
# that reference other packages — needs the resolved + sorted sibling
# TypeDescriptions. Until that closure is walked, the hash is exact for
# self-contained types; a referencing type still keys correctly by name (always
# exact) but its hash may differ from a remote's until refs are co-registered, in
# which case the wire/`td` path (exact by construction) takes over.
function _il_type_info(il, package::AbstractString, qualifier::AbstractString,
                       name::AbstractString)
    struct_ast = _primary_struct(il, package)
    if struct_ast === nothing
        # No single struct to hash (or the AST shape was unexpected): key by name
        # with the Humble placeholder hash. Correct for keyexpr *structure*; the
        # wire/cache path supplies the exact hash when it matters (§11).
        return TypeInfo(name, TypeHash())
    end
    bare = split_ros_name(name)[3]
    return ROSZenoh.type_info_from_struct(struct_ast, bare;
                                          package=package, qualifier=qualifier)
end

# The primary `StructDecl` AST for an IL interface, recovered by lowering then
# scanning the (possibly module-wrapped) decl vector. Variants are discriminated by
# `isa` against the Moshi `@data` *sum types* (`Parse.TypeDecl.Type` /
# `Parse.ModuleDecl.Type`) plus positional field access (`getproperty(d, i)`) —
# both stable across Moshi's emission without importing its `@match` macro. Every
# ROS-message type decl is a `StructDecl` (ROS interfaces have no IDL union/enum/
# typedef), so a `TypeDecl` here *is* the struct. Returns `nothing` if none found.
function _primary_struct(il, package::AbstractString)
    decls = lower(il; package=package)
    return _scan_for_struct(decls)
end

# Depth-first scan for the first type-decl struct; recurses through module wrappers.
function _scan_for_struct(decls)
    for d in decls
        if d isa IDLParser.Parse.TypeDecl.Type
            return d                            # ROS msg type decls are all StructDecl
        elseif d isa IDLParser.Parse.ModuleDecl.Type
            inner = getproperty(d, 2)           # MDecl(name::Symbol, decls::Vector)
            found = _scan_for_struct(inner)
            found === nothing || return found
        end
    end
    return nothing
end

# ── unified resolution: the registry-first lookup with acquisition fallbacks ─

"""
    resolve_type(node_or_ctx, info::TypeInfo; ament=true, cache=true) -> Union{RegistryEntry, Nothing}

The §11 acquisition front door: return the registry entry for `info`, acquiring it
if needed. Order: registry hit → content-addressed cache → ament/static lookup by
name. The dynamic over-the-wire path is *not* triggered here (it needs the remote
node identity and the async `ServiceClient`, the §12/§15 layer's seam — that layer
calls [`register_type_description!`](@ref) on its reply). Returns `nothing` when
the type can't be resolved locally; the caller then either kicks off dynamic
discovery or delivers raw bytes.

Acquired entries are *not* realized (codegen deferred); call [`realize!`](@ref) or
[`registered_type`](@ref) at first use.
"""
function resolve_type(ctxlike, info::TypeInfo; ament::Bool=true, cache::Bool=true)
    reg = registry(_ctx(ctxlike))

    hit = lookup_type(reg, info)
    hit isa RegistryEntry && return hit

    if cache
        td = _cache_load(info)
        if td !== nothing
            entry = register_type_description!(reg, td, info;
                                               provenance=:cache, cache=false)
            return entry
        end
    end

    if ament
        entry = load_ament_type(reg, info.name)
        entry === nothing || return entry
    end

    return nothing
end

"""
    registered_type(node_or_ctx, info::TypeInfo) -> Union{Type, Nothing}

The concrete (generated) Julia type for `info`, realizing its codegen on first
use. `nothing` when the type isn't registered/resolvable or its codegen failed —
the signal to deliver raw bytes (§11 until-ready). The decode/handler path reaches
the returned type via `Base.invokelatest` (newer world age).
"""
function registered_type(ctxlike, info::TypeInfo)
    entry = resolve_type(ctxlike, info)
    entry === nothing && return nothing
    realize!(entry)
    return entry.type
end

# ── content-addressed cache (project-local, opt-in) ─────────────────────────
# A store keyed by RIHS01, holding the wire `TypeDescription` JSON (the form
# `calculate_rihs01_hash` hashes over, so self-validating on load). Discovery
# checks it before the network; a fetch writes it back. Codegen is cheap, so we
# cache the *definition*, not generated code, and regenerate on load.
#
# Location & opt-in (D5, revised): the cache is a **project-local** folder (in the
# active project's dir), `.json` blobs keyed by RIHS01 hash. It is **off by default**
# — persistence is enabled by the user adding the [`@ros_cache`](@ref) macro to their
# module (which calls [`enable_project_cache!`](@ref) from its `__init__`), or by the
# `$ROS_TYPESUPPORT_CACHE` env override (ops/tests, force-enables with that dir).
# ROSNode is a library; discovered types are deployment-specific, so they belong in
# the importing project, owned by the user — not a package-global scratchspace.
# The JSON body is the exact `to_ros2_json` text plus the type name on a header line
# so a reload reconstructs the `TypeInfo` without re-parsing the name out of the blob.

# Cache opt-in state. `dir === nothing` ⇒ use the project-local default when enabled.
mutable struct _CacheState
    enabled::Bool
    dir::Union{String, Nothing}
end
const _CACHE = _CacheState(false, nothing)

# The project-local default cache dir: `<active-project-dir>/ros_typesupport`. Falls
# back to the cwd when no project is active (a bare `julia` REPL).
function _default_project_cache_dir()
    proj = Base.active_project()
    base = proj === nothing ? pwd() : dirname(proj)
    return joinpath(base, "ros_typesupport")
end

"""
    enable_project_cache!(dir = <project>/ros_typesupport) -> String

Turn on project-local persistence of dynamically-discovered type descriptions
(D5): discovery then writes `.json` `TypeDescription` blobs (keyed by RIHS01) under
`dir`, and resolution reads them before going to the wire. Off by default — the
[`@ros_cache`](@ref) macro calls this from its `__init__` so a project opts in by
adding the macro. Returns the (created) directory.
"""
function enable_project_cache!(dir::AbstractString=_default_project_cache_dir())
    _CACHE.enabled = true
    _CACHE.dir = String(dir)
    isdir(_CACHE.dir) || mkpath(_CACHE.dir)
    return _CACHE.dir
end

"Disable project-local type-description persistence (the default state)."
disable_project_cache!() = (_CACHE.enabled = false; nothing)

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

# Cache file for a hash: `<dir>/<64-hex>.json`. The bare hex (not the `RIHS01_`
# prefix) names the file — content-addressed by the 32-byte digest.
function _cache_path(info::TypeInfo)
    hex = bytes2hex(collect(info.hash.value))
    return joinpath(_cache_dir(), hex * ".json")
end

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

# Load + self-validate a cached `TypeDescriptionMsg` for `info`. Recomputes the
# RIHS01 over the parsed blob and discards on mismatch (a stale/corrupt entry can
# never decode wrong, DESIGN §cache). Returns `nothing` on miss, parse failure, or
# hash mismatch.
function _cache_load(info::TypeInfo)
    _cache_enabled() || return nothing
    path = _cache_path(info)
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
# hashing form); the inverse parser lives here because it's a cache/persistence
# concern, not a hashing one. We parse the fixed, hand-written shape directly
# rather than add a JSON dep — same posture ROSMessages takes writing it.

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

# ── export_typesupport: graduate a discovered type into durable form ─────────
# The explicit "port forward" half (vs. the automatic cache). Three formats per
# §11: `:msg` (an interface-package layout from `IL.unparse`), `:julia` (the
# `generate_code` output as `.jl` source — the type goes static/fast again), and
# `:typedesc` (the raw wire blob, language-agnostic). RIHS01 is the roundtrip
# invariant across all three.

"""
    export_typesupport(node_or_ctx, names; to=pwd(), format=:msg) -> Vector{String}

Graduate discovered (or any registered) types out of the ephemeral registry into
durable, user-owned form (§11). `names` is a type name (or iterable of them);
`format` selects the output:

- `:msg` / `:julia_text` — `IL.unparse` the IL into ROS interface text
  (`<to>/<package>/<qualifier>/<Name>.<ext>`). Most portable: `colcon build` turns
  it into a real package. (A *discovered* type lost its constants/defaults through
  RIHS01/`lift`, so the emitted text carries fields only — wire-faithful, lossy as
  docs.)
- `:julia` — emit `Generation.generate_code`'s output as `.jl` source
  (`<to>/<package>.jl`) to check in and `include`. The strongest port-forward: the
  once-dynamic type becomes static, precompilable, and back on the min-copy fast
  path.
- `:typedesc` — the raw `TypeDescription` JSON bundle (`<to>/<Name>.typedesc.json`);
  language-agnostic and reloadable by us.

Returns the paths written. Errors if a requested name isn't registered (resolve it
first via discovery/ament).
"""
function export_typesupport(ctxlike, names; to::AbstractString=pwd(),
                            format::Symbol=:msg)
    reg = registry(_ctx(ctxlike))
    namelist = names isa AbstractString ? (names,) : names
    written = String[]
    for name in namelist
        entry = _registered_entry_by_name(reg, String(name))
        entry === nothing &&
            throw(ArgumentError("export_typesupport: type $(name) is not registered \
                                 (discover or load it before exporting)"))
        append!(written, _export_one(entry, to, format))
    end
    return written
end

# Find a registered entry by name alone (export is name-addressed; if multiple
# hash versions coexist, the newest-registered wins — `Dict` insertion order
# isn't guaranteed, so we just take any match and note it). Returns `nothing` if
# unregistered.
function _registered_entry_by_name(reg::TypeRegistry, name::AbstractString)
    @lock reg.lock begin
        for ((n, _), v) in reg.entries
            n == name && v isa RegistryEntry && return v
        end
    end
    return nothing
end

function _export_one(entry::RegistryEntry, to::AbstractString, format::Symbol)
    if format === :typedesc
        return [_export_typedesc(entry, to)]
    elseif format === :julia
        return [_export_julia(entry, to)]
    elseif format === :msg || format === :julia_text
        return [_export_interface_text(entry, to)]
    else
        throw(ArgumentError("export_typesupport: unknown format $(repr(format)) \
                             (expected :msg, :julia, or :typedesc)"))
    end
end

# `:typedesc` — the raw wire blob as canonical JSON. Requires the entry to carry a
# `TypeDescriptionMsg` (wire/cache provenance); a purely-static entry has none.
function _export_typedesc(entry::RegistryEntry, to::AbstractString)
    entry.td === nothing &&
        throw(ArgumentError("type $(entry.info.name) has no TypeDescription to \
                             export as :typedesc (it was acquired statically)"))
    _, _, bare = split_ros_name(entry.info.name)
    isdir(to) || mkpath(to)
    path = joinpath(to, bare * ".typedesc.json")
    open(path, "w") do io
        println(io, entry.info.name)
        print(io, to_ros2_json(entry.td))
    end
    return path
end

# `:msg`/`:srv`/`:action` — `IL.unparse` into a ros interface-package layout. The
# qualifier/extension follow the IL kind. Constants/defaults are absent on a
# discovered type (RIHS01/`lift` dropped them); the text is fields-only but
# wire-faithful.
function _export_interface_text(entry::RegistryEntry, to::AbstractString)
    package, _, _ = split_ros_name(entry.info.name)
    qualifier, ext = _il_qualifier_ext(entry.il)
    _, _, bare = split_ros_name(entry.info.name)
    dir = joinpath(to, package, qualifier)
    isdir(dir) || mkpath(dir)
    path = joinpath(dir, bare * ext)
    open(path, "w") do io
        IL.unparse(io, entry.il)
    end
    return path
end

# `:julia` — the generated `.jl` source for the whole package, the strongest
# port-forward (the type becomes static/fast). Emits `generate_code`'s `Expr`s as
# pretty-printed source so the file is human-readable and `include`-able.
function _export_julia(entry::RegistryEntry, to::AbstractString)
    package, _, _ = split_ros_name(entry.info.name)
    exprs = _generate_exprs(entry.il, package)
    isdir(to) || mkpath(to)
    fname = isempty(package) ? split_ros_name(entry.info.name)[3] : package
    path = joinpath(to, fname * ".jl")
    open(path, "w") do io
        println(io, "# Generated by ROSNode export_typesupport(:julia) for ",
                entry.info.name)
        println(io, "# RIHS01: ", to_rihs_string(entry.info.hash))
        println(io, "using CDRSerialization")
        for ex in exprs
            println(io, ex)
            println(io)
        end
    end
    return path
end

# Map an IL interface kind to its (qualifier, file-extension) for `:msg` export.
_il_qualifier_ext(::IL.RMessage) = ("msg", ".msg")
_il_qualifier_ext(::IL.RService) = ("srv", ".srv")
_il_qualifier_ext(::IL.RAction)  = ("action", ".action")
_il_qualifier_ext(_)             = ("msg", ".msg")

# ── type_info specialization for registered (dynamic) types (§11) ───────────
# serialization.jl's `type_info(::Type{T})` defaults to the zero hash for a
# generated type (the Humble placeholder) — correct for keyexpr structure, wrong
# for cross-version matching. A registered type *does* know its real hash (it's
# the registry key), so we expose a lookup keyed by the generated type itself:
# realize-time stamps the entry, and `type_info_of` recovers the real `TypeInfo`.
# Static types keep the reflective default; dynamic ones carry the verified hash.

# Reverse map: generated `Type` → its `RegistryEntry`, populated at `realize!`.
# Kept as an IdDict so identity (not name) keys it — distinct hash-versions are
# distinct generated types.
const _TYPE_TO_ENTRY = IdDict{Type, RegistryEntry}()
const _TYPE_TO_ENTRY_LOCK = ReentrantLock()

# Hook realize! to record the reverse mapping once a type exists.
function _record_type_entry!(entry::RegistryEntry)
    entry.type isa Type || return entry
    @lock _TYPE_TO_ENTRY_LOCK _TYPE_TO_ENTRY[entry.type] = entry
    entry
end

"""
    type_info_of(::Type{T}) -> TypeInfo

The `TypeInfo` (name + *real* RIHS01) for a type — the registry's verified hash
for a dynamically-generated type, falling back to serialization.jl's reflective
[`type_info`](@ref) (zero-hash placeholder) for a statically-included one. This is
the §11 specialization of type identity: a registered type carries its true hash,
so keyexprs and liveliness for it match the wire across versions.
"""
function type_info_of(::Type{T}) where {T}
    entry = @lock _TYPE_TO_ENTRY_LOCK get(_TYPE_TO_ENTRY, T, nothing)
    entry === nothing ? type_info(T) : entry.info
end

# ── statically-generated type registration (@ros_import / @ros_cache, D5) ───────
# `@ros_import` (ament/vendored, by name) and `@ros_cache` (baked from the
# discovered-type cache) generate static types into the caller module and record
# them in a module-local global
#   `__ros_static_types__::Vector{Tuple{Type, String}}`
# — each generated type paired with its canonical wire `TypeDescription` JSON. The
# macro creates the global once (idempotently) and appends to it; the value is baked
# into the module image (package) or built at eval (script/REPL). At Context creation
# we find those globals — across loaded packages *and* Main's own submodule tree (so
# script/REPL interface modules, absent from `loaded_modules`, are covered) — and
# register each: bind the precompiled type to its real RIHS01 + a `:static` entry, so
# keyexpr-only resolution and the §13 server use it directly (no runtime codegen, the
# D9 "converge to fast" endgame). A plain module-local global — no method-table
# reflection, no `__init__` to clobber.

# The conventional module-local globals the macros populate: the baked static types,
# and (for `@ros_cache`) the opt-in marker carrying the persistence dir (`""` ⇒ the
# project default).
const _STATIC_GLOBAL = :__ros_static_types__
const _CACHE_MARKER  = :__ros_cache_dir__
const _STATIC_ENTRY_CACHE = IdDict{Any, RegistryEntry}()
const _STATIC_ENTRY_LOCK = ReentrantLock()

# Intern (once, memoized) the registry entry for a statically-generated type `T` from
# its baked JSON: parse → internal `TypeDescriptionMsg` → real hash → bind `mod`/`type`
# to the compiled `T` (so `realize!` is a no-op). Records the reverse mapping so
# `type_info_of(T)` reports the real hash. `nothing` on an unparseable blob.
function _intern_static_entry!(@nospecialize(T), json::AbstractString)
    @lock _STATIC_ENTRY_LOCK begin
        haskey(_STATIC_ENTRY_CACHE, T) && return _STATIC_ENTRY_CACHE[T]
        tdmsg = _parse_type_description_json(json)
        tdmsg === nothing && return nothing
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(tdmsg))
        hash === nothing && return nothing
        e = RegistryEntry(TypeInfo(tdmsg.type_description.type_name, hash),
                          lift(tdmsg); td=tdmsg, provenance=:static)
        e.mod = parentmodule(T)
        e.type = T
        _record_type_entry!(e)                 # type_info_of(T) → the real hash
        _STATIC_ENTRY_CACHE[T] = e
        return e
    end
end

# The ROSNode-local singleton the macros flush into and Context pulls from — so no
# module-table or method-table scanning is needed. `@ros_import`/`@ros_cache` emit
# code in the caller that calls `_absorb_static_module!` at module load (a generated
# `__init__` for precompiled packages; a guarded top-level call at eval for
# script/REPL modules), reading the module-local accumulator into here. Interned by
# type, so repeated absorbs are idempotent.
struct _StaticTypeIndex
    lock::ReentrantLock
    entries::Vector{RegistryEntry}        # interned static type entries
    seen::Set{Any}                        # types already interned (dedup)
    cache_dirs::Set{String}               # @ros_cache opt-in persistence dirs
end
const _STATIC_TYPES = _StaticTypeIndex(ReentrantLock(), RegistryEntry[], Set{Any}(), Set{String}())

"""
    absorb_static_types!(mod::Module) -> nothing

Flush a module's baked `@ros_import`/`@ros_cache` declarations (its
`__ros_static_types__` / `__ros_cache_dir__` globals) into ROSNode's static-type
singleton (D5). The macros call this at module load automatically — call it yourself
only from a *custom* `__init__` in a module that uses these macros (the macro defers
to a user-defined `__init__` rather than clobber it). Idempotent.
"""
function absorb_static_types!(mod::Module)
    if isdefined(mod, _CACHE_MARKER)
        d = try; getglobal(mod, _CACHE_MARKER); catch; nothing; end
        dir = (d isa AbstractString && !isempty(d)) ? String(d) : _default_project_cache_dir()
        @lock _STATIC_TYPES.lock push!(_STATIC_TYPES.cache_dirs, dir)
    end
    if isdefined(mod, _STATIC_GLOBAL)
        descriptors = try; getglobal(mod, _STATIC_GLOBAL); catch; return nothing; end
        descriptors isa AbstractVector || return nothing
        for d in descriptors
            try
                T, json = d
                T isa Type || continue
                @lock _STATIC_TYPES.lock (T in _STATIC_TYPES.seen) && continue
                e = _intern_static_entry!(T, json)
                e === nothing && continue
                @lock _STATIC_TYPES.lock begin
                    push!(_STATIC_TYPES.seen, T)
                    push!(_STATIC_TYPES.entries, e)
                end
            catch err
                @error "typesupport: absorbing static type failed" mod exception=(err, catch_backtrace())
            end
        end
    end
    return nothing
end

const _absorb_static_module! = absorb_static_types!   # internal alias used by the macros

"""
    _register_static_types!(ctx) -> ctx

Pull every absorbed static type (`@ros_import`/`@ros_cache`) and cache opt-in from the
[`_STATIC_TYPES`](@ref) singleton into `ctx` (D5): register each entry (binding the
precompiled type to its real RIHS01, so keyexpr-only resolution and the §13 server use
it directly — no runtime codegen) and enable project-local persistence for any
`@ros_cache` dir. Called at Context creation alongside the well-known bootstrap types.
"""
function _register_static_types!(ctx)
    reg = registry(ctx)
    @lock _STATIC_TYPES.lock begin
        for e in _STATIC_TYPES.entries
            register_type!(reg, e.info, e)
        end
        for dir in _STATIC_TYPES.cache_dirs
            try
                enable_project_cache!(dir)
            catch err
                @error "typesupport: enabling project cache failed" dir exception=err
            end
        end
    end
    return ctx
end
