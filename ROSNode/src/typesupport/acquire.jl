# ── dynamic acquisition: TypeDescription → registry entry ───────────────────
# A GetTypeDescription reply (`TypeDescriptionMsg`) is verified against its wire
# `TypeInfo`, lifted to IL, and turned into a registry entry. The discovery layer
# issues the GetTypeDescription call (it holds the `ServiceClient` and remote
# node identity) and hands the reply here.

"""
    verify_type_description(td::TypeDescriptionMsg, info::TypeInfo) -> Bool

The RIHS01 integrity gate: recompute `calculate_rihs01_hash` over the raw received
`TypeDescriptionMsg` and require it to equal `info`'s advertised RIHS01 (the
type-identity hash carried on every [`RegistryEntry`](@ref)). A mismatch means the
remote's definition disagrees with the hash it advertised, so the type is unsafe to
decode. Hashing the raw `td` keeps the gate exact, since RIHS01 ignores the constants
and defaults that `lift` would drop.
"""
function verify_type_description(td::TypeDescriptionMsg, info::TypeInfo)
    expected = to_rihs_string(info.hash)
    got = calculate_rihs01_hash(td)
    return got == expected
end

"""
    entry_from_type_description(td::TypeDescriptionMsg, info::TypeInfo;
                                provenance=:wire, verify=true) -> RegistryEntry

Build a [`RegistryEntry`](@ref) from a wire `TypeDescriptionMsg`. With
`verify=true` (the default) the hash gate runs first and a mismatch throws — a
discovered type must hash-match the name it travels under. `lift` reconstructs the
main IL; nested types survive as `RRef`s in its fields, and codegen resolves them
from the referenced closure carried in `td`.

Codegen is deferred to `realize!` / first use, keeping registration cheap
and letting discovery succeed even when a type later fails to generate.
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
`(info.name, info.hash)`, returning the entry. The dynamic-discovery
landing point: a GetTypeDescription reply is verified, lifted, and registered in
one call.

**Caches only `:wire` provenance** (best-effort): the content-addressed JSON cache
exists to skip re-discovering dynamically-discovered types across runs. Ament- and
vendor-acquired types are statically loadable (`@ros_msgs`) and resolve directly, so
they stay out of the cache even when a caller passes `cache=true`.
"""
function register_type_description!(reg::TypeRegistry, td::TypeDescriptionMsg,
                                    info::TypeInfo; provenance::Symbol=:wire,
                                    cache::Bool=true)
    entry = entry_from_type_description(td, info; provenance=provenance)
    register_type!(reg, info, entry)
    cache && provenance === :wire && _cache_store(info, td)
    return entry
end

# ── ament / colcon acquisition (static, no wire) ────────────────────────────
# The "I'm in a sourced workspace" path: scan the install-prefix search paths for
# installed interface packages and parse their `share/<pkg>/{msg,srv,action}/*`
# files straight to IL — same as feeding them to `@ros_msgs`, but resolved by
# package/type name at runtime with no codegen until first use.

# Standard system install roots, searched after the env-var prefixes so a sourced
# workspace overlay still wins. One `/opt/ros/<distro>` per distro we expect on a
# vanilla install; nonexistent ones are dropped by the `isdir` filter.
const _ROS_SYSTEM_DISTROS = ("rolling", "jazzy", "kilted", "lyrical", "humble")

# User-added prefixes (highest precedence), populated via `add_search_path!`.
const _USER_SEARCH_PATHS = String[]

# Guards the process-global typesupport config — `_USER_SEARCH_PATHS` and the
# `_CACHE` state (cache.jl) — so a late reconfigure cannot race concurrent type
# resolution. A leaf lock: critical sections touch only the guarded fields.
const _TS_CONFIG_LOCK = ReentrantLock()

"""
    add_search_path!(path) -> Vector{String}

Register `path` as an install-prefix search root for ament type acquisition,
process-wide and at the highest precedence (ahead of `AMENT_PREFIX_PATH`).
`path` should be an install prefix whose interface files live under
`<path>/share/<pkg>/{msg,srv,action}/`, the same shape as an
`AMENT_PREFIX_PATH` entry. The path is expanded (`~`), normalized to an
absolute path, and de-duplicated against prior registrations. Returns a
snapshot of the current user-registered search-path list.

The registration is global mutable process state, lock-guarded so it is safe
to call concurrently with type resolution.
"""
function add_search_path!(path::AbstractString)
    p = abspath(expanduser(String(path)))
    @lock _TS_CONFIG_LOCK begin
        p in _USER_SEARCH_PATHS || push!(_USER_SEARCH_PATHS, p)
        return copy(_USER_SEARCH_PATHS)
    end
end

# Colon-separated entries of env var `var`, empties dropped (no `isdir` filter —
# `search_prefixes` does that once over the merged set).
function _env_prefix_paths(var::AbstractString)
    raw = get(ENV, var, "")
    isempty(raw) && return String[]
    return String[p for p in split(raw, ':') if !isempty(p)]
end

"""
    ament_prefix_paths() -> Vector{String}

The `AMENT_PREFIX_PATH` environment entries (colon-separated, ROS's
install-prefix search path), filtered to existing directories. Returns an empty
vector outside a sourced workspace. This is one component of the merged ament
search order; [`search_prefixes`](@ref) returns the full, de-duplicated list.
"""
ament_prefix_paths() = String[p for p in _env_prefix_paths("AMENT_PREFIX_PATH") if isdir(p)]

"""
    search_prefixes() -> Vector{String}

The ordered, de-duplicated install-prefix roots scanned for ament interface
files, filtered to existing directories. Precedence (the overlay rule — the
first occurrence of a directory wins):

1. roots added via [`add_search_path!`](@ref) (user-registered),
2. `AMENT_PREFIX_PATH` entries (a sourced workspace),
3. `CMAKE_PREFIX_PATH` entries,
4. the `/opt/ros/<distro>` system installs for rolling, jazzy, kilted, lyrical,
   and humble.

Underlies [`discover_ament_packages`](@ref) and the by-name ament lookup behind
[`load_ament_type`](@ref).
"""
function search_prefixes()
    prefixes = String[]
    seen = Set{String}()
    user = @lock _TS_CONFIG_LOCK copy(_USER_SEARCH_PATHS)
    sources = Iterators.flatten((user,
                                 _env_prefix_paths("AMENT_PREFIX_PATH"),
                                 _env_prefix_paths("CMAKE_PREFIX_PATH"),
                                 (joinpath("/opt/ros", d) for d in _ROS_SYSTEM_DISTROS)))
    for p in sources
        (isempty(p) || p in seen || !isdir(p)) && continue
        push!(seen, p)
        push!(prefixes, p)
    end
    return prefixes
end

# An interface file under a prefix: `<prefix>/share/<pkg>/{msg,srv,action}/<X>.ext`.
const _IFACE_EXTS = (".msg", ".srv", ".action")

"""
    discover_ament_packages() -> Dict{String, Vector{String}}

Scan the [`search_prefixes`](@ref) install roots for installed ROS 2 interface
packages, mapping each package name to the absolute paths of its
`.msg` / `.srv` / `.action` files, grouped by qualifier (`msg`, then `srv`,
then `action`) and name-sorted within each group. A
package counts as an interface package when its `share/<pkg>` directory holds
at least one such file.

Under the overlay rule the first prefix to define a package wins; later
prefixes' copies of the same package are shadowed. Returns an empty `Dict`
outside a sourced workspace.
"""
function discover_ament_packages()
    out = Dict{String, Vector{String}}()
    for prefix in search_prefixes()
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

# Locate the interface file for a fully-qualified ROS2 name across the search
# prefixes: `<prefix>/share/<pkg>/<qual>/<Name>.<ext>`. Returns the first match
# (overlay order) or `nothing`.
function _find_ament_file(name::AbstractString)
    package, qualifier, bare = split_ros_name(name)
    isempty(package) && return nothing
    ext = qualifier == "srv" ? ".srv" : qualifier == "action" ? ".action" : ".msg"
    for prefix in search_prefixes()
        path = joinpath(prefix, "share", package, qualifier, bare * ext)
        isfile(path) && return path
    end
    return nothing
end

"""
    load_ament_type(reg::TypeRegistry, name; register=true) -> Union{RegistryEntry, Nothing}

Resolve a fully-qualified ROS 2 type `name` (`"<pkg>/<qualifier>/<Name>"`)
against the installed ament workspace: locate its `.msg` / `.srv` /
`.action` file across [`search_prefixes`](@ref) (overlay order, first match
wins), parse it to IL, compute the RIHS01 hash from the parsed definition, and
— with `register=true` (the default) — register the resulting
[`RegistryEntry`](@ref) under `(name, hash)` with `:ament` provenance. Returns
the entry, or `nothing` when the type is not installed.

The hash is computed locally from the parsed AST so an ament-acquired type keys
identically to a wire-discovered one — RIHS01 is the shared identity. The three
hash outcomes:

- Self-contained message type: the hash is exact.
- Type that references other packages: keys correctly by name but its hash may
  differ from a remote's until its referenced siblings are co-registered, at
  which point the wire/cache path supplies the exact hash.
- No struct declaration at all to hash (lowering yields none): the entry is
  keyed with the RIHS01 placeholder hash (version `01`, all-zero digest) —
  correct for keyexpr structure, with the wire/cache path supplying the exact
  hash for cross-version matching.

The entry is left unrealized (codegen deferred); nested references resolve once
the package's siblings are co-registered, the usual whole-package acquisition
shape.
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
# ROSZenoh's `type_info_from_struct` (threads it through `type_description_from_struct`
# + `calculate_rihs01_hash`). The walk recovers structs via ROSMessages' `lift(decls)`
# seam to avoid the Moshi `@match` macro, which ROSNode does not depend on.
#
# TODO: collect nested-ref `references` for byte-parity hashing of cross-package
# types (needs resolved + sorted sibling TypeDescriptions). Today the hash is exact
# for self-contained types; a referencing type keys correctly by name but its hash
# may differ from a remote's until refs are co-registered, where the exact wire/`td`
# path takes over.
function _il_type_info(il, package::AbstractString, qualifier::AbstractString,
                       name::AbstractString)
    struct_ast = _primary_struct(il, package)
    if struct_ast === nothing
        # No single struct to hash: key by name with the Humble placeholder hash.
        # Correct for keyexpr structure; the wire/cache path supplies the exact hash.
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
            return d
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

Return the [`RegistryEntry`](@ref) for `info`, acquiring it from a local source
when it is not yet registered. This is the local acquisition front door;
it tries three sources in order:

1. a registry hit (already known to this `Context`);
2. the content-addressed on-disk cache, when `cache=true` — a cached blob is
   re-validated against `info`'s RIHS01 and registered with `:cache`
   provenance;
3. an ament/static lookup by name in the installed workspace, when `ament=true`
   (see [`load_ament_type`](@ref)).

Returns `nothing` when none of the local sources resolves the type. The dynamic
over-the-wire path (issuing a `GetTypeDescription` query) stays with the
discovery layer, which holds the remote node identity and async
`ServiceClient`; on a `nothing` result the caller either kicks off that
discovery or delivers raw bytes for the topic.

`node_or_ctx` is a node or a `Context`; the entry's `Context` registry is
reached through it. A resolved entry stays unrealized (codegen deferred) — call
`realize!` or `registered_type` at first use to obtain its
concrete type.
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

The concrete (generated) Julia type for `info`, realizing its codegen on first use.
Returns `nothing` when the type fails to resolve or generate — the signal to deliver
raw bytes until the type is ready. The decode/handler path reaches the returned type via
`Base.invokelatest`, since codegen runs in a newer world age.
"""
function registered_type(ctxlike, info::TypeInfo)
    entry = resolve_type(ctxlike, info)
    entry === nothing && return nothing
    realize!(entry)
    return entry.type
end

