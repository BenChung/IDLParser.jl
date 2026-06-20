# ── dynamic acquisition: TypeDescription → registry entry ───────────────────
# A GetTypeDescription reply is verified against its wire `TypeInfo`, lifted to IL,
# and turned into a registry entry. The discovery layer issues the call and hands
# the reply here.

"""
    verify_type_description(td::TypeDescriptionMsg, info::TypeInfo) -> Bool

The RIHS01 integrity gate: recompute `calculate_rihs01_hash` over the raw received
`TypeDescriptionMsg` and require it to equal `info`'s advertised RIHS01 (the
type-identity hash carried on every [`RegistryEntry`](@ref)). A match certifies that
the local definition can safely decode the remote's wire bytes; a mismatch means the
remote's definition disagrees with the hash it advertised. Hash the raw `td`, since
RIHS01 is computed over the canonical field closure and excludes the constants and
defaults `lift` drops.
"""
function verify_type_description(td::TypeDescriptionMsg, info::TypeInfo)
    expected = to_rihs_string(info.hash)
    got = calculate_rihs01_hash(td)
    return got == expected
end

"""
    entry_from_type_description(td::TypeDescriptionMsg, info::TypeInfo;
                                provenance=:wire, verify=true) -> RegistryEntry

Build a [`RegistryEntry`](@ref) from a wire `TypeDescriptionMsg`. A discovered
type must hash-match the name it travels under, so `verify` gates on the RIHS01
hash:

- `true` (default) — the hash gate runs first and a mismatch throws.
- `false` — the gate is skipped.

`lift` reconstructs the main IL; nested types survive as `RRef`s in its fields,
and codegen resolves them from the referenced closure carried in `td`.

Codegen is deferred to `realize!` at first use, so registration stays cheap and
discovery succeeds even for a type that later fails to generate.
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

Caching is best-effort and provenance-scoped:

- `:wire` — cached in the content-addressed JSON cache, which skips
  re-discovering dynamic types across runs.
- `:ament`/vendor — bypasses the cache regardless of `cache`, since these types
  are statically loadable and resolve by name.
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
# Scan install-prefix search paths for installed interface packages and parse their
# `share/<pkg>/{msg,srv,action}/*` files straight to IL, resolved by package/type
# name at runtime with codegen deferred to first use.

# Standard system install roots. The env-var prefixes precede these so a sourced
# workspace overlay wins; the `isdir` filter drops a distro absent on the host.
const _ROS_SYSTEM_DISTROS = ("rolling", "jazzy", "kilted", "lyrical", "humble")

# User-added prefixes (highest precedence), populated via `add_search_path!`.
const _USER_SEARCH_PATHS = String[]

# Guards the process-global typesupport config (`_USER_SEARCH_PATHS` and the `_CACHE`
# state) against a late reconfigure racing concurrent type resolution. A leaf lock:
# critical sections touch only the guarded fields.
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

# Colon-separated entries of env var `var`, empties dropped. `search_prefixes`
# applies the `isdir` filter once over the merged set.
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
# prefixes: `<prefix>/share/<pkg>/<qual>/<Name>.<ext>`. First match (overlay order).
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
identically to a wire-discovered one — RIHS01 is the shared identity. Three hash
outcomes by interface shape:

- Self-contained message type: the hash is exact.
- Type that references other packages: keys correctly by name; its hash matches a
  remote's once its referenced siblings are co-registered, at which point the
  wire/cache path supplies the exact hash.
- Interface that lowers to no struct: the entry takes the RIHS01 placeholder hash
  (version `01`, all-zero digest), correct for keyexpr structure, with the
  wire/cache path supplying the exact hash for cross-version matching.

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
# ROSZenoh's `type_info_from_struct`. The hash is exact for self-contained types; a
# cross-package referencing type keys correctly by name and matches a remote's only
# once its sibling refs are co-registered (the wire/cache path then supplies the
# byte-parity hash).
function _il_type_info(il, package::AbstractString, qualifier::AbstractString,
                       name::AbstractString)
    struct_ast = _primary_struct(il, package)
    if struct_ast === nothing
        # No single struct to hash: key by name with the placeholder hash, correct
        # for keyexpr structure. The wire/cache path supplies the exact hash.
        return TypeInfo(name, TypeHash())
    end
    bare = split_ros_name(name)[3]
    return ROSZenoh.type_info_from_struct(struct_ast, bare;
                                          package=package, qualifier=qualifier)
end

# The primary `StructDecl` AST for an IL interface, by lowering then scanning the
# decl vector. Every ROS-message type decl is a `StructDecl` (ROS interfaces have no
# IDL union/enum/typedef), so the first `TypeDecl` is the struct.
function _primary_struct(il, package::AbstractString)
    decls = lower(il; package=package)
    return _scan_for_struct(decls)
end

# Depth-first scan for the first type-decl struct, recursing through module wrappers.
# Scan IDLParser's Moshi `@data` sum types with `isa` + positional `getproperty`, not
# `@match`: both are stable across Moshi's emission and avoid a precompile-breaking
# dependency on Moshi's macro layer.
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

# The RIHS01 placeholder (version 01 + all-zero digest) the ament path assigns a type
# with no single struct to hash. It carries no comparable identity, so a mismatch
# against it is not a real revision divergence.
_is_placeholder_hash(h::TypeHash) = h == TypeHash()

# A pinned (`:static`/`:authored`) entry for `name` registered under a hash other than
# `other` — the user-pinned type whose RIHS01 a peer now contradicts. Scans under the
# registry lock; the registry is keyed on `(name, hash)`, with no by-name index.
function _pinned_conflict(reg::TypeRegistry, name::AbstractString, other::TypeHash)
    @lock reg.lock begin
        for ((n, h), entry) in reg.entries
            (n == name && h != other && entry isa RegistryEntry &&
             entry.provenance in (:static, :authored)) && return entry
        end
    end
    return nothing
end

# The type-revision diagnostic: a peer advertises `name` under `got`, but the local
# definition hashes to `expected`. `weak` softens it to `@debug` (the user opted into
# following the peer); otherwise `@warn`, since the pin is enforced.
function _warn_revision_mismatch(name::AbstractString, expected::TypeHash,
                                 got::TypeHash; weak::Bool)
    msg = "type-revision mismatch for $(name): a peer advertises \
           $(to_rihs_string(got)) but the locally pinned definition is \
           $(to_rihs_string(expected)). Likely a differing interface-definition \
           revision across distros/forks/dev workspaces. Regenerate the static type \
           to match the peer, or pass weak_types=true to Context to follow the peer's \
           revision."
    weak ? (@debug msg) : (@warn msg)
    return nothing
end

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

The ament lookup is name-only, so its result is re-validated against `info`'s
RIHS01 before it is trusted (the same revalidation the cache path runs). A real-hash
mismatch is rejected with a diagnostic and returns `nothing` so the caller
wire-discovers the peer's actual revision; the all-zero placeholder hash (an
uncomparable cross-package ref) is accepted with a `@debug` note. The diagnostic
loudness tracks the `Context`'s trust setting:

- default pinned trust — loud (`@warn`).
- `weak_types=true` — quiet (`@debug`).

When the ament path fires the diagnostic, `warned[]` (if given) is set `true`, so
a wire-suppression site owns the no-ament-file pinned-conflict case without
double-warning.

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
function resolve_type(ctxlike, info::TypeInfo; ament::Bool=true, cache::Bool=true,
                      warned::Union{Nothing,Ref{Bool}}=nothing)
    ctx = _ctx(ctxlike)
    reg = registry(ctx)

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
        # Name-only lookup: defer registration until its hash is validated against
        # `info`, so a revision-mismatched entry never lands under the registry.
        entry = load_ament_type(reg, info.name; register=false)
        if entry !== nothing
            h = entry.info.hash
            if h == info.hash
                return register_type!(reg, entry.info, entry)
            elseif _is_placeholder_hash(h)
                @debug "typesupport: ament entry for $(info.name) carries the RIHS01 \
                        placeholder hash; accepting (uncomparable cross-package ref)"
                return register_type!(reg, entry.info, entry)
            else
                # Real-hash mismatch: reject so the wire path fetches the peer's
                # revision (and weak mode can bind it). Signal the diagnostic so the
                # wire-suppression site does not warn twice.
                _warn_revision_mismatch(info.name, h, info.hash; weak=ctx.weak_types)
                warned === nothing || (warned[] = true)
                return nothing
            end
        end
    end

    return nothing
end

"""
    registered_type(node_or_ctx, info::TypeInfo) -> Union{Type, Nothing}

For a message type, the concrete (generated) Julia type for `info`, realizing its codegen
on first use. Returns `nothing` when the type fails to resolve or generate (the signal to
deliver raw bytes), and for service/action entries, which have no single umbrella type —
the service/action layer fetches their sections separately. The decode/handler path reaches
the returned type via `Base.invokelatest`, since codegen runs in a newer world age.
"""
function registered_type(ctxlike, info::TypeInfo)
    entry = resolve_type(ctxlike, info)
    entry === nothing && return nothing
    realize!(entry)
    return entry.type
end

