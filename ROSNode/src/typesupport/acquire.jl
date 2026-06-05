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

