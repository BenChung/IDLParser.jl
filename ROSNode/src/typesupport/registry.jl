# Type support — the runtime type registry and its acquisition paths.
# IL is the universal hub: every acquisition path (static `@ros_import`, ament scan of
# an installed workspace, dynamic GetTypeDescription over the wire, authored types)
# normalizes into `ROSMessages.IL` before codegen, hashing, or persistence.
#
# The registry (storage + lock owned by the Context) is keyed by `(name, RIHS01)` so
# evolved versions of a type coexist — the hash is the decode-safety key, not just a
# version tag. This file owns the entry shape (`RegistryEntry`), the acquisition
# front-ends, and the codegen pipeline.
#
# Runtime codegen runs the macro pipeline programmatically:
#   lower → resolve_constants → generate_code → eval into a fresh module.
# A dynamic type is born in a newer world age than the compiled dispatcher, so its
# decode/handler calls cross that boundary via `invokelatest`.

using ROSMessages: ROSMessages, IL, lift, lower, message_il,
                   TypeDescription, TypeDescriptionMsg, FieldDescription,
                   FieldTypeDescription,
                   type_description_from_struct, calculate_rihs01_hash,
                   to_ros2_json
import IDLParser
using ROSZenoh: ROSZenoh, TypeInfo, TypeHash, to_rihs_string,
                type_hash_from_rihs_string

# `register_type!` / `lookup_type` / `TypeRegistry` / `registry` come from the Context
# layer; `type_info` / `ros_type_name` from serialization (this file specializes the
# former per registered type). Include order puts them in module scope already.

export TypeRegistry, RegistryEntry, register_type!, lookup_type,
       resolve_type, export_typesupport,
       ament_prefix_paths, search_prefixes, add_search_path!,
       discover_ament_packages, load_ament_type,
       enable_project_cache!, disable_project_cache!, absorb_static_types!

# ── registry entry shape ───────────────────────────────────────────────────

"""
    RegistryEntry(info::TypeInfo, il; td=nothing, provenance::Symbol=:wire)

One record in the [`TypeRegistry`](@ref), keyed there by `(type_name,
RIHS01-hash)`. Holds everything the runtime knows about a single ROS 2
interface type
(https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html):

- `info::TypeInfo` — the qualified name plus RIHS01 hash this entry answers for.
- `il` — the `ROSMessages.IL` (`RMessage` / `RService` / `RAction`), the hub
  form every consumer (codegen, hashing, persistence, the type-description
  server) reads from.
- `td::Union{TypeDescriptionMsg, Nothing}` — the wire type description (main
  type plus its referenced closure), the source of truth for persistence and
  the `~/get_type_description` service. Present for `:wire` and `:cache`
  entries (the received blob) and for `:static` / `:authored` entries (the
  baked or reflected description they were generated from); `nothing` for an
  `:ament` entry, which is parsed straight from installed `.msg` / `.srv` /
  `.action` text and so has no wire description.
- `provenance::Symbol` — where the type came from: `:static`, `:authored`,
  `:ament`, `:wire`, or `:cache`.
- `mod::Union{Module, Nothing}` and `type` — the generated module and the
  concrete struct within it, populated by `realize!` at first use (a
  statically-interned entry arrives with them already bound to the compiled
  type). Callers reach them via `Base.invokelatest`, since codegen runs in a
  newer world age than the compiled dispatcher.

The convenience constructor defers codegen (`mod`/`type` start `nothing`) and
defaults `provenance` to `:wire`.
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

# ── codegen: IL → generated module via the macro pipeline ───────────────────
# The dynamic-type birth path. `lower(il; package)` → `resolve_constants` →
# `generate_code` produces the same `Expr` vector the static macros splice; eval it
# into a fresh anonymous module so a runtime type lives in a clean namespace clear of a
# statically-included one. The generated code defines nested
# `<package>.<qualifier>.<Name>` submodules, walked to fetch the concrete type back out.

# Run lower → resolve → generate over one IL interface and return the `Expr`s.
# `package` threads through `lower` so cross-package `RRef`s project correctly.
# `resolve_constants` dispatches on `Vector{<:Parse.CanAnnotate{Parse.Decl}}`, so
# retype `lower`'s `Vector{Any}` into `Parse.Decl[]` before the call.
# `emit_imports=true` adds an inline `import StaticArrays, CDRSerialization`, required by
# the `:julia` textual export: a reparse of the source has only bare names, not the
# spliced live module objects the eval paths carry.
function _generate_exprs(il, package::AbstractString; emit_imports::Bool=false)
    decls = IDLParser.Parse.Decl[]
    append!(decls, lower(il; package=package))
    resolved = IDLParser.ConstResolution.resolve_constants(decls)
    return IDLParser.Generation.generate_code(resolved; emit_imports=emit_imports)
end

# The full generation closure for a dynamic entry: every nested type it references
# (lifted from the `td`'s referenced descriptions, each under its own package) followed
# by the main IL. `generate_code` topo-sorts, so a nested-message field resolves to a
# sibling generated into the SAME module. The closure is required: a `pkg/msg/Nested`
# field without it is an UndefVarError in the gensym module.
function _closure_ils(entry::RegistryEntry, package::AbstractString)
    ils = Tuple{String, Any}[]
    if entry.td !== nothing
        for rtd in entry.td.referenced_type_descriptions
            rpkg, _, _ = split_ros_name(rtd.type_name)
            push!(ils, (rpkg, lift(rtd)))
        end
    end
    push!(ils, (package, entry.il))
    return ils
end

# Eval an interface's generated code into a fresh module and return it. Generated code
# splices the `StaticArrays`/`CDRSerialization` module objects directly, resolving them
# regardless of the eval target's deps. The gensym'd target is rooted under
# `CDRSerialization` (a stable, always-loaded host); its gensym name keeps it clear of a
# static include.
#
# TODO: dynamic types accumulate as hidden submodules of CDRSerialization rather than
# ROSNode. A dedicated runtime-codegen root module would localize them.
function _eval_module(entry::RegistryEntry, package::AbstractString)
    name = gensym(isempty(package) ? :ros_dynamic : Symbol(package))
    target = Core.eval(CDRSerialization, :(module $name end))
    for ex in _generate_exprs_multi(_closure_ils(entry, package))
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
# The bindings are younger than the running world, so `isdefined`/`getfield` go through
# `invokelatest`; Julia 1.12+ rejects a prior-world binding access. This is the
# realize-time half of the dynamic-types-via-invokelatest rule (the per-message
# decode/handler hop is the other half).
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
newer world age than the compiled dispatcher).

A codegen/eval failure is logged and leaves the entry unrealized (`mod`/`type`
stay `nothing`) so a single bad type can't abort discovery; the subscription path
then falls back to raw bytes for that topic.
"""
const _REALIZE_LOCK = ReentrantLock()

function realize!(entry::RegistryEntry)
    entry.mod === nothing || return entry
    # Serialize codegen so concurrent first-uses share one eval rather than each
    # building a gensym module for the same type. The section runs only codegen and
    # binding reads (no user handler code), so it is deadlock-free under the lock.
    @lock _REALIZE_LOCK begin
        entry.mod === nothing || return entry
        package, _, _ = split_ros_name(entry.info.name)
        return _realize_locked!(entry, package)
    end
end

function _realize_locked!(entry::RegistryEntry, package::AbstractString)
    try
        mod = _eval_module(entry, package)
        entry.mod = mod
        # A message has one umbrella struct to bind as `entry.type`; a service /
        # action has sections (`Foo_Request`/`Foo_Goal`/…) and no umbrella type,
        # so leave `entry.type === nothing` and let the service/action layer fetch
        # its sections from `entry.mod` (no warning — that's the expected shape).
        if entry.il isa IL.RMessage
            entry.type = _fetch_generated_type(mod, entry.info.name)
            if entry.type === nothing
                @warn "typesupport: codegen produced no type for $(entry.info.name); \
                       delivered raw bytes for this topic"
            else
                _record_type_entry!(entry)     # map type → entry so type_info_of recovers the hash
            end
        end
    catch err
        @error "typesupport: codegen/eval failed for $(entry.info.name)" exception=(err, catch_backtrace())
    end
    return entry
end

