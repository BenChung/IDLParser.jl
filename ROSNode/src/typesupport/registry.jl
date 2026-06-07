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

# `register_type!` / `lookup_type` / `TypeRegistry` / `registry` are context.jl's;
# `type_info` / `ros_type_name` are serialization.jl's (this file specializes the
# former per registered type). All are already in the module's scope by include
# order, so no `import` is needed.

export TypeRegistry, RegistryEntry, register_type!, lookup_type,
       resolve_type, export_typesupport,
       ament_prefix_paths, search_prefixes, add_search_path!,
       discover_ament_packages, load_ament_type,
       enable_project_cache!, disable_project_cache!, absorb_static_types!

# ── registry entry shape (§11) ────────────────────────────────────────────
# The value stored in the Context's `(name, hash)`-keyed table. Holds the IL (the
# hub form), the wire `TypeDescription` blob (source of truth for persistence and
# the `~/get_type_description` server, §15), a provenance tag, and — once realized
# — the generated `Module` and the type within it reached via `invokelatest`.

"""
    RegistryEntry

A single type registry record (§11). Keyed in the [`TypeRegistry`](@ref) by
`(type_name, RIHS01-hash)`. ROS 2 interface/type-description concepts:
<https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html>. Carries:

- `info` — the `TypeInfo` (qualified name + hash) this entry answers for.
- `il` — the `ROSMessages.IL` (`RMessage`/`RService`/`RAction`), the hub form
  every consumer reads from.
- `td` — the wire `TypeDescriptionMsg` (main + referenced closure), the source of
  truth for persistence and the type-description service; `nothing` for a
  statically-acquired type, which has no wire description.
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
# `emit_imports=true` adds an inline `import StaticArrays, CDRSerialization` to the
# generated package modules, required by the `:julia` textual export: reparsing the
# source degrades the spliced module objects to bare names. Eval paths splice live
# module objects, which resolve without the imports.
function _generate_exprs(il, package::AbstractString; emit_imports::Bool=false)
    decls = IDLParser.Parse.Decl[]
    append!(decls, lower(il; package=package))
    resolved = IDLParser.ConstResolution.resolve_constants(decls)
    return IDLParser.Generation.generate_code(resolved; emit_imports=emit_imports)
end

# The full generation closure for a dynamic entry: every nested type it references
# (lifted from the `td`'s referenced descriptions, each under its own package) followed
# by the main IL. `generate_code` topo-sorts, so a nested-message field resolves to a
# sibling generated into the SAME module — without the closure a `pkg/msg/Nested` field
# is an UndefVarError in the gensym module. Mirrors @ros_import's multi-type closure.
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

# Eval an interface's generated code into a fresh module and return it. Generated
# code splices the `StaticArrays`/`CDRSerialization` module objects directly, so it
# resolves them regardless of the eval target's deps. The gensym'd target is rooted
# under `CDRSerialization` (a stable, always-loaded host); its gensym name keeps it
# from clashing with a static include.
#
# TODO: dynamic types accumulate as hidden submodules of CDRSerialization rather
# than ROSNode. A dedicated runtime-codegen root module would localize them.
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
then falls back to raw bytes for that topic.
"""
const _REALIZE_LOCK = ReentrantLock()

function realize!(entry::RegistryEntry)
    entry.mod === nothing || return entry
    # Serialize codegen so concurrent first-uses share one eval instead of each
    # building a gensym module for the same type. The section runs only codegen and
    # binding reads (no user handler code), so holding it across is deadlock-free.
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

