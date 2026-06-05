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
            # D10B S3: first-wins (skip a present `(name,hash)`) — never clobber. Decode
            # identity is now per-module (the `home` table, S1/S2), so this registry only
            # feeds the §13 server (any same-`(name,hash)` struct serves the identical
            # descriptor) and the no-home/canonical fallback. This removes the old
            # last-`__init__`-wins global clobber while keeping advertised types describable.
            lookup_type(reg, e.info) === nothing && register_type!(reg, e.info, e)
        end
        # Register every `@ros_cache` opt-in dir deterministically: reads search them
        # all, writes go to a single stable (min-string) dir — no last-wins collapse over
        # the unordered set.
        if !isempty(_STATIC_TYPES.cache_dirs)
            try
                _register_cache_dirs!(_STATIC_TYPES.cache_dirs)
            catch err
                @error "typesupport: enabling project cache failed" dirs=collect(_STATIC_TYPES.cache_dirs) exception=err
            end
        end
    end
    return ctx
end

