# Static type registration for the `@ros_import` / `@ros_cache` macros.
#
# Each macro generates interface types into the caller module and records them in a
# module-local global `__ros_static_types__::Vector{Tuple{Type, String}}` — each type
# paired with its canonical wire `TypeDescription` JSON. This module-local global bakes
# into the consumer's package image (or builds at eval in a script/REPL module). A load
# hook absorbs it into ROSNode's process-global singleton, and Context creation
# registers each type bound to its real RIHS01 hash, so keyexpr-only resolution and the
# type-description server use the precompiled type directly. The two-phase split (bake
# the type-agnostic frames at precompile, defer the global-dict registration to load)
# keeps the registry from emptying when a precompiled consumer discards ROSNode's state.

# Module-local globals the macros populate: the baked static types, and the
# `@ros_cache` persistence-dir marker (`""` selects the project default).
const _STATIC_GLOBAL = :__ros_static_types__
const _CACHE_MARKER  = :__ros_cache_dir__
const _STATIC_ENTRY_CACHE = IdDict{Any, RegistryEntry}()
const _STATIC_ENTRY_LOCK = ReentrantLock()

# Build and memoize the registry entry for a statically-generated type `T` from its
# baked JSON: parse to a `TypeDescriptionMsg`, compute the RIHS01 hash, and bind the
# entry's `mod`/`type` to the compiled `T` so `realize!` is a no-op. Records the
# reverse mapping so `type_info_of(T)` reports the hash. Returns `nothing` when the
# JSON fails to parse.
function _intern_static_entry!(@nospecialize(T), json::AbstractString; provenance::Symbol=:static)
    @lock _STATIC_ENTRY_LOCK begin
        haskey(_STATIC_ENTRY_CACHE, T) && return _STATIC_ENTRY_CACHE[T]
        tdmsg = _parse_type_description_json(json)
        tdmsg === nothing && return nothing
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(tdmsg))
        hash === nothing && return nothing
        e = RegistryEntry(TypeInfo(tdmsg.type_description.type_name, hash),
                          lift(tdmsg); td=tdmsg, provenance=provenance)
        e.mod = parentmodule(T)
        e.type = T
        _record_type_entry!(e)                 # type_info_of(T) → the real hash
        _STATIC_ENTRY_CACHE[T] = e
        return e
    end
end

# Process-global singleton that the macros flush into and Context pulls from. The load
# hook (`absorb_static_types!`, run from a generated `__init__` in precompiled packages
# or at eval in script/REPL modules) reads the module's accumulator into here. Entries
# are interned by type, so repeated absorbs are idempotent.
struct _StaticTypeIndex
    lock::ReentrantLock
    entries::Vector{RegistryEntry}        # interned static type entries
    seen::Set{Any}                        # types already interned (dedup)
    cache_dirs::Set{String}               # @ros_cache opt-in persistence dirs
end
const _STATIC_TYPES = _StaticTypeIndex(ReentrantLock(), RegistryEntry[], Set{Any}(), Set{String}())

"""
    absorb_static_types!(mod::Module) -> nothing

Flush a module's baked `@ros_import` / `@ros_cache` declarations and authored
types (`@ros_message` / `@ros_service` / `@ros_action`) into ROSNode's
process-wide static-type singleton, so a later `Context` picks up the
precompiled interface types and cache opt-ins. The macros emit a call to this
at module load — from a generated `__init__` in a precompiled package, or a
top-level call at eval in a script/REPL module. Call it yourself only when your
module defines its own `__init__` (which the macros otherwise generate for you).
Idempotent: entries are interned by type, so repeated absorbs of the same module
are no-ops.

It drains three module-local globals when present: `__ros_cache_dir__` (a
`@ros_cache` opt-in directory), `__ros_authored_types__` (types authored with
`@ros_message` / `@ros_service` / `@ros_action`, interned by reflection and
folded into the module's resolve table), and `__ros_static_types__` (each
`@ros_import` type paired with its canonical `TypeDescription` JSON). Each
carries a precompiled ROS 2 interface type and its type-description bytes;
registration into a `Context` happens later, at `Context` creation. A per-type
failure is logged and skipped so one bad declaration cannot abort the absorb.
Returns `nothing`.
"""
function absorb_static_types!(mod::Module)
    if isdefined(mod, _CACHE_MARKER)
        d = try; getglobal(mod, _CACHE_MARKER); catch; nothing; end
        dir = (d isa AbstractString && !isempty(d)) ? String(d) : _default_project_cache_dir()
        @lock _STATIC_TYPES.lock push!(_STATIC_TYPES.cache_dirs, dir)
    end
    # Authored types (@ros_message/…): drained in push order (nested-first), interned by
    # *reflection* (their IL isn't known until load time), and folded into the module's
    # resolve table so dynamic subscribers map the wire RIHS to the user's struct.
    # `_intern_authored_entry!` lives in authored.jl (included later; late-bound here).
    if isdefined(mod, _AUTHORED_GLOBAL)
        authored = try; getglobal(mod, _AUTHORED_GLOBAL); catch; nothing; end
        if authored isa AbstractVector
            for d in authored
                try
                    T, fqn = d
                    T isa Type || continue
                    @lock _STATIC_TYPES.lock (T in _STATIC_TYPES.seen) && continue
                    e = _intern_authored_entry!(T, String(fqn))
                    e === nothing && continue
                    @lock _STATIC_TYPES.lock begin
                        push!(_STATIC_TYPES.seen, T)
                        push!(_STATIC_TYPES.entries, e)
                    end
                    _merge_resolve!(mod, Tuple{Symbol, Type}[(Symbol(to_rihs_string(e.info.hash)), T)])
                catch err
                    @error "typesupport: absorbing authored type failed" mod exception=(err, catch_backtrace())
                end
            end
        end
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

const _absorb_static_module! = absorb_static_types!

"""
    _register_static_types!(ctx) -> ctx

Register every absorbed static type and cache opt-in from the [`_STATIC_TYPES`](@ref)
singleton into `ctx`: bind each precompiled type to its RIHS01 hash so keyexpr-only
resolution and the type-description server use it directly, and enable project-local
persistence for each `@ros_cache` dir. Called at Context creation alongside the
well-known bootstrap types.

The RIHS01 hash identifies a ROS 2 interface type; see
https://design.ros2.org/articles/idl_interface_definition.html.
"""
function _register_static_types!(ctx)
    reg = registry(ctx)
    @lock _STATIC_TYPES.lock begin
        for e in _STATIC_TYPES.entries
            # First-wins: skip a present `(name, hash)`, never clobber. Decode identity is
            # per-module via the `home` table, so this registry feeds only the
            # type-description server (any struct with the same `(name, hash)` serves the
            # identical descriptor) and the canonical fallback.
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

