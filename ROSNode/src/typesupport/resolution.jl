# ── per-module resolution tables (D10B S1) ──────────────────────────────────────
# Each module that `@ros_import`s/`@ros_cache`s a type bakes a `__ros_resolve__ ::
# Dict{Symbol, ResolveEntry}` (key = RIHS01 string) into its image, mapping a wire type
# to the Julia struct *this module* resolves it to. It is built at the module's
# **precompile** by `_merge_resolve!` — where the loaded-module set is exactly the
# module's dependency closure — so the table is a pure function of the declared deps,
# deterministic and immune to ambient runtime load order. A Context looks through one
# module's table (its `home`, D10B S2); decode identity is per-module, not global.

"""
    ResolveEntry(type, origin, tied)

One `(wire-RIHS01 → Julia struct)` resolution in a module's `__ros_resolve__`.
`origin` is the *minting* module (the package that generated `type`, for the tie
warning); `tied` is set when this entry settled a cross-module tie (a fork) so a
closer dependent adopts it rather than re-deriving/re-warning.
"""
struct ResolveEntry
    type::Type
    origin::Module
    tied::Bool
end

const _RESOLVE_GLOBAL = :__ros_resolve__

# The minting module of a generated type, for provenance (`pkg.msg` etc.).
_origin_of(@nospecialize(T)) = parentmodule(T)

# Warn once (at the lowest common dependent) that one wire type has two distinct Julia
# structs in this closure. `keep` is the deterministically-picked entry, `drop` the other.
function _warn_tie(rihs::Symbol, keep::ResolveEntry, drop::ResolveEntry)
    @warn "ROSNode: ambiguous wire type — two distinct Julia structs for the same \
           RIHS01 reached a common dependent. Resolution deterministically picks \
           `$(keep.type)` (from $(keep.origin)) over `$(drop.type)` (from $(drop.origin)); \
           cross-module handoff of a value needs `as(x, T)`. Fix: route both through one \
           shared message package so they alias a single struct." rihs=String(rihs) maxlog=1
end

# Fold source table `src` into `dst` (this module's), applying the D10B assembly rule.
# Agreement (diamond) is a no-op; a fresh fork is picked deterministically by the
# fully-qualified type string and marked `tied` (+ warned once); an already-`tied`
# settlement is sticky (suppresses re-derivation/re-warn from a closer dependent).
function _fold_resolve!(dst::Dict{Symbol, ResolveEntry}, src::Dict{Symbol, ResolveEntry})
    for (sym, e) in src
        prev = get(dst, sym, nothing)
        if prev === nothing
            dst[sym] = e
        elseif prev.type === e.type
            (e.tied && !prev.tied) && (dst[sym] = ResolveEntry(prev.type, prev.origin, true))
        elseif prev.tied
            # already settled lower in the tree — keep, suppress
        elseif e.tied
            dst[sym] = e                              # adopt the settled one
        else                                          # fresh fork: deterministic pick + warn
            keep, drop = string(prev.type) <= string(e.type) ? (prev, e) : (e, prev)
            _warn_tie(sym, keep, drop)
            dst[sym] = ResolveEntry(keep.type, keep.origin, true)
        end
    end
    return dst
end

# Reduce all candidate entries for one wire RIHS01 (gathered across the whole closure)
# to the single winner. Agreement is a no-op (carry the `tied` flag forward if any arm
# was already settled). A fork — two or more distinct structs — is settled by a true
# *global* `argmin` over the fully-qualified type string, so the survivor is the
# globally smallest FQN regardless of the order candidates were gathered in (a pairwise
# sticky fold would instead let the first-settled pair win and drop a later, smaller
# arm). Warns once, naming the global winner against a representative loser.
function _reduce_candidates(sym::Symbol, cands::Vector{ResolveEntry})
    keep = cands[1]                      # global argmin(string(type))
    drop = cands[1]                      # global argmax — a deterministic representative loser
    for e in cands
        string(e.type) < string(keep.type) && (keep = e)
        string(e.type) > string(drop.type) && (drop = e)
    end
    if keep.type !== drop.type           # fork: distinct structs for one wire type
        _warn_tie(sym, keep, drop)
        return ResolveEntry(keep.type, keep.origin, true)
    end
    # All agreed: settled iff some arm was already tied lower in the tree.
    return any(e -> e.tied, cands) ? ResolveEntry(keep.type, keep.origin, true) : keep
end

"""
    _merge_resolve!(mod::Module, own::Vector{<:Tuple{Symbol, Type}}) -> Dict

Populate `mod.__ros_resolve__` (D10B S1): globally reduce every loaded module's table
into it (at precompile that is exactly `mod`'s dependency closure — diamond/fork resolved
here), then add `mod`'s own freshly-minted `(rihs, T)` entries (own wins for `mod`).
Idempotent across multiple `@ros_import`/`@ros_cache` calls in one module. Emitted by the
macros at module top level so it runs — and bakes — at precompile.

The closure pick is a *global* `argmin(string(type))` over all candidates for a wire
type, not a pairwise sticky fold: with three or more forks the survivor is the globally
smallest FQN regardless of `loaded_modules` iteration order (a `Dict`, unordered), so the
documented "deterministic pick by fully-qualified type string" holds beyond two sources.
"""
function _merge_resolve!(mod::Module, own)
    tbl = getglobal(mod, _RESOLVE_GLOBAL)::Dict{Symbol, ResolveEntry}
    # Gather every closure candidate per wire type (incl. any already in `tbl` from a
    # prior `@ros_import`/`@ros_cache` call in this module, for idempotency), then reduce
    # each set to its global winner — order-independent by construction.
    cands = Dict{Symbol, Vector{ResolveEntry}}()
    for (sym, e) in tbl
        push!(get!(() -> ResolveEntry[], cands, sym), e)
    end
    for D in values(Base.loaded_modules)
        D === mod && continue
        isdefined(D, _RESOLVE_GLOBAL) || continue
        dt = getglobal(D, _RESOLVE_GLOBAL)
        dt isa Dict{Symbol, ResolveEntry} || continue
        for (sym, e) in dt
            push!(get!(() -> ResolveEntry[], cands, sym), e)
        end
    end
    for (sym, cs) in cands
        tbl[sym] = _reduce_candidates(sym, cs)
    end
    for (sym, T) in own                               # mod's own mints — own wins
        prev = get(tbl, sym, nothing)
        if prev === nothing || prev.type === T
            tbl[sym] = ResolveEntry(T, _origin_of(T), prev === nothing ? false : prev.tied)
        else                                          # own overrides a different closure type
            _warn_tie(sym, ResolveEntry(T, _origin_of(T), true), prev)
            tbl[sym] = ResolveEntry(T, _origin_of(T), true)
        end
    end
    return tbl
end

"""
    resolve_in_home(home::Module, rihs::Symbol) -> Union{Type, Nothing}

The Julia struct `home`'s baked `__ros_resolve__` resolves wire type `rihs` (RIHS01
string) to, or `nothing` if `home` (and its closure) never imported it — the D10B S2
per-Context resolution step, keyed by the Context's `home` module.
"""
function resolve_in_home(home::Module, rihs::Symbol)
    isdefined(home, _RESOLVE_GLOBAL) || return nothing
    tbl = getglobal(home, _RESOLVE_GLOBAL)
    tbl isa Dict{Symbol, ResolveEntry} || return nothing
    e = get(tbl, rihs, nothing)
    e === nothing ? nothing : e.type
end

# D10B S2: at Context construction with no `home`, hint once if some loaded module has a
# non-empty resolution table — the user almost certainly meant to bind a home (forgetting
# it silently disables resolving their `@ros_import` types on the dynamic path).
function _maybe_hint_no_home()
    for D in values(Base.loaded_modules)
        isdefined(D, _RESOLVE_GLOBAL) || continue
        t = getglobal(D, _RESOLVE_GLOBAL)
        if t isa Dict{Symbol, ResolveEntry} && !isempty(t)
            @info "ROSNode: Context created with no `home` — dynamic (keyexpr-only) \
                   subscriptions resolve only content-canonical types, not a module's \
                   `@ros_import` structs. Pass `home=@__MODULE__` or use `@context` to \
                   resolve to your module's types." maxlog=1
            return
        end
    end
end
