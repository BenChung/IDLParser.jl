# §D9 — per-node interaction manifest.
#
# The persistent record of which *dynamic* (runtime-discovered, §11/D5) type
# interactions a node actually exercised — `(role, RIHS01 hash, name, topic)` — keyed
# by node identity (`node.fqn`). It is the "what to warm" list for dynamic types,
# the dynamic-side analogue of the declared entities D8 warms for static ones.
#
# Across runs it is **append-mostly union**: each run records the interactions it
# newly used, so the file monotonically approaches the deployment's full dynamic
# participant set, and the next run's Tier-1 startup warm (node.jl
# `_replay_manifest_warm`) replays it — resolving each type from the cache / baked
# statics and running `_warm_dynamic` *before going live*, shifting the first-message
# JIT to startup. Warm-up cost decreases monotonically toward zero as the manifest
# saturates (D9 convergence).
#
# Correctness-safe by construction (inherited from §11): everything keys on RIHS01,
# so a stale entry whose type no longer resolves is simply skipped — never a wrong
# decode. Only *used* interactions are recorded (the writer hook is the dynamic
# dispatch's first-sight branch), never everything seen on the graph.
#
# Persistence shares the blob cache's opt-in (`_cache_enabled()` — `@ros_cache` /
# `enable_project_cache!` / the `$ROS_TYPESUPPORT_CACHE` env override): no surprise
# files, and the manifest is only useful when Tier-1 can resolve its entries (same
# gate). On-disk form is line-oriented TSV under a `manifests/` subdir of the cache
# dir, one record per line — append-friendly, crash-safe, no JSON dependency.

# One recorded interaction. `role` is `:subscription` today (the keyexpr-only
# `Subscription`, D5 S5, is the only path that meets a runtime-born type; the field
# is carried for fidelity to D9 and so a future dynamic pub/service needs no
# migration). `name`/`hash` are the resolved `TypeInfo` (name kept so Tier-1 can
# `resolve_or_discover` without scanning blobs); `topic` scopes the warm to the sub.
struct Interaction
    role::Symbol
    hash::TypeHash
    name::String
    topic::String
end

# In-memory state for one node's manifest: the union of recorded interactions (both
# the prior-run entries loaded from disk and this run's, so a record is written at
# most once), guarded by a lock. `loaded` makes the disk read lazy + once.
mutable struct _ManifestState
    const lock::ReentrantLock
    const lines::Set{String}          # canonical serialized lines already recorded (the union key)
    const entries::Vector{Interaction}
    loaded::Bool
end
_ManifestState() = _ManifestState(ReentrantLock(), Set{String}(), Interaction[], false)

# fqn → state. Lazily created; one process-wide table (a node's identity is its fqn).
const _MANIFESTS = Dict{String, _ManifestState}()
const _MANIFESTS_LOCK = ReentrantLock()

# The manifest directory: a `manifests/` subdir of the (project-local) cache dir, so
# per-node files never collide with the content-addressed `<hex>.json` blobs.
function _manifest_dir()
    d = joinpath(_cache_dir(), "manifests")
    isdir(d) || mkpath(d)
    return d
end

# A filesystem-safe leaf for an fqn (`/ns/node` → `_ns_node`): every char outside
# `[A-Za-z0-9_-]` becomes `_`. Collisions across distinct fqns are not a correctness
# hazard (entries self-validate by RIHS01), only a (vanishingly unlikely) sharing.
_sanitize_fqn(fqn::AbstractString) = replace(String(fqn), r"[^A-Za-z0-9_-]" => "_")

_manifest_path(fqn::AbstractString) = joinpath(_manifest_dir(), _sanitize_fqn(fqn) * ".tsv")

# Serialize / parse one record as `role \t RIHS01_hash \t name \t topic`. ROS names
# and topics cannot contain tabs, so the split is unambiguous. Parsing tolerates
# malformed lines (returns `nothing`, skipped) and a bad hash (the RIHS string fails
# to parse) so a corrupt manifest degrades to "warm less", never an error.
_serialize_interaction(it::Interaction) =
    string(it.role, '\t', to_rihs_string(it.hash), '\t', it.name, '\t', it.topic)

function _parse_interaction(line::AbstractString)
    parts = split(line, '\t')
    length(parts) == 4 || return nothing
    h = type_hash_from_rihs_string(parts[2])
    h === nothing && return nothing
    return Interaction(Symbol(parts[1]), h, String(parts[3]), String(parts[4]))
end

# Get-or-create the state for `fqn`, reading + parsing the on-disk file once. Holds
# the table lock only to fetch the per-node state; the file read happens under that
# state's own lock (so two nodes load concurrently). Best-effort: a read failure
# leaves an empty (but `loaded`) state — we warm nothing rather than erroring.
function _manifest_state(fqn::AbstractString)
    st = @lock _MANIFESTS_LOCK get!(() -> _ManifestState(), _MANIFESTS, String(fqn))
    @lock st.lock begin
        if !st.loaded
            st.loaded = true
            path = _manifest_path(fqn)
            if isfile(path)
                try
                    for line in eachline(path)
                        isempty(strip(line)) && continue
                        it = _parse_interaction(line)
                        it === nothing && continue
                        ser = _serialize_interaction(it)
                        ser in st.lines && continue
                        push!(st.lines, ser)
                        push!(st.entries, it)
                    end
                catch err
                    @warn "manifest: read failed for $(fqn) (warming nothing this run)" exception=err
                end
            end
        end
    end
    return st
end

"""
    load_manifest(fqn) -> Vector{Interaction}

The recorded dynamic-type interactions for node `fqn` (§D9), the union of every run
that wrote to its manifest. Empty when persistence is off (`_cache_enabled()`) or the
node has none yet. Used by Tier-1 startup warm (`_replay_manifest_warm`).
"""
function load_manifest(fqn::AbstractString)
    _cache_enabled() || return Interaction[]
    st = _manifest_state(fqn)
    return @lock st.lock copy(st.entries)
end

"""
    note_interaction!(fqn, role, hash, name, topic) -> Bool

Record that node `fqn` used dynamic type `(name, hash)` in `role` on `topic` (§D9).
Append-mostly union: a record already present (this run or a prior one) is a no-op;
a new one is appended to the node's manifest file. Returns `true` iff newly recorded.
Off (returns `false`) unless persistence is enabled — same opt-in as the blob cache.
Best-effort: a write failure is logged, never fatal (the type is already live).
"""
function note_interaction!(fqn::AbstractString, role::Symbol, hash::TypeHash,
                           name::AbstractString, topic::AbstractString)
    _cache_enabled() || return false
    it = Interaction(role, hash, String(name), String(topic))
    ser = _serialize_interaction(it)
    st = _manifest_state(fqn)
    @lock st.lock begin
        ser in st.lines && return false
        push!(st.lines, ser)
        push!(st.entries, it)
        try
            open(_manifest_path(fqn), "a") do io
                println(io, ser)
            end
        catch err
            @warn "manifest: write failed for $(fqn)" exception=err
        end
        return true
    end
end
