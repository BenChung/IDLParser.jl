# Per-node manifest of runtime-discovered message types a node used — the "what to
# warm" list for dynamic types.
#
# Each record is `(role, RIHS01 hash, name, topic)`, keyed by node fully-qualified
# name. Startup warm (`_replay_manifest_warm`) resolves each type and runs
# `_warm_dynamic` while the node starts, moving first-message JIT cost to startup.
#
# Persistence is append-only union: a run appends the interactions it newly used, so a
# node's file grows toward the deployment's full set and warm-up cost falls as it
# saturates. Records key on the RIHS01 hash, so an entry whose type no longer resolves
# is skipped and the hash guards against a wrong decode.
#
# Writes share the blob cache's opt-in (`_cache_enabled()`: `@ros_cache`,
# `enable_project_cache!`, or the `$ROS_TYPESUPPORT_CACHE` env override), since a
# manifest is useful only when startup warm can resolve its entries. On-disk form is
# one TSV record per line under a `manifests/` subdir of the cache dir: appends are
# crash-safe and need no JSON dependency.

# One recorded interaction. `role` is `:subscription` today — the keyexpr-only
# `Subscription` is the only path that meets a runtime-born type — but storing it lets
# a future dynamic publisher or service join with no on-disk migration. `name`/`hash`
# identify the resolved type (`name` lets `resolve_or_discover` skip a blob scan);
# `topic` scopes the warm to the subscription.
struct Interaction
    role::Symbol
    hash::TypeHash
    name::String
    topic::String
end

# In-memory state for one node's manifest, guarded by `lock`. `lines` is the union key
# (prior-run entries plus this run's), so each record is written at most once; `loaded`
# makes the disk read lazy and once-only.
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
# `[A-Za-z0-9_-]` becomes `_`. Two fqns that collide share one file but stay correct,
# since each entry self-validates by its RIHS01 hash.
_sanitize_fqn(fqn::AbstractString) = replace(String(fqn), r"[^A-Za-z0-9_-]" => "_")

_manifest_path(fqn::AbstractString) = joinpath(_manifest_dir(), _sanitize_fqn(fqn) * ".tsv")

# Serialize / parse one record as `role \t RIHS01_hash \t name \t topic`. ROS names
# and topics contain no tabs, so the split is unambiguous. A malformed line or
# unparseable hash yields `nothing` and is skipped, so a corrupt manifest degrades
# to warming fewer types and stays non-fatal.
_serialize_interaction(it::Interaction) =
    string(it.role, '\t', to_rihs_string(it.hash), '\t', it.name, '\t', it.topic)

function _parse_interaction(line::AbstractString)
    parts = split(line, '\t')
    length(parts) == 4 || return nothing
    h = type_hash_from_rihs_string(parts[2])
    h === nothing && return nothing
    return Interaction(Symbol(parts[1]), h, String(parts[3]), String(parts[4]))
end

# Get-or-create the state for `fqn`, reading and parsing the on-disk file once. The
# table lock is held only to fetch the per-node state; the file read runs under that
# state's own lock, so two nodes load concurrently. Best-effort: a read failure leaves
# an empty but `loaded` state, so the node warms nothing this run.
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

The recorded dynamic-type interactions for node `fqn`, the union of every run that
wrote to its manifest. Startup warm (`_replay_manifest_warm`) consumes this to
resolve and warm each type before the node goes live. Empty in two cases:

- persistence is off (`_cache_enabled()`)
- the node has recorded no interactions yet

The `hash` field of each entry is the RIHS01 type description hash that identifies
the interface (`.msg`/`.srv`/`.action`). See
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html
"""
function load_manifest(fqn::AbstractString)
    _cache_enabled() || return Interaction[]
    st = _manifest_state(fqn)
    return @lock st.lock copy(st.entries)
end

"""
    note_interaction!(fqn, role, hash, name, topic) -> Bool

Record that node `fqn` used dynamic type `(name, hash)` in `role` on `topic`.
Append-only union: an already-present record (this run or a prior one) is a no-op,
and a new one is appended to the node's manifest file. Returns:

- `true` — record newly appended
- `false` — record already present (this run or a prior one)
- `false` — persistence is off (the same opt-in as the blob cache)

A write failure is logged and the call still returns; the type is already live, so
the manifest is an optimization.
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
