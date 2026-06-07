# The self-contained discovery index: a live ROS 2 computation-graph view built
# from Zenoh `@ros2_lv/**` liveliness tokens on a session we do not own.
#
# It deliberately mirrors ROSNode's `GraphIndex`/`_ingest_liveliness!` rather than
# reusing it, for two reasons: (1) every record retains the session `z_id` parsed
# from the token — the join key to the transport graph, which ROSNode's
# `EndpointInfo` drops; (2) `NN` node tokens are kept, so zero-endpoint nodes and
# the (rmw_zenoh-only) enclave survive, which ROSNode discards. The cost is a
# second `@ros2_lv/**` subscriber on the shared session — cheap and isolated.

using Zenoh: Zenoh, Session, Keyexpr, LivelinessSubscriberHandler, zid, to_le_bytes
using ROSZenoh: RmwZenoh, KeyExprFormat, parse_liveliness,
                NodeEntity, EndpointEntity

# Render a Zenoh id (raw little-endian bytes) to the canonical lowercase-hex form:
# bytes reversed (MSB first) with leading zero bytes elided — byte-for-byte the form
# rmw_zenoh writes into liveliness tokens (and `ROSZenoh.ZenohId` prints). The join
# only works if transport zids render identically to token zids, so both sides go
# through this one function.
function _render_zid(le::NTuple{16, UInt8})
    be = reverse(collect(le))
    i = findfirst(!=(0x00), be)
    i === nothing && return "0"
    bytes2hex(@view be[i:end])
end

_session_zid(s::Session) = _render_zid(to_le_bytes(zid(s)))

"""
    Endpoint

One discovered publisher/subscription/service/client. Carries the owning session
`z_id` (the transport join key), the session-local `node_id`/`entity_id`, the
`TypeInfo` (name + RIHS01 hash; `nothing` for an `EMPTY_TOPIC_TYPE` token), and
`QosProfile`. `is_local` marks our own session's entities. A liveliness token
means the entity is *declared* — not that any data flows (see [`topic_edges`](@ref)).
"""
struct Endpoint
    z_id::String
    domain_id::Int
    node_id::Int
    entity_id::Int
    node_name::String
    namespace::String
    kind::EndpointKind
    topic::String                       # the keyexpr's topic field, FQN as advertised
    type::Union{TypeInfo, Nothing}
    qos::QosProfile
    is_local::Bool
end

"""
    GraphNode

A discovered node, recovered from an `NN` liveliness token. Carries its session
`z_id` and `enclave` (rmw_zenoh transmits `%`/empty for enclave, so it is usually
blank; kept for parity and the rare populated case).
"""
struct GraphNode
    z_id::String
    name::String
    namespace::String
    enclave::String
end

"""
    ReachIndex

The live index feeding every computation-graph query. One `@ros2_lv/**`
liveliness subscriber (FIFO channel drained by a Julia task, mirroring ROSNode's
deadlock-safe pattern) maintains `endpoints` and `nodes`, both keyed by the token
keyexpr (PUT inserts/updates, DELETE removes). `changed` is notified on every
change; `on_change` listeners fire outside the lock. Open with [`open_index`](@ref),
release with `close`.
"""
mutable struct ReachIndex
    session::Session
    local_zid::String                   # our own session's rendered z_id (is_local key)
    format::KeyExprFormat
    lock::ReentrantLock
    endpoints::Dict{String, Endpoint}
    nodes::Dict{String, GraphNode}
    changed::Threads.Condition
    listeners::Vector{Any}
    _sub::Any
    _task::Union{Task, Nothing}
    _closed::Bool
end

# Accept a bare `Zenoh.Session` or anything exposing `.session` (e.g. a ROSNode
# Context) — the duck type that keeps ROSNode out of our dependency graph.
_session(s::Session) = s
_session(x) = hasproperty(x, :session) ? x.session::Session :
    throw(ArgumentError("ROSReach needs a Zenoh.Session or an object with a `.session` field"))

# rmw_zenoh tokens live under `@ros2_lv/**`; the `**/@ros2_lv/**` form also catches
# ros2dds's `@/<zid>/@ros2_lv/**` (those parse with `node === nothing`, so they
# carry no join key — see `_endpoint`).
_lv_wildcard(::RmwZenoh) = "@ros2_lv/**"
_lv_wildcard(::KeyExprFormat) = "**/@ros2_lv/**"

"""
    open_index(session_or_ctx; format=RmwZenoh(), history=true, capacity=1024) -> ReachIndex

Start a liveliness subscriber on the session and build a live computation-graph
index. `session_or_ctx` is a `Zenoh.Session` or anything with a `.session` field.
`history=true` replays the existing token set so a late join sees the current
graph. Discovery is eventually-consistent: a freshly-opened index converges over
the next moments (use [`on_change`](@ref) to react, or poll the queries).

The view is *declared* entities, not active connections. `close` it to release
the subscriber and stop the drain task.
"""
function open_index(session_or_ctx; format::KeyExprFormat=RmwZenoh(),
                    history::Bool=true, capacity::Integer=1024)
    s = _session(session_or_ctx)
    idx = ReachIndex(s, _session_zid(s), format, ReentrantLock(),
                     Dict{String, Endpoint}(), Dict{String, GraphNode}(),
                     Threads.Condition(), Any[], nothing, nothing, false)
    ke = Keyexpr(_lv_wildcard(format))
    sub = LivelinessSubscriberHandler(s, ke; channel=:fifo, capacity=capacity,
                                      history=history)
    idx._sub = sub
    # Migratable @spawn: the index update is lock-guarded and needs no thread
    # affinity. Closing the handler ends the iterator → the task exits.
    idx._task = Threads.@spawn begin
        try
            for sample in sub
                try
                    _ingest!(idx, sample)
                catch err
                    @error "ROSReach: liveliness sample handling failed" exception=(err, catch_backtrace())
                end
            end
        catch err
            idx._closed ||
                @error "ROSReach: liveliness consumer task failed" exception=(err, catch_backtrace())
        end
    end
    return idx
end

# Apply one liveliness sample. PUT = appeared (parse + insert into the right
# table), DELETE = withdrew (remove by the token key from whichever table holds it).
function _ingest!(idx::ReachIndex, sample)
    key = Zenoh.keyexpr(sample)
    if Zenoh.kind(sample) !== Zenoh.SampleKinds.PUT
        removed = @lock idx.lock begin
            e = pop!(idx.endpoints, key, nothing)
            e === nothing ? pop!(idx.nodes, key, nothing) : e
        end
        removed === nothing || _notify!(idx)
        return nothing
    end

    entity = parse_liveliness(idx.format, key)
    if entity isa NodeEntity
        n = GraphNode(string(entity.z_id), entity.name, entity.namespace, entity.enclave)
        @lock idx.lock idx.nodes[key] = n
    elseif entity isa EndpointEntity
        @lock idx.lock idx.endpoints[key] = _endpoint(entity, idx.local_zid)
    else
        return nothing
    end
    _notify!(idx)
    return nothing
end

# Build an `Endpoint` from a parsed token. rmw_zenoh tokens carry the node (hence
# z_id/ids); ros2dds tokens yield `node === nothing` — blank identity, no join key.
# `is_local` compares the token's session z_id against ours.
function _endpoint(e::EndpointEntity, local_zid::AbstractString)
    e.node === nothing &&
        return Endpoint("", -1, -1, e.id, "", "", e.kind, e.topic, e.type_info, e.qos, false)
    zid_hex = string(e.node.z_id)
    Endpoint(zid_hex, e.node.domain_id, e.node.id, e.id, e.node.name, e.node.namespace,
             e.kind, e.topic, e.type_info, e.qos, zid_hex == local_zid)
end

# Wake waiters and fire `on_change` listeners. Snapshot listeners under the lock,
# fire outside it (handlers may query the index or register more).
function _notify!(idx::ReachIndex)
    @lock idx.changed notify(idx.changed)
    fns = @lock idx.lock copy(idx.listeners)
    isempty(fns) && return nothing
    for f in fns
        try
            f(idx)
        catch err
            @error "ROSReach: on_change listener threw" exception=(err, catch_backtrace())
        end
    end
    nothing
end

"""
    on_change(f, idx) -> f

Register `f(idx)` to fire on every discovery change (an endpoint or node appears
or disappears). Returns `f`. The push form behind the live [`watch`](@ref) view.
"""
function on_change(f, idx::ReachIndex)
    @lock idx.lock push!(idx.listeners, f)
    f
end

"An immutable snapshot of the discovered endpoints (copied under the lock)."
snapshot_endpoints(idx::ReachIndex) = @lock idx.lock collect(values(idx.endpoints))

"An immutable snapshot of the discovered nodes (from `NN` tokens)."
snapshot_nodes(idx::ReachIndex) = @lock idx.lock collect(values(idx.nodes))

function Base.close(idx::ReachIndex)
    idx._closed && return nothing
    idx._closed = true
    if idx._sub !== nothing
        try
            close(idx._sub)
        catch err
            @error "ROSReach: closing liveliness subscriber failed" exception=(err, catch_backtrace())
        end
        idx._sub = nothing
    end
    # Wake any graph waiters so they unwind rather than hang.
    @lock idx.changed notify(idx.changed; all=true)
    nothing
end

Base.isopen(idx::ReachIndex) = !idx._closed

Base.show(io::IO, idx::ReachIndex) =
    print(io, "ReachIndex(", length(idx.endpoints), " endpoints, ",
          length(idx.nodes), " nodes, ", idx._closed ? "closed" : "open", ")")
