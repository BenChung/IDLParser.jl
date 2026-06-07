# Computation-graph queries over an index snapshot — the rcl-style surface
# (node_names / topic_names_and_types / publishers_info / counts) plus the
# eligible-edge inference. Every query filters a consistent snapshot copied under
# the lock, never the live dict, so a query can't race a discovery update.

using ROSZenoh: qos_compatible, TypeHash, TypeInfo

"""
    endpoints(idx; topic=nothing, kind=nothing, node=nothing, namespace=nothing) -> Vector{Endpoint}

Every discovered endpoint matching the filters. `topic` matches the advertised
topic field; `kind` is an [`EndpointKind`](@ref) (`Publisher`/`Subscription`/
`Service`/`Client`); `node`/`namespace` filter by the owning node. A `nothing`
filter is a wildcard. The view is eventually-consistent for remotes.
"""
function endpoints(idx::ReachIndex; topic::Union{AbstractString, Nothing}=nothing,
                   kind::Union{EndpointKind, Nothing}=nothing,
                   node::Union{AbstractString, Nothing}=nothing,
                   namespace::Union{AbstractString, Nothing}=nothing)
    out = Endpoint[]
    for e in snapshot_endpoints(idx)
        topic     === nothing || e.topic == topic           || continue
        kind      === nothing || e.kind == kind             || continue
        node      === nothing || e.node_name == node        || continue
        namespace === nothing || e.namespace == String(namespace) || continue
        push!(out, e)
    end
    out
end

"Publishers on `topic` (each carrying TypeInfo + QoS + owning session)."
publishers_info(idx::ReachIndex, topic::AbstractString) = endpoints(idx; topic=topic, kind=Publisher)
"Subscriptions on `topic`; dual of [`publishers_info`](@ref)."
subscriptions_info(idx::ReachIndex, topic::AbstractString) = endpoints(idx; topic=topic, kind=Subscription)
"Number of publishers on `topic`."
count_publishers(idx::ReachIndex, topic::AbstractString) = length(publishers_info(idx, topic))
"Number of subscriptions on `topic`."
count_subscribers(idx::ReachIndex, topic::AbstractString) = length(subscriptions_info(idx, topic))

"""
    node_names(idx) -> Vector{GraphNode}

The distinct nodes in the graph, deduped by `(namespace, name)`. Sourced from `NN`
tokens (real enclave + z_id, including zero-endpoint nodes) merged with nodes
recovered from endpoints' owning identity. Endpoints without a node (the ros2dds
bridge-compat gap) contribute nothing.
"""
function node_names(idx::ReachIndex)
    seen = Set{Tuple{String, String}}()
    out = GraphNode[]
    for n in snapshot_nodes(idx)
        k = (n.namespace, n.name)
        k in seen && continue
        push!(seen, k); push!(out, n)
    end
    for e in snapshot_endpoints(idx)
        isempty(e.node_name) && continue
        k = (e.namespace, e.node_name)
        k in seen && continue
        push!(seen, k); push!(out, GraphNode(e.z_id, e.node_name, e.namespace, ""))
    end
    out
end

"""
    topic_names_and_types(idx; include_services=false) -> Dict{String, Vector{TypeInfo}}

Each topic mapped to the distinct `TypeInfo`s advertised on it. More than one
entry ⇒ a type mismatch on the wire (different name or RIHS01 version). Pub/sub
only by default; `include_services=true` folds in service/client endpoints.
"""
function topic_names_and_types(idx::ReachIndex; include_services::Bool=false)
    out = Dict{String, Vector{TypeInfo}}()
    for e in snapshot_endpoints(idx)
        include_services || (e.kind === Publisher || e.kind === Subscription) || continue
        e.type === nothing && continue
        types = get!(() -> TypeInfo[], out, e.topic)
        any(==(e.type), types) || push!(types, e.type)
    end
    out
end

"""
    TopicEdge

A single *eligible* publisher→subscriber pairing on a topic: the two endpoints
share a topic and (per [`topic_edges`](@ref)) `type_ok`/`qos_ok` say whether their
type and QoS are compatible. **Eligibility is not connectivity** — it means the
pair *could* communicate, not that any sample flows. `type_verified` is `false`
when either side carries no type or an all-zero hash placeholder, so `type_ok`
rests on an assumption rather than a real comparison.
"""
struct TopicEdge
    topic::String
    pub::Endpoint
    sub::Endpoint
    type_ok::Bool
    qos_ok::Bool
    type_verified::Bool
end

_is_zero_hash(h::TypeHash) = all(==(0x00), h.value)

# Type comparison with the honesty flags. Missing type (EMPTY token) ⇒ can't
# refute, so `type_ok=true` but `type_verified=false`. A zero-hash placeholder on
# either side likewise makes the hash comparison untrustworthy.
function _type_compat(pub::Endpoint, sub::Endpoint)
    pt, st = pub.type, sub.type
    (pt === nothing || st === nothing) && return (true, false)
    verified = !(_is_zero_hash(pt.hash) || _is_zero_hash(st.hash))
    ok = pt.name == st.name && pt.hash == st.hash
    (ok, verified)
end

"""
    topic_edges(idx; topic=nothing) -> Vector{TopicEdge}

Every eligible publisher→subscriber pairing (optionally restricted to one
`topic`): the cartesian product of pubs × subs sharing a topic, each annotated
with type/QoS compatibility. This is the honest "what *could* talk to what" view;
for real routing truth on your *own* publishers use [`publisher_matched`](@ref).
"""
function topic_edges(idx::ReachIndex; topic::Union{AbstractString, Nothing}=nothing)
    snap = snapshot_endpoints(idx)
    topic === nothing || (snap = filter(e -> e.topic == topic, snap))
    pubs = filter(e -> e.kind === Publisher, snap)
    subs = filter(e -> e.kind === Subscription, snap)
    out = TopicEdge[]
    for p in pubs, s in subs
        p.topic == s.topic || continue
        type_ok, verified = _type_compat(p, s)
        qos_ok = isempty(qos_compatible(s.qos, p.qos))   # sub requests, pub offers
        push!(out, TopicEdge(p.topic, p, s, type_ok, qos_ok, verified))
    end
    out
end

"""
    endpoints_on_session(idx, zid) -> Vector{Endpoint}

Every endpoint owned by the session `zid` — the computation→transport drill-down
("what is this peer publishing?"). `zid` is a rendered hex z_id (see [`local_zid`](@ref)).
"""
endpoints_on_session(idx::ReachIndex, zid::AbstractString) =
    filter(e -> e.z_id == zid, snapshot_endpoints(idx))

"""
    session_of(idx, node_name; namespace=nothing) -> Union{String, Nothing}

The session `z_id` a given ROS node lives on (the transport→reachability bridge),
or `nothing` if the node isn't discovered / carries no session (ros2dds).
"""
function session_of(idx::ReachIndex, node_name::AbstractString;
                    namespace::Union{AbstractString, Nothing}=nothing)
    for n in snapshot_nodes(idx)
        n.name == node_name || continue
        namespace === nothing || n.namespace == String(namespace) || continue
        isempty(n.z_id) || return n.z_id
    end
    for e in snapshot_endpoints(idx)
        e.node_name == node_name || continue
        namespace === nothing || e.namespace == String(namespace) || continue
        isempty(e.z_id) || return e.z_id
    end
    return nothing
end
