# Render the ROS graph + its changes to Rerun (DESIGN.md §2.3/§2.4). The recorder's
# reconcile loop calls these with filtered ROSReach snapshots.

_fqn(ns::AbstractString, name::AbstractString) = ns == "/" ? "/" * name : string(ns, "/", name)
_node_id(e)  = "n:" * _fqn(e.namespace, e.node_name)
_node_id(n::ROSReach.GraphNode) = "n:" * _fqn(n.namespace, n.name)
_topic_entity(topic::AbstractString) = "/topics/" * lstrip(topic, '/')
_node_entity(ns::AbstractString, name::AbstractString) = "/nodes" * _fqn(ns, name)

const _GRAPH_DIRECTED = UInt8(2)   # rerun.components.GraphType.Directed (u8 atom)

# Whole-graph topology as a bipartite directed graph: nodes & topics/services are
# GraphNodes; pub/sub/server/client are directed GraphEdges. Re-logged in full each pass.
function log_graph!(sink::Sink, eps, nodes, times::TimeStamps)
    ids = String[]; seen = Set{String}()
    edges = NamedTuple{(:first, :second),Tuple{String,String}}[]
    addid!(id) = (id in seen || (push!(ids, id); push!(seen, id)); id)

    for n in nodes
        addid!(_node_id(n))
    end
    for e in eps
        nid = addid!(_node_id(e))
        if e.kind == ROSReach.Publisher
            push!(edges, (first = nid, second = addid!("t:" * e.topic)))
        elseif e.kind == ROSReach.Subscription
            push!(edges, (first = addid!("t:" * e.topic), second = nid))
        elseif e.kind == ROSReach.Service
            push!(edges, (first = addid!("s:" * e.topic), second = nid))
        elseif e.kind == ROSReach.Client
            push!(edges, (first = nid, second = addid!("s:" * e.topic)))
        end
    end

    isempty(ids) || _push!(sink, "/graph", Rerun.Archetypes.GraphNodes(ids), times, false)
    isempty(edges) || _push!(sink, "/graph",
        Rerun.Archetypes.GraphEdges(edges; graph_type = [_GRAPH_DIRECTED]), times, false)
    return nothing
end

# Discrete add/remove events as a text stream (DESIGN.md §2.4).
function log_events!(sink::Sink, added_ep, removed_ep, added_nd, removed_nd, times::TimeStamps)
    for e in added_ep;   _event!(sink, "+ ", e, times); end
    for e in removed_ep; _event!(sink, "- ", e, times); end
    for n in added_nd;   _push!(sink, "/graph/events", Rerun.Archetypes.TextLog(["+ node " * _fqn(n.namespace, n.name)]), times, false); end
    for n in removed_nd; _push!(sink, "/graph/events", Rerun.Archetypes.TextLog(["- node " * _fqn(n.namespace, n.name)]), times, false); end
    return nothing
end
_kind_str(k) = k == ROSReach.Publisher ? "Publisher" : k == ROSReach.Subscription ? "Subscription" :
               k == ROSReach.Service ? "Service" : k == ROSReach.Client ? "Client" : "?"

function _event!(sink::Sink, sign, e, times)
    arrow = e.kind == ROSReach.Subscription || e.kind == ROSReach.Client ? " ← " : " → "
    ty = e.type === nothing ? "?" : e.type.name
    line = string(sign, _kind_str(e.kind), " ", _fqn(e.namespace, e.node_name), arrow, e.topic, " [", ty, "]")
    _push!(sink, "/graph/events", Rerun.Archetypes.TextLog([line]), times, false)
    return nothing
end

# Per-node and per-topic metadata (markdown TextDocument), re-logged on change.
function log_meta!(sink::Sink, eps, nodes, times::TimeStamps)
    bynode = Dict{Tuple{String,String},Vector{Any}}()
    bytopic = Dict{String,Vector{Any}}()
    for e in eps
        push!(get!(bynode, (e.namespace, e.node_name), Any[]), e)
        e.kind == ROSReach.Publisher || e.kind == ROSReach.Subscription || continue
        push!(get!(bytopic, e.topic, Any[]), e)
    end
    for n in nodes
        es = get(bynode, (n.namespace, n.name), Any[])
        body = string("# ", _fqn(n.namespace, n.name), "\n\n",
                      "- z_id: `", n.z_id, "`\n- enclave: `", n.enclave, "`\n- endpoints: ", length(es))
        _push!(sink, _node_entity(n.namespace, n.name), Rerun.Archetypes.TextDocument([body]), times, false)
    end
    for (topic, es) in bytopic
        npub = count(e -> e.kind == ROSReach.Publisher, es)
        nsub = count(e -> e.kind == ROSReach.Subscription, es)
        ty = something(findfirst(e -> e.type !== nothing, es), nothing)
        tyname = ty === nothing ? "?" : es[ty].type.name
        body = string("# ", topic, "\n\n- type: `", tyname, "`\n- publishers: ", npub, "\n- subscribers: ", nsub)
        _push!(sink, _topic_entity(topic) * "/_meta", Rerun.Archetypes.TextDocument([body]), times, false)
    end
    return nothing
end

# Mark removed nodes gone (so scrubbing past the change shows them cleared).
function clear_removed!(sink::Sink, removed_nd, times::TimeStamps)
    for n in removed_nd
        _push!(sink, _node_entity(n.namespace, n.name), Rerun.Archetypes.Clear([true]), times, false)
    end
    return nothing
end

# Clear a removed topic's metadata leaf (the node Clear can't reach it — disjoint roots).
function clear_removed_topics!(sink::Sink, removed_topics, times::TimeStamps)
    for t in removed_topics
        _push!(sink, _topic_entity(t) * "/_meta", Rerun.Archetypes.Clear([true]), times, false)
    end
    return nothing
end
