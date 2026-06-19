# The node's local discovery graph, computed a-priori from the static plan.
#
# Every wire endpoint a node declares for itself is a pure function of the node identity plus
# its declared ports — not something we should have to recover by watching the node build.
# `node_endpoint_descs` enumerates that set using the SAME `_*_desc` functions each pattern
# uses for its own `make_entity` (`_publisher_desc`/`_service_desc`/`_action_descs`/…): one
# derivation per endpoint, shared between construction and enumeration, not a parallel copy
# kept in lockstep by a test.
#
# A declared port is not 1:1 with a wire endpoint: an action server expands to five, a
# parameter server to seven, and the node itself contributes a presence shell +
# `~/get_type_description`. This file AGGREGATES the per-pattern descriptors into the node's
# full set (`_base_node_descs` / `_ordered_member_descs`), populates the composed members'
# ports into the index a-priori on reserved ids (`_prime_local_graph!`, Stage B), and exposes
# the result (`local_graph` / `describe_graph`). `EndpointDesc` itself lives in context.jl,
# beside `EndpointInfo`, so the patterns (loaded earlier) can own their descriptors.

export local_graph, describe_graph

# One declared port's wire endpoints under its resolved wire name `w`. `:timer` is not an
# endpoint (empty); `:action` expands to five; every other kind is one. Each delegates to the
# pattern's own descriptor — the shared identity source — so this never re-derives identity.
function _port_descs(node, p::PortSpec, w::AbstractString)
    if p.kind === :publisher
        return EndpointDesc[_publisher_desc(node, w, p.msgtype)]
    elseif p.kind === :subscription
        return EndpointDesc[_subscription_desc(node, w, p.msgtype)]
    elseif p.kind === :service
        return EndpointDesc[_service_desc(node, w, p.msgtype)]
    elseif p.kind === :action
        return _action_descs(node, w, action_type(ActionTypeSupport(typeof(p.reaction))))
    elseif p.kind === :timer
        return EndpointDesc[]
    else
        @warn "node_endpoint_descs: port kind :$(p.kind) not enumerated" port = p.name
        return EndpointDesc[]
    end
end

# The endpoints every node declares for itself, independent of any composition: the presence
# shell (derived from the SAME `_node_endpoint` the Node injects), `~/get_type_description`
# (served by default, via the shared `_service_desc`), and — when the node carries a parameter
# schema — the parameter services. A `serve_type_description=false` node over-reports the
# type-description service here (the node doesn't retain that flag); the default is assumed.
function _base_node_descs(node::Node)
    descs = EndpointDesc[_endpoint_desc(_node_endpoint(node.entity))]
    push!(descs, _service_desc(node, "~/get_type_description", GetTypeDescription_Request))
    node.parameters === nothing || append!(descs, _param_descs(node))
    descs
end

"""
    node_endpoint_descs(cn::ComponentNode) -> Vector{EndpointDesc}
    node_endpoint_descs(node::Node) -> Vector{EndpointDesc}

The node's local discovery graph as [`EndpointDesc`](@ref)s — the wire endpoints the node
declares for itself, derived a-priori from the node identity and (the `ComponentNode` form)
its constructed members + resolved wire map, before any materialise. The bare `Node` form
covers what any node creates (presence shell, `~/get_type_description`, parameter services);
the `ComponentNode` form adds each member's ports (one per pub/sub/service, five per action,
none per timer).

Endpoints created imperatively or from runtime data — dynamic subscriptions, the lazy
`/rosout` publisher, runtime clients — are NOT included: they are genuine runtime, not a
static function of the plan.
"""
node_endpoint_descs(node::Node) = _base_node_descs(node)
node_endpoint_descs(cn::ComponentNode) =
    vcat(_base_node_descs(cn.node), _ordered_member_descs(cn))

# The composed members' port descriptors in MATERIALISE ORDER — the contract the Stage B id
# queue relies on: members in `order`, each member's ports in spec order, an action expanded
# to its five in `_action_descs` order. Shared by `node_endpoint_descs(cn)` and the priming.
function _ordered_member_descs(cn::ComponentNode, order::Vector{Symbol} = cn.order)
    descs = EndpointDesc[]
    for nm in order
        m  = cn.members[nm]
        wm = get(cn.wires, nm, Dict{Symbol, String}())
        for p in mixin_spec(_base(typeof(m))).ports
            append!(descs, _port_descs(cn.node, p, get(wm, p.name, _wire(p))))
        end
    end
    descs
end

# ── Stage B: a-priori population of the composed members' local endpoints ─────────

# The EndpointEntity a (descriptor, reserved id) implies — identical to the one `make_entity`
# builds for the matching entity, so the liveliness key + gid derived here agree with it.
_desc_entity(node::Node, d::EndpointDesc, id::Integer) =
    EndpointEntity(; id = id, node = node.entity, kind = d.kind,
                   topic = d.topic, type_info = d.type_info, qos = d.qos)

# Populate the composed members' port endpoints into the local graph a-priori, in ONE shot,
# on a freshly reserved id block — then arm the node's id queue so materialise lands the
# matching entities on those exact ids (and skips the per-declare inject). Returns the injected
# liveliness keys for `_reconcile_local_graph!` to drop orphans if a configure aborts.
function _prime_local_graph!(cn::ComponentNode, order::Vector{Symbol})
    node = cn.node; ctx = node.context
    descs = _ordered_member_descs(cn, order)
    isempty(descs) && return String[]
    base = reserve_entity_ids!(ctx, length(descs))
    lvkeys = String[]; infos = EndpointInfo[]
    @lock ctx.graph.lock for (i, d) in enumerate(descs)
        e = _desc_entity(node, d, base + i - 1)
        k = liveliness_keyexpr(ctx.format, e)
        info = _endpoint_info(e; is_local = true)
        ctx.graph.local_endpoints[k] = info
        push!(lvkeys, k); push!(infos, info)
    end
    _notify_graph_change!(ctx, infos, EndpointInfo[])
    @lock node.lock (empty!(node._id_queue); append!(node._id_queue, base:(base + length(descs) - 1)))
    lvkeys
end

# Close the materialise window: drop the id queue + materialising flag, then drop any primed
# endpoint with no backing entity — orphans from a member whose configure aborted (its ports
# never materialised). On a clean bring-up every primed key is backed → a no-op.
function _reconcile_local_graph!(node::Node, primed::Vector{String})
    @lock node.lock (node._materialising = false; empty!(node._id_queue))
    isempty(primed) && return nothing
    live = Set(e.lv_key for e in (@lock node.lock copy(node.entities)))
    orphans = EndpointInfo[]
    @lock node.context.graph.lock for k in primed
        k in live && continue
        info = pop!(node.context.graph.local_endpoints, k, nothing)
        info === nothing || push!(orphans, info)
    end
    isempty(orphans) || _notify_graph_change!(node.context, EndpointInfo[], orphans)
    nothing
end

# ── introspection: the node-as-built's own discovery graph ────────────────────────

"""
    local_graph(node) -> Vector{EndpointInfo}
    local_graph(cn::ComponentNode) -> Vector{EndpointInfo}

The endpoints this node declares for itself — its own `is_local` slice of the discovery
graph (publishers, subscriptions, services, the node-presence shell, parameter services, …),
each with its gid. The live counterpart of [`node_endpoint_descs`](@ref): a composed `@node`'s
members' ports are populated a-priori (the local graph is a static function of the plan),
while imperative/dynamic endpoints appear as they are created. Reads the local half of the
index directly, so it never includes a discovered remote.
"""
local_graph(cn::ComponentNode) = local_graph(cn.node)
function local_graph(node::Node)
    locals, _ = _local_remote_split(node.context)
    EndpointInfo[e for e in locals
                 if e.node_name == node.name && e.namespace == node.namespace]
end

# Short kind tag for a discovered/declared endpoint (the `EndpointKind` singletons).
_ep_kindstr(k::EndpointKind) =
    k === Publisher ? "pub" : k === Subscription ? "sub" :
    k === Service   ? "srv" : k === Client       ? "cli" : string(k)

"""
    describe_graph([io], node_or_cn)

Print the node's own discovery graph — every endpoint it declares, by kind, resolved topic,
and type — the discovery-side companion of [`describe_wiring`](@ref) (which shows the
per-member wire-name expansion). Useful for answering "what does this node advertise?".
"""
describe_graph(x) = describe_graph(stdout, x)
describe_graph(io::IO, cn::ComponentNode) = describe_graph(io, cn.node)
function describe_graph(io::IO, node::Node)
    g = sort(local_graph(node); by = e -> (_ep_kindstr(e.kind), e.topic))
    println(io, "local graph of ", node.fqn, " — ", length(g), " endpoint(s)")
    for e in g
        ty = e.type === nothing ? "" : e.type.name
        println(io, "  ", rpad(_ep_kindstr(e.kind), 4), rpad(e.topic, 40), ty)
    end
    return nothing
end
