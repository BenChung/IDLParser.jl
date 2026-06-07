# The join: relate the computation graph (endpoints, grouped by owning session
# z_id) to the transport graph (which sessions we're linked to). The join is
# *grouping, not topology* — it answers "which session owns this endpoint, and is
# that session in my transport reach", never "pub A reaches sub B via link L". In
# the default router deployment a cross-session edge transits zenohd, so the most
# we honestly assert per edge is the booleans in `EdgeTransport`.
#
# The one connectivity fact we CAN assert is for our *own* publishers, via Zenoh's
# routing-plane `matching_status`. There is no subscriber-side equivalent in the
# Zenoh C API, so `subscription_matched` is necessarily liveliness-derived (a count,
# labeled compatibility-not-connectivity).

using Zenoh: Zenoh, matching_status

"""
    SessionView

A transport session grouped with the ROS entities it owns: its `zid`, an inferred
`role` (`:local`/`:router`/`:peer`), whether we hold a direct hop-1 `connected`
link to it, and the `nodes`/`endpoints` discovered on it. A remote peer reached
*through* a router shows `connected=false` (we have no direct link) yet may be
perfectly usable — "connected" is direct-link, not "can I talk to it".
"""
struct SessionView
    zid::String
    role::Symbol
    connected::Bool
    nodes::Vector{GraphNode}
    endpoints::Vector{Endpoint}
end

"""
    session_views(idx, x) -> Vector{SessionView}

Group every discovered endpoint and node by owning session `z_id`, marking each
session's role and whether we hold a direct transport link to it (`x` is a
[`ReachIndex`](@ref)/`Session`/`.session` holder used to read the live link set).
Sessions with no z_id (ros2dds) are skipped — they can't be joined.
"""
function session_views(idx::ReachIndex, x=idx)
    routers = Set(connected_routers(x))
    peers = Set(connected_peers(x))
    me = local_zid(x)
    eps = Dict{String, Vector{Endpoint}}()
    for e in snapshot_endpoints(idx)
        isempty(e.z_id) && continue
        push!(get!(() -> Endpoint[], eps, e.z_id), e)
    end
    nds = Dict{String, Vector{GraphNode}}()
    for n in snapshot_nodes(idx)
        isempty(n.z_id) && continue
        push!(get!(() -> GraphNode[], nds, n.z_id), n)
    end
    out = SessionView[]
    for zid in union(keys(eps), keys(nds))
        role = zid == me ? :local : (zid in routers ? :router : :peer)
        connected = zid == me || zid in routers || zid in peers
        push!(out, SessionView(zid, role, connected,
                               get(nds, zid, GraphNode[]), get(eps, zid, Endpoint[])))
    end
    out
end

"""
    EdgeTransport

What can be said, honestly, about the transport beneath an eligible [`TopicEdge`](@ref):

- `same_session` — pub and sub share a z_id (same process / intra-session; no network hop).
- `pub_in_transport` / `sub_in_transport` — that endpoint's session is in our direct link set (local or a connected router/peer).
- `via_router` — the edge crosses sessions and we are attached to a router, so it most likely transits `zenohd` (pub→router→sub). Deliberately **no** fabricated point-to-point path — in router mode there is no node-to-node link to report.
"""
struct EdgeTransport
    edge::TopicEdge
    same_session::Bool
    pub_in_transport::Bool
    sub_in_transport::Bool
    via_router::Bool
end

"""
    edge_transport(idx, x, edge) -> EdgeTransport

Annotate an eligible [`TopicEdge`](@ref) with the honest transport booleans (see
[`EdgeTransport`](@ref)). `x` supplies the live transport link set.
"""
function edge_transport(idx::ReachIndex, x, edge::TopicEdge)
    routers = connected_routers(x)
    linkset = Set(routers); union!(linkset, connected_peers(x)); push!(linkset, local_zid(x))
    pz, sz = edge.pub.z_id, edge.sub.z_id
    same = !isempty(pz) && pz == sz
    EdgeTransport(edge, same,
                  edge.pub.is_local || pz in linkset,
                  edge.sub.is_local || sz in linkset,
                  !same && !isempty(routers))
end

"""
    publisher_matched(pub) -> Bool

The one real routing-plane connectivity fact available: `true` iff `pub` (a
`Zenoh.Publisher` you own) currently has at least one matching subscriber reachable
through routing. Unlike the liveliness graph this is ground truth — but it is a
boolean for *your own* endpoint, not an adjacency, and there is no subscriber-side
equivalent in the Zenoh C API.
"""
publisher_matched(pub) = matching_status(pub)

"""
    wait_for_publisher_matched(pub; timeout=nothing, pollint=0.05) -> Bool

Block until `pub` has a matching subscriber (real routing match), or `timeout`
seconds elapse (`nothing` = forever). Returns `true` when matched, `false` on
timeout. Polls [`publisher_matched`](@ref) (a cheap C call).
"""
function wait_for_publisher_matched(pub; timeout::Union{Real, Nothing}=nothing,
                                    pollint::Real=0.05)
    deadline = timeout === nothing ? nothing : time() + Float64(timeout)
    while true
        publisher_matched(pub) && return true
        deadline === nothing || time() < deadline || return false
        sleep(pollint)
    end
end

"""
    subscription_matched(idx, topic) -> Int

A **liveliness-derived** count of publishers eligible for subscriptions on `topic`
— compatibility, not connectivity (Zenoh exposes no subscriber-side routing match).
For real routing truth use [`publisher_matched`](@ref) on a publisher you own.
"""
subscription_matched(idx::ReachIndex, topic::AbstractString) = count_publishers(idx, topic)
