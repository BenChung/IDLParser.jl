"""
    ROSReach

Introspection for a ROS 2-over-Zenoh system, across the two distinct meanings of
"the graph", plus directed reachability diagnosis:

  - **Computation graph** — the ROS view: nodes and the pubs/subs/services/clients
    they declare, the topics/types/QoS on them, and which pairs are *eligible* to
    talk. Built from Zenoh `@ros2_lv/**` liveliness tokens ([`open_index`](@ref)).
  - **Transport graph** — the Zenoh view: which sessions (peers/routers) this
    session is actually linked to, and reachability via scouting
    ([`transport_topology`](@ref), [`scout_peers`](@ref)).
  - **Reachability** — "can I reach X, and if not, why" ([`diagnose`](@ref)).

The two graphs join on the session `z_id` (the only honest key). What each view
can and cannot tell you is documented on each function; see [`open_index`](@ref)
and [`edge_transport`](@ref) for the load-bearing caveats.

Self-contained: depends only on `ROSZenoh` + `Zenoh`, takes a `Zenoh.Session`
(or anything exposing `.session`, e.g. a ROSNode `Context`). It runs its own
liveliness subscriber, so it neither modifies nor depends on ROSNode.
"""
module ROSReach

import ROSZenoh
import Zenoh

# EndpointKind + its values for `endpoints(idx; kind=Publisher)`; type vocabulary
# for inspecting results. (Zenoh.Publisher — the transport handle — stays qualified
# everywhere to avoid the name clash with the EndpointKind value `Publisher`.)
using ROSZenoh: EndpointKind, Publisher, Subscription, Service, Client,
                TypeInfo, QosProfile
using Zenoh: WhatAmIs

include("index.jl")
include("queries.jl")
include("transport.jl")
include("join.jl")
include("reach.jl")
include("render.jl")
include("tui.jl")

export
    # index
    ReachIndex, open_index, Endpoint, GraphNode, on_change,
    snapshot_endpoints, snapshot_nodes,
    EndpointKind, Publisher, Subscription, Service, Client, TypeInfo, QosProfile,
    # computation queries
    endpoints, node_names, topic_names_and_types, topic_edges, TopicEdge,
    publishers_info, subscriptions_info, count_publishers, count_subscribers,
    session_of, endpoints_on_session, wiring_warnings, WiringWarning,
    # transport + reachability
    TransportPeer, TransportTopology, local_zid,
    connected_routers, connected_peers, transport_topology,
    scout_peers, on_scouted, WhatAmIs,
    # join + connectivity
    SessionView, session_views, EdgeTransport, edge_transport,
    publisher_matched, wait_for_publisher_matched, subscription_matched,
    # directed reachability
    ReachReport, ReachLayer, diagnose,
    # rendering + tui
    graph_report, to_dot, watch

end # module ROSReach
