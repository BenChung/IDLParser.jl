# Transport graph + scouting: the Zenoh view. `connected_*` are this session's
# *actual* hop-1 links right now; `scout_peers` is sessionless reachability ("who's
# out there in theory"). Both render zids through `_render_zid` (index.jl) so they
# compare byte-for-byte against the liveliness-token z_ids the computation graph
# carries — that string equality is the entire computation↔transport join.
#
# CAVEATS (shipped as docs): in the default rmw_zenoh deployment data flows
# node→zenohd→node, so `connected_routers` is usually one local `zenohd` and
# `connected_peers` is often empty; the real path for a pub→sub edge is two hops
# through a router the ROS graph never names. `router_zids`/`peer_zids`/`scout` are
# blocking C calls — never call them from the index consumer task or an on_change
# listener (they would stall discovery); use a separate task.

using Zenoh: Zenoh, Session, Config, router_zids, peer_zids, scout, Hello,
             whatami_string, to_le_bytes

_session(idx::ReachIndex) = idx.session

"""
    TransportPeer

A Zenoh session seen at the transport layer. `zid` is the rendered session id;
`role` is `:router`/`:peer`/`:client`; `locators` are the addresses it *claims*
to be reachable at (not proof of routability); `connected` is `true` for an
established link (from `connected_*`), `false` for a scouting result.
"""
struct TransportPeer
    zid::String
    role::Symbol
    locators::Vector{String}
    connected::Bool
end

"""
    TransportTopology

This session's local transport view: its own `self` z_id, the `routers` and `peers`
it is linked to right now (rendered z_ids), and an inferred `mode`. `mode` is a
heuristic over the link sets (`:router` when attached to routers with no direct
peers, `:peer` when peers are present, `:unknown` when isolated) — not an
authoritative read of the session's configured mode.
"""
struct TransportTopology
    self::String
    routers::Vector{String}
    peers::Vector{String}
    mode::Symbol
end

_render_ids(ids) = String[_render_zid(to_le_bytes(id)) for id in ids]

"""
    local_zid(x) -> String

Our own session's rendered z_id — the value remote endpoints' `is_local` is keyed
on. `x` is a [`ReachIndex`](@ref), a `Zenoh.Session`, or anything with `.session`.
"""
local_zid(idx::ReachIndex) = idx.local_zid
local_zid(x) = _session_zid(_session(x))

"""
    connected_routers(x) -> Vector{String}

The rendered z_ids of the routers this session is linked to right now. In the
default deployment this is typically a single local `zenohd`.
"""
connected_routers(x) = _render_ids(router_zids(_session(x)))

"""
    connected_peers(x) -> Vector{String}

The rendered z_ids of the peers this session is *directly* linked to. Often empty
in router mode (traffic transits the router, not a direct peer mesh).
"""
connected_peers(x) = _render_ids(peer_zids(_session(x)))

"""
    transport_topology(x) -> TransportTopology

One-call local transport view: self z_id + connected routers/peers + inferred mode.
See [`TransportTopology`](@ref) for the `mode` caveat. This is hop-1 and
session-local; it does not compose into a node-to-node topology (see
[`edge_transport`](@ref)).
"""
function transport_topology(x)
    s = _session(x)
    routers = _render_ids(router_zids(s))
    peers = _render_ids(peer_zids(s))
    mode = !isempty(peers) ? :peer : (!isempty(routers) ? :router : :unknown)
    TransportTopology(_session_zid(s), routers, peers, mode)
end

"""
    scout_peers(; config=Config(), what=nothing, timeout_ms=1000) -> Vector{TransportPeer}

Reachability "in theory": run a Zenoh scouting round and return every peer heard
(`connected=false`). `what` filters roles (`WhatAmIs.ROUTER`/`.PEER`/`.CLIENT`,
or a tuple). This is **fuzzy and point-in-time**: multicast scouting reaches only
the local network segment; gossip depends on router connectivity; a peer's
`locators` are *advertised*, not a guarantee you can connect.

Scouting is sessionless — it takes a `Config`, not the running session, so its
scope may differ from the live session's. Pass a `config` matching how you
connected (e.g. multicast disabled for a `localhost_only` setup, in which case
this returns ~nothing — by design).
"""
function scout_peers(; config::Config=Config(), what=nothing, timeout_ms::Integer=1000)
    hellos = scout(config; what=what, timeout_ms=timeout_ms)
    [TransportPeer(_render_zid(to_le_bytes(h.zid)), Symbol(whatami_string(h.whatami)),
                   h.locators, false) for h in hellos]
end

"""
    on_scouted(f, config; what=nothing, timeout_ms=1000)

Streaming form of [`scout_peers`](@ref): invoke `f(::TransportPeer)` for each peer
as it is heard during the scouting round, instead of collecting them. Blocks until
the round ends.
"""
function on_scouted(f, config::Config; what=nothing, timeout_ms::Integer=1000)
    scout(config; what=what, timeout_ms=timeout_ms) do h
        f(TransportPeer(_render_zid(to_le_bytes(h.zid)),
                        Symbol(whatami_string(h.whatami)), h.locators, false))
    end
    nothing
end
