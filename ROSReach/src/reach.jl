# Directed reachability: "try to reach <target> and tell me why I can't". Passive
# first (existing session state + normal broadcast scouting, no directed
# connection), escalating to an active probe only on request (`active=true`): a raw
# TCP connect to the target's locator, then a short-lived throwaway Zenoh session
# pointed at it. The report walks layers DNS→TCP→Zenoh→graph and names the first
# one that fails, with honest caveats (NAT asymmetry, behind-router nodes,
# domain ≠ transport reachability).

using Zenoh: Zenoh, Config, router_zids, peer_zids
import Sockets

"""
    ReachLayer

One step of a [`ReachReport`](@ref): `name` (`:resolve`/`:graph`/`:link`/`:scout`/
`:tcp`/`:zenoh`), `ok` (`true`/`false`, or `nothing` when not attempted), and a
human `detail`.
"""
struct ReachLayer
    name::Symbol
    ok::Union{Bool, Nothing}
    detail::String
end

"""
    ReachReport

The outcome of [`diagnose`](@ref): the parsed `target` and its `kind`
(`:locator`/`:zid`/`:node`), any `resolved_zid` and `locators` discovered, the
ordered `layers` walked, a best-effort `reachable` verdict (`nothing` = unknown
without an active probe), a one-line `reason`, and `caveats` to read the verdict by.
"""
struct ReachReport
    target::String
    kind::Symbol
    resolved_zid::Union{String, Nothing}
    locators::Vector{String}
    layers::Vector{ReachLayer}
    reachable::Union{Bool, Nothing}
    reason::String
    caveats::Vector{String}
end

const _LOCATOR_PROTOS = ("tcp", "udp", "tls", "quic", "ws", "wss",
                         "unixsock-stream", "serial", "vsock")

# Classify the target string into :node (ROS FQN/name), :locator (proto/host:port),
# or :zid (bare lowercase hex). A `/`-leading string is a ROS name; a `proto/...`
# string is a locator; all-hex is a session id; anything else falls back to a node
# name (a bare relative name like "talker").
function _classify(target::AbstractString)
    startswith(target, "/") && return :node
    if occursin('/', target)
        first(split(target, '/')) in _LOCATOR_PROTOS && return :locator
    end
    all(c -> c in "0123456789abcdef", target) && !isempty(target) && return :zid
    return :node
end

function _resolve_node(idx::ReachIndex, fqn::AbstractString)
    s = lstrip(fqn, '/')
    if occursin('/', s)
        parts = rsplit(s, '/'; limit=2)
        return session_of(idx, String(parts[2]); namespace="/" * String(parts[1]))
    end
    return session_of(idx, String(s))
end

# Split a Zenoh locator `proto/host:port[#cfg]` into (host, port). Handles IPv6
# `[::1]:7447`. Returns `nothing` if it can't be parsed into host+port.
function _locator_hostport(loc::AbstractString)
    body = split(loc, '/'; limit=2)
    length(body) == 2 || return nothing
    addr = first(split(body[2], '#'))               # strip a trailing #config
    if startswith(addr, "[")                          # [ipv6]:port
        close_b = findfirst(']', addr)
        close_b === nothing && return nothing
        host = addr[2:close_b-1]
        rest = addr[close_b+1:end]
        startswith(rest, ":") || return nothing
        port = tryparse(Int, rest[2:end])
    else
        i = findlast(':', addr)
        i === nothing && return nothing
        host = addr[1:i-1]
        port = tryparse(Int, addr[i+1:end])
    end
    port === nothing && return nothing
    (host, port)
end

# Raw TCP connect with a wall-clock bound, on its own task so a hung connect can't
# block us. Distinguishes refused/no-route/timeout from a Zenoh-level problem.
function _tcp_probe(host::AbstractString, port::Integer, timeout_s::Real)
    result = Ref{Any}(nothing)
    t = Threads.@spawn try
        sock = Sockets.connect(host, port)
        close(sock)
        result[] = :ok
    catch err
        result[] = err
    end
    if Base.timedwait(() -> istaskdone(t), Float64(timeout_s); pollint=0.02) !== :ok
        return (false, "TCP connect to $host:$port timed out after $(timeout_s)s")
    end
    result[] === :ok && return (true, "TCP connect to $host:$port succeeded")
    return (false, "TCP connect to $host:$port failed: $(sprint(showerror, result[]))")
end

# Open a throwaway session pointed only at `locators` (multicast off) and poll for
# any established link up to `timeout_s`. Confirms the Zenoh handshake — catches
# version/transport/auth mismatches a bare TCP connect can't see.
function _zenoh_probe(locators::Vector{String}, timeout_s::Real)
    cfg = Config()
    cfg["scouting/multicast/enabled"] = false
    cfg["connect/endpoints"] = locators
    s = Base.open(cfg)
    try
        deadline = time() + Float64(timeout_s)
        while time() < deadline
            (!isempty(router_zids(s)) || !isempty(peer_zids(s))) &&
                return (true, "Zenoh session established a link to $(locators)")
            sleep(0.05)
        end
        return (false, "opened a session to $(locators) but no Zenoh link formed within $(timeout_s)s (version/transport/auth mismatch, or nothing listening)")
    finally
        try; close(s); catch; end
    end
end

const _REACH_CAVEATS = [
    "Transport reachability ≠ ROS visibility: a peer on a different ROS_DOMAIN_ID is reachable at the Zenoh layer but invisible to the ROS graph.",
    "A node behind a router advertises no direct locator — you reach it via the router, not point-to-point.",
    "Firewalls/NAT can make reachability asymmetric: a failed outbound probe doesn't prove the peer can't reach you.",
    "Scouting only covers the local segment (multicast) or gossip-connected routers; absence is not proof of unreachability.",
]

"""
    diagnose(idx, x, target; active=false, timeout_ms=1500) -> ReachReport

Diagnose whether `target` is reachable and, if not, why. `target` is a Zenoh
**locator** (`tcp/host:port`), a session **z_id** (hex), or a ROS **node name**
(`/talker`). `x` is a [`ReachIndex`](@ref)/`Session`/`.session` holder used to read
the live transport links.

Passive by default (layers `resolve→graph→link→scout`: existing state + a normal
scouting round, no directed connection). With `active=true` it adds a directed
probe to the target's locators (layers `tcp→zenoh`): a raw TCP connect, then a
short-lived throwaway Zenoh session. Read the verdict alongside `report.caveats`.
"""
function diagnose(idx::ReachIndex, x, target::AbstractString;
                  active::Bool=false, timeout_ms::Integer=1500)
    kind = _classify(target)
    layers = ReachLayer[]
    resolved_zid = nothing
    locators = String[]
    timeout_s = timeout_ms / 1000

    # ── resolve ──────────────────────────────────────────────────────────────
    if kind === :locator
        locators = [String(target)]
        push!(layers, ReachLayer(:resolve, true, "target is a locator: $target"))
    elseif kind === :zid
        resolved_zid = String(target)
        push!(layers, ReachLayer(:resolve, true, "target is a session z_id: $target"))
    else
        resolved_zid = _resolve_node(idx, target)
        if resolved_zid === nothing
            push!(layers, ReachLayer(:resolve, false,
                "node $target is not in the ROS graph (or carries no session id)"))
            return ReachReport(String(target), kind, nothing, locators, layers, false,
                "node $target is not discovered — check the ROS_DOMAIN_ID, that the node is running, and that it's within multicast/gossip reach", copy(_REACH_CAVEATS))
        end
        push!(layers, ReachLayer(:resolve, true, "node $target lives on session $resolved_zid"))
    end

    # ── graph presence (for zid/node targets) ─────────────────────────────────
    if resolved_zid !== nothing
        n_ep = length(endpoints_on_session(idx, resolved_zid))
        present = n_ep > 0 || any(n -> n.z_id == resolved_zid, snapshot_nodes(idx))
        push!(layers, ReachLayer(:graph, present,
            present ? "session $resolved_zid is advertising $n_ep endpoint(s) in the ROS graph" :
                      "session $resolved_zid is not present in the ROS graph"))
    end

    # ── direct transport link ──────────────────────────────────────────────────
    routers = connected_routers(x)
    linkset = Set(routers); union!(linkset, connected_peers(x)); push!(linkset, local_zid(x))
    linked = resolved_zid !== nothing && resolved_zid in linkset
    if resolved_zid !== nothing
        push!(layers, ReachLayer(:link, linked,
            linked ? "we hold a direct hop-1 link to $resolved_zid" :
            (isempty(routers) ? "no direct link and no router attached" :
             "no direct link, but we are attached to router(s) $(routers) that can relay")))
    end

    # ── scouting (passive discovery; fills in locators for zid/node) ────────────
    scouted = false
    try
        for p in scout_peers(; timeout_ms=min(timeout_ms, 1000))
            if resolved_zid === nothing || p.zid == resolved_zid
                resolved_zid === nothing && continue
                scouted = true
                isempty(p.locators) || (locators = p.locators)
                break
            end
        end
    catch err
        push!(layers, ReachLayer(:scout, nothing, "scouting failed: $(sprint(showerror, err))"))
    end
    if resolved_zid !== nothing
        push!(layers, ReachLayer(:scout, scouted,
            scouted ? "heard $resolved_zid via scouting; locators=$(locators)" :
                      "did not hear $resolved_zid via scouting (off-segment, gossip-only, or multicast disabled)"))
    end

    # ── active probe (opt-in) ──────────────────────────────────────────────────
    if active
        if isempty(locators)
            push!(layers, ReachLayer(:tcp, nothing, "no locator known for the target — nothing to probe directly"))
        else
            tcp_ok = false
            for loc in locators
                hp = _locator_hostport(loc)
                hp === nothing && continue
                ok, detail = _tcp_probe(hp[1], hp[2], timeout_s)
                push!(layers, ReachLayer(:tcp, ok, detail))
                ok && (tcp_ok = true; break)
            end
            if tcp_ok
                ok, detail = _zenoh_probe(locators, timeout_s)
                push!(layers, ReachLayer(:zenoh, ok, detail))
            end
        end
    end

    return _verdict(String(target), kind, resolved_zid, locators, layers, linked, routers, active)
end

# Derive the verdict from the walked layers. Definite when a link/probe confirms or
# a probe refutes; unknown (nothing) when only passive evidence is inconclusive.
function _verdict(target, kind, resolved_zid, locators, layers, linked, routers, active)
    lyr(name) = (i = findfirst(l -> l.name === name, layers); i === nothing ? nothing : layers[i])
    zenoh = lyr(:zenoh); tcp = lyr(:tcp)

    if zenoh !== nothing && zenoh.ok === true
        return ReachReport(target, kind, resolved_zid, locators, layers, true,
            "reachable: a Zenoh link to the target established", copy(_REACH_CAVEATS))
    end
    if zenoh !== nothing && zenoh.ok === false
        return ReachReport(target, kind, resolved_zid, locators, layers, false,
            "unreachable at the Zenoh layer: $(zenoh.detail)", copy(_REACH_CAVEATS))
    end
    if tcp !== nothing && tcp.ok === false
        return ReachReport(target, kind, resolved_zid, locators, layers, false,
            "unreachable at the network layer: $(tcp.detail)", copy(_REACH_CAVEATS))
    end
    if linked
        return ReachReport(target, kind, resolved_zid, locators, layers, true,
            "reachable: a direct transport link to the target's session is established", copy(_REACH_CAVEATS))
    end
    if resolved_zid !== nothing && !isempty(routers)
        return ReachReport(target, kind, resolved_zid, locators, layers, true,
            "likely reachable via router $(routers) (the target is in the graph but not directly linked)" *
            (active ? "" : " — pass active=true to confirm with a direct probe"), copy(_REACH_CAVEATS))
    end
    return ReachReport(target, kind, resolved_zid, locators, layers, nothing,
        "indeterminate from passive evidence" * (active ? " and no usable locator to probe" :
         " — pass active=true to probe the locator directly"), copy(_REACH_CAVEATS))
end
