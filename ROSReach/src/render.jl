# Rendering: human-facing views over the query layer. `graph_report` is a
# `ros2 node info`-style text dump (nodes → endpoints, topics → types, transport
# summary); `to_dot` emits Graphviz for the computation and/or transport graph.
# Plus terse `Base.show` methods so results are legible at the REPL.

using ROSZenoh: TypeInfo, EndpointKind

_typestr(t::Union{TypeInfo, Nothing}) = t === nothing ? "<no type>" : t.name
_kindstr(k::EndpointKind) = k === Publisher ? "pub" : k === Subscription ? "sub" :
                            k === Service ? "srv" : "cli"
_shortzid(z::AbstractString) = isempty(z) ? "?" : (length(z) <= 12 ? z : z[1:12] * "…")

# ── Base.show ────────────────────────────────────────────────────────────────

Base.show(io::IO, e::Endpoint) =
    print(io, _kindstr(e.kind), " ", e.topic, " :: ", _typestr(e.type),
          " [", _shortzid(e.z_id), e.is_local ? " local" : "", "]")

Base.show(io::IO, n::GraphNode) =
    print(io, isempty(n.namespace) || n.namespace == "/" ? "/" : n.namespace * "/",
          n.name, " [", _shortzid(n.z_id), "]")

Base.show(io::IO, p::TransportPeer) =
    print(io, p.role, " ", _shortzid(p.zid), p.connected ? " (linked)" : "",
          isempty(p.locators) ? "" : " @ $(p.locators)")

function Base.show(io::IO, t::TransportTopology)
    print(io, "TransportTopology(self=", _shortzid(t.self), ", mode=", t.mode,
          ", routers=", length(t.routers), ", peers=", length(t.peers), ")")
end

Base.show(io::IO, e::TopicEdge) =
    print(io, e.pub.node_name, " → ", e.sub.node_name, " on ", e.topic,
          " [", e.type_ok ? (e.type_verified ? "type✓" : "type?") : "type✗",
          " ", e.qos_ok ? "qos✓" : "qos✗", "]")

Base.show(io::IO, w::WiringWarning) =
    print(io, "⚠ ", _kindstr(w.consumer.kind), " ", w.consumer.topic,
          " (", w.consumer.node_name, ") has no ", _kindstr(w.producer.kind), "; ",
          _kindstr(w.producer.kind), " ", w.producer.topic,
          " (", w.producer.node_name, ") shares its basename")

# ── graph_report ───────────────────────────────────────────────────────────────

"""
    graph_report([io], idx, x=idx; transport=true)

Print a `ros2 node info`-style dump of the live graph to `io` (default `stdout`):
each node with its publishers/subscriptions/services, every topic with the
type(s) on it, and — when `transport=true` and `x` can supply a session — a
transport summary (self z_id, connected routers/peers, inferred mode). Topics with
more than one type are flagged (a wire-level type mismatch).
"""
graph_report(idx::ReachIndex, x=idx; kwargs...) = graph_report(stdout, idx, x; kwargs...)

function graph_report(io::IO, idx::ReachIndex, x=idx; transport::Bool=true)
    eps = snapshot_endpoints(idx)
    nodes = node_names(idx)
    println(io, "ROS graph — ", length(nodes), " node(s), ", length(eps), " endpoint(s)")

    println(io, "\nNodes:")
    if isempty(nodes)
        println(io, "  (none discovered yet)")
    end
    for n in sort(nodes; by = n -> (n.namespace, n.name))
        println(io, "  ", n)
        ne = filter(e -> e.node_name == n.name && e.namespace == n.namespace, eps)
        for e in sort(ne; by = e -> (_kindstr(e.kind), e.topic))
            println(io, "    ", _kindstr(e.kind), "  ", rpad(e.topic, 28), " ", _typestr(e.type))
        end
    end

    println(io, "\nTopics:")
    tnt = topic_names_and_types(idx)
    for topic in sort(collect(keys(tnt)))
        types = tnt[topic]
        np = count_publishers(idx, topic); ns = count_subscribers(idx, topic)
        flag = length(types) > 1 ? "  ⚠ type mismatch" : ""
        println(io, "  ", rpad(topic, 28), " ", join((t.name for t in types), ", "),
                "  ($np pub, $ns sub)", flag)
    end

    warns = wiring_warnings(idx)
    if !isempty(warns)
        println(io, "\nWiring warnings — ", length(warns),
                " unmatched consumer(s) with a look-alike producer:")
        for w in warns
            println(io, "  ", w)
            println(io, "      ", w.note)
        end
    end

    if transport
        try
            t = transport_topology(x)
            println(io, "\nTransport:")
            println(io, "  self     ", _shortzid(t.self), "  (mode: ", t.mode, ")")
            println(io, "  routers  ", isempty(t.routers) ? "(none)" : join(_shortzid.(t.routers), ", "))
            println(io, "  peers    ", isempty(t.peers) ? "(none)" : join(_shortzid.(t.peers), ", "))
        catch err
            println(io, "\nTransport: (unavailable: ", sprint(showerror, err), ")")
        end
    end
    nothing
end

# ── ReachReport rendering ────────────────────────────────────────────────────

_mark(ok::Union{Bool, Nothing}) = ok === nothing ? "·" : (ok ? "✓" : "✗")

function Base.show(io::IO, ::MIME"text/plain", r::ReachReport)
    verdict = r.reachable === nothing ? "UNKNOWN" : (r.reachable ? "REACHABLE" : "UNREACHABLE")
    println(io, "reach ", r.target, "  →  ", verdict)
    r.resolved_zid === nothing || println(io, "  session: ", r.resolved_zid)
    isempty(r.locators) || println(io, "  locators: ", join(r.locators, ", "))
    println(io, "  why: ", r.reason)
    println(io, "  layers:")
    for l in r.layers
        println(io, "    [", _mark(l.ok), "] ", rpad(string(l.name), 8), " ", l.detail)
    end
    if !isempty(r.caveats)
        println(io, "  caveats:")
        for c in r.caveats
            println(io, "    - ", c)
        end
    end
end

Base.show(io::IO, r::ReachReport) =
    print(io, "ReachReport(", r.target, " → ",
          r.reachable === nothing ? "unknown" : (r.reachable ? "reachable" : "unreachable"), ")")

# ── Graphviz DOT ───────────────────────────────────────────────────────────────

"""
    to_dot([io], idx, x=idx; which=:both)

Emit Graphviz DOT for the graph(s). `which` is `:computation` (nodes ↔ topics,
rqt_graph-style; topics with a type mismatch in red), `:transport` (this session
→ its connected routers/peers), or `:both`. Pipe the output through `dot`:
`open("g.dot","w") do io; to_dot(io, idx); end` then `dot -Tpng g.dot -o g.png`.
"""
to_dot(idx::ReachIndex, x=idx; kwargs...) = (to_dot(stdout, idx, x; kwargs...))

function to_dot(io::IO, idx::ReachIndex, x=idx; which::Symbol=:both)
    println(io, "digraph ros {")
    println(io, "  rankdir=LR;")
    if which === :computation || which === :both
        _dot_computation(io, idx)
    end
    if which === :transport || which === :both
        _dot_transport(io, idx, x)
    end
    println(io, "}")
    nothing
end

_dot_id(s::AbstractString) = "\"" * replace(s, "\"" => "\\\"") * "\""

function _dot_computation(io::IO, idx::ReachIndex)
    eps = snapshot_endpoints(idx)
    tnt = topic_names_and_types(idx)
    println(io, "  subgraph cluster_comp { label=\"computation\"; color=gray;")
    for n in node_names(idx)
        fqn = (n.namespace == "/" || isempty(n.namespace) ? "/" : n.namespace * "/") * n.name
        println(io, "    ", _dot_id(fqn), " [shape=box, style=filled, fillcolor=lightblue];")
    end
    for (topic, types) in tnt
        color = length(types) > 1 ? "red" : "black"
        println(io, "    ", _dot_id(topic), " [shape=ellipse, color=", color, "];")
    end
    for e in eps
        isempty(e.node_name) && continue
        fqn = (e.namespace == "/" || isempty(e.namespace) ? "/" : e.namespace * "/") * e.node_name
        if e.kind === Publisher
            println(io, "    ", _dot_id(fqn), " -> ", _dot_id(e.topic), ";")
        elseif e.kind === Subscription
            println(io, "    ", _dot_id(e.topic), " -> ", _dot_id(fqn), ";")
        end
    end
    println(io, "  }")
end

function _dot_transport(io::IO, idx::ReachIndex, x)
    println(io, "  subgraph cluster_xport { label=\"transport\"; color=gray;")
    try
        t = transport_topology(x)
        self = "self:" * _shortzid(t.self)
        println(io, "    ", _dot_id(self), " [shape=doublecircle, style=filled, fillcolor=gold];")
        for r in t.routers
            id = "router:" * _shortzid(r)
            println(io, "    ", _dot_id(id), " [shape=hexagon, style=filled, fillcolor=orange];")
            println(io, "    ", _dot_id(self), " -> ", _dot_id(id), " [dir=both];")
        end
        for p in t.peers
            id = "peer:" * _shortzid(p)
            println(io, "    ", _dot_id(id), " [shape=box];")
            println(io, "    ", _dot_id(self), " -> ", _dot_id(id), " [dir=both];")
        end
    catch err
        println(io, "    \"transport unavailable\" [shape=note];")
    end
    println(io, "  }")
end
