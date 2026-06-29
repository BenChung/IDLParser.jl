# The Recorder: owns the Context/index/node/sink and the reconcile loop (DESIGN.md §1, §7).
# Phase 2: graph recording (discovery → diff → GraphNodes/GraphEdges/events/meta/Clear).
# Data subscriptions (Phase 4) attach here later.

const _EpKey = Tuple{String,Int,Int,String,String}  # (z_id, node_id, entity_id, type-name, topic)
const _NdKey = Tuple{String,String,String}          # (z_id, name, namespace)

mutable struct Recorder
    config::RecorderConfig
    ctx::ROSNode.Context
    own_ctx::Bool
    idx::ROSReach.ReachIndex
    node::ROSNode.Node
    sink::Sink
    local_zid::String
    dirty::Threads.Atomic{Bool}
    running::Threads.Atomic{Bool}
    stopping::Threads.Atomic{Bool}
    prev_eps::Dict{_EpKey,ROSReach.Endpoint}
    prev_nodes::Dict{_NdKey,ROSReach.GraphNode}
    prev_topics::Set{String}
    subs::Dict{String,Any}                  # topic → DynamicSubscriptionHandle (data path)
    data_count::Threads.Atomic{Int}
    data_errors::Threads.Atomic{Int}
    task::Union{Task,Nothing}
end

"""
    Recorder(config; ctx=nothing)

Open a recorder. With `ctx`, observe that Context's session (the recorder adds its own
node/index to it); without, create one. `start!` begins recording; `stop!` finishes.
"""
function Recorder(config::RecorderConfig; ctx::Union{ROSNode.Context,Nothing}=nothing)
    own = ctx === nothing
    # An owned Context must not drain on a reaction throw (the default :shutdown would tear
    # the recorder down on one bad message); a shared Context belongs to the host (DESIGN.md §7).
    c = own ? ROSNode.Context(; on_reaction_error = :continue) : ctx
    idx = ROSReach.open_index(c; history=true)
    node = ROSNode.Node(c, "reros_recorder")
    sink = Sink(config.app_id; recording_id=config.recording_id, sinks=config.sinks,
                timelines=config.timelines, capacity=config.capacity, drop_when_full=config.drop_when_full)
    return Recorder(config, c, own, idx, node, sink, idx.local_zid,
                    Threads.Atomic{Bool}(false), Threads.Atomic{Bool}(false), Threads.Atomic{Bool}(false),
                    Dict{_EpKey,ROSReach.Endpoint}(), Dict{_NdKey,ROSReach.GraphNode}(), Set{String}(),
                    Dict{String,Any}(), Threads.Atomic{Int}(0), Threads.Atomic{Int}(0), nothing)
end

_ep_key(e)::_EpKey = (e.z_id, e.node_id, e.entity_id, e.type === nothing ? "" : e.type.name, e.topic)
_nd_key(n)::_NdKey = (n.z_id, n.name, n.namespace)

# Self-exclusion is NODE-granular, not blanket is_local (DESIGN.md §2.3, arch-4): drop only the
# recorder's OWN node's entities. Standalone (recorder owns the session) → that's all local
# entities; shared Context (§8 embed) → the host node + siblings ARE recorded.
_is_own_ep(rec::Recorder, e)   = e.z_id == rec.local_zid && _fqn(e.namespace, e.node_name) == rec.node.fqn
_is_own_node(rec::Recorder, n) = n.z_id == rec.local_zid && _fqn(n.namespace, n.name) == rec.node.fqn
_keep_ep(rec::Recorder, e) =
    !_is_own_ep(rec, e) && !_denied_node(rec.config, e.node_name) &&
    !_denied_topic(rec.config, e.topic) && !conn_denied(rec.config, e)
_keep_node(rec::Recorder, n) =
    !_is_own_node(rec, n) && !_denied_node(rec.config, n.name) && !conn_denied_node(rec.config, n)

"""One reconcile pass: snapshot → filter → diff → log graph/events/meta/Clear."""
function reconcile!(rec::Recorder)
    eps = filter(e -> _keep_ep(rec, e), ROSReach.snapshot_endpoints(rec.idx))
    nds = filter(n -> _keep_node(rec, n), ROSReach.snapshot_nodes(rec.idx))
    times = TimeStamps(_now_ns(), nothing, next_seq!(rec.sink))

    cur_e = Dict{_EpKey,ROSReach.Endpoint}(_ep_key(e) => e for e in eps)
    cur_n = Dict{_NdKey,ROSReach.GraphNode}(_nd_key(n) => n for n in nds)
    added_e   = [e for (k, e) in cur_e if !haskey(rec.prev_eps, k)]
    removed_e = [e for (k, e) in rec.prev_eps if !haskey(cur_e, k)]
    added_n   = [n for (k, n) in cur_n if !haskey(rec.prev_nodes, k)]
    removed_n = [n for (k, n) in rec.prev_nodes if !haskey(cur_n, k)]

    cur_topics = Set{String}(e.topic for e in eps
                             if e.kind == ROSReach.Publisher || e.kind == ROSReach.Subscription)
    removed_topics = setdiff(rec.prev_topics, cur_topics)

    log_graph!(rec.sink, eps, nds, times)
    log_events!(rec.sink, added_e, removed_e, added_n, removed_n, times)
    log_meta!(rec.sink, eps, nds, times)
    clear_removed!(rec.sink, removed_n, times)
    clear_removed_topics!(rec.sink, removed_topics, times)   # clear the stale /_meta docs
    reconcile_subs!(rec, eps)             # open/close data subscriptions (subscribe.jl)

    rec.prev_eps = cur_e
    rec.prev_nodes = cur_n
    rec.prev_topics = cur_topics
    return rec
end

"""Begin recording: hook `on_change` (coalescing flag) and run the reconcile loop. Idempotent
— a second call while running is a no-op (no duplicate listener or orphaned loop task)."""
function start!(rec::Recorder; interval::Real=0.2)
    Threads.atomic_cas!(rec.running, false, true) && return rec   # already running
    ROSReach.on_change(rec.idx) do _
        rec.dirty[] = true        # never block the index consumer task (DESIGN.md §2.1)
    end
    rec.dirty[] = true            # force an initial pass
    rec.task = Threads.@spawn _reconcile_loop(rec, Float64(interval))
    Base.errormonitor(rec.task)
    return rec
end

function _reconcile_loop(rec::Recorder, interval::Float64)
    while rec.running[]
        if Threads.atomic_xchg!(rec.dirty, false)
            try
                reconcile!(rec)
            catch err
                @error "ReROS: reconcile failed" exception=(err, catch_backtrace())
            end
        end
        sleep(interval)
    end
    return nothing
end

"""Stop recording: halt the loop, do a final reconcile, drain the sink, free resources.
Idempotent and safe under concurrent invocation — exactly one caller runs the teardown."""
function stop!(rec::Recorder)
    Threads.atomic_cas!(rec.stopping, false, true) && return rec   # another caller owns teardown
    if rec.running[]
        rec.running[] = false
        rec.task === nothing || wait(rec.task)
    end
    try; reconcile!(rec); catch; end       # a final pass (may no-op if the session is tearing down)
    for (_, s) in rec.subs
        try; close(s); catch; end
    end
    empty!(rec.subs)
    close(rec.sink)
    try; close(rec.idx); catch; end
    rec.own_ctx && close(rec.ctx)
    return rec
end

"""
    attach_recorder!(node, config) -> Recorder

Embed a recorder in an existing node, sharing its Context (DESIGN.md §8) — the "thin adapter"
form. Starts recording immediately and stops on the host Context's shutdown. Self-exclusion is
node-granular, so the host node and its siblings ARE recorded; only the recorder's own node is
filtered out. (For a launch-composable `@component`, wrap this in a component's lifecycle hooks.)
"""
function attach_recorder!(node::ROSNode.Node, config::RecorderConfig)
    rec = Recorder(config; ctx = node.context)
    start!(rec)
    ROSNode.on_shutdown(() -> stop!(rec), node.context)
    return rec
end
