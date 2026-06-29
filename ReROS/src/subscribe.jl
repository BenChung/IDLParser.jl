# Data path (DESIGN.md §3): reconcile the desired-vs-active dynamic subscriptions, and
# for each delivered (decoded) message run it through the mapper into the sink.

# Desired = topics with a publisher the config selects for data. Reconcile against the
# active DynamicSubscriptionHandles: open new, close gone. The subscription QoS is derived
# from the discovered publishers (DESIGN.md §3.3) so the recorder actually matches them
# (incl. transient_local latched history, via the ROSNode dynamic-sub advanced path).
function reconcile_subs!(rec::Recorder, eps)
    desired = Dict{String,Vector{Any}}()      # topic → its publisher endpoints
    for e in eps
        e.kind == ROSReach.Publisher || continue
        wants_data(rec.config, e) && push!(get!(desired, e.topic, Any[]), e)
    end
    for (t, pubs) in desired
        haskey(rec.subs, t) && continue
        try
            rec.subs[t] = _open_data_sub(rec, t, pubs)
        catch err
            @error "ReROS: failed to open data subscription" topic=t exception=(err, catch_backtrace()) maxlog=5
        end
    end
    for t in collect(keys(rec.subs))
        haskey(desired, t) && continue
        try; close(rec.subs[t]); catch; end
        delete!(rec.subs, t)
    end
    return nothing
end

# Permissive QoS that matches the discovered publishers (DESIGN.md §3.3): best-effort if any
# publisher is, transient_local if any offers it (to capture latched state). The depth tracks
# the largest publisher depth but is clamped to `cap` — the recorder is not a buffer.
function _derive_qos(pubs; cap::Int = 10)
    isempty(pubs) && return ROSNode.default_qos()
    rel = any(e -> e.qos.reliability === :best_effort, pubs) ? :best_effort : :reliable
    dur = any(e -> e.qos.durability === :transient_local, pubs) ? :transient_local : :volatile
    depth = maximum(e -> (e.qos.history === :keep_all ? cap : max(1, e.qos.depth)), pubs; init = 1)
    return ROSReach.QosProfile(reliability = rel, durability = dur, history = :keep_last,
                               depth = min(cap, depth))
end

# Resolve the topic's Julia type from a discovered publisher (name + RIHS), or `nothing`.
function _resolve_topic_type(rec::Recorder, pubs)
    for e in pubs
        e.type === nothing && continue
        T = try
            ROSNode.resolve_or_discover(rec.node, e.type.name, e.type.hash)
        catch; nothing end
        T === nothing || return T
    end
    return nothing
end

# Open a data subscription. transient_local needs the latched-history query, which only works
# over a concrete keyexpr → use ROSNode's TYPED subscription there (resolving the discovered
# type); otherwise the keyexpr-only dynamic sub (type resolved per sample). DESIGN.md §3.3.
function _open_data_sub(rec::Recorder, topic::AbstractString, pubs)
    base = _topic_entity(topic)
    t = String(topic)
    q = _derive_qos(pubs; cap = rec.config.sub_depth)
    r = rule_for(rec.config, first(pubs))                # the matched record rule
    mapper = r === nothing ? :auto : r.mapper            # per-topic mapper override (DESIGN-MAPPING §9)
    if q.durability === :transient_local
        T = _resolve_topic_type(rec, pubs)
        if T !== nothing
            # match=:weak keeps the latched-history (advanced) subscriber but wildcards the
            # type tail, so publishers with a different type/RIHS on this topic are still
            # captured (a per-sample type backstop decodes them).
            return ROSNode.Subscription(rec.node, topic, T; qos = q, match = :weak) do msg
                _record_msg(rec, t, base, mapper, msg)
            end
        end
        @warn "ReROS: transient_local topic type unresolved; recording live-only (no latched)" topic=t maxlog=3
    end
    return ROSNode.Subscription(rec.node, topic; qos = q) do msg
        _record_msg(rec, t, base, mapper, msg)
    end
end

# Per-sample: stamp, build a MapContext, map the decoded value into the sink. Isolated —
# a throwing mapper is counted and swallowed (the recorder must survive, DESIGN.md §5.2).
function _record_msg(rec::Recorder, topic::String, base::String, mapper, msg)
    Threads.atomic_add!(rec.data_count, 1)
    times = TimeStamps(_now_ns(), stamp_of(msg), next_seq!(rec.sink))
    ctx = MapContext(topic, base, times, 0, rec.config.opts, rec.config.mappers, mapper)
    try
        map_into!(rec.sink, msg, ctx)
    catch err
        Threads.atomic_add!(rec.data_errors, 1)
        @error "ReROS: mapper threw; message dropped" topic=topic exception=(err, catch_backtrace()) maxlog=5
    end
    return nothing
end
