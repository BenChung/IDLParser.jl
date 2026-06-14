# ── on-change events ──────────────────────────────────────────────────────────────
# `/parameter_events` is a batched, post-commit notification: one message per
# transaction listing the changed parameters. This file fans the batch to in-process
# listeners; the service wiring attaches the wire `rcl_interfaces/msg/ParameterEvent`
# publisher to `_events_pub`.

"""
    ParameterEventBatch

Post-commit change set for one transaction, delivered to in-process listeners.
`changed` maps each declared or dynamic parameter whose value moved to its new
value; `previous` holds the prior value. The service layer assembles the wire
`rcl_interfaces/msg/ParameterEvent` from this.

See the ROS 2 parameters concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html
"""
struct ParameterEventBatch
    changed::Dict{Symbol, Any}
    previous::Dict{Symbol, Any}
    stamp_ns::Int64
end

"""
    on_parameter_event(f, server::ParameterServer) -> f
    on_parameter_event(f, client::ParameterClient) -> f

Register a callback `f(batch::ParameterEventBatch)` for parameter changes
— the in-process and remote sides of ROS 2's `/parameter_events`. Returns `f`.

On a [`ParameterServer`](@ref) the listener runs synchronously after each
committed transaction, under the server's mutation lock; a throwing listener is
logged and the commit proceeds. The batch's `changed` maps each moved parameter
to its new value and `previous` holds the prior value.

On a [`ParameterClient`](@ref) it subscribes the remote's `/parameter_events`
and calls `f` per event from this client's target node, surfacing the event's
new and changed parameters. Client-side caveats:

  - a typed client coerces the changed values to their field types, and
    `batch.previous` is empty (the wire event carries only current values);
  - a remote's `deleted_parameters` (a parameter undeclaration) is not reported;
  - the node filter is an exact string compare of the event's `node` field against
    the resolved `target`, so a remote whose published FQN differs (namespace or
    normalization mismatch) has its events silently dropped;
  - the subscription is reaped by `close(client)`.
"""
function on_parameter_event(f, s::ParameterServer)
    push!(s.listeners, f)
    return f
end

# Assemble the change batch (only fields whose value moved) and fan it to listeners
# plus the wire publisher. Caller holds `s.lock`.
function _emit_parameter_event!(s::ParameterServer{P}, old::P, new::P,
                                overrides::AbstractDict, dyn_overrides::AbstractDict,
                                dyn_previous::AbstractDict) where {P}
    changed  = Dict{Symbol, Any}()
    previous = Dict{Symbol, Any}()
    for name in keys(overrides)
        nv = getfield(new, name); ov = getfield(old, name)
        isequal(nv, ov) && continue
        changed[name] = nv; previous[name] = ov
    end
    for (k, v) in dyn_overrides
        # An existing dynamic key carries its prior value (classifies as changed);
        # a re-set to the same value is a no-op, matching the declared-field path.
        if haskey(dyn_previous, k)
            isequal(v, dyn_previous[k]) && continue
            previous[k] = dyn_previous[k]
        end
        changed[k] = v
    end
    isempty(changed) && return nothing

    # `use_sim_time` is well-known: changing it switches the ROS clock to `/clock`,
    # so `now(node)` and ROS timers follow simulated time.
    haskey(changed, :use_sim_time) && _on_use_sim_time_changed(s, changed[:use_sim_time])

    batch = ParameterEventBatch(changed, previous, _server_stamp_ns(s))
    for f in s.listeners
        try
            f(batch)
        catch err
            @error "on_parameter_event listener threw" exception=(err, catch_backtrace())
        end
    end

    # `_events_pub` is attached by `wire_parameter_services!`; before then the wire
    # publish is skipped.
    if s._events_pub !== nothing
        try
            _publish_parameter_event(s, batch)
        catch err
            @error "/parameter_events publish failed" exception=(err, catch_backtrace())
        end
    end
    return nothing
end

# Event timestamp: the node's sim-aware ROS clock when one is attached, otherwise
# host wall-clock ns. The wire publish reuses this stamp, so in-process listeners
# and /parameter_events carry the same instant.
function _server_stamp_ns(s::ParameterServer)
    node = s.node
    node === nothing && return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    try
        return nanoseconds(Dates.now(node))
    catch
        return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    end
end

# `use_sim_time` toggle hook. The Context hosts the single `/clock` sub and per-node
# opt-in; this routes the committed value there so `now(node, ROS())` follows
# `/clock` and registered jump callbacks fire. Runs synchronously under the commit
# `s.lock`, which `set_use_sim_time!` is safe to be called under.
function _on_use_sim_time_changed(s::ParameterServer, enabled)
    node = s.node
    node === nothing && return nothing
    set_use_sim_time!(_ctx(node), node, Bool(enabled))
    return nothing
end

