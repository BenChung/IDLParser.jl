# ── on-change events ──────────────────────────────────────────────────────────────
# `/parameter_events` is a batched, post-commit notification: one message per
# transaction listing the changed parameters. This file fans the batch to in-process
# listeners; the wire `rcl_interfaces/msg/ParameterEvent` publish is attached by the
# service layer (see `_events_pub` below).

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
    on_parameter_event(f, server) -> f

Register `f(batch::ParameterEventBatch)` to run after each committed transaction,
the in-process side of `/parameter_events`. Listeners run synchronously under the
commit's mutation lock; a throwing listener is logged and the commit proceeds.
"""
function on_parameter_event(f, s::ParameterServer)
    push!(s.listeners, f)
    return f
end

# Assemble the change batch (only fields whose value moved) and fan it to listeners
# plus the wire publisher. A change to `use_sim_time` reroutes the node's clock,
# so its hook fires here. Caller holds `s.lock`.
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

    # `_events_pub` is attached by `wire_parameter_services!` once the service layer
    # is wired; until then the wire publish is skipped.
    if s._events_pub !== nothing
        try
            _publish_parameter_event(s, batch)
        catch err
            @error "/parameter_events publish failed" exception=(err, catch_backtrace())
        end
    end
    return nothing
end

# Event timestamp: the node's clock when one is attached, otherwise host wall-clock
# ns. Keeps this file decoupled from the clock wiring.
function _server_stamp_ns(s::ParameterServer)
    node = s.node
    node === nothing && return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    try
        return nanoseconds(Dates.now(node, System()))
    catch
        return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    end
end

# `use_sim_time` toggle hook. The Context hosts the single `/clock` sub and per-node
# opt-in; this routes the committed value there so `now(node, ROS())` follows
# `/clock` and registered jump callbacks fire. Runs under the commit `s.lock`;
# `set_use_sim_time!` documents why synchronous activation is safe under it.
function _on_use_sim_time_changed(s::ParameterServer, enabled)
    node = s.node
    node === nothing && return nothing
    set_use_sim_time!(_ctx(node), node, Bool(enabled))
    return nothing
end

