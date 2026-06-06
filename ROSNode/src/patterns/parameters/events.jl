# ── on-change events (§10) ───────────────────────────────────────────────────────
# `/parameter_events` is a batched, post-commit notification: one message per
# transaction listing the (new/changed/deleted) parameters. We fan it to in-process
# listeners now and TODO publish the `rcl_interfaces/msg/ParameterEvent` on the
# wire once the §8 service layer + generated type land.

"""
    ParameterEventBatch

The post-commit change set for one transaction (§10): `changed` is name → new
value for declared+dynamic fields whose value moved, with the prior value in
`previous`. The shape the in-process listeners receive; the wire
`rcl_interfaces/msg/ParameterEvent` is assembled from it by the §8 service layer.
"""
struct ParameterEventBatch
    changed::Dict{Symbol, Any}
    previous::Dict{Symbol, Any}
    stamp_ns::Int64
end

"""
    on_parameter_event(f, server) -> f

Register `f(batch::ParameterEventBatch)` to run after each committed transaction
(§10) — the in-process side of `/parameter_events`. Listeners run post-swap,
outside no lock guarantee beyond the mutation lock the commit holds; a throwing
listener is logged, never fatal.
"""
function on_parameter_event(f, s::ParameterServer)
    push!(s.listeners, f)
    return f
end

# Assemble the change batch (only fields whose value actually moved) and fan it to
# listeners + the wire publisher. `use_sim_time` flips the node's clock routing
# (§7) — that hook is fired here when it changes. Caller holds `s.lock`.
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

    # Well-known param: a change to `use_sim_time` reroutes the node's ROS clock
    # (§7). Fire the hook so `now(node)`/ROS timers follow `/clock`.
    haskey(changed, :use_sim_time) && _on_use_sim_time_changed(s, changed[:use_sim_time])

    batch = ParameterEventBatch(changed, previous, _server_stamp_ns(s))
    for f in s.listeners
        try
            f(batch)
        catch err
            @error "on_parameter_event listener threw" exception=(err, catch_backtrace())
        end
    end

    # TODO(layer §8): publish `rcl_interfaces/msg/ParameterEvent` on
    # `/parameter_events` via `s._events_pub` once the Service layer + generated
    # type are available; the publisher is attached by `wire_parameter_services!`.
    if s._events_pub !== nothing
        try
            _publish_parameter_event(s, batch)
        catch err
            @error "/parameter_events publish failed" exception=(err, catch_backtrace())
        end
    end
    return nothing
end

# Wall-clock stamp for the event; falls back to raw ns if the node has no clock
# surface yet (this file stays decoupled from the §7 wiring).
function _server_stamp_ns(s::ParameterServer)
    node = s.node
    node === nothing && return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    try
        return nanoseconds(Dates.now(node, System()))
    catch
        return round(Int64, Dates.datetime2unix(Dates.now()) * 1e9)
    end
end

# `use_sim_time` toggle hook (§7). The Context hosts the single `/clock` sub and
# per-node opt-in; this routes the committed value to it so `now(node, ROS())` follows
# `/clock` and registered jump callbacks fire. Runs under the commit `s.lock` (see
# `set_use_sim_time!` for why synchronous activation is safe).
function _on_use_sim_time_changed(s::ParameterServer, enabled)
    node = s.node
    node === nothing && return nothing
    set_use_sim_time!(_ctx(node), node, Bool(enabled))
    return nothing
end

