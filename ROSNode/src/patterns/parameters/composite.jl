# ── the multi-schema parameter façade (§4.4) ─────────────────────────────────────
# A composed node (`@node N = [ … ]`) has one node-core but several member mixins,
# each with its own typed `ParameterServer{P_M}`. To a ROS2 parameter client the
# node must still look like ONE node: a single flat namespace, one set of the six
# standard services, one `/parameter_events`. `CompositeParameterServer` is that
# façade — it aggregates the member servers behind member-prefixed names
# (`<member>.<field>`, the §4.3 prefix generalised, dot-separated as ROS2 nests
# parameters) and reuses the schema-independent service core (services.jl) verbatim
# by implementing the same six reflection handlers + `parameter_names`.
#
# Each member keeps its own `ParameterServer{P_M}`, so `parameters(m)` stays
# mixin-local and type-stable (§3.5); the façade is only the node-level view. It is
# wired for a composed node, while a single mixin promoted to a node (`run(M)`)
# keeps the un-prefixed plain server (§4.3) — the prefix tracks the construction
# path, not the member count.

export CompositeParameterServer

"""
    CompositeParameterServer(node, members) -> CompositeParameterServer

The node-level parameter view of a composed node (§4.4): a façade over the
members' per-mixin [`ParameterServer`](@ref)s that presents one flat,
member-prefixed `ros2 param` namespace (`<member>.<field>`, dot-separated as
ROS 2 nests parameters), one `/parameter_events` stream, and one set of the six
standard parameter services. `members` is a
`Vector{Pair{Symbol,ParameterServer}}` of member-name => its server in declared
order — the order `ros2 param list` reflects. Reached as `node.parameters` for
a composed node.

Each member keeps its own typed `ParameterServer{P_M}` (so a member's own
`parameters(m)` stays type-stable); the façade is only the node-level aggregate.
It reuses the schema-independent service core by implementing the same six
reflection handlers plus [`parameter_names`](@ref). It has no node-level dynamic
tier — [`dynamic_parameters`](@ref) on it raises `ArgumentError`, directing you
to a member's own server. A node-level atomic set
([`set_parameters_atomically`](@ref)) validates every affected member first and
commits only if all pass, coalescing the per-member commits into one
`/parameter_events`.
"""
mutable struct CompositeParameterServer <: AbstractParameterServer
    const node::Any                                       # the shared node-core
    const members::Vector{Pair{Symbol, ParameterServer}}  # member name => its server, declared order
    _events_pub::Any                                      # /parameter_events publisher (set by wiring)
    const services::Vector{Any}                           # the six wired parameter-service handles
end

CompositeParameterServer(node, members::Vector{Pair{Symbol, ParameterServer}}) =
    CompositeParameterServer(node, members, nothing, Any[])

# A composed node has no node-level dynamic (undeclared) tier — each member's typed
# `ParameterServer{P_M}` owns its own. Surface that as a clear error (composed
# `node.parameters` is a façade), not a `MethodError` from the `ParameterServer`-only form.
dynamic_parameters(::CompositeParameterServer) =
    throw(ArgumentError("a composed @node has no node-level dynamic parameters; reach a member's server " *
                        "(member schemas are typed — `parameters(m)`)"))

Base.show(io::IO, s::CompositeParameterServer) =
    print(io, "CompositeParameterServer(", length(s.members), " members: ",
          join((String(nm) for (nm, _) in s.members), ", "), ")")

# The member server for a name, or `nothing` if no such member.
function _member_server(s::CompositeParameterServer, member::Symbol)
    for (nm, srv) in s.members
        nm === member && return srv
    end
    return nothing
end

# Split a flat `<member>.<field>` name on its FIRST dot → `(member, field)`, or
# `nothing` when unprefixed. Member and field names are Julia identifiers (no dots),
# and members are one level deep, so the first dot is the boundary.
function _split_prefixed(name)
    str = String(name)
    i = findfirst(==('.'), str)
    i === nothing && return nothing
    return (Symbol(SubString(str, 1, prevind(str, i))),
            Symbol(SubString(str, nextind(str, i))))
end

# Resolve a flat name to its `(member-server, field)`, or `nothing` when the name is
# unprefixed, names no live member, or names no declared field on that member.
function _resolve_member_field(s::CompositeParameterServer, name)
    sp = _split_prefixed(name)
    sp === nothing && return nothing
    member, field = sp
    srv = _member_server(s, member)
    (srv !== nothing && field in declared_names(srv)) ? (srv, field) : nothing
end

# ── the six reflection handlers, flat over `<member>.<field>` (§4.4) ─────────────
# Each mirrors its `ParameterServer` counterpart (services.jl) but splits the
# prefixed name and delegates to the owning member server; outputs re-prefix.

function parameter_names(s::CompositeParameterServer)
    names = Symbol[]
    for (nm, srv) in s.members
        for f in declared_names(srv)
            push!(names, Symbol(nm, ".", f))
        end
    end
    return names
end

function describe_parameters(s::CompositeParameterServer, names)
    out = ParameterDescriptor[]
    for name in names
        rf = _resolve_member_field(s, name)
        if rf === nothing
            push!(out, ParameterDescriptor(Symbol(name), Nothing, PARAMETER_NOT_SET, "", nothing, false, nothing))
        else
            srv, field = rf
            d = srv.by_name[field]   # re-key onto the prefixed name; everything else is the member's
            push!(out, ParameterDescriptor(Symbol(name), d.type, d.ptype, d.description,
                                           d.constraint, d.read_only, d.default))
        end
    end
    return out
end

function get_parameter_types(s::CompositeParameterServer, names)
    map(names) do name
        rf = _resolve_member_field(s, name)
        rf === nothing ? PARAMETER_NOT_SET :
            parameter_type(fieldtype(schema_type(rf[1]), rf[2]))
    end
end

function get_parameters(s::CompositeParameterServer, names)
    map(names) do name
        rf = _resolve_member_field(s, name)
        rf === nothing ? nothing : parameter(rf[1], rf[2])
    end
end

function list_parameters(s::CompositeParameterServer; prefixes=(), depth::Integer=0)
    _filter_names(parameter_names(s), prefixes, depth)
end

# Per-item set: each `(name, value)` is its own (single-member) atomic transaction,
# yielding one independent `(successful, reason)` — ROS2's `SetParameters`.
function set_parameters(s::CompositeParameterServer, pairs)
    map(pairs) do (name, value)
        set_parameters_atomically(s, ((name, value),))
    end
end

# All-or-nothing across members. Members own independent locks, so true node-wide
# atomicity would need a two-phase commit; we approximate it: group by member,
# validate every member's candidate first (coerce + per-field constraints + the user
# `validate` hook, no commit), and only commit once all pass — so a rejection on any
# member commits nothing. The residual gap is a concurrent set on a member between
# the validate and commit phases, acceptable for a composed node's parameter surface.
function set_parameters_atomically(s::CompositeParameterServer, pairs)
    bymember = Dict{Symbol, Vector{Tuple{Symbol, Any}}}()
    order = Symbol[]
    for (name, value) in pairs
        sp = _split_prefixed(name)
        sp === nothing && return (false, "parameter $(name): expected a `<member>.<field>` name")
        member, field = sp
        srv = _member_server(s, member)
        srv === nothing && return (false, "parameter $(name): no member `$(member)`")
        field in declared_names(srv) ||
            return (false, "parameter $(name): member `$(member)` has no parameter `$(field)`")
        haskey(bymember, member) || push!(order, member)
        push!(get!(Vector{Tuple{Symbol, Any}}, bymember, member), (field, value))
    end

    # phase 1 — validate every member's candidate without committing.
    for member in order
        srv = _member_server(s, member)
        P = schema_type(srv)
        try
            base = current(srv)
            coerced = Dict{Symbol, Any}(f => _coerce_param(fieldtype(P, f), v) for (f, v) in bymember[member])
            candidate = setproperties(base, NamedTuple(coerced))
            _validate_candidate(srv.by_name, base, candidate, keys(coerced))
        catch err
            err isa ParameterRejection && return (false, "$(member).$(err.reason)")
            err isa ArgumentError && return (false, "$(member): " * sprint(showerror, err))
            rethrow()
        end
    end

    # phase 2 — commit. Each member's commit fires its forwarding listener; a node-level
    # atomic set must surface as ONE `/parameter_events` (rclcpp parity), not one per
    # member. We collect the member batches into a task-local buffer (the member
    # transactions + their synchronous listeners run on *this* task, so a concurrent
    # direct `parameters(m)` set on another task is unaffected and still publishes its
    # own event), then publish a single combined event. `transaction` re-validates under
    # the member's lock, so a concurrent set that moved the base between phase 1 and here
    # can still reject — mapped to `(false, reason)` (the `SetParametersResult` contract).
    entries = Tuple{String, ParameterEventBatch}[]
    failure = nothing
    task_local_storage(_PARAM_COLLECTOR_KEY, entries) do
        for member in order
            srv = _member_server(s, member)
            try
                transaction(srv) do p
                    for (f, v) in bymember[member]
                        setproperty!(p, f, v)
                    end
                end
            catch err
                err isa ParameterRejection ? (failure = (false, "$(member).$(err.reason)")) :
                err isa ArgumentError       ? (failure = (false, "$(member): " * sprint(showerror, err))) :
                                              rethrow()
                break
            end
        end
    end
    # Publish one event for whatever did commit (all, in the common case; the already-
    # committed prefix on a rare phase-2 race), then report the verdict.
    isempty(entries) || _publish_event!(s, entries)
    return failure === nothing ? (true, "") : failure
end

# ── node-level /parameter_events (§4.4) ──────────────────────────────────────────
# The façade owns the one `/parameter_events` publisher (created by the wiring); the
# member servers have none. A forwarding listener on each member republishes that
# member's post-commit batch on the node's publisher with prefixed names — so a
# change driven through the node-level `~/set_parameters` AND one made directly via
# `parameters(m)`/`@on_parameter` both surface a node-level event. When a node-level
# atomic set is in progress on this task, the listener instead *collects* its batch
# into the task-local buffer so the whole set publishes as one event (above).

# Task-local key under which `set_parameters_atomically` parks its batch collector.
const _PARAM_COLLECTOR_KEY = :__rosnode_param_event_collector__

function _wire_composite_events!(s::CompositeParameterServer)
    for (nm, srv) in s.members
        prefix = String(nm)
        on_parameter_event(srv) do batch
            _on_member_event!(s, prefix, batch)
        end
    end
    return s
end

# The forwarding listener: append to the current task's collector when a node-level
# atomic set is running (coalesced into one event by the caller), else publish this one
# member's change immediately. Task-local, so a concurrent direct mutation on another
# task always takes the immediate path.
function _on_member_event!(s::CompositeParameterServer, prefix::String, batch::ParameterEventBatch)
    collector = get(task_local_storage(), _PARAM_COLLECTOR_KEY, nothing)
    if collector === nothing
        _publish_event!(s, Tuple{String, ParameterEventBatch}[(prefix, batch)])
    else
        push!(collector::Vector{Tuple{String, ParameterEventBatch}}, (prefix, batch))
    end
    return nothing
end

# Assemble + publish ONE node-level `ParameterEvent` from a set of `(member-prefix,
# batch)` entries, prefixing each moved name with `<member>.`. Mirrors
# `_publish_parameter_event` (services.jl) but over the façade's publisher.
function _publish_event!(s::CompositeParameterServer, entries::Vector{Tuple{String, ParameterEventBatch}})
    s._events_pub === nothing && return nothing
    node = s.node
    new_params     = _Parameter[]
    changed_params = _Parameter[]
    for (prefix, batch) in entries
        for (name, value) in batch.changed
            p = _Parameter(; name = string(prefix, ".", name), value = _to_param_value(value))
            push!(haskey(batch.previous, name) ? changed_params : new_params, p)
        end
    end
    (isempty(new_params) && isempty(changed_params)) && return nothing
    # The merged event carries the newest member commit's stamp.
    sec, nanosec = _sec_nanosec(maximum(b.stamp_ns for (_, b) in entries))
    ev = _ParameterEvent(; stamp = _Time(; sec, nanosec),
        node = node === nothing ? "" : String(node.fqn),
        new_parameters = new_params, changed_parameters = changed_params,
        deleted_parameters = _Parameter[])
    publish(s._events_pub, ev)
    return nothing
end
