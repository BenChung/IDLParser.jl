# ── the six standard parameter services + /parameter_events (§10) ───────────────
# All generic over `P` via reflection (`fieldnames`/`fieldtypes` + `descriptors`),
# reflecting over the union of declared fields + dynamic dict so a ROS client sees
# one flat namespace. The handlers below are the schema-independent core; the wire
# binding (a `Service(node, …)` per service + the `/parameter_events` publisher)
# is the §8 Service-layer's job and is staged behind a precompile-safe stub.

"""
The six standard ROS 2 parameter service base names, under the node's private
namespace (§10). Every node exposes these so any parameter client can introspect
and mutate its parameters: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html
"""
const PARAMETER_SERVICE_NAMES = (
    "describe_parameters",
    "get_parameter_types",
    "get_parameters",
    "list_parameters",
    "set_parameters",
    "set_parameters_atomically",
)

"""
    describe_parameters(server::ParameterServer, names) -> Vector{ParameterDescriptor}
    describe_parameters(server::CompositeParameterServer, names) -> Vector{ParameterDescriptor}
    describe_parameters(client::ParameterClient, names; timeout_ms=2000) -> Vector{ParameterDescriptor}

The ROS 2 `DescribeParameters` service: a [`ParameterDescriptor`](@ref) per
requested name (§10). A declared name returns its schema descriptor; a live
dynamic name returns a synthesized descriptor (its runtime type, no constraint,
not read-only); an unknown name returns a `PARAMETER_NOT_SET` descriptor. The
[`ParameterServer`](@ref) and [`CompositeParameterServer`](@ref) forms reflect
locally; the [`ParameterClient`](@ref) form is the remote dual over the wire.
The wire carries name, type, description, and read-only faithfully but encodes
the numeric-range or choice-set constraint only as the human
`additional_constraints` string (and carries no default), so a client-decoded
descriptor reads back with `constraint === nothing` and `default === nothing`.
"""
function describe_parameters(s::ParameterServer{P}, names) where {P}
    out = ParameterDescriptor[]
    for name in names
        sym = Symbol(name)
        if haskey(s.by_name, sym)
            push!(out, s.by_name[sym])
        elseif s.allow_undeclared && (dv = _dynamic_find(s, sym)) !== nothing
            v = something(dv)
            push!(out, ParameterDescriptor(sym, typeof(v), parameter_type(typeof(v)),
                                           "", nothing, false, v))
        else
            # ROS2 returns a NOT_SET descriptor for an unknown name.
            push!(out, ParameterDescriptor(sym, Nothing, PARAMETER_NOT_SET, "", nothing, false, nothing))
        end
    end
    return out
end

"""
    get_parameter_types(server::ParameterServer, names) -> Vector{ParameterType}
    get_parameter_types(server::CompositeParameterServer, names) -> Vector{ParameterType}
    get_parameter_types(client::ParameterClient, names; timeout_ms=2000) -> Vector{ParameterType}

The ROS 2 `GetParameterTypes` service: the [`ParameterType`](@ref) tag of each
requested name (§10), flat over both tiers. An unknown name reads back as
`PARAMETER_NOT_SET`. The [`ParameterServer`](@ref) form reflects locally (the
declared field type, or the runtime type of a dynamic value); the
[`ParameterClient`](@ref) form is the remote dual over the wire, raising
[`ServiceError`](@ref) on a timeout or error reply.
"""
function get_parameter_types(s::ParameterServer{P}, names) where {P}
    map(names) do name
        sym = Symbol(name)
        if sym in fieldnames(P)
            parameter_type(fieldtype(P, sym))
        elseif s.allow_undeclared && (dv = _dynamic_find(s, sym)) !== nothing
            parameter_type(typeof(something(dv)))
        else
            PARAMETER_NOT_SET
        end
    end
end

"""
    get_parameters(server::ParameterServer, names) -> Vector
    get_parameters(server::CompositeParameterServer, names) -> Vector
    get_parameters(client::ParameterClient, names; timeout_ms=2000) -> Vector{Any}

The ROS 2 `GetParameters` service: the current value of each requested name
(§10). Reads flat over both tiers; an unset or unknown name reads back as
`nothing` (which the service maps to a `PARAMETER_NOT_SET` `ParameterValue`).
The [`ParameterServer`](@ref) form reads locally — declared fields from the
live atomic struct, dynamic ones from the side dict. The
[`ParameterClient`](@ref) form is the remote dual: one wire call decoded back to
native Julia values, raising [`ServiceError`](@ref) on a timeout or error reply.
"""
function get_parameters(s::ParameterServer{P}, names) where {P}
    map(names) do name
        sym = Symbol(name)
        if sym in fieldnames(P)
            getfield(@atomic(s.value), sym)
        elseif s.allow_undeclared && (dv = _dynamic_find(s, sym)) !== nothing
            something(dv)
        else
            nothing
        end
    end
end

"""
    list_parameters(server::ParameterServer; prefixes=(), depth=0) -> Vector{Symbol}
    list_parameters(server::CompositeParameterServer; prefixes=(), depth=0) -> Vector{Symbol}
    list_parameters(client::ParameterClient; prefixes=String[], depth=0, timeout_ms=2000) -> Vector{Symbol}

The ROS 2 `ListParameters` service: the flat union of parameter names,
optionally kept to those starting with one of `prefixes` (§10). The
[`ParameterServer`](@ref) form lists declared names then dynamic names; the
[`CompositeParameterServer`](@ref) form lists every member's `<member>.<field>`;
the [`ParameterClient`](@ref) form is the remote dual, raising
[`ServiceError`](@ref) on a timeout or error reply.

A name matches a prefix by plain `startswith` — `"nav"` also matches
`navigator.max_speed` — looser than rclcpp's separator-bounded match. `depth`
bounds how far past each matched prefix (past the root when `prefixes` is
empty) the listing reaches; `depth=0` is `DEPTH_RECURSIVE`, the full list.
Levels count as rclcpp counts them — `.` separators in the post-prefix tail,
the joining dot excluded — so an exact prefix match always survives and
`depth=1` reaches a prefix's direct children.
"""
function list_parameters(s::ParameterServer; prefixes=(), depth::Integer=0)
    _filter_names(parameter_names(s), prefixes, depth)
end

# The shared `ListParameters` prefix+depth filter (semantics in the docstring
# above). The count difference minus one is the post-prefix separator count
# with the joining dot excluded, matching rclcpp's `substr(prefix.length() + 1)`.
function _filter_names(names, prefixes, depth)
    if isempty(prefixes)
        depth == 0 && return names
        return filter(n -> count('.', String(n)) < depth, names)
    end
    matches(str, p) = startswith(str, p) &&
        (depth == 0 || str == p || count('.', str) - count('.', p) - 1 < depth)
    filter(n -> any(p -> matches(String(n), String(p)), prefixes), names)
end

"""
    set_parameters_atomically(server::ParameterServer, pairs) -> (successful::Bool, reason::String)
    set_parameters_atomically(server::CompositeParameterServer, pairs) -> (successful::Bool, reason::String)
    set_parameters_atomically(client::ParameterClient, params; timeout_ms=2000) -> (successful::Bool, reason::String)

The ROS 2 `SetParametersAtomically` service: apply all pairs as one transaction
(§10). Either every pair commits and the result is `(true, "")`, or the first
rejection aborts the whole set and the result is `(false, reason)`. A
constraint, read-only, or `validate` rejection (and an `ArgumentError`, e.g. an
undeclared name) maps to the `(false, reason)` result; a value that fails to
coerce to its declared field type throws instead, which a wire caller sees as
[`ServiceError`](@ref) rather than a `SetParametersResult`.

The [`ParameterServer`](@ref) form commits through one [`transaction`](@ref).
The [`CompositeParameterServer`](@ref) form groups pairs by member, validates
every member's candidate first, commits only if all pass, and surfaces the set
as one combined `/parameter_events`; member servers lock independently, so a
concurrent mutation racing the two phases can still leave earlier members
committed while the call reports `(false, reason)`. The
[`ParameterClient`](@ref) form is the remote dual; values are native Julia (the
wire tag is inferred) and `params` may be a vector of `name => value`, a
NamedTuple, or a Dict. Transport failure raises [`ServiceError`](@ref).
"""
function set_parameters_atomically(s::ParameterServer{P}, pairs) where {P}
    try
        transaction(s) do p
            for (name, value) in pairs
                setproperty!(p, Symbol(name), value)
            end
        end
        return (true, "")
    catch err
        err isa ParameterRejection && return (false, err.reason)
        err isa ArgumentError && return (false, sprint(showerror, err))
        rethrow()
    end
end

"""
    set_parameters(server::ParameterServer, pairs) -> Vector{Tuple{Bool,String}}
    set_parameters(server::CompositeParameterServer, pairs) -> Vector{Tuple{Bool,String}}
    set_parameters(client::ParameterClient, params; timeout_ms=2000) -> Vector{Tuple{Bool,String}}

The ROS 2 `SetParameters` service: apply each `name => value` pair as its own
independent transaction, yielding one `(successful, reason)`
`SetParametersResult` per pair (§10). A pair that violates a constraint, hits
the read-only gate, or fails `validate` fails on its own while the others may
succeed; a value that fails to coerce to its declared field type throws instead
of producing a per-pair result (a wire caller sees [`ServiceError`](@ref), with
earlier pairs already committed). To apply all pairs all-or-nothing, use
[`set_parameters_atomically`](@ref). The [`ParameterServer`](@ref) form applies
locally; the [`ParameterClient`](@ref) form is the remote dual over the wire.
"""
function set_parameters(s::ParameterServer{P}, pairs) where {P}
    map(pairs) do (name, value)
        set_parameters_atomically(s, ((name, value),))
    end
end

# ── wire types (§10) ──────────────────────────────────────────────────────────
# The `rcl_interfaces` generated structs the parameter services marshal over.
# Vendored under `Interfaces`; aliased here for readability. The generated keyword
# constructors require every field (no defaults), so each build below is exhaustive.

const _RCL_MSG = Interfaces.rcl_interfaces.msg
const _RCL_SRV = Interfaces.rcl_interfaces.srv
const _ParameterValue       = _RCL_MSG.ParameterValue
const _Parameter            = _RCL_MSG.Parameter
const _ParameterEvent       = _RCL_MSG.ParameterEvent
const _SetParametersResult  = _RCL_MSG.SetParametersResult
const _ListParametersResult = _RCL_MSG.ListParametersResult
const _WireDescriptor       = _RCL_MSG.ParameterDescriptor
const _Time                 = Interfaces.builtin_interfaces.msg.Time

# ── value marshalling (§10) ─────────────────────────────────────────────────────
# A Julia parameter value ⇄ the `ParameterValue` tagged union. The `type` byte
# selects the live arm; because the union is flat on the wire, every field is
# present, so the other ten carry zeroed placeholders.

# A `ParameterValue` with `type=tag` and one arm filled; the rest are zero/empty.
_param_value(tag::ParameterType; bool=false, int=Int64(0), dbl=0.0, str="",
             bytes=UInt8[], bools=Bool[], ints=Int64[], dbls=Float64[], strs=String[]) =
    _ParameterValue(; type = UInt8(tag), bool_value = bool, integer_value = int,
        double_value = dbl, string_value = str, byte_array_value = bytes,
        bool_array_value = bools, integer_array_value = ints,
        double_array_value = dbls, string_array_value = strs)

# A Julia value → its `ParameterValue`. `Symbol` marshals as STRING. `nothing` (an
# unset/unknown param) is NOT_SET. `Bool` must be matched ahead of `Integer`,
# since `Bool <: Integer`.
function _to_param_value(x)
    x === nothing                      && return _param_value(PARAMETER_NOT_SET)
    x isa Bool                         && return _param_value(PARAMETER_BOOL;          bool = x)
    x isa Integer                      && return _param_value(PARAMETER_INTEGER;       int  = Int64(x))
    x isa AbstractFloat                && return _param_value(PARAMETER_DOUBLE;         dbl  = Float64(x))
    x isa Symbol                       && return _param_value(PARAMETER_STRING;         str  = String(x))
    x isa AbstractString               && return _param_value(PARAMETER_STRING;         str  = String(x))
    x isa AbstractVector{UInt8}        && return _param_value(PARAMETER_BYTE_ARRAY;     bytes = collect(UInt8, x))
    x isa AbstractVector{Bool}         && return _param_value(PARAMETER_BOOL_ARRAY;     bools = collect(Bool, x))
    x isa AbstractVector{<:Integer}    && return _param_value(PARAMETER_INTEGER_ARRAY;  ints  = collect(Int64, x))
    x isa AbstractVector{<:AbstractFloat}  && return _param_value(PARAMETER_DOUBLE_ARRAY; dbls = collect(Float64, x))
    x isa AbstractVector{<:AbstractString} && return _param_value(PARAMETER_STRING_ARRAY; strs = collect(String, x))
    throw(ArgumentError("not a legal parameter value: $(typeof(x))"))
end

# A `ParameterValue` → its Julia value, dispatched on the `type` tag. NOT_SET (and
# any unknown tag) reads back as `nothing`, the unset-parameter sentinel.
function _from_param_value(pv)
    t = _ptype(pv.type)
    t === PARAMETER_BOOL          && return pv.bool_value
    t === PARAMETER_INTEGER       && return pv.integer_value
    t === PARAMETER_DOUBLE        && return pv.double_value
    t === PARAMETER_STRING        && return pv.string_value
    t === PARAMETER_BYTE_ARRAY    && return pv.byte_array_value
    t === PARAMETER_BOOL_ARRAY    && return pv.bool_array_value
    t === PARAMETER_INTEGER_ARRAY && return pv.integer_array_value
    t === PARAMETER_DOUBLE_ARRAY  && return pv.double_array_value
    t === PARAMETER_STRING_ARRAY  && return pv.string_array_value
    return nothing
end

# An internal `ParameterDescriptor` → the wire `rcl_interfaces/msg/ParameterDescriptor`.
# The constraint forms (numeric range / choice set) don't map onto ROS2's
# `FloatingPointRange`/`IntegerRange` arms cleanly (those carry a step and a single
# value type), so we leave the range sequences empty and surface the human form via
# `additional_constraints`. `dynamic_typing` is true only for a synthesized dynamic
# descriptor (no fixed declared type).
function _to_descriptor(d::ParameterDescriptor)
    constraints = d.constraint === nothing ? "" : string("∈ ", d.constraint)
    return _WireDescriptor(; name = String(d.name), type = UInt8(d.ptype),
        description = d.description, additional_constraints = constraints,
        read_only = d.read_only, dynamic_typing = (d.type === Nothing),
        floating_point_range = _RCL_MSG.FloatingPointRange[],
        integer_range = _RCL_MSG.IntegerRange[])
end

# One handler `(successful, reason)` tuple → its wire `SetParametersResult`.
_to_set_result((ok, reason)) = _SetParametersResult(; successful = ok, reason = reason)

"""
    wire_parameter_services!(server) -> server

Declare the six standard parameter services + the `/parameter_events` publisher on
the server's node (§10/§13), each bound to the reflection handlers above. Generic
over `P` — one implementation serves every schema. The services are node-private
(`~/…`); `/parameter_events` is absolute (every node publishes the same topic).
Service handles are tracked on the node (closed with it) and held on the server.

Generic over any [`AbstractParameterServer`](@ref): both `ParameterServer{P}` and
the multi-schema [`CompositeParameterServer`](@ref) (§4.4) supply the six reflection
handlers + `parameter_names`, so one wiring serves a plain node and a composed
(member-prefixed) one alike.
"""
function wire_parameter_services!(s::AbstractParameterServer)
    node = s.node

    push!(s.services, Service(node, "~/describe_parameters", _RCL_SRV.DescribeParameters_Request) do req
        descs = describe_parameters(s, req.names)
        return _RCL_SRV.DescribeParameters_Response(;
            descriptors = _WireDescriptor[_to_descriptor(d) for d in descs])
    end)

    push!(s.services, Service(node, "~/get_parameter_types", _RCL_SRV.GetParameterTypes_Request) do req
        return _RCL_SRV.GetParameterTypes_Response(;
            types = UInt8[UInt8(t) for t in get_parameter_types(s, req.names)])
    end)

    push!(s.services, Service(node, "~/get_parameters", _RCL_SRV.GetParameters_Request) do req
        return _RCL_SRV.GetParameters_Response(;
            values = _ParameterValue[_to_param_value(v) for v in get_parameters(s, req.names)])
    end)

    push!(s.services, Service(node, "~/list_parameters", _RCL_SRV.ListParameters_Request) do req
        names = list_parameters(s; prefixes = req.prefixes, depth = req.depth)
        return _RCL_SRV.ListParameters_Response(;
            result = _ListParametersResult(;
                names = String[String(n) for n in names], prefixes = String[]))
    end)

    push!(s.services, Service(node, "~/set_parameters", _RCL_SRV.SetParameters_Request) do req
        pairs = [(Symbol(p.name), _from_param_value(p.value)) for p in req.parameters]
        results = set_parameters(s, pairs)
        return _RCL_SRV.SetParameters_Response(;
            results = _SetParametersResult[_to_set_result(r) for r in results])
    end)

    push!(s.services, Service(node, "~/set_parameters_atomically", _RCL_SRV.SetParametersAtomically_Request) do req
        pairs = [(Symbol(p.name), _from_param_value(p.value)) for p in req.parameters]
        ok, reason = set_parameters_atomically(s, pairs)
        return _RCL_SRV.SetParametersAtomically_Response(;
            result = _SetParametersResult(; successful = ok, reason = reason))
    end)

    s._events_pub = Publisher(node, "/parameter_events", _ParameterEvent)
    return s
end

# Assemble `rcl_interfaces/msg/ParameterEvent` from the post-commit batch and
# publish it on `/parameter_events`. A name present in `previous` is a change; a
# fresh dynamic param (absent from `previous`) is new. The transaction path does
# not model deletions, so `deleted_parameters` stays empty.
function _publish_parameter_event(s::ParameterServer, batch::ParameterEventBatch)
    s._events_pub === nothing && return nothing
    node = s.node

    new_params     = _Parameter[]
    changed_params = _Parameter[]
    for (name, value) in batch.changed
        p = _Parameter(; name = String(name), value = _to_param_value(value))
        push!(haskey(batch.previous, name) ? changed_params : new_params, p)
    end

    sec, nanosec = _sec_nanosec(batch.stamp_ns)
    ev = _ParameterEvent(; stamp = _Time(; sec, nanosec),
        node = node === nothing ? "" : String(node.fqn),
        new_parameters = new_params, changed_parameters = changed_params,
        deleted_parameters = _Parameter[])
    publish(s._events_pub, ev)
    return nothing
end

# ── node integration (§10) ──────────────────────────────────────────────────────
# `Node(ctx, name, P)` bakes a `@parameters` schema `P` into the node: build the
# base node, attach a `ParameterServer{P}` (reached as `node.parameters`), and wire
# the six standard services + `/parameter_events` so the node is driveable by any
# ROS2 parameter client (rclcpp/rclpy/hiroz) — and by our own `ParameterClient`.

"""
    Node(ctx, name, ::Type{P}; overrides=(;), allow_undeclared=false, kwargs...) -> Node

Construct a node with a declared parameter schema `P` (a [`@parameters`](@ref)
struct), §10. Equivalent to `Node(ctx, name; kwargs...)` plus a `ParameterServer{P}`
attached at `node.parameters`, with the six standard parameter services and
`/parameter_events` wired (`wire_parameter_services!`). `overrides` overlays
startup values (CLI/launch/YAML) onto the schema defaults; `allow_undeclared` opens
the dynamic tier. Remaining `kwargs` forward to the base [`Node`](@ref) constructor.
"""
function Node(ctx::Context, name::AbstractString, ::Type{P};
              overrides::NamedTuple=(;), allow_undeclared::Bool=false,
              kwargs...) where {P}
    node = Node(ctx, name; kwargs...)
    server = ParameterServer{P}(node; overrides = overrides, allow_undeclared = allow_undeclared)
    wire_parameter_services!(server)
    node.parameters = server
    # Startup overrides commit through the ctor, NOT a transaction, so the `use_sim_time`
    # event hook never fires for them (§7). Activate sim explicitly if the committed value
    # is true, so a node started with `use_sim_time=true` follows `/clock` from the start.
    if hasfield(P, :use_sim_time) && getfield((@atomic server.value), :use_sim_time) === true
        set_use_sim_time!(ctx, node, true)
    end
    return node
end

dynamic_parameters(node::Node) = dynamic_parameters(node.parameters)
