# ── the six standard parameter services + /parameter_events (§10) ───────────────
# All generic over `P` via reflection (`fieldnames`/`fieldtypes` + `descriptors`),
# reflecting over the union of declared fields + dynamic dict so a ROS client sees
# one flat namespace. The handlers below are the schema-independent core; the wire
# binding (a `Service(node, …)` per service + the `/parameter_events` publisher)
# is the §8 Service-layer's job and is staged behind a precompile-safe stub.

"The six standard parameter service base names (§10), under the node's private namespace."
const PARAMETER_SERVICE_NAMES = (
    "describe_parameters",
    "get_parameter_types",
    "get_parameters",
    "list_parameters",
    "set_parameters",
    "set_parameters_atomically",
)

# describe: name → its ParameterDescriptor (declared) or a synthesized one for a
# live dynamic param. The reflection the `describe_parameters` service replies with.
function describe_parameters(s::ParameterServer{P}, names) where {P}
    out = ParameterDescriptor[]
    for name in names
        sym = Symbol(name)
        if haskey(s.by_name, sym)
            push!(out, s.by_name[sym])
        elseif s.allow_undeclared && haskey(s.dynamic, sym)
            v = s.dynamic[sym]
            push!(out, ParameterDescriptor(sym, typeof(v), parameter_type(typeof(v)),
                                           "", nothing, false, v))
        else
            # ROS2 returns a NOT_SET descriptor for an unknown name.
            push!(out, ParameterDescriptor(sym, Nothing, PARAMETER_NOT_SET, "", nothing, false, nothing))
        end
    end
    return out
end

# get_types: name → ParameterType tag (NOT_SET for unknowns). Flat over both tiers.
function get_parameter_types(s::ParameterServer{P}, names) where {P}
    map(names) do name
        sym = Symbol(name)
        if sym in fieldnames(P)
            parameter_type(fieldtype(P, sym))
        elseif s.allow_undeclared && haskey(s.dynamic, sym)
            parameter_type(typeof(s.dynamic[sym]))
        else
            PARAMETER_NOT_SET
        end
    end
end

# get: name → current value (or `nothing` for an unknown — the service maps it to
# a NOT_SET `ParameterValue`).
function get_parameters(s::ParameterServer{P}, names) where {P}
    map(names) do name
        sym = Symbol(name)
        if sym in fieldnames(P)
            getfield(@atomic(s.value), sym)
        elseif s.allow_undeclared && haskey(s.dynamic, sym)
            s.dynamic[sym]
        else
            nothing
        end
    end
end

# list: the flat union of names, optionally prefix-filtered. (`depth`/separator
# semantics of `ListParameters` are a TODO until the wire request type lands.)
function list_parameters(s::ParameterServer; prefixes=())
    names = parameter_names(s)
    isempty(prefixes) && return names
    filter(n -> any(p -> startswith(String(n), String(p)), prefixes), names)
end

# set (atomic): apply `name => value` pairs as one transaction. Returns
# `(successful, reason)` — the `SetParametersResult` shape. A rejection is caught
# into `successful=false` (the external-client asymmetry, §10), never re-raised.
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

# set (per-item): each pair its own transaction → one `SetParametersResult` each,
# independent success/failure (ROS2's `SetParameters`, vs the atomic variant).
function set_parameters(s::ParameterServer{P}, pairs) where {P}
    map(pairs) do (name, value)
        set_parameters_atomically(s, ((name, value),))
    end
end

# ── wire types (§10) ──────────────────────────────────────────────────────────
# The `rcl_interfaces` generated structs the parameter services marshal over.
# Vendored under `Interfaces`; aliased here for readability. Keyword constructors
# require *every* field (the generator emits no defaults), so each build below is
# exhaustive.

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
# selects the live arm; the other ten fields carry zeroed placeholders (the union
# is flat on the wire, so every field is present — the kw ctor needs all of them).

# A `ParameterValue` with `type=tag` and one arm filled; the rest are zero/empty.
_param_value(tag::ParameterType; bool=false, int=Int64(0), dbl=0.0, str="",
             bytes=UInt8[], bools=Bool[], ints=Int64[], dbls=Float64[], strs=String[]) =
    _ParameterValue(; type = UInt8(tag), bool_value = bool, integer_value = int,
        double_value = dbl, string_value = str, byte_array_value = bytes,
        bool_array_value = bools, integer_array_value = ints,
        double_array_value = dbls, string_array_value = strs)

# A Julia value → its `ParameterValue`. `Symbol` is string-with-choices sugar, so
# it marshals as STRING. `nothing` (an unset/unknown param) is NOT_SET. `Bool` is
# matched before `Integer` (it `<: Integer`).
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
# any unknown tag) reads back as `nothing` — the unset-parameter sentinel `set`
# rejects upstream.
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
"""
function wire_parameter_services!(s::ParameterServer)
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
        names = list_parameters(s; prefixes = req.prefixes)
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
# publish it on `/parameter_events`. A declared field that *moved* is a change; a
# name absent from `previous` (a fresh dynamic param) is new. Deletions aren't
# modeled by the transaction path yet, so `deleted_parameters` stays empty.
function _publish_parameter_event(s::ParameterServer, batch::ParameterEventBatch)
    s._events_pub === nothing && return nothing
    node = s.node

    new_params     = _Parameter[]
    changed_params = _Parameter[]
    for (name, value) in batch.changed
        p = _Parameter(; name = String(name), value = _to_param_value(value))
        push!(haskey(batch.previous, name) ? changed_params : new_params, p)
    end

    ev = _ParameterEvent(; stamp = to_msg(_Time, Dates.now(node)),
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
`/parameter_events` wired ([`wire_parameter_services!`](@ref)). `overrides` overlays
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

"The undeclared/dynamic parameter dict of a node's parameter server (§10)."
dynamic_parameters(node::Node) = dynamic_parameters(node.parameters)
