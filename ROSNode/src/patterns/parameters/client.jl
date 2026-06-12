# ── the remote parameter client (§10/§12) ───────────────────────────────────────
# The async, fallible dual of the local `node.parameters` accessor: §12.1 frames a
# remote's parameters as "intrinsically asymmetric — a local accessor for us vs. an
# async, fallible client for a remote." `ParameterClient` is that client, driving a
# remote node's six standard parameter services + `/parameter_events` over the §8
# Service layer (same wire contract as rclcpp/rclpy/hiroz, so it talks to any of
# them). It follows the `fetch_type_description` template (introspection.jl): a
# lazily-built `ServiceClient` per service, `wait_for_service`, `call`, decode.
#
# Three layers over one set of verbs (all dispatching local-vs-remote on the first
# arg, so a `ParameterServer` and a `ParameterClient` read identically):
#   L0  the six service generics — `get_parameters(c, names)`, … — dynamic, fallible.
#   L1  `c[:field]` / `c[:field] = v` — one round-trip each (indexing, not property
#       access, to signal a remote lookup that can cost/fail — cf. `server.field`).
#   L2  `fetch(c)::P` materializes the whole remote into the shared `@parameters`
#       struct, and `transaction(c) do p … end` drafts + pushes the diff atomically,
#       reusing the server's `setproperties`/`_validate_candidate`/`ParameterRejection`
#       so the mutation API is uniform local↔remote. The schema type `P` is the
#       structure; a schemaless `ParameterClient{Nothing}` degrades to L0 + a dynamic
#       `NamedTuple` snapshot for talking to a node whose schema we don't share.

"""
    ParameterClient(node, target) -> ParameterClient{Nothing}
    ParameterClient(node, target, ::Type{P}) -> ParameterClient{P}

A client for the parameters of the remote node at FQN `target` (e.g.
`"/planner"`), §10. The dual of the local `node.parameters` server: it drives the
remote's six standard parameter services and `/parameter_events` over the wire.

See the ROS 2 parameter concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html

Passing a [`@parameters`](@ref) schema `P` (the same struct the remote baked in via
`Node(ctx, name, P)`) makes it a typed lens — `fetch(client)` returns a `P` and
`transaction(client) do p … end` mutates it type-stably. Without a schema it is
dynamic (returns `NamedTuple`/`Any`), for talking to an arbitrary ROS2 node.

The underlying [`ServiceClient`](@ref)s are built lazily per service and reaped by
`close(client)`. Calls are fallible: a timeout or error reply raises
[`ServiceError`](@ref) (and a rejected set raises [`ParameterRejection`](@ref)).
Call [`wait_for_service`](@ref) first so the route is matched before the opening request.
"""
mutable struct ParameterClient{P}
    const node::Any
    const target::String                       # remote node FQN (resolved)
    const clients::Dict{Symbol, Any}           # lazily-built ServiceClient per service base name
    const subs::Vector{Any}                    # /parameter_events subscriptions (events)
    const lock::ReentrantLock
end

function ParameterClient(node, target::AbstractString, ::Type{P} = Nothing) where {P}
    return ParameterClient{P}(node, resolve_name(node, target; kind = :node),
                              Dict{Symbol, Any}(), Any[], ReentrantLock())
end

Base.isopen(c::ParameterClient) = isopen(c.node)

Base.show(io::IO, c::ParameterClient{P}) where {P} =
    print(io, "ParameterClient", P === Nothing ? "" : string("{", nameof(P), "}"),
          "(", c.target, isopen(c) ? "" : ", closed", ")")

# The `*_Request` type for a service base name — the `Srv` a `ServiceClient` keys on
# (its `_Response` sibling is resolved automatically).
function _param_req_type(sym::Symbol)
    sym === :get_parameters            && return _RCL_SRV.GetParameters_Request
    sym === :get_parameter_types       && return _RCL_SRV.GetParameterTypes_Request
    sym === :set_parameters            && return _RCL_SRV.SetParameters_Request
    sym === :set_parameters_atomically && return _RCL_SRV.SetParametersAtomically_Request
    sym === :list_parameters           && return _RCL_SRV.ListParameters_Request
    sym === :describe_parameters       && return _RCL_SRV.DescribeParameters_Request
    throw(ArgumentError("unknown parameter service $(repr(sym))"))
end

# Lazily build and cache the `ServiceClient` for `<target>/<sym>`: the remote's
# node-private `~/<sym>` service resolved to its absolute name.
function _svc(c::ParameterClient, sym::Symbol)
    @lock c.lock begin
        haskey(c.clients, sym) && return c.clients[sym]
        base = endswith(c.target, "/") ? c.target : c.target * "/"
        sc = ServiceClient(c.node, base * String(sym), _param_req_type(sym))
        c.clients[sym] = sc
        return sc
    end
end

"""
    close(client::ParameterClient)

Reap every lazily-built service client and event subscription (§6). Idempotent.
"""
function Base.close(c::ParameterClient)
    @lock c.lock begin
        for sc in values(c.clients)
            try; close(sc); catch err; @debug "close(ParameterClient): service client" exception = err; end
        end
        empty!(c.clients)
        for s in c.subs
            try; close(s); catch err; @debug "close(ParameterClient): event sub" exception = err; end
        end
        empty!(c.subs)
    end
    return nothing
end

"""
    wait_for_service(client::ParameterClient; timeout=nothing) -> Bool

Block until the remote's parameter services are routing-matched (§12.1), waiting on
`get_parameters` as the representative service. Returns `false` on timeout — the
same routing-plane wait as the [`ServiceClient`](@ref) form.
"""
wait_for_service(c::ParameterClient; timeout::Union{Real, Nothing} = nothing) =
    wait_for_service(_svc(c, :get_parameters); timeout = timeout)

# Normalize the accepted set forms to a list of `name => value` pairs: a NamedTuple
# (`(max_speed=80,)`), a Dict, or any iterable of `Pair`/2-tuples.
_param_pairs(x::NamedTuple) = [k => v for (k, v) in pairs(x)]
_param_pairs(x::AbstractDict) = collect(pairs(x))
_param_pairs(x) = collect(x)

_to_wire_params(ps) =
    _Parameter[_Parameter(name = String(first(p)), value = _to_param_value(last(p))) for p in ps]

# ── L0: the six service generics, remote arm (§10) ───────────────────────────────
# Same names + return types as the server methods (services.jl); these add the wire
# call + `timeout_ms` and decode the reply back to the identical Julia types.

"""
    get_parameters(client, names; timeout_ms=2000) -> Vector{Any}

Remote `GetParameters` (§10): the current value of each name (native Julia values;
an unset/unknown name reads back as `nothing`). Mirrors the local server method.
"""
function get_parameters(c::ParameterClient, names; timeout_ms::Integer = 2000)
    req = _RCL_SRV.GetParameters_Request(names = String[String(n) for n in names])
    resp = call(_svc(c, :get_parameters), req; timeout_ms = timeout_ms)
    return Any[_from_param_value(v) for v in resp.values]
end

"""
    get_parameter_types(client, names; timeout_ms=2000) -> Vector{ParameterType}

Remote `GetParameterTypes` (§10): the [`ParameterType`](@ref) of each name
(`PARAMETER_NOT_SET` for an unknown).
"""
function get_parameter_types(c::ParameterClient, names; timeout_ms::Integer = 2000)
    req = _RCL_SRV.GetParameterTypes_Request(names = String[String(n) for n in names])
    resp = call(_svc(c, :get_parameter_types), req; timeout_ms = timeout_ms)
    return ParameterType[_ptype(t) for t in resp.types]
end

"""
    set_parameters_atomically(client, params; timeout_ms=2000) -> (successful, reason)

Remote `SetParametersAtomically` (§10): apply all of `params` as one transaction,
returning the `SetParametersResult` as a `(successful::Bool, reason::String)` tuple.
`params` is any `_param_pairs`-able form — a vector of `name => value`, a
NamedTuple, or a Dict; values are native Julia (the wire tag is inferred).
"""
function set_parameters_atomically(c::ParameterClient, params; timeout_ms::Integer = 2000)
    req = _RCL_SRV.SetParametersAtomically_Request(parameters = _to_wire_params(_param_pairs(params)))
    resp = call(_svc(c, :set_parameters_atomically), req; timeout_ms = timeout_ms)
    return (resp.result.successful, resp.result.reason)
end

"""
    set_parameters(client, params; timeout_ms=2000) -> Vector{Tuple{Bool,String}}

Remote `SetParameters` (§10): each pair is its own transaction, so each gets an
independent `(successful, reason)` result. Use `set_parameters_atomically` to apply
all pairs as a single all-or-nothing transaction.
"""
function set_parameters(c::ParameterClient, params; timeout_ms::Integer = 2000)
    req = _RCL_SRV.SetParameters_Request(parameters = _to_wire_params(_param_pairs(params)))
    resp = call(_svc(c, :set_parameters), req; timeout_ms = timeout_ms)
    return Tuple{Bool, String}[(r.successful, r.reason) for r in resp.results]
end

"""
    list_parameters(client; prefixes=String[], depth=0, timeout_ms=2000) -> Vector{Symbol}

Remote `ListParameters` (§10): the parameter names, optionally prefix-filtered
(`depth=0` is `DEPTH_RECURSIVE`).
"""
function list_parameters(c::ParameterClient; prefixes = String[], depth::Integer = 0,
                         timeout_ms::Integer = 2000)
    req = _RCL_SRV.ListParameters_Request(prefixes = String[String(p) for p in prefixes],
                                          depth = UInt64(depth))
    resp = call(_svc(c, :list_parameters), req; timeout_ms = timeout_ms)
    return Symbol[Symbol(n) for n in resp.result.names]
end

"""
    describe_parameters(client, names; timeout_ms=2000) -> Vector{ParameterDescriptor}

Remote `DescribeParameters` (§10): a [`ParameterDescriptor`](@ref) per name. The
wire form carries name/type/description/read-only faithfully. The wire encodes the
numeric-range and choice-set `constraint` only as the human `additional_constraints`
string, so the structured `constraint` field reads back as `nothing`.
"""
function describe_parameters(c::ParameterClient, names; timeout_ms::Integer = 2000)
    req = _RCL_SRV.DescribeParameters_Request(names = String[String(n) for n in names])
    resp = call(_svc(c, :describe_parameters), req; timeout_ms = timeout_ms)
    return ParameterDescriptor[_from_wire_descriptor(w) for w in resp.descriptors]
end

function _from_wire_descriptor(w)
    pt = _ptype(w.type)
    return ParameterDescriptor(Symbol(w.name), _param_julia_type(pt), pt,
                               String(w.description), nothing, w.read_only, nothing)
end

# ── singular conveniences + L1 indexing (§10) ────────────────────────────────────
# `parameter`/`set_parameter!`/`parameter_names` are the same exported verbs the
# server uses; `c[:name]` / `c[:name] = v` are the indexing sugar (an explicit
# "remote lookup", distinct from the server's cheap `server.name` field read).

"""
    parameter(client, name; timeout_ms=2000)

Read one remote parameter by `name` (§10) — `get_parameters` for a single name.
"""
parameter(c::ParameterClient, name; timeout_ms::Integer = 2000) =
    get_parameters(c, (name,); timeout_ms = timeout_ms)[1]

"""
    set_parameter!(client, name, value; timeout_ms=2000) -> value

Set one remote parameter (§10), atomically. Raises [`ParameterRejection`](@ref) if
the remote rejects it (constraint / read-only / `validate`) — the same exception the
local `set_parameter!` raises, so the failure contract is uniform local↔remote.
"""
function set_parameter!(c::ParameterClient, name, value; timeout_ms::Integer = 2000)
    ok, reason = set_parameters_atomically(c, (Symbol(name) => value,); timeout_ms = timeout_ms)
    ok || throw(ParameterRejection(reason))
    return value
end

"The remote parameter names (§10) — `list_parameters`."
parameter_names(c::ParameterClient; kwargs...) = list_parameters(c; kwargs...)

Base.getindex(c::ParameterClient, name::Union{Symbol, AbstractString}) = parameter(c, name)
Base.setindex!(c::ParameterClient, value, name::Union{Symbol, AbstractString}) =
    set_parameter!(c, name, value)

# ── L2: typed snapshot + transaction (§10) ───────────────────────────────────────

"""
    fetch(client::ParameterClient{P}; timeout_ms=2000) -> P

Materialize the whole remote into one value (§10): for a typed client a type-stable
`P` (one round-trip, then read fields locally); for a schemaless client a
`NamedTuple` of every remote parameter. An unset field falls back to the schema
default.
"""
function Base.fetch(c::ParameterClient{P}; timeout_ms::Integer = 2000) where {P}
    P === Nothing && return _fetch_dynamic(c; timeout_ms = timeout_ms)
    fns  = collect(fieldnames(P))
    vals = get_parameters(c, fns; timeout_ms = timeout_ms)
    kw = Dict{Symbol, Any}()
    for (f, v) in zip(fns, vals)
        v === nothing || (kw[f] = v)          # NOT_SET → let the schema default fill in
    end
    return P(; kw...)
end

function _fetch_dynamic(c::ParameterClient; timeout_ms::Integer = 2000)
    names = list_parameters(c; timeout_ms = timeout_ms)
    vals  = get_parameters(c, names; timeout_ms = timeout_ms)
    return NamedTuple{Tuple(names)}(Tuple(vals))
end

# The client-side analog of the server's `Draft` (server.jl) — a mutable overlay
# over a fetched base, but with no server/dynamic tier: only declared fields, each
# coerced to its field type on assignment (fail-fast before the wire).
mutable struct _ClientDraft{P}
    const base::P
    const overrides::Dict{Symbol, Any}
end

function Base.getproperty(d::_ClientDraft{P}, name::Symbol) where {P}
    name in (:base, :overrides) && return getfield(d, name)
    ov = getfield(d, :overrides)
    haskey(ov, name) && return ov[name]
    name in fieldnames(P) && return getfield(getfield(d, :base), name)
    throw(ArgumentError("no parameter $(repr(name)) in $(P)"))
end

function Base.setproperty!(d::_ClientDraft{P}, name::Symbol, value) where {P}
    name in fieldnames(P) ||
        throw(ArgumentError("no parameter $(repr(name)) in $(P)"))
    getfield(d, :overrides)[name] = _coerce_param(fieldtype(P, name), value)
    return value
end

Base.propertynames(d::_ClientDraft{P}) where {P} = fieldnames(P)

_descriptor_map(::Type{P}) where {P} =
    Dict{Symbol, ParameterDescriptor}(d.name => d for d in descriptors(P))

"""
    transaction(client::ParameterClient{P}) do p … end -> P

Remote mutation with the same do-block shape as the server's [`transaction`](@ref)
(§10): runs `f` against a draft of the fetched current value, then pushes the diff
as one `set_parameters_atomically`. The client validates the candidate locally first
(per-field constraints + read-only gate + user `validate`) for fast feedback, then
the remote re-validates authoritatively. Returns the new `P` on success,
or raises [`ParameterRejection`](@ref) on a rejected set — uniform with the local
path. Requires a typed client (schemaless remotes use `set_parameters_atomically`).
"""
function transaction(f, c::ParameterClient{P}; timeout_ms::Integer = 2000) where {P}
    P === Nothing &&
        throw(ArgumentError("transaction requires a typed ParameterClient{P}; for a schemaless remote use set_parameters_atomically"))
    base = fetch(c; timeout_ms = timeout_ms)::P
    draft = _ClientDraft{P}(base, Dict{Symbol, Any}())
    f(draft)                                              # may throw → abort, nothing pushed
    isempty(draft.overrides) && return base
    candidate = setproperties(base, NamedTuple(draft.overrides))
    _validate_candidate(_descriptor_map(P), base, candidate, keys(draft.overrides))
    ok, reason = set_parameters_atomically(c, collect(pairs(draft.overrides)); timeout_ms = timeout_ms)
    ok || throw(ParameterRejection(reason))
    return candidate
end

# ── remote events (§10) ──────────────────────────────────────────────────────────

"""
    on_parameter_event(f, client::ParameterClient) -> f

Watch the remote's parameter changes (§10): subscribe `/parameter_events`, filter to
this client's `target`, and call `f(batch::ParameterEventBatch)` per event. The
remote analog of the server-side `on_parameter_event`. For a typed client the changed
values are coerced to their field types. `batch.previous` is empty: the wire event
carries only current values. The subscription is reaped by `close(client)`.
"""
function on_parameter_event(f, c::ParameterClient{P}) where {P}
    sub = Subscription(c.node, "/parameter_events", _ParameterEvent) do ev
        String(ev.node) == c.target || return nothing
        changed = Dict{Symbol, Any}()
        for p in Iterators.flatten((ev.new_parameters, ev.changed_parameters))
            sym = Symbol(p.name)
            v = _from_param_value(p.value)
            if P !== Nothing && v !== nothing && sym in fieldnames(P)
                v = _coerce_param(fieldtype(P, sym), v)
            end
            changed[sym] = v
        end
        stamp = Int64(ev.stamp.sec) * 1_000_000_000 + Int64(ev.stamp.nanosec)
        f(ParameterEventBatch(changed, Dict{Symbol, Any}(), stamp))
        return nothing
    end
    push!(c.subs, sub)
    return f
end
