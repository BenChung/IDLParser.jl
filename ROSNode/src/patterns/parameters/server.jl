# ── the parameter server ──────────────────────────────────────────────────
# Holds the live schema value behind an atomic field swapped whole on every commit,
# so a racing reader sees a complete old-or-new value, never a half-applied one.
# Alongside: the cached descriptors, the dynamic side dict and its `allow_undeclared`
# gate, the mutation lock, the on-change listeners, and a slot for the
# `/parameter_events` publisher the service wiring attaches. `node` is held loosely
# (`Any`) so this file stays independent of the Node type.

"""
    ParameterServer(node, initial::P; allow_undeclared=false) -> ParameterServer{P}
    ParameterServer{P}(node; overrides=(;), allow_undeclared=false) -> ParameterServer{P}

The parameter subsystem for one node, parameterized on a [`@parameters`](@ref)
schema `P` (ROS 2 node parameters). It owns the live schema value and
applies sets directly; [`ParameterClient`](@ref) is its remote dual, driving
another node's parameter services asynchronously and fallibly over the wire.

The live schema value sits behind an `@atomic` field swapped whole on every
commit, so a concurrent reader sees a complete old-or-new value and rollback is
free. Read a declared field with `server.field` (type-stable). Mutate through
[`transaction`](@ref), the single-statement sugar `server.field = v`,
[`setproperties!`](@ref), or [`set_parameter!`](@ref) — all route through one
commit path under the server's mutation lock.

Undeclared parameters live in a separate `Any`-typed side dict reached via
[`dynamic_parameters`](@ref), gated by `allow_undeclared` (default off); with
the gate off, a dynamic read or write raises `ArgumentError`. The two-argument
form takes an already-built `initial::P`; the `{P}`-keyword form builds the
initial value by overlaying `overrides` (startup CLI/launch/YAML) onto the
schema defaults. A startup override of a `read_only` field is allowed —
read-only blocks only runtime sets.

Typically reached as `node.parameters`. The six standard parameter services and
`/parameter_events` are wired generically over `P` by
`wire_parameter_services!` (done for you by `Node(ctx, name, P)`).

Implements the ROS 2 parameter model: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html
"""
mutable struct ParameterServer{P} <: AbstractParameterServer
    const node::Any                                  # the owning Node (loosely held)
    @atomic value::P                                 # live schema value
    const descriptors::Vector{ParameterDescriptor}
    const by_name::Dict{Symbol, ParameterDescriptor}
    const dynamic::Dict{Symbol, Any}                 # undeclared params (Any-typed)
    const allow_undeclared::Bool
    const lock::ReentrantLock                        # the single parameter-mutation lock
    const listeners::Vector{Any}                     # on_parameter_event callbacks
    _events_pub::Any                                 # /parameter_events publisher
    const services::Vector{Any}                      # the six wired parameter-service handles
end

function ParameterServer(node, initial::P; allow_undeclared::Bool=false) where {P}
    ds = descriptors(P)
    by = Dict{Symbol, ParameterDescriptor}(d.name => d for d in ds)
    return ParameterServer{P}(node, initial, ds, by, Dict{Symbol, Any}(),
                              allow_undeclared, ReentrantLock(), Any[], nothing, Any[])
end

ParameterServer{P}(node; overrides::NamedTuple=(;), allow_undeclared::Bool=false) where {P} =
    ParameterServer(node, P(; overrides...); allow_undeclared=allow_undeclared)

Base.show(io::IO, s::ParameterServer{P}) where {P} =
    print(io, "ParameterServer{", nameof(P), "}(",
          length(s.descriptors), " declared, ", length(s.dynamic), " dynamic)")

"The current (live) schema value — a complete, never half-applied, struct."
current(s::ParameterServer) = @atomic s.value

"The declared schema type `P` of this server."
schema_type(::ParameterServer{P}) where {P} = P

"""
    declared_names(server::ParameterServer{P}) -> NTuple{N,Symbol}

The declared parameter names of a [`ParameterServer`](@ref), in schema order —
`fieldnames(P)`. [`parameter_names`](@ref) adds the dynamic tier for the
full flat namespace.
"""
declared_names(s::ParameterServer{P}) where {P} = fieldnames(P)

"""
    dynamic_parameters(server::ParameterServer) -> Dict{Symbol,Any}
    dynamic_parameters(node::Node) -> Dict{Symbol,Any}

The undeclared (dynamic) parameter side dict of a node's parameter server
— the `Any`-typed tier for parameters outside the [`@parameters`](@ref) schema.
The dict is returned by reference, so a direct write
(`dynamic_parameters(node)[:gain] = 2.0`) adds or changes an undeclared
parameter immediately, but bypasses the mutation lock and publishes no
`/parameter_events` — listeners and remote [`ParameterClient`](@ref)s never
observe it. Route through [`set_parameter!`](@ref)/[`transaction`](@ref) when an
event is required. Requires `allow_undeclared=true`; with the gate off it raises
`ArgumentError`. A [`CompositeParameterServer`](@ref) has no node-level dynamic
tier and raises `ArgumentError` directing you to a member's own server.
"""
function dynamic_parameters(s::ParameterServer)
    s.allow_undeclared ||
        throw(ArgumentError("undeclared parameters disabled (allow_undeclared=false)"))
    return s.dynamic
end

# ── reads ──────────────────────────────────────────────────────────────────
# `server.field` derefs the live struct (type-stable for a declared field).
# Reserved struct members read through; any other name is a parameter, declared
# names taking priority over the dynamic dict.

const _SERVER_FIELDS = fieldnames(ParameterServer)

# Read-lock asymmetry: declared-field reads are lock-free atomic loads against the
# live struct, while a dynamic-tier read takes `s.lock` because `_commit!` mutates
# `s.dynamic` under it from other OS threads. `Some`-wrapped hit / `nothing` miss.
function _dynamic_find(s::ParameterServer, name::Symbol)
    @lock s.lock begin
        dyn = s.dynamic
        haskey(dyn, name) ? Some(dyn[name]) : nothing
    end
end

function Base.getproperty(s::ParameterServer{P}, name::Symbol) where {P}
    name in _SERVER_FIELDS && return getfield(s, name)
    if name in fieldnames(P)
        return getfield(@atomic(s.value), name)
    end
    if getfield(s, :allow_undeclared)
        v = _dynamic_find(s, name)
        v !== nothing && return something(v)
    end
    throw(ArgumentError("no parameter $(repr(name)) (declared: $(fieldnames(P)))"))
end

Base.propertynames(s::ParameterServer{P}) where {P} = fieldnames(P)

"""
    parameter(server::ParameterServer, name::Symbol)
    parameter(client::ParameterClient, name; timeout_ms=2000)

Read one parameter by `name` across both tiers. On a
[`ParameterServer`](@ref) a declared field reads from the live atomic struct and
a dynamic one from the side dict (when `allow_undeclared`); an undeclared name
with the gate off raises `ArgumentError`. On a [`ParameterClient`](@ref) it is
one remote `GetParameters` for the single name — an unset or unknown name reads
back as `nothing`, and a timeout or error reply raises [`ServiceError`](@ref).
This is the flat-namespace read the services and the composite resolver use.
"""
function parameter(s::ParameterServer{P}, name::Symbol) where {P}
    name in fieldnames(P) && return getfield(@atomic(s.value), name)
    if s.allow_undeclared
        v = _dynamic_find(s, name)
        v !== nothing && return something(v)
    end
    throw(ArgumentError("no parameter $(repr(name))"))
end

"""
    parameter_names(server::ParameterServer) -> Vector{Symbol}
    parameter_names(server::CompositeParameterServer) -> Vector{Symbol}
    parameter_names(client::ParameterClient; kwargs...) -> Vector{Symbol}

The flat list of parameter names a node exposes — what `list` and
`/parameter_events` reflect over. For a [`ParameterServer`](@ref) this is the
union of declared field names (first, in schema order) and the live dynamic
names (sorted, only when `allow_undeclared`). For a
[`CompositeParameterServer`](@ref) it is every member's declared names as
`<member>.<field>` in declared order. For a [`ParameterClient`](@ref) it
forwards to a remote `ListParameters` ([`list_parameters`](@ref)).
"""
function parameter_names(s::ParameterServer{P}) where {P}
    names = collect(Symbol, fieldnames(P))
    s.allow_undeclared && append!(names, sort!(@lock s.lock collect(keys(s.dynamic))))
    return names
end

# ── transactions: the mutation primitive ──────────────────────────────────
# A `Draft` is a mutable overlay over the live base: `p.field = v` records a
# pending override that later reads in the block observe, while the live value
# stays untouched until commit. On clean block exit we build the candidate via
# `setproperties`, validate the whole candidate (per-field descriptors + user
# `validate`), swap once, and fire one batched event. A throw aborts: the
# candidate is dropped and the live cell keeps its last committed value (free
# rollback).

mutable struct Draft{P}
    const server::ParameterServer{P}
    const base::P
    const overrides::Dict{Symbol, Any}   # declared-field overrides (pending)
    const dynamic::Dict{Symbol, Any}     # dynamic-field overrides (pending)
end

# Reads see pending overrides then the base / live dict; writes record into the
# right tier. Declared names always win; a dynamic name routes to the dict only
# when `allow_undeclared` is on.
function Base.getproperty(d::Draft{P}, name::Symbol) where {P}
    name in (:server, :base, :overrides, :dynamic) && return getfield(d, name)
    ov = getfield(d, :overrides)
    haskey(ov, name) && return ov[name]
    name in fieldnames(P) && return getfield(getfield(d, :base), name)
    srv = getfield(d, :server)
    dyn = getfield(d, :dynamic)
    haskey(dyn, name) && return dyn[name]
    if srv.allow_undeclared && haskey(srv.dynamic, name)
        return srv.dynamic[name]
    end
    throw(ArgumentError("no parameter $(repr(name))"))
end

function Base.setproperty!(d::Draft{P}, name::Symbol, value) where {P}
    if name in fieldnames(P)
        ftype = fieldtype(P, name)
        getfield(d, :overrides)[name] = _coerce_param(ftype, value)
    else
        srv = getfield(d, :server)
        srv.allow_undeclared ||
            throw(ArgumentError("undeclared parameter $(repr(name)); set allow_undeclared=true to use the dynamic dict"))
        getfield(d, :dynamic)[name] = value
    end
    return value
end

Base.propertynames(d::Draft{P}) where {P} = fieldnames(P)

"""
    transaction(f, server::ParameterServer{P}) -> P
    transaction(server::ParameterServer{P}) do p … end -> P
    transaction(f, client::ParameterClient{P}; timeout_ms=2000) -> P

The parameter mutation primitive. Runs `f(draft)` against a mutable draft
of the current parameters: assignments record pending overrides that reads
inside the block observe, while nothing live is touched and nothing is published
until commit.

For a [`ParameterServer`](@ref) the call holds the server's single mutation lock
for the block's whole duration, so transactions serialize (the consistency
boundary the foreign-thread dispatch relies on). On clean exit the commit
pipeline runs in order:

1. assemble the candidate via [`setproperties`](@ref);
2. validate it as a whole (the per-field read-only gate and constraints, then
   the user [`validate`](@ref) hook);
3. swap it in with one atomic whole-struct store;
4. publish one batched `/parameter_events`.

Any throw aborts with free rollback — the live cell never held a
partial state. Returns the committed schema value. A constraint, read-only, or
`validate` violation raises [`ParameterRejection`](@ref), which unwinds the lock
with nothing committed.

For a [`ParameterClient`](@ref) the same do-block shape drives a remote: `f`
runs against a draft of the fetched current value, the candidate is checked
locally first for fast feedback (field constraints, the read-only gate, and any
`validate` method defined in the client process — rules defined only on the
remote are enforced by the remote's authoritative re-validation), then pushed as
one `set_parameters_atomically`. Returns the new `P` on success or raises
[`ParameterRejection`](@ref) on a rejected set. A schemaless
`ParameterClient{Nothing}` raises `ArgumentError` — use
[`set_parameters_atomically`](@ref) instead.

```julia
transaction(node.parameters) do p
    p.max_speed = 9.0
    p.mode = :manual
end
```
"""
function transaction(f, s::ParameterServer{P}) where {P}
    @lock s.lock begin
        base = @atomic s.value
        draft = Draft{P}(s, base, Dict{Symbol, Any}(), Dict{Symbol, Any}())
        f(draft)                                     # may throw → abort, no commit
        return _commit!(s, base, draft.overrides, draft.dynamic)
    end
end

# Validate a candidate against the descriptors and the user `validate` hook, throwing
# `ParameterRejection` on the first violation. Per-field checks run only for fields
# whose value actually changed (`moved` lists the override names), so an idempotent
# set — a replace-all restating a read-only field at its current value — commits as a
# no-op. For a changed field the read-only gate runs first, then the constraint; the
# gate blocks runtime sets only, since a startup override goes through the ctor.
# Shared by the server commit and the client-side transaction pre-check, so both
# raise the same rejection.
function _validate_candidate(by_name::AbstractDict, base::P, candidate::P, moved) where {P}
    for name in moved
        d = get(by_name, name, nothing)
        d === nothing && continue
        isequal(getfield(candidate, name), getfield(base, name)) && continue
        d.read_only &&
            throw(ParameterRejection("parameter $(name) is read-only"))
        reason = _check_constraint(d, getfield(candidate, name))
        reason === nothing ||
            throw(ParameterRejection("parameter $(name): $(reason)"))
    end
    validate(candidate)            # cross-field rules over the whole candidate
    return candidate
end

# Build + validate the candidate, swap once, publish one event batch. Caller holds
# `s.lock`. Returns the committed value. Validation throws `ParameterRejection`
# (constraint / read-only / user `validate`) → the lock unwinds, nothing committed.
function _commit!(s::ParameterServer{P}, base::P,
                  overrides::AbstractDict, dyn_overrides::AbstractDict) where {P}
    isempty(overrides) && isempty(dyn_overrides) && return base   # nothing to do

    candidate = isempty(overrides) ? base :
                setproperties(base, NamedTuple(overrides))

    _validate_candidate(s.by_name, base, candidate, keys(overrides))

    # Atomic whole-struct swap — readers see old-or-new, never partial.
    @atomic s.value = candidate

    # Snapshot the dynamic tier's prior values *before* the commit overwrites them,
    # keyed by the override names — the event classifier needs them to tell an
    # updated existing dynamic param (changed) from a fresh one (new), and to drop
    # an idempotent re-set.
    dyn_previous = Dict{Symbol, Any}()
    for k in keys(dyn_overrides)
        haskey(s.dynamic, k) && (dyn_previous[k] = s.dynamic[k])
    end

    # Commit dynamic-tier overrides under the same lock (wire-atomic across tiers).
    for (k, v) in dyn_overrides
        s.dynamic[k] = v
    end

    _emit_parameter_event!(s, base, candidate, overrides, dyn_overrides, dyn_previous)
    return candidate
end

# ── single-statement sugar ────────────────────────────────────────────────
# `server.field = v` and `setproperties!(server, …)` are implicit single-statement
# transactions routed through the same commit path. A NamedTuple is a partial
# overlay; a full `P` value is replace-all (every field).

function Base.setproperty!(s::ParameterServer{P}, name::Symbol, value) where {P}
    name in _SERVER_FIELDS && return setfield!(s, name, value)
    transaction(s) do p
        setproperty!(p, name, value)
    end
    return value
end

"""
    setproperties!(server::ParameterServer{P}, overrides::NamedTuple) -> P
    setproperties!(server::ParameterServer{P}, full::P) -> P

Effectful multi-field set as one implicit transaction. A NamedTuple is a
partial overlay — only the named fields change. A full `P` value is replace-all
— every field is staged, and validation and the event batch consider only the
fields whose value actually changes, so restating a read-only field at its
current value commits as a no-op rather than a rejection. Commits atomically
through the same path as [`transaction`](@ref) and returns the committed value;
a rejection raises [`ParameterRejection`](@ref).
"""
function setproperties!(s::ParameterServer{P}, overrides::NamedTuple) where {P}
    transaction(s) do p
        for (k, v) in pairs(overrides)
            setproperty!(p, k, v)
        end
    end
end

function setproperties!(s::ParameterServer{P}, full::P) where {P}
    transaction(s) do p
        for f in fieldnames(P)
            setproperty!(p, f, getfield(full, f))
        end
    end
end

"""
    set_parameter!(server::ParameterServer, name::Symbol, value) -> P
    set_parameter!(client::ParameterClient, name, value; timeout_ms=2000) -> value

Set one parameter by name, atomically. On a [`ParameterServer`](@ref) a
declared field routes to the typed struct (value coerced to the field type) and
an undeclared name to the dynamic dict (when `allow_undeclared`); it runs as a
single-statement transaction and returns the whole committed schema struct `P`
(for a dynamic name the returned struct does not carry that key — only the
declared tier is part of `P`). On a [`ParameterClient`](@ref) it issues one
`set_parameters_atomically` and returns the passed `value` on success, raising
[`ParameterRejection`](@ref) if the remote rejects it (constraint, read-only, or
`validate`) — the same exception the local form raises, so the failure contract
is uniform local↔remote.
"""
function set_parameter!(s::ParameterServer{P}, name::Symbol, value) where {P}
    transaction(s) do p
        setproperty!(p, name, value)
    end
end

