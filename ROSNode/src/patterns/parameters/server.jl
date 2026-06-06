# ── the parameter server (§10) ──────────────────────────────────────────────────
# Holds the live schema value behind an atomic field (whole-struct swap, so a
# racing reader sees a complete old-or-new value), the descriptors (cached
# once), the dynamic side dict + `allow_undeclared` gate, the mutation lock, the
# on-change listeners, and a slot for the `/parameter_events` publisher the
# service wiring attaches. `node` is held loosely (`Any`) to keep this file
# independent of the Node type — the node attaches its server and routes
# `node.parameters` to it.

"""
    ParameterServer{P}(node, initial::P; allow_undeclared=false) -> ParameterServer{P}

The §10 parameter subsystem for one node, parameterized on the declared schema
`P`. The live value sits behind an atomic field (whole-struct swap → readers see a
complete value, rollback is free); undeclared params live in `dynamic_parameters`
(gated by `allow_undeclared`, default off). Mutate via [`transaction`](@ref) or
the sugar (`server.field = v`, [`setproperties!`](@ref)); read declared fields
with `server.field`.

Implements the ROS 2 parameter model: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html

Typically reached as `node.parameters`; the six standard parameter services and
`/parameter_events` are wired generically over `P` (see [`wire_parameter_services!`](@ref)).
"""
mutable struct ParameterServer{P}
    const node::Any                                  # the owning Node (loosely held)
    @atomic value::P                                 # live schema value
    const descriptors::Vector{ParameterDescriptor}
    const by_name::Dict{Symbol, ParameterDescriptor}
    const dynamic::Dict{Symbol, Any}                 # undeclared params (Any-typed)
    const allow_undeclared::Bool
    const lock::ReentrantLock                        # the single parameter-mutation lock
    const listeners::Vector{Any}                     # on_parameter_event callbacks
    _events_pub::Any                                 # /parameter_events publisher (§8 wire)
    const services::Vector{Any}                      # the six wired parameter-service handles
end

function ParameterServer(node, initial::P; allow_undeclared::Bool=false) where {P}
    ds = descriptors(P)
    by = Dict{Symbol, ParameterDescriptor}(d.name => d for d in ds)
    return ParameterServer{P}(node, initial, ds, by, Dict{Symbol, Any}(),
                              allow_undeclared, ReentrantLock(), Any[], nothing, Any[])
end

# Convenience: build the server straight from a schema type, overlaying startup
# overrides (CLI/launch/YAML) as a NamedTuple onto the schema defaults. Startup
# overrides bypass the read-only runtime gate (§10).
"""
    ParameterServer{P}(node; overrides=(;), allow_undeclared=false)

Build the server for schema `P`, constructing the initial value by overlaying
`overrides` (startup CLI/launch/YAML) onto the schema defaults. A startup override
of a `read_only` field is allowed — read-only only blocks runtime sets (§10).
"""
ParameterServer{P}(node; overrides::NamedTuple=(;), allow_undeclared::Bool=false) where {P} =
    ParameterServer(node, P(; overrides...); allow_undeclared=allow_undeclared)

Base.show(io::IO, s::ParameterServer{P}) where {P} =
    print(io, "ParameterServer{", nameof(P), "}(",
          length(s.descriptors), " declared, ", length(s.dynamic), " dynamic)")

"The current (live) schema value (§10) — a complete, never half-applied, struct."
current(s::ParameterServer) = @atomic s.value

"The declared schema type `P` of this server."
schema_type(::ParameterServer{P}) where {P} = P

"The declared parameter names, in schema order (§10)."
declared_names(s::ParameterServer{P}) where {P} = fieldnames(P)

"The dynamic (undeclared) parameter dict (§10) — errors if `allow_undeclared` is off."
function dynamic_parameters(s::ParameterServer)
    s.allow_undeclared ||
        throw(ArgumentError("undeclared parameters disabled (allow_undeclared=false)"))
    return s.dynamic
end

# ── reads (§10) ──────────────────────────────────────────────────────────────────
# `server.field` derefs the live struct (type-stable for a declared field).
# Reserved struct members read through; any other name is a parameter, with
# declared names taking priority over the dynamic dict.

const _SERVER_FIELDS = fieldnames(ParameterServer)

function Base.getproperty(s::ParameterServer{P}, name::Symbol) where {P}
    name in _SERVER_FIELDS && return getfield(s, name)
    if name in fieldnames(P)
        return getfield(@atomic(s.value), name)
    end
    # A dynamic param read is allowed only when the gate is open.
    if getfield(s, :allow_undeclared)
        dyn = getfield(s, :dynamic)
        haskey(dyn, name) && return dyn[name]
    end
    throw(ArgumentError("no parameter $(repr(name)) (declared: $(fieldnames(P)))"))
end

Base.propertynames(s::ParameterServer{P}) where {P} = fieldnames(P)

"""
    parameter(server, name::Symbol)

Read a parameter by `name` across both tiers (§10): a declared field reads from
the live struct, a dynamic one from the side dict (when `allow_undeclared`). The
flat-namespace read the services use; throws for an undeclared name when the gate
is off.
"""
function parameter(s::ParameterServer{P}, name::Symbol) where {P}
    name in fieldnames(P) && return getfield(@atomic(s.value), name)
    if s.allow_undeclared && haskey(s.dynamic, name)
        return s.dynamic[name]
    end
    throw(ArgumentError("no parameter $(repr(name))"))
end

"""
    parameter_names(server) -> Vector{Symbol}

The flat union of declared field names and live dynamic names (§10) — what `list`
and `/parameter_events` reflect over. Declared names come first, in schema order.
"""
function parameter_names(s::ParameterServer{P}) where {P}
    names = collect(Symbol, fieldnames(P))
    s.allow_undeclared && append!(names, sort!(collect(keys(s.dynamic))))
    return names
end

# ── transactions: the mutation primitive (§10) ──────────────────────────────────
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
    transaction(f, server::ParameterServer)
    transaction(server::ParameterServer) do p … end

The mutation primitive (§10). Runs `f(draft)` against a mutable draft of the
current parameters — assignments record pending overrides (reads inside see them),
touch nothing live, and publish nothing. On clean exit the candidate is built,
validated as a whole (per-field constraints + read-only gate + user
[`validate`](@ref)), swapped in atomically under the node's single mutation lock,
and one batched `/parameter_events` is published. Any throw aborts with free
rollback — the live cell never held a partial state.

Returns the committed schema value. Held under the lock for the block's duration,
so transactions serialize (the consistency boundary the foreign-thread dispatch
relies on).
"""
function transaction(f, s::ParameterServer{P}) where {P}
    @lock s.lock begin
        base = @atomic s.value
        draft = Draft{P}(s, base, Dict{Symbol, Any}(), Dict{Symbol, Any}())
        f(draft)                                     # may throw → abort, no commit
        return _commit!(s, base, draft.overrides, draft.dynamic)
    end
end

# Validate a candidate against the descriptors + the user `validate` hook, throwing
# `ParameterRejection` on the first violation. Per-field checks run only for fields
# whose value actually changed; `moved` lists the override names, and an idempotent
# set (e.g. a replace-all re-setting a read-only field to its current value) commits
# as a no-op. For a changed field the read-only gate runs first, then the
# constraint; the gate blocks runtime sets only, since startup overrides go through
# the ctor. Shared by the server commit and the client-side transaction pre-check,
# so both raise the same rejection.
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

# ── single-statement sugar (§10) ────────────────────────────────────────────────
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
    setproperties!(server, overrides::NamedTuple)
    setproperties!(server, full::P)

Effectful multi-field set as one implicit transaction (§10). A NamedTuple is a
partial overlay (only the named fields change); a full `P` value is replace-all
(every field is set, so each is validated and event-reported). Commits atomically
through the same path as [`transaction`](@ref); returns the committed value.
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
    set_parameter!(server, name::Symbol, value) -> committed value

Set one parameter by name across both tiers (§10) — a declared field routes to the
typed struct, an undeclared name to the dynamic dict (when `allow_undeclared`).
The single-name entry the parameter `set` service calls per item.
"""
function set_parameter!(s::ParameterServer{P}, name::Symbol, value) where {P}
    transaction(s) do p
        setproperty!(p, name, value)
    end
end

