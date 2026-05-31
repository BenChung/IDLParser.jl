# §10 Parameters. Declared parameters are typed and baked into the node: a
# `@parameters` macro generates an immutable struct (type-stable reads) plus a
# `descriptors(::Type)` method, and the live value sits behind an atomic `Ref{P}`
# inside a `ParameterServer{P}`. Mutation is transactional — `transaction(server)
# do p … end` builds a mutable draft, validates the *whole* candidate, swaps once,
# and fires one `/parameter_events` batch. Single-statement sugar (`server.x = v`,
# `setproperties!(server, …)`) routes through the same commit path. Undeclared
# params live in a separate `dynamic_parameters` dict, gated by `allow_undeclared`.
#
# The six standard parameter services + `/parameter_events` are generic over `P`
# via reflection (`fieldnames`/`fieldtypes` + `descriptors()`); they reflect over
# the union of declared fields and the dynamic dict so a ROS client sees one flat
# namespace. The Service/Client pattern layer (§8) and the `rcl_interfaces`
# generated types aren't in this package yet, so the wire-facing service handlers
# are staged behind a `TODO(layer §8)` that does not break precompilation.

import Dates

export @parameters, ParameterServer, ParameterDescriptor,
       descriptors, validate, transaction, setproperties, setproperties!,
       dynamic_parameters, declared_names, parameter_names, parameter, set_parameter!,
       on_parameter_event, readonly, ParameterRejection

# ── legal value types (§10) ───────────────────────────────────────────────────
# The closed set ROS2's `ParameterValue` tagged union can carry: bool, int64,
# float64, string, byte arrays, and `Vector` of {bool,int64,float64,string}.
# `Symbol` is accepted as sugar over string-with-choices. Anything else is
# rejected at macro expansion (below) rather than silently coerced on the wire.

const _SCALAR_PARAM_TYPES = (Bool, Int64, Float64, String, Symbol)
const _ARRAY_ELT_TYPES    = (Bool, Int64, Float64, String, UInt8)

# Map a Julia field type to the ROS2 `ParameterType` tag (rcl_interfaces values).
# 0 = NOT_SET; we never store NOT_SET for a declared field.
const PARAMETER_NOT_SET        = UInt8(0)
const PARAMETER_BOOL           = UInt8(1)
const PARAMETER_INTEGER        = UInt8(2)
const PARAMETER_DOUBLE         = UInt8(3)
const PARAMETER_STRING         = UInt8(4)
const PARAMETER_BYTE_ARRAY     = UInt8(5)
const PARAMETER_BOOL_ARRAY     = UInt8(6)
const PARAMETER_INTEGER_ARRAY  = UInt8(7)
const PARAMETER_DOUBLE_ARRAY   = UInt8(8)
const PARAMETER_STRING_ARRAY   = UInt8(9)

"""
    parameter_type(::Type) -> UInt8

The `rcl_interfaces/msg/ParameterType` tag for a declared field type. `Symbol`
maps to STRING (it is string-with-choices sugar). Throws for anything outside the
legal value set — the same check the macro applies at expansion time.
"""
function parameter_type(::Type{T}) where {T}
    T === Bool            && return PARAMETER_BOOL
    T === Int64           && return PARAMETER_INTEGER
    T === Float64         && return PARAMETER_DOUBLE
    (T === String || T === Symbol) && return PARAMETER_STRING
    T === Vector{UInt8}   && return PARAMETER_BYTE_ARRAY
    T === Vector{Bool}    && return PARAMETER_BOOL_ARRAY
    T === Vector{Int64}   && return PARAMETER_INTEGER_ARRAY
    T === Vector{Float64} && return PARAMETER_DOUBLE_ARRAY
    T === Vector{String}  && return PARAMETER_STRING_ARRAY
    throw(ArgumentError("not a legal parameter type: $(T)"))
end

# Predicate form used by the macro to validate field types up front.
_is_legal_param_type(::Type{T}) where {T} =
    (T in _SCALAR_PARAM_TYPES) ||
    (T <: Vector && eltype(T) in _ARRAY_ELT_TYPES)

# ── descriptors (§10) ───────────────────────────────────────────────────────────
# One per declared field. `constraint` is `nothing` (no bound), a 2-tuple
# `(lo, hi)` numeric range, or a tuple of allowed values (the `∈ choices` form);
# `validate_value` checks a candidate field value against it. `read_only` blocks
# *runtime* sets (a startup override is still allowed, §10).

"""
    ParameterDescriptor

Per-field metadata for a declared parameter (§10): its `name`, Julia `type`, ROS2
`ptype` tag, human `description`, optional `constraint`, `read_only` flag, and the
schema `default`. Built by [`@parameters`](@ref); reflected over by the six
parameter services (`describe`/`get_types`/…). `constraint` is `nothing`, a
numeric `(lo, hi)` range, or a tuple of allowed values (`∈ choices`).
"""
struct ParameterDescriptor
    name::Symbol
    type::Type
    ptype::UInt8
    description::String
    constraint::Any        # nothing | (lo, hi)::Tuple | choices::Tuple
    read_only::Bool
    default::Any
end

Base.show(io::IO, d::ParameterDescriptor) =
    print(io, "ParameterDescriptor(", d.name, "::", d.type,
          d.read_only ? ", readonly" : "", ")")

# Validate a candidate value against a descriptor's constraint. Returns `nothing`
# on success, or a human reason string on violation (the wire `reason` field).
function _check_constraint(d::ParameterDescriptor, value)
    c = d.constraint
    c === nothing && return nothing
    if c isa Tuple && length(c) == 2 && c[1] isa Number && c[2] isa Number
        lo, hi = c
        (value < lo || value > hi) &&
            return "value $(value) out of range [$(lo), $(hi)]"
    elseif c isa Tuple
        value in c ||
            return "value $(repr(value)) not in allowed set $(c)"
    end
    return nothing
end

# ── readonly marker (§10) ───────────────────────────────────────────────────────
# `field = default |> readonly` tags a field read-only. The macro detects the pipe
# *syntactically* at parse time (`_parse_param_field`) and strips it — the default
# is never wrapped at runtime. `readonly`/`_ReadOnly` exist only as a harmless
# runtime identity so a stray `readonly(x)` outside the DSL doesn't error.

struct _ReadOnly
    value::Any
end

"""
    readonly(default)

Mark a `@parameters` field read-only: `field::T = default |> readonly`. Read-only
blocks *runtime* sets (an exception internally; a rejected `SetParametersResult`
on the wire), but a startup override (CLI/launch/YAML) is still allowed (§10).
The marker is consumed at macro expansion, so the stored default is the bare value.
"""
readonly(x) = _ReadOnly(x)

# ── the @parameters macro (§10) ───────────────────────────────────────────────
# Generates: (1) an immutable struct of the declared fields; (2) a keyword
# constructor that overlays overrides onto the defaults and coerces to field
# types; (3) a `descriptors(::Type{P})` method returning the ordered descriptor
# vector. Field syntax, consistent with ROSMessages' annotated form:
#
#   "doc"                              # optional leading string → field description
#   name::T = default                  # default required
#   name::T = default ∈ lo..hi         # numeric range constraint
#   name::T = default ∈ (a, b, c)      # choice-set constraint
#   name::T = default |> readonly      # read-only at runtime

"""
    @parameters struct P
        "doc"  field::T = default [∈ lo..hi | ∈ (choices…)] [|> readonly]
        …
    end

Declare a typed parameter schema (§10). Generates an immutable `struct P` (so
reads are type-stable), a keyword constructor `P(; field=default, …)` that
overlays startup overrides onto the defaults and coerces to the field types, and a
`descriptors(::Type{P})` method. Legal field types are the ROS2 `ParameterValue`
set — `Bool`, `Int64`, `Float64`, `String`, `Symbol` (string-with-choices sugar),
and `Vector` of those (plus `Vector{UInt8}` byte arrays). Anything else errors at
expansion.

Per-field annotations: a leading string literal is the description; `∈ lo..hi` is
a numeric range; `∈ (a, b, …)` is a choice set; `|> readonly` blocks runtime sets.
A user `validate(::P)` method (default no-op) adds cross-field rules — exactly
ROS2's `add_on_set_parameters_callback`.
"""
macro parameters(structdef)
    structdef isa Expr && structdef.head === :struct ||
        throw(ArgumentError("@parameters expects `struct …`"))
    sig  = structdef.args[2]
    body = structdef.args[3]
    name = sig isa Symbol ? sig : throw(ArgumentError("@parameters: parametric structs are not supported"))

    fieldexprs = Expr[]      # `name::T` for the struct body
    descrips   = Expr[]      # ParameterDescriptor(...) constructions
    kwparams   = Expr[]      # `name = coerced(default)` keyword args
    ctorassign = Expr[]      # positional args into the inner constructor

    pending_doc = ""         # a leading string literal annotates the next field
    for stmt in body.args
        if stmt isa LineNumberNode
            continue
        elseif stmt isa String
            pending_doc = stmt
            continue
        elseif stmt isa Expr && stmt.head === :string
            # interpolated/concatenated literal — fold to its textual form
            pending_doc = string(stmt)
            continue
        end

        fname, ftype, default, constraint, ro = _parse_param_field(stmt)
        doc = pending_doc; pending_doc = ""

        # Selective hygiene: user-supplied type/default/constraint are escaped so
        # they resolve in the caller's module; the field name stays a bare symbol
        # (the struct's own field); ROSNode helpers are spliced as *values*
        # (`$(fn)`), which survive any later `esc` and bind to this module.
        push!(fieldexprs, :($(fname)::$(esc(ftype))))
        push!(kwparams, Expr(:kw, fname, esc(default)))
        push!(ctorassign, :($(_coerce_param)($(esc(ftype)), $(fname))))
        push!(descrips, :(
            $(ParameterDescriptor)(
                $(QuoteNode(fname)), $(esc(ftype)), $(parameter_type)($(esc(ftype))),
                $(doc), $(esc(constraint)), $(ro),
                $(_coerce_param)($(esc(ftype)), $(esc(default))))))
    end

    ctor_kw = Expr(:parameters, kwparams...)
    typ = esc(name)

    # The struct + keyword constructor. Built with leaves already escaped, so the
    # whole block is *not* re-escaped. The inner constructor name matches the
    # (escaped) type so `new` resolves to it; `ctorassign` carries spliced helpers.
    structblock = Expr(:struct, false, typ,
        Expr(:block, fieldexprs...,
             Expr(:function, Expr(:call, typ, ctor_kw),
                  Expr(:block, Expr(:call, :new, ctorassign...)))))

    # The `descriptors` reflection method — the reflection root the parameter
    # services read. `descriptors` is referenced through a `GlobalRef` to this
    # (the macro's) module so it *extends* ROSNode.descriptors rather than getting
    # hygiene-gensym'd into a fresh local; it dispatches on the escaped user type.
    descriptors_ref = GlobalRef(@__MODULE__, :descriptors)
    descmethod = Expr(:(=),
        Expr(:call, descriptors_ref, :(::Type{$(typ)})),
        :(ParameterDescriptor[$(descrips...)]))

    return quote
        Base.@__doc__ $(structblock)
        $(descmethod)
        nothing
    end
end

# Split one field statement into (name, type, default, constraint, read_only).
# Accepts `name::T = default`, optionally suffixed `∈ constraint` then `|> readonly`
# (both detected syntactically and stripped — the returned `default` is bare).
function _parse_param_field(stmt)
    stmt isa Expr && stmt.head === :(=) ||
        throw(ArgumentError("@parameters field needs a default: `name::T = default`, got $(stmt)"))
    lhs = stmt.args[1]
    rhs = stmt.args[2]

    lhs isa Expr && lhs.head === :(::) ||
        throw(ArgumentError("@parameters field needs a type annotation: `name::T`, got $(lhs)"))
    fname = lhs.args[1]::Symbol
    ftype = lhs.args[2]

    constraint = nothing
    # `∈` parses as a `:call` with head `:∈`/`in`; peel it off the default.
    if rhs isa Expr && rhs.head === :call && rhs.args[1] in (:∈, :in)
        constraint = _normalize_constraint(rhs.args[3])
        rhs = rhs.args[2]
    end

    ro = false
    # `default |> readonly` desugars to `|>(default, readonly)`; detect & unwrap.
    if rhs isa Expr && rhs.head === :call && rhs.args[1] === :|> && rhs.args[3] === :readonly
        ro = true
        rhs = rhs.args[2]
    end

    return fname, ftype, rhs, constraint, ro
end

# Normalize the `∈` right-hand side to a literal `constraint` expression a
# `ParameterDescriptor` can hold: a `lo..hi` range becomes the tuple `(lo, hi)`;
# a tuple/vector of choices stays as the tuple of allowed values.
function _normalize_constraint(c)
    if c isa Expr && c.head === :call && c.args[1] === :(..)
        return Expr(:tuple, c.args[2], c.args[3])
    elseif c isa Expr && c.head === :tuple
        return c
    elseif c isa Expr && c.head === :vect
        return Expr(:tuple, c.args...)
    end
    return c
end

# Coerce an override value to the declared field type. Bools/ints/floats and
# strings convert through `convert`; `Symbol` accepts a string or symbol; arrays
# convert element-wise. A failed coercion surfaces as the ctor's error.
_coerce_param(::Type{T}, v) where {T} = convert(T, v)
_coerce_param(::Type{Symbol}, v::AbstractString) = Symbol(v)
_coerce_param(::Type{Symbol}, v::Symbol) = v
_coerce_param(::Type{String}, v::Symbol) = String(v)
_coerce_param(::Type{Vector{T}}, v::AbstractVector) where {T} = collect(T, v)

# ── descriptors / validate fallbacks (§10) ──────────────────────────────────────

"""
    descriptors(::Type{P}) -> Vector{ParameterDescriptor}

The ordered per-field metadata for a `@parameters` schema `P` — the reflection
root for the parameter services. A type with no generated method (a plain struct
used as a schema) gets an empty descriptor list.
"""
descriptors(::Type) = ParameterDescriptor[]
descriptors(p) = descriptors(typeof(p))

"""
    validate(candidate::P)

User hook for cross-field rules — ROS2's `add_on_set_parameters_callback`,
expressed as a method on the *whole* candidate (§10). The default is a no-op.
Throw [`ParameterRejection`](@ref) (or any exception) to reject a transaction; an
internal caller sees the throw, an external client sees a rejected
`SetParametersResult`. Define `ROSNode.validate(p::PlannerParams) = …` to override.
"""
validate(_) = nothing

"""
    ParameterRejection(reason)

A parameter set was rejected — by a per-field constraint, a `read_only` runtime
set, or a user `validate`. Carries the human `reason` string that becomes the
wire `SetParametersResult.reason`. Internal callers see it raised; the service
layer catches it into a `successful=false` reply (§10).
"""
struct ParameterRejection <: Exception
    reason::String
end
Base.showerror(io::IO, e::ParameterRejection) =
    print(io, "ParameterRejection: ", e.reason)

# ── structural setproperties (no ConstructionBase dep) ──────────────────────────
# Reconstruct an immutable schema value with a subset of fields overridden, via
# the keyword constructor `@parameters` generates. A NamedTuple is a partial
# overlay; the field order is the struct's own.

"""
    setproperties(base::P, overrides::NamedTuple) -> P
    setproperties(base::P, overrides::P) -> P

A pure (non-effectful) rebuild of an immutable schema value with `overrides`
applied — a NamedTuple is a partial overlay, a full `P` value is replace-all
(§10). No `node` involved: this is the value-level primitive the transaction
commit and the `setproperties!` sugar build on.
"""
function setproperties(base::P, overrides::NamedTuple) where {P}
    fns = fieldnames(P)
    vals = ntuple(length(fns)) do i
        f = fns[i]
        haskey(overrides, f) ? getfield(overrides, f) : getfield(base, f)
    end
    return P(; NamedTuple{fns}(vals)...)
end
setproperties(::P, overrides::P) where {P} = overrides

# ── the parameter server (§10) ──────────────────────────────────────────────────
# Holds the live schema value behind an atomic `Ref{P}` (whole-struct swap, so a
# racing reader sees old-or-new, never half-applied), the descriptors (cached
# once), the dynamic side dict + `allow_undeclared` gate, the mutation lock, the
# on-change listeners, and a slot for the `/parameter_events` publisher the §8
# service wiring attaches. `node` is held loosely (`Any`) so this file does not
# depend on a particular Node shape — the node attaches its server and routes
# `node.parameters` to it.

"""
    ParameterServer{P}(node, initial::P; allow_undeclared=false) -> ParameterServer{P}

The §10 parameter subsystem for one node, parameterized on the declared schema
`P`. The live value sits behind an atomic field (whole-struct swap → readers see a
complete value, rollback is free); undeclared params live in `dynamic_parameters`
(gated by `allow_undeclared`, default off). Mutate via [`transaction`](@ref) or
the sugar (`server.field = v`, [`setproperties!`](@ref)); read declared fields
with `server.field`.

Typically reached as `node.parameters`; the six standard parameter services and
`/parameter_events` are wired generically over `P` (see [`wire_parameter_services!`](@ref)).
"""
mutable struct ParameterServer{P}
    const node::Any                                  # the owning Node (loosely held)
    @atomic value::P                                 # live schema value (atomic swap)
    const descriptors::Vector{ParameterDescriptor}
    const by_name::Dict{Symbol, ParameterDescriptor}
    const dynamic::Dict{Symbol, Any}                 # undeclared params (Any-typed)
    const allow_undeclared::Bool
    const lock::ReentrantLock                        # the single parameter-mutation lock
    const listeners::Vector{Any}                     # on_parameter_event callbacks
    _events_pub::Any                                 # /parameter_events publisher (§8 wire)
end

function ParameterServer(node, initial::P; allow_undeclared::Bool=false) where {P}
    ds = descriptors(P)
    by = Dict{Symbol, ParameterDescriptor}(d.name => d for d in ds)
    return ParameterServer{P}(node, initial, ds, by, Dict{Symbol, Any}(),
                              allow_undeclared, ReentrantLock(), Any[], nothing)
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

"The declared field names (§10)."
declared_names(s::ParameterServer{P}) where {P} = fieldnames(P)

"The dynamic (undeclared) parameter dict (§10) — errors if `allow_undeclared` is off."
function dynamic_parameters(s::ParameterServer)
    s.allow_undeclared ||
        throw(ArgumentError("undeclared parameters disabled (allow_undeclared=false)"))
    return s.dynamic
end

# ── reads (§10) ──────────────────────────────────────────────────────────────────
# `server.field` derefs the live struct (type-stable for a declared field). The
# reserved members (`node`/`value`/…) read through normally; everything else is a
# parameter name. Declared names always win over the dynamic dict.

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
the live struct, a dynamic one from the side dict (when `allow_undeclared`).
Throws if undeclared and the gate is off — the flat-namespace read the services use.
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
# A `Draft` is a mutable overlay over the live base: `p.field = v` records an
# override (reads see pending values), assignments touch nothing live and publish
# nothing. On clean block exit we build the candidate via `setproperties`, validate
# the *whole* thing (per-field descriptors + user `validate`), swap once, and fire
# one batched event. Any throw aborts — the candidate is dropped, the cell never
# held a partial state (free rollback).

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

# Build + validate the candidate, swap once, publish one event batch. Caller holds
# `s.lock`. Returns the committed value. Validation throws `ParameterRejection`
# (constraint / read-only / user `validate`) → the lock unwinds, nothing committed.
function _commit!(s::ParameterServer{P}, base::P,
                  overrides::AbstractDict, dyn_overrides::AbstractDict) where {P}
    isempty(overrides) && isempty(dyn_overrides) && return base   # nothing to do

    candidate = isempty(overrides) ? base :
                setproperties(base, NamedTuple(overrides))

    # Per-field validation, but only for fields whose value actually *moved* — an
    # idempotent set (incl. a replace-all that re-sets a read-only field to its
    # current value) is a no-op, not a rejection. For a moved field: read-only gate
    # first (runtime sets only — startup overrides go through the ctor, not here),
    # then the constraint.
    for (name, _) in overrides
        d = get(s.by_name, name, nothing)
        d === nothing && continue
        isequal(getfield(candidate, name), getfield(base, name)) && continue
        d.read_only &&
            throw(ParameterRejection("parameter $(name) is read-only"))
        reason = _check_constraint(d, getfield(candidate, name))
        reason === nothing ||
            throw(ParameterRejection("parameter $(name): $(reason)"))
    end

    # Cross-field rules over the whole candidate (ROS2's on-set callback).
    validate(candidate)

    # Atomic whole-struct swap — readers see old-or-new, never partial.
    @atomic s.value = candidate

    # Commit dynamic-tier overrides under the same lock (wire-atomic across tiers).
    for (k, v) in dyn_overrides
        s.dynamic[k] = v
    end

    _emit_parameter_event!(s, base, candidate, overrides, dyn_overrides)
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
                                overrides::AbstractDict, dyn_overrides::AbstractDict) where {P}
    changed  = Dict{Symbol, Any}()
    previous = Dict{Symbol, Any}()
    for name in keys(overrides)
        nv = getfield(new, name); ov = getfield(old, name)
        isequal(nv, ov) && continue
        changed[name] = nv; previous[name] = ov
    end
    for (k, v) in dyn_overrides
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
# per-node opt-in; this notifies it. TODO(time §7): call the Context's sim-time
# (de)activation once that surface lands — today the value is already committed in
# the struct, so `now(node)` will read it through when the §7 routing is wired.
function _on_use_sim_time_changed(s::ParameterServer, enabled)
    node = s.node
    node === nothing && return nothing
    # TODO(time §7): `set_use_sim_time!(node.context, node, enabled)` to (de)activate
    # the Context `/clock` subscription and fire the ROS-clock jump callbacks.
    return nothing
end

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

"""
    wire_parameter_services!(server)

Declare the six standard parameter services + the `/parameter_events` publisher on
the server's node (§10/§13), each bound to the reflection handlers above. Generic
over `P` — one implementation serves every schema.

TODO(layer §8): needs the Service/Client pattern layer and the `rcl_interfaces`
generated request/response types (`DescribeParameters`, `GetParameters`,
`SetParameters`, …, and `msg/ParameterEvent`), which aren't in this package yet.
Until they land this is a no-op so a node with declared parameters precompiles and
runs with the full *in-process* surface (`transaction`/sugar/reads/events); the
wire-facing services attach here once §8 exists. The handler bodies
(`describe_parameters`, `get_parameters`, `set_parameters`, …) are already written
and tested against `P` by reflection, so wiring is a thin request/response
marshal over them.
"""
function wire_parameter_services!(s::ParameterServer)
    # TODO(layer §8): for each name in PARAMETER_SERVICE_NAMES, declare a
    #   Service(node, "~/<name>", <ReqType>) do req … marshal → handler → resp end
    # and a Publisher(node, "/parameter_events", ParameterEvent); store the latter
    # in `s._events_pub` so `_emit_parameter_event!` publishes the batch.
    return s
end

# Placeholder for the wire publish; real body lands with the generated
# `rcl_interfaces/msg/ParameterEvent` + §8 publisher (above).
function _publish_parameter_event(s::ParameterServer, batch::ParameterEventBatch)
    # TODO(layer §8): assemble `rcl_interfaces/msg/ParameterEvent` from `batch`
    # (node FQN, new_parameters/changed_parameters/deleted_parameters) and
    # `publish(s._events_pub, evt)`.
    return nothing
end
