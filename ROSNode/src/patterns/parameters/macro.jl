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

See the ROS 2 parameters concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Parameters.html

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

    # The struct + keyword constructor. Leaves are already escaped, so the whole
    # block stays unescaped. The inner constructor name matches the (escaped) type
    # so `new` resolves to it; `ctorassign` carries spliced helpers.
    structblock = Expr(:struct, false, typ,
        Expr(:block, fieldexprs...,
             Expr(:function, Expr(:call, typ, ctor_kw),
                  Expr(:block, Expr(:call, :new, ctorassign...)))))

    # The `descriptors` reflection method the parameter services read. Referenced
    # through a `GlobalRef` to this (the macro's) module so it extends
    # ROSNode.descriptors instead of being hygiene-gensym'd into a fresh local;
    # it dispatches on the escaped user type.
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
expressed as a method on the whole candidate (§10). The default is a no-op.
Throw [`ParameterRejection`](@ref) (or any exception) to reject a transaction; an
internal caller sees the throw, an external client sees a rejected
`SetParametersResult`. Define `ROSNode.validate(p::PlannerParams) = …` to override.
"""
validate(_) = nothing

"""
    ParameterRejection(reason)

Signals a rejected parameter set, raised by a per-field constraint, a `read_only`
runtime set, or a user `validate`. Carries the human `reason` string that becomes
the wire `SetParametersResult.reason`. Internal callers see it raised; the service
layer catches it into a `successful=false` reply (§10).
"""
struct ParameterRejection <: Exception
    reason::String
end
Base.showerror(io::IO, e::ParameterRejection) =
    print(io, "ParameterRejection: ", e.reason)

# ── structural setproperties (no ConstructionBase dep) ──────────────────────────

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

