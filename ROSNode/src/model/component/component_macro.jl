# @component — the Revise-safe, concise authoring tier that EMITS the value API in ONE top-level
# expression (DESIGN-COMPONENT-MACRO.md). There is no mutable per-component spec store: ports/params
# are arguments to a single `component(…)` call, and
# `member_schema(::Type{Base})` + the reaction/hook methods are emitted alongside the struct — so a
# struct-body edit under Revise re-runs the WHOLE block and re-keys everything on the new type together.
# Single-component authoring only (`node(…)` still composes a node kind); coexists with the raw
# `component`/`node` combinators (the primitive for dynamic composition). Neither is mandatory.

export @component

# Reactions/hooks/@service handlers are node-first `f(node, m, …)`; arg 2 is the component instance.
# Strip a signature's `where {…}` and `::Ret` wrappers down to the inner `f(…)` call (the SAME Expr
# object, so mutating it mutates the def). Handles `f(…)`, `f(…)::Ret`, and `f(…) where {…}` reactions/hooks.
function _peel_to_call(sig)
    while sig isa Expr && (sig.head === :where || sig.head === :(::))
        sig = sig.args[1]
    end
    return sig
end

# A bare `m` is annotated `m::Base` (so it dispatches on the component, the functor convention); an
# explicit `m::T` is left as written. Mutates the def in place and returns it.
function _component_inject_member!(fdef::Expr, base)
    call = _peel_to_call(fdef.args[1])
    (call isa Expr && call.head === :call) ||
        error("@component: expected a `function f(node, m, …)` reaction/hook, got `$(fdef)`")
    length(call.args) >= 3 ||
        error("@component: a reaction/hook is node-first and needs at least `(node, m)`, got `$(call)`")
    call.args[3] isa Symbol && (call.args[3] = Expr(:(::), call.args[3], base))   # bare `m` → `m::Base`
    return fdef
end

# Is `x` a method-definition signature (`f(…)`, `f(…)::Ret`, or `f(…) where {…}`)?
_is_method_sig(x) = (c = _peel_to_call(x); c isa Expr && c.head === :call)
# Is `ln` a method DEFINITION (`function f … end` or `f(…) = …`)?
_is_method_def(ln) = ln isa Expr && (ln.head === :function || (ln.head === :(=) && _is_method_sig(ln.args[1])))

# The function name out of a (member-injected) def — used to reference the handler in `component(…)`.
function _component_defname(fdef::Expr)
    n = _peel_to_call(fdef.args[1]).args[1]
    n isa Symbol || error("@component: reaction/handler name must be a plain symbol, got `$(n)`")
    return n
end

# The lifecycle hooks the runtime dispatches on (node, member). A standalone def of one of these in the
# block must EXTEND the ROSNode generic, not define a shadowing local — so rewrite its name to a GlobalRef.
const _COMPONENT_HOOKS = (:configure, :activate, :deactivate, :cleanup, :on_error)
function _component_qualify_hook!(fdef::Expr, M)
    call = _peel_to_call(fdef.args[1])
    call.args[1] in _COMPONENT_HOOKS || return fdef
    call.args[1] = GlobalRef(M, call.args[1])      # `configure(…)` → `ROSNode.configure(…)` (extend, don't shadow)
    return fdef
end

# The emitted `@parameters` struct name for component base `M` → `MParams`.
_params_name(base::Symbol) = Symbol(base, :Params)

"""
    @component mutable struct Name{Name} <: Component{Name}
        field::T = default                       # private state; inline default feeds the zero-arg ctor
        @param  rate::Int64 = 5 ∈ 1..50          # → an emitted `@parameters struct NameParams`
        @provides Iface                          # interface(s) this component provides
        @requires src::Sensor                    # inject a sibling (a component type or an @interface)
        @publishes out::T on "~/out"             # a publisher port (no handler)
        @hears  function on_msg(node, m, msg::T) … end       # subscription port + handler (bundled)
        @every  :rate function tick(node, m) … end           # timer port + handler (rate = Hz or :param)
        @service "~/srv" function srv(node, m, x::X)::@NamedTuple{…} … end   # inline service authoring
        configure(node, m) = …                   # lifecycle hooks live here too (re-coupled on a struct edit)
    end

Author a single component in one expression. Expands to the `mutable struct` (`{Name}` injected if you
write just `Name`), a zero-arg `Name{Name}()` ctor from the inline field defaults, an
`@parameters struct NameParams` from the `@param` fields, the reaction/hook/`@service`/`@action` method
defs (a bare `m` annotated as `m::Name`), and `member_schema(::Type{Name}) = component(Name, …)` on the
BARE base. Editing anything re-runs the whole block, so the struct and every method re-key on the new
type together.

Conventions:
- **Wire clause.** The declaration directive `@publishes name::T` takes the trailing `on "wire"` form; the
  handler directives `@hears`/`@service`/`@action` take a **leading** `"wire"` string (matching the
  standalone `@service`/`@action` macros). The split follows the name source: `@publishes` names the port,
  the handler directives derive it from the function.
- **`@hears`/`@every` are inline-only in v1**: the handler is written in place as `function f(node, m, …) … end`
  (so a struct edit re-couples it). To wire a port to an externally-defined handler, drop to the raw
  `hears(:n, T, h)` / `every(:n, rate, h)` combinators.
- A reaction/hook/handler is node-first `(node, m, …)`; a non-node-first interface impl (e.g.
  `battery(s::Sensor)`) is defined OUTSIDE the block.

`@component` is sugar over the value API — the raw `component`/`node` combinators remain the primitive
for dynamic composition. v1 covers the DI-free common case (plus `@provides`); a DI **consumer**
(`@requires` + an injected ctor) or a client port is authored with the raw `component(M, …; requires=(I,),
ctor=f)` / `uses(:n, marker)` API. Use it from a module that does `using ROSNode`.
"""
macro component(structexpr)
    (structexpr isa Expr && structexpr.head === :struct) ||
        error("@component expects a `mutable struct Name … end`, got `$(structexpr)`")
    structexpr.args[1] === true ||
        error("@component: a component holds mutable state — write `mutable struct …`")
    newhead, base = _mixin_struct_head(structexpr.args[2], "@component")    # inject {Name} / validate the head
    M = @__MODULE__                                            # ROSNode — for esc-immune GlobalRefs

    fields = Any[]; defaults = Any[]; nodefault = Symbol[]    # struct fields + ctor default exprs
    params = Any[]                                            # @parameters body lines (docs + field stmts)
    provides = Any[]; ports = Any[]; emits = Any[]            # provides ifaces / value-API ports / method defs
    reqs = Tuple{Symbol, Any, Symbol}[]                      # @requires deps: (field, marker, hidden-param)

    for ln in structexpr.args[3].args
        ln isa LineNumberNode && continue
        if ln isa Expr && ln.head === :macrocall
            mname = ln.args[1]
            rest  = filter(a -> !(a isa LineNumberNode), ln.args[2:end])
            if mname === Symbol("@param")
                isempty(rest) && error("@component: `@param [\"doc\"] field::T = default [∈ …] [|> …]`")
                length(rest) > 2 && error("@component: `@param` takes an optional leading doc string then " *
                    "ONE `field::T = default [∈ …] [|> …]` statement, got $(length(rest)) parts in `$(ln)`")
                if length(rest) == 2                         # a leading doc string → its own @parameters line
                    rest[1] isa AbstractString ||
                        error("@component: `@param`'s leading doc must be a string literal, got `$(rest[1])`")
                    push!(params, rest[1])
                end
                push!(params, last(rest))                    # the `field::T = default [∈ …] [|> …]` statement
            elseif mname === Symbol("@provides")
                isempty(rest) && error("@component: `@provides Iface [Iface2 …]` needs at least one interface")
                for r in rest                                # accept `@provides A B` and `@provides (A, B)` alike
                    (r isa Expr && r.head === :tuple) ? append!(provides, r.args) : push!(provides, r)
                end
            elseif mname === Symbol("@requires")
                isempty(rest) && error("@component: `@requires field::Marker [field2::Marker2 …]` — name the " *
                    "injected field and the component or @interface type to inject into it, e.g. " *
                    "`@requires battery_src::Sensor`")
                for r in rest
                    entries = (r isa Expr && r.head === :tuple) ? r.args : (r,)   # `@requires (a::A, b::B)` too
                    for e in entries
                        (e isa Expr && e.head === :(::) && e.args[1] isa Symbol) || error("@component: " *
                            "`@requires` takes `field::Marker` (got `$(e)`) — name the injected field and its " *
                            "component/@interface type, e.g. `@requires battery_src::Sensor`")
                        push!(reqs, (e.args[1]::Symbol, e.args[2], Symbol("__", e.args[1], "_T")))
                    end
                end
            elseif mname === Symbol("@uses")
                error("@component: `@uses` is not supported yet — author a client port with the raw value API " *
                      "for now: a `uses(:name, marker)` port in `member_schema(::Type{M}) = component(M, …)`.")
            elseif mname === Symbol("@publishes")
                isempty(rest) && error("@component: `@publishes name::T [on \"wire\"]`")
                decl = rest[1]
                on   = length(rest) >= 2 ? rest[2] : nothing
                wire = length(rest) >= 3 ? rest[3] : nothing
                name, T = _parse_port_decl(decl, "@component")
                w = _parse_on(on, wire, "@component")
                push!(ports, :( $(GlobalRef(M, :publishes))($(QuoteNode(name)), $T; on = $w) ))
            elseif mname === Symbol("@hears")
                isempty(rest) && error("@component: `@hears [\"wire\"] function f(node, m, msg::T) … end`")
                if length(rest) == 2
                    rest[1] isa AbstractString ||
                        error("@component: `@hears`'s leading wire must be a string literal, got `$(rest[1])`")
                    wire, fdef = String(rest[1]), rest[2]
                else
                    wire, fdef = nothing, rest[1]
                end
                _is_method_def(fdef) || error("@component: `@hears [\"wire\"] function f(node, m, msg::T) … end`")
                _component_inject_member!(fdef, base)
                fname = _component_defname(fdef)
                call  = _peel_to_call(fdef.args[1])
                length(call.args) >= 4 || error("@component: `@hears` handler needs `(node, m, msg::T)`")
                msgarg = call.args[4]
                (msgarg isa Expr && msgarg.head === :(::)) || error("@component: `@hears` message arg needs a type, got `$(msgarg)`")
                T = msgarg.args[2]
                push!(emits, fdef)
                push!(ports, :( $(GlobalRef(M, :hears))($(QuoteNode(fname)), $T, $fname; on = $wire) ))
            elseif mname === Symbol("@every")
                length(rest) == 2 || error("@component: `@every rate function f(node, m) … end` (rate = Hz or :param)")
                rate, fdef = rest[1], rest[2]
                _is_method_def(fdef) || error("@component: `@every rate function f(node, m) … end`")
                _component_inject_member!(fdef, base)
                fname = _component_defname(fdef)
                push!(emits, fdef)
                push!(ports, :( $(GlobalRef(M, :every))($(QuoteNode(fname)), $rate, $fname) ))
            elseif mname === Symbol("@service") || mname === Symbol("@action")
                args = copy(ln.args)
                i = findlast(a -> _is_method_def(a), args)
                i === nothing && error("@component: `$(mname)` needs an inline `function … end` handler")
                _component_inject_member!(args[i], base)
                fname = _component_defname(args[i])
                push!(emits, Expr(:macrocall, args...))      # the @service/@action authoring macrocall
                push!(ports, fname)                          # the trait-bearing handler (→ _to_port converts)
            else
                error("@component: unknown directive `$(mname)` in the body")
            end
        elseif _is_method_def(ln)
            # a standalone method def — must be node-first `(node, m, …)` (its 2nd arg is annotated `m::Base`).
            # A lifecycle-hook name (configure/activate/…) extends the ROSNode generic (qualified via GlobalRef);
            # any other node-first def stays a module-local function (e.g. a named reaction handler). A
            # non-node-first helper (e.g. an interface impl `f(x)`) belongs OUTSIDE the block.
            push!(emits, _component_qualify_hook!(_component_inject_member!(ln, base), M))
        elseif ln isa Expr && ln.head === :(::) && ln.args[1] isa Symbol
            push!(fields, ln); push!(nodefault, ln.args[1])   # struct field, NO default
        elseif ln isa Expr && ln.head === :(=) && ln.args[1] isa Expr && ln.args[1].head === :(::) && ln.args[1].args[1] isa Symbol
            push!(fields, ln.args[1]); push!(defaults, ln.args[2])   # struct field WITH default
        else
            error("@component: unexpected body line `$(ln)` — expected a field, a directive, or a reaction/hook def")
        end
    end

    isempty(nodefault) ||
        error("@component: field(s) $(nodefault) need an inline default (e.g. `x::T = …`) so the zero-arg " *
              "`$(base){…}()` ctor can build the component (a value set later still needs a default placeholder)")

    # The user authors only the phantom `{Name}` param; the macro OWNS any further params. A DI consumer
    # declares its deps with `@requires field::Marker` (above): the macro adds one hidden param per dep to
    # hold the injected sibling type-stably, so a user-written extra param would collide with that scheme.
    # Reject it here, at the authoring site, pointing at `@requires` and the raw API.
    tvs    = (newhead.args[1] isa Expr && newhead.args[1].head === :curly) ? newhead.args[1].args[2:end] : Any[]
    pnames = map(_param_name, tvs)                                  # bare names for the curly
    length(pnames) == 1 ||
        error("@component: `$(base)` declares type parameter(s) beyond the member-path `Name` " *
              "($(join(pnames, ", "))). Author only `$(base){Name}`; inject dependencies with " *
              "`@requires field::Marker` (the macro adds the parameters that hold them), or drop to the raw " *
              "value API — `member_schema(::Type{$(base)}) = component($(base), …; requires=(I,), ctor=f)` — " *
              "for a hand-rolled parametric component.")
    NM = pnames[1]                                                  # the member-path parameter (any spelling)
    if isempty(reqs)
        # No DI: the zero-arg ctor the functor `DefaultCtor` calls — `Base{Name}() = Base{Name}(defaults…)`.
        # A field-less component already has the implicit inner `Base{Name}()`; emitting an outer one would
        # call itself (its body is the same zero-arg call) and recurse — so skip it and use the implicit one.
        curly = Expr(:curly, base, NM)
        ctor  = isempty(fields) ? nothing :
                Expr(:(=), Expr(:where, Expr(:call, curly), tvs...), Expr(:call, curly, defaults...))
    else
        # DI consumer: add one hidden param per required dep, a field of that param type to hold the injected
        # sibling (type-stable reads), and a `construct(::Type{base}, node, ::Val{Name}, deps…)` method that
        # places `Name` and stores them. The resolver injects each dep positionally in `requires` order;
        # `typeof(dep)` fixes the hidden param to the resolved sibling's concrete type. There is no zero-arg
        # ctor — a `requires`-bearing component can't run standalone (the default `construct` rejects it).
        append!(newhead.args[1].args, (psym for (_, _, psym) in reqs))         # base{Name, __a_T, …}
        for (fld, _, psym) in reqs
            push!(fields, Expr(:(::), fld, psym))                             # injected field, no default
        end
        depsyms = Symbol[fld for (fld, _, _) in reqs]
        csig = Expr(:where, Expr(:call, GlobalRef(M, :construct),
                        :(::Type{$base}), :node, Expr(:(::), Expr(:curly, :Val, NM)), depsyms...), NM)
        cbody = Expr(:call, Expr(:curly, base, NM, (:(typeof($d)) for d in depsyms)...), defaults..., depsyms...)
        ctor  = Expr(:(=), csig, cbody)
    end

    paramsblock = isempty(params) ? nothing :
        Expr(:macrocall, Symbol("@parameters"), __source__,
             Expr(:struct, false, _params_name(base), Expr(:block, params...)))

    Pargs = isempty(params) ? () : (_params_name(base),)
    schemacall = Expr(:call, GlobalRef(M, :component), base, Pargs..., ports...)
    # kwargs (`; provides=…, requires=…`) ride a `:parameters` node placed right after the callee.
    kws = Expr(:parameters)
    isempty(provides) || push!(kws.args, Expr(:kw, :provides, Expr(:tuple, provides...)))
    isempty(reqs)     || push!(kws.args, Expr(:kw, :requires, Expr(:tuple, (mk for (_, mk, _) in reqs)...)))
    isempty(kws.args) || insert!(schemacall.args, 2, kws)
    memberschema = Expr(:(=), Expr(:call, GlobalRef(M, :member_schema), :(::Type{$base})), schemacall)

    out = Expr(:block,
               Expr(:struct, true, newhead, Expr(:block, fields...)),   # mutable struct (fields only)
               (ctor === nothing ? () : (ctor,))...,
               (paramsblock === nothing ? () : (paramsblock,))...,
               emits...,
               memberschema,
               base)
    return esc(out)
end
