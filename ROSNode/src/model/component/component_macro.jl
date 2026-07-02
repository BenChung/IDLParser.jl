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

# Reactions/hooks are emitted VERBATIM — the macro reads only the handler name (and, for `@hears`, the
# message type) and never inspects or rewrites the member argument. You type it yourself: `m::Base` (any
# name) dispatches the reaction on the component and keeps `entities`/`parameters` type-stable, while a
# bare `m` is an ordinary `::Any` catch-all — the same dispatch rules as any Julia method.

# Is `x` a method-definition signature (`f(…)`, `f(…)::Ret`, or `f(…) where {…}`)?
_is_method_sig(x) = (c = _peel_to_call(x); c isa Expr && c.head === :call)
# Is `ln` a method DEFINITION (`function f … end` or `f(…) = …`)?
_is_method_def(ln) = ln isa Expr && (ln.head === :function || (ln.head === :(=) && _is_method_sig(ln.args[1])))

# The function name out of a reaction/handler def — used to reference the handler in `component(…)`.
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

# ── shared directive engine ───────────────────────────────────────────────────────────────────────
# The `@component`/`@schema` port sublanguage. Both front-ends parse the SAME port directives through
# `parse_port_directive`; they differ only in framing (a struct vs. a `member_schema`-only block) and in
# the binding directives (`@param`/`@requires`/`@provides`/`@ctor`). Each port nonterminal lowers to one
# value combinator (functor.jl). The directive forms + combinator mapping are the canonical reference in
# the `@component` docstring (and `@schema`'s for its framing) — keep them in sync with this parser.
# Quick map: @publishes/@uses → _parse_decl_port; @hears/@serves/@runs → _parse_handler_port;
# @every → _parse_every; the `[name =>] handler` split → _parse_bound_handler; wires → _parse_wires.

const _PORT_DIRECTIVES = (Symbol("@publishes"), Symbol("@uses"), Symbol("@hears"),
                          Symbol("@serves"), Symbol("@runs"), Symbol("@every"),
                          Symbol("@service"), Symbol("@action"))
is_port_directive(mname) = mname in _PORT_DIRECTIVES

# a directive's args with LineNumberNodes stripped (args[2] is the directive's own line node).
_dargs(ln::Expr) = filter(a -> !(a isa LineNumberNode), ln.args[2:end])

# ⟨rate⟩ — a number (Hz) or a `:param` symbol (spliced back as a Symbol literal).
function _parse_rate(x, who)
    x isa Real && return x
    x isa QuoteNode && x.value isa Symbol && return x
    x isa Expr && x.head === :quote && length(x.args) == 1 && x.args[1] isa Symbol && return QuoteNode(x.args[1])
    error("$(who): a timer rate must be a number (Hz) or a `:param` symbol, got `$(x)`")
end

# Split a directive's args into (wire, middle): a leading `"wire"` String and/or a trailing `on "wire"`,
# at most one of each and never both. `middle` is what remains (the port-decl or bound-handler args).
# `allow_lead=false` (the decl directives) forbids the leading-string form — only handler directives
# carry a leading wire (the @component-legacy spelling).
function _parse_wires(args, who; allow_lead::Bool = true)
    a = collect(args); lead = nothing; trail = nothing
    if allow_lead && !isempty(a) && a[1] isa AbstractString
        lead = String(a[1]); a = a[2:end]
    end
    if length(a) >= 2 && a[end-1] === :on
        a[end] isa AbstractString ||
            error("$(who): `on` must be followed by a wire string literal, got `$(a[end])`")
        trail = String(a[end]); a = a[1:end-2]
    end
    (lead !== nothing && trail !== nothing) &&
        error("$(who): give the wire once — a leading \"wire\" string or a trailing `on \"wire\"`, not both")
    return (lead === nothing ? trail : lead), a
end

# ⟨bound⟩ → (name_override, port_type, handler-spec). The `port =>` prefix sets the port. `name =>`
# renames it; `name::T =>` also states the message/request/action type ON THE PORT; `::T =>` states just
# the type and keeps the default name (the handler's) — so an inline handler is typed without the macro
# reading its signature. The type may instead ride the handler (`handler::T`); `_parse_handler_port`
# enforces it appears on exactly one side. No `=>` ⇒ no override, no port type.
function _parse_bound_handler(x, who)
    if x isa Expr && x.head === :call && length(x.args) == 3 && x.args[1] === :(=>)
        lhs = x.args[2]
        lhs isa Symbol && return lhs, nothing, x.args[3]                        # name =>
        if lhs isa Expr && lhs.head === :(::)
            length(lhs.args) == 1 && return nothing, lhs.args[1], x.args[3]     # ::T =>  (default the name)
            length(lhs.args) == 2 && lhs.args[1] isa Symbol &&
                return lhs.args[1]::Symbol, lhs.args[2], x.args[3]              # name::T =>
            lhs.args[1] isa AbstractString && error("$(who): a leading wire binds to the type — " *
                "`\"wire\" ::T` parses as `(\"wire\")::T`. Name the port (`\"wire\" name::T => …`) or put " *
                "the wire in a trailing `on \"wire\"`.")
        end
        error("$(who): a port override is `name =>`, `name::Type =>`, or `::Type =>`, got `$(lhs)` in `$(x)`")
    end
    return nothing, nothing, x
end

# ⟨publishes⟩ / ⟨uses⟩ — a `name::T` decl plus an optional TRAILING wire; no handler. The decl directives
# name the port from the decl, so the wire is trailing-only (a leading string is a handler-directive form).
function _parse_decl_port(mname, rest, M, who)
    isempty(rest) && error("$(who): `$(who) name::T [on \"wire\"]`")
    rest[1] isa AbstractString && error("$(who): the wire goes in a trailing `on \"wire\"`, not a leading " *
        "string — write `$(who) name::T on $(repr(String(rest[1])))`")
    wire, mid = _parse_wires(rest, who; allow_lead = false)
    length(mid) == 1 || error("$(who): expected `name::T [on \"wire\"]`, got `$(Tuple(rest))`")
    name, T = _parse_port_decl(mid[1], who)
    comb = mname === Symbol("@publishes") ? :publishes : :uses
    return :( $(GlobalRef(M, comb))($(QuoteNode(name)), $T; on = $wire) )
end

# A handler reference is a bare name (`f`) or a qualified path (`Mod.f`) — the value combinators accept
# either, and @schema's purpose includes wiring externally-defined handlers. `_ref_name` is the port name
# such a reference contributes (its final path component).
_is_ref(x) = x isa Symbol || (x isa Expr && x.head === :. && length(x.args) == 2)
_ref_name(s::Symbol) = s
_ref_name(e::Expr) = e.args[2] isa QuoteNode ? e.args[2].value::Symbol :
    error("@component/@schema: could not read a port name from the handler reference `$(e)`")

# ⟨hears⟩ / ⟨serves⟩ / ⟨runs⟩ — a wired, optionally-renamed handler. `kind ∈ (:hears,:serves,:runs)`.
# `legacy_author` routes @service/@action's authoring inline-def. Returns (combinator-call, emits).
function _parse_handler_port(kind::Symbol, rest, base, M, src, who)
    isempty(rest) && error("$(who): needs a handler — `$(who) [name[::T] =>] handler[::T] [on \"wire\"]`")
    wire, mid = _parse_wires(rest, who)
    length(mid) == 1 ||
        error("$(who): expected `[\"wire\"] [name[::T] =>] handler[::T] [on \"wire\"]`, got `$(Tuple(rest))`")
    name_override, port_type, spec = _parse_bound_handler(mid[1], who)
    comb = kind === :hears ? :hears : kind === :serves ? :serves : :runs
    emits = Any[]
    if _is_method_def(spec)                                  # ⟨inline-def⟩ — emitted verbatim
        fname = _component_defname(spec)
        name  = name_override === nothing ? fname : name_override
        if kind === :hears
            if port_type === nothing                         # no port-stated type — read `msg::T` off the signature
                call = _peel_to_call(spec.args[1])
                length(call.args) >= 4 ||
                    error("$(who): an inline subscription handler needs a typed message argument — " *
                          "`function $(fname)(node, m::$(base), msg::T) … end`, or state the type on the port " *
                          "(`$(who) $(name)::T => function …`), got `$(call)`")
                msgarg = call.args[4]
                (msgarg isa Expr && msgarg.head === :(::)) ||
                    error("$(who): the message argument needs a type, `msg::T` — or state it on the port " *
                          "(`$(who) $(name)::T => …`), got `$(msgarg)`")
                T = msgarg.args[2]
            else                                             # port-stated type — emit verbatim, never peek the signature
                T = port_type
            end
            push!(emits, spec)
            return :( $(GlobalRef(M, :hears))($(QuoteNode(name)), $T, $fname; on = $wire) ), emits
        else                                                # :serves/:runs — author the type+impl inline
            port_type === nothing ||
                error("$(who): an inline $(kind === :serves ? "service" : "action") derives its type from the " *
                      "definition — drop the `::$(port_type)` on the port (state a type only with a handler reference)")
            authoring = kind === :serves ? Symbol("@service") : Symbol("@action")
            margs = wire === nothing ? Any[spec] : Any[wire, spec]   # wire rides the authoring macro
            push!(emits, Expr(:macrocall, authoring, src, margs...))
            return :( $(GlobalRef(M, comb))($(QuoteNode(name)), $fname) ), emits   # trait carries the wire
        end
    elseif spec isa Expr && spec.head === :(::)             # ⟨typed reference⟩  handler::T  (handler = f or Mod.f)
        _is_ref(spec.args[1]) ||
            error("$(who): a typed handler reference must be `handler::T` (`handler` a name or `Mod.name`), got `$(spec)`")
        port_type === nothing ||
            error("$(who): the type is given twice — on the port (`::$(port_type)`) and the handler " *
                  "(`::$(spec.args[2])`); state it once")
        h, T = spec.args[1], spec.args[2]
        name = name_override === nothing ? _ref_name(h) : name_override
        return :( $(GlobalRef(M, comb))($(QuoteNode(name)), $T, $h; on = $wire) ), emits
    elseif _is_ref(spec)                                    # ⟨reference⟩ — a trait handler, or port-typed (f or Mod.f)
        if port_type !== nothing                             # `[name]::T => handler` — the type rides the port
            name = name_override === nothing ? _ref_name(spec) : name_override
            return :( $(GlobalRef(M, comb))($(QuoteNode(name)), $port_type, $spec; on = $wire) ), emits
        end
        kind === :hears && error("$(who): a subscription needs a message type — write `$(spec)::MsgType`, " *
            "or state it on the port (`$(who) name::MsgType => $(spec)`)")
        name = name_override === nothing ? _ref_name(spec) : name_override
        return :( $(GlobalRef(M, comb))($(QuoteNode(name)), $spec; on = $wire) ), emits
    else
        error("$(who): could not parse the handler `$(spec)` — expected `name::T`, a handler reference, " *
              "or an inline `function f(node, m::$(base), …) … end`")
    end
end

# ⟨every⟩ — canonical `[name =>] handler at rate`, or legacy `rate function…end` (no `at`).
function _parse_every(rest, base, M, who)
    isempty(rest) && error("$(who): `@every [name =>] handler at rate`  (rate = Hz or `:param`)")
    atpos = findfirst(==(:at), rest)
    emits = Any[]
    if atpos !== nothing
        atpos == lastindex(rest) - 1 ||
            error("$(who): `at` must be followed by exactly one rate, got `$(Tuple(rest[atpos+1:end]))`")
        before = rest[1:atpos-1]
        rate   = _parse_rate(rest[atpos+1], who)
        length(before) == 1 || error("$(who): expected `[name =>] handler at rate`, got `$(Tuple(rest))`")
        name_override, port_type, spec = _parse_bound_handler(before[1], who)
        port_type === nothing || error("$(who): a timer carries no message type — drop the `::$(port_type)` " *
            "(write `@every [name =>] handler at rate`)")
        if _is_method_def(spec)
            push!(emits, spec)
            handler = _component_defname(spec); defname = handler
        elseif _is_ref(spec)
            handler = spec; defname = _ref_name(spec)
        else
            error("$(who): a timer handler is a reference (a name or `Mod.name`) or an inline " *
                  "`function f(node, m::$(base)) … end`, got `$(spec)`")
        end
        name = name_override === nothing ? defname : name_override
        return :( $(GlobalRef(M, :every))($(QuoteNode(name)), $rate, $handler) ), emits
    else                                                    # legacy: rate, then an inline def
        length(rest) == 2 || error("$(who): the legacy form is `@every rate function f(node, m) … end`; " *
            "the canonical form is `@every [name =>] handler at rate` (an inline def types its member `m::$(base)`)")
        rate = _parse_rate(rest[1], who)
        _is_method_def(rest[2]) ||
            error("$(who): legacy `@every rate function … end` needs an inline handler; " *
                  "to wire a reference use `@every handler at rate`")
        push!(emits, rest[2])
        fname = _component_defname(rest[2])
        return :( $(GlobalRef(M, :every))($(QuoteNode(fname)), $rate, $fname) ), emits
    end
end

# Parse one ⟨port-dir⟩ into (combinator-call-expr, emits::Vector). `base` names the component for member-arg validation; `M` is ROSNode.
function parse_port_directive(ln::Expr, base, M)
    mname = ln.args[1]::Symbol
    who   = String(mname)
    src   = ln.args[2]                                       # the directive's LineNumberNode
    rest  = _dargs(ln)
    if mname === Symbol("@publishes") || mname === Symbol("@uses")
        return _parse_decl_port(mname, rest, M, who), Any[]
    elseif mname === Symbol("@hears")
        return _parse_handler_port(:hears, rest, base, M, src, who)
    elseif mname === Symbol("@serves") || mname === Symbol("@service")
        return _parse_handler_port(:serves, rest, base, M, src, who)
    elseif mname === Symbol("@runs") || mname === Symbol("@action")
        return _parse_handler_port(:runs, rest, base, M, src, who)
    elseif mname === Symbol("@every")
        return _parse_every(rest, base, M, who)
    end
    error("$(who): not a port directive")
end

"""
    @component mutable struct Name{Name} <: Component{Name}
        field::T = default                       # private state; inline default feeds the zero-arg ctor
        @param  rate::Int64 = 5 ∈ 1..50          # → an emitted `@parameters struct NameParams`
        @provides Iface                          # interface(s) this component provides
        @requires src::Sensor                    # inject a sibling (a component type or an @interface)
        @publishes out::T on "~/out"             # a publisher port (no handler)
        @hears  function on_msg(node, m::Name, msg::T) … end   # subscription port + handler (bundled)
        @every  :rate function tick(node, m::Name) … end       # timer port + handler (rate = Hz or :param)
        @service "~/srv" function srv(node, m::Name, x::X)::@NamedTuple{…} … end   # inline service authoring
        configure(node, m::Name) = …             # lifecycle hooks live here too (re-coupled on a struct edit)
    end

Author a single component in one expression. The block body is a sequence of directives over a grammar
shared with [`@schema`](@ref); each **port directive** lowers to one value combinator, and `@component`
additionally emits the `mutable struct` (`{Name}` injected if you write just `Name`), a zero-arg
`Name{Name}()` ctor from the inline field defaults, an `@parameters struct NameParams` from the `@param`
fields, the handler/hook defs verbatim, and `member_schema(::Type{Name}) = component(Name, …)` on the bare
base. Editing anything re-runs the whole block, so the struct and every method re-key together.

# Grammar

A `@component` block lowers to the single `component(…)` call the raw API writes by hand. These are equivalent:

```julia
@component mutable struct Sensor{Name} <: Component{Name}
    level::Float64 = 100.0
    @param     rate::Int64 = 5 ∈ 1..50
    @publishes telemetry::Telemetry on "~/telemetry"
    @hears     odom::Odometry => on_odom
    @every     tick at :rate
    @provides  BatterySource
end
# ≡  the raw value API:
mutable struct Sensor{Name} <: Component{Name}; level::Float64; end
Sensor{Name}() where {Name} = Sensor{Name}(100.0)
@parameters struct SensorParams; rate::Int64 = 5 ∈ 1..50; end
member_schema(::Type{Sensor}) = component(Sensor, SensorParams,
    publishes(:telemetry, Telemetry; on = "~/telemetry"),
    hears(:odom, Odometry, on_odom),
    every(:tick, :rate, tick);
    provides = (BatterySource,))
```

Each block line is a directive, a plain field, or a method. What each lowers to (`[…]` optional):

| Block line | Lowers to |
|---|---|
| `@publishes name::T [on "wire"]` | `publishes(:name, T; on)` — a publisher |
| `@hears [name[::T] =>] handler[::T] [on "wire"]` | `hears(:name, T, handler; on)` — a subscription (`T` on the port or the handler) |
| `@serves [name[::T] =>] handler[::T] [on "wire"]` | `serves(:name, [Req,] handler; on)` — a service |
| `@runs [name[::T] =>] exec[::T] [on "wire"]` | `runs(:name, [Action,] exec; on)` — an action server |
| `@every [name =>] handler at rate` | `every(:name, rate, handler)` — a timer |
| `@uses name::Marker [on "wire"]` | `uses(:name, Marker; on)` — a persistent client |
| `@param ["doc"] x::T = d [∈ range]` | a field of `@parameters struct NameParams` |
| `@provides I [I…]` | `provides = (I, …)` |
| `@requires field::Marker [, …]` | `requires = (Marker, …)`, plus the type parameter, field, and `construct` that inject the sibling |
| `x::T = default` | a private state field (feeds the zero-arg ctor) |
| `configure(node, m::Name) = …` | a lifecycle hook, dispatched on the component |

`@param` takes the full [`@parameters`](@ref) field grammar — a default, an `∈` constraint, and `|>` coercion.

**Handlers** (`@hears`/`@serves`/`@runs`/`@every`) bind a handler one of three ways:
- a **reference** — a name in scope, e.g. `@serves safe` (an `@service`/`@action` handler's trait carries its type and wire);
- a **typed reference** — `handler::T` naming the message / request / action type, e.g. `@hears on_odom::Odometry`;
- an **inline definition** — `function f(node, m::Name, …) … end` written in place, re-coupled to the struct on an edit.

**The type.** `@hears` needs a message type (`@serves`/`@runs` a request/action type, unless authored
inline). State it on the **handler** (`handler::T`) or on the **port** (`name::T =>`, left of `=>`) — once,
either side. The port form types an inline `@hears` handler *without* the macro reading its signature:
`@hears odom::Odometry => function on_odom(node, m, msg) … end` subscribes `:odom` to `Odometry` and emits
the handler verbatim. Drop the name (`::T =>`) to keep the default port name (the handler's) —
`@hears ::Odometry => function on_odom(…) … end` gives port `:on_odom`. An inline `@hears` with no stated
type falls back to reading `msg::T` off the signature; an inline `@serves`/`@runs` derives its type from
the definition (state none on the port).

**Names & wires.** A port's name defaults to its handler's name; prefix `name =>` (or `name::T =>`) to
override it (`@serves status => safe`). The wire — the topic, or service/action name — defaults to the port
name; override it with a trailing `on "wire"`, or — handy when an inline handler would bury a trailing
clause after `end` — a **leading `"wire"` string** (`@hears "~/odom" function on_odom(…) … end`). Give the
wire at most once; a timer addresses no wire.

**Rate.** `@every … at rate` takes a frequency in Hz (a `Real`), or a parameter `:name` that binds the
period live to that parameter (the legacy spelling is `@every rate function … end`).

**Member argument.** Type a reaction's second argument with the component — `m::Name`, any name — to
dispatch on it and keep `entities`/`parameters` type-stable. The macro emits the handler verbatim, so a
bare `m` is an ordinary `::Any` catch-all it leaves untouched. A non-node-first interface impl (e.g.
`battery(s::Sensor)`) is defined OUTSIDE the block.

# Formal grammar

The authoritative EBNF (`→` names the combinator each port directive lowers to; `[…]` optional, `{…}`
zero-or-more). The port sublanguage (⟨port-dir⟩) is shared with [`@schema`](@ref).

```
⟨component⟩  ::= "@component" "mutable" "struct" ⟨head⟩ { ⟨field⟩ | ⟨directive⟩ | ⟨method-def⟩ } "end"
⟨head⟩       ::= Name                                     macro injects {Name} <: Component{Name}
              | Name{Name} <: Component{Name}             written explicitly (any spelling of Name)
⟨field⟩      ::= Symbol "::" ⟨type⟩ "=" ⟨expr⟩            private state + its zero-arg-ctor default
⟨directive⟩  ::= ⟨port-dir⟩ | ⟨bind-dir⟩

⟨port-dir⟩   ::= ⟨publishes⟩ | ⟨hears⟩ | ⟨serves⟩ | ⟨runs⟩ | ⟨every⟩ | ⟨uses⟩
⟨publishes⟩  ::= "@publishes" ⟨decl⟩ ["on" String]            → publishes(:n, T; on)
⟨uses⟩       ::= "@uses"      ⟨decl⟩ ["on" String]            → uses(:n, Marker; on)
⟨hears⟩      ::= "@hears"  [String] ⟨bound⟩ ["on" String]     → hears(:n, T, h; on)
⟨serves⟩     ::= "@serves" [String] ⟨bound⟩ ["on" String]    → serves(:n, [Req,] h; on)
⟨runs⟩       ::= "@runs"   [String] ⟨bound⟩ ["on" String]    → runs(:n, [Action,] exec; on)
⟨every⟩      ::= "@every" ⟨bound⟩ "at" ⟨rate⟩                 → every(:n, rate, h)
              | "@every" ⟨rate⟩ ⟨inline⟩                     (legacy, equivalent)
⟨bound⟩      ::= [⟨port⟩ "=>"] ⟨handler⟩                      ⟨port⟩ overrides the name; `::T` types the port
⟨port⟩       ::= Symbol | [Symbol] "::" ⟨type⟩                a port name and/or its type (drop the name to default it)
⟨handler⟩    ::= Symbol                                       a reference (a trait carries its type/wire)
              | Symbol "::" ⟨type⟩                            a typed reference: msg / request / action type
              | ⟨inline⟩                                      a reaction, or an @service/@action def
⟨inline⟩     ::= "function" ⟨call⟩ ⟨body⟩ "end" | ⟨call⟩ "=" ⟨expr⟩
⟨decl⟩       ::= Symbol "::" ⟨type⟩
⟨rate⟩       ::= Real (Hz) | ":" Symbol (a parameter, bound live)

⟨bind-dir⟩   ::= ⟨param⟩ | ⟨provides⟩ | ⟨requires⟩
⟨param⟩      ::= "@param" [String] Symbol "::" ⟨type⟩ "=" ⟨expr⟩ ["∈" ⟨expr⟩] ["|>" ⟨expr⟩]
                                                             → a field of `@parameters struct NameParams`
⟨provides⟩   ::= "@provides" ⟨type⟩ { ⟨type⟩ }               → provides = (…,)
⟨requires⟩   ::= "@requires" Symbol "::" ⟨marker⟩ { "," … }  → requires = (…,) + a hidden type param, field, `construct`
```

Side conditions: the message/request/action type is stated once — on the port (`name::T =>`) or the
handler (`handler::T`); a timer takes none, and an inline `@serves`/`@runs` derives it from the definition.
The wire is given at most once, a leading `String` or a trailing `on "wire"`. `@service`/`@action` are
legacy aliases of `@serves`/`@runs` with an inline definition.

`@component` is sugar over the value API — the raw `component`/`node` combinators remain the primitive
for dynamic composition. It covers the common case (ports, `@param`, `@provides`, `@requires field::Marker`
DI consumers); to author a `member_schema` over a hand-written struct (a parametric DI consumer, an
externally-defined handler, a custom ctor) with the same directives, use [`@schema`](@ref). Use it from a
module that does `using ROSNode`.
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
            elseif is_port_directive(mname)
                # Shared port sublanguage — @publishes/@hears/@serves/@runs/@every/@uses (+ @service/@action
                # legacy authoring). `@component` lifts any inline handler into a module-level def (emits) and
                # the directive lowers to its value combinator; the rest of the block stays struct-specific.
                call, em = parse_port_directive(ln, base, M)
                append!(emits, em); push!(ports, call)
            else
                error("@component: unknown directive `$(mname)` in the body")
            end
        elseif _is_method_def(ln)
            # a standalone method def, emitted verbatim — node-first `(node, m::Base, …)`, you type the member.
            # A lifecycle-hook name (configure/activate/…) extends the ROSNode generic (qualified via GlobalRef);
            # any other node-first def stays a module-local function (e.g. a named reaction handler). A
            # non-node-first helper (e.g. an interface impl `f(x)`) belongs OUTSIDE the block.
            push!(emits, _component_qualify_hook!(ln, M))
        elseif ln isa Expr && ln.head === :(::) && ln.args[1] isa Symbol
            push!(fields, ln); push!(nodefault, ln.args[1])   # struct field, NO default
        elseif ln isa Expr && ln.head === :(=) && ln.args[1] isa Expr && ln.args[1].head === :(::) && ln.args[1].args[1] isa Symbol
            push!(fields, ln.args[1]); push!(defaults, ln.args[2])   # struct field WITH default
        else
            error("@component: unexpected body line `$(ln)` — expected a field, a directive, or a reaction/hook def")
        end
    end

    isempty(nodefault) || error("@component: field(s) $(nodefault) need an inline default (e.g. `x::T = …`) " *
        (isempty(reqs) ?
            "so the zero-arg `$(base){…}()` ctor can build the component" :
            "so the injected `construct(::Type{$(base)}, node, ::Val{Name}, deps…)` can build the component " *
            "(it threads the field defaults, then the deps)") *
        " (a value set later still needs a default placeholder)")

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

export @schema

"""
    @schema Base [Params] begin
        @publishes out::T on "~/out"             # a publisher port
        @hears     on_odom::Odometry             # a subscription bound to the in-scope `on_odom`
        @hears     odom => on_odom::Odometry      # …under a different port name
        @serves    safe                          # a pre-authored @service handler, by reference
        @runs      countup                       # a pre-authored @action handler, by reference
        @every     tick at :rate                 # a timer bound to the `rate` parameter
        @uses      cli::AddTwoInts_Request        # a persistent service client
        @provides  Iface                         # interface(s) this component provides
        @requires  Sensor                        # DI evidence — the marker type(s) it depends on
        @ctor      make_component                 # external injecting constructor (optional)
    end

Author a component's [`member_schema`](@ref) declaratively — the same port directives as [`@component`](@ref),
lowered to the `publishes`/`hears`/`serves`/`runs`/`every`/`uses` value combinators. Unlike `@component`,
`@schema` defines **no struct**: you write the `mutable struct Base{Name} <: Component{Name} … end` and the
lifecycle hooks as ordinary definitions, and `@schema` wires the ports to your handlers (referenced by
name, or written inline in a directive). That decoupling is what keeps the representation power the raw
combinators have — parametric state, externally defined or shared handlers, DI consumers
(`@requires`/`@ctor`), descriptor reuse — that `@component`'s all-in-one form gives up.

Expands to `member_schema(::Type{Base}) = component(Base, [Params,] ports…; provides, requires, ctor)`
(plus any inline handler defs). It emits nothing else, so it composes with a hand-written struct:

```julia
mutable struct Guard{Name, B} <: Component{Name}; battery_src::B; end
@parameters struct GuardParams; min_battery::Float64 = 20.0; end
@service "~/safe_to_fly" function safe(node, g::Guard, target::Float64)::@NamedTuple{ok::Bool} … end

@schema Guard GuardParams begin
    @serves   safe
    @requires BatterySource    # the default ctor injects it: Guard{Name, typeof(src)}(src)
end
```

# Grammar

The header is `@schema Base [Params] begin … end`, where `Params` is an existing [`@parameters`](@ref)
struct type. The **port directives** inside (`@publishes`/`@hears`/`@serves`/`@runs`/`@every`/`@uses`,
their handler/name/wire forms, and the combinator each lowers to) are identical to [`@component`](@ref)'s —
see its docstring. Where `@schema` parts ways is the binding directives:

| Directive | Lowers to |
|---|---|
| `@param ["doc"] x::T = d [∈ range]` | a field of `@parameters struct BaseParams` (when no `Params` is given) |
| `@provides I [I…]` | `provides = (I, …)` |
| `@requires Marker [, …]` | `requires = (Marker, …)` — bare marker types, DI evidence only |
| `@ctor f` | `ctor = f` — an external constructor (optional; see notes) |

`@param` takes the full [`@parameters`](@ref) field grammar (a default, an `∈` constraint, `|>` coercion).

`@requires` lists **bare marker types** (`@requires Sensor`, not `field::Marker`) — the parametric struct,
its injected field, and its constructor are yours to write. Assembly builds the component by calling
`Base(node, ::Val{Name}, deps...)`: embed an inner constructor of that shape and it is found automatically,
or pass `@ctor f` to wire an external callable `f(node, ::Val{Name}, deps...) where {Name} -> Base{Name, …}`
([`construct`](@ref) is the full contract). That decoupling buys back the representation power `@component`'s
all-in-one form gives up.

Notes:
- **Params.** `@schema Base P` references the existing [`@parameters`](@ref) struct `P`; `@schema Base`
  with `@param` lines authors `BaseParams` inline. The two are mutually exclusive.
- **`@requires` / `@ctor`.** For the holder shape — the free type parameters past `Name` are exactly the
  injected deps, in order — the default builds `Base{Name, typeof.(deps)…}(deps…)`, so no constructor is
  needed. For a custom shape, embed an inner constructor `Base(node, ::Val{Name}, deps…)` (found
  automatically) or a [`construct`](@ref) method; **`@ctor` is only for an external callable, or to override
  either.**
- **Member argument.** An inline handler is emitted verbatim — type its second argument with the component
  (`function f(node, m::Base, …) … end`, any name) to dispatch on it and keep the accessors type-stable; a
  bare `m` is an ordinary `::Any` catch-all the macro leaves untouched.

# Formal grammar

The authoritative EBNF for `@schema`'s framing; the port sublanguage (⟨port-dir⟩) is shared with
[`@component`](@ref) — see its grammar.

```
⟨schema⟩      ::= "@schema" Base [⟨params-type⟩] "begin" { ⟨port-dir⟩ | ⟨bind-dir⟩ } "end"
⟨params-type⟩ ::= Symbol | ⟨dotted⟩                         an existing `@parameters` struct type
⟨bind-dir⟩    ::= ⟨param⟩ | ⟨provides⟩ | ⟨requires⟩ | ⟨ctor⟩
⟨param⟩       ::= "@param" [String] Symbol "::" ⟨type⟩ "=" ⟨expr⟩ ["∈" ⟨expr⟩] ["|>" ⟨expr⟩]
                                                            → a field of `@parameters struct BaseParams`
⟨provides⟩    ::= "@provides" ⟨type⟩ { ⟨type⟩ }             → provides = (…,)
⟨requires⟩    ::= "@requires" ⟨marker⟩ { "," ⟨marker⟩ }     → requires = (…,)   (bare markers, DI evidence)
⟨ctor⟩        ::= "@ctor" ⟨expr⟩                            → ctor = f, an f(node, ::Val{Name}, deps...)
```

The `@ctor` callable's full signature is `f(node, ::Val{Name}, deps...) where {Name} -> Base{Name, …}` —
one `dep` per `@requires`, in order; see [`construct`](@ref). It is usually elided: embed an inner
constructor of that shape on the struct and assembly finds it.

Use it from a module that does `using ROSNode`.
"""
macro schema(args...)
    length(args) >= 2 || error("@schema: `@schema Base [Params] begin … end`")
    block = last(args)
    (block isa Expr && block.head === :block) ||
        error("@schema: the body must be a `begin … end` block, got `$(block)`")
    length(args) <= 3 || error("@schema: too many arguments — `@schema Base [Params] begin … end`")
    base = args[1]
    base isa Symbol ||
        error("@schema: the first argument is the component state type (a bare name), got `$(base)`")
    ptype = length(args) == 3 ? args[2] : nothing
    ptype === nothing || ptype isa Symbol || (ptype isa Expr && ptype.head === :curly) ||
        error("@schema: the second argument is the parameters struct TYPE (a `@parameters` name), got " *
              "`$(ptype)` — for inline parameters drop it and declare them with `@param` lines")
    M = @__MODULE__

    ports = Any[]; emits = Any[]; provides = Any[]; requires = Any[]; params = Any[]; ctorx = nothing

    for ln in block.args
        ln isa LineNumberNode && continue
        (ln isa Expr && ln.head === :macrocall) ||
            error("@schema: the block holds directives only — define the struct, reactions, and lifecycle " *
                  "hooks separately. Got `$(ln)`")
        mname = ln.args[1]
        rest  = filter(a -> !(a isa LineNumberNode), ln.args[2:end])
        if mname === Symbol("@param")
            ptype === nothing || error("@schema: `$(base)` was given an explicit parameters struct " *
                "`$(ptype)`; remove the `@param` line(s), or drop `$(ptype)` and declare parameters with `@param`")
            isempty(rest) && error("@schema: `@param [\"doc\"] field::T = default [∈ …] [|> …]`")
            length(rest) > 2 && error("@schema: `@param` takes an optional leading doc string then ONE " *
                "`field::T = default [∈ …] [|> …]` statement, got $(length(rest)) parts in `$(ln)`")
            if length(rest) == 2
                rest[1] isa AbstractString ||
                    error("@schema: `@param`'s leading doc must be a string literal, got `$(rest[1])`")
                push!(params, rest[1])
            end
            push!(params, last(rest))
        elseif mname === Symbol("@provides")
            isempty(rest) && error("@schema: `@provides Iface [Iface2 …]` needs at least one interface")
            for r in rest
                (r isa Expr && r.head === :tuple) ? append!(provides, r.args) : push!(provides, r)
            end
        elseif mname === Symbol("@requires")
            isempty(rest) && error("@schema: `@requires Marker [Marker2 …]` — the @interface/component " *
                "TYPE(s) this component depends on. Author the injected field + constructor on your struct, " *
                "and name the constructor with `@ctor`.")
            for r in rest
                entries = (r isa Expr && r.head === :tuple) ? r.args : (r,)   # flatten `(A, B)` AND validate each
                for e in entries
                    (e isa Expr && e.head === :(::)) &&
                        error("@schema: `@requires` takes bare marker TYPE(s) here (e.g. `@requires Sensor`), not " *
                            "`field::Marker` (got `$(e)`) — `@schema` does not author the struct, so declare the " *
                            "injected field on your `mutable struct` and pass the constructor with `@ctor`. " *
                            "(`field::Marker` is `@component`'s form.)")
                    push!(requires, e)
                end
            end
        elseif mname === Symbol("@ctor")
            length(rest) == 1 || error("@schema: `@ctor f` — name the injecting constructor, a callable " *
                "`f(node, ::Val{Name}, deps...) where {Name} -> $(base){Name, …}` (one dep per `@requires`)")
            ctorx === nothing || error("@schema: `@ctor` given more than once")
            ctorx = rest[1]
        elseif is_port_directive(mname)
            call, em = parse_port_directive(ln, base, M)
            append!(emits, em); push!(ports, call)
        else
            error("@schema: unknown directive `$(mname)` — expected a port directive " *
                  "(@publishes/@hears/@serves/@runs/@every/@uses) or @param/@provides/@requires/@ctor")
        end
    end

    paramsblock = isempty(params) ? nothing :
        Expr(:macrocall, Symbol("@parameters"), __source__,
             Expr(:struct, false, _params_name(base), Expr(:block, params...)))
    Pargs = ptype !== nothing ? (ptype,) : (isempty(params) ? () : (_params_name(base),))
    schemacall = Expr(:call, GlobalRef(M, :component), base, Pargs..., ports...)
    kws = Expr(:parameters)
    isempty(provides) || push!(kws.args, Expr(:kw, :provides, Expr(:tuple, provides...)))
    isempty(requires) || push!(kws.args, Expr(:kw, :requires, Expr(:tuple, requires...)))
    ctorx === nothing || push!(kws.args, Expr(:kw, :ctor, ctorx))
    isempty(kws.args) || insert!(schemacall.args, 2, kws)
    memberschema = Expr(:(=), Expr(:call, GlobalRef(M, :member_schema), :(::Type{$base})), schemacall)

    out = Expr(:block,
               (paramsblock === nothing ? () : (paramsblock,))...,
               emits...,
               memberschema,
               base)
    return esc(out)
end
