# Components — a node is a collection of mixins (DESIGN-COMPONENTS.md). A `@mixin` is a
# state-only mutable struct; `@param`/`@publishes`/`@uses` declare HAS ports;
# `@every`/`@hears`/`@serves`/`@runs` author DOES reactions; lifecycle hooks are plain
# methods dispatched on the mixin; `@node` assembles members and `run` instantiates.
#
# Phase 1 (DESIGN §11.1): `@mixin` + the HAS/DOES surface (publisher / subscription /
# timer), lifecycle methods, single-member `@node`, and `run` (standalone). Deferred to
# later increments: `@serves`/`@runs`, multi-mixin assembly + the DI protocol (§4),
# member-name namespacing (§4.4), the typed `P_Foo`/`E_Foo` codegen + §10 parameter
# services (§3.5), `container`/`LoadNode` (§7), and `retime!`/drift detection (§3.4).
#
# The macros accumulate specs into per-type registries at load (the authored-types
# deferred pattern, typesupport/authored.jl); `run` reads them to materialize a node.

abstract type Component end

export Component, @mixin, @param, @publishes, @uses, @every, @hears, @serves, @runs,
       @interface, @provides, provides, requires, construct,
       entities, parameters, configure, activate, deactivate, cleanup, on_error

# ── spec registries ───────────────────────────────────────────────────────────
# One `MixinSpec` per `@mixin` type, populated by the declaration macros.

"A declared parameter of a mixin (§3.1) — the §10 grammar's parsed form."
struct ParamSpec
    name::Symbol
    type::Any            # the field type (a Type at runtime)
    default::Any
    constraint::Any
    readonly::Bool
    doc::String
end

"""
A declared port of a mixin (§3). `kind` is `:publisher`/`:client` (HAS) or
`:subscription`/`:service`/`:action`/`:timer` (DOES). `wire` overrides the topic/name
(else the identifier is used); `reaction` is the dispatched handler (DOES) or `nothing`
(HAS); `extra` carries kind-specific data (e.g. a timer's `rate`).
"""
struct PortSpec
    name::Symbol
    kind::Symbol
    msgtype::Any
    wire::Union{String, Nothing}
    reaction::Any
    extra::NamedTuple
end

mutable struct MixinSpec
    const params::Vector{ParamSpec}
    const ports::Vector{PortSpec}
end
MixinSpec() = MixinSpec(ParamSpec[], PortSpec[])

const _MIXINS = IdDict{Type, MixinSpec}()
const _MIXINS_LOCK = ReentrantLock()

_register_mixin!(::Type{M}) where {M} =
    @lock _MIXINS_LOCK (get!(MixinSpec, _MIXINS, M); nothing)
mixin_spec(::Type{M}) where {M} = @lock _MIXINS_LOCK _MIXINS[M]
ismixin(::Type{M}) where {M} = @lock _MIXINS_LOCK haskey(_MIXINS, M)
ismixin(@nospecialize(_)) = false

_add_param!(::Type{M}, p::ParamSpec) where {M} =
    @lock _MIXINS_LOCK (push!(get!(MixinSpec, _MIXINS, M).params, p); nothing)
_add_port!(::Type{M}, p::PortSpec) where {M} =
    @lock _MIXINS_LOCK (push!(get!(MixinSpec, _MIXINS, M).ports, p); nothing)

# ── the per-instance runtime binding (the hidden `__rt__`, §3.5/§5) ─────────────
# Each constructed mixin carries a back-ref to its node-core + its materialised
# surface, so `entities(m)`/`parameters(m)` resolve against it without a field on the
# user's struct beyond the one hidden slot.

mutable struct MixinRuntime
    const corenode::Any              # the ComponentNode (below)
    const member::Symbol             # member name within the node
    pserver::Any                     # the member's ParameterServer{P_M}
    ports::Any                       # NamedTuple of materialised port handles (set at materialize)
    timers::Vector{Any}              # paused timers, started at the activate step
end

# ── @mixin ──────────────────────────────────────────────────────────────────────

"""
    @mixin struct M
        field::T = default
        …
    end

Declare a mixin (DESIGN §2): a **mutable, state-only** struct subtyping
[`Component`](@ref) (so reactions can update fields), with a hidden runtime slot the
framework fills at construction. Entities and parameters are *not* fields — they
attach to the type with `@param`/`@publishes`/`@every`/`@hears` and are reached
reflectively via [`parameters`](@ref) / [`entities`](@ref). Every field needs a
default (the mixin is built with `construct`, default `M()`).
"""
macro mixin(structexpr)
    (structexpr isa Expr && structexpr.head === :struct) ||
        error("@mixin expects `struct Name … end`, got $(structexpr)")
    name = _mixin_name(structexpr.args[2])
    body = structexpr.args[3]
    # Inject the hidden runtime slot, force mutable + a kwdef ctor, subtype Component.
    newbody = Expr(:block, body.args..., :(__rt__::Any = nothing))
    newstruct = Expr(:struct, true, Expr(:(<:), name, :Component), newbody)
    return esc(quote
        Base.@kwdef $newstruct
        $(GlobalRef(@__MODULE__, :_register_mixin!))($name)
        # Register the mixin as a loadable kind too (mixin-as-node, §4.3/§7), so a
        # container's `load_node` can instantiate it by name.
        $(GlobalRef(@__MODULE__, :register_node_kind!))(string(nameof($name)), $name)
        # Generate the parameter schemas at module load (so `P_M`'s ctor exists before
        # any frame uses it — the authored-types deferred pattern). The first `@mixin`
        # in a module installs the hook; later ones see it defined and skip.
        if !isdefined(@__MODULE__, :__init__)
            function __init__()
                $(GlobalRef(@__MODULE__, :_finalize_mixins!))(@__MODULE__)
            end
        end
        $name
    end)
end

_mixin_name(n::Symbol) = n
function _mixin_name(n::Expr)
    n.head === :(<:) && return _mixin_name(n.args[1])
    n.head === :curly && error("@mixin: parametric mixins are unsupported")
    error("@mixin: could not read the struct name from $(n)")
end

# ── @param (§3.1) ─────────────────────────────────────────────────────────────
# Reuses the §10 `@parameters` field grammar via `_parse_param_field`:
#   @param M  "doc"  name::T = default [∈ lo..hi | ∈ (a,b)] [|> readonly]

"""
    @param M  [\"doc\"]  field::T = default  [∈ lo..hi | ∈ (choices…)]  [|> readonly]

Attach a parameter to mixin `M` (§3.1) — the §10 `@parameters` field grammar. Read it
live in a reaction with `parameters(m).field`.
"""
macro param(M, args...)
    (doc, fieldstmt) = length(args) == 2 ? (args[1], args[2]) :
                       length(args) == 1 ? ("", args[1]) :
                       error("@param: expected `@param M [\"doc\"] field::T = default …`")
    doc isa AbstractString || (doc = "")
    fname, ftype, default, constraint, ro = _parse_param_field(fieldstmt)
    spec = :( $(ParamSpec)($(QuoteNode(fname)), $(esc(ftype)), $(esc(default)),
                           $(esc(constraint === nothing ? :nothing : constraint)),
                           $(ro), $(String(doc))) )
    return :( $(_add_param!)($(esc(M)), $spec) )
end

# ── HAS ports: @publishes / @uses (§3.1) ────────────────────────────────────────

"""
    @publishes M name :: T  [on \"topic\"]

Attach an output port to mixin `M` (§3.1). The topic defaults to `name`; `on \"…\"`
overrides it. Drive it from a reaction with `publish(entities(m).name, msg)`.
"""
macro publishes(M, decl, on=nothing, wire=nothing)
    name, T = _parse_port_decl(decl)
    w = _parse_on(on, wire)
    return :( $(_add_port!)($(esc(M)),
              $(PortSpec)($(QuoteNode(name)), :publisher, $(esc(T)), $w, nothing, (;))) )
end

"""
    @uses M name :: T  [on \"topic\"]

Attach a client port (service/action client) to mixin `M` (§3.1). Drive it from a
reaction via the handle, `entities(m).name`.
"""
macro uses(M, decl, on=nothing, wire=nothing)
    name, T = _parse_port_decl(decl)
    w = _parse_on(on, wire)
    return :( $(_add_port!)($(esc(M)),
              $(PortSpec)($(QuoteNode(name)), :client, $(esc(T)), $w, nothing, (;))) )
end

# `name :: T` → (name::Symbol, T-expr).
function _parse_port_decl(decl)
    (decl isa Expr && decl.head === :(::) && decl.args[1] isa Symbol) ||
        error("@publishes/@uses: expected `name :: T`, got $(decl)")
    (decl.args[1]::Symbol, decl.args[2])
end

# the optional `on "topic"` clause → a String literal or nothing.
function _parse_on(on, wire)
    on === nothing && return nothing
    on === :on || (on isa Symbol && return nothing)
    wire isa AbstractString ? String(wire) :
        error("@publishes/@uses: `on` must be followed by a topic string literal")
end

# ── DOES reactions: @every / @hears (§3.2) ──────────────────────────────────────
# Each defines the reaction method (dispatched on the mixin) AND records a port that
# `run` materialises, wiring the entity's data route to the reaction.

"""
    @every rate function f(m::M) … end

A timer on mixin `M` firing `f(m)` at `rate` Hz (§3.2). `rate` is a literal Hz number,
or `:param` to take the rate from parameter `param` (drift on a later change is
detected + warned, §3.4 — not auto-applied; rewire with `@on_parameter`, a later
increment).
"""
macro every(rate, f)
    fname, M, _ = _parse_reaction_sig(f, 1)
    return esc(quote
        $f
        $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :timer, nothing, nothing,
                                      $fname, (rate = $rate,)))
    end)
end

"""
    @hears [\"topic\"] function f(m::M, msg::T) … end

A subscription on mixin `M` running `f(m, msg)` per message (§3.2). The topic defaults
to the function name; a leading string literal overrides it. `T` is the second
argument's annotation.
"""
macro hears(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    fname, M, argtypes = _parse_reaction_sig(f, 2)
    T = argtypes[2]
    return esc(quote
        $f
        $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :subscription, $T, $wire,
                                      $fname, (;)))
    end)
end

"""
    @serves [\"name\"] function f(m::M, req::SrvReqType) … end                       # existing type
    @serves [\"name\"] function f(m::M, a::T, …)::@NamedTuple{out::U, …} … end          # author inline

A service on mixin `M` (§3.2), in either of `@ros_service`'s two type-sources. Without
a return annotation, `f(m, req)` serves the **existing** service type `SrvReqType` (a
generated `*_Request`) and returns the matching `*_Response`. With a `::@NamedTuple{…}`
return, the arguments after `m` are the **authored** request fields and the return is
the response — the macro generates `f_Request`/`f_Response` (package from `@ros_package`,
else the module name) and a splatting adapter. The service name defaults to the
function name; a leading string overrides it.
"""
macro serves(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@serves expects a `function f(m::M, …) … end` definition")
    fname, allargs, rettype = _parse_handler_sig(f)
    isempty(allargs) && error("@serves: the handler needs a first `m::M` argument")
    M = allargs[1][2]
    if rettype === nothing
        length(allargs) >= 2 ||
            error("@serves: the existing-type form needs `req::SrvReqType`; the inline form needs a `::@NamedTuple{…}` return")
        ReqT = allargs[2][2]
        return esc(quote
            $f
            $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :service, $ReqT, $wire, $fname, (;)))
        end)
    end
    pkg = isdefined(__module__, :__ros_package__) ?
          String(getfield(__module__, :__ros_package__)) : _snake(String(nameof(__module__)))
    return esc(_emit_serves_inline(__module__, pkg, fname, M, allargs[2:end], _nt_fields(rettype), wire, f))
end

# Inline `@serves`: generate `<f>_Request`/`<f>_Response` from the signature (reusing the
# authored-types codegen) + a splatting adapter `(m, req) -> Response` recorded as the
# port's reaction; the existing `:service` materialiser then wires it.
function _emit_serves_inline(caller::Module, pkg::AbstractString, fname::Symbol, M,
                             reqargs, respfields, wire, f)
    pkgsym   = Symbol(pkg)
    reqname  = Symbol(fname, "_Request")
    respname = Symbol(fname, "_Response")
    reqpath  = Expr(:., Expr(:., pkgsym, QuoteNode(:srv)), QuoteNode(reqname))
    resppath = Expr(:., Expr(:., pkgsym, QuoteNode(:srv)), QuoteNode(respname))
    adaptersym = Symbol("__serves_", fname)
    splat = [:( getfield(req, $(QuoteNode(an))) ) for (an, _) in reqargs]
    block = Expr(:toplevel)
    push!(block.args, f)                                                          # the handler (keeps m + fields)
    append!(block.args, _emit_structs(caller, pkg, :srv, [(reqname, reqargs), (respname, respfields)]))
    append!(block.args, _authored_register_stmts([(reqpath,  string(pkg, "/srv/", reqname)),
                                                   (resppath, string(pkg, "/srv/", respname))]))
    push!(block.args, :(function $adaptersym(m, req)
        out = $fname(m, $(splat...))
        return out isa $resppath ? out : $(ROSMessages.struct_from_nt)($resppath, out)
    end))
    push!(block.args, :( $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :service,
                                                       $reqpath, $wire, $adaptersym, (;))) ))
    return block
end

"""
    @runs [\"name\"] function f(m::M, goalfield::T, …, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…} … end

An action server on mixin `M` (§3.2), authored inline like `@ros_action`: the
arguments after `m` (minus the `FeedbackSink`) are the Goal fields, the sink's
`@NamedTuple` gives the Feedback fields, and the `@NamedTuple` return is the Result.
The macro generates the action's Goal/Result/Feedback + protocol-wrapper types
(package from `@ros_package`, else the module name) and runs `f(m, goalfields…, fb)`
per accepted goal; `fb((…,))` publishes feedback and is a cancel checkpoint. The
action name defaults to the function name; a leading string overrides it.
"""
macro runs(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@runs expects a `function f(m::M, …) … end` definition")
    fname, allargs, rettype = _parse_handler_sig(f)
    rettype === nothing && error("@runs: the handler needs a `::@NamedTuple{…}` result return")
    length(allargs) >= 2 || error("@runs: the handler needs `m::M` plus goal fields and a `fb::FeedbackSink{…}`")
    M = allargs[1][2]
    pkg = isdefined(__module__, :__ros_package__) ?
          String(getfield(__module__, :__ros_package__)) : _snake(String(nameof(__module__)))
    return esc(_emit_runs_inline(__module__, pkg, fname, M, allargs, rettype, wire, f))
end

# Inline `@runs`: replicate `@ros_action`'s type-gen (Goal/Result/Feedback + the five
# protocol wrappers) but with the leading `m` stripped from the Goal fields, wire the
# action type-support to `typeof(f)`, and record an `:action` port whose extra carries
# the feedback parameter's position in the FULL handler (incl. `m`).
function _emit_runs_inline(caller::Module, pkg::AbstractString, fname::Symbol, M,
                           allargs, rettype, wire, f)
    pkgsym = Symbol(pkg)
    goalfb = allargs[2:end]
    fb_rel = findfirst(a -> _is_feedbacksink(a[2]), goalfb)
    fb_rel === nothing && error("@runs: the handler needs a `fb::FeedbackSink{@NamedTuple{…}}` parameter")
    fb_pos_full = fb_rel + 1                                   # position in [m, goalfields…, fb]
    feedback_fields = _nt_fields(_feedbacksink_nt(goalfb[fb_rel][2]))
    goal_args = Tuple{Symbol, Any}[goalfb[i] for i in eachindex(goalfb) if i != fb_rel]
    result_fields = _nt_fields(rettype)
    goalsym, resultsym, fbsym = Symbol(fname, "_Goal"), Symbol(fname, "_Result"), Symbol(fname, "_Feedback")
    actpath(sym) = Expr(:., Expr(:., pkgsym, QuoteNode(:action)), QuoteNode(sym))
    actmod = Expr(:., pkgsym, QuoteNode(:action))
    uuidexpr = :(ROSNode.Interfaces.unique_identifier_msgs.msg.UUID)
    timeexpr = :(ROSNode.Interfaces.builtin_interfaces.msg.Time)
    specs = Any[
        (goalsym, goal_args), (resultsym, result_fields), (fbsym, feedback_fields),
        (Symbol(fname, "_SendGoal_Request"),  [(:goal_id, uuidexpr), (:goal, actpath(goalsym))]),
        (Symbol(fname, "_SendGoal_Response"), [(:accepted, :Bool), (:stamp, timeexpr)]),
        (Symbol(fname, "_GetResult_Request"),  [(:goal_id, uuidexpr)]),
        (Symbol(fname, "_GetResult_Response"), [(:status, :Int8), (:result, actpath(resultsym))]),
        (Symbol(fname, "_FeedbackMessage"),    [(:goal_id, uuidexpr), (:feedback, actpath(fbsym))]),
    ]
    block = Expr(:toplevel)
    push!(block.args, _rebuild_handler(f, fname, allargs, fb_pos_full, rettype))  # keeps m, strips fb's type
    append!(block.args, _emit_structs(caller, pkg, :action, specs))
    push!(block.args, :(ROSNode.ActionTypeSupport(::Type{typeof($fname)}) =
        ROSNode.ActionTypeSupport{typeof($fname), $(actpath(goalsym)), $(actpath(resultsym)), $(actpath(fbsym))}()))
    push!(block.args, :(ROSNode._action_wrapper(::Type{typeof($fname)}, suffix::AbstractString) =
        getfield($actmod, Symbol($(string(fname)), suffix))))
    regpairs = Tuple{Any, String}[(actpath(goalsym), string(pkg, "/action/", goalsym)),
        (actpath(resultsym), string(pkg, "/action/", resultsym)),
        (actpath(fbsym), string(pkg, "/action/", fbsym))]
    for suf in ("_SendGoal_Request", "_SendGoal_Response", "_GetResult_Request", "_GetResult_Response", "_FeedbackMessage")
        push!(regpairs, (actpath(Symbol(fname, suf)), string(pkg, "/action/", Symbol(fname, suf))))
    end
    append!(block.args, _authored_register_stmts(regpairs))
    push!(block.args, :( $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :action, nothing,
                                                       $wire, $fname, (fb_pos = $fb_pos_full,))) ))
    return block
end

# The action body the §9 ActionServer runs per accepted goal: splat the Goal's fields
# into the handler, prepend the mixin instance, inject a FeedbackSink at the recorded
# position, and convert the returned @NamedTuple to the Result message.
function _component_action_adapter(f, support, fb_pos::Int, m)
    G = goal_type(support); R = result_type(support); FB = feedback_type(support)
    return function (g)
        sink = FeedbackSink{FB}(g)
        callargs = Any[m]
        for n in fieldnames(G)
            push!(callargs, getfield(g.request, n))
        end
        insert!(callargs, fb_pos, sink)
        out = f(callargs...)
        return out isa R ? out : ROSMessages.struct_from_nt(R, out)
    end
end

# Parse `function f(m::M, …) … end` → (f::Symbol, M-expr, [arg-type-exprs]).
# `nargs` is the expected arity (the first arg is the mixin instance `m::M`).
function _parse_reaction_sig(f, nargs::Int)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@every/@hears expects a `function …` definition, got $(f)")
    sig = f.args[1]
    sig isa Expr && sig.head === :(::) && (sig = sig.args[1])   # strip a return annotation
    (sig isa Expr && sig.head === :call && sig.args[1] isa Symbol) ||
        error("@every/@hears: could not parse the handler signature")
    args = sig.args[2:end]
    length(args) >= 1 || error("@every/@hears: the handler needs a first `m::M` argument")
    (args[1] isa Expr && args[1].head === :(::)) ||
        error("@every/@hears: the first argument must be `m::MixinType`")
    M = args[1].args[2]
    argtypes = Any[a isa Expr && a.head === :(::) ? a.args[2] : :Any for a in args]
    return (sig.args[1]::Symbol, M, argtypes)
end

# ── lifecycle (§3.3): plain methods, defaulting to no-ops on Component ──────────

"""
    configure(m) / activate(m) / deactivate(m) / cleanup(m) / on_error(m)

Lifecycle hooks for a mixin (§3.3), each defaulting to a no-op on [`Component`](@ref).
`configure` acquires resources, `cleanup` releases them; under the action three-way a
hook may return `failure` to revert or throw to enter error processing (a later
increment wires the managed control surface).
"""
configure(::Component) = nothing
activate(::Component) = nothing
deactivate(::Component) = nothing
cleanup(::Component) = nothing
on_error(::Component) = nothing

# ── reflective accessors (§3.5) ─────────────────────────────────────────────────

"""
    entities(m::Component) -> NamedTuple

The mixin's materialised port handles, keyed by name (§3.5): `entities(m).image` is
the live `Publisher`, `entities(m).tick` the `Timer`. Reached reflectively through the
hidden runtime binding; errors if read before the ports are materialised.
"""
function _ports_value(m::Component)
    rt = getfield(m, :__rt__)
    rt === nothing && error("entities($(typeof(m))): not materialised yet (no node-core)")
    return (rt::MixinRuntime).ports
end
entities(m::Component) = _ports_value(m)

# Type-stable per-mixin `entities`. The ports NamedTuple's concrete type is fixed per
# mixin (the handle types follow the port specs' fixed QoS/message types), so we capture
# it at first materialise and generate `entities(m::M)::PortsNT` — a typed field load
# for `entities(m).image`, mirroring `parameters(m)`. The route type isn't known at
# codegen, hence the capture-then-generate rather than ahead-of-time.
const _ENTITIES_DONE = Base.IdSet{Type}()

function _ensure_entities_accessor!(::Type{M}, ports) where {M}
    @lock _MIXINS_LOCK (M in _ENTITIES_DONE && return nothing)
    Core.eval(parentmodule(M),
        :( $(GlobalRef(@__MODULE__, :entities))(m::$M) =
             $(GlobalRef(@__MODULE__, :_ports_value))(m)::$(typeof(ports)) ))
    @lock _MIXINS_LOCK push!(_ENTITIES_DONE, M)
    return nothing
end

"""
    parameters(m::Component) -> P_M

The mixin's live parameter snapshot (§3.5): a typed `@parameters`-style struct
generated from the mixin's `@param`s, read type-stably as `parameters(m).fps`. Backed
by the node-core's §10 `ParameterServer`, so the node is `ros2 param`-driveable and a
set is transactional. (For a single-mixin node the node schema *is* `P_M`; multi-mixin
slicing/namespacing, §4.4, is a later increment.)
"""
# The live parameter snapshot for `m` — dynamically typed. `_ensure_schema!` generates
# a per-mixin `parameters(::M) = _param_value(m)::P_M` that adds the concrete-type
# assert for a type-stable read; this generic is the pre-generation fallback.
function _param_value(m::Component)
    rt = getfield(m, :__rt__)
    rt === nothing && error("parameters($(typeof(m))): not materialised yet (no node-core)")
    return current((rt::MixinRuntime).pserver)
end
parameters(m::Component) = _param_value(m)

# ── construction hook (§4.2) ────────────────────────────────────────────────────
# A mixin with no dependencies default-constructs; the DI form (`construct(::Type{M},
# node, deps…)`) lands with the §4 protocol in a later increment.

"""
    construct(::Type{M}, node) -> M

Build a mixin instance (§4.2). Defaults to `M()` (every field defaulted). Override to
inject dependencies once the DI protocol (§4) lands.
"""
construct(::Type{M}, node) where {M <: Component} = M()

# ── DI: interfaces, provision, requirements (§4.1/§4.2) ─────────────────────────
# An interface is a NAME (a marker type) for a set of generic functions; provision is
# Holy-trait evidence (`provides`), not subtyping and not method-existence (§4.1). A
# mixin declares what it needs with `requires` and consumes injected providers in its
# `construct` (§4.2). Resolution + toposort live in run.jl.

"Supertype of the marker types `@interface` defines (§4.1)."
abstract type ComponentInterface end

"""
    @interface Name  method(_)::T  …

Declare an interface (§4.1): a NAME — a marker type `Name <: ComponentInterface` — for
the generic functions a provider must define. `@provides`/`requires` resolve against
this marker; a provider satisfies it by defining the listed methods (the method list
is documentation here; an expansion-time backing check is a later increment).
"""
macro interface(name, methods...)
    return esc(quote
        struct $name <: $(GlobalRef(@__MODULE__, :ComponentInterface)) end
        $name
    end)
end

"""
    provides(::Type{M}) -> Tuple of interfaces

The interfaces mixin `M` provides (§4.1) — Holy-trait evidence, set with `@provides`.
Empty by default.
"""
provides(::Type) = ()

"""
    requires(::Type{M}) -> Tuple of interfaces

The interfaces mixin `M` depends on (§4.2). Empty by default; override to declare
dependencies, which the node assembler resolves to sibling providers, toposorts, and
injects into `construct(::Type{M}, node, deps…)`. Pin an ambiguous provider with
`I => MemberOrType`.
"""
requires(::Type) = ()

"""
    @provides M Interface…

Declare that mixin `M` provides the given interfaces (§4.1) — emits the Holy-trait
evidence `provides(::Type{M}) = (Interface…,)`. A mixin may provide any number.
"""
macro provides(M, ifaces...)
    return esc(:( $(GlobalRef(@__MODULE__, :provides))(::Type{$M}) = ($(ifaces...),) ))
end

# ── parameter-schema codegen (§3.5) ─────────────────────────────────────────────
# A mixin's `@param`s accumulate across separate statements, so the typed schema `P_M`
# can't be generated at `@mixin` expansion. It's generated lazily at first `run`
# (cached): a `@parameters`-style coercing struct + its `descriptors` + the dispatched
# `pschema(::Type{M}) = P_M` accessor, evaluated into the mixin's home module — the
# authored-types deferred-codegen pattern (typesupport/authored.jl). `run` reaches the
# new-world type through `invokelatest`; reactions compiled afterward see it statically,
# so `parameters(m).fps` is a type-stable field load.

"`pschema(::Type{M})` → the mixin's generated parameter schema type `P_M` (§3.5)."
pschema(::Type{M}) where {M} =
    error("$(M): no parameter schema generated yet — call `run` (or `_ensure_schema!`) first")

const _SCHEMAS = IdDict{Type, Type}()

"""
    _ensure_schema!(M) -> P_M

Generate (once, cached) the typed parameter schema `P_M` for mixin `M` from its
`@param` specs, defining `descriptors(::Type{P_M})` and `pschema(::Type{M})`. Returns
`P_M`. New-world: callers touching `P_M` right after must go through `invokelatest`.
"""
function _ensure_schema!(::Type{M}) where {M}
    @lock _MIXINS_LOCK (haskey(_SCHEMAS, M) && return _SCHEMAS[M])
    P = _gen_schema(parentmodule(M), Symbol("__P_", nameof(M), "__"), mixin_spec(M).params, M)
    @lock _MIXINS_LOCK (_SCHEMAS[M] = P)
    return P
end

# The mixin types defined in `mod`, and the load-time finalizer the `@mixin` `__init__`
# hook calls — generate every such mixin's schema so `P_M` exists at module-load world
# (before `run`/transactions touch its ctor).
_mixins_of(mod::Module) =
    @lock _MIXINS_LOCK Type[M for M in keys(_MIXINS) if parentmodule(M) === mod]

function _finalize_mixins!(mod::Module)
    # Also drain any authored types (`@ros_message`/inline `@serves`) registered in this
    # module: the component `__init__` is the one hook, so it covers both — resolving the
    # `@mixin`-vs-authored `__init__` collision.
    try
        absorb_static_types!(mod)
    catch err
        @debug "component: absorb_static_types! in _finalize_mixins! skipped" mod exception=err
    end
    for M in _mixins_of(mod)
        _ensure_schema!(M)
    end
    return nothing
end

# Embed a parsed default/constraint value as an AST literal: a bare `Symbol` would be
# read as an identifier, so quote it; every other value (numbers, strings, tuples,
# types) interpolates as itself.
_lit(x) = x isa Symbol ? QuoteNode(x) : x

# Build + eval `P_M` (a coercing struct mirroring @parameters), its `descriptors`, and
# the `pschema` accessor, into module `mod`.
function _gen_schema(mod::Module, psym::Symbol, params::Vector{ParamSpec}, ::Type{M}) where {M}
    fields  = [:($(p.name)::$(p.type)) for p in params]
    kws     = [Expr(:kw, p.name, _lit(p.default)) for p in params]
    coerced = [:($(_coerce_param)($(p.type), $(p.name))) for p in params]
    ctor    = Expr(:function, Expr(:call, psym, Expr(:parameters, kws...)),
                   Expr(:block, Expr(:call, :new, coerced...)))
    structdef = Expr(:struct, false, psym, Expr(:block, fields..., ctor))
    descs = [:( $(ParameterDescriptor)($(QuoteNode(p.name)), $(p.type),
                  $(parameter_type)($(p.type)), $(p.doc), $(_lit(p.constraint)),
                  $(p.readonly), $(_coerce_param)($(p.type), $(_lit(p.default)))) )
             for p in params]
    block = quote
        $structdef
        $(GlobalRef(@__MODULE__, :descriptors))(::Type{$psym}) =
            $(ParameterDescriptor)[$(descs...)]
        $(GlobalRef(@__MODULE__, :pschema))(::Type{$M}) = $psym
        # the type-stable per-mixin accessor: `parameters(m::M)::P_M`
        $(GlobalRef(@__MODULE__, :parameters))(m::$M) =
            $(GlobalRef(@__MODULE__, :_param_value))(m)::$psym
        $psym
    end
    return Core.eval(mod, block)
end
