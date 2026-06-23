# Components — a node is a collection of components. A component's `mutable struct S{Name} <: Component{Name}`
# holds its private state; `member_schema(S) = component(S, Params, …)` ties it to its public parameters,
# its ports (`publishes`/`uses` HAS ports, `hears`/`serves`/`runs`/`every` DOES reactions) and DI evidence;
# lifecycle hooks are plain methods dispatched on the component. `node("a" => A, …)` assembles
# members into a `NodeSchema` and `run` instantiates it.

"""
    abstract type Component{Name} end

Supertype of every component type. A component is a cohesive chunk
of a node — its private state and public parameters, the ports its [`member_schema`](@ref) declares,
and the lifecycle hooks dispatched on it; a node is a collection of components sharing one node-core.

`Name` is the component's relative path within its node — its member name, lifted to a type
parameter so resolution against the node's typed runtime collapses through dispatch
(`entities(node, m)` / `parameters(node, m)` read `node.ports`/`node.pservers` at the
member named `Name`). A component instance holds **no** node reference — the node owns the
runtime, the instance carries only its state plus this path — which is what keeps the
node↔member relationship acyclic and fully typed.

`Component` carries the default lifecycle hooks ([`configure`](@ref) /
[`activate`](@ref) / [`deactivate`](@ref) / [`cleanup`](@ref) / [`on_error`](@ref), all
no-ops) and the default DI evidence ([`provides`](@ref) / [`requires`](@ref), both
empty), so a bare component needs none of them spelled out. Subtype it as
`mutable struct M{Name} <: Component{Name} … end` (the [`@component`](@ref) macro injects the
`Name` parameter for you; a parametric one writes `struct M{Name, …} <: Component{Name}` itself).
"""
abstract type Component{Name} end

# The component's relative path (member name within its node), lifted from the type parameter.
# Constant-folds, so `entities(node, m)`/`parameters(node, m)` resolve statically.
_path(::Component{Name}) where {Name} = Name

export Component, @interface, @provides, provides, requires, construct,
       entities, parameters, configure, activate, deactivate, cleanup, on_error


# The bare name of a type parameter (`T`, `T<:Bound`, `T>:Lo`).
_param_name(p::Symbol) = p
function _param_name(p::Expr)
    p.head in (:(<:), :(>:)) && return _param_name(p.args[1])
    error("@component: could not read a type-parameter name from $(p)")
end

# Resolve a component struct header → (struct-head-expr, base::Symbol). Non-parametric components
# get the `Name` parameter + `<: Component{Name}` injected; parametric ones MUST write both
# themselves (`struct M{Name, …} <: Component{Name}`) so the macro never reorders their params,
# and the name parameter is READ from the `Component{Name}` clause (any name, any position).
function _mixin_struct_head(h, who::AbstractString = "@component")
    if h isa Symbol                                   # `struct Counter … end` → inject
        return (Expr(:(<:), Expr(:curly, h, :Name), Expr(:curly, :Component, :Name)), h)
    elseif h isa Expr && h.head === :curly            # `struct Guard{…} …` without the supertype
        error("$(who): parametric `$(h.args[1])` must subtype `Component{Name}` explicitly — " *
              "write `struct $(h.args[1]){Name, …} <: Component{Name} … end` (Name = the member-path parameter)")
    elseif h isa Expr && h.head === :(<:)
        sub, super = h.args[1], h.args[2]
        if sub isa Symbol                             # `struct Counter <: X` — non-parametric, macro owns the supertype
            error("$(who): non-parametric `$sub` must not declare a supertype; the macro adds " *
                  "`<: Component{Name}` — just write `struct $sub … end`")
        elseif sub isa Expr && sub.head === :curly
            base   = sub.args[1]
            pnames = map(_param_name, sub.args[2:end])
            (super isa Expr && super.head === :curly && super.args[1] === :Component && length(super.args) == 2) ||
                error("$(who): parametric `$base` must subtype `Component{Name}` with `Name` one of its " *
                      "type parameters; got `<: $super`")
            nm = super.args[2]
            nm in pnames ||
                error("$(who): `Component{$nm}` — `$nm` is not a type parameter of `$base{$(join(pnames, ", "))}`")
            return (h, base)
        else
            error("$(who): could not read the struct name from $(sub)")
        end
    else
        error("$(who): could not parse the struct header $(h)")
    end
end


# `name :: T` → (name::Symbol, T-expr). `who` names the calling macro in errors (`@component` reuses these).
function _parse_port_decl(decl, who::AbstractString = "@publishes/@uses")
    (decl isa Expr && decl.head === :(::) && decl.args[1] isa Symbol) ||
        error("$(who): expected `name :: T`, got $(decl)")
    (decl.args[1]::Symbol, decl.args[2])
end

# the optional `on "topic"` clause → a String literal or nothing.
function _parse_on(on, wire, who::AbstractString = "@publishes/@uses")
    on === nothing && return nothing
    on === :on || (on isa Symbol && return nothing)
    wire isa AbstractString ? String(wire) :
        error("$(who): `on` must be followed by a topic string literal")
end



# The action body the `ActionServer` runs per accepted goal: splat the Goal's fields
# into the handler, prepend the node + component instance, inject a FeedbackSink at the recorded
# position, and convert the returned @NamedTuple to the Result message.
function _component_action_adapter(f, support, fb_pos::Int, node, m)
    G = goal_type(support); R = result_type(support); FB = feedback_type(support)
    return function (g)
        sink = FeedbackSink{FB}(g)
        callargs = Any[node, m]
        for n in _surface_fields(G)                 # synthetic-only (goal-less) action splats no fields
            push!(callargs, getfield(g.request, n))
        end
        insert!(callargs, fb_pos, sink)
        out = f(callargs...)
        return out isa R ? out : _from_nt(R, out)   # empty Result builds via R(), not struct_from_nt
    end
end


# ── lifecycle hooks: plain methods, defaulting to no-ops on Component ───────────
# The user-override side. The transition drivers (configure!/activate!/…) own the
# state-machine edges and call these.

"""
    configure(node, m::M)

Component hook you override to acquire resources (open devices, load models) from
parameters read via [`parameters`](@ref). Default no-op on [`Component`](@ref). Runs at
the configure transition, driven by [`configure!`](@ref). [`cleanup`](@ref) is the
paired release. Like the reactions, hooks are node-first — the node is the first argument.

Member ports are materialised immediately before this hook, so [`entities`](@ref) is
available. Override it by importing or qualifying the generic.
"""
configure(node, ::Component) = nothing

"""
    activate(node, m::M)

Component hook you override to start streaming or enable behavior. Default no-op on
[`Component`](@ref). Runs at the activate transition, driven by [`activate!`](@ref).

The member's (paused) timers start immediately *after* this hook returns successfully (so a tick
cannot fire mid-activate or into a member whose activate failed); the [`isactive`](@ref) dispatch
gate then opens and ports go live.
"""
activate(node, ::Component) = nothing

"""
    deactivate(node, m::M)

Component hook you override to pause behavior. Default no-op on [`Component`](@ref).
Runs at the deactivate transition, driven by [`deactivate!`](@ref).

Ports stay materialised; the [`isactive`](@ref) dispatch gate closes, silencing
publishes, subscription dispatch, timer ticks, and service dispatch until the node is
[`Active`](@ref) again.
"""
deactivate(node, ::Component) = nothing

"""
    cleanup(node, m::M)

Component hook you override to release what [`configure`](@ref) acquired (close devices,
free handles). Default no-op on [`Component`](@ref). Runs at the cleanup/shutdown
transition, driven by [`cleanup!`](@ref) / [`shutdown!`](@ref).

Framework guarantees:

  - Fires at most once per [`configure`](@ref), guarded on the member's materialised
    ports — only for a configured member, and a second teardown trigger is a no-op.
  - The framework closes the member's port handles after it.
  - A throw is caught and logged, so one member's failure does not block the others'
    teardown.
  - Materialisation precedes [`configure`](@ref), so a member whose own [`configure`](@ref)
    threw still has its `cleanup` run — write it to tolerate partially-acquired state
    (e.g. guard `m.handle === nothing`).
"""
cleanup(node, ::Component) = nothing

"""
    on_error(node, m::M)

Component hook you override to recover or reset member state. Default no-op on
[`Component`](@ref). Runs at the error-processing step that a throwing transition driver
([`configure!`](@ref) / [`activate!`](@ref) / [`deactivate!`](@ref) / [`cleanup!`](@ref))
enters.

A throw here is caught and logged, so the remaining members still recover — but any
member's throw means recovery failed and the node lands [`Finalized`](@ref). On a
recovered error the framework then runs the cleanup fan-out — each member's
[`cleanup`](@ref) runs and its ports close — before the node lands [`Unconfigured`](@ref),
releasing whatever [`configure`](@ref) acquired. On the [`Finalized`](@ref) path that
fan-out is deferred until the node is closed.
"""
on_error(node, ::Component) = nothing

# ── accessors: the node owns the runtime, the component carries only its path ──────────
# `entities(node, m)` / `parameters(node, m)` resolve the member's port handles / parameter
# snapshot from the node's typed `ports`/`pservers` carriers, keyed by the component's `_path`.
# Both are single generic methods (defined in run.jl, beside `ComponentNode` where its typed
# fields exist); the path is a constant lifted from `typeof(m)`, so the field reads
# constant-fold — no per-member codegen, no `Any`.

"""
    entities(node, m::Component) -> NamedTuple
    entities(node::ComponentNode) -> NamedTuple

The materialised port handles as a `NamedTuple`. Drive each handle through its own
typed methods (`publish`, …). The ports must be materialised first (after the node
is configured); reading before then errors with a clear message, and reactions run
only after materialisation.

- `entities(node, m::Component)` — one member's handles, keyed by port identifier:
  `entities(node, m).image` is the live `Publisher`, `entities(node, m).tick` the
  `Timer`, `entities(node, m).<service>` the `Service`. Read from the node's typed
  `ports` carrier at the member named `_path(m)`, so `entities(node, m).field` is an
  inlinable typed field load — the component stays state-only and holds no node reference.
  Inside a reaction the `node` is the first argument.
- `entities(node::ComponentNode)` — every member's handles aggregated and namespaced
  by member name in declared order: `entities(node).camera.image` is member
  `camera`'s `image` port, so two members' `image` handles don't collide. Each value
  is the corresponding per-member view.
"""
function entities end


"""
    parameters(node, m::Component) -> P_M
    parameters(node::ComponentNode) -> NamedTuple

The component's live parameter snapshot: a typed `@parameters`-style struct `P_M`
(the `member_schema(M) = component(M, P_M, …)` parameter schema), read type-stably as
`parameters(node, m).field`. Backed by the node-core's [`ParameterServer`](@ref); the
returned struct is a complete snapshot, so a reaction never sees half-applied state.

Read from the node's typed `pservers` carrier at the member named `_path(m)`, so
`parameters(node, m).fps` is a typed field load — the component holds no node reference.
For a single-component node the node schema *is* `P_M` and parameters are un-prefixed; a
composed node member-namespaces them at the node level while `parameters(node, m)`
stays member-local. Inside a reaction the `node` is the first argument.
"""
function parameters end

# ── construction hook ─────────────────────────────────────────────────────────────
# Every component carries a `Name` type parameter (the member's path). The assembly passes the
# member name as a `Val{Name}` so `construct` can bind it; a non-parametric component (Name is its
# only parameter) default-constructs `M{Name}()`, a component with further parameters needs a DI
# `construct` (`member_schema(M)`'s `ctor=`) that places `Name` itself. DI is resolved/toposorted in functor.jl.

"""
    construct(::Type{M}, node, ::Val{Name}) -> M{Name}
    construct(::Type{M}, node, ::Val{Name}, deps…) -> M{Name…}

Build a component instance during node assembly. `Name` is the member's path within the node
(its name in the `node(…)` member list), supplied by the framework as a `Val`. A component that
`requires` interfaces or sibling components receives the resolved providers as `deps…`,
positionally in `requires` order, and stores them — typically a sibling dep into a field typed by
that component (whose own `Name` parameter then carries that sibling's path).

Two defaults cover the common shapes, so most components define no `construct` at all:

  - **No dependencies** — `construct(::Type{M}, node, ::Val{Name})` returns `M{Name}()`, for a
    component whose every field past `Name` is defaulted.
  - **Pure dependency holder** — `construct(::Type{M}, node, ::Val{Name}, deps…)` builds
    `M{Name, typeof.(deps)…}(deps…)` from the type itself, for a component whose free type
    parameters past `Name` are exactly the injected deps, in order (the `M{Name, B}; src::B`
    pattern). The injected types fix those parameters, so the reads stay type-stable.

Override either to do more — store deps into named fields, set non-dep fields, place `Name` in a
different parameter position. Declare your override as a `construct(::Type{M}, node, ::Val{Name},
deps…)` method (below), or as `member_schema(M)`'s `ctor=` keyword (the `ctor=` overrides the
method when both are given). The [`@component`](@ref) macro's `@requires` directive emits the
override for you, threading the deps after the component's inline field defaults.

`node` is the node-core handle. An injected provider is constructed-but-unconfigured at this
point (its `configure` runs later, in dependency-first order), so store it and use it from
`configure` onward.

```julia
requires(::Type{Detector}) = (PoseSource,)                       # depend on an interface
# default construct suffices for `Detector{Name, B}; src::B` — no method needed.

requires(::Type{Watch}) = (Sensor,)                             # …a sibling component, stored in a named field
construct(::Type{Watch}, node, ::Val{Name}, s::Sensor) where {Name} = Watch{Name}(; sensor = s)
```
"""
function construct(::Type{M}, node, ::Val{Name}) where {M, Name}
    MN = M{Name}                                  # bind the member-path parameter
    MN isa DataType && return MN()                # non-parametric: Name was the only free parameter
    error("$(nameof(M)) has type parameters beyond its member name: define a DI " *
          "`construct(::Type{$(nameof(M))}, node, ::Val{Name}, deps…) where {Name}` that places `Name`, " *
          "or compose it in a `node(…)` so it receives its dependencies.")
end

# Default DI construction: with no user/`@component`/`ctor=` override, build the component from its own
# type — `M{Name, typeof.(deps)…}(deps…)`. Fits the pure-holder shape where the free parameters past
# `Name` are exactly the injected deps, in order, and the fields are those deps. Any other shape (named
# fields, extra non-dep fields, a different parameter order) defines its own `construct`/`ctor=`.
function construct(::Type{M}, node, ::Val{Name}, dep, deps...) where {M, Name}
    args = (dep, deps...)
    MN = try
        M{Name, map(typeof, args)...}
    catch
        error("$(nameof(M)): the default dependency-injection constructor builds " *
              "`$(nameof(M)){Name, <dep types>}` from its $(length(args)) injected dependency(ies), but " *
              "`$(nameof(M))`'s type parameters past `Name` are not one-per-dependency. Define " *
              "`construct(::Type{$(nameof(M))}, node, ::Val{Name}, deps…) where {Name}`, or " *
              "`member_schema`'s `ctor=`, for a component with other fields or a different parameter shape.")
    end
    return MN(args...)
end

# ── DI: interfaces, provision, requirements ─────────────────────────────────────
# An interface is a NAME (a marker type) for a set of generic functions; provision is
# Holy-trait evidence (`provides`), not subtyping and not method-existence. A
# component declares what it needs with `requires` and consumes injected providers in its
# `ctor`. Resolution + toposort live in functor.jl.

"Supertype of the marker types `@interface` defines."
abstract type ComponentInterface end

"""
    @interface Name
    @interface Name  method₁(_)::T  method₂(_)  …

Declare a dependency interface: a NAME for a set of generic functions, emitted
as an empty marker struct `Name <: ComponentInterface`. [`@provides`](@ref) /
[`requires`](@ref) resolve against this marker — provision is Holy-trait evidence, not
subtyping (a component may provide any number) and not method existence.

The method signatures after the name are documentation only: the macro discards them,
and a provider satisfies the interface by defining the methods and declaring
`@provides M Name` — the resolver trusts the declaration, with no check that the methods
actually exist.

```julia
@interface PoseSource  last_pose(_)::Pose
```
"""
macro interface(name, methods...)
    return esc(quote
        struct $name <: $(GlobalRef(@__MODULE__, :ComponentInterface)) end
        $name
    end)
end

"""
    provides(::Type{M}) -> Tuple

The interfaces component `M` provides — Holy-trait evidence consulted by the node
assembler to resolve sibling [`requires`](@ref) edges. Defaults to the empty tuple
`()`; set it with [`@provides`](@ref) (or `component(M, …; provides=(I,))`). Each element is
an `@interface` marker type.
"""
provides(::Type) = ()

"""
    requires(::Type{M}) -> Tuple

The dependencies component `M` declares. Defaults to the empty tuple `()`; set it by defining
`requires(::Type{M})` or with `component(M, …; requires=(I,))` (the keyword overrides the method). Each
entry is one of two kinds:

  - An `@interface` marker — resolved against sibling members' [`@provides`](@ref)
    evidence. Consumed through the interface's methods and typically stored in a free
    type parameter for type-stable access.
  - A concrete component type — resolved against the sibling member that *is* that
    component (matched on its base, so a parametric component is named by its base too). Types the
    injected dependency concretely: the consuming `ctor` can annotate `deps…` with
    the component type, no free type parameter needed.

At assembly the node resolver maps each requirement to the
single matching sibling (excluding `M` itself, so a component cannot satisfy its own
requirement), forms a dependency edge, toposorts the members, and injects the resolved
providers positionally into the member's `ctor(node, ::Val{Name}, deps…)` in `requires` order.

Zero matches is an unsatisfied-dependency error; more than one is an ambiguity error
(restructure so a single sibling satisfies it). Pin-pair disambiguation (an entry
`I => :member` or `I => ComponentType`) is unsupported — a `Pair` entry raises an error
directing you to restructure.
"""
requires(::Type) = ()

# Read the trait-method DI declarations from `_component` without the `requires`/`provides` keyword
# arguments shadowing the generics there. `component(…; requires=/provides=)` overrides these.
_declared_requires(::Type{M}) where {M} = requires(M)
_declared_provides(::Type{M}) where {M} = provides(M)

"""
    @provides M  Interface₁  Interface₂  …

Declare that component `M` provides the given interfaces — emits the Holy-trait
evidence `provides(::Type{M}) = (Interface₁, …)`. A component may provide any number of
interfaces.

The declaration is taken on trust, with no check that `M` actually defines each
interface's methods. The node assembler reads this evidence to resolve
siblings' [`requires`](@ref) to providers.

```julia
last_pose(m::ImageCapture) = m.pose
@provides ImageCapture PoseSource
```
"""
macro provides(M, ifaces...)
    return esc(:( $(GlobalRef(@__MODULE__, :provides))(::Type{$M}) = ($(ifaces...),) ))
end


"""
    ros_init!(mod::Module) -> nothing

ROSNode's module-load initialization — the single hook the framework's macros install as
`__init__`. Idempotent and safe to call repeatedly: it drains the module's authored types
(`@ros_message`/`@ros_service`/`@ros_action`) and registers every node-kind schema on the
module's `__node_kinds__` roster (the [`@register_nodes`](@ref) list) by name.

`@register_nodes`/the authored macros auto-install `__init__() = ros_init!(@__MODULE__)`
**only when the module has no `__init__` of its own**. If you bring your own `__init__`,
call this from it so ROSNode keeps initializing:

```julia
function __init__()
    ROSNode.ros_init!(@__MODULE__)   # keep ROSNode's load-time setup running
    my_own_setup()
end
```

Skipping it in a *precompiled* package leaves authored types unresolved and node-kind
schemas unregistered, so `load_node`-by-name and dynamic wire-type resolution fail;
a direct `run(MyComponent)` / `run(schema)` still works (it needs no registry).
"""
function ros_init!(mod::Module)
    # One hook covers both layers — drain authored types AND register node-kind schemas — so a
    # module mixing `@register_nodes` with `@ros_message` initializes fully whichever macro
    # installed `__init__`.
    try
        absorb_static_types!(mod)
    catch err
        @debug "ros_init!: absorb_static_types! skipped" mod exception=err
    end
    if isdefined(mod, :__node_kinds__)
        for (nm, K) in getfield(mod, :__node_kinds__)
            register_node_kind!(nm, K)
        end
    end
    return nothing
end

