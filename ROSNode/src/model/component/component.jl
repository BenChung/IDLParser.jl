# Components — a node is a collection of mixins. A `@mixin` is a
# state-only mutable struct; `@param`/`@publishes`/`@uses` declare HAS ports;
# `@every`/`@hears`/`@serves`/`@runs` author DOES reactions; lifecycle hooks are plain
# methods dispatched on the mixin; `@node` assembles members and `run` instantiates.
#
# The declaration macros accumulate specs into per-type module-local stores at load;
# `run` reads them to materialize a node.

"""
    abstract type Component{Name} end

Supertype of every `@mixin` type. A mixin is a cohesive chunk
of a node — its own mutable state, the ports authored onto its type, and the lifecycle
hooks dispatched on it; a node is a collection of mixins sharing one node-core.

`Name` is the mixin's relative path within its node — its member name, lifted to a type
parameter so resolution against the node's typed runtime collapses through dispatch
(`entities(node, m)` / `parameters(node, m)` read `node.ports`/`node.pservers` at the
member named `Name`). A mixin instance holds **no** node reference — the node owns the
runtime, the instance carries only its state plus this path — which is what keeps the
node↔member relationship acyclic and fully typed.

`Component` carries the default lifecycle hooks ([`configure`](@ref) /
[`activate`](@ref) / [`deactivate`](@ref) / [`cleanup`](@ref) / [`on_error`](@ref), all
no-ops) and the default DI evidence ([`provides`](@ref) / [`requires`](@ref), both
empty), so a bare mixin needs none of them spelled out. Subtype it with
`@mixin struct M … end` (the macro injects the `Name` parameter for a non-parametric
mixin; a parametric one writes `struct M{Name, …} <: Component{Name}` itself).
"""
abstract type Component{Name} end

# The mixin's relative path (member name within its node), lifted from the type parameter.
# Constant-folds, so `entities(node, m)`/`parameters(node, m)` resolve statically.
_path(::Component{Name}) where {Name} = Name

export Component, @mixin, @param, @publishes, @uses, @every, @hears, @serves, @runs,
       @interface, @provides, provides, requires, construct,
       entities, parameters, configure, activate, deactivate, cleanup, on_error

# ── spec registries ───────────────────────────────────────────────────────────
# One `MixinSpec` per `@mixin` type, populated by the declaration macros.

"A declared parameter of a mixin — the parsed form of the `@parameters` field grammar."
struct ParamSpec
    name::Symbol
    type::Any            # the field type (a Type at runtime)
    default::Any
    constraint::Any
    readonly::Bool
    doc::String
end

"""
A declared port of a mixin. `kind` is one of two categories:

  - HAS — `:publisher`, `:client`: a typed handle the mixin holds, no authored handler.
  - DOES — `:subscription`, `:service`, `:action`, `:timer`: a dispatched reaction.

`wire` overrides the topic/name (else the identifier is used). The remaining fields:

  - `reaction` — the dispatched handler for a DOES port, `nothing` for a HAS port.
  - `extra` — kind-specific data (e.g. a timer's `rate`).
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

# Spec storage is per-mixin in the mixin's own module: `@mixin` defines a `const
# MixinSpec()` there plus a `mixin_spec(::Type{M})` method returning it, so the spec
# rides that module's precompile image. A ROSNode-global registry instead would lose a
# precompiled consumer's mixins, since a top-level mutation of ROSNode's dict runs in the
# consumer's precompile process and is discarded before its package cache is written.
# `ismixin` is likewise a per-type method. A missing `mixin_spec` method means the
# `@mixin` is absent or defined below.
function mixin_spec end
ismixin(@nospecialize(_)) = false

# Serialises concurrent first-touches of the process-global codegen caches (`_SCHEMAS`)
# and entity-accessor codegen.
const _CACHE_LOCK = ReentrantLock()

# Declarations attach to the registered base. A concrete instantiation reaching here is
# a value-level spelling (type alias, `typeof`) the syntactic macro guards cannot see;
# it would push onto the wrong mixin's spec, invisible to materialisation.
function _check_decl_key(::Type{M}) where {M}
    M isa DataType && Base.typename(M).wrapper !== M && error(
        "component declarations attach to the mixin base — use `$(Base.typename(M).wrapper)`, not the instantiation `$(M)`")
    return nothing
end

# Replace-by-name, not append. Revise re-evaluates a declaration's whole top-level
# expression when its body is edited — for `@hears`/`@every`/`@serves`/`@runs` that
# expression also carries this registration, so a plain `push!` would double the
# port/param on every reaction edit and clash at assembly (two publishers on one wire).
# A name is unique within a mixin (it keys `entities(node, m)` / the parameter schema), so an
# existing same-name entry is the prior definition — overwrite it in place.
function _add_param!(::Type{M}, p::ParamSpec) where {M}
    _check_decl_key(M)
    ps = mixin_spec(M).params
    i = findfirst(q -> q.name === p.name, ps)
    i === nothing ? push!(ps, p) : (ps[i] = p)
    return nothing
end
function _add_port!(::Type{M}, p::PortSpec) where {M}
    _check_decl_key(M)
    ps = mixin_spec(M).ports
    i = findfirst(q -> q.name === p.name, ps)
    i === nothing ? push!(ps, p) : (ps[i] = p)
    return nothing
end

# ── per-mixin baked artifacts ───────────────────────────────────────────────────
# Three module-local `const`s per mixin, keyed by its short `nameof` in its home module,
# so each rides the consuming package's precompile image:
#
#   __ros_spec_<Name>__      ::MixinSpec     declaration store, `@param`/`@publishes`/…
#                                            push onto it. Eager (`@mixin`). Reached by
#                                            `mixin_spec(::Type{M})`.
#   __ros_pschema_<Name>__   ::Type{P_M}     typed parameter schema, the `@param` set.
#                                            Lazy (`_gen_schema`). Reached by
#                                            `pschema(::Type{<:M})`; gives the node's per-member
#                                            `pservers` carrier its element types.
#
# A defined const is the bake-reuse marker: `_ensure_schema!` skips regeneration when it already
# exists, whether generated this process or baked at the consumer's precompile by
# `@precompile_nodes`. Re-emitting would redefine the struct and invalidate the specialisations
# baked against it, so the skip is a correctness guard, not an optimisation. The materialised-port
# NamedTuple type is derived statically from the specs (`_ports_nt_type`/`_handle_type`) — no
# per-mixin marker, since the node owns the typed `ports` carrier.
_spec_sym(name::Symbol)            = Symbol("__ros_spec_", name, "__")
_pschema_sym(::Type{M}) where {M}  = Symbol("__ros_pschema_", nameof(M), "__")

# ── @mixin ──────────────────────────────────────────────────────────────────────

"""
    @mixin struct M
        field::T = default
        …
    end
    @mixin struct M{Name, B} <: Component{Name}
        dep::B               # free type parameter, supplied by `construct`
        field::T = default
    end

Declare a mixin: a mutable, state-only struct subtyping
[`Component`](@ref), expanded with `Base.@kwdef`. Reactions dispatch on the type and
mutate its fields, so the struct holds only state — ports and parameters attach to the
*type* with `@param`/`@publishes`/`@uses`/`@every`/`@hears`/`@serves`/`@runs` and are
reached through [`entities`](@ref) / [`parameters`](@ref) (the node owns the runtime;
the instance holds no node reference), never as fields.

Every mixin carries a `Name` type parameter — its member path within a node — so the
node's typed runtime resolves through dispatch. A **non-parametric** mixin
(`@mixin struct M … end`) has `Name` and the `<: Component{Name}` supertype injected by
the macro; a **parametric** mixin must write both itself
(`@mixin struct M{Name, B} <: Component{Name}`), so the macro never reorders its type
parameters — it reads the name parameter out of the `Component{Name}` clause (call it
anything, place it anywhere) and errors if it is missing or not a type parameter.

The macro defines the mixin's spec store and its `mixin_spec`/`ismixin` dispatch in
*this* module, records the type as a loadable node kind by name (so a container's
[`load_node`](@ref) can instantiate it), and installs ROSNode's single load hook
[`ros_init!`](@ref) as the module's `__init__` unless the module defines its own. That
hook drains the module's authored types (`@ros_message` / inline `@serves`/`@runs`),
generates each mixin's parameter schema, and registers each kind. If you write your own
`__init__`, call `ROSNode.ros_init!(@__MODULE__)` from it to keep that setup running.
Every declared field needs a default unless [`construct`](@ref) is overridden to supply
it.

The spec store is a `const` in the defining module, reached by dispatch
(`mixin_spec(::Type{M})`), so it rides that module's own precompile image and a mixin
declared in a precompiled package keeps its parameters and ports. The loadable-kind
registration is likewise deferred to `ros_init!`, immediate only in the REPL/script case.

A parametric mixin (`struct M{Name, B} <: Component{Name}`) gives type-stable dependency
injection: a free-type-parameter field with no default is filled by `construct`. Supply
the DI form `construct(::Type{M}, node, ::Val{Name}, deps…) where {Name}` for composed
injection. Reactions and declarations annotate the bare base (`m::M`, never `m::M{…}`);
the base covers every instantiation, and the reaction macros reject a curly-annotated
`m::M{…}`.

A component module extending the framework lifecycle/DI generics must `import ROSNode:
configure, activate, deactivate, cleanup, on_error, requires, construct` (or qualify
them) — a bare definition under `using ROSNode` defines a shadowing function the
framework never calls.
"""
macro mixin(structexpr)
    (structexpr isa Expr && structexpr.head === :struct) ||
        error("@mixin expects `struct Name … end`, got $(structexpr)")
    newhead, base = _mixin_struct_head(structexpr.args[2])
    body = structexpr.args[3]
    # State-only: the mixin holds no runtime slot — the node owns ports/pservers, and the
    # `Name` parameter (in `newhead`) carries the member's path for typed resolution. Force
    # mutable + a kwdef ctor.
    newstruct = Expr(:struct, true, newhead, body)
    specsym = _spec_sym(base)
    return esc(quote
        Base.@kwdef $newstruct
        # Spec store + dispatch, both `const`/method in this module so they ride its
        # precompile image. `@param`/`@publishes`/… run as later top-level statements, so
        # `mixin_spec` is already visible when they push onto the spec. Reuse an existing
        # spec rather than resetting it: under Revise a struct-body edit re-evaluates only
        # this `@mixin` expression, not the separate `@param`/`@publishes` statements, so a
        # fresh `MixinSpec()` here would drop every port/param until the next full reload.
        const $specsym = isdefined(@__MODULE__, $(QuoteNode(specsym))) ?
            getfield(@__MODULE__, $(QuoteNode(specsym)))::$(MixinSpec) : $(MixinSpec)()
        $(GlobalRef(@__MODULE__, :mixin_spec))(::$(Type){$base}) = $specsym
        $(GlobalRef(@__MODULE__, :ismixin))(::$(Type){$base}) = true
        # Module-local roster of mixin bases, drained by `ros_init!` at load to generate
        # each schema and register each as a loadable kind.
        if !isdefined(@__MODULE__, :__mixin_bases__)
            global __mixin_bases__ = $(Any)[]
        end
        # Replace-by-name: a Revise re-eval of this `@mixin` must not append a duplicate
        # base (which re-bakes the schema/anchors at load). Keyed by name, not identity,
        # because redefining the struct yields a NEW type — replacing the prior entry keeps
        # the roster pointing at the live type for the next load's schema gen/registration.
        let mb = __mixin_bases__, j = findfirst(b -> nameof(b) === nameof($base), mb)
            j === nothing ? push!(mb, $base) : (mb[j] = $base)
        end
        # Register the loadable kind immediately for the REPL/script case; a precompiled
        # package defers to `ros_init!`, since a top-level mutation of ROSNode's registry
        # would not survive precompile.
        if ccall(:jl_generating_output, Cint, ()) == 0
            $(GlobalRef(@__MODULE__, :register_node_kind!))(string(nameof($base)), $base)
        end
        # Install ROSNode's load hook unless the module brings its own `__init__` (which
        # must then call `ROSNode.ros_init!(@__MODULE__)`).
        if !isdefined(@__MODULE__, :__init__)
            function __init__()
                $(GlobalRef(@__MODULE__, :ros_init!))(@__MODULE__)
            end
        end
        $base
    end)
end

# The bare name of a type parameter (`T`, `T<:Bound`, `T>:Lo`).
_param_name(p::Symbol) = p
function _param_name(p::Expr)
    p.head in (:(<:), :(>:)) && return _param_name(p.args[1])
    error("@mixin: could not read a type-parameter name from $(p)")
end

# Resolve the `@mixin` struct header → (struct-head-expr, base::Symbol). Non-parametric mixins
# get the `Name` parameter + `<: Component{Name}` injected; parametric mixins MUST write both
# themselves (`struct M{Name, …} <: Component{Name}`) so the macro never reorders their params,
# and the name parameter is READ from the `Component{Name}` clause (any name, any position).
function _mixin_struct_head(h)
    if h isa Symbol                                   # `struct Counter … end` → inject
        return (Expr(:(<:), Expr(:curly, h, :Name), Expr(:curly, :Component, :Name)), h)
    elseif h isa Expr && h.head === :curly            # `struct Guard{…} …` without the supertype
        error("@mixin: parametric mixin `$(h.args[1])` must subtype `Component{Name}` explicitly — " *
              "write `struct $(h.args[1]){Name, …} <: Component{Name} … end` (Name = the member-path parameter)")
    elseif h isa Expr && h.head === :(<:)
        sub, super = h.args[1], h.args[2]
        if sub isa Symbol                             # `struct Counter <: X` — non-parametric, macro owns the supertype
            error("@mixin: non-parametric mixin `$sub` must not declare a supertype; the macro adds " *
                  "`<: Component{Name}` — just write `struct $sub … end`")
        elseif sub isa Expr && sub.head === :curly
            base   = sub.args[1]
            pnames = map(_param_name, sub.args[2:end])
            (super isa Expr && super.head === :curly && super.args[1] === :Component && length(super.args) == 2) ||
                error("@mixin: parametric mixin `$base` must subtype `Component{Name}` with `Name` one of its " *
                      "type parameters; got `<: $super`")
            nm = super.args[2]
            nm in pnames ||
                error("@mixin: `Component{$nm}` — `$nm` is not a type parameter of `$base{$(join(pnames, ", "))}`")
            return (h, base)
        else
            error("@mixin: could not read the struct name from $(sub)")
        end
    else
        error("@mixin: could not parse the struct header $(h)")
    end
end

# ── @param ──────────────────────────────────────────────────────────────────────
# Reuses the `@parameters` field grammar via `_parse_param_field`:
#   @param M  "doc"  name::T = default [∈ lo..hi | ∈ (a,b)] [|> readonly]

"""
    @param M  ["doc"]  field::T = default
    @param M  ["doc"]  field::T = default ∈ lo..hi
    @param M  ["doc"]  field::T = default ∈ (choice₁, choice₂, …)
    @param M  ["doc"]  field::T = default |> readonly

Declare a ROS 2 parameter on mixin `M`, reusing the `@parameters` field grammar.
The optional leading string is the parameter's description (surfaced through its
`ParameterDescriptor`). `∈ lo..hi` constrains to a closed numeric range; `∈ (a, b, …)`
(or a `[…]` list) constrains to an explicit choice set. `|> readonly` marks the
parameter read-only — a startup override still applies, runtime sets are rejected.

The parameter joins the mixin's generated schema `P_M`, served by the node-core's
[`ParameterServer`](@ref). Read it live in a reaction with `parameters(node, m).field` — a
type-stable field load against the current snapshot. A default is mandatory (the
grammar errors otherwise). The macro expands to the registration side effect and
returns `nothing`.

```julia
@param ImageCapture "capture rate" fps::Int64 = 30 ∈ 1..120
@param ImageCapture device::String = "/dev/video0"
```
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

# ── HAS ports: @publishes / @uses ───────────────────────────────────────────────

"""
    @publishes M  name :: T
    @publishes M  name :: T  on "topic"

Declare an output port (a publisher) on mixin `M` for message type `T`. The
topic defaults to the port identifier `name`; `on "topic"` overrides it with an
explicit ROS topic name. This is a HAS port — a typed handle the mixin holds, with no
authored handler.

At the configure step the framework materialises a `Publisher` on the node-core; drive
it from a reaction with `publish(entities(node, m).name, msg)`. The materialised publisher
honours the node's lifecycle gating, so a managed node only emits while Active. The
macro expands to the registration side effect.

```julia
@publishes ImageCapture image :: sensor_msgs.msg.Image
@publishes Heartbeat   beat  :: std_msgs.msg.UInt32  on "~/heartbeat"
```
"""
macro publishes(M, decl, on=nothing, wire=nothing)
    name, T = _parse_port_decl(decl)
    w = _parse_on(on, wire)
    return :( $(_add_port!)($(esc(M)),
              $(PortSpec)($(QuoteNode(name)), :publisher, $(esc(T)), $w, nothing, (;))) )
end

"""
    @uses M  name :: T
    @uses M  name :: T  on "name"

Declare a client port (a service or action client) on mixin `M` for type `T`.
The wire name defaults to the port identifier `name`; `on "…"` overrides it. This is a
HAS port — a handle the mixin drives from its reactions and awaits a reply on.

Construct the client directly — `ServiceClient(node, name, T)` or
`ActionClient(node, name, A)` against the node-core. `@uses` records a `:client` port
spec, but port materialisation does not build a handle for the kind: it warns and skips,
so `entities(node, m).name` holds no client handle.

```julia
@uses Detector planner :: NavigateToPose on "/planner"
```
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

# ── DOES reactions: @every / @hears ─────────────────────────────────────────────
# Each defines the reaction method (dispatched on the mixin) AND records a port that
# `run` materialises, wiring the entity's data route to the reaction.

"""
    @every rate    function f(node, m::M) … end
    @every :param  function f(node, m::M) … end

Declare a timer on mixin `M` that fires the reaction `f(node, m)` at `rate` Hz, a
DOES port. `rate` is a literal Hz number, or a `Symbol` `:param` naming one of `M`'s
parameters, whose value at materialisation sets the period. Reactions are node-first —
the node is the first argument. The macro both defines the dispatched method `f` and
records a timer port pointing at it.

The timer is built paused at the configure step and started at the activate step, so a
tick cannot fire before the node is configured or while a managed node is gated below
Active. Reach the live `Timer` handle through `entities(node, m)` under the reaction's name.

A `:param` rate is read once at materialisation and then fixed for the timer's life; a
later change to that parameter leaves the period as set. A `:param` naming a
non-parameter or a non-numeric parameter errors at materialisation, naming the member,
port, and symbol.

```julia
@every :fps function tick(node, m::ImageCapture)
    publish(entities(node, m).image, grab_frame(m.dev))
end
```
"""
macro every(rate, f)
    fname, M, _ = _parse_reaction_sig(f, 2)
    return esc(quote
        $f
        $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :timer, nothing, nothing,
                                      $fname, (rate = $rate,)))
    end)
end

"""
    @hears           function f(node, m::M, msg::T) … end
    @hears "topic"   function f(node, m::M, msg::T) … end

Declare a subscription on mixin `M` that runs `f(node, m, msg)` once per received
message, a DOES port. The message type `T` is taken from the message argument's
annotation. The topic defaults to the function name; a leading string literal
overrides it. The macro both defines the dispatched method `f` and records a
subscription port pointing at it.

At the configure step the framework materialises a `Subscription` on the node-core
whose callback invokes `f` with the node and the live `m`. Dispatch is gated by the node
lifecycle, so a managed node only delivers messages to the handler while Active.
Reactions are node-first (`node` first, then the mixin); the mixin argument must be
annotated with the bare mixin base (`m::M`) — a curly instantiation `m::M{…}` is rejected.

```julia
@hears function odom(node, m::ImageCapture, msg::Odometry)
    m.pose = msg.pose.pose
end
```
"""
macro hears(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    fname, M, argtypes = _parse_reaction_sig(f, 3)
    T = argtypes[3]
    return esc(quote
        $f
        $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :subscription, $T, $wire,
                                      $fname, (;)))
    end)
end

"""
    @serves          function f(node, m::M, req::SrvReqType) … end                  # existing type
    @serves "name"   function f(node, m::M, req::SrvReqType) … end
    @serves          function f(node, m::M, a::A, …)::@NamedTuple{out::U, …} … end  # author inline
    @serves "name"   function f(node, m::M, a::A, …)::@NamedTuple{out::U, …} … end

Declare a service server on mixin `M`, a DOES port, in either of
`@ros_service`'s two type-sources. The service name defaults to the function name; a
leading string literal overrides it (e.g. `"~/set_mode"`).

The return annotation selects the type-source:

  - **Existing type** — no return annotation: `f(node, m, req)` where `req::SrvReqType` is
    a generated `*_Request`, and `f` returns the matching `*_Response`.
  - **Authored inline** — a `::@NamedTuple{…}` return: the arguments after `node, m` are the
    authored request fields and the return is the response. The macro generates
    `f_Request`/`f_Response` (package from the module's `@ros_package`, else the
    snake-cased module name), registers them, and emits a splatting adapter that calls
    `f(node, m, fields…)` and converts the returned `@NamedTuple` to the response struct.

Either
way the framework materialises a `Service` on the node-core whose callback runs the
handler with the live `m`; service dispatch honours lifecycle gating.

Reactions are node-first (`node` first); the mixin argument must be the bare mixin base
(`m::M`), a curly instantiation is rejected. The existing-type form needs at least `req`
after `node, m`; the inline form needs the `@NamedTuple` return.

```julia
@serves function arm(node, m::ImageCapture, enable::Bool)::@NamedTuple{success::Bool}
    m.enabled = enable
    (success = enable,)
end
```
"""
macro serves(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@serves expects a `function f(node, m::M, …) … end` definition")
    fname, allargs, rettype = _parse_handler_sig(f; allow_untyped=true)
    length(allargs) >= 2 || error("@serves: the handler is node-first — write `(node, m::M, …)`")
    M = allargs[2][2]
    M isa Expr && M.head === :curly && error(string(
        "@serves: reactions must cover the mixin base — got `m::$(M)`",
        M.args[1] === :Union ? "" : "; annotate `m::$(M.args[1])`"))
    if rettype === nothing
        length(allargs) >= 3 ||
            error("@serves: the existing-type form needs `(node, m, req::SrvReqType)`; the inline form needs a `::@NamedTuple{…}` return")
        ReqT = allargs[3][2]
        return esc(quote
            $f
            $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :service, $ReqT, $wire, $fname, (;)))
        end)
    end
    pkg = isdefined(__module__, :__ros_package__) ?
          String(getfield(__module__, :__ros_package__)) : _snake(String(nameof(__module__)))
    return esc(_emit_serves_inline(__module__, pkg, fname, M, allargs[3:end], _nt_fields(rettype), wire, f))
end

# Inline `@serves`: generate `<f>_Request`/`<f>_Response` from the signature (reusing the
# authored-types codegen) + a splatting adapter `(node, m, req) -> Response` recorded as the
# port's reaction; the existing `:service` materialiser then wires it (`_sub_cb` supplies
# node + m, the adapter splats the request fields into the node-first handler).
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
    push!(block.args, f)                                                          # the handler (keeps node + m + fields)
    append!(block.args, _emit_structs(caller, pkg, :srv, [(reqname, reqargs), (respname, respfields)]))
    append!(block.args, _authored_register_stmts([(reqpath,  string(pkg, "/srv/", reqname)),
                                                   (resppath, string(pkg, "/srv/", respname))]))
    push!(block.args, :(function $adaptersym(node, m, req)
        out = $fname(node, m, $(splat...))
        return out isa $resppath ? out : $(ROSMessages.struct_from_nt)($resppath, out)
    end))
    push!(block.args, :( $(_add_port!)($M, $(PortSpec)($(QuoteNode(fname)), :service,
                                                       $reqpath, $wire, $adaptersym, (;))) ))
    return block
end

"""
    @runs          function f(node, m::M, goal₁::T, …, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…} … end
    @runs "name"   function f(node, m::M, goal₁::T, …, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…} … end

Declare an action server on mixin `M`, a DOES port, authored inline like
`@ros_action`. After `node, m`, the arguments other than the `FeedbackSink` are the Goal
fields, the sink's `@NamedTuple` parameter gives the Feedback fields, and the
`@NamedTuple` return is the Result. The macro generates the action's
`_Goal`/`_Result`/`_Feedback` plus the five protocol-wrapper types
(`_SendGoal_Request`/`_Response`, `_GetResult_Request`/`_Response`,
`_FeedbackMessage`), registers them (package from `@ros_package`, else the snake-cased
module name), and wires the action type-support to `typeof(f)`. The action name
defaults to the function name; a leading string overrides it.

The framework runs `f(node, m, goalfields…, fb)` once per accepted goal, passing a
[`FeedbackSink`](@ref) as `fb` — the per-goal feedback-publication and
cancellation-checkpoint handle. A normal return settles the goal SUCCEEDED with the
converted Result; a throw settles it ABORTED. Reactions are node-first; the mixin
argument must be the bare mixin base, and the handler needs a `::@NamedTuple{…}` return
and exactly one `FeedbackSink` parameter.

Action dispatch honours the node's lifecycle gating: a managed node only accepts and
runs goals, and publishes feedback and status, while Active; requests to an inactive
node get an error reply.

```julia
@runs function fib(node, m::Counter, order::Int32,
                   fb::FeedbackSink{@NamedTuple{partial_sequence::Vector{Int32}}})::@NamedTuple{sequence::Vector{Int32}}
    seq = Int32[0, 1]
    for i in 3:order
        push!(seq, seq[end] + seq[end-1])
        fb((partial_sequence = seq,))
    end
    (sequence = seq,)
end
```
"""
macro runs(a, b=nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@runs expects a `function f(node, m::M, …) … end` definition")
    fname, allargs, rettype = _parse_handler_sig(f; allow_untyped=true)
    rettype === nothing && error("@runs: the handler needs a `::@NamedTuple{…}` result return")
    length(allargs) >= 3 || error("@runs: the handler is node-first — write `(node, m::M, goalfields…, fb::FeedbackSink{…})`")
    M = allargs[2][2]
    M isa Expr && M.head === :curly && error(string(
        "@runs: reactions must cover the mixin base — got `m::$(M)`",
        M.args[1] === :Union ? "" : "; annotate `m::$(M.args[1])`"))
    pkg = isdefined(__module__, :__ros_package__) ?
          String(getfield(__module__, :__ros_package__)) : _snake(String(nameof(__module__)))
    return esc(_emit_runs_inline(__module__, pkg, fname, M, allargs, rettype, wire, f))
end

# Inline `@runs`: replicate `@ros_action`'s type-gen (Goal/Result/Feedback + the five
# protocol wrappers) but with the leading `node, m` stripped from the Goal fields, wire the
# action type-support to `typeof(f)`, and record an `:action` port whose extra carries
# the feedback parameter's position in the FULL handler (incl. `node, m`).
function _emit_runs_inline(caller::Module, pkg::AbstractString, fname::Symbol, M,
                           allargs, rettype, wire, f)
    pkgsym = Symbol(pkg)
    goalfb = allargs[3:end]                                    # past node + m
    fb_rel = findfirst(a -> _is_feedbacksink(a[2]), goalfb)
    fb_rel === nothing && error("@runs: the handler needs a `fb::FeedbackSink{@NamedTuple{…}}` parameter")
    fb_pos_full = fb_rel + 2                                   # position in [node, m, goalfields…, fb]
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
    push!(block.args, _rebuild_handler(f, fname, allargs, fb_pos_full, rettype))  # keeps node + m, strips fb's type
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

# The action body the `ActionServer` runs per accepted goal: splat the Goal's fields
# into the handler, prepend the node + mixin instance, inject a FeedbackSink at the recorded
# position, and convert the returned @NamedTuple to the Result message.
function _component_action_adapter(f, support, fb_pos::Int, node, m)
    G = goal_type(support); R = result_type(support); FB = feedback_type(support)
    return function (g)
        sink = FeedbackSink{FB}(g)
        callargs = Any[node, m]
        for n in fieldnames(G)
            push!(callargs, getfield(g.request, n))
        end
        insert!(callargs, fb_pos, sink)
        out = f(callargs...)
        return out isa R ? out : ROSMessages.struct_from_nt(R, out)
    end
end

# Parse `function f(node, m::M, …) … end` → (f::Symbol, M-expr, [arg-type-exprs]).
# Node-first convention: arg 1 is the node, arg 2 is the mixin instance `m::M` (the dispatch
# target). `nargs` is the expected arity (incl. the node).
function _parse_reaction_sig(f, nargs::Int)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@every/@hears expects a `function …` definition, got $(f)")
    sig = f.args[1]
    sig isa Expr && sig.head === :(::) && (sig = sig.args[1])   # strip a return annotation
    (sig isa Expr && sig.head === :call && sig.args[1] isa Symbol) ||
        error("@every/@hears: could not parse the handler signature")
    args = sig.args[2:end]
    length(args) >= 2 ||
        error("@every/@hears: the handler is node-first — write `(node, m::M, …)`")
    (args[2] isa Expr && args[2].head === :(::)) ||
        error("@every/@hears: the second argument must be the mixin `m::MixinType` (the first is the node)")
    M = args[2].args[2]
    M isa Expr && M.head === :curly && error(string(
        "@every/@hears: reactions must cover the mixin base — got `m::$(M)`",
        M.args[1] === :Union ? "" : "; annotate `m::$(M.args[1])`"))
    argtypes = Any[a isa Expr && a.head === :(::) ? a.args[2] : :Any for a in args]
    return (sig.args[1]::Symbol, M, argtypes)
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

The member's (paused) timers start immediately before this hook, after which the
[`isactive`](@ref) dispatch gate opens and ports go live.
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

# ── accessors: the node owns the runtime, the mixin carries only its path ──────────
# `entities(node, m)` / `parameters(node, m)` resolve the member's port handles / parameter
# snapshot from the node's typed `ports`/`pservers` carriers, keyed by the mixin's `_path`.
# Both are single generic methods (defined in run.jl, beside `ComponentNode` where its typed
# fields exist); the path is a constant lifted from `typeof(m)`, so the field reads
# constant-fold — no per-mixin codegen, no `Any`.

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
  inlinable typed field load — the mixin stays state-only and holds no node reference.
  Inside a reaction the `node` is the first argument.
- `entities(node::ComponentNode)` — every member's handles aggregated and namespaced
  by member name in declared order: `entities(node).camera.image` is member
  `camera`'s `image` port, so two members' `image` handles don't collide. Each value
  is the corresponding per-member view.
"""
function entities end

# The materialised handle type for a port as a pure function of its spec, so the node's typed
# `ports` carrier (and the per-member view type) can be derived WITHOUT materialising. `nothing`
# for a kind whose handle type isn't statically derivable (`:action` server type, `:client` not
# materialised), which falls that member's cell back to a dynamic `Any` holder. Component
# publishers carry no per-port QoS, so the route is always the plain `Zenoh.Publisher`.
function _handle_type(p::PortSpec)
    p.kind === :publisher    && return PublisherHandle{p.msgtype, Zenoh.Publisher}
    p.kind === :subscription && return SubscriptionHandle{p.msgtype}
    p.kind === :service      && return ServiceHandle{request_type(p.msgtype), response_type(p.msgtype)}
    p.kind === :timer        && return Timer{ROS}
    return nothing
end

# The `entities(node, m)` NamedTuple type for a mixin's ports (spec order = materialise order), or
# `nothing` if any port's handle type isn't statically derivable.
function _ports_nt_type(specs::Vector{PortSpec})
    names = Symbol[]
    types = Any[]
    for p in specs
        ht = _handle_type(p)
        ht === nothing && return nothing
        push!(names, p.name)
        push!(types, ht)
    end
    return NamedTuple{(names...,), Tuple{types...}}
end

"""
    parameters(node, m::Component) -> P_M
    parameters(node::ComponentNode) -> NamedTuple

The mixin's live parameter snapshot: a typed `@parameters`-style struct `P_M`
generated from `M`'s [`@param`](@ref) declarations, read type-stably as
`parameters(node, m).field`. Backed by the node-core's [`ParameterServer`](@ref); the
returned struct is a complete snapshot, so a reaction never sees half-applied state.

Read from the node's typed `pservers` carrier at the member named `_path(m)`, so
`parameters(node, m).fps` is a typed field load — the mixin holds no node reference.
For a single-mixin node the node schema *is* `P_M` and parameters are un-prefixed; a
composed `@node` member-namespaces them at the node level while `parameters(node, m)`
stays mixin-local. Inside a reaction the `node` is the first argument.
"""
function parameters end

# ── construction hook ─────────────────────────────────────────────────────────────
# Every `@mixin` carries a `Name` type parameter (the member's path). The assembly passes the
# member name as a `Val{Name}` so `construct` can bind it; a non-parametric mixin (Name is its
# only parameter) default-constructs `M{Name}()`, a mixin with further parameters needs a DI
# `construct` that places `Name` itself. The DI form is resolved/toposorted in run.jl.

"""
    construct(::Type{M}, node, ::Val{Name}) -> M{Name}
    construct(::Type{M}, node, ::Val{Name}, deps…) -> M{Name…}

Build a mixin instance during node assembly. `Name` is the member's path within the node
(its name in the `@node` list), supplied by the framework as a `Val`. The default
`construct(::Type{M}, node, ::Val{Name})` returns `M{Name}()` for a non-parametric mixin
(every field defaulted). Override it to inject dependencies: a mixin that `requires`
interfaces or sibling mixins receives the resolved providers as `deps…`, positionally in
`requires` order, and stores them — typically a sibling mixin dep into a field typed by that
mixin (whose own `Name` parameter then carries that sibling's path).

A **parametric** mixin (free type parameters beyond `Name`) must define a `construct` that
places `Name` explicitly, threading the `Val{Name}` through `where`:

`node` is the node-core handle. An injected provider is constructed-but-unconfigured at this
point (its `configure` runs later, in dependency-first order), so store it and use it from
`configure` onward.

```julia
requires(::Type{Detector}) = (PoseSource,)                       # depend on an interface
construct(::Type{Detector}, node, ::Val{Name}, src) where {Name} = Detector{Name, typeof(src)}(; src)

requires(::Type{Watch}) = (Sensor,)                              # …or a sibling mixin directly
construct(::Type{Watch}, node, ::Val{Name}, s::Sensor) where {Name} = Watch{Name}(; sensor = s)
```
"""
function construct(::Type{M}, node, ::Val{Name}) where {M, Name}
    MN = M{Name}                                  # bind the member-path parameter
    MN isa DataType && return MN()                # non-parametric: Name was the only free parameter
    error("$(nameof(M)) has type parameters beyond its member name: define a DI " *
          "`construct(::Type{$(nameof(M))}, node, ::Val{Name}, deps…) where {Name}` that places `Name`, " *
          "or compose it in a `@node` so it receives its dependencies.")
end

# ── DI: interfaces, provision, requirements ─────────────────────────────────────
# An interface is a NAME (a marker type) for a set of generic functions; provision is
# Holy-trait evidence (`provides`), not subtyping and not method-existence. A
# mixin declares what it needs with `requires` and consumes injected providers in its
# `construct`. Resolution + toposort live in run.jl.

"Supertype of the marker types `@interface` defines."
abstract type ComponentInterface end

"""
    @interface Name
    @interface Name  method₁(_)::T  method₂(_)  …

Declare a dependency interface: a NAME for a set of generic functions, emitted
as an empty marker struct `Name <: ComponentInterface`. [`@provides`](@ref) /
[`requires`](@ref) resolve against this marker — provision is Holy-trait evidence, not
subtyping (a mixin may provide any number) and not method existence.

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

The interfaces mixin `M` provides — Holy-trait evidence consulted by the node
assembler to resolve sibling [`requires`](@ref) edges. Defaults to the empty tuple
`()`; set it with [`@provides`](@ref) (or define a method directly). Each element is
an `@interface` marker type.
"""
provides(::Type) = ()

"""
    requires(::Type{M}) -> Tuple

The dependencies mixin `M` declares. Defaults to the empty tuple `()`; override it to
declare them. Each entry is one of two kinds:

  - An `@interface` marker — resolved against sibling members' [`@provides`](@ref)
    evidence. Consumed through the interface's methods and typically stored in a free
    type parameter for type-stable access.
  - A concrete `@mixin` type — resolved against the sibling member that *is* that mixin
    (matched on its base, so a parametric mixin is named by its base too). Types the
    injected dependency concretely: the consuming `construct` can annotate `deps…` with
    the mixin type, no free type parameter needed.

At assembly the node resolver maps each requirement to the
single matching sibling (excluding `M` itself, so a mixin cannot satisfy its own
requirement), forms a dependency edge, toposorts the members, and injects the resolved
providers positionally into `construct(::Type{M}, node, deps…)` in `requires` order.

Zero matches is an unsatisfied-dependency error; more than one is an ambiguity error
(restructure so a single sibling satisfies it). Pin-pair disambiguation (an entry
`I => :member` or `I => MixinType`) is unsupported — a `Pair` entry raises an error
directing you to restructure.
"""
requires(::Type) = ()

"""
    @provides M  Interface₁  Interface₂  …

Declare that mixin `M` provides the given interfaces — emits the Holy-trait
evidence `provides(::Type{M}) = (Interface₁, …)`. A mixin may provide any number of
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

# ── parameter-schema codegen ──────────────────────────────────────────────────────
# A mixin's `@param`s accumulate across separate statements, so the typed schema `P_M`
# is generated lazily at first `run` (cached), not at `@mixin` expansion: a
# `@parameters`-style coercing struct + its `descriptors` + the covariant
# `pschema(::Type{<:M}) = P_M` accessor, evaluated into the mixin's home module. `run`
# reaches the new-world type through `invokelatest`; reactions compiled afterward see it
# statically, so `parameters(node, m).fps` is a type-stable field load.

"`pschema(::Type{M})` → the mixin's generated parameter schema type `P_M`."
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
    @lock _CACHE_LOCK (haskey(_SCHEMAS, M) && return _SCHEMAS[M])
    mod = parentmodule(M)
    psym = _pschema_sym(M)
    # If `P_M` already exists — generated this process or baked at the consumer's precompile
    # — reuse it: re-`_gen_schema` would redefine `P_M`/`pschema`/`parameters` and invalidate
    # the baked specialisations. The defined `const` is the reuse marker, riding the consumer's
    # pkgimage where the ROSNode-global `_SCHEMAS` cannot.
    P = isdefined(mod, psym) ? getfield(mod, psym) : _gen_schema(mod, psym, mixin_spec(M).params, M)
    @lock _CACHE_LOCK (_SCHEMAS[M] = P)
    return P
end

"""
    ros_init!(mod::Module) -> nothing

ROSNode's module-load initialization — the single hook the framework's macros install as
`__init__`. Idempotent and safe to call repeatedly: it drains the module's authored types
(`@ros_message`/`@ros_service`/`@ros_action`), generates each `@mixin`'s parameter schema
`P_M`, and registers every `@mixin`/`@node` kind by name.

`@mixin`/`@node`/the authored macros auto-install `__init__() = ros_init!(@__MODULE__)`
**only when the module has no `__init__` of its own**. If you bring your own `__init__`,
call this from it so ROSNode keeps initializing:

```julia
function __init__()
    ROSNode.ros_init!(@__MODULE__)   # keep ROSNode's load-time setup running
    my_own_setup()
end
```

Skipping it in a *precompiled* package leaves authored types unresolved and `@mixin`/
`@node` kinds unregistered, so `load_node`-by-name and dynamic wire-type resolution fail;
a direct `run(MyMixin)` still works (the schema generates lazily at `run`).
"""
function ros_init!(mod::Module)
    # One hook covers both layers — drain authored types AND finalize mixins/nodes — so a
    # module mixing `@mixin` with `@ros_message` initializes fully whichever macro
    # installed `__init__`.
    try
        absorb_static_types!(mod)
    catch err
        @debug "ros_init!: absorb_static_types! skipped" mod exception=err
    end
    if isdefined(mod, :__mixin_bases__)
        for M in getfield(mod, :__mixin_bases__)
            _ensure_schema!(M)                          # `P_M` exists at the load world
            register_node_kind!(string(nameof(M)), M)   # loadable kind (mixin-as-node)
        end
    end
    if isdefined(mod, :__node_kinds__)
        for (nm, K) in getfield(mod, :__node_kinds__)
            register_node_kind!(nm, K)
        end
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
    # `pschema(::Type{<:M}) = P_M` (keyed on the base, so it covers every `M{name,…}`
    # instantiation) + the parameter descriptors. The `parameters` accessor is NOT generated
    # per-mixin anymore — it is one generic `parameters(node, m)` reading the node's typed
    # `pservers` carrier (see run.jl). pschema still drives that carrier's element types.
    block = quote
        $structdef
        $(GlobalRef(@__MODULE__, :descriptors))(::Type{$psym}) =
            $(ParameterDescriptor)[$(descs...)]
        $(GlobalRef(@__MODULE__, :pschema))(::Type{<:$M}) = $psym
        $psym
    end
    return Core.eval(mod, block)
end
