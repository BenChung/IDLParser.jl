# Functor node API (PLAN-NODE-PLAN.md). A node kind is a typed SCHEMA VALUE built from value
# combinators; node() resolves DI once and freezes the plan into the schema type; run(::NodeSchema)
# materialises onto the existing ComponentNode runtime. This is the sole component API: a node kind is
# a value, not a declaration-tracked macro.
#
# Reuses the existing runtime wholesale: PortCell/ComponentNode, entities/parameters/_path, the
# pattern constructors, ParameterServer/CompositeParameterServer, and the a-priori graph priming
# (_desc_entity/_reconcile_local_graph!). It changes none of that — it is a new front end onto it.

export component, node, publishes, hears, serves, runs, every, uses, remap, member_schema, MemberSchema, NodeSchema, precompile_node
export message_type, port_name, port_names, param_names, port, member_names, descname   # introspection
export register_node_kinds!, @register_nodes                                            # load-by-name registration

# ── descriptors: typed port values; name + message/handler types ride the TYPE ───────────────────
abstract type PortDesc{Name} end
struct Pub{Name, T}            <: PortDesc{Name}; wire::Union{String, Nothing}; end
struct Sub{Name, T, F}         <: PortDesc{Name}; handler::F; wire::Union{String, Nothing}; end
struct Srv{Name, Rq, Rs, F}    <: PortDesc{Name}; handler::F; wire::Union{String, Nothing}; end
struct Act{Name, A, F, H}      <: PortDesc{Name}; handler::F; wire::Union{String, Nothing}; end
struct Tmr{Name, F}            <: PortDesc{Name}; handler::F; rate::Union{Float64, Symbol}; end

descname(::Type{<:PortDesc{Name}}) where {Name} = Name

# ── combinators: plain Symbol names (the ctor materialises the type param; no surface Val) ────────
_norm_wire(on) = on === nothing ? nothing : String(on)
"""
    publishes(name::Symbol, T; on=nothing) -> Pub

A publisher port carrying message type `T`, sent with `publish(entities(node, m).name, msg)`. `name`
keys the handle in `entities(node, m)`; `on` sets its topic (the port `name` by default).
"""
publishes(name::Symbol, ::Type{T}; on = nothing) where {T} = Pub{name, T}(_norm_wire(on))

"""
    hears(name::Symbol, T, handler; on=nothing) -> Sub

A subscription on message type `T` that runs `handler(node, m, msg::T)` per message. `on` sets the topic
it subscribes to (the reaction `name` by default).
"""
hears(name::Symbol, ::Type{T}, handler; on = nothing) where {T} = Sub{name, T, typeof(handler)}(handler, _norm_wire(on))
# single service-request marker, but request/response are RESOLVED EAGERLY here (normal world) into
# the descriptor type params. The @generated carrier builder must NOT call response_type from a
# generator world: a user-AUTHORED service's _Response binding postdates ROSNode's definition world
# and would be invisible there (imported/vendored types live in ROSNode's world and masked this).
"""
    serves(name::Symbol, ReqType, handler; on=nothing) -> Srv
    serves(name::Symbol, handler; on=nothing) -> Srv          # a bare @service handler
    serves(name::Symbol, existing::Srv; on=nothing) -> Srv    # rebind under another name

A service-server port answering `handler(node, m, req)` over request type `ReqType`. `on` sets the
service name (the port `name` by default). Author the request/response types inline with
[`@service`](@ref) (then `serves` takes the bare handler), or pass a pre-authored `ReqType` here. See
[Services](../communication/services.md).
"""
serves(name::Symbol, ::Type{Req}, handler; on = nothing) where {Req} =
    Srv{name, request_type(Req), response_type(Req), typeof(handler)}(handler, _norm_wire(on))
"""
    every(name::Symbol, rate, handler) -> Tmr

A timer firing `handler(node, m)` at `rate` — a frequency in Hz (a `Real`), or a parameter `Symbol` to
bind the period live to that parameter. A timer addresses no topic, and fires only while the node is `Active`.
"""
every(name::Symbol, rate::Union{Real, Symbol}, handler) =
    Tmr{name, typeof(handler)}(handler, rate isa Symbol ? rate : Float64(rate))
# action server over a PRE-authored @ros_action type — `action` is the authored marker (its type is
# the action type ActionTypeSupport dispatches on); `exec(node, m, goal::GoalHandle)` runs per goal.
# Inline @ros_action-from-the-handler authoring is the `@action` macro (functor_authoring.jl) atop this.
# The concrete `ActionServer{A,G,R,Fb}` handle type `H` is resolved EAGERLY here (normal world, like
# `serves`) — never from `handle_type` in the @generated carrier, whose generator world can't see a
# user-authored action's `ActionTypeSupport(typeof(marker))` specialization (the Srv world-age trap).
_action_server_type(::Type{A}) where {A} =
    (s = ActionTypeSupport(A); ActionServer{A, goal_type(s), result_type(s), feedback_type(s)})
"""
    runs(name::Symbol, Action, exec; on=nothing) -> Act
    runs(name::Symbol, existing::Act; on=nothing) -> Act      # rebind under another name

An action-server port running `exec(node, m, goal)` per accepted goal, over a pre-authored `@ros_action`
type `Action`. `on` sets the action name (the port `name` by default). Author the action inline with
[`@action`](@ref), or pass a pre-authored `Action` here. See [Actions](../communication/actions.md).
"""
runs(name::Symbol, action, exec; on = nothing) =
    (AT = typeof(action); Act{name, AT, typeof(exec), _action_server_type(AT)}(exec, _norm_wire(on)))

# Rebind an EXISTING descriptor under another port name / wire — reuse a `@service`/`@action`-authored
# (or any) service/action elsewhere without re-authoring (the "same service, another name" case).
serves(name::Symbol, d::Srv{N, Rq, Rs, F}; on = nothing) where {N, Rq, Rs, F} =
    Srv{name, Rq, Rs, F}(d.handler, on === nothing ? d.wire : _norm_wire(on))
runs(name::Symbol, d::Act{N, A, F, H}; on = nothing) where {N, A, F, H} =
    Act{name, A, F, H}(d.handler, on === nothing ? d.wire : _norm_wire(on))

# ── client ports: a persistent ServiceClient/ActionClient held in the member's entities (rclcpp-style;
#    created at configure, reused for call/send). `IsAct` (resolved at uses()) picks the materialiser. ──
# `IsAct` (resolved at uses()) picks the materialiser; `H` is the CONCRETE client handle type
# (`ServiceClient{Req,Resp}` / `ActionClient{A,G,R,F}`), resolved EAGERLY at uses() (like `serves`)
# and baked here so `handle_type`/`ports_nt_type` are concrete and the client cell is typed.
struct Use{Name, IsAct, M, H} <: PortDesc{Name}; marker::M; wire::Union{String, Nothing}; end
"""
    uses(name::Symbol, marker; on=nothing) -> Use

Declare a persistent service/action CLIENT port. `marker` is a service request type (or `@ros_service`
marker) → a `ServiceClient`, or an `@ros_action`/authored action marker → an `ActionClient`. The client
is materialised at configure into `entities(node, m).name` and held for the node's life (reuse it for
`call`/`send`), mirroring rclcpp's `create_client`. `on` sets the service/action name (else the port `name`).
"""
function uses(name::Symbol, marker; on = nothing)
    MT = marker isa Type ? marker : typeof(marker)
    # ActionTypeSupport throws ArgumentError for a non-action marker (a service request type) → service
    # client; any OTHER error (e.g. an UndefVar from a half-registered marker) is real — don't mask it.
    # (A service marker could in principle be misread as an action only if its module also defined sibling
    # `<Name>_Goal/_Result/_Feedback` types — which generated/@ros_service types never emit.)
    # Resolve the CONCRETE client handle type HERE (normal world), exactly as `serves()` resolves Req/Resp,
    # and bake it into the Use type's `H` param so the materialised client lands in a TYPED cell (call/send
    # dispatch statically). The resolution matches what ServiceClient/ActionClient actually RETURN: a Function
    # marker forwards through `typeof(marker)`, so request_type/ActionTypeSupport over `MT` give the same params.
    isact, H = try
        s = ActionTypeSupport(MT)
        true, ActionClient{MT, goal_type(s), result_type(s), feedback_type(s)}
    catch e
        e isa ArgumentError ?
            (false, ServiceClient{request_type(MT), response_type(MT)}) : rethrow()
    end
    return Use{name, isact, typeof(marker), H}(marker, _norm_wire(on))
end
# A client builds its OWN entities lazily (port_descs(::Use)=[] → not in the primed reserved-id block),
# but ServiceClient → make_entity pops the armed `_id_queue` while `_materialising`, stealing an id meant
# for a real primed endpoint and shifting every later one. Build clients with `_materialising` cleared so
# they take fresh ids via `next_entity_id!` (ActionClient already does, but clear uniformly for safety).
function _construct_client(core, build)
    prev = core._materialising
    core._materialising = false
    try; return build(); finally; core._materialising = prev; end
end
# Unlike Sub/Srv/Act, a client passes no `warmup=:off` — clients self-warm on first call/send and have no
# `_anchor_functor_member!` branch, so inheriting the node's WarmupPolicy here is the deliberate choice.
construct_port(d::Use{N, false, M, H}, core, cn, m, pv, wm) where {N, M, H} =
    _construct_client(core, () -> ServiceClient(core, _wirename(d, wm), d.marker))
construct_port(d::Use{N, true, M, H},  core, cn, m, pv, wm) where {N, M, H} =
    _construct_client(core, () -> ActionClient(core, _wirename(d, wm), d.marker))
port_descs(d::Use, node, w) = EndpointDesc[]    # clients prime lazily — no a-priori local-graph endpoints

# a node() member with explicit wire remaps — remap(SchemaOrType, port => "wire" | (member, port), …)
struct _Remap; schema::Any; remaps::Vector{Pair{Symbol, Any}}; end
"""
    remap(component, port => "name" | (member, port), …)

Wrap a `node(…)` member with explicit name overrides for one or more of its ports. `port => "name"`
retargets that port's topic (or service/action name); `port => (member, port)` ties it to another
member's port. Use it to resolve a name clash or to connect two members onto one topic.
"""
remap(s, rs::Vararg{Pair}) = _Remap(s, Pair{Symbol, Any}[Symbol(r.first) => r.second for r in rs])
_unwrap(x::_Remap) = (x.schema, x.remaps)
_unwrap(x) = (x, Pair{Symbol, Any}[])

# ── handle types (pure type fn of the descriptor; mirrors _handle_type) ──────────────────────────
handle_type(::Type{Pub{N, T}})         where {N, T}          = PublisherHandle{T, Zenoh.Publisher}
handle_type(::Type{Sub{N, T, F}})      where {N, T, F}       = SubscriptionHandle{T}
handle_type(::Type{Srv{N, Rq, Rs, F}}) where {N, Rq, Rs, F}  = ServiceHandle{Rq, Rs}   # resolved in serves()
handle_type(::Type{Tmr{N, F}})         where {N, F}          = Timer{ROS}
handle_type(::Type{Use{N, IsAct, M, H}}) where {N, IsAct, M, H} = H   # client: resolved eagerly in uses()
handle_type(::Type{Act{N, A, F, H}})   where {N, A, F, H}    = H   # action server: resolved eagerly in runs()
handle_type(::Type{<:PortDesc})                              = Any

# the per-member materialised-ports NamedTuple TYPE (the cell's P / the entities view)
@generated function ports_nt_type(::Type{Ports}) where {Ports <: Tuple}
    Ds = (Ports.parameters...,); names = Tuple(descname(D) for D in Ds)
    types = [handle_type(D) for D in Ds]
    return :( NamedTuple{$names, Tuple{$(types...)}} )
end

# a paramless member defaults to this shared empty @parameters struct, so the node-core's
# ParameterServer carrier is uniform and parameters(node, m) returns a value, not nothing.
@parameters struct EmptyParams end

# ── MemberSchema: S=state type, P=param schema type (EmptyParams if none), Req/Prov=DI evidence as
#    Tuple{Type{I}…}, Ctor=the construct callable. ports/ctor are the only fields; the rest ride the
#    type so node() can freeze a plan and the @generated builders can read it. ─────────────────────
struct MemberSchema{S, Ports <: Tuple, P, Req <: Tuple, Prov <: Tuple, Ctor}
    ports::Ports
    ctor::Ctor
end

struct DefaultCtor{S} end                                    # non-DI default: build S{Name}()
(::DefaultCtor{S})(node, ::Val{Name}) where {S, Name} = S{Name}()

# DI default when DI is declared via the trait surface (`requires(::Type{S})`/`construct(::Type{S}, …)`)
# rather than a `ctor=` kwarg: route the frozen-plan ctor call to `construct`, which receives the
# resolved deps positionally (`construct(::Type{S}, node, ::Val{Name}, deps…)`). With no user/@component
# method, this lands on the DEFAULT `construct` that builds `S{Name, typeof.(deps)…}(deps…)` from the type.
struct ConstructAdapter{S} end
(::ConstructAdapter{S})(node, args...) where {S} = construct(S, node, args...)

function _evidence(t::Tuple)
    for x in t
        x isa Type || error("node: a `requires`/`provides` entry must be an @interface or component TYPE; " *
            "got `$(x)`. Pin pairs (`I => :member`) are not supported — restructure so a single sibling " *
            "provides the requirement.")
    end
    return Tuple{map(x -> Type{x}, t)...}
end

# Ports are PortDesc values OR bare @service/@action handlers (converted via _to_port, functor_authoring.jl).
"""
    component(State, [Params,] ports…; provides=(), requires=(), ctor=nothing) -> MemberSchema

Tie a component's `State` type and an optional `Params` ([`@parameters`](@ref)) type to its ports — the
`publishes`/`hears`/`serves`/`runs`/`every`/`uses` descriptors, plus bare [`@service`](@ref)/[`@action`](@ref)
handlers. The result is the component's [`member_schema`](@ref). Drop `Params` for a component with no
public parameters. `provides`/`requires` declare the component's dependency-injection evidence and `ctor`
its injected constructor (see [Parametric Components](parametric.md)).
"""
component(::Type{S}, ports...; kw...) where {S} = _component(S, EmptyParams, map(_to_port, ports); kw...)
component(::Type{S}, ::Type{P}, ports...; kw...) where {S, P} = _component(S, P, map(_to_port, ports); kw...)
function _component(::Type{S}, ::Type{P}, ports::Tuple;
                    requires::Tuple = (), provides::Tuple = (), ctor = nothing) where {S, P}
    # DI evidence comes from the kwargs, falling back to the trait surface when a kwarg is omitted:
    # `requires(::Type{S})`/`provides(::Type{S})` (set via `@provides` or a plain method) and, for the
    # ctor, `construct(::Type{S}, …)`. An explicit kwarg / `ctor=` always wins.
    req  = isempty(requires) ? _declared_requires(S) : requires
    prov = isempty(provides) ? _declared_provides(S) : provides
    # Validate the `requires`/`provides` ENTRIES first (must be @interface/component TYPEs, no `=>` pins) —
    # a malformed entry is the more fundamental error, surfaced before the ctor-arity checks below.
    Req = _evidence(req); Prov = _evidence(prov)
    c = ctor !== nothing ? ctor : (isempty(req) ? DefaultCtor{S}() : ConstructAdapter{S}())
    # Validate an EXPLICIT ctor's arity WHERE it's declared: it must accept (node, ::Val{Name}, deps…), one
    # dep per requirement — else the mismatch only surfaces deep inside the @generated build_members at run().
    # No `ctor=` is fine: DI routes through `construct(::Type{S}, node, ::Val{Name}, deps…)` — a user/@component
    # method if defined, else the default that builds `S{Name, typeof.(deps)…}(deps…)` from the type itself.
    # Method.nargs = func + node + ::Val{Name} + deps.
    if !isempty(req) && ctor !== nothing
        # arity-only (deps are often typed, e.g. `s::Sensor`, so a typed `hasmethod` probe false-positives).
        want = 3 + length(req)
        any(m -> m.nargs == want || m.isva, methods(ctor)) || throw(ArgumentError(
            "component($(S)): the `ctor` has no (node, ::Val{Name}, $(length(req)) dep(s)) method " *
            "to match its `requires` ($(length(req))); a DI ctor is `f(node, ::Val{Name}, deps...)`"))
    end
    return MemberSchema{S, typeof(ports), P, Req, Prov, typeof(c)}(ports, c)
end

statetype(::Type{<:MemberSchema{S}}) where {S} = S
paramtype(::Type{<:MemberSchema{S, Po, P}}) where {S, Po, P} = P
_markers(::Type{Tuple{}}) = ()
_markers(::Type{T}) where {T <: Tuple} = map(p -> p.parameters[1], (T.parameters...,))
requires_of(::Type{<:MemberSchema{S, Po, P, Req}}) where {S, Po, P, Req} = _markers(Req)
provides_of(::Type{<:MemberSchema{S, Po, P, Req, Prov}}) where {S, Po, P, Req, Prov} = _markers(Prov)

# ── public introspection: read a descriptor / schema without indexing type params or struct fields ──
"`message_type(d)` — the ROS message type a `publishes`/`hears` port carries."
message_type(::Pub{N, T}) where {N, T}    = T
message_type(::Sub{N, T, F}) where {N, T, F} = T
"`port_name(d)` — a port descriptor's name (the key in the member's `entities` NamedTuple)."
port_name(d::PortDesc) = descname(typeof(d))
"`port_names(ms)` — the declared port names of a `MemberSchema` (`member_schema(S)`), in order."
port_names(ms::MemberSchema) = Symbol[descname(typeof(d)) for d in ms.ports]
"`param_names(ms)` — the parameter field names of a `MemberSchema`'s `@parameters` schema."
param_names(ms::MemberSchema) = collect(fieldnames(paramtype(typeof(ms))))
"`port(ms, name)` — the descriptor for port `name` in a `MemberSchema`, or `nothing`."
port(ms::MemberSchema, name::Symbol) = (i = findfirst(d -> descname(typeof(d)) === name, ms.ports);
                                        i === nothing ? nothing : ms.ports[i])
# `member_names(::NodeSchema)` is defined after the NodeSchema struct (below).

# ── DI: data-driven resolver + toposort; node() runs them ONCE in the normal world ───────────────
_basetype(::Type{T}) where {T} = Base.typename(T).wrapper

function resolve_di_edges(names::Vector{Symbol}, reqs::Vector, provs::Vector, bases::Vector)
    providers = Dict{Any, Vector{Symbol}}()
    for (i, ps) in enumerate(provs), I in ps
        push!(get!(Vector{Symbol}, providers, I), names[i])
    end
    by_mixin = Dict{Any, Vector{Symbol}}()
    for (i, b) in enumerate(bases)
        push!(get!(Vector{Symbol}, by_mixin, b), names[i])
    end
    edges = Dict{Symbol, Vector{Symbol}}()
    for (i, rs) in enumerate(reqs)
        deps = Symbol[]
        for I in rs
            cands = if I isa Type && I <: ComponentInterface
                filter(!=(names[i]), get(providers, I, Symbol[]))
            elseif I isa Type && I <: Component
                filter(!=(names[i]), get(by_mixin, _basetype(I), Symbol[]))
            else
                error("node: member `$(names[i])` requires `$I`, which is neither an @interface nor a component type")
            end
            isempty(cands) && error("node: member `$(names[i])` requires $I, but no sibling provides/is it")
            length(cands) == 1 || error("node: member `$(names[i])` requires $I, matched by multiple: $cands")
            push!(deps, cands[1])
        end
        edges[names[i]] = deps
    end
    return edges
end

function toposort(names::Vector{Symbol}, edges::Dict{Symbol, Vector{Symbol}})
    color = Dict(n => :white for n in names); order = Symbol[]
    function visit(n, stack)
        color[n] === :black && return
        color[n] === :gray && error("node: dependency cycle: $(join(vcat(stack, n), " ▸ "))")
        color[n] = :gray
        for d in edges[n]; visit(d, vcat(stack, n)); end
        color[n] = :black; push!(order, n)
    end
    for n in names; visit(n, Symbol[]); end
    return order
end

# ── wire resolution + clobber (port of _resolve_wires/_check_clobbers over schema descriptors) ────
# Resolve every member port's wire name: the remap target if any, else the descriptor default. A
# cross-member remap (port => (member, port)) resolves by fixpoint. Returns (wires, explicitly-remapped).
function functor_resolve_wires(names::Vector{Symbol}, schemas::Vector, rmaps::Dict{Symbol, Vector{Pair{Symbol, Any}}})
    wires    = Dict{Symbol, Dict{Symbol, String}}()
    portset  = Dict{Symbol, Set{Symbol}}()
    refs     = Dict{Tuple{Symbol, Symbol}, Tuple{Symbol, Symbol}}()
    remapped = Set{Tuple{Symbol, Symbol}}()
    for (nm, ms) in zip(names, schemas)
        pnames = Symbol[_dname(d) for d in ms.ports]
        portset[nm] = Set(pnames)
        rmap = Dict{Symbol, Any}(get(rmaps, nm, Pair{Symbol, Any}[]))
        for (port, _) in rmap
            port in portset[nm] || error("node: member `$(nm)` remaps unknown port `$(port)`")
        end
        w = Dict{Symbol, String}()
        for d in ms.ports
            pn = _dname(d); t = get(rmap, pn, nothing)
            if t === nothing
                w[pn] = _port_wire(d)
            elseif t isa AbstractString
                w[pn] = String(t); push!(remapped, (nm, pn))
            else
                refs[(nm, pn)] = (Symbol(t[1]), Symbol(t[2])); push!(remapped, (nm, pn))
            end
        end
        wires[nm] = w
    end
    remaining = Set(keys(refs))
    while !isempty(remaining)
        progressed = false
        for mp in collect(remaining)
            refm, refport = refs[mp]
            haskey(portset, refm) || error("node: member `$(mp[1])` remaps `$(mp[2])` to unknown member `$(refm)`")
            refport in portset[refm] || error("node: member `$(mp[1])` remaps `$(mp[2])` to `$(refm).$(refport)`, not a port")
            if haskey(wires[refm], refport)
                wires[mp[1]][mp[2]] = wires[refm][refport]; delete!(remaining, mp); progressed = true
            end
        end
        progressed || error("node: unresolvable or cyclic wire remap among $(collect(remaining))")
    end
    return wires, remapped
end

# the (bucket, resolved-wire-name) keys a port occupies on the wire, for accidental-clobber detection.
# Pub/Srv take one; an Act EXPANDS to its five wire endpoints (3 services + 2 topics) via the same
# `_action_descs` the a-priori graph uses, so a sibling port colliding with an action's internal
# `<base>/_action/…` keyexpr is caught too (the bare action base is not itself a wire endpoint).
# Sub/Tmr/Use (inputs/clients) own no output name → no keys.
_act_type(::Act{N, A, F}) where {N, A, F} = A
_clobber_keys(d::Pub, node, wire) = Tuple{Symbol, String}[(:topic,   resolve_name(node, wire; kind = :topic))]
_clobber_keys(d::Srv, node, wire) = Tuple{Symbol, String}[(:service, resolve_name(node, wire; kind = :service))]
_clobber_keys(d::Act, node, wire) =
    Tuple{Symbol, String}[(e.kind === Publisher ? :topic : :service, e.topic) for e in _action_descs(node, wire, _act_type(d))]
_clobber_keys(::PortDesc, node, wire) = Tuple{Symbol, String}[]

# error on two ports resolving to one (bucket, name) unless at least one was explicitly remapped.
function functor_check_clobbers(order::Vector{Symbol}, members, node, wires, remapped)
    seen = Dict{Tuple{Symbol, String}, Tuple{Symbol, Symbol}}()
    for nm in order, d in getfield(members, nm).ports
        pn = _dname(d)
        cur_remapped = (nm, pn) in remapped
        for key in _clobber_keys(d, node, wires[nm][pn])
            if haskey(seen, key)
                prev = seen[key]
                (prev in remapped || cur_remapped) ||
                    error("node: members `$(prev[1]).$(prev[2])` and `$(nm).$(pn)` both resolve to $(key[2]) — " *
                          "unintended clobber; remap one (e.g. `remap(M, $(pn) => \"…\")`) or rename.")
                # Suppressed (a remap excuses this collision). Don't let a remapped occupant keep shadowing
                # the slot and mask a LATER un-remapped collider: if THIS port is not remapped, make it the
                # live occupant so the next un-remapped port on this name errors.
                cur_remapped || (seen[key] = (nm, pn))
            else
                seen[key] = (nm, pn)
            end
        end
    end
    return nothing
end

# the node schema: members (declared order); the resolved plan frozen as data (order/edges/wires) AND
# in the TYPE (Order/DepIdx params, for the @generated build_members).
struct NodeSchema{Members <: NamedTuple, Order, DepIdx}
    members::Members
    order::Vector{Symbol}
    edges::Dict{Symbol, Vector{Symbol}}
    wires::Dict{Symbol, Dict{Symbol, String}}
    remapped::Set{Tuple{Symbol, Symbol}}
    name::Union{Symbol, Nothing}        # registry key / default instance name; nothing = anonymous (run-only)
end

"`member_schema(::Type{S}) -> MemberSchema` — the schema associated with a member state type (the trait that lets `node(\"name\" => S, …)` carry one name per component)."
function member_schema end
_asschema(x::MemberSchema) = x
function _asschema(::Type{T}) where {T}
    # member_schema MUST be defined on the bare component BASE (`::Type{S}`), not a `{Name}`
    # instantiation — node()/_basetype resolve it via the base. Catch the footgun with a clear error.
    hasmethod(member_schema, Tuple{Type{T}}) || throw(ArgumentError(
        "node: no `member_schema(::Type{$(T)})` — define it on the bare component base, " *
        "`member_schema(::Type{$(Base.typename(T).wrapper)}) = component(...)` (not on a `{Name}` instantiation)"))
    return member_schema(T)
end

"""
    node("name" => StateOrSchema, …; name=nothing, register=(name !== nothing)) -> NodeSchema

Assemble a node kind. Each member is `"name" => S` (a state type whose `member_schema(S)` gives its
`MemberSchema`) or `"name" => component(…)` directly. DI is resolved ONCE here, in the normal
world; the resolved member order + dependency edges are frozen into the returned `NodeSchema`'s type
so the construction is type-stable with no runtime `Core.eval`.

`name` (a `Symbol`/`String`) is the node KIND's name — the key it is registered under in the
process-global node-kind registry, and its default instance name. When `name` is given the kind is
**registered by default** (the `rclcpp_components_register_nodes` analog, via
[`register_node_kind!`](@ref)), so a container's [`load_node`](@ref) / `ros2 component load` can
instantiate it by name; pass `register=false` to build the schema without registering, or `name`
with `register=true` to force it.

Immediate registration is skipped while a package is generating precompile output
(`jl_generating_output`) — a top-level mutation of the registry would not survive precompilation.
A plain `node(…)` call cannot inject the module roster + `__init__` load hook, so a **precompiled
package must re-register its `const` schemas at load** from its own
`__init__`, e.g. `__init__() = register_node_kind!("Rig", Rig)`. In the REPL, scripts, and runtime
(including dynamic composition), registration is immediate and needs no `__init__`.
"""
function node(pairs::Pair...; name::Union{Nothing, Symbol, AbstractString} = nothing,
              register::Union{Bool, Nothing} = nothing, resolve_di::Bool = true)
    # `register === nothing` ⇒ the implicit default (register iff a `name` is given). Tracking
    # explicitness lets us warn ONLY for an explicit `register=true` under precompile (below): the
    # implicit named-default is almost always paired with `@register_nodes` / a BYO `__init__` that
    # re-registers at load, so warning there is a false-positive on the documented workflow.
    explicit_reg = register === true
    do_register  = register === nothing ? (name !== nothing) : register
    names   = Symbol[]; schemas = Any[]; rmaps = Dict{Symbol, Vector{Pair{Symbol, Any}}}()
    for p in pairs
        nm = Symbol(p.first); (sx, rs) = _unwrap(p.second)
        push!(names, nm); push!(schemas, _asschema(sx)); rmaps[nm] = rs
    end
    allunique(names) || throw(ArgumentError("node: duplicate member name(s) " *
        "$(unique(n for n in names if count(==(n), names) > 1)) — each member needs a distinct key"))
    if resolve_di
        reqs  = [requires_of(typeof(s)) for s in schemas]
        provs = [provides_of(typeof(s)) for s in schemas]
        bases = Any[_basetype(statetype(typeof(s))) for s in schemas]
        edges = resolve_di_edges(names, reqs, provs, bases)
        order = toposort(names, edges)
    else                                       # promote path: no siblings to resolve; zero-dep ctors
        edges = Dict(n => Symbol[] for n in names)
        order = copy(names)
    end
    wires, remapped = functor_resolve_wires(names, schemas, rmaps)
    members = NamedTuple{(names...,)}((schemas...,))
    pos     = Dict(n => i for (i, n) in enumerate(order))
    depidx  = Tuple(Tuple(pos[d] for d in edges[n]) for n in order)
    kname   = name === nothing ? nothing : Symbol(name)
    schema  = NodeSchema{typeof(members), (order...,), depidx}(members, order, edges, wires, remapped, kname)
    if do_register
        kname === nothing &&
            throw(ArgumentError("node(...; register=true) needs a `name=` to register the kind under; " *
                                "pass `name=\"Foo\"`, or `register=false` for a run-only schema"))
        # Immediate in the REPL/script/runtime case. While generating precompile output the registry
        # mutation would not survive (discarded with ROSNode's deserialized state before the package
        # cache is written), and a plain `node(…)` — unlike the `@register_nodes` macro — cannot
        # inject a module roster + `__init__` load hook. We warn ONLY when the caller EXPLICITLY asked to
        # register (`register=true`): the implicit named-default is the case the documented `@register_nodes`
        # / BYO-`__init__` workflow handles, so warning there would false-positive on the correct path.
        if ccall(:jl_generating_output, Cint, ()) == 1
            explicit_reg && @warn "node(…; register=true, name=$(repr(String(kname)))) cannot register a kind " *
                "under precompilation — the registry mutation is discarded with ROSNode's state. Re-register " *
                "at load: `@register_nodes` (auto-installs an `__init__`) or `register_node_kinds!(...)` / " *
                "`register_node_kind!($(repr(String(kname))), schema)` from your module's `__init__`."
        else
            register_node_kind!(String(kname), schema)
        end
    end
    return schema
end

# The kind's name (registry key + default instance name) for the composition layer
# (`load_node`/`add!`/the wire `~/_container/load_node` service). Anonymous schemas error.
_kind_default_name(s::NodeSchema) =
    s.name === nothing ?
        throw(ArgumentError("run/load of a NodeSchema needs a name: the schema is anonymous — " *
                            "build it with `node(…; name=\"Foo\")` or pass `name=` to `run`/`add!`")) :
        String(s.name)

"`member_names(s)` — the member names of a `NodeSchema` (declared order)."
member_names(s::NodeSchema) = collect(s.order)

"""
    register_node_kinds!(schema, …) -> nothing

Eagerly register one or more named functor node-kind schemas in the process-global registry (keyed by
each schema's `.name`), making them loadable by name (`load_node` / `ros2 component load`). Call this
from a consumer's `__init__` when you bring your own `__init__` (it runs at package LOAD, so the
registration survives precompilation). For the common case use [`@register_nodes`](@ref), which also
auto-installs `__init__`.
"""
function register_node_kinds!(schemas::NodeSchema...)
    for s in schemas
        s.name === nothing &&
            throw(ArgumentError("register_node_kinds!: an anonymous schema can't be registered; build it with `node(…; name=\"Foo\")`"))
        register_node_kind!(String(s.name), s)
    end
    return nothing
end

"""
    @register_nodes Rig Counter …

Expose functor node-kind schemas for load-by-name (`ros2 component load` / a container's `load_node`).
Pushes each onto the module's `__node_kinds__` roster (drained at load by [`ros_init!`](@ref), so the
registration survives precompilation) and installs `__init__() = ros_init!(@__MODULE__)` **iff** the
module has none. If you bring your own `__init__`, this still rosters the kinds — call
`ROSNode.ros_init!(@__MODULE__)` inside it (the same hook that absorbs `@ros_message`/`@service` types),
or `register_node_kinds!(Rig, Counter)` directly.
"""
macro register_nodes(schemas...)
    modu = __module__
    roster = esc(:__node_kinds__)
    pushes = [:( let s = $(esc(sx))
                     s.name === nothing && throw(ArgumentError(
                         "@register_nodes: a schema is anonymous and can't be registered by name; build it with `node(…; name=\"Foo\")`"))
                     local k = String(s.name); local nk = $roster
                     local i = findfirst(e -> e[1] == k, nk)
                     i === nothing ? push!(nk, (k, s)) : (nk[i] = (k, s))   # replace-by-name (Revise-idempotent)
                 end ) for sx in schemas]
    return quote
        if !isdefined($modu, :__node_kinds__)
            $roster = $(Tuple{String, Any})[]
        end
        $(pushes...)
        if ccall(:jl_generating_output, Cint, ()) == 0          # eager in REPL/runtime; skipped under precompile
            for (k, s) in $roster; $(register_node_kind!)(k, s); end
        end
        if !isdefined($modu, :__init__)
            $(esc(:__init__))() = $(ros_init!)($modu)
        end
        nothing
    end
end

# the dep-threaded member constructor: reads the frozen Order/DepIdx from the TYPE (never the generics),
# threads each member's resolved deps positionally. Returns the typed member NamedTuple.
@generated function build_members(ns::NodeSchema{Members, Order, DepIdx}, node) where {Members, Order, DepIdx}
    locs = [Symbol(:_m, i) for i in eachindex(Order)]
    body = Expr(:block)
    for (k, name) in enumerate(Order)
        deps = Any[locs[j] for j in DepIdx[k]]
        push!(body.args, :( $(locs[k]) =
            getfield(getfield(ns, :members), $(QuoteNode(name))).ctor(node, $(Val(name)), $(deps...)) ))
    end
    push!(body.args, :( NamedTuple{$Order}(($(locs...),)) ))
    return body
end

# ── typed carriers (the shared @generated tuple builders — survive the 32-cutoff plain map hits) ──
@generated function node_ports_carrier(schema::NodeSchema{Members}) where {Members}
    nms = Members.parameters[1]; MSs = (Members.parameters[2].parameters...,)
    vals = [:( PortCell{$(ports_nt_type(MS.parameters[2]))}() ) for MS in MSs]
    return :( NamedTuple{$nms}(($(vals...),)) )
end

@generated function build_pservers(schema::NodeSchema{Members}, node, overrides) where {Members}
    nms = Members.parameters[1]; MSs = (Members.parameters[2].parameters...,)
    vals = [:( _build_pserver(node, $(MS.parameters[3]),
                 _filter_overrides($(MS.parameters[3]), $(QuoteNode(nm)), overrides)) ) for (nm, MS) in zip(nms, MSs)]
    return :( NamedTuple{$nms}(($(vals...),)) )
end
function _filter_overrides(::Type{P}, member::Symbol, overrides::NamedTuple) where {P}
    fns = fieldnames(P); out = Dict{Symbol, Any}()
    for (k, v) in pairs(overrides); k in fns && (out[k] = v); end
    prefix = string(member, ".")
    for (k, v) in pairs(overrides)
        ks = String(k)
        if startswith(ks, prefix)
            f = Symbol(SubString(ks, ncodeunits(prefix) + 1)); f in fns && (out[f] = v)
        end
    end
    return (; out...)
end

# ── materialise: per-port handle via construct_port (the bridge to the substrate); @generated ────
# A mid-materialise throw is cleaned up by the caller via the node.entities snapshot (every handle a
# construct_port builds — and every entity a pattern ctor registers before it returns — lands in
# node.entities, so closing the tail reclaims partial work the NamedTuple never captured).
@generated function materialize_ports(ms::MemberSchema{S, Ports}, cnode, m, pv, wm) where {S, Ports}
    Ds = (Ports.parameters...,); names = Tuple(descname(D) for D in Ds)
    locs = [Symbol(:_h, i) for i in 1:length(Ds)]
    body = Expr(:block)
    for i in 1:length(Ds)
        push!(body.args, :( $(locs[i]) = construct_port(getfield(ms, :ports)[$i], getfield(cnode, :node), cnode, m, pv, wm) ))
    end
    push!(body.args, :( NamedTuple{$names}(($(locs...),)) ))
    return body
end

_dname(d::PortDesc) = descname(typeof(d))
_port_wire(d::PortDesc) = (hasfield(typeof(d), :wire) && d.wire !== nothing) ? d.wire : String(_dname(d))
_wirename(d::PortDesc, wm) = get(wm, _dname(d), _port_wire(d))   # remap override, else the descriptor default
_hz(d::Tmr, pv) = d.rate isa Symbol ? Float64(getproperty(pv, d.rate)) : d.rate

# retain the _sub_cb/_timer_cb function barriers (concrete callback type, recoverable for anchors)
construct_port(d::Pub{N, T},         core, cn, m, pv, wm) where {N, T}         = Publisher(core, _wirename(d, wm), T)
construct_port(d::Sub{N, T, F},      core, cn, m, pv, wm) where {N, T, F}      = Subscription(_sub_cb(d.handler, cn, m), core, _wirename(d, wm), T; warmup = :off)
construct_port(d::Srv{N, Rq, Rs, F}, core, cn, m, pv, wm) where {N, Rq, Rs, F} = Service(_sub_cb(d.handler, cn, m), core, _wirename(d, wm), Rq; warmup = :off)
construct_port(d::Tmr{N, F},         core, cn, m, pv, wm) where {N, F}         = _paused_timer(core, Duration(round(Int64, 1.0e9 / _hz(d, pv))), _timer_cb(d.handler, cn, m))
construct_port(d::Act{N, A, F},      core, cn, m, pv, wm) where {N, A, F}      = _make_action_server(core, _wirename(d, wm), A; body = _action_body(d.handler, cn, m), warmup = :off)
_action_body(h, cn, m) = goal -> h(cn, m, goal)         # the do-block exec, node-first over the GoalHandle

# ── a-priori graph priming (Stage B over the schema descriptors) ─────────────────────────────────
port_descs(d::Pub{N, T},         node, w) where {N, T}        = EndpointDesc[_publisher_desc(node, w, T)]
port_descs(d::Sub{N, T, F},      node, w) where {N, T, F}     = EndpointDesc[_subscription_desc(node, w, T)]
port_descs(d::Srv{N, Rq, Rs, F}, node, w) where {N, Rq, Rs, F} = EndpointDesc[_service_desc(node, w, Rq)]
port_descs(d::Act{N, A, F},      node, w) where {N, A, F}     = _action_descs(node, w, A)
port_descs(d::Tmr, node, w)                                  = EndpointDesc[]

# enumerate the node's member-port endpoints in MATERIALISE order (DI order × spec order × action 1→5)
function ordered_schema_descs(schema::NodeSchema, node::Node)
    descs = EndpointDesc[]
    for nm in schema.order
        wm = get(schema.wires, nm, Dict{Symbol, String}())
        for d in getfield(schema.members, nm).ports
            append!(descs, port_descs(d, node, _wirename(d, wm)))
        end
    end
    return descs
end

# Populate the members' endpoints into the local graph a-priori on a reserved id block, arm the node's
# id queue so materialise lands on those ids (and skips the per-declare inject). Returns primed lvkeys
# for _reconcile_local_graph! to drop orphans on an aborted configure.
function prime_schema_graph!(cnode::ComponentNode, schema::NodeSchema)
    node = cnode.node; ctx = node.context
    descs = ordered_schema_descs(schema, node)
    isempty(descs) && return String[]
    base = reserve_entity_ids!(ctx, length(descs))
    lvkeys = String[]; infos = EndpointInfo[]
    @lock ctx.graph.lock for (i, d) in enumerate(descs)
        e    = _desc_entity(node, d, base + i - 1)
        k    = liveliness_keyexpr(ctx.format, e)
        info = _endpoint_info(e; is_local = true)
        ctx.graph.local_endpoints[k] = info
        push!(lvkeys, k); push!(infos, info)
    end
    _notify_graph_change!(ctx, infos, EndpointInfo[])
    @lock node.lock (empty!(node._id_queue); append!(node._id_queue, base:(base + length(descs) - 1)))
    return lvkeys
end

# Runtime warm: anchor the member's reaction handlers against the concrete node + member types once
# materialised, honouring the node's `WarmupPolicy` via `_warmup!` (off / inline-sync / background).
# Routes through `_anchor_functor_member!` over the FROZEN `MemberSchema` (the same precompile walk
# `precompile_schema` bakes ahead of time). It is precompile-only for every non-`:off` mode: per-port
# subscriptions/services/actions build
# with `warmup = :off`, and the `Execute()` sample-run leg lives only in the pattern `_warm_*` funcs
# (which are `:off`ed here), so `:precompile` and `:execute` both reduce to the same anchor.
function _warm_functor_member!(cnode::ComponentNode, m, ms::MemberSchema)
    _warmup!(cnode.node.warmup, () -> _anchor_functor_member!(typeof(cnode), typeof(m), ms))
    return nothing
end

# ── lifecycle fan-outs (shared by unmanaged run() and the managed LifecycleNode callbacks) ────────
# configure: prime the local graph, then per member [materialise ports + configure hook] in DI order.
# First-failure-wins — returns `failure` after rolling back the configured-so-far in reverse.
# close + drop every entity registered on `node` since it held `n0` entities (the per-member
# materialise window). Reclaims both fully-built port handles AND the partial entities a pattern ctor
# (e.g. an action server's ≤5 endpoints) registered before throwing — neither reachable via the cell.
function _close_member_entities!(node::Node, n0::Int)
    extra = @lock node.lock (n = length(node.entities); n > n0 ? node.entities[(n0 + 1):n] : Any[])
    for e in Iterators.reverse(extra)
        e isa Entity && (try; dispose(node, e); catch; end)
    end
    return nothing
end

function functor_members_configure!(cnode::ComponentNode, schema::NodeSchema)
    core = cnode.node
    primed = prime_schema_graph!(cnode, schema)
    try
        for (i, nm) in enumerate(schema.order)
            m = cnode.members[nm]
            core._materialising = true
            n0 = @lock core.lock length(core.entities)   # entity-set snapshot before THIS member materialises
            vals = try
                materialize_ports(getfield(schema.members, nm), cnode, m,
                                  current(getfield(cnode.pservers, nm)), get(schema.wires, nm, Dict{Symbol, String}()))
            catch
                # A construct_port threw mid-materialise. The cell never received the NamedTuple, and a
                # pattern ctor (e.g. an action server) may have registered several entities in
                # node.entities BEFORE throwing — unreachable via the cell, so _member_cleanup! can't see
                # them. Close + drop everything this member registered, then roll back the members
                # configured so far, so a managed :error->reconfigure loop can't accumulate orphans.
                _close_member_entities!(core, n0)
                foreach(j -> _member_cleanup!(cnode, schema.order[j]), (i - 1):-1:1)
                rethrow()
            finally
                core._materialising = false        # reset BEFORE the user hook (materialise window closes here)
            end
            getfield(cnode.ports, nm).v = vals
            # Warm the member's reaction handlers now its handles exist. No-op when the node WarmupPolicy is
            # `:off` (the default); the per-`run`/`@compile_workload` bake covers a deployed node.
            _warm_functor_member!(cnode, m, getfield(schema.members, nm))
            if configure(cnode, m) === failure
                foreach(j -> _member_cleanup!(cnode, schema.order[j]), i:-1:1)
                return failure
            end
        end
    finally
        _reconcile_local_graph!(core, primed)
    end
    return nothing
end

# halt a member's running timers on an UNMANAGED activate rollback — `close(Timer)` is the only stop a
# Timer exposes, and it latches `open=false` permanently (a re-`_start!` re-arms `impl` but `_fire!` stays
# gated off). That's fine unmanaged: a failed `run` tears the node down (no retry). A MANAGED node must
# NOT take this path — its `isactive` gate is already closed during a failed activate (the transition
# never reached `Active`), so its armed timers are silent without closing them, and a later retry can
# re-arm them. Hence this is gated to the unmanaged branch in the rollback below.
function _halt_member_timers!(cnode::ComponentNode, nm::Symbol)
    cell = getfield(cnode.ports, nm)
    cell.v === nothing && return nothing
    for h in values(cell.v); h isa Timer && (try; close(h); catch; end); end
    return nothing
end

# activate: per member [activate hook, THEN arm its paused timers] in DI order; first-failure-wins.
# Timers arm only after the hook returns non-failure, so a tick can never fire mid-hook or into a member
# whose activate failed. On failure, roll back the members already activated (cancel their goals, run
# deactivate; unmanaged also halts their now-running timers — managed relies on the closed gate) newest-first.
function functor_members_activate!(cnode::ComponentNode)
    unmanaged = cnode.lifecycle === nothing
    for (i, nm) in enumerate(cnode.order)
        m = cnode.members[nm]
        if activate(cnode, m) === failure
            for j in (i - 1):-1:1
                nmj = cnode.order[j]
                _member_cancel_goals!(cnode, nmj)
                unmanaged && _halt_member_timers!(cnode, nmj)
                _member_deactivate!(cnode, nmj)
            end
            return failure
        end
        for h in values(getfield(cnode.ports, nm).v); h isa Timer && _start!(h); end
    end
    return nothing
end

# ── run ──────────────────────────────────────────────────────────────────────────────────────────
"""
    run(schema::NodeSchema; name=<kind name>, namespace=nothing, ctx=nothing, overrides=(;),
        peers=String[], localhost_only=false, managed=false, autostart=!managed,
        log_level=nothing, block=true) -> ComponentNode

Instantiate a functor node: open/own a Context, build the node-core, construct members in DI order,
materialise ports (a-priori graph priming + the reserved-id window), and bring it up.

- `managed=false` (default): a plain `Node`; autostart runs configure → activate, no control surface.
- `managed=true`: a [`LifecycleNode`](@ref) — the lifecycle services + the `isactive` dispatch gate;
  starts `Unconfigured`, driven externally (`ros2 lifecycle`) unless `autostart=true`. Ports
  materialise at the configure transition (the fan-out runs from the lifecycle callbacks).

`name` defaults to the schema's kind name (from `node(…; name=…)`); an anonymous schema requires an
explicit `name`. `log_level` (a `LogLevel`, default `nothing` = the node default) sets the node's
logger level before any member code runs, so the configure/activate records honor it — this is the
hook the container's `~/_container/load_node` service drives from a `ros2 component load` request.

`block=true` (default, matching `run(::Type{<:Component})`) spins until the Context drains. A member hook
returning the [`failure`](@ref) token aborts the bring-up (unmanaged: torn down + error rethrown;
managed: the transition lands `:failure` and unwinds).
"""
function Base.run(schema::NodeSchema; ctx::Union{Context, Nothing} = nothing,
                  name::AbstractString = _kind_default_name(schema),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;), peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false, managed::Bool = false, autostart::Bool = !managed,
                  log_level::Union{LogLevel, Nothing} = nothing,
                  warmup::Union{Symbol, WarmupMode} = :off, warmup_sync::Bool = false,
                  on_reaction_error::Union{Symbol, ReactionErrorPolicy} = :shutdown,
                  flat::Bool = false, block::Bool = true)
    owns = ctx === nothing
    ctx  = owns ? Context(; peers = collect(String, peers), localhost_only = localhost_only,
                            on_reaction_error = on_reaction_error) : ctx
    cnode = nothing
    cnref = Ref{Any}(nothing)        # the managed transition callbacks reach the cnode through this
    try
        if managed
            ln = LifecycleNode(ctx, name; namespace = namespace, warmup = warmup, warmup_sync = warmup_sync,
                on_configure  = _ -> functor_members_configure!(cnref[], schema),
                on_activate   = _ -> functor_members_activate!(cnref[]),
                on_deactivate = _ -> (foreach(reverse(cnref[].order)) do nm
                                          _member_cancel_goals!(cnref[], nm); _member_deactivate!(cnref[], nm)
                                      end; nothing),
                on_cleanup    = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(cnref[].order)); nothing),
                on_shutdown   = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(cnref[].order)); nothing),
                on_error      = _ -> _members_on_error!(cnref[], cnref[].order))
            core = inner_node(ln)
        else
            ln = nothing
            core = Node(ctx, name; namespace = namespace, warmup = warmup, warmup_sync = warmup_sync)
        end
        log_level === nothing || set_logger_level!(core, log_level)   # before any member code runs
        members  = build_members(schema, core)
        pservers = build_pservers(schema, core, overrides)
        ports    = node_ports_carrier(schema)
        cnode = ComponentNode{typeof(members), typeof(ports), typeof(pservers)}(
                    core, members, ports, pservers, schema.order, schema.wires, schema.members,
                    ln, owns ? ctx : nothing, nothing, true)
        cnref[] = cnode
        functor_check_clobbers(schema.order, schema.members, core, schema.wires, schema.remapped)
        if flat && length(schema.order) == 1
            srv = getfield(pservers, schema.order[1])    # promote: the lone member's plain server → UN-prefixed params
            core.parameters = srv; wire_parameter_services!(srv)
        else
            facade = CompositeParameterServer(core,
                Pair{Symbol, ParameterServer}[nm => getfield(pservers, nm) for nm in schema.order])
            wire_parameter_services!(facade); _wire_composite_events!(facade); core.parameters = facade
        end

        if managed
            if autostart
                configure!(ln) === :success || @error "functor run: autostart configure! failed" node = name
                state(ln) === Inactive() &&
                    (activate!(ln) === :success || @error "functor run: autostart activate! failed" node = name)
            end
        else
            functor_members_configure!(cnode, schema) === failure &&
                error("functor run: a member's configure returned the failure token")
            functor_members_activate!(cnode) === failure &&
                error("functor run: a member's activate returned the failure token")
        end
        # Close the node when the Context drains. Retained so `close`/`unload_node` can deregister it
        # from a borrowed (e.g. container) ctx — see `close(::ComponentNode)`.
        cnode._shutdown_hook = on_shutdown(ctx) do
            try; close(cnode); catch err
                @error "functor run: teardown hook threw" node = name exception = (err, catch_backtrace())
            end
        end
    catch
        cnode === nothing || (try; close(cnode); catch; end)
        owns && close(ctx)
        rethrow()
    end
    if block
        try; spin(ctx; handle_signals = owns); finally; owns && close(ctx); end
    end
    return cnode
end

# Promote a single component to a standalone node: wrap `member_schema(S)` into a one-member node and
# run it. With `flat=true` (default) the node's parameters are UN-prefixed (the component's plain
# `ParameterServer`, so `ros2 param get /node fps`, not `node.fps`) — the idiomatic surface for a node
# that *is* one component. DI is skipped (a solo component has no siblings), so `S`'s ctor is called
# zero-dep: the default `S{Name}()` works; a `requires`-bearing component must supply a zero-dep ctor.
# Dispatched into from `run(::Type{M})` in run.jl when `M <: Component`; `add!`/`load_node` of a bare
# type route through that too.
function _run_functor_promote(::Type{S}; name::AbstractString = _default_name(S),
                              flat::Bool = true,
                              warmup::Union{Symbol, WarmupMode} = :off, warmup_sync::Bool = false,
                              kwargs...) where {S <: Component}
    ms = _asschema(S)        # clear error if S has no `member_schema` method
    isempty(requires_of(typeof(ms))) || throw(ArgumentError(
        "run($(S)): cannot promote a `requires`-bearing component standalone — it has no sibling to satisfy " *
        "DI; compose it in `node(\"name\" => $(S), provider => …)`, or author a no-`requires` schema with a zero-dep ctor"))
    schema = node(Symbol(_default_name(S)) => ms; register = false, resolve_di = false)
    return run(schema; name = name, flat = flat, warmup = warmup, warmup_sync = warmup_sync, kwargs...)
end

# ── precompile_schema: bake the first-run residue precompile(run) can't reach ─────────────────────
# Two gaps: (1) `run` threads cnode through a `Union{Nothing,CN}`-typed local, so inference never
# specializes the lifecycle fan-out (construct/materialise/configure/activate/teardown) on the concrete
# `ComponentNode` — anchor it on the derived `CN`; (2) the spawned consume/serve/dispatch bodies live
# behind `@spawn`, unreachable from `precompile(run)` — port `_anchor_reactions!`'s per-(T,H) chain to
# the functor descriptors. `precompile_schema(typeof(schema))` belongs in a consumer `@compile_workload`
# (alongside `precompile(run, (typeof(schema),))`).
_anchor_port(::Type{Pub{N, T}}) where {N, T} =
    (precompile(encode, (T,)); precompile(_publisher_desc, (Node, String, Type{T}));
     precompile(publish, (handle_type(Pub{N, T}), T)); nothing)   # the monomorphic send the first publish() JITs
_anchor_port(::Type{<:PortDesc}) = nothing

# the sub receive→dispatch chain the first message JITs, keyed on the concrete `_sub_cb` closure `H`
function _anchor_functor_sub!(::Type{CN}, ::Type{Mc}, ::Type{T}, @nospecialize(reaction)) where {CN, Mc, T}
    precompile(reaction, (CN, Mc, T))
    H = _cb_type(_sub_cb, reaction, CN, Mc); H === nothing && return nothing
    precompile(_consume_loop,        (Entity, Type{T}, H, Owned, Zenoh.SubscriberHandler, Nothing))
    precompile(_dispatch_decoded,    (Entity, SampleHolder, Type{T}, H, Owned))
    precompile(decode_owned,         (Memory{UInt8}, Type{T}))
    precompile(_decode_on_consumer,  (Entity, SampleHolder, Type{T}))
    precompile(_invoke_owned,        (Entity, T, H))
    precompile(with_payload_memory,  (_OwnedRun{T, H}, SampleHolder))
    precompile(with_payload_memory,  (_OwnedRun{T, H}, Zenoh.ZBytes{Ptr{Zenoh.LibZenohC.z_loaned_bytes_t}}))
    # the INTRA-PROCESS delivery leg (set_intra_process!(true), the container default): the subscription
    # is a LocalSubscription whose `@spawn`ed worker runs `_local_consume_loop` → `_invoke_local` →
    # handler. Behind @spawn, so unreachable from precompile(run); it dominated the baked first-run JIT.
    LS = LocalSubscription{T, H}
    precompile(LS, (Entity, QosProfile, H, Bool, Serial, Channel{Any}))   # the registry ctor (register_local_subscription!)
    precompile(_local_consume_loop, (LS,))
    precompile(_invoke_local,       (LS, T))
    precompile(_ipc_copy,           (T,))
    return nothing
end
# the service request-decode → handler → response-encode serve tree, keyed on Req/Resp + the closure
function _anchor_functor_srv!(::Type{CN}, ::Type{Mc}, ::Type{Req}, ::Type{Resp}, @nospecialize(reaction)) where {CN, Mc, Req, Resp}
    precompile(reaction, (CN, Mc, Req))
    try
        precompile(decode_owned,         (Memory{UInt8}, Type{Req}))
        precompile(decode_view,          (Zenoh.PayloadView, Type{Req}))
        precompile(encode,               (Resp,))
        precompile(service_type_info_of, (Type{Req}, Type{Resp}))
        Q = Zenoh.Query{Base.RefValue{Zenoh.LibZenohC.z_owned_query_t}}
        precompile(_serve_query,            (Entity, Q, Type{Req}, Type{Resp}, Function, Bool))
        precompile(_spawn_service_consumer, (Entity, Type{Req}, Type{Resp}, Function, Bool, Serial))
        Hs = _cb_type(_sub_cb, reaction, CN, Mc)
        if Hs !== nothing
            Ss = Base.return_types(_service_scheduler, (Serial, Entity, Type{Req}, Type{Resp}, Hs, Bool))
            length(Ss) == 1 && isconcretetype(only(Ss)) &&
                precompile(_service_consume_loop, (only(Ss), Zenoh.QueryableHandler, Entity))
        end
    catch
    end
    return nothing
end

# the action codec set (goal/result/feedback + the five protocol wrappers + CancelGoal), keyed on the
# action marker type A, plus the per-goal exec — lives behind the @spawn boundary, so precompile(run)
# can't reach it.
function _anchor_functor_action!(::Type{CN}, ::Type{Mc}, ::Type{A}, @nospecialize(exec)) where {CN, Mc, A}
    try
        support = ActionTypeSupport(A)
        G, R, Fb = goal_type(support), result_type(support), feedback_type(support)
        precompile(decode_owned, (Memory{UInt8}, Type{G}))
        precompile(encode, (R,)); precompile(encode, (Fb,))
        precompile(decode_owned, (Memory{UInt8}, Type{_send_goal_request_type(A)}))
        precompile(encode,       (_send_goal_response_type(A),))
        precompile(decode_owned, (Memory{UInt8}, Type{_get_result_request_type(A)}))
        precompile(encode,       (_get_result_response_type(A),))
        precompile(encode,       (_feedback_message_type(A),))
        precompile(decode_owned, (Memory{UInt8}, Type{_CancelGoal_Request}))
        precompile(encode,       (_CancelGoal_Response,))
        precompile(exec, (CN, Mc, GoalHandle{A, G, R, Fb}))            # the per-goal node-first exec
    catch
    end
    return nothing
end

# walk a member's FROZEN descriptors and anchor the spawned-task body per kind (codec leaves rode
# `_anchor_port`). Takes the node's actual `MemberSchema` so the anchored handlers match what `run`
# materialises — incl. inline `component(…)` members (no `member_schema` method) and rebound/remapped ones.
function _anchor_functor_member!(::Type{CN}, ::Type{Mc}, ms::MemberSchema) where {CN, Mc}
    for d in ms.ports
        D = typeof(d)
        if d isa Sub
            _anchor_functor_sub!(CN, Mc, D.parameters[2], d.handler)
        elseif d isa Srv
            _anchor_functor_srv!(CN, Mc, D.parameters[2], D.parameters[3], d.handler)
        elseif d isa Tmr
            precompile(d.handler, (CN, Mc))
        elseif d isa Act
            _anchor_functor_action!(CN, Mc, D.parameters[2], d.handler)
        end
    end
    return nothing
end

# the per-descriptor codec/wire/close/cell anchors, from the schema TYPE alone (no handler values needed)
function _anchor_schema_codecs(::Type{NS}) where {NS <: NodeSchema}
    for MS in (NS.parameters[1].parameters[2].parameters...,)
        Ports = MS.parameters[2]
        for D in (Ports.parameters...,)
            _anchor_port(D)                                              # encode / _publisher_desc (codec leaves)
            precompile(port_descs, (D, Node, String))                   # a-priori graph priming enumeration
            precompile(_wirename,  (D, Dict{Symbol, String}))           # wire resolution at materialise
            Hd = handle_type(D); isconcretetype(Hd) && precompile(close, (Hd,))   # per-handle teardown
        end
        NTm = ports_nt_type(Ports)
        precompile(setproperty!, (PortCell{NTm}, Symbol, NTm))          # the `cell.v = handles` write
    end
    return nothing
end

# derive the concrete (CN, MembersT) from the @generated builders' return types, or nothing if uninferable
function _derive_cn_mc(::Type{NS}) where {NS <: NodeSchema}
    mt = Base.return_types(build_members, (NS, Node));   (length(mt) == 1 && isconcretetype(only(mt))) || return nothing
    pt = Base.return_types(node_ports_carrier, (NS,));    length(pt) == 1 || return nothing
    st = Base.return_types(build_pservers, (NS, Node, NamedTuple{(), Tuple{}})); length(st) == 1 || return nothing
    MembersT = only(mt)
    return (ComponentNode{MembersT, only(pt), only(st)}, MembersT)
end

function _precompile_lifecycle!(::Type{CN}, ::Type{NS}) where {CN, NS}
    precompile(functor_members_configure!, (CN, NS))
    precompile(functor_members_activate!,  (CN,))
    precompile(Base.close, (CN,))
    return nothing
end

"""
    precompile_node(schema::NodeSchema)
    precompile_node(::Type{<:NodeSchema})

Bake a functor node's first-`run` path into the calling package's precompile image. Call it from a
consumer `@compile_workload` (PrecompileTools) so the node's `run` specialization, the `@generated`
carrier builders, the lifecycle fan-out, and the spawned consume/serve/dispatch bodies compile ahead
of time — turning a cold first `run` into a near-warm one. Combines `precompile(run, (typeof(schema),))`
with the `precompile_schema` spawned-task/reaction walker.

```julia
using ROSNode, PrecompileTools
const Rig = node("a" => A, "b" => B; name = "Rig")
@compile_workload precompile_node(Rig)
```
"""
precompile_node(s::NodeSchema) = (precompile(Base.run, (typeof(s),)); precompile_schema(s); nothing)
# Prefer the VALUE form (`precompile_node(Rig)`): it anchors reaction bodies from the node's FROZEN
# handlers, so inline `component(…)` members and rebound/remapped handlers bake correctly. The type form
# is best-effort (no handler values → reaction anchors fall back to a guarded `member_schema(base)`).
function precompile_node(::Type{NS}) where {NS <: NodeSchema}
    precompile(Base.run, (NS,))
    precompile_schema(NS)
    return nothing
end

# canonical (value): the per-descriptor codec anchors ride the schema TYPE; the handler-bearing reaction
# anchors ride the FROZEN per-member MemberSchema — exactly what `run` materialises.
function precompile_schema(s::NodeSchema)
    NS = typeof(s)
    _anchor_schema_codecs(NS)
    cm = _derive_cn_mc(NS); cm === nothing && return nothing
    CN, MembersT = cm
    _precompile_lifecycle!(CN, NS)
    names = MembersT.parameters[1]
    Mcs   = (MembersT.parameters[2].parameters...,)
    for (nm, Mc) in zip(names, Mcs)
        isconcretetype(Mc) && _anchor_functor_member!(CN, Mc, getfield(s.members, nm))
    end
    return nothing
end

# best-effort (type-only): no handler values, so reaction anchors fall back to `member_schema(base)`,
# GUARDED — an inline `component(…)` member (no such method) is skipped, never a precompile abort.
function precompile_schema(::Type{NS}) where {NS <: NodeSchema}
    _anchor_schema_codecs(NS)
    cm = _derive_cn_mc(NS); cm === nothing && return nothing
    CN, MembersT = cm
    _precompile_lifecycle!(CN, NS)
    for Mc in (MembersT.parameters[2].parameters...,)
        isconcretetype(Mc) || continue
        base = _basetype(Mc)
        hasmethod(member_schema, Tuple{Type{base}}) && _anchor_functor_member!(CN, Mc, member_schema(base))
    end
    return nothing
end

# ── describe_wiring support: members expose ports via their frozen `MemberSchema`. Map each descriptor
#    to the (name, kind, default-wire) row describe_wiring wants.
_desc_kind(::Pub) = :publisher
_desc_kind(::Sub) = :subscription
_desc_kind(::Srv) = :service
_desc_kind(::Act) = :action
_desc_kind(::Tmr) = :timer
_desc_kind(::Use{N, false}) where {N} = :service_client
_desc_kind(::Use{N, true})  where {N} = :action_client
_functor_describe_ports(ms::MemberSchema) =
    Tuple{Symbol, Symbol, String}[(_dname(d), _desc_kind(d), _port_wire(d)) for d in ms.ports]

# ── pretty-printers: a schema VALUE prints its structure + authored wire names. Resolving those wire
#    names to fully-qualified ROS topics needs a node, so that stays `describe_wiring`'s job on a built
#    ComponentNode. ─────────────────────────────────────────────────────────────────────────────────
const _NO_REMAP = Dict{Symbol, String}()

# the "(params P, provides …, requires …)" annotation, omitting whichever parts are empty
function _schema_anno(ms::MemberSchema)
    T = typeof(ms); parts = String[]
    P = paramtype(T); P === EmptyParams || push!(parts, "params $(nameof(P))")
    pv = provides_of(T); isempty(pv) || push!(parts, "provides $(join((nameof(x) for x in pv), ", "))")
    rq = requires_of(T); isempty(rq) || push!(parts, "requires $(join((nameof(x) for x in rq), ", "))")
    isempty(parts) ? "" : " (" * join(parts, ", ") * ")"
end

# one indented "name  kind  wire" line per port (the wire is the post-remap name from `wmap`, else the
# descriptor default); timers carry no wire.
function _port_lines(ms::MemberSchema, indent::AbstractString, wmap)
    rows = _functor_describe_ports(ms)
    isempty(rows) && return [indent * "(no ports)"]
    map(rows) do (pname, pkind, dwire)
        kindstr = _port_kindstr(pkind)
        pkind === :timer ? indent * rpad(string(pname), 14) * kindstr :
                           indent * rpad(string(pname), 14) * rpad(kindstr, 7) * get(wmap, pname, dwire)
    end
end

# a lone descriptor, compact: `pub :telemetry → ~/telemetry`
Base.show(io::IO, d::PortDesc) = print(io, _port_kindstr(_desc_kind(d)), " :", _dname(d),
    _desc_kind(d) === :timer ? "" : " → " * _port_wire(d))

function Base.show(io::IO, ::MIME"text/plain", ms::MemberSchema)
    lines = ["MemberSchema $(nameof(statetype(typeof(ms))))" * _schema_anno(ms)]
    append!(lines, _port_lines(ms, "  ", _NO_REMAP))
    print(io, join(lines, "\n"))
end
Base.show(io::IO, ms::MemberSchema) = print(io, "MemberSchema(", nameof(statetype(typeof(ms))), ")")

function Base.show(io::IO, ::MIME"text/plain", ns::NodeSchema)
    nm = ns.name === nothing ? "(anonymous)" : "\"$(ns.name)\""
    lines = ["NodeSchema $nm — $(length(ns.order)) member(s), DI order: $(join(ns.order, " → "))"]
    for mem in ns.order
        ms = getfield(ns.members, mem)
        push!(lines, "  $mem :: $(nameof(statetype(typeof(ms))))" * _schema_anno(ms))
        append!(lines, _port_lines(ms, "    ", get(ns.wires, mem, _NO_REMAP)))
    end
    print(io, join(lines, "\n"))
end
Base.show(io::IO, ns::NodeSchema) =
    print(io, "NodeSchema(", ns.name === nothing ? "anonymous" : repr(String(ns.name)), ", ", length(ns.order), " members)")
