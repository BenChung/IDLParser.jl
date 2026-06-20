# Node assembly + run/container. `@node` names a
# collection of members; `run(K)` builds a node-core, constructs each member (in DI
# dependency order), and drives the lifecycle; `container` composes nodes into
# one process.
#
# One assembly path serves both flavours:
#   - unmanaged (default): the node-core is a plain `Node`; assembly autostarts
#     (configure → activate) and there is no control surface or gating cost.
#   - managed (`managed=true`): the node-core is a `LifecycleNode` (the five
#     lifecycle_msgs services + ~/transition_event + the managed-node dispatch gate); its
#     transitions fan out to the members' `configure`/`activate`/… hooks. Ports
#     materialise at the configure transition, so an inactive node's entities are
#     gated/silent until `activate`.

export @node, @precompile_nodes, describe_wiring

# ── the node-core wrapper ───────────────────────────────────────────────────────

"""
    ComponentNode

A live component node: the node-core (a `Node`, or the inner `Node` of
a `LifecycleNode` when managed) shared by the node's member mixins, the constructed
member instances by name, and the `LifecycleNode` handle (`lifecycle`) when managed.
Returned by `run` / held by a [`Container`](@ref).
"""
# `Members` is the typed member container — a `NamedTuple{names, Tuple{concrete mixin types}}` for a
# `@node` with a generated `_build_members` (the node-as-built form, grounded), or `Dict{Symbol,Any}`
# for the dynamic fallback (mixin-as-node / un-finalized). `members[nm]` reads work for both
# (`getindex` by Symbol); the typed form makes the dispatch concrete. `::ComponentNode` sites match
# the UnionAll.
# A per-member materialised-ports cell: holds the member's typed ports NamedTuple once its
# configure step materialises it, `nothing` before/after (so `entities` errors clearly pre-config
# and cleanup can drop the handles). One cell per member lets configure/cleanup run independently;
# the `Union{Nothing,P}` narrows to `P` after the `=== nothing` guard, so `entities` stays typed.
mutable struct PortCell{P}
    v::Union{Nothing, P}
    PortCell{P}() where {P} = new{P}(nothing)
end

# The node owns the runtime, fully typed. A member (`Component{Name}`) holds no node reference;
# `entities(node, m)`/`parameters(node, m)` read these carriers at the member named `_path(m)`,
# so the node↔member relationship is acyclic (members never name the node).
#   Members  — `@NamedTuple{a::Counter{:a}, …}` (typed) or `Dict{Symbol,Any}` (dynamic fallback)
#   Ports    — `@NamedTuple{a::PortCell{PortsA}, …}` keyed by member name; cells filled at configure
#   Pservers — `@NamedTuple{a::ParameterServer{Pa}, …}` keyed by member name; built at construct
mutable struct ComponentNode{Members, Ports, Pservers}
    const node::Node
    const members::Members
    const ports::Ports              # per-member PortCell, keyed by member name
    const pservers::Pservers        # per-member ParameterServer, keyed by member name
    const order::Vector{Symbol}     # member names in DI construction (toposort) order — drives
                                    # lifecycle fan-out + reverse-order teardown
    const wires::Dict{Symbol, Dict{Symbol, String}}  # member => (port => resolved wire name), remaps
    lifecycle::Any                  # the LifecycleNode (managed) or nothing
    owned_ctx::Union{Context, Nothing}  # the Context `run` opened for this node, closed by
                                        # `close`; `nothing` when the caller supplied one
    @atomic open::Bool              # single-winner close latch (unmanaged path)
end

# ── accessors (methods of the `entities`/`parameters` generics declared in component.jl) ──
# `_path(m)` is a constant lifted from the mixin's type parameter, so `getfield(carrier, _path(m))`
# folds to the member's field — a typed load, no dispatch, no `Any`.
function entities(node::ComponentNode, m::Component)
    cell = getfield(node.ports, _path(m))
    v = cell.v
    v === nothing && error("entities($(nameof(typeof(m)))): member `$(_path(m))` not materialised yet (configure the node first)")
    return v
end

parameters(node::ComponentNode, m::Component) = current(getfield(node.pservers, _path(m)))

Base.show(io::IO, c::ComponentNode) =
    print(io, "ComponentNode(", c.node.fqn, c.lifecycle === nothing ? "" : ", managed",
          ", members=", c.order, ")")

"""
    parameters(node::ComponentNode) -> NamedTuple

The node-level parameter aggregation: one entry per member, keyed by member name in
declared order, each value the member's per-mixin snapshot (see
[`parameters(m::Component)`](@ref)). So `parameters(node).camera` is member `camera`'s
live snapshot, and the result is heterogeneous — for iterating a whole node, where
`parameters(node, m)` is the type-stable per-mixin view. The flat `<member>.<field>` wire
namespace these views live under is owned by [`CompositeParameterServer`](@ref).
"""
parameters(c::ComponentNode) = (; (nm => parameters(c, c.members[nm]) for nm in c.order)...)

entities(c::ComponentNode) = (; (nm => entities(c, c.members[nm]) for nm in c.order)...)

inner_node(c::ComponentNode) = c.node
"The `LifecycleNode` driving a managed component node, or `nothing` (unmanaged)."
lifecycle(c::ComponentNode) = c.lifecycle

# A port kind's `resolve_name` channel (mirrors `_clobber_channel`'s resolve kind):
# topics for pub/sub, the service channel for service/client/action.
_resolve_kind(kind::Symbol) =
    (kind === :service || kind === :client || kind === :action) ? :service : :topic
_port_kindstr(kind::Symbol) =
    kind === :publisher ? "pub" : kind === :subscription ? "sub" :
    kind === :service   ? "srv" : kind === :client       ? "cli" :
    kind === :action    ? "act" : kind === :timer ? "timer" : String(kind)

"""
    describe_wiring([io], node::ComponentNode)

Print each member port's identifier and the fully-qualified ROS name it resolves
to, so the name expansion is visible at a glance — in particular a private
`~/foo` resolving to `/<node>/foo` versus a relative `foo` resolving to
`/<namespace>/foo`, the usual reason a `@hears` and a publisher that "should"
match land on different topics. The middle column is the authored (post-remap)
wire name; the arrow shows where it resolves. Timers carry no wire and are listed
bare.
"""
describe_wiring(c::ComponentNode) = describe_wiring(stdout, c)

function describe_wiring(io::IO, c::ComponentNode)
    println(io, "wiring of ", c.node.fqn, " — ", length(c.order), " member(s)")
    for mem in c.order
        m = c.members[mem]
        base = _base(typeof(m))
        ports = mixin_spec(base).ports
        println(io, "  ", mem, " :: ", nameof(base))
        isempty(ports) && (println(io, "    (no ports)"); continue)
        wmap = get(c.wires, mem, Dict{Symbol, String}())
        for p in ports
            kindstr = _port_kindstr(p.kind)
            if p.kind === :timer
                println(io, "    ", rpad(string(p.name), 18), kindstr)
                continue
            end
            wire = get(wmap, p.name, _wire(p))
            fqn  = resolve_name(c.node, wire; kind = _resolve_kind(p.kind))
            println(io, "    ", rpad(string(p.name), 18), rpad(kindstr, 6),
                    rpad(wire, 22), " → ", fqn)
        end
    end
    return nothing
end

# Closing a node tears it down: run each member's `cleanup` in reverse DI order, then
# undeclare its entities + node token. The single-winner `@atomicswap` latch makes this
# idempotent — an `unload_node` service task and the Context drain hook race to close, and
# `_member_cleanup!`'s materialised-ports guard is check-then-act, not atomic. A managed
# node routes through `close(LifecycleNode)` → `shutdown!` → its `on_shutdown` hook, which
# fans out the same `_member_cleanup!` behind its own latch. `_member_cleanup!` no-ops on a
# member whose ports aren't materialised, so close is safe on a half-built node too.
function Base.close(c::ComponentNode)
    if c.lifecycle === nothing
        (@atomicswap c.open = false) || return nothing
        foreach(nm -> _member_cleanup!(c, nm), reverse(c.order))
        close(c.node)
    else
        close(c.lifecycle)
    end
    # A `run`-owned Context closes with its node. Skip when a drain is already
    # underway: the drain hook re-enters here, and a nested `close(ctx)` would
    # park on its own drain until `drain_timeout`.
    c.owned_ctx === nothing || is_shutdown(c.owned_ctx) || close(c.owned_ctx)
    return nothing
end

# ── @node ─────────────────────────────────────────────────────────────────────

"""
A member of a `@node`: a name within the node, the mixin type filling it, and its
per-member wire-topic **remaps**. Each remap is `port => target`, where `target`
is a `String` (an explicit wire name, ROS `-r`-style) or a `(member, port)` pair
pointing at another member's resolved wire name. Topics resolve in the node namespace
as authored unless remapped here.
"""
struct NodeMember
    name::Symbol
    mixin::Type
    remaps::Vector{Pair{Symbol, Any}}   # port => String | (member::Symbol, port::Symbol)
end
NodeMember(name::Symbol, mixin::Type) = NodeMember(name, mixin, Pair{Symbol, Any}[])

"""
    NodeKind

A named collection of [`NodeMember`](@ref)s — the result of `@node N =
[…]`. `run(N)` / `add!(c, N)` instantiate it.
"""
# Parametric on the node's name (a `Symbol` type param) so each `@node` is a DISTINCT type
# (`typeof(Rig) === NodeKind{:Rig}`) — the per-node identity the node-as-built machinery
# (`_build_members`/`ComponentNode{…}`) dispatches on. The `name` field mirrors the param so the
# many `k.name` reads are unchanged; every `::NodeKind` / `isa NodeKind` site matches the UnionAll.
struct NodeKind{Name}
    name::Symbol
    members::Vector{NodeMember}
end
NodeKind(name::Symbol, members::Vector{NodeMember}) = NodeKind{name}(name, members)

Base.show(io::IO, k::NodeKind) =
    print(io, "NodeKind(", k.name, ", ", [m.name => nameof(m.mixin) for m in k.members], ")")

"""
    @node N = ["name" => Mixin, …]
    @node N = ["name" => Mixin{port => "wire", port2 => other.port, …}, …]

Assemble a node kind from member mixins. A node is one node in the ROS graph whose
entities are contributed by several mixin members sharing one node-core. Each member
is `"name" => Mixin`, where the string `name` is that member's namespace within the
node: it prefixes the member's parameters in the flat `<member>.<field>` scheme of
[`CompositeParameterServer`](@ref), and keys its slice of the node-level entity view
(`entities(node).name`). The name is always written out, so the namespace is explicit
and two members of the same mixin type stay distinct instances.

Binds `N` to a `NodeKind` and registers it by name in the process-global
node-kind registry (the `rclcpp_components_register_nodes` analog), so a container's
`load_node` / `ros2 component load` can instantiate it from its name. Registration
runs when the defining module's body is evaluated (scripts, the REPL, and `include`d
modules); a precompiled package registers its kinds at load through its
[`ros_init!`](@ref)/`__init__` hook, so `load_node`-by-name works there too. Evaluates
to the `NodeKind`. Bring the node
up with `run(N; …)` or `add!(container, N; …)`.

A member may remap its ports with brace syntax, `Mixin{port => target}` — the ROS
`-r`-style remap. `target` is a wire-name string, or another member's port written
`other_member.port`, which resolves to that port's resolved wire name. Topics resolve
in the node namespace as authored unless remapped. Assembly errors on an unintended
clobber — two members' same-channel output ports (publishers, service servers, action
servers) landing on one resolved name with neither explicitly remapped; fix it by
remapping one onto a distinct name, or express a deliberate share by explicitly
remapping at least one onto the shared name.

Expansion errors (raised when the macro runs) on: a right-hand side that is not a
list; a member without an explicit `"name" => Mixin` form; a member name containing a
dot or duplicated within the node; or a member spelled as a mixin instantiation
(`Mixin{Provider}` with no `=>` pairs) — name the base mixin and let dependency
injection choose the instantiation. A remap of a port the mixin does not declare, a
remap to an unknown member/port, or a cyclic cross-member remap chain errors at
assembly (`run`/`add!`).

```julia
@node Rig = ["front" => Camera{image => "front/image"},
             "rear"  => Camera{image => "rear/image"},
             "fuse"  => Stitcher{left => front.image, right => rear.image}]
run(Rig; name = "rig")
```
"""
macro node(assign)
    (assign isa Expr && assign.head === :(=)) ||
        error("@node expects `N = [\"name\" => Mixin, …]`")
    N, rhs = assign.args[1], assign.args[2]
    members = _parse_members(rhs)
    memexprs = [:( $(NodeMember)($(QuoteNode(nm)), $(esc(mx)),
                                 $(Pair{Symbol, Any})[$(rms...)]) )
                for (nm, mx, rms) in members]
    modu = __module__
    # Register the kind by name (the `rclcpp_components_register_nodes` analog) so a
    # container's `load_node` can instantiate it. Immediate for the REPL/script case;
    # deferred to `ros_init!` for a precompiled package.
    return quote
        const $(esc(N)) = $(NodeKind)($(QuoteNode(N)), $(NodeMember)[$(memexprs...)])
        if ccall(:jl_generating_output, Cint, ()) == 0
            $(register_node_kind!)($(string(N)), $(esc(N)))
        end
        if !isdefined($modu, :__node_kinds__)
            $(esc(:__node_kinds__)) = $(Tuple{String, Any})[]
        end
        # Replace-by-name so a Revise re-eval of this `@node` refreshes the roster entry
        # rather than appending a stale duplicate of the same kind.
        let nk = $(esc(:__node_kinds__)), e = ($(string(N)), $(esc(N)))
            local i = findfirst(x -> x[1] == e[1], nk)
            i === nothing ? push!(nk, e) : (nk[i] = e)
        end
        # Install ROSNode's load hook unless the module brings its own `__init__` (then
        # call `ROSNode.ros_init!(@__MODULE__)` from it).
        if !isdefined($modu, :__init__)
            $(esc(:__init__))() = $(ros_init!)($modu)
        end
        $(esc(N))
    end
end

# ── module-end bake (`@precompile_nodes`) ───────────────────────────────────────
# Bake each declared mixin's typed parameter schema (`__ros_pschema_<M>__`), entities accessor
# (`__ros_entities_<M>__`), and reaction-handler specialisations into the CONSUMING module's
# precompile image, so a runtime `run(MyNode)` skips that first-touch codegen + JIT. The named
# `const`s/methods are `Core.eval`'d into the user module so they ride its pkgimage, and each
# handler's `precompile` runs inside `@precompile_nodes`' `@compile_workload` so the transitive
# frames it pulls in (codecs/dispatch on the user's message types, which ROSNode's own
# type-agnostic workload cannot know) land in the user image too. Guarded per base so one
# malformed mixin can't break the package's precompile. Idempotent: at load, `ros_init!`/the
# materialise path find the markers present and reuse the bakes rather than re-`Core.eval`ing.
function _finalize_module!(mod::Module)
    isdefined(mod, :__mixin_bases__) || return nothing
    # Per BASE: only the node-INDEPENDENT artifacts. The mixin's typed parameter schema, its
    # generated `_build_ports` method, and its action codecs (keyed on the action type, not the
    # node). The reaction handlers + construction/materialise path are node-first (they specialise
    # on the concrete `ComponentNode`), so they bake per-`@node` in `_anchor_node_plan!` — not here.
    for M in getfield(mod, :__mixin_bases__)
        try
            _ensure_schema!(M)            # __ros_pschema_<M>__ + descriptors/pschema
            _anchor_action_codecs!(M)     # goal/result/feedback + wrapper codecs (node-independent)
            _gen_build_ports!(mod, M)     # __ros_build_ports_<M>__ + _build_ports(cnode, m::M, …)
        catch err
            @debug "_finalize_module!: skipped a mixin bake" mod mixin = M exception = err
        end
    end
    # Re-bake the type-agnostic assembly/lifecycle scaffolding (`_assemble`, `_resolve_wires`,
    # `_check_clobbers`, `_resolve_di`, `_member_*`, …) into THIS module's image. ROSNode bakes
    # these once in its own image (`_component_precompile_specs`), but a consumer's `@mixin` defs
    # supersede the generic `mixin_spec(::Type)`/`requires(::Type)`/`provides(::Type)`/`construct`
    # fallbacks those frames specialised against — INVALIDATING ROSNode's copies the moment this
    # module loads. Re-running the specs here (after the module's mixin methods exist) compiles
    # versions whose backedges already include those methods, so they ride this package's image
    # un-invalidated and the first `run` doesn't recompile the whole assembly path.
    try
        for (f, ts) in _component_precompile_specs()
            precompile(f, ts)
        end
    catch err
        @debug "_finalize_module!: skipped scaffolding re-anchor" mod exception = err
    end
    # Per-`@node` static-DI bake: the loop above anchors each mixin BASE, which misses a
    # parametric mixin (`Guard{B}`) — its instantiation is fixed only once a node resolves DI.
    # A declared `@node` is static, so we resolve that here (§ `_anchor_node_plan!`) and anchor
    # the construction + reaction path on each member's exact instantiation. Also bakes the
    # planning (DI/toposort/wire-remap) specialised on this node's member types.
    if isdefined(mod, :__node_kinds__)
        for (_, K) in getfield(mod, :__node_kinds__)
            K isa NodeKind || continue
            # Functional bake FIRST + in its own guard: the node-as-built member constructor
            # `_build_members(::Type{NodeKind{:N}}, node)` drives the typed runtime assembly path,
            # so a throw in the (perf-only) node-plan bake below must never skip it.
            try
                _gen_build_members!(mod, K)
                _gen_build_ports_carrier!(mod, K)
                _gen_build_pservers!(mod, K)
            catch err
                @debug "_finalize_module!: skipped node-carrier gen" mod node = K exception = err
            end
            try
                # `invokelatest`: `_anchor_node_plan!` reads `pschema`/`mixin_spec`/`_build_members`
                # methods that `_ensure_schema!`/`_gen_build_members!` `Core.eval`'d earlier in THIS
                # `_finalize_module!` frame — only visible in the latest world.
                Base.invokelatest(_anchor_node_plan!, K)   # perf: precompile the assembly/fan-out/reaction path
            catch err
                @debug "_finalize_module!: skipped a node-plan bake" mod node = K exception = err
            end
        end
    end
    return nothing
end

# The node-as-built member constructor: `_build_members(::Type{NodeKind{:N}}, node)` returns the
# member NamedTuple, generated once per `@node` as the toposort'd straight-line `construct` sequence
# (deps threaded positionally from prior members). Its return type IS the node's typed `Members`.
# Generic here; per-node methods are `Core.eval`'d at finalize (marker-cached, Revise-safe, like the
# entities accessor). `__rt__`/pservers are NOT set here — the assembly does that after building the
# node (the back-ref needs the `ComponentNode`); deps are injected as constructed objects only.
function _build_members end

function _gen_build_members!(mod::Module, K::NodeKind)
    KT = typeof(K)                                # NodeKind{:N}
    nm_sym = KT.parameters[1]
    marker = Symbol("__ros_build_members_", nm_sym)
    isdefined(mod, marker) && return nothing
    order, bytype, edges = _members_plan(K)
    loc = Dict(nm => Symbol("_m_", nm) for nm in order)
    body = Expr(:block)
    for nm in order
        M = bytype[nm]
        deps = Any[loc[d] for d in edges[nm]]
        push!(body.args, :($(loc[nm]) = $(GlobalRef(@__MODULE__, :construct))(
            $M, node, $(Val(nm)), $(deps...))))     # name threaded → `construct` builds `M{nm}`
    end
    push!(body.args, :(return $(Expr(:tuple, (Expr(:(=), nm, loc[nm]) for nm in order)...))))
    Core.eval(mod, quote
        const $marker = true
        $(GlobalRef(@__MODULE__, :_build_members))(::$(Type){$KT}, node::$(GlobalRef(@__MODULE__, :Node))) = $body
    end)
    return nothing
end

# The node's typed runtime carriers, generated per `@node` like `_build_members`: the empty
# port cells (filled at configure) and the per-member parameter servers. The straight-line,
# member-unrolled form makes each field's type concrete, so the returned NamedTuples — and thus
# the `ComponentNode{Members, Ports, Pservers}` built from them — are fully typed and bake into
# the consumer image. `_assemble` falls back to the equivalent dynamic generators (below) when a
# node has no generated method (mixin-as-node, or not finalised). The per-member type expressions
# (`_ports_nt_type`/`pschema`) are constant-folded at the spliced mixin type, exactly as the
# dynamic forms compute them — so behaviour is identical, only the iteration is unrolled.
function _build_ports_carrier end
function _build_pservers end

function _gen_build_ports_carrier!(mod::Module, K::NodeKind)
    KT = typeof(K)
    nm_sym = KT.parameters[1]
    marker = Symbol("__ros_build_ports_carrier_", nm_sym)
    isdefined(mod, marker) && return nothing
    order, bytype, _ = _members_plan(K)
    # Splice the cell's CONCRETE port-NamedTuple type as a literal (resolved now from `mixin_spec`,
    # which `@mixin` defined before finalize): `PortCell{<concrete>}()` is then a static call the
    # `_build_ports_carrier` anchor bakes — a runtime `_ports_nt_type(…)` wouldn't const-fold (the
    # spec is a non-bits struct), leaving the constructor dynamic and unbaked.
    cell(M) = :($(GlobalRef(@__MODULE__, :PortCell)){$(something(_ports_nt_type(mixin_spec(M).ports), Any))}())
    fields = Expr(:tuple, (Expr(:(=), nm, cell(bytype[nm])) for nm in order)...)
    Core.eval(mod, quote
        const $marker = true
        $(GlobalRef(@__MODULE__, :_build_ports_carrier))(::$(Type){$KT}, node::$(GlobalRef(@__MODULE__, :Node))) = $fields
    end)
    return nothing
end

function _gen_build_pservers!(mod::Module, K::NodeKind)
    KT = typeof(K)
    nm_sym = KT.parameters[1]
    marker = Symbol("__ros_build_pservers_", nm_sym)
    isdefined(mod, marker) && return nothing
    order, bytype, _ = _members_plan(K)
    srv(M, nm) = :($(GlobalRef(@__MODULE__, :_build_pserver))(node,
        $(GlobalRef(@__MODULE__, :pschema))($M),
        $(GlobalRef(@__MODULE__, :_member_overrides))($M, $(QuoteNode(nm)), overrides)))
    fields = Expr(:tuple, (Expr(:(=), nm, srv(bytype[nm], nm)) for nm in order)...)
    Core.eval(mod, quote
        const $marker = true
        # `overrides` is `@nospecialize`d (it is forwarded to the already-`@nospecialize`d
        # `_member_overrides`), so a node bakes one carrier MI regardless of the caller's override set.
        $(GlobalRef(@__MODULE__, :_build_pservers))(::$(Type){$KT}, node::$(GlobalRef(@__MODULE__, :Node)),
            $(Expr(:macrocall, GlobalRef(Base, Symbol("@nospecialize")), LineNumberNode(@__LINE__, @__FILE__), :overrides))) = $fields
    end)
    return nothing
end

# Bake the static assembly of a declared `@node`. The plan — DI resolution, toposort, and wire
# remaps — is a pure function of the `NodeKind` (its members' `requires`/`provides`/`construct`
# methods, all defined by the time `@precompile_nodes` runs), so we compute it here and, walking
# the resolved order, derive each member's CONCRETE instantiation from `construct`'s return type
# given its already-resolved deps. That recovers the exact shape — `Guard{Sensor}`, not the base
# `Guard` — letting the per-member anchors cover a parametric mixin the base-mixin bake skips
# (`_anchor_construction!(Guard)` is empty for the non-concrete base, so its construct/materialise/
# reaction path otherwise JITs at first `run`). Running `_members_plan`/`_resolve_wires` also bakes
# the planning machinery specialised on this node's member types. Inference/precompile only — no
# `construct`/`configure` is executed (it may touch user state), only its return type is queried.
function _anchor_node_plan!(K::NodeKind)
    order, bytype, edges = _members_plan(K)   # bakes _resolve_di/_toposort for these member types
    _resolve_wires(K)                          # bakes wire-remap resolution for these ports
    # Resolve each member's CONCRETE instantiation `Mc = M{name}` from `construct`'s return type,
    # given its already-resolved deps (the name threaded as `Val{name}`).
    concrete = Dict{Symbol, Any}()
    for nm in order
        M = bytype[nm]
        deptypes = Any[concrete[d] for d in edges[nm]]
        rts = Base.return_types(construct, (Type{M}, Node, Val{nm}, deptypes...))
        Tc = (length(rts) == 1 && isconcretetype(only(rts))) ? only(rts) : M
        concrete[nm] = Tc
    end
    # The generated member + carrier constructors (eval'd into this module → latest world), keyed on
    # `NodeKind{:N}` whose method this module owns → caches in the consumer image. `_build_pservers`
    # is `@nospecialize`d on overrides; the bake covers the no-override (empty NamedTuple) run.
    Base.invokelatest(precompile, _build_members,       (Type{typeof(K)}, Node))
    Base.invokelatest(precompile, _build_ports_carrier, (Type{typeof(K)}, Node))
    Base.invokelatest(precompile, _build_pservers,      (Type{typeof(K)}, Node, NamedTuple{(), Tuple{}}))

    # Derive the concrete node type `ComponentNode{Members, Ports, Pservers}` from the resolved
    # members. Members/Ports/Pservers all carry the consumer's mixin + message types, so the
    # node-level fan-out + per-member materialise/reaction frames keyed on it are CONSUMER-cacheable
    # (unlike the `NodeKind{:N}`-keyed planning, which is external and re-infers regardless).
    all(nm -> isconcretetype(concrete[nm]), order) || return nothing
    Members  = NamedTuple{(order...,), Tuple{(concrete[nm] for nm in order)...}}
    Ports    = NamedTuple{(order...,),
                          Tuple{(PortCell{something(_ports_nt_type(mixin_spec(_base(concrete[nm])).ports), Any)}
                                 for nm in order)...}}
    Pservers = NamedTuple{(order...,),
                          Tuple{(ParameterServer{pschema(_base(concrete[nm])) } for nm in order)...}}
    CN = ComponentNode{Members, Ports, Pservers}

    # The node constructor itself: `_assemble` is `@nospecialize(K)`, so it builds `CN` dynamically
    # from the runtime carrier types — anchor the concrete constructor (unmanaged run: `lifecycle` and
    # `owned_ctx` are `Nothing`) so that dynamic construction reuses cached code instead of re-inferring.
    Base.invokelatest(precompile, CN,
        (Node, Members, Ports, Pservers, Vector{Symbol}, Dict{Symbol, Dict{Symbol, String}}, Nothing, Nothing, Bool))
    # The `CompositeParameterServer` member list `Pair{Symbol,ParameterServer}[nm => server …]` widens
    # each concrete server to the abstract element type (a `convert`), built dynamically in `_assemble`
    # over the runtime member names — anchor the per-schema `Pair` + `convert` so it reuses cached code.
    for nm in order
        P = pschema(_base(concrete[nm]))
        Base.invokelatest(precompile, Pair, (Symbol, ParameterServer{P}))
        Base.invokelatest(precompile, convert,
            (Type{Pair{Symbol, ParameterServer}}, Pair{Symbol, ParameterServer{P}}))
    end
    # Field writes the assembly/materialise performs on the concrete node + cells: the owned Context
    # onto the node (`cnode.owned_ctx = ctx`) and each member's built ports into its cell at configure
    # (`cell.v = ports`). Both run in the `@nospecialize`d `_assemble`/materialise, so anchor them here.
    Base.invokelatest(precompile, setproperty!, (CN, Symbol, Context))
    for nm in order
        PC = fieldtype(Ports, nm)                       # PortCell{<ports NT>}
        Base.invokelatest(precompile, setproperty!, (PC, Symbol, PC.parameters[1]))
    end

    # Node-level fan-out + local-graph priming, keyed on the concrete node.
    Base.invokelatest(precompile, _prime_local_graph!,   (CN, Vector{Symbol}))
    Base.invokelatest(precompile, _ordered_member_descs, (CN, Vector{Symbol}))
    prt = Base.invokelatest(Base.return_types, _prime_local_graph!, (CN, Vector{Symbol}))
    length(prt) == 1 && Base.invokelatest(precompile, _reconcile_local_graph!, (Node, only(prt)))
    for fn in (_member_materialize!, _member_configure!, _member_activate!,
               _member_deactivate!, _member_cleanup!, _member_cancel_goals!)
        Base.invokelatest(precompile, fn, (CN, Symbol))
    end
    Base.invokelatest(precompile, _members_on_error!, (CN, Vector{Symbol}))

    # Per member, on the concrete node + instantiation: reaction handlers + decode/dispatch + the
    # construction/materialise path (incl. the generated typed `_build_ports`). Action codecs are
    # node-independent (keyed on the action type) and already baked per-base.
    for nm in order
        Mc = concrete[nm]
        _ensure_schema!(_base(Mc))
        Base.invokelatest(_anchor_reactions!,    CN, Mc)
        Base.invokelatest(_anchor_construction!, CN, Mc)
    end
    return nothing
end

"""
    @precompile_nodes

Bake this module's `@mixin`/`@node` declarations into its precompile image for a faster
first `run`. Place it as the LAST top-level statement in a module that declares mixins/nodes
(after every `@mixin`/`@param`/`@publishes`/… and reaction handler). It generates each
mixin's typed `parameters(node, m)`/`entities(node, m)` accessors and precompiles its reaction handlers
— including the transitive codec/dispatch frames they specialise — into THIS package's image.

Expands to a `PrecompileTools.@compile_workload` (referenced through ROSNode, so the
consuming package needs no `using PrecompileTools` of its own). Opt-in and side-effect-free:
it runs only `precompile`/codegen, never user reactions, and only at precompile
(`jl_generating_output`) — at plain `include`/REPL it's a no-op and the same accessors
generate lazily at the first `run` (capturing the authoritative materialised type).
"""
macro precompile_nodes()
    # Build the `@compile_workload` macrocall via GlobalRef so it resolves to ROSNode's
    # PrecompileTools regardless of the consuming module's imports; the workload runs
    # `_finalize_module!` at the user package's precompile, capturing its transitive frames.
    return Expr(:macrocall,
                GlobalRef(PrecompileTools, Symbol("@compile_workload")),
                __source__,
                Expr(:call, _finalize_module!, __module__))
end

# Each member is `"name" => Mixin` (string name — the namespace), optionally with
# wire-topic remaps `"name" => Mixin{port => "wire", port2 => other.port, …}`. A
# bare identifier name (`name => Mixin`) is accepted too, but the type is never
# auto-named: an explicit name is required, so a member's namespace is always written.
# Returns `(name, mixin_expr, remap_exprs)` per member, where each remap_expr is a
# `port => target` Pair-expr (target a String literal, or a `(member, port)` tuple-expr).
function _parse_members(rhs)
    (rhs isa Expr && rhs.head in (:vect, :vcat)) ||
        error("@node: the right-hand side must be a list, `[\"name\" => Mixin, …]`")
    out = Tuple{Symbol, Any, Vector{Any}}[]
    seen = Set{Symbol}()
    for el in rhs.args
        (el isa Expr && el.head === :call && el.args[1] === :(=>)) ||
            error("@node: each member needs an explicit name — `\"name\" => Mixin` (got `$(el)`)")
        nm = el.args[2]
        name = nm isa AbstractString ? Symbol(nm) :
               nm isa Symbol ? nm :
               error("@node: a member name must be a string, e.g. `\"sensor\" => Sensor` (got `$(nm)`)")
        # The member name is the namespace prefix (`<member>.<field>`), so it must be
        # a single dotless segment — a dot would mis-split the prefixed parameter name —
        # and unique, or two members would share one prefix / clobber in the member Dict.
        occursin('.', String(name)) &&
            error("@node: member name `$(name)` must not contain a dot — it is the `<member>.<field>` parameter prefix")
        name in seen &&
            error("@node: duplicate member name `$(name)` — member names must be unique within a node")
        push!(seen, name)
        mixin, remaps = _parse_member_rhs(name, el.args[3])
        push!(out, (name, mixin, remaps))
    end
    return out
end

# A member's right-hand side: a bare mixin `K`, or `K{port => target, …}` (the
# remap form — Julia parses the braces as a `:curly`). Returns `(mixin_expr, remap_exprs)`.
function _parse_member_rhs(name::Symbol, rhs)
    (rhs isa Expr && rhs.head === :curly) || return (rhs, Any[])
    mixin = rhs.args[1]
    # `K{Sensor}` (no `=>` pairs) is a mixin instantiation, not the remap form — members
    # always name the base; DI picks the provider, and hence the instantiation.
    any(s -> s isa Expr && s.head === :call && s.args[1] === :(=>), rhs.args[2:end]) ||
        error("@node: member `$(name)` names a mixin instantiation `$(rhs)` — name the base mixin `$(mixin)`; the provider is chosen by DI")
    remaps = Any[]
    for spec in rhs.args[2:end]
        (spec isa Expr && spec.head === :call && spec.args[1] === :(=>)) ||
            error("@node: member `$(name)` remap must be `port => \"wire\"` or `port => other.port` (got `$(spec)`)")
        port = spec.args[2]
        port isa Symbol ||
            error("@node: member `$(name)` remap key must be a port name (got `$(port)`)")
        target = spec.args[3]
        tgt = if target isa AbstractString
            String(target)
        elseif target isa Expr && target.head === :. && target.args[1] isa Symbol &&
               target.args[2] isa QuoteNode
            :( ($(QuoteNode(target.args[1])), $(QuoteNode(target.args[2].value))) )  # (member, port)
        else
            error("@node: member `$(name)` remap target must be a \"wire\" string or `member.port` (got `$(target)`)")
        end
        push!(remaps, :( $(QuoteNode(port)) => $tgt ))
    end
    return (mixin, remaps)
end

function _snake(s::AbstractString)
    io = IOBuffer()
    for (i, c) in enumerate(s)
        (isuppercase(c) && i > 1) && print(io, '_')
        print(io, lowercase(c))
    end
    String(take!(io))
end
_default_name(::Type{M}) where {M} = _snake(String(nameof(M)))

# ── port materialisation ────────────────────────────────────────────────────────

# Function barrier for component reaction callbacks. `p.reaction` is stored `Any`, so a
# closure capturing it directly (`msg -> react(m, msg)`) types the captured `react` as
# `Any` and dynamically dispatches the handler on every message/tick — and inference dies
# through it, so the warm never reaches it. Routing through these `where {F}` helpers
# specialises on the concrete reaction type, so the materialised callback is monomorphic and
# precompilable, with the one dispatch paid once here at configure.
# Reaction-callback function barriers. `p.reaction` is stored `Any`, so a closure capturing it
# directly would type it `Any` and dynamically dispatch every message/tick. Routing through these
# `where {F}` helpers specialises on the concrete reaction type, so the materialised callback is
# monomorphic. Node-first: the closure captures the `ComponentNode` and the member, calling the
# handler `reaction(node, m, …)`.
_sub_cb(reaction::F, node, m) where {F} = msg -> reaction(node, m, msg)   # subscription + service
_timer_cb(reaction::F, node, m) where {F} = () -> reaction(node, m)       # @every timer

# Create each declared port's runtime handle against the node-core. Returns the handle
# NamedTuple (the member's `entities`). Endpoints build against the core node (`cnode.node`);
# reaction closures capture the `cnode` so the handler receives it as `node`. `wiremap` is the
# member's resolved remap (port => wire name); a port not in it uses its authored `_wire(p)`.
function _materialize_ports!(cnode::ComponentNode, m, specs::Vector{PortSpec}, pvalue,
                             wiremap::Dict{Symbol, String} = Dict{Symbol, String}())
    node = cnode.node
    handles = Pair{Symbol, Any}[]
    for p in specs
        if p.kind === :publisher
            push!(handles, p.name => Publisher(node, _wire(p, wiremap), p.msgtype))
        elseif p.kind === :subscription
            # `warmup = :off`: the endpoint's own warm is redundant — the member warms all its
            # reactions once after materialise, in `_warm_member_reactions!`.
            sub = Subscription(_sub_cb(p.reaction, cnode, m), node, _wire(p, wiremap), p.msgtype; warmup = :off)
            push!(handles, p.name => sub)
        elseif p.kind === :service
            srv = Service(_sub_cb(p.reaction, cnode, m), node, _wire(p, wiremap), p.msgtype; warmup = :off)
            push!(handles, p.name => srv)
        elseif p.kind === :action
            f = p.reaction
            support = ActionTypeSupport(typeof(f))
            body = _component_action_adapter(f, support, p.extra.fb_pos, cnode, m)
            push!(handles, p.name => _make_action_server(node, _wire(p, wiremap), typeof(f); body = body))
        elseif p.kind === :timer
            hz = p.extra.rate
            if hz isa Symbol
                hz in fieldnames(typeof(pvalue)) || error(
                    "@every: member `$(_path(m))` timer `$(p.name)` rate `:$(hz)` does not name a declared parameter of $(_base(typeof(m)))")
                hz = getproperty(pvalue, hz)
                hz isa Real || error(
                    "@every: member `$(_path(m))` timer `$(p.name)` rate parameter `:$(p.extra.rate)` must be numeric (got `$(typeof(hz))`)")
            end
            t = _paused_timer(node, Duration(round(Int64, 1.0e9 / hz)), _timer_cb(p.reaction, cnode, m))
            push!(handles, p.name => t)
        else
            @warn "component: port kind :$(p.kind) not yet materialised" port = p.name type = typeof(m)
        end
    end
    return NamedTuple(handles)
end

# A port's wire name: its explicit `on \"…\"` override, else the identifier.
_wire(p::PortSpec) = p.wire === nothing ? String(p.name) : p.wire

# A port's wire name under a member's resolved remap map: the remapped name if
# present, else the authored `_wire(p)`.
_wire(p::PortSpec, wiremap::Dict{Symbol, String}) = get(wiremap, p.name, _wire(p))

# ── generated typed materialise (the node-as-built materialise body) ──────────
# `_build_ports(cnode, m::Base, wiremap, pvalue)` is the typed, straight-line counterpart of
# `_materialize_ports!`: generated once per port-bearing mixin BASE (marker-cached, Revise-safe),
# with each port's kind/msgtype/reaction/`extra` BAKED at generation time — so it has no
# `Vector{PortSpec}` walk, no Symbol-kind branch, no `Any`-field reads, and returns a CONCRETE
# ports NamedTuple. The wire name (runtime remap) and a param-driven timer rate stay runtime.
# Dispatch is on the member instance `m` (covering a parametric mixin's instantiations); endpoints
# build against the core node (`cnode.node`) and reaction closures capture `cnode`. The dynamic
# `_materialize_ports!` remains the fallback for an un-finalized mixin.
function _build_ports end

# Fallback for an un-finalized mixin (no generated typed method, e.g. a REPL mixin without
# `@precompile_nodes`/`ros_init!`): the dynamic walk. Dispatching `_member_materialize!` on this
# (vs a runtime `hasmethod` branch) keeps the compiler from inferring the dynamic path as the
# untaken branch when a typed `_build_ports` exists.
_build_ports(cnode::ComponentNode, m, wiremap, pvalue) =
    _materialize_ports!(cnode, m, mixin_spec(_base(typeof(m))).ports, pvalue, wiremap)

# Runtime resolution of a `@every :param`-style timer rate from the member's parameter snapshot.
function _timer_rate(pvalue, sym::Symbol)
    sym in propertynames(pvalue) ||
        error("@every: timer rate `:$(sym)` does not name a declared parameter")
    hz = getproperty(pvalue, sym)
    hz isa Real || error("@every: timer rate `:$(sym)` must be numeric (got `$(typeof(hz))`)")
    return hz
end

function _gen_build_ports!(mod::Module, M::Type)
    ports = mixin_spec(M).ports     # may be empty — a port-less mixin gets an empty typed materialise
    marker = Symbol("__ros_build_ports_", nameof(M))
    isdefined(mod, marker) && return nothing
    R = @__MODULE__
    body = Expr(:block)
    push!(body.args, :(node = getfield(cnode, :node)))   # the core node, for endpoint construction
    pnames = Symbol[]; hlocs = Symbol[]
    for p in ports
        loc  = Symbol("_h_", p.name)
        wire = :(get(wiremap, $(QuoteNode(p.name)), $(_wire(p))))     # runtime remap, authored default baked
        if p.kind === :publisher
            push!(body.args, :($loc = $(GlobalRef(R, :Publisher))(node, $wire, $(p.msgtype))))
        elseif p.kind === :subscription
            push!(body.args, :($loc = $(GlobalRef(R, :Subscription))(
                $(GlobalRef(R, :_sub_cb))($(p.reaction), cnode, m), node, $wire, $(p.msgtype); warmup = :off)))
        elseif p.kind === :service
            push!(body.args, :($loc = $(GlobalRef(R, :Service))(
                $(GlobalRef(R, :_sub_cb))($(p.reaction), cnode, m), node, $wire, $(p.msgtype); warmup = :off)))
        elseif p.kind === :action
            push!(body.args, :($loc = $(GlobalRef(R, :_make_action_server))(
                node, $wire, $(typeof(p.reaction));
                body = $(GlobalRef(R, :_component_action_adapter))(
                    $(p.reaction), $(GlobalRef(R, :ActionTypeSupport))($(typeof(p.reaction))),
                    $(p.extra.fb_pos), cnode, m))))
        elseif p.kind === :timer
            rate = p.extra.rate
            hz = rate isa Symbol ? :($(GlobalRef(R, :_timer_rate))(pvalue, $(QuoteNode(rate)))) : rate
            push!(body.args, :($loc = $(GlobalRef(R, :_paused_timer))(node,
                $(GlobalRef(R, :Duration))(round(Int64, 1.0e9 / $hz)),
                $(GlobalRef(R, :_timer_cb))($(p.reaction), cnode, m))))
        else
            continue
        end
        push!(pnames, p.name); push!(hlocs, loc)
    end
    nt = Expr(:tuple, Expr(:parameters, (Expr(:kw, pn, hl) for (pn, hl) in zip(pnames, hlocs))...))
    push!(body.args, :(return $nt))
    Core.eval(mod, quote
        const $marker = true
        function $(GlobalRef(R, :_build_ports))(cnode::$(GlobalRef(R, :ComponentNode)), m::$M, wiremap, pvalue)
            $body
        end
    end)
    return nothing
end

# ── wire-topic remap resolution + clobber detection ──────────────────────────────

# Resolve every member port's wire name: the remap target if any, else the authored
# `_wire(p)`. A cross-member remap (`port => (member, port)`) resolves to the referenced
# port's wire, by fixpoint (so a ref to a remapped port follows it). Returns
# `member => (port => wire-string)` for ALL ports, plus the set of explicitly-remapped
# `(member, port)`. Errors on a remap of an unknown port, a ref to an unknown
# member/port, or an unresolvable/cyclic ref chain.
# `@nospecialize(k)`: the wire plan is a pure function of `k.members` (data — `Vector{Symbol}`/
# `Dict{Symbol,…}`, never node-kind-dependent types), so specialising per `NodeKind{:N}` only
# spawns an external MI the consumer can't cache. Nospecialised, this is ONE MI baked in ROSNode's
# own image (precompile.jl anchors it on the abstract `NodeKind`); every `@node` reuses it.
function _resolve_wires(@nospecialize(k::NodeKind))
    wires    = Dict{Symbol, Dict{Symbol, String}}()
    portset  = Dict{Symbol, Set{Symbol}}()
    refs     = Dict{Tuple{Symbol, Symbol}, Tuple{Symbol, Symbol}}()  # (m,port) => (refm, refport)
    remapped = Set{Tuple{Symbol, Symbol}}()
    for mem in k.members
        ports = mixin_spec(mem.mixin).ports
        portset[mem.name] = Set(p.name for p in ports)
        rmap = Dict{Symbol, Any}(mem.remaps)
        for (port, _) in rmap
            port in portset[mem.name] ||
                error("@node `$(k.name)`: member `$(mem.name)` remaps unknown port `$(port)`")
        end
        w = Dict{Symbol, String}()
        for p in ports
            t = get(rmap, p.name, nothing)
            if t === nothing
                w[p.name] = _wire(p)
            elseif t isa AbstractString
                w[p.name] = String(t); push!(remapped, (mem.name, p.name))
            else
                refs[(mem.name, p.name)] = t; push!(remapped, (mem.name, p.name))
            end
        end
        wires[mem.name] = w
    end
    remaining = Set(keys(refs))
    while !isempty(remaining)
        progressed = false
        for mp in collect(remaining)
            refm, refport = refs[mp]
            haskey(portset, refm) ||
                error("@node `$(k.name)`: member `$(mp[1])` remaps `$(mp[2])` to unknown member `$(refm)`")
            refport in portset[refm] ||
                error("@node `$(k.name)`: member `$(mp[1])` remaps `$(mp[2])` to `$(refm).$(refport)`, not a port of `$(refm)`")
            if haskey(wires[refm], refport)
                wires[mp[1]][mp[2]] = wires[refm][refport]
                delete!(remaining, mp); progressed = true
            end
        end
        progressed ||
            error("@node `$(k.name)`: unresolvable or cyclic wire remap among $(collect(remaining))")
    end
    return wires, remapped
end

# A port kind's clobber **channel**: a `(bucket, resolve_name-kind)` for an output/server
# port, or `nothing` for kinds that may share a name (subscriptions/clients) or have no
# wire name (timers). Pub↔sub sharing is intra-node wiring, so only same-bucket outputs
# collide. Services and actions bucket apart (an action's name nests its own services).
_clobber_channel(kind::Symbol) =
    kind === :publisher ? (:topic,   :topic)   :
    kind === :service   ? (:service, :service) :
    kind === :action    ? (:action,  :service) : nothing

# Error on an unintended clobber: two members' same-channel output ports
# resolving to one wire name, unless at least one was explicitly remapped (a deliberate
# share). Resolves against the node namespace (so `~/x` vs `/ns/node/x` compare equal).
function _check_clobbers(@nospecialize(k::NodeKind), node::Node, wires, remapped)   # see `_resolve_wires` re: @nospecialize
    seen = Dict{Tuple{Symbol, String}, Tuple{Symbol, Symbol, Bool}}()
    for mem in k.members
        for p in mixin_spec(mem.mixin).ports
            ch = _clobber_channel(p.kind)
            ch === nothing && continue
            full = resolve_name(node, wires[mem.name][p.name]; kind = ch[2])
            key  = (ch[1], full)
            here = (mem.name, p.name, (mem.name, p.name) in remapped)
            if haskey(seen, key)
                prev = seen[key]
                (prev[3] || here[3]) ||
                    error("@node `$(k.name)`: members `$(prev[1]).$(prev[2])` and " *
                          "`$(here[1]).$(here[2])` both resolve to $(p.kind) name `$(full)` — an " *
                          "unintended clobber. Remap one (e.g. `$(here[1]) => $(nameof(mem.mixin))" *
                          "{$(here[2]) => \"…\"}`) or rename.")
            else
                seen[key] = here
            end
        end
    end
    return nothing
end

# A ROS-clock timer built but NOT started (its `Base.Timer` is created at activate, so
# a tick can't fire before configure / while a managed node is gated).
function _paused_timer(node::Node, period::Duration, f)
    c = clock(node, ROS())
    Timer{ROS}(c, period, f, true, nothing)
end

# ── DI resolution + toposort ──────────────────────────────────────────────────

# Resolve each member's `requires` to a single providing sibling (excluding self),
# returning `member-name => [provider names]` in `requires` order. A requirement is
# either an `@interface` marker — matched against members' `@provides` evidence — or a
# concrete `@mixin` type, matched against the sibling that IS that mixin (on its base,
# so a parametric mixin is named by its base too). Errors on an unsatisfied or
# ambiguous requirement.
function _resolve_di(members::Vector{NodeMember})
    providers = Dict{Type, Vector{Symbol}}()           # interface  => members that provide it
    for mem in members, I in provides(mem.mixin)
        push!(get!(Vector{Symbol}, providers, I), mem.name)
    end
    by_mixin = Dict{Type, Vector{Symbol}}()            # mixin base => members of that mixin
    for mem in members
        push!(get!(Vector{Symbol}, by_mixin, _base(mem.mixin)), mem.name)
    end
    edges = Dict{Symbol, Vector{Symbol}}()
    for mem in members
        deps = Symbol[]
        for I in requires(mem.mixin)
            cands = if I isa Type && I <: ComponentInterface
                filter(!=(mem.name), get(providers, I, Symbol[]))
            elseif I isa Type && ismixin(_base(I))
                filter(!=(mem.name), get(by_mixin, _base(I), Symbol[]))
            elseif I isa Pair
                error("@node: member `$(mem.name)` requires $(I): pin pairs " *
                      "(`I => :member`) are not yet supported; restructure so a single " *
                      "member provides the interface")
            else
                error("@node: member `$(mem.name)` requires `$(I)`, which is neither an " *
                      "`@interface` nor a `@mixin` type")
            end
            isempty(cands) &&
                error("@node: member `$(mem.name)` ($(mem.mixin)) requires $(I), but no " *
                      "other member provides or is it")
            length(cands) == 1 ||
                error("@node: member `$(mem.name)` requires $(I), matched by multiple " *
                      "members $(cands) — restructure so a single member satisfies it")
            push!(deps, cands[1])
        end
        edges[mem.name] = deps
    end
    return edges
end

# DFS toposort: dependencies first; a back-edge is a dependency cycle.
function _toposort(members::Vector{NodeMember}, edges::Dict{Symbol, Vector{Symbol}})
    color = Dict(m.name => :white for m in members)
    order = Symbol[]
    function visit(n, stack)
        color[n] === :black && return
        color[n] === :gray &&
            error("@node: dependency cycle: $(join(vcat(stack, n), " ▸ "))")
        color[n] = :gray
        for d in edges[n]
            visit(d, vcat(stack, n))
        end
        color[n] = :black
        push!(order, n)
    end
    for m in members
        visit(m.name, Symbol[])
    end
    return order
end

# The construction plan for a node kind: (member order, name→type, name→dep-names).
_members_plan(::Type{M}) where {M <: Component} =
    (Symbol[Symbol(_default_name(M))],
     Dict{Symbol, Type}(Symbol(_default_name(M)) => M),
     Dict{Symbol, Vector{Symbol}}(Symbol(_default_name(M)) => Symbol[]))
function _members_plan(@nospecialize(k::NodeKind))   # see `_resolve_wires` re: @nospecialize
    edges = _resolve_di(k.members)
    order = _toposort(k.members, edges)
    return (order, Dict{Symbol, Type}(m.name => m.mixin for m in k.members), edges)
end

# Filter run/add! overrides to this member: a mixin-local key (`fps`) applies
# to every member that declares it; a prefixed key (`var"camera.fps"` — the form a
# composed node's wire `Parameter.name` carries, and the disambiguator when two
# members share a field name) targets one member and wins over the mixin-local form.
function _member_overrides(@nospecialize(M::Type), member::Symbol, @nospecialize(overrides::NamedTuple))
    fns = fieldnames(pschema(M))
    out = Dict{Symbol, Any}()
    for (k, v) in pairs(overrides)
        k in fns && (out[k] = v)                          # mixin-local
    end
    prefix = string(member, ".")
    for (k, v) in pairs(overrides)
        ks = String(k)
        if startswith(ks, prefix)                         # prefixed `member.field` wins
            f = Symbol(SubString(ks, ncodeunits(prefix) + 1))
            f in fns && (out[f] = v)
        end
    end
    return (; out...)
end

_build_pserver(node::Node, ::Type{P}, overrides::NamedTuple) where {P} =
    ParameterServer{P}(node; overrides = overrides)

# ── per-member lifecycle steps (run at the matching transition) ──────────────────

# The base for instance-keyed lookups: `Guard{Sensor} → Guard`, identity for plain
# types. Registries and generated accessors are keyed on the registered base, so
# `typeof(m)` (a concrete instantiation for a parametric mixin) must normalize first.
_base(::Type{T}) where {T} = Base.typename(T).wrapper

# Friendly run-entry gate: a concrete instantiation spelled at the value level (type
# alias, `typeof`) bypasses the macro guards — point at the base instead of the bare
# "not a @mixin" / downstream KeyError.
function _check_runnable(M, what::AbstractString)
    ismixin(M) && return nothing
    M isa DataType && ismixin(_base(M)) && error(
        "$(what): name the base mixin `$(_base(M))` — the instantiation is chosen by `construct`/DI (got `$(M)`)")
    error("$(what): $(M) is not a @mixin")
end

# Anchor a node member's reaction handlers, `precompile`-only and side-effect-free. Keyed on the
# CONCRETE node type `CN` (the reactions are node-first — `reaction(node, m, …)` — and the node is
# the concrete `ComponentNode{Members,Ports,Pservers}`, so the handler + its materialised callback
# specialise on it) and the member's concrete instantiation `Mc` (= `M{name}`). Specs come from
# `mixin_spec` keyed at `Mc`'s base. Serves both the runtime warm (`_warm_member_reactions!`, the
# live `cnode`/member types) and the per-`@node` bake (`_anchor_node_plan!`).
function _anchor_reactions!(::Type{CN}, ::Type{Mc}) where {CN, Mc}
    for p in mixin_spec(_base(Mc)).ports
        p.reaction === nothing && continue
        if p.kind === :subscription
            precompile(p.reaction, (CN, Mc, p.msgtype))
            # The consumer's decode→dispatch chain. A component subscription builds with
            # `warmup = :off`, so anchor that chain here through the materialised callback type —
            # the `_sub_cb` closure the consumer actually invokes — in the default `Owned` view.
            rts = Base.return_types(_sub_cb, (typeof(p.reaction), CN, Mc))
            if length(rts) == 1 && isconcretetype(only(rts))
                H = only(rts)
                T = p.msgtype
                # The consumer task body the worker actually runs: a Serial component sub spawns
                # one `_consume_loop` over a plain `Zenoh.SubscriberHandler` with no novelty gate
                # (`Nothing`). Anchoring it bakes the receive→dispatch wrappers — `_consume_loop`
                # → `_dispatch_decoded` → `_run`/`with_payload_memory` → `decode_owned`/handler —
                # the loop the first message JITs. The leaf anchors below stay (they also serve the
                # warm path); this is the wrapper the leaf-only anchors didn't cover.
                precompile(_consume_loop, (Entity, Type{T}, H, Owned, Zenoh.SubscriberHandler, Nothing))
                precompile(_dispatch_decoded, (Entity, SampleHolder, Type{T}, H, Owned))
                precompile(decode_owned, (Memory{UInt8}, Type{T}))
                precompile(_decode_on_consumer, (Entity, SampleHolder, Type{T}))
                precompile(_invoke_owned, (Entity, T, H))
                # The Owned receive callback handed to Zenoh's (non-inlined) `with_payload_memory`
                # — a named `_OwnedRun{T,H}`, so the trampoline is anchorable. The `AbstractSample`
                # method (the consumer's `SampleHolder`) dispatches to the borrowed-`ZBytes` one.
                precompile(with_payload_memory, (_OwnedRun{T, H}, SampleHolder))
                precompile(with_payload_memory, (_OwnedRun{T, H}, Zenoh.ZBytes{Ptr{Zenoh.LibZenohC.z_loaned_bytes_t}}))
            end
        elseif p.kind === :service
            precompile(p.reaction, (CN, Mc, p.msgtype))
            # The request decode + response encode the server runs around the handler. A
            # component service builds with `warmup = :off`, so its own `_warm_service` is
            # suppressed — this is the only place that codec is anchored (offline bake + the
            # runtime member warm). Guarded: a hand-rolled service type that `request_type`/
            # `response_type` can't resolve degrades to the handler-only warm.
            try
                Req = request_type(p.msgtype); Resp = response_type(p.msgtype)
                precompile(decode_owned, (Memory{UInt8}, Type{Req}))           # view=false: copied Memory
                precompile(decode_view,  (Zenoh.PayloadView, Type{Req}))       # view=true: borrowed PayloadView
                precompile(encode, (Resp,))
                precompile(service_type_info_of, (Type{Req}, Type{Resp}))      # service-level wire type identity
                # The SERVE TREE around the handler — the same `ScopedValues.Scope{ResultCell{Query,
                # Resp}}` + `_serve_query`/`_decode_and_serve`/`settle_handler!` cluster baked for the
                # fixed parameter services in ROSNode's image, but keyed on THIS user service's
                # `Resp` (it depends on `Resp`, not the handler — so the abstract-`Function` handler
                # the component service stores still bakes it). Plus the consumer task body on the
                # concrete `_sub_cb` scheduler closure. Without this, the first request JITs the whole
                # ~400 ms tree (the dominant first-`run` cost on a service-bearing mixin; see
                # examples/startup/STARTUP-REPORT.md). The owned-query type is the FIFO queryable's.
                Q = Zenoh.Query{Base.RefValue{Zenoh.LibZenohC.z_owned_query_t}}
                precompile(_serve_query,            (Entity, Q, Type{Req}, Type{Resp}, Function, Bool))
                precompile(_spawn_service_consumer, (Entity, Type{Req}, Type{Resp}, Function, Bool, Serial))
                Hs = _cb_type(_sub_cb, p.reaction, CN, Mc)   # the concrete `_sub_cb` handler closure
                if Hs !== nothing
                    Ss = Base.return_types(_service_scheduler, (Serial, Entity, Type{Req}, Type{Resp}, Hs, Bool))
                    if length(Ss) == 1 && isconcretetype(only(Ss))
                        precompile(_service_consume_loop, (only(Ss), Zenoh.QueryableHandler, Entity))
                    end
                end
            catch
            end
        elseif p.kind === :timer
            precompile(p.reaction, (CN, Mc))
        elseif p.kind === :action
            # The action adapter calls the user handler via an `Any[]` splat, which inference
            # can't follow, so the server's adapter-frame warm never anchors the handler `f`.
            # Anchor it here with the exact signature the adapter calls (node-first):
            # `f(node, m, goalfields…, FeedbackSink{FB})`, sink inserted at `fb_pos`. Guarded —
            # a malformed reconstruction must never break configure/precompile.
            try
                f = p.reaction
                support = ActionTypeSupport(typeof(f))
                argtypes = Any[CN, Mc]
                for ft in fieldtypes(goal_type(support))
                    push!(argtypes, ft)
                end
                insert!(argtypes, p.extra.fb_pos, FeedbackSink{feedback_type(support)})
                precompile(f, (argtypes...,))
            catch
            end
        end
    end
    return nothing
end

# The concrete materialised-callback closure type for a reaction — the `_sub_cb`/`_timer_cb`
# the materialise path actually invokes (node-first: it captures the `ComponentNode` `CN` + the
# member `Mc`) — or `nothing` if it isn't a single concrete type.
function _cb_type(cb, @nospecialize(reaction), ::Type{CN}, ::Type{Mc}) where {CN, Mc}
    rts = Base.return_types(cb, (typeof(reaction), CN, Mc))
    (length(rts) == 1 && isconcretetype(only(rts))) ? only(rts) : nothing
end

# The `(callable, argtypes)` anchors for a mixin's CONSTRUCTION path — the machinery that
# *builds* a member at `run` (endpoint constructors on its message types, the materialise frame,
# the typed `ParameterServer{P_M}` + service wiring, lifecycle hooks), as opposed to the message
# *handlers* `_anchor_reactions!` bakes. That path is the dominant first-`run` cost and is reached
# at runtime only through the `Any`-typed member dispatch (`cnode.members[nm]`), so it is never
# statically reachable from ROSNode's own precompile image — only this base-type bake (in the
# consumer's `@precompile_nodes`) anchors it.
#
# Returned as a list, not emitted inline, so the drift test (`component_entities_static.jl`)
# consumes the SAME source and cannot fall behind a renamed/re-aritied builder. Empty for a
# non-concrete `M`. A parametric mixin's instantiation (e.g. `Guard{Sensor}`) is not known from
# the base alone, but `_anchor_node_plan!` resolves it from each `@node`'s static DI and calls
# this on the concrete type — so the specs key the mixin metadata on `_base(M)` (defined on the
# base only) while the anchored signatures use the concrete `M`. The endpoint builders are
# anchored on the concrete `_sub_cb`/`_timer_cb` closure types (the materialise path's
# `p.reaction` is `Any`, so inference can't reach them on its own — the function-barrier reason
# `_sub_cb` exists).
function _construction_precompile_specs(::Type{CN}, ::Type{Mc}) where {CN, Mc}
    specs = Tuple{Any, Any}[]
    isconcretetype(Mc) || return specs
    M = _base(Mc)
    P = pschema(M)
    # typed parameter server + the six standard parameter services (their fixed rcl_interfaces
    # codecs are baked into ROSNode's image by the warm-up workload).
    push!(specs, (ParameterServer, (Node, P)))
    push!(specs, (_build_pserver,  (Node, Type{P}, NamedTuple{(), Tuple{}})))
    # A mixin-as-node (`run(MyMixin)`) wires a `ParameterServer{P}`; bake its six services'
    # construction + handler bodies here (the `@node` path's `CompositeParameterServer` is baked
    # once in ROSNode's image). The named `_ParamSvcHandler` makes them precompilable by name.
    append!(specs, _parameter_service_specs(ParameterServer{P}))
    for p in mixin_spec(M).ports
        if p.kind === :publisher
            # the a-priori local-graph descriptor (`_port_descs` → `_publisher_desc`, positional/
            # default-qos): `PortSpec.msgtype` is abstractly typed, so `_prime_local_graph!` reaches
            # this concrete form only via dynamic dispatch — anchor it on the known `p.msgtype`.
            push!(specs, (_publisher_desc, (Node, String, Type{p.msgtype})))
            # the send path the first publish JITs: the builder, the encode + reusable-encoder +
            # CDR-write leaves, then the monomorphic publish over the plain `Zenoh.Publisher` route
            # (a component publisher carries no per-port QoS — see `_handle_type`).
            push!(specs, (_make_publisher, (Node, String, Type{p.msgtype})))
            push!(specs, (encode, (p.msgtype,)))
            push!(specs, (_encode_to_vector, (p.msgtype,)))
            push!(specs, (ReusableEncoder, (Type{p.msgtype},)))
            push!(specs, (publish, (PublisherHandle{p.msgtype, Zenoh.Publisher}, p.msgtype)))
        elseif p.kind === :subscription || p.kind === :service
            # the a-priori local-graph descriptor (positional/default-qos), reached only via
            # dynamic dispatch from `_prime_local_graph!` (see the publisher branch).
            push!(specs, (p.kind === :subscription ? _subscription_desc : _service_desc,
                          (Node, String, Type{p.msgtype})))
            # The materialise path passes `p.reaction` (typed `Any` in the spec), so the
            # construction call there isn't inferable on its own — anchor the inner builder on the
            # concrete `_sub_cb` closure type `H` (node-first: captures `CN` + `Mc`). It's built
            # with `warmup = :off` (a `Symbol`), so the heavy builder body specialises on that
            # kwarg type; pin the matching `Core.kwcall`.
            H = _cb_type(_sub_cb, p.reaction, CN, Mc)
            H === nothing && continue
            inner = p.kind === :subscription ? _make_subscription : _make_service
            kw = NamedTuple{(:warmup,), Tuple{Symbol}}
            push!(specs, (Core.kwcall, (kw, typeof(inner), H, Node, String, Type{p.msgtype})))
            # a service's consumer-task setup (the handler reaches `_spawn_service_consumer` as an
            # abstract `Function`; view=false ⇒ Bool, Serial concurrency).
            if p.kind === :service
                try
                    Req = request_type(p.msgtype); Resp = response_type(p.msgtype)
                    push!(specs, (_spawn_service_consumer, (Entity, Type{Req}, Type{Resp}, Function, Bool, Serial)))
                catch
                end
            end
        elseif p.kind === :timer
            H = _cb_type(_timer_cb, p.reaction, CN, Mc)
            H === nothing || push!(specs, (_paused_timer, (Node, Duration, H)))
        end
    end
    # the grounded typed materialise on the concrete node + member (pvalue is the param snapshot
    # `P` from `parameters(cnode, m)`), plus the member lifecycle hooks the fan-out invokes.
    push!(specs, (_build_ports, (CN, Mc, Dict{Symbol, String}, P)))
    for h in (configure, activate, deactivate, cleanup, on_error)
        push!(specs, (h, (CN, Mc)))
    end
    return specs
end

# `precompile`-only, no session, no run. Guarded so a quirk in one mixin (a `pschema`/spec that
# can't resolve) can't break the package's precompile.
function _anchor_construction!(::Type{CN}, ::Type{Mc}) where {CN, Mc}
    try
        for (f, ts) in _construction_precompile_specs(CN, Mc)
            precompile(f, ts)
        end
    catch err
        @debug "_anchor_construction!: skipped a construction bake" node = CN mixin = Mc exception = err
    end
    return nothing
end

# The `(callable, argtypes)` anchors for a mixin's action members' first-goal CODEC path: goal
# decode, result/feedback encode, and the three wire service wrappers' codecs (SendGoal /
# GetResult / CancelGoal) — the same set `_warm_action` warms. Keyed on the action type via the
# reaction, so it is independent of the entities accessor AND of `M`'s concreteness: it runs even
# for an action-bearing mixin, which `_finalize_module!` otherwise skips entirely (its action
# handle type isn't statically derivable, so `_ports_nt_type` returns `nothing`). The handler +
# dispatch still warm at first `run` via `_make_action_server`'s `_warm_action`; this just moves
# the codec offline. Built per port under its own guard so a not-yet-generated wrapper drops only
# that port's anchors. Returned as a list so the drift test consumes the same source.
function _action_codec_precompile_specs(::Type{M}) where {M}
    specs = Tuple{Any, Any}[]
    for p in mixin_spec(_base(M)).ports
        (p.kind === :action && p.reaction !== nothing) || continue
        try
            support = ActionTypeSupport(typeof(p.reaction))
            A = action_type(support)
            push!(specs, (decode_owned, (Memory{UInt8}, Type{goal_type(support)})))
            push!(specs, (encode, (result_type(support),)))
            push!(specs, (encode, (feedback_type(support),)))
            push!(specs, (decode_owned, (Memory{UInt8}, Type{_send_goal_request_type(A)})))
            push!(specs, (encode, (_send_goal_response_type(A),)))
            push!(specs, (decode_owned, (Memory{UInt8}, Type{_get_result_request_type(A)})))
            push!(specs, (encode, (_get_result_response_type(A),)))
            push!(specs, (encode, (_feedback_message_type(A),)))
            push!(specs, (decode_owned, (Memory{UInt8}, Type{_CancelGoal_Request})))
            push!(specs, (encode, (_CancelGoal_Response,)))
        catch err
            @debug "_action_codec_precompile_specs: skipped an action port" mixin = M port = p.name exception = err
        end
    end
    return specs
end

# `precompile`-only anchoring of the action codec specs above.
function _anchor_action_codecs!(::Type{M}) where {M}
    for (f, ts) in _action_codec_precompile_specs(M)
        precompile(f, ts)
    end
    return nothing
end

# Runtime warm: anchor the member's handlers against the concrete node + member types once
# materialised. Honours `node.warmup` via `_warmup!` (off / inline-sync / background).
# Subscriptions' own per-port warm is `:off`ed in `_materialize_ports!`; timers and services have
# no other warm. The per-`@node` bake (`@precompile_nodes` → `_anchor_node_plan!`) does the same
# ahead of time, so a baked node skips this work.
function _warm_member_reactions!(cnode::ComponentNode, m)
    _warmup!(cnode.node.warmup, () -> _anchor_reactions!(typeof(cnode), typeof(m)))
    return nothing
end

function _member_materialize!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    node = cnode.node
    wiremap = get(cnode.wires, nm, Dict{Symbol, String}())
    pvalue = parameters(cnode, m)               # the member's parameter snapshot (drives timer rates)
    # Stage B: only the declared port entities created here draw their reserved ids from the
    # node's queue — gate `make_entity` on this window so a user `configure` hook (which runs
    # between members) that creates an imperative endpoint can't steal a queued id.
    node._materialising = true
    ports = try
        # A finalized mixin dispatches to its generated typed `_build_ports` (grounded materialise,
        # concrete ports NamedTuple); an un-finalized one falls to the dynamic-walk method above.
        _build_ports(cnode, m, wiremap, pvalue)
    finally
        node._materialising = false
    end
    getfield(cnode.ports, nm).v = ports         # publish the handles into the node's typed cell
    # Now that the member's handles exist, anchor each reaction handler.
    _warm_member_reactions!(cnode, m)
    return nothing
end

# configure: materialise the member's ports, then run its `configure` hook. Returns
# the hook's value (the `failure` token aborts + rolls back the fan-out below).
function _member_configure!(cnode::ComponentNode, nm::Symbol)
    _member_materialize!(cnode, nm)
    return configure(cnode, cnode.members[nm])
end

# activate: start the member's (paused) timers, then run its `activate` hook. Returns
# the hook's value (the `failure` token aborts + rolls back the fan-out below). Timers live in
# the member's materialised handles, so start every `Timer` among them.
function _member_activate!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    for h in values(entities(cnode, m))
        h isa Timer && _start!(h)
    end
    return activate(cnode, m)
end

# Cancel-on-deactivate: cooperatively cancel every live goal on the member's action
# servers and bounded-wait for settle (budget = the Context drain timeout). Runs
# before the member `deactivate` hook so a goal body unwinds against state still
# valid (nav2 `terminate_all`). Guarded — a stuck server can't strand the fan-out.
# A no-op for a member with no materialised ports (never configured / already cleaned).
function _member_cancel_goals!(cnode::ComponentNode, nm::Symbol)
    cell = getfield(cnode.ports, nm)
    cell.v === nothing && return nothing
    budget = cnode.node.context.drain_timeout
    for h in values(cell.v)
        h isa ActionServer || continue
        try
            cancel_all!(h; timeout = budget)
        catch err
            @error "component: cancel_all! threw" member = nm exception = (err, catch_backtrace())
        end
    end
    return nothing
end

# deactivate: run the member's `deactivate` hook, guarded — this is also the
# rollback step for a failed `activate` fan-out, where one member's throw must not
# strand the rest's unwind (log-and-continue, like `_member_cleanup!`).
function _member_deactivate!(cnode::ComponentNode, nm::Symbol)
    try
        deactivate(cnode, cnode.members[nm])
    catch err
        @error "component: deactivate threw" member = nm exception = (err, catch_backtrace())
    end
    return nothing
end

# cleanup: run the member's `cleanup` hook, then close + drop its materialised ports
# (so a re-configure rematerialises). The managed-node dispatch gate already silenced them. A no-op
# when the member has no materialised ports — never configured, or already cleaned —
# so cleanup runs exactly once per configure (idempotent across teardown triggers).
function _member_cleanup!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    cell = getfield(cnode.ports, nm)
    cell.v === nothing && return nothing        # nothing acquired to release
    try
        cleanup(cnode, m)
    catch err
        @error "component: cleanup threw" member = nm exception = (err, catch_backtrace())
    end
    for h in values(cell.v)
        try; close(h); catch; end
    end
    cell.v = nothing
    return nothing
end

# on_error: run every member's hook, catching throws so one member cannot strand the
# rest's recovery. Any throw still means recovery failed: return the `failure` token,
# which `_handle_error!` honors by landing the node Finalized rather than silently
# recovering to Unconfigured.
function _members_on_error!(cnode::ComponentNode, order::Vector{Symbol})
    failed = false
    for nm in order
        try
            on_error(cnode, cnode.members[nm])
        catch err
            failed = true
            @error "component: on_error threw" member = nm exception = (err, catch_backtrace())
        end
    end
    return failed ? failure : nothing
end

# configure fan-out: forward in DI order. A member `configure` returning `failure`
# aborts the fan-out at that member (first-failure-wins) and unwinds the members that
# already configured — reverse order, `cleanup` + close ports — before returning the
# `failure` token (the lifecycle driver maps it to `:failure`, staying Unconfigured).
# A member throw propagates out (the lifecycle driver maps it to `:error`).
function _members_configure!(cnode::ComponentNode, order::Vector{Symbol})
    primed = _prime_local_graph!(cnode, order)        # one-shot a-priori local graph + reserved ids
    try
        for (i, nm) in enumerate(order)
            if _member_configure!(cnode, nm) === failure
                foreach(j -> _member_cleanup!(cnode, order[j]), i:-1:1)
                return failure
            end
        end
    finally
        _reconcile_local_graph!(cnode.node, primed)   # drop the queue + any aborted-member orphans
    end
    return nothing
end

# activate fan-out: forward in DI order. A member `activate` returning `failure`
# aborts at that member and unwinds the already-activated members — reverse order,
# guarded `deactivate` — before returning `failure` (lifecycle driver → `:failure`,
# staying Inactive). Ports stay materialised (a failed activate is still Inactive).
function _members_activate!(cnode::ComponentNode, order::Vector{Symbol})
    for (i, nm) in enumerate(order)
        if _member_activate!(cnode, nm) === failure
            foreach(j -> _member_deactivate!(cnode, order[j]), (i - 1):-1:1)
            return failure
        end
    end
    return nothing
end

# ── assembly (the one path) ──────────────────────────────────────────────────────

function _assemble(ctx::Context, @nospecialize(K), name, namespace, @nospecialize(overrides);
                   managed::Bool, autostart::Bool,
                   log_level::Union{LogLevel, Nothing} = nothing,
                   warmup::Union{Symbol, WarmupMode} = :off, warmup_sync::Bool = false)
    order, bytype, edges = _members_plan(K)
    composed = K isa NodeKind            # the construction path drives prefixing, not member count
    declared = composed ? Symbol[m.name for m in K.members] : copy(order)  # listing/view order
    # wire-topic remaps: resolve every member port's wire name (mixin-as-node has none).
    wires, remapped = composed ? _resolve_wires(K) :
                      (Dict{Symbol, Dict{Symbol, String}}(), Set{Tuple{Symbol, Symbol}}())
    cnref = Ref{Any}(nothing)            # the managed callbacks reach the cnode through this

    if managed
        ln = LifecycleNode(ctx, name; namespace = namespace,
            warmup = warmup, warmup_sync = warmup_sync,
            on_configure  = _ -> _members_configure!(cnref[], order),
            on_activate   = _ -> _members_activate!(cnref[], order),
            on_deactivate = _ -> (foreach(reverse(order)) do nm
                                      _member_cancel_goals!(cnref[], nm)   # cancel live goals before the hook
                                      _member_deactivate!(cnref[], nm)
                                  end; nothing),
            on_cleanup    = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(order)); nothing),
            on_shutdown   = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(order)); nothing),
            on_error      = _ -> _members_on_error!(cnref[], order))
        node = inner_node(ln)
    else
        ln = nothing
        node = Node(ctx, name; namespace = namespace, warmup = warmup, warmup_sync = warmup_sync)
    end
    # Before any member code (construct/configure/activate), so a requested level
    # governs the node's logging from the start.
    log_level === nothing || set_logger_level!(node, log_level)

    # `cnode`/`mbrs` are built INSIDE the try (the typed path needs the members before the node can
    # be typed), so declare them here for the catch/return to see.
    local cnode, mbrs

    # A failure mid-assembly (clobber/construct/DI/param-wiring/configure) must not leave
    # a half-built node — and its already-declared entities — orphaned on the (possibly
    # shared) Context. Tear it down before rethrowing; `close` is a no-op on the parts
    # not yet built.
    try
        # assembly-time clobber check: two members' same-channel outputs landing on
        # one wire name (without an explicit remap) is a hard error you fix by remapping.
        composed && _check_clobbers(K, node, wires, remapped)

        # Construct members in DI order; attach each member's typed ParameterServer{P_M}. No
        # materialise/configure here — that's the configure step. A composed `@node` with a generated
        # `_build_members` (the toposort'd straight-line constructor) builds a TYPED member NamedTuple
        # → grounded assembly + a typed `ComponentNode{Members}`; otherwise (mixin-as-node, or no bake
        # yet) the dynamic `Dict` path (unchanged behaviour). `cnref[]` is set before any managed
        # transition runs (the callbacks reach the cnode through it).
        if composed && hasmethod(_build_members, Tuple{Type{typeof(K)}, Node})
            mbrs = _build_members(typeof(K), node)        # typed NamedTuple of `M{name}` instances
        else
            mbrs = Dict{Symbol, Any}()
            for nm in order
                M = bytype[nm]
                deps = Any[mbrs[d] for d in edges[nm]]
                mbrs[nm] = construct(M, node, Val(nm), deps...)   # builds `M{nm}` (name threaded)
            end
        end
        # The node's typed runtime carriers, keyed by member name: per-member parameter servers
        # (built now) and empty port cells (filled at the configure step). A member holds no
        # reference back here, so `ComponentNode` stays acyclic. A finalized `@node` dispatches to
        # its generated straight-line builders (typed carriers → fully-typed `ComponentNode`, baked);
        # mixin-as-node / un-finalized falls to the equivalent dynamic generators.
        KT = typeof(K)
        pservers = composed && hasmethod(_build_pservers, Tuple{Type{KT}, Node, typeof(overrides)}) ?
                   _build_pservers(KT, node, overrides) :
                   (; (nm => _build_pserver(node, pschema(bytype[nm]), _member_overrides(bytype[nm], nm, overrides))
                       for nm in order)...)
        ports    = composed && hasmethod(_build_ports_carrier, Tuple{Type{KT}, Node}) ?
                   _build_ports_carrier(KT, node) :
                   (; (nm => PortCell{something(_ports_nt_type(mixin_spec(bytype[nm]).ports), Any)}()
                       for nm in order)...)
        cnode = ComponentNode{typeof(mbrs), typeof(ports), typeof(pservers)}(
                    node, mbrs, ports, pservers, order, wires, ln, nothing, true)
        cnref[] = cnode

        # node-level `ros2 param` surface: a mixin promoted to a node wires its
        # single server un-prefixed; a composed `@node` (even with one member) wires a
        # member-prefixed `CompositeParameterServer` over all members' servers, in declared
        # order (the `ros2 param list` order). Either way each member's `parameters(node, m)`
        # reads its own server, mixin-local.
        if composed
            members = Pair{Symbol, ParameterServer}[
                nm => getfield(cnode.pservers, nm) for nm in declared]
            facade = CompositeParameterServer(node, members)
            wire_parameter_services!(facade)
            _wire_composite_events!(facade)
            node.parameters = facade
        else
            srv = getfield(cnode.pservers, order[1])
            node.parameters = srv
            wire_parameter_services!(srv)
        end

        if managed
            # Log rather than throw on a failed transition: a managed node is
            # retryable via `configure!`.
            if autostart
                res = configure!(ln)
                res === :success ||
                    @error "component: autostart configure! failed" node = name result = res
                if state(ln) === Inactive()
                    res = activate!(ln)
                    res === :success ||
                        @error "component: autostart activate! failed" node = name result = res
                end
            end
        else
            primed = _prime_local_graph!(cnode, order)
            try
                foreach(nm -> _member_configure!(cnode, nm), order)
            finally
                _reconcile_local_graph!(cnode.node, primed)
            end
            foreach(nm -> _member_activate!(cnode, nm), order)
        end

        # the node tears down on the node-core's close. The dominant teardown is a
        # Context drain (a `container`/`run` shutdown closes the Context, not each
        # ComponentNode), so register a drain hook that fully closes the node — running
        # member `cleanup` in reverse DI order and, for a managed node, driving
        # `close(LifecycleNode)` so its dispatch-gate registration is dropped (the drain
        # otherwise closes the inner Node but not the wrapper). Idempotent with an explicit
        # `close(cn)`/`unload_node`: `close` no-ops on an already-closed node, and the
        # materialised-ports guard in `_member_cleanup!` runs cleanup once per configure.
        on_shutdown(ctx) do
            try
                close(cnode)
            catch err
                @error "component: teardown hook threw" node = name exception = (err, catch_backtrace())
            end
        end
    catch
        try; close(cnode); catch; end
        rethrow()
    end
    return cnode
end

# ── run (standalone) ─────────────────────────────────────────────────────────────

"""
    run(M::Type{<:Component}; …) -> ComponentNode
    run(N::NodeKind; …) -> ComponentNode

Instantiate a node standalone: open a `Context` (unless `ctx` is
given), build the node-core, construct the mixin(s) in DI order, and autostart
(configure → activate). Lifetime depends on `block`:

- `block=true` (default) — `spin` until shutdown, then close an owned Context on exit.
- `block=false` — return the node holding any owned Context; `close` on the node closes
  it after member teardown.
- A caller-supplied `ctx` is never closed.

`overrides` is a NamedTuple of mixin-local parameter values.
`log_level` (a `LogLevel`, default `nothing` = the node default) sets the
node's logger level ([`set_logger_level!`](@ref)) before any member code runs, so
records from the construct/configure/activate hooks already honor it.
`warmup`/`warmup_sync` (default `:off`) set the node's [`WarmupPolicy`](@ref) — opt into a
runtime warm-up of the dispatch chain for an un-precompiled node or a parametric mixin the
[`@precompile_nodes`](@ref) bake can't reach; deployed nodes rely on that offline bake instead.

The `managed` keyword selects the node-core kind:

- `managed=true` — a [`LifecycleNode`](@ref): the lifecycle control surface plus the
  managed-node dispatch gate ([`isactive`](@ref) — entities are silent unless the node
  is [`Active`](@ref)). Starts `Unconfigured` and is driven externally (`ros2
  lifecycle`), unless `autostart=true` brings it up.
- `managed=false` (default) — autostarts, with no control surface or gate.
"""
function Base.run(::Type{M}; name::AbstractString = _default_name(M),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  log_level::Union{LogLevel, Nothing} = nothing,
                  warmup::Union{Symbol, WarmupMode} = :off, warmup_sync::Bool = false,
                  block::Bool = true) where {M <: Component}
    _check_runnable(M, "run")
    _ensure_schema!(M)
    return _run(ctx, M, name, namespace, overrides, peers, localhost_only, managed, autostart,
                log_level, warmup, warmup_sync, block)
end

# `@nospecialize(k::NodeKind)`: without it the kwarg body (`#run#…`) specialises on the concrete
# `NodeKind{:N}`, so each `@node` re-infers the whole entry at first `run` (the biggest bring-up
# frame). The body is node-kind-agnostic — it reads `k.members` (data) and forwards to the already
# `@nospecialize`d `_run` — so one abstract-`NodeKind` body bakes into ROSNode's image for every node.
function Base.run(@nospecialize(k::NodeKind); name::AbstractString = String(k.name),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  log_level::Union{LogLevel, Nothing} = nothing,
                  warmup::Union{Symbol, WarmupMode} = :off, warmup_sync::Bool = false,
                  block::Bool = true)
    for mem in k.members
        _check_runnable(mem.mixin, "@node member `$(mem.name)`")
        _ensure_schema!(mem.mixin)
    end
    return _run(ctx, k, name, namespace, overrides, peers, localhost_only, managed, autostart,
                log_level, warmup, warmup_sync, block)
end

# Shared run body: open/own the Context, assemble (under `invokelatest` so freshly
# generated `P_M` methods are visible), then spin/close when blocking.
function _run(ctx, @nospecialize(K), name, namespace, overrides, peers, localhost_only,
              managed, autostart, log_level, warmup, warmup_sync, block)
    owns = ctx === nothing
    ctx = owns ? Context(; peers = collect(String, peers), localhost_only = localhost_only) : ctx
    local cnode
    try
        cnode = Base.invokelatest(_assemble, ctx, K, name, namespace, overrides;
                                  managed = managed, autostart = autostart,
                                  log_level = log_level, warmup = warmup, warmup_sync = warmup_sync)
    catch err
        owns && close(ctx)
        rethrow()
    end
    owns && (cnode.owned_ctx = ctx)
    if block
        try
            spin(ctx; handle_signals = owns)
        finally
            owns && close(ctx)
        end
    end
    return cnode
end

# ── container: composing nodes into one process ──────────────────────────────────

"""
    Container

A set of nodes sharing one process and one `Context` — the
deploy-time composition of `run`'s standalone form. Built by
[`container`](@ref); [`add!`](@ref) instantiates each node on the shared Context, so
the nodes share one session, discovery, type registry, and Julia scheduler, plus —
when intra-process delivery is enabled (`container`'s default) — the intra-process
short-circuit, where same-Context publisher↔subscriber pairs bypass serialize/Zenoh/decode.

The container is itself a ROS node (`inner_node(c)`, named after the container)
hosting the `~/_container/{load_node,unload_node,list_nodes}` services over the
vendored `composition_interfaces` types, so `ros2 component load/unload/list` and the
`ComposableNodeContainer` launch action drive it. Every loaded node is tracked under
a container-unique id (1-based, monotonic), reported by `list_nodes` / `ros2
component list` and addressable by `unload_node` / `ros2 component unload`.

Construct one through [`container`](@ref). `close(c)` closes the shared Context,
which drains every loaded node.
"""
mutable struct Container
    const ctx::Context
    const name::String
    const node::Node                           # the container management node (~/_container/* services)
    const nodes::Vector{ComponentNode}         # loaded nodes, in load order
    const loaded::Dict{UInt64, ComponentNode}  # unique_id => node (ros2 component list/unload)
    _next_uid::UInt64                          # monotonic 1-based id allocator (guarded by `lock`)
    const services::Vector{Any}                # the three _container service handles
    const lock::ReentrantLock
end
Base.show(io::IO, c::Container) = print(io, "Container(", c.name, ", ", length(c.nodes), " nodes)")
inner_context(c::Container) = c.ctx
"""
    inner_node(c::Container) -> Node

The container's own management node, hosting the `~/_container/{load_node,unload_node,list_nodes}`
composition services. The accessor returning the wrapped [`Node`](@ref); see
[`inner_node(::LifecycleNode)`](@ref) for the analogous unwrap on a managed node.
"""
inner_node(c::Container) = c.node
Base.close(c::Container) = close(c.ctx)

"""
    container([name="container"]; namespace=nothing, peers=String[],
              localhost_only=false, intra_process=true, block=true) do c
        add!(c, K; name=…, overrides=…, managed=…)
        …
    end -> Container

Compose nodes into one process. Opens a single `Context` — one
shared Zenoh session, discovery, type registry, and Julia scheduler — runs `f(c)` to
populate it via [`add!`](@ref), then parks on `spin` until the Context drains. The
same node code runs standalone under `run` or composed here; only the
deploy-time wiring differs.

Returns the live [`Container`](@ref). Behavior depends on `block`:

- `block=true` (default) — `spin` handles signals for the duration (interactively, the first
  Ctrl-C drains gracefully and a second forces exit; deployed, SIGINT and SIGTERM exit and
  drain via an atexit hook), and the call returns once the Context has drained, closing it.
- `block=false` — the call returns the running `Container` immediately, leaving you to
  drive it and `close(c)` it; the Context stays open until then.
- An exception during `f(c)` closes the Context and propagates.

The container is itself a node named `name` (optionally under `namespace`) exposing
the `~/_container/{load_node,unload_node,list_nodes}` services, so a running
container accepts `ros2 component load/unload/list` and the `ComposableNodeContainer`
launch action.

`intra_process=true` (default) enables the intra-process short-circuit (same-Context
publisher↔subscriber pairs bypass serialize/Zenoh/decode) through `set_intra_process!`,
a process-global switch.

```julia
container("vision") do c
    add!(c, Perception; name = "perception", overrides = (fps = 60,))
    add!(c, Recorder;   name = "recorder")
end
```
"""
function container(f::Function, name::AbstractString = "container";
                   namespace::Union{AbstractString, Nothing} = nothing,
                   peers::AbstractVector{<:AbstractString} = String[],
                   localhost_only::Bool = false,
                   intra_process::Bool = true, block::Bool = true)
    set_intra_process!(intra_process)
    ctx = Context(; peers = collect(String, peers), localhost_only = localhost_only)
    local c
    try
        node = Node(ctx, name; namespace = namespace)
        c = Container(ctx, String(name), node, ComponentNode[], Dict{UInt64, ComponentNode}(),
                      UInt64(0), Any[], ReentrantLock())
        _wire_container_services!(c)      # ~/_container/{load_node,unload_node,list_nodes}
        f(c)
    catch err
        close(ctx)
        rethrow()
    end
    if block
        try
            spin(ctx; handle_signals = true)
        finally
            close(ctx)
        end
    end
    return c
end

"""
    add!(c::Container, K; name=…, namespace=nothing, overrides=(;), managed=false,
         autostart=!managed) -> ComponentNode

Instantiate node kind `K` — a `@mixin` type promoted to a node (un-prefixed
parameters) or a `@node` composition (member-prefixed parameters) — on
container `c`'s shared Context, and track it under a container-unique id so it
appears in `list_nodes` / `ros2 component list` and can be `unload_node`-ed. Returns
the live `ComponentNode`. `name` defaults to the kind's name (a mixin's
snake-cased type name).

Forwards to `run(K; ctx = c.ctx, block = false, kwargs...)`: it constructs the mixins
in dependency order, wires the parameter services, and brings the node up. Unmanaged
(the default), the node autostarts (configure → activate) during this call;
`managed=true` declares the lifecycle control surface and the node starts
`Unconfigured` unless `autostart=true`, leaving an external orchestrator
(`ros2 lifecycle`) to drive it. `overrides` is a NamedTuple of parameter values keyed
by mixin-local name (`fps`), or by prefixed `var"member.field"` to target one member
when two members share a field name.

These keywords are rejected with an `ArgumentError`, each fixed by the container:

- `ctx` — pinned to `c.ctx`; a foreign `ctx` would build a node the container tracks
  but whose Context it never closes.
- `block` — pinned to `false`; `block=true` would park `add!` on `spin`.
- `peers`, `localhost_only` — transport is fixed when `container` opens the Context, so
  `run`'s values would have no effect here.

```julia
container("fleet") do c
    vehicle = add!(c, Vehicle; name = "vehicle", overrides = (min_battery = 90.0,))
    add!(c, GroundStation; name = "ground", managed = true)
end
```
"""
function add!(c::Container, @nospecialize(K); kwargs...)
    # Guard explicitly: the last-wins kwarg splat would otherwise let ctx/block
    # silently override the pinned values, and peers/localhost_only only apply
    # when `run` owns the Context (it never does here).
    for k in (:ctx, :block, :peers, :localhost_only)
        haskey(kwargs, k) &&
            throw(ArgumentError("add!: `$k` is fixed by the container; set transport options on `container`"))
    end
    cn = run(K; ctx = c.ctx, block = false, kwargs...)
    _track_loaded!(c, cn)
    return cn
end

export container, add!, Container
