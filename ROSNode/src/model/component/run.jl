# Node assembly + run/container (DESIGN-COMPONENTS.md §4/§5/§7). `@node` names a
# collection of members; `run(K)` builds a node-core, constructs each member (in DI
# dependency order, §4.2), and drives the lifecycle; `container` composes nodes into
# one process (§7).
#
# One assembly path serves both flavours (§5 "vocabulary always, surface opt-in"):
#   - unmanaged (default): the node-core is a plain `Node`; assembly autostarts
#     (configure → activate) and there is no control surface or gating cost.
#   - managed (`managed=true`): the node-core is a `LifecycleNode` (the five
#     lifecycle_msgs services + ~/transition_event + dispatch gating, §14.2); its
#     transitions fan out to the members' `configure`/`activate`/… hooks. Ports
#     materialise at the configure transition, so an inactive node's entities are
#     gated/silent until `activate`.
#
# Deferred: member-name namespacing for node-level `ros2 param` on multi-member nodes
# (§4.4 — per-member servers work; `parameters(m)` is correct), `LoadNode`/`ros2
# component` dynamic composition (§7), and a §D8 warm-up hook (first-run JIT latency).

export @node

# ── the node-core wrapper ───────────────────────────────────────────────────────

"""
    ComponentNode

A live component node (DESIGN §4/§5): the node-core (a `Node`, or the inner `Node` of
a `LifecycleNode` when managed) shared by the node's member mixins, the constructed
member instances by name, and the `LifecycleNode` handle (`lifecycle`) when managed.
Returned by [`run`](@ref) / held by a [`Container`](@ref).
"""
mutable struct ComponentNode
    const node::Node
    const members::Dict{Symbol, Any}
    const order::Vector{Symbol}     # member names in DI construction (toposort) order — drives
                                    # lifecycle fan-out + reverse-order teardown (§4.2/§5)
    lifecycle::Any                  # the LifecycleNode (managed) or nothing
end

Base.show(io::IO, c::ComponentNode) =
    print(io, "ComponentNode(", c.node.fqn, c.lifecycle === nothing ? "" : ", managed",
          ", members=", c.order, ")")

"""
    parameters(node::ComponentNode) -> NamedTuple

The node-level, member-namespaced parameter aggregation (§3.5/§4.4):
`parameters(node).camera` is member `camera`'s live snapshot. Heterogeneous (one
entry per member, keyed by member name in declared order) — for iterating a whole
node, where `parameters(m)` is the type-stable per-mixin view.
"""
parameters(c::ComponentNode) = (; (nm => parameters(c.members[nm]) for nm in c.order)...)

"""
    entities(node::ComponentNode) -> NamedTuple

The node-level, member-namespaced handle aggregation (§3.5/§4.4):
`entities(node).camera.image` is member `camera`'s `image` port (so two members'
`image` handles don't collide). Member ports must be materialised first (after the
node is configured), like the per-mixin [`entities`](@ref).
"""
entities(c::ComponentNode) = (; (nm => entities(c.members[nm]) for nm in c.order)...)

inner_node(c::ComponentNode) = c.node
"The `LifecycleNode` driving a managed component node, or `nothing` (unmanaged)."
lifecycle(c::ComponentNode) = c.lifecycle

# Closing a node tears it down (§5): run each member's `cleanup` in reverse DI order,
# then undeclare its entities + node token. Unmanaged closes here directly; managed
# routes through `close(LifecycleNode)` → `shutdown!` → the `on_shutdown` hook, which
# fans out the same `_member_cleanup!`. `_member_cleanup!` is a no-op on a member whose
# ports aren't materialised (never configured / already cleaned), so this is safe on a
# half-built node and idempotent under a double close.
function Base.close(c::ComponentNode)
    if c.lifecycle === nothing
        foreach(nm -> _member_cleanup!(c, nm), reverse(c.order))
        close(c.node)
    else
        close(c.lifecycle)
    end
    return nothing
end

# ── @node ─────────────────────────────────────────────────────────────────────

"A member of a `@node`: a name within the node and the mixin type filling it (§4.4)."
struct NodeMember
    name::Symbol
    mixin::Type
end

"""
    NodeKind

A named collection of [`NodeMember`](@ref)s (DESIGN §4) — the result of `@node N =
[…]`. `run(N)` / `add!(c, N)` instantiate it.
"""
struct NodeKind
    name::Symbol
    members::Vector{NodeMember}
end

Base.show(io::IO, k::NodeKind) =
    print(io, "NodeKind(", k.name, ", ", [m.name => nameof(m.mixin) for m in k.members], ")")

"""
    @node N = ["name" => Mixin, …]

Assemble a node from member mixins (DESIGN §4/§4.4). Each member is given an **explicit
name** — a string — which is its namespace within the node (the prefix for its
parameters and its `entities(node)` slice, §4.3/§4.4). The name is written out rather
than derived, so the namespace is never a surprise. `run(N)` / `add!(container, N)`
instantiate it.
"""
macro node(assign)
    (assign isa Expr && assign.head === :(=)) ||
        error("@node expects `N = [\"name\" => Mixin, …]`")
    N, rhs = assign.args[1], assign.args[2]
    members = _parse_members(rhs)
    memexprs = [:( $(NodeMember)($(QuoteNode(nm)), $(esc(mx))) ) for (nm, mx) in members]
    # Also register the kind by name (the `rclcpp_components_register_nodes` analog, §7)
    # so a container's `load_node` can instantiate it from a `ros2 component load` request.
    return quote
        const $(esc(N)) = $(NodeKind)($(QuoteNode(N)), $(NodeMember)[$(memexprs...)])
        $(register_node_kind!)($(string(N)), $(esc(N)))
        $(esc(N))
    end
end

# Each member is `"name" => Mixin` (string name — the namespace, §4.4). A bare identifier
# name (`name => Mixin`) is accepted too, but the type is never auto-named: an explicit
# name is required, so a member's namespace is always written, not derived.
function _parse_members(rhs)
    (rhs isa Expr && rhs.head in (:vect, :vcat)) ||
        error("@node: the right-hand side must be a list, `[\"name\" => Mixin, …]`")
    out = Tuple{Symbol, Any}[]
    seen = Set{Symbol}()
    for el in rhs.args
        (el isa Expr && el.head === :call && el.args[1] === :(=>)) ||
            error("@node: each member needs an explicit name — `\"name\" => Mixin` (got `$(el)`)")
        nm = el.args[2]
        name = nm isa AbstractString ? Symbol(nm) :
               nm isa Symbol ? nm :
               error("@node: a member name must be a string, e.g. `\"sensor\" => Sensor` (got `$(nm)`)")
        # The member name is the §4.4 namespace prefix (`<member>.<field>`), so it must be
        # a single dotless segment — a dot would mis-split the prefixed parameter name —
        # and unique, or two members would share one prefix / clobber in the member Dict.
        occursin('.', String(name)) &&
            error("@node: member name `$(name)` must not contain a dot — it is the `<member>.<field>` parameter prefix (§4.4)")
        name in seen &&
            error("@node: duplicate member name `$(name)` — member names must be unique within a node (§4.4)")
        push!(seen, name)
        push!(out, (name, el.args[3]))
    end
    return out
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

# Create each declared port's runtime handle against the node-core. Returns the
# handle NamedTuple (`entities(m)`) and the paused timers to start at activate.
function _materialize_ports!(node::Node, m, specs::Vector{PortSpec}, pvalue)
    handles = Pair{Symbol, Any}[]
    timers = Any[]
    for p in specs
        if p.kind === :publisher
            push!(handles, p.name => Publisher(node, _wire(p), p.msgtype))
        elseif p.kind === :subscription
            react = p.reaction
            sub = Subscription(node, _wire(p), p.msgtype) do msg
                react(m, msg)
            end
            push!(handles, p.name => sub)
        elseif p.kind === :service
            react = p.reaction
            srv = Service(node, _wire(p), p.msgtype) do req
                react(m, req)
            end
            push!(handles, p.name => srv)
        elseif p.kind === :action
            f = p.reaction
            support = ActionTypeSupport(typeof(f))
            body = _component_action_adapter(f, support, p.extra.fb_pos, m)
            push!(handles, p.name => _make_action_server(node, _wire(p), typeof(f); body = body))
        elseif p.kind === :timer
            hz = p.extra.rate isa Symbol ? getproperty(pvalue, p.extra.rate) : p.extra.rate
            react = p.reaction
            t = _paused_timer(node, Duration(round(Int64, 1.0e9 / hz)), () -> react(m))
            push!(handles, p.name => t)
            push!(timers, t)
        else
            @warn "component: port kind :$(p.kind) not yet materialised" port = p.name type = typeof(m)
        end
    end
    return (NamedTuple(handles), timers)
end

# A port's wire name: its explicit `on \"…\"` override, else the identifier (§3).
_wire(p::PortSpec) = p.wire === nothing ? String(p.name) : p.wire

# A ROS-clock timer built but NOT started (its `Base.Timer` is created at activate, so
# a tick can't fire before configure / while a managed node is gated).
function _paused_timer(node::Node, period::Duration, f)
    c = clock(node, ROS())
    Timer{ROS}(c, period, f, true, nothing)
end

# ── DI resolution + toposort (§4.2) ─────────────────────────────────────────────

# Resolve each member's `requires` to a single providing sibling (excluding self),
# returning `member-name => [provider names]` in `requires` order. Errors on an
# unsatisfied or ambiguous interface (§4.2).
function _resolve_di(members::Vector{NodeMember})
    providers = Dict{Type, Vector{Symbol}}()
    for mem in members, I in provides(mem.mixin)
        push!(get!(Vector{Symbol}, providers, I), mem.name)
    end
    edges = Dict{Symbol, Vector{Symbol}}()
    for mem in members
        deps = Symbol[]
        for I in requires(mem.mixin)
            cands = filter(!=(mem.name), get(providers, I, Symbol[]))
            isempty(cands) &&
                error("@node: member `$(mem.name)` ($(mem.mixin)) requires $(I), but no " *
                      "other member provides it")
            length(cands) == 1 ||
                error("@node: member `$(mem.name)` requires $(I), provided by multiple " *
                      "members $(cands) — pin one with `requires(::Type{$(mem.mixin)}) = ($(I) => :member,)`")
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
function _members_plan(k::NodeKind)
    edges = _resolve_di(k.members)
    order = _toposort(k.members, edges)
    return (order, Dict{Symbol, Type}(m.name => m.mixin for m in k.members), edges)
end

# Filter run/add! overrides to this member (§4.3): a mixin-local key (`fps`) applies
# to every member that declares it; a prefixed key (`var"camera.fps"` — the form a
# composed node's wire `Parameter.name` carries, and the disambiguator when two
# members share a field name) targets one member and wins over the mixin-local form.
function _member_overrides(::Type{M}, member::Symbol, overrides::NamedTuple) where {M}
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

# ── per-member lifecycle steps (run at the matching transition, §5) ─────────────

function _member_materialize!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    rt = getfield(m, :__rt__)::MixinRuntime
    ports, timers = _materialize_ports!(cnode.node, m, mixin_spec(typeof(m)).ports,
                                        current(rt.pserver))
    rt.ports = ports
    rt.timers = timers
    _ensure_entities_accessor!(typeof(m), ports)   # type-stable `entities(m)::PortsNT` (§3.5)
    return nothing
end

# configure: materialise the member's ports, then run its `configure` hook.
function _member_configure!(cnode::ComponentNode, nm::Symbol)
    _member_materialize!(cnode, nm)
    configure(cnode.members[nm])
    return nothing
end

# activate: start the member's (paused) timers, then run its `activate` hook.
function _member_activate!(cnode::ComponentNode, nm::Symbol)
    rt = getfield(cnode.members[nm], :__rt__)::MixinRuntime
    for t in rt.timers
        _start!(t)
    end
    activate(cnode.members[nm])
    return nothing
end

# cleanup: run the member's `cleanup` hook, then close + drop its materialised ports
# (so a re-configure rematerialises). Gating (§14.2) already silenced them. A no-op
# when the member has no materialised ports — never configured, or already cleaned —
# so cleanup runs exactly once per configure (idempotent across teardown triggers, §5).
function _member_cleanup!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    rt = getfield(m, :__rt__)::MixinRuntime
    rt.ports === nothing && return nothing      # nothing acquired to release
    try
        cleanup(m)
    catch err
        @error "component: cleanup threw" member = nm exception = (err, catch_backtrace())
    end
    for h in values(rt.ports)
        try; close(h); catch; end
    end
    rt.ports = nothing
    empty!(rt.timers)
    return nothing
end

# ── assembly (the one path; §4/§5) ──────────────────────────────────────────────

function _assemble(ctx::Context, @nospecialize(K), name, namespace, overrides;
                   managed::Bool, autostart::Bool)
    order, bytype, edges = _members_plan(K)
    composed = K isa NodeKind            # the construction path drives prefixing (§4.3), not member count
    declared = composed ? Symbol[m.name for m in K.members] : copy(order)  # listing/view order
    cnref = Ref{Any}(nothing)            # the managed callbacks reach the cnode through this

    if managed
        ln = LifecycleNode(ctx, name; namespace = namespace,
            on_configure  = _ -> (foreach(nm -> _member_configure!(cnref[], nm), order); nothing),
            on_activate   = _ -> (foreach(nm -> _member_activate!(cnref[], nm), order); nothing),
            on_deactivate = _ -> (foreach(nm -> deactivate(cnref[].members[nm]), reverse(order)); nothing),
            on_cleanup    = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(order)); nothing),
            on_shutdown   = _ -> (foreach(nm -> _member_cleanup!(cnref[], nm), reverse(order)); nothing),
            on_error      = _ -> (foreach(nm -> on_error(cnref[].members[nm]), order); nothing))
        node = inner_node(ln)
    else
        ln = nothing
        node = Node(ctx, name; namespace = namespace)
    end

    cnode = ComponentNode(node, Dict{Symbol, Any}(), order, ln)
    cnref[] = cnode

    # A failure mid-assembly (construct/DI/param-wiring/configure) must not leave a
    # half-built node — and its already-declared entities — orphaned on the (possibly
    # shared) Context. Tear it down before rethrowing; `close` is a no-op on the parts
    # not yet built.
    try
        # construct members in dependency order, injecting resolved siblings (§4.2); attach
        # each member's typed ParameterServer{P_M}. No materialise/configure here — that
        # happens at the configure step (immediately for unmanaged, at the transition for managed).
        for nm in order
            M = bytype[nm]
            deps = Any[cnode.members[d] for d in edges[nm]]
            m = construct(M, node, deps...)
            pserver = _build_pserver(node, pschema(M), _member_overrides(M, nm, overrides))
            setfield!(m, :__rt__, MixinRuntime(cnode, nm, pserver, nothing, Any[]))
            cnode.members[nm] = m
        end

        # node-level `ros2 param` surface (§4.3/§4.4): a mixin promoted to a node wires its
        # single server un-prefixed; a composed `@node` (even with one member) wires a
        # member-prefixed `CompositeParameterServer` over all members' servers, in declared
        # order (the `ros2 param list` order). Either way each member's `parameters(m)`
        # reads its own server, mixin-local (§3.5).
        if composed
            members = Pair{Symbol, ParameterServer}[
                nm => (getfield(cnode.members[nm], :__rt__)::MixinRuntime).pserver for nm in declared]
            facade = CompositeParameterServer(node, members)
            wire_parameter_services!(facade)
            _wire_composite_events!(facade)
            node.parameters = facade
        else
            srv = (getfield(cnode.members[order[1]], :__rt__)::MixinRuntime).pserver
            node.parameters = srv
            wire_parameter_services!(srv)
        end

        if managed
            autostart && (configure!(ln); state(ln) === Inactive() && activate!(ln))
        else
            foreach(nm -> _member_configure!(cnode, nm), order)
            foreach(nm -> _member_activate!(cnode, nm), order)
        end

        # §5: a mixin's `cleanup` runs on the node-core's teardown. The dominant teardown
        # is a Context drain (a `container`/`run` shutdown closes the Context, not each
        # ComponentNode), so register a drain hook that fans out member cleanup in reverse
        # DI order. Idempotent with an explicit `close(cn)`/`unload_node` (the
        # materialised-ports guard in `_member_cleanup!` makes cleanup run once per
        # configure), and it covers a managed node too (drain closes the inner Node, not
        # the LifecycleNode, so its transition hooks don't otherwise fire at drain).
        on_shutdown(ctx) do
            try
                foreach(nm -> _member_cleanup!(cnode, nm), reverse(order))
            catch err
                @error "component: teardown cleanup hook threw" node = name exception = (err, catch_backtrace())
            end
        end
    catch
        try; close(cnode); catch; end
        rethrow()
    end
    return cnode
end

# ── run (standalone, §5/§7) ──────────────────────────────────────────────────────

"""
    run(M::Type{<:Component}; …) -> ComponentNode
    run(N::NodeKind; …) -> ComponentNode

Instantiate a node standalone (DESIGN §5/§7): open a `Context` (unless `ctx` is
given), build the node-core, construct the mixin(s) in DI order, autostart
(configure → activate), and — when `block` (default) — `spin` until shutdown, closing
an owned Context on exit. `overrides` is a NamedTuple of mixin-local parameter values
(§4.3).

`managed=true` makes the node-core a `LifecycleNode` (the lifecycle control surface +
dispatch gating, §5/§14.2); it then starts `Unconfigured` and is driven externally
(`ros2 lifecycle`) unless `autostart=true`. Unmanaged (default) autostarts and has no
control surface.
"""
function Base.run(::Type{M}; name::AbstractString = _default_name(M),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  block::Bool = true) where {M <: Component}
    ismixin(M) || error("run: $(M) is not a @mixin")
    _ensure_schema!(M)
    return _run(ctx, M, name, namespace, overrides, peers, localhost_only, managed, autostart, block)
end

function Base.run(k::NodeKind; name::AbstractString = String(k.name),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  block::Bool = true)
    for mem in k.members
        _ensure_schema!(mem.mixin)
    end
    return _run(ctx, k, name, namespace, overrides, peers, localhost_only, managed, autostart, block)
end

# Shared run body: open/own the Context, assemble (under `invokelatest` so freshly
# generated `P_M` methods are visible), then spin/close when blocking.
function _run(ctx, @nospecialize(K), name, namespace, overrides, peers, localhost_only,
              managed, autostart, block)
    owns = ctx === nothing
    ctx = owns ? Context(; peers = collect(String, peers), localhost_only = localhost_only) : ctx
    local cnode
    try
        cnode = Base.invokelatest(_assemble, ctx, K, name, namespace, overrides;
                                  managed = managed, autostart = autostart)
    catch err
        owns && close(ctx)
        rethrow()
    end
    if block
        try
            spin(ctx; handle_signals = owns)
        finally
            owns && close(ctx)
        end
    end
    return cnode
end

# ── container: composing nodes into one process (§7) ────────────────────────────

"""
    Container

A set of nodes sharing one process / `Context` (DESIGN §7) — the deploy-time
composition of [`run`](@ref)'s standalone form. Built by [`container`](@ref); `add!`
instantiates nodes on its shared Context. It is itself a ROS node (`node`, named
after the container) hosting the `~/_container/{load_node,unload_node,list_nodes}`
services, so `ros2 component load/unload/list` drives it (§7); every loaded node
gets a container-unique id (`loaded`) for `ros2 component list`/`unload`.
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
"The container's own management node (hosts the `~/_container/*` composition services)."
inner_node(c::Container) = c.node
Base.close(c::Container) = close(c.ctx)

"""
    container([name]) do c
        add!(c, K; name=…, overrides=…, managed=…)
        …
    end -> Container

Compose nodes into one process (DESIGN §7): open a single `Context` — shared session,
discovery, type registry, thread pool — run the block to populate it via [`add!`](@ref),
then `spin` until shutdown (unless `block=false`, which returns the live `Container`).
With `intra_process=true` (default) same-Context publisher↔subscriber pairs take the
§15.1 direct in-process path (process-global today). Same node code as standalone
`run` — only the deploy-time wiring differs.

The container is itself a node (named `name`, optionally under `namespace`) exposing
the `~/_container/{load_node,unload_node,list_nodes}` services (§7), so a running
container accepts `ros2 component load/unload/list` and the `ComposableNodeContainer`
launch action.
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
        _wire_container_services!(c)      # ~/_container/{load_node,unload_node,list_nodes} (§7)
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
    add!(c::Container, K; name, namespace, overrides, managed) -> ComponentNode

Instantiate node kind `K` (a `@mixin` used as a node, or a `@node`) on the container's
shared Context (§7) and track it with a container-unique id (so it shows up in
`ros2 component list` and can be `unload`ed). Returns the live [`ComponentNode`](@ref).
"""
function add!(c::Container, @nospecialize(K); kwargs...)
    cn = run(K; ctx = c.ctx, block = false, kwargs...)
    _track_loaded!(c, cn)
    return cn
end

export container, add!, Container
