# Node assembly + run/container. A `NodeSchema` (from `node(…)`) names a
# collection of members; `run(schema)` builds a node-core, constructs each member (in DI
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

export describe_wiring

# ── the node-core wrapper ───────────────────────────────────────────────────────

"""
    ComponentNode

A live component node: the node-core (a `Node`, or the inner `Node` of
a `LifecycleNode` when managed) shared by the node's member components, the constructed
member instances by name, and the `LifecycleNode` handle (`lifecycle`) when managed.
Returned by `run` / held by a [`Container`](@ref).
"""
# `Members` is the typed member container — a `NamedTuple{names, Tuple{concrete component types}}`
# built by `build_members` (functor.jl) from the resolved schema, so every member is grounded and
# `members[nm]` (`getindex` by Symbol) reads a concrete type with concrete dispatch. `::ComponentNode`
# sites match the UnionAll.
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
#   Members  — `@NamedTuple{a::Counter{:a}, …}` (typed, built by `build_members`)
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
    # The frozen per-member `MemberSchema` NamedTuple — the SOURCE OF TRUTH for introspection
    # (`_ordered_member_descs`/`describe_wiring`) so they read the node's ACTUAL descriptors (incl.
    # inline `component(…)` members with no `member_schema`, and rebound/remapped handlers) instead
    # of re-deriving `member_schema(base)`. Always populated (the schema's `.members`).
    const member_schemas::Any
    lifecycle::Any                  # the LifecycleNode (managed) or nothing
    owned_ctx::Union{Context, Nothing}  # the Context `run` opened for this node, closed by
                                        # `close`; `nothing` when the caller supplied one
    _shutdown_hook::Any             # the `on_shutdown` closure registered against a BORROWED ctx; `close`
                                    # deregisters it (+ the node resource) so the ctx doesn't retain us
    @atomic open::Bool              # single-winner close latch (unmanaged path)
end

# ── accessors (methods of the `entities`/`parameters` generics declared in component.jl) ──
# `_path(m)` is a constant lifted from the component's type parameter, so `getfield(carrier, _path(m))`
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
declared order, each value the member's per-member snapshot (see
[`parameters(node, m)`](@ref)). So `parameters(node).camera` is member `camera`'s
live snapshot, and the result is heterogeneous — for iterating a whole node, where
`parameters(node, m)` is the type-stable per-member view. The flat `<member>.<field>` wire
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
    (kind === :service || kind === :client || kind === :action ||
     kind === :service_client || kind === :action_client) ? :service : :topic
_port_kindstr(kind::Symbol) =
    kind === :publisher ? "pub" : kind === :subscription ? "sub" :
    kind === :service   ? "srv" : kind === :client       ? "cli" :
    kind === :action    ? "act" :
    kind === :service_client ? "cli" : kind === :action_client ? "acli" :
    kind === :timer ? "timer" : String(kind)

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

# Per-member wiring rows `(name, kind, default-wire)` from the node's FROZEN per-member `MemberSchema`.
# The method (over a `MemberSchema`) is provided by functor.jl (loaded later).
function _functor_describe_ports end

function describe_wiring(io::IO, c::ComponentNode)
    ms = c.member_schemas                    # the frozen per-member MemberSchema NT
    println(io, "wiring of ", c.node.fqn, " — ", length(c.order), " member(s)")
    for mem in c.order
        base = _base(typeof(c.members[mem]))
        rows = _functor_describe_ports(getfield(ms, mem))
        println(io, "  ", mem, " :: ", nameof(base))
        isempty(rows) && (println(io, "    (no ports)"); continue)
        wmap = get(c.wires, mem, Dict{Symbol, String}())
        for (pname, pkind, dwire) in rows
            kindstr = _port_kindstr(pkind)
            if pkind === :timer
                println(io, "    ", rpad(string(pname), 18), kindstr)
                continue
            end
            wire = get(wmap, pname, dwire)
            fqn  = resolve_name(c.node, wire; kind = _resolve_kind(pkind))
            println(io, "    ", rpad(string(pname), 18), rpad(kindstr, 6),
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
    if c.owned_ctx === nothing
        # Borrowed ctx (e.g. loaded onto a composition container): release our registrations so the
        # long-lived ctx doesn't retain a closed node/graph after unload. Idempotent (filter-by-identity).
        ctx = c.node.context
        c._shutdown_hook === nothing || deregister_on_shutdown!(ctx, c._shutdown_hook)
        deregister_resource!(ctx, c.node)
    else
        is_shutdown(c.owned_ctx) || close(c.owned_ctx)
    end
    return nothing
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


# A ROS-clock timer built but NOT started (its `Base.Timer` is created at activate, so
# a tick can't fire before configure / while a managed node is gated).
function _paused_timer(node::Node, period::Duration, f)
    c = clock(node, ROS())
    Timer{ROS}(c, period, f, true, nothing)
end


_build_pserver(node::Node, ::Type{P}, overrides::NamedTuple) where {P} =
    ParameterServer{P}(node; overrides = overrides)

# ── per-member lifecycle steps (run at the matching transition) ──────────────────

# The base for instance-keyed lookups: `Guard{Sensor} → Guard`, identity for plain
# types. Registries and generated accessors are keyed on the registered base, so
# `typeof(m)` (a concrete instantiation for a parametric component) must normalize first.
_base(::Type{T}) where {T} = Base.typename(T).wrapper


# The concrete materialised-callback closure type for a reaction — the `_sub_cb`/`_timer_cb`
# the materialise path actually invokes (node-first: it captures the `ComponentNode` `CN` + the
# member `Mc`) — or `nothing` if it isn't a single concrete type.
function _cb_type(cb, @nospecialize(reaction), ::Type{CN}, ::Type{Mc}) where {CN, Mc}
    rts = Base.return_types(cb, (typeof(reaction), CN, Mc))
    (length(rts) == 1 && isconcretetype(only(rts))) ? only(rts) : nothing
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


# ── run (standalone) ─────────────────────────────────────────────────────────────

"""
    run(M::Type{<:Component}; …) -> ComponentNode
    run(schema::NodeSchema; …) -> ComponentNode

Instantiate a node standalone: open a `Context` (unless `ctx` is
given), build the node-core, construct the component(s) in DI order, and autostart
(configure → activate). `run(M::Type)` promotes a single component to a one-member node
(see [`node`](@ref)); `run(::NodeSchema)` (functor.jl) runs an assembled kind. Lifetime
depends on `block`:

- `block=true` (default) — `spin` until shutdown, then close an owned Context on exit.
- `block=false` — return the node holding any owned Context; `close` on the node closes
  it after member teardown.
- A caller-supplied `ctx` is never closed.

`overrides` is a NamedTuple of (un-prefixed) parameter values.
`log_level` (a `LogLevel`, default `nothing` = the node default) sets the
node's logger level ([`set_logger_level!`](@ref)) before any member code runs, so
records from the construct/configure/activate hooks already honor it.
`warmup`/`warmup_sync` (default `:off`) set the node's [`WarmupPolicy`](@ref) — opt into a
runtime warm-up of the dispatch chain for an un-precompiled node or a parametric component the
[`precompile_node`](@ref) bake can't reach; deployed nodes rely on that offline bake instead.

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
                  flat::Bool = true, block::Bool = true) where {M <: Component}
    # Promote a single component to a standalone node (see `_run_functor_promote`, functor.jl).
    # `_asschema` errors clearly if `M` has no `member_schema` method.
    return _run_functor_promote(M; name = name, namespace = namespace, overrides = overrides,
                ctx = ctx, peers = peers, localhost_only = localhost_only, managed = managed,
                autostart = autostart, log_level = log_level,
                warmup = warmup, warmup_sync = warmup_sync, flat = flat, block = block)
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

Instantiate node kind `K` — a component `Type` promoted to a node (un-prefixed
parameters) or a `NodeSchema` composition (member-prefixed parameters) — on
container `c`'s shared Context, and track it under a container-unique id so it
appears in `list_nodes` / `ros2 component list` and can be `unload_node`-ed. Returns
the live `ComponentNode`. `name` defaults to the kind's name (a component's
snake-cased type name).

Forwards to `run(K; ctx = c.ctx, block = false, kwargs...)`: it constructs the components
in dependency order, wires the parameter services, and brings the node up. Unmanaged
(the default), the node autostarts (configure → activate) during this call;
`managed=true` declares the lifecycle control surface and the node starts
`Unconfigured` unless `autostart=true`, leaving an external orchestrator
(`ros2 lifecycle`) to drive it. `overrides` is a NamedTuple of parameter values keyed
by un-prefixed name (`fps`), or by prefixed `var"member.field"` to target one member
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
