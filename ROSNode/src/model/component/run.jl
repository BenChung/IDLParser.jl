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
mutable struct ComponentNode
    const node::Node
    const members::Dict{Symbol, Any}
    const order::Vector{Symbol}     # member names in DI construction (toposort) order — drives
                                    # lifecycle fan-out + reverse-order teardown
    const wires::Dict{Symbol, Dict{Symbol, String}}  # member => (port => resolved wire name), remaps
    lifecycle::Any                  # the LifecycleNode (managed) or nothing
    owned_ctx::Union{Context, Nothing}  # the Context `run` opened for this node, closed by
                                        # `close`; `nothing` when the caller supplied one
    @atomic open::Bool              # single-winner close latch (unmanaged path)
end

Base.show(io::IO, c::ComponentNode) =
    print(io, "ComponentNode(", c.node.fqn, c.lifecycle === nothing ? "" : ", managed",
          ", members=", c.order, ")")

"""
    parameters(node::ComponentNode) -> NamedTuple

The node-level, member-namespaced parameter aggregation:
`parameters(node).camera` is member `camera`'s live snapshot. Heterogeneous (one
entry per member, keyed by member name in declared order) — for iterating a whole
node, where `parameters(m)` is the type-stable per-mixin view. The flat
`<member>.<field>` wire namespace these views live under is owned by
[`CompositeParameterServer`](@ref).
"""
parameters(c::ComponentNode) = (; (nm => parameters(c.members[nm]) for nm in c.order)...)

"""
    entities(node::ComponentNode) -> NamedTuple

The node-level, member-namespaced handle aggregation:
`entities(node).camera.image` is member `camera`'s `image` port (so two members'
`image` handles don't collide). Member ports must be materialised first (after the
node is configured), like the per-mixin [`entities`](@ref).
"""
entities(c::ComponentNode) = (; (nm => entities(c.members[nm]) for nm in c.order)...)

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
struct NodeKind
    name::Symbol
    members::Vector{NodeMember}
end

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
runs when the defining module's body is evaluated — scripts, the REPL, and `include`d
modules; a precompiled package's kinds are absent from the registry at load
(registering those is a known follow-up). Evaluates to the `NodeKind`. Bring the node
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
        push!($(esc(:__node_kinds__)), ($(string(N)), $(esc(N))))
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
    for M in getfield(mod, :__mixin_bases__)
        try
            _ensure_schema!(M)                          # __ros_pschema_<M>__ + descriptors/pschema
            nt = _entities_accessor_from_specs!(M)      # __ros_entities_<M>__ + entities(m::M)
            # Bake handlers only once a typed accessor exists. A mixin with a non-derivable
            # port (action/client) keeps the generic `entities`, so its handlers warm at
            # first `run`; baking here would compile against the generic accessor and be
            # invalidated when materialise emits the typed one.
            nt === nothing || _anchor_reactions!(M)
        catch err
            @debug "_finalize_module!: skipped a mixin bake" mod mixin = M exception = err
        end
    end
    return nothing
end

"""
    @precompile_nodes

Bake this module's `@mixin`/`@node` declarations into its precompile image for a faster
first `run`. Place it as the LAST top-level statement in a module that declares mixins/nodes
(after every `@mixin`/`@param`/`@publishes`/… and reaction handler). It generates each
mixin's typed `parameters(m)`/`entities(m)` accessors and precompiles its reaction handlers
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
_sub_cb(reaction::F, m) where {F} = msg -> reaction(m, msg)     # subscription + service: react(m, x)
_timer_cb(reaction::F, m) where {F} = () -> reaction(m)         # @every timer: react(m)

# Create each declared port's runtime handle against the node-core. Returns the
# handle NamedTuple (`entities(m)`) and the paused timers to start at activate.
# `wiremap` is the member's resolved remap (port => wire name); a port not in it
# uses its authored `_wire(p)`.
function _materialize_ports!(node::Node, m, member::Symbol, specs::Vector{PortSpec}, pvalue,
                             wiremap::Dict{Symbol, String} = Dict{Symbol, String}())
    handles = Pair{Symbol, Any}[]
    timers = Any[]
    for p in specs
        if p.kind === :publisher
            push!(handles, p.name => Publisher(node, _wire(p, wiremap), p.msgtype))
        elseif p.kind === :subscription
            # `warmup = :off`: the endpoint's own warm would fire now, before the typed
            # `entities(m::M)`/`parameters(m::M)` accessors are generated (at the end of
            # `_member_materialize!`), compiling the handler against the generic accessors only
            # to invalidate it. The member warms all its reactions once afterward, in
            # `_warm_member_reactions!`.
            sub = Subscription(_sub_cb(p.reaction, m), node, _wire(p, wiremap), p.msgtype; warmup = :off)
            push!(handles, p.name => sub)
        elseif p.kind === :service
            srv = Service(_sub_cb(p.reaction, m), node, _wire(p, wiremap), p.msgtype; warmup = :off)
            push!(handles, p.name => srv)
        elseif p.kind === :action
            f = p.reaction
            support = ActionTypeSupport(typeof(f))
            body = _component_action_adapter(f, support, p.extra.fb_pos, m)
            push!(handles, p.name => _make_action_server(node, _wire(p, wiremap), typeof(f); body = body))
        elseif p.kind === :timer
            hz = p.extra.rate
            if hz isa Symbol
                hz in fieldnames(typeof(pvalue)) || error(
                    "@every: member `$(member)` timer `$(p.name)` rate `:$(hz)` does not name a declared parameter of $(_base(typeof(m)))")
                hz = getproperty(pvalue, hz)
                hz isa Real || error(
                    "@every: member `$(member)` timer `$(p.name)` rate parameter `:$(p.extra.rate)` must be numeric (got `$(typeof(hz))`)")
            end
            t = _paused_timer(node, Duration(round(Int64, 1.0e9 / hz)), _timer_cb(p.reaction, m))
            push!(handles, p.name => t)
            push!(timers, t)
        else
            @warn "component: port kind :$(p.kind) not yet materialised" port = p.name type = typeof(m)
        end
    end
    return (NamedTuple(handles), timers)
end

# A port's wire name: its explicit `on \"…\"` override, else the identifier.
_wire(p::PortSpec) = p.wire === nothing ? String(p.name) : p.wire

# A port's wire name under a member's resolved remap map: the remapped name if
# present, else the authored `_wire(p)`.
_wire(p::PortSpec, wiremap::Dict{Symbol, String}) = get(wiremap, p.name, _wire(p))

# ── wire-topic remap resolution + clobber detection ──────────────────────────────

# Resolve every member port's wire name: the remap target if any, else the authored
# `_wire(p)`. A cross-member remap (`port => (member, port)`) resolves to the referenced
# port's wire, by fixpoint (so a ref to a remapped port follows it). Returns
# `member => (port => wire-string)` for ALL ports, plus the set of explicitly-remapped
# `(member, port)`. Errors on a remap of an unknown port, a ref to an unknown
# member/port, or an unresolvable/cyclic ref chain.
function _resolve_wires(k::NodeKind)
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
function _check_clobbers(k::NodeKind, node::Node, wires, remapped)
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
function _members_plan(k::NodeKind)
    edges = _resolve_di(k.members)
    order = _toposort(k.members, edges)
    return (order, Dict{Symbol, Type}(m.name => m.mixin for m in k.members), edges)
end

# Filter run/add! overrides to this member: a mixin-local key (`fps`) applies
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

# Anchor a mixin's reaction handlers against type `M`, `precompile`-only and side-effect-free.
# Specs come from `mixin_spec` keyed at `M`'s base, so the same routine serves both the
# runtime warm (concrete `typeof(m)`, warming the actual instantiation incl. parametric) and
# the module-end bake (base mixin, from `_finalize_module!`). Must run after the typed
# accessors exist, so `parameters(m).x`/`entities(m).p` in a handler compile against the typed
# forms rather than the generic ones — otherwise the bake is wasted and then invalidated.
function _anchor_reactions!(::Type{M}) where {M}
    for p in mixin_spec(_base(M)).ports
        p.reaction === nothing && continue
        if p.kind === :subscription
            precompile(p.reaction, (M, p.msgtype))
            # The consumer's decode→dispatch chain. A component subscription builds with
            # `warmup = :off` (its own warm would fire before the typed accessors exist), so
            # anchor that chain here through the materialised callback type — the `_sub_cb`
            # closure the consumer actually invokes — in the default `Owned` view.
            rts = Base.return_types(_sub_cb, (typeof(p.reaction), M))
            if length(rts) == 1 && isconcretetype(only(rts))
                H = only(rts)
                T = p.msgtype
                precompile(_dispatch_decoded, (Entity, SampleHolder, Type{T}, H, Owned))
                precompile(decode_owned, (Memory{UInt8}, Type{T}))
                precompile(_decode_on_consumer, (Entity, SampleHolder, Type{T}))
                precompile(_invoke_owned, (Entity, T, H))
            end
        elseif p.kind === :service
            precompile(p.reaction, (M, p.msgtype))
        elseif p.kind === :timer
            precompile(p.reaction, (M,))
        elseif p.kind === :action
            # The action adapter calls the user handler via an `Any[]` splat, which inference
            # can't follow, so the server's adapter-frame warm never anchors the handler `f`.
            # Anchor it here with the exact signature the adapter calls:
            # `f(m, goalfields…, FeedbackSink{FB})`, sink inserted at `fb_pos`. Guarded —
            # a malformed reconstruction must never break configure/precompile.
            try
                f = p.reaction
                support = ActionTypeSupport(typeof(f))
                argtypes = Any[M]
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

# Runtime warm: anchor the member's handlers against its concrete `typeof(m)` once the typed
# accessors exist. Honours `node.warmup` via `_warmup!` (off / inline-sync / background).
# Subscriptions' own per-port warm is `:off`ed in `_materialize_ports!`; timers and services
# have no other warm. The module-end bake (`@precompile_nodes`) does the same for the base
# mixin ahead of time, so a baked node skips this work.
function _warm_member_reactions!(node::Node, m)
    _warmup!(node.warmup, () -> _anchor_reactions!(typeof(m)))
    return nothing
end

function _member_materialize!(cnode::ComponentNode, nm::Symbol)
    m = cnode.members[nm]
    rt = getfield(m, :__rt__)::MixinRuntime
    specs = mixin_spec(_base(typeof(m))).ports
    ports, timers = _materialize_ports!(cnode.node, m, nm, specs, current(rt.pserver),
                                        get(cnode.wires, nm, Dict{Symbol, String}()))
    rt.ports = ports
    rt.timers = timers
    # type-stable `entities(m)::PortsNT` — the base goes in as BOTH the generated
    # signature and the dedup key, so one method covers every instantiation (a concrete
    # signature here would silently strand other instantiations on the untyped generic).
    _entities_accessor_from_ports!(_base(typeof(m)), ports)
    # Now that the typed accessors exist, anchor each reaction handler.
    _warm_member_reactions!(cnode.node, m)
    return nothing
end

# configure: materialise the member's ports, then run its `configure` hook. Returns
# the hook's value (the `failure` token aborts + rolls back the fan-out below).
function _member_configure!(cnode::ComponentNode, nm::Symbol)
    _member_materialize!(cnode, nm)
    return configure(cnode.members[nm])
end

# activate: start the member's (paused) timers, then run its `activate` hook. Returns
# the hook's value (the `failure` token aborts + rolls back the fan-out below).
function _member_activate!(cnode::ComponentNode, nm::Symbol)
    rt = getfield(cnode.members[nm], :__rt__)::MixinRuntime
    for t in rt.timers
        _start!(t)
    end
    return activate(cnode.members[nm])
end

# Cancel-on-deactivate: cooperatively cancel every live goal on the member's action
# servers and bounded-wait for settle (budget = the Context drain timeout). Runs
# before the member `deactivate` hook so a goal body unwinds against state still
# valid (nav2 `terminate_all`). Guarded — a stuck server can't strand the fan-out.
# A no-op for a member with no materialised ports (never configured / already cleaned).
function _member_cancel_goals!(cnode::ComponentNode, nm::Symbol)
    rt = getfield(cnode.members[nm], :__rt__)::MixinRuntime
    rt.ports === nothing && return nothing
    budget = cnode.node.context.drain_timeout
    for h in values(rt.ports)
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
        deactivate(cnode.members[nm])
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

# on_error: run every member's hook, catching throws so one member cannot strand the
# rest's recovery. Any throw still means recovery failed: return the `failure` token,
# which `_handle_error!` honors by landing the node Finalized rather than silently
# recovering to Unconfigured.
function _members_on_error!(cnode::ComponentNode, order::Vector{Symbol})
    failed = false
    for nm in order
        try
            on_error(cnode.members[nm])
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
    for (i, nm) in enumerate(order)
        if _member_configure!(cnode, nm) === failure
            foreach(j -> _member_cleanup!(cnode, order[j]), i:-1:1)
            return failure
        end
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

function _assemble(ctx::Context, @nospecialize(K), name, namespace, overrides;
                   managed::Bool, autostart::Bool,
                   log_level::Union{LogLevel, Nothing} = nothing)
    order, bytype, edges = _members_plan(K)
    composed = K isa NodeKind            # the construction path drives prefixing, not member count
    declared = composed ? Symbol[m.name for m in K.members] : copy(order)  # listing/view order
    # wire-topic remaps: resolve every member port's wire name (mixin-as-node has none).
    wires, remapped = composed ? _resolve_wires(K) :
                      (Dict{Symbol, Dict{Symbol, String}}(), Set{Tuple{Symbol, Symbol}}())
    cnref = Ref{Any}(nothing)            # the managed callbacks reach the cnode through this

    if managed
        ln = LifecycleNode(ctx, name; namespace = namespace,
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
        node = Node(ctx, name; namespace = namespace)
    end
    # Before any member code (construct/configure/activate), so a requested level
    # governs the node's logging from the start.
    log_level === nothing || set_logger_level!(node, log_level)

    cnode = ComponentNode(node, Dict{Symbol, Any}(), order, wires, ln, nothing, true)
    cnref[] = cnode

    # A failure mid-assembly (clobber/construct/DI/param-wiring/configure) must not leave
    # a half-built node — and its already-declared entities — orphaned on the (possibly
    # shared) Context. Tear it down before rethrowing; `close` is a no-op on the parts
    # not yet built.
    try
        # assembly-time clobber check: two members' same-channel outputs landing on
        # one wire name (without an explicit remap) is a hard error you fix by remapping.
        composed && _check_clobbers(K, node, wires, remapped)

        # construct members in dependency order, injecting resolved siblings; attach
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

        # node-level `ros2 param` surface: a mixin promoted to a node wires its
        # single server un-prefixed; a composed `@node` (even with one member) wires a
        # member-prefixed `CompositeParameterServer` over all members' servers, in declared
        # order (the `ros2 param list` order). Either way each member's `parameters(m)`
        # reads its own server, mixin-local.
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
            foreach(nm -> _member_configure!(cnode, nm), order)
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
given), build the node-core, construct the mixin(s) in DI order, autostart
(configure → activate), and — when `block` (default) — `spin` until shutdown, closing
an owned Context on exit. With `block=false` the returned node holds any owned
Context, and `close` on the node closes it after member teardown; a caller-supplied
`ctx` is never closed. `overrides` is a NamedTuple of mixin-local parameter values.
`log_level` (a `LogLevel`, default `nothing` = the node default) sets the
node's logger level ([`set_logger_level!`](@ref)) before any member code runs, so
records from the construct/configure/activate hooks already honor it.

`managed=true` makes the node-core a [`LifecycleNode`](@ref): the lifecycle control
surface plus the managed-node dispatch gate ([`isactive`](@ref) — entities are silent
unless the node is [`Active`](@ref)). It then starts `Unconfigured` and is driven
externally (`ros2 lifecycle`) unless `autostart=true`. Unmanaged (default) autostarts
and has no control surface or gate.
"""
function Base.run(::Type{M}; name::AbstractString = _default_name(M),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  log_level::Union{LogLevel, Nothing} = nothing,
                  block::Bool = true) where {M <: Component}
    _check_runnable(M, "run")
    _ensure_schema!(M)
    return _run(ctx, M, name, namespace, overrides, peers, localhost_only, managed, autostart,
                log_level, block)
end

function Base.run(k::NodeKind; name::AbstractString = String(k.name),
                  namespace::Union{AbstractString, Nothing} = nothing,
                  overrides::NamedTuple = (;),
                  ctx::Union{Context, Nothing} = nothing,
                  peers::AbstractVector{<:AbstractString} = String[],
                  localhost_only::Bool = false,
                  managed::Bool = false, autostart::Bool = !managed,
                  log_level::Union{LogLevel, Nothing} = nothing,
                  block::Bool = true)
    for mem in k.members
        _check_runnable(mem.mixin, "@node member `$(mem.name)`")
        _ensure_schema!(mem.mixin)
    end
    return _run(ctx, k, name, namespace, overrides, peers, localhost_only, managed, autostart,
                log_level, block)
end

# Shared run body: open/own the Context, assemble (under `invokelatest` so freshly
# generated `P_M` methods are visible), then spin/close when blocking.
function _run(ctx, @nospecialize(K), name, namespace, overrides, peers, localhost_only,
              managed, autostart, log_level, block)
    owns = ctx === nothing
    ctx = owns ? Context(; peers = collect(String, peers), localhost_only = localhost_only) : ctx
    local cnode
    try
        cnode = Base.invokelatest(_assemble, ctx, K, name, namespace, overrides;
                                  managed = managed, autostart = autostart,
                                  log_level = log_level)
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
"The container's own management node (hosts the `~/_container/*` composition services)."
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

Returns the live [`Container`](@ref). With `block=true` (default) `spin` installs a
SIGINT handler for the duration — the first Ctrl-C drains gracefully, a second forces
exit — and the call returns once the Context has drained, closing it. SIGTERM is not
yet wired and terminates the process without draining. With `block=false` the call
returns the running `Container` immediately, leaving you to drive it and `close(c)`
it; the Context stays open until then. An exception during `f(c)` closes the Context
and propagates.

The container is itself a node named `name` (optionally under `namespace`) exposing
the `~/_container/{load_node,unload_node,list_nodes}` services, so a running
container accepts `ros2 component load/unload/list` and the `ComposableNodeContainer`
launch action.

`intra_process=true` (default) enables the intra-process short-circuit (same-Context
publisher↔subscriber pairs bypass serialize/Zenoh/decode) through `set_intra_process!` —
a process-global switch today.

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

`ctx`, `block`, `peers`, and `localhost_only` are rejected with an `ArgumentError`:
the container pins `ctx = c.ctx` and `block = false` (a foreign `ctx` would build a
node the container tracks but whose Context it never closes; `block=true` would park
`add!` on `spin`), and transport is fixed when `container` opens the Context, so
`run`'s `peers`/`localhost_only` would have no effect here.

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
