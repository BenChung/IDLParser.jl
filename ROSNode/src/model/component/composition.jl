# Dynamic composition — `ros2 component` parity. Two halves:
#
#   • a process-global **node-kind registry by name** (the `rclcpp_components_register_nodes`
#     analog): `@node`/`@mixin` register their kind, so a name can be instantiated later;
#   • a container's **`~/_container/{load_node,unload_node,list_nodes}` services** over the
#     vendored `composition_interfaces` types, so `ros2 component load/unload/list` and the
#     `ComposableNodeContainer` / `LoadComposableNodes` launch actions drive a running
#     container. A runtime-loaded node is just one more node-core on the Context — the
#     same `run`/assembly path as a launch-time `add!`, tracked under a container-unique id.

export register_node_kind!, node_kind, node_kinds, load_node, unload_node, list_nodes

# ── the node-kind registry (rclcpp_components_register_nodes analog) ─────────────
# Process-global name → kind. Inherently a runtime registry (string → kind can't
# dispatch), so unlike the per-mixin spec store it can't move into the defining module.
# `@mixin`/`@node` therefore register a kind via the dual path — immediately in the
# REPL/script case, and through `ros_init!` (the load hook) for a precompiled package,
# where a top-level mutation of this dict would not survive precompile.

const _NODE_KINDS = Dict{String, Any}()
const _NODE_KINDS_LOCK = ReentrantLock()

"""
    register_node_kind!(name::AbstractString, K) -> K

Register node kind `K` under `name` in the process-global kind registry, returning `K`.
This is the `rclcpp_components_register_nodes` analog: it makes a kind
instantiable by name, so a container's [`load_node`](@ref) (and the
`~/_container/load_node` service behind `ros2 component load`) can build it from a load
request.

`K` is a `NodeKind` (from `@node N = […]`) or a `@mixin` type used directly as
a node. `@node`/`@mixin` register a kind via the dual path that survives precompilation:
immediately at module-body evaluation in the REPL/script/`Main` case, and through the
module's load hook ([`ros_init!`](@ref)) for a precompiled package — where a top-level
mutation of this registry would be discarded with ROSNode's deserialized state before
the package cache is written. The call is thread-safe (guarded by the registry lock),
and a repeated `name` is last-writer-wins.
"""
function register_node_kind!(name::AbstractString, @nospecialize(K))
    @lock _NODE_KINDS_LOCK (_NODE_KINDS[String(name)] = K)
    return K
end

"""
    node_kind(name::AbstractString) -> Union{NodeKind, Type, Nothing}

The node kind registered under `name` — the `NodeKind` or `@mixin` type passed
to [`register_node_kind!`](@ref) — or `nothing` when no kind carries that name.
Thread-safe (guarded by the registry lock).

The lookup is by the exact registered name; a container's load path additionally
tolerates a namespaced spelling (`my_pkg::Talker`, `pkg/Talker`, `pkg.Talker`) by
retrying on the last `::`/`/`/`.`-separated segment.
"""
node_kind(name::AbstractString) = @lock _NODE_KINDS_LOCK get(_NODE_KINDS, String(name), nothing)

"""
    node_kinds() -> Vector{String}

The names of every registered node kind, sorted ascending. Each name resolves
through [`node_kind`](@ref) and is loadable by a container's [`load_node`](@ref) / the
`~/_container/load_node` service. Thread-safe (guarded by the registry lock).
"""
node_kinds() = @lock _NODE_KINDS_LOCK sort!(collect(keys(_NODE_KINDS)))

# Resolve a `(package_name, plugin_name)` load request to a registered kind. The
# registry is keyed by bare name; we try `plugin_name` verbatim, then tolerate a
# namespaced/`::`-qualified spelling (`my_pkg::Talker`, `pkg/Talker`) by its last
# segment. `package_name` is advisory (we don't shard the registry by package).
function resolve_node_kind(package_name::AbstractString, plugin_name::AbstractString)
    k = node_kind(plugin_name)
    k === nothing || return k
    for sep in ("::", "/", ".")
        r = findlast(sep, plugin_name)
        if r !== nothing
            seg = plugin_name[nextind(plugin_name, last(r)):end]
            isempty(seg) || (k = node_kind(seg))
            k === nothing || return k
        end
    end
    return nothing
end

# The default node name for a kind when a load request leaves `node_name` empty.
_kind_default_name(k::NodeKind) = String(k.name)
_kind_default_name(::Type{M}) where {M} = _default_name(M)

# ── container loaded-set bookkeeping ─────────────────────────────────────────────

# Assign the next container-unique id (1-based, like rclcpp) to a freshly loaded node
# and track it for `list_nodes`/`unload_node`. Returns the id.
function _track_loaded!(c::Container, cn::ComponentNode)
    @lock c.lock begin
        uid = (c._next_uid += UInt64(1))
        c.loaded[uid] = cn
        push!(c.nodes, cn)
        return uid
    end
end

# Wire `Parameter[]` from a load request into a `run` `overrides` NamedTuple. Names
# may be mixin-local (`fps`) or member-prefixed (`camera.fps`) — `_member_overrides`
# (run.jl) accepts both.
function _overrides_from_wire(params)
    isempty(params) && return (;)
    return (; (Symbol(p.name) => _from_param_value(p.value) for p in params)...)
end

# ── programmatic composition API (the verbs the services call) ───────────────────

"""
    load_node(c::Container, package_name, plugin_name;
              name="", namespace="", parameters=(;)) -> (ComponentNode, unique_id::UInt64)

Instantiate the registered kind named `plugin_name` on container `c`, returning the
live `ComponentNode` and the container-unique id it is tracked under.
This is the programmatic form of `ros2 component load`; the `~/_container/load_node`
service (`composition_interfaces/srv/LoadNode`) drives the same path from the wire.

Resolution tries `plugin_name` verbatim against the process-global registry, then its
last `::`/`/`/`.`-separated segment; `package_name` is advisory and does not shard the
registry. `name` defaults to the kind's name (the `@node` name, or the mixin type's
snake-cased name) and `namespace` to the Context's namespace; pass either to override.

`parameters` is a `run` `overrides` NamedTuple. A key is mixin-local (`fps`, applying
to every member that declares it) or member-prefixed (`var"camera.fps"`, targeting one
member and winning over the mixin-local form). The prefix is the member's name within
the node — a `@node` member's declared name, or the snake-cased mixin type for a mixin
loaded directly as a node — independent of the node instance `name`.

The node is built by `run(K; ctx = c.ctx, block = false, …)` on the container's shared
Context and autostarts (configure, then activate) like a standalone unmanaged node,
then is registered for [`list_nodes`](@ref) / [`unload_node`](@ref) under a fresh
1-based id. Throws `ArgumentError` when no kind is registered under `plugin_name`;
assembly errors (a wire-name clobber, unsatisfied or ambiguous DI, a `construct` or
`configure` exception) propagate from the underlying `run`. The wire
`~/_container/load_node` service catches the same errors and returns them as a
`success = false` response instead.

```julia
container("vision"; block = false) do c
    cn, uid = load_node(c, "my_pkg", "Camera"; name = "front",
                        parameters = (; var"camera.fps" = 30))
end
```
"""
function load_node(c::Container, package_name::AbstractString, plugin_name::AbstractString;
                   name::AbstractString = "", namespace::AbstractString = "",
                   parameters::NamedTuple = (;))
    K = resolve_node_kind(package_name, plugin_name)
    K === nothing &&
        throw(ArgumentError("load_node: no component `$(plugin_name)` registered" *
                            (isempty(package_name) ? "" : " (package `$(package_name)`)")))
    nm = isempty(name) ? _kind_default_name(K) : name
    ns = isempty(namespace) ? nothing : namespace
    cn = run(K; ctx = c.ctx, block = false, name = nm, namespace = ns, overrides = parameters)
    return (cn, _track_loaded!(c, cn))
end

"""
    unload_node(c::Container, unique_id::Integer) -> Bool

Close and forget the loaded node tracked under `unique_id` on container `c`, returning
`true` when a node carried that id and `false` otherwise. This is the programmatic
form of `ros2 component unload`; the `~/_container/unload_node` service
(`composition_interfaces/srv/UnloadNode`) drives the same path.

The id is popped from the container's loaded set and node list inside one critical
section, so two concurrent unloads of the same id cannot both claim it (and
double-close). The `close` — which runs each member's `cleanup` in reverse dependency
order and tears down the node's entities, driving a managed node through its
`LifecycleNode` shutdown — happens outside the lock. `close` is idempotent, so a later
Context drain that also closes the node is a no-op.
"""
function unload_node(c::Container, unique_id::Integer)
    uid = UInt64(unique_id)
    # Pop-and-own under the lock (concurrent unloads can't double-close); `close` —
    # entity teardown + member cleanup — is too heavy to run holding it.
    cn = @lock c.lock begin
        node = get(c.loaded, uid, nothing)
        node === nothing && return false
        delete!(c.loaded, uid)
        filter!(n -> n !== node, c.nodes)
        node
    end
    close(cn)
    return true
end

"""
    list_nodes(c::Container) -> Vector{Tuple{UInt64, String}}

The nodes loaded on container `c` as `(unique_id, full_node_name)` pairs, ascending by
id. This is the programmatic form of `ros2 component list`; the
`~/_container/list_nodes` service (`composition_interfaces/srv/ListNodes`) returns the
same pairs split into the response's parallel `unique_ids` / `full_node_names` arrays.
`full_node_name` is the node's fully-qualified name (`namespace/name`). Thread-safe
(snapshot taken under the container lock).
"""
function list_nodes(c::Container)
    @lock c.lock begin
        ids = sort!(collect(keys(c.loaded)))
        return Tuple{UInt64, String}[(uid, String(c.loaded[uid].node.fqn)) for uid in ids]
    end
end

# ── the wire control surface: ~/_container/{load,unload,list}_node ───────────────

const _COMP_SRV = Interfaces.composition_interfaces.srv

# A LoadNode `log_level` (an `rcl_interfaces/msg/Log` severity byte: 10/20/30/40/50,
# 0 = unset) → a Julia `LogLevel` for `set_logger_level!`, or `nothing` to leave the
# node's default.
_loglevel_from_ros(b::Integer) =
    b == 0    ? nothing :
    b == 0x32 ? Fatal :
    b == 0x28 ? Logging.Error :
    b == 0x1e ? Logging.Warn :
    b == 0x14 ? Logging.Info :
    b == 0x0a ? Logging.Debug :
    (@warn "load_node: invalid log_level; left unset" log_level = Int(b); nothing)

"""
    _wire_container_services!(c::Container) -> c

Declare the three `composition_interfaces` services under the container node's
`~/_container/` namespace, so `ros2 component load/unload/list` (and the
`ComposableNodeContainer` launch action) drive the container. The handles are tracked
on the container (closed with its Context).
"""
function _wire_container_services!(c::Container)
    node = c.node

    push!(c.services, Service(node, "~/_container/load_node", _COMP_SRV.LoadNode_Request) do req
        K = resolve_node_kind(req.package_name, req.plugin_name)
        if K === nothing
            return _COMP_SRV.LoadNode_Response(; success = false,
                error_message = "unknown component '$(req.plugin_name)'" *
                    (isempty(req.package_name) ? "" : " (package '$(req.package_name)')"),
                full_node_name = "", unique_id = UInt64(0))
        end
        # Rejected up front: a success=true response with unapplied remaps would tell
        # the caller (ros2 component load --remap-rule, launch remappings) the remap
        # took effect.
        if !isempty(req.remap_rules)
            return _COMP_SRV.LoadNode_Response(; success = false,
                error_message = "remap_rules not supported (remap wiring is a follow-up); " *
                    "load without remaps or remap at the @node member (`Mixin{port => target}`)",
                full_node_name = "", unique_id = UInt64(0))
        end
        local cn = nothing
        try
            nm = isempty(req.node_name) ? _kind_default_name(K) : req.node_name
            ns = isempty(req.node_namespace) ? nothing : req.node_namespace
            ov = _overrides_from_wire(req.parameters)
            # Through `run`, not post-hoc: the level must be set before autostart so
            # the configure/activate records already honor it.
            cn = run(K; ctx = c.ctx, block = false, name = nm, namespace = ns, overrides = ov,
                     log_level = _loglevel_from_ros(req.log_level))
            isempty(req.extra_arguments) ||
                @debug "load_node: extra_arguments ignored" node = cn.node.fqn
            # Tracking last, so a failure response never leaves a tracked node.
            uid = _track_loaded!(c, cn)
            return _COMP_SRV.LoadNode_Response(; success = true, error_message = "",
                full_node_name = String(cn.node.fqn), unique_id = uid)
        catch err
            err isa ShutdownException && rethrow()
            # The client never learns an id on failure, so a surviving node would be
            # unreachable from the wire — close anything already built.
            cn === nothing || close(cn)
            return _COMP_SRV.LoadNode_Response(; success = false,
                error_message = sprint(showerror, err), full_node_name = "", unique_id = UInt64(0))
        end
    end)

    push!(c.services, Service(node, "~/_container/unload_node", _COMP_SRV.UnloadNode_Request) do req
        ok = unload_node(c, req.unique_id)
        return _COMP_SRV.UnloadNode_Response(; success = ok,
            error_message = ok ? "" : "no loaded node with unique_id $(req.unique_id)")
    end)

    push!(c.services, Service(node, "~/_container/list_nodes", _COMP_SRV.ListNodes_Request) do req
        entries = list_nodes(c)
        return _COMP_SRV.ListNodes_Response(;
            full_node_names = String[fqn for (_uid, fqn) in entries],
            unique_ids = UInt64[uid for (uid, _fqn) in entries])
    end)

    return c
end
