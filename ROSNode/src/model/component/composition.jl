# Dynamic composition — `ros2 component` parity (DESIGN-COMPONENTS.md §7). Two halves:
#
#   • a process-global **node-kind registry by name** (the `rclcpp_components_register_nodes`
#     analog): `@node`/`@mixin` register their kind, so a name can be instantiated later;
#   • a container's **`~/_container/{load_node,unload_node,list_nodes}` services** over the
#     vendored `composition_interfaces` types, so `ros2 component load/unload/list` and the
#     `ComposableNodeContainer` / `LoadComposableNodes` launch actions drive a running
#     container. A runtime-loaded node is just one more node-core on the Context (§7) — the
#     same `run`/assembly path as a launch-time `add!`, tracked under a container-unique id.

export register_node_kind!, node_kind, node_kinds, load_node, unload_node, list_nodes

# ── the node-kind registry (rclcpp_components_register_nodes analog, §7) ─────────
# Process-global name → kind (a `NodeKind` from `@node`, or a `@mixin` type used as a
# node). `@node`/`@mixin` emit the registration as a top-level statement, so it runs
# when the defining module's body is evaluated — fine for scripts / REPL / `Main` and
# `include`d modules. Like the component layer's `_register_mixin!`, it is NOT deferred
# to `__init__`, so a *precompiled* package's kinds are not in this table at load (the
# top-level mutation of ROSNode's global doesn't persist across precompile); registering
# such a package's kinds is a follow-up if/when components ship as precompiled libraries.

const _NODE_KINDS = Dict{String, Any}()
const _NODE_KINDS_LOCK = ReentrantLock()

"""
    register_node_kind!(name, K) -> K

Register node kind `K` (a [`NodeKind`](@ref) or a `@mixin` type) under `name` in the
process-global registry (§7) — the `rclcpp_components_register_nodes` analog that
`@node`/`@mixin` emit, so a container's `load_node` can instantiate it by name.
"""
function register_node_kind!(name::AbstractString, @nospecialize(K))
    @lock _NODE_KINDS_LOCK (_NODE_KINDS[String(name)] = K)
    return K
end

"The registered kind for `name` (a `NodeKind` or `@mixin` type), or `nothing` (§7)."
node_kind(name::AbstractString) = @lock _NODE_KINDS_LOCK get(_NODE_KINDS, String(name), nothing)

"The names of every registered node kind, sorted (§7)."
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

# ── container loaded-set bookkeeping (§7) ────────────────────────────────────────

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

# ── programmatic composition API (the verbs the services call, §7) ───────────────

"""
    load_node(c::Container, package_name, plugin_name;
              name="", namespace="", parameters=(;)) -> (ComponentNode, unique_id)

Instantiate the registered kind named `plugin_name` on container `c` (§7) — the
programmatic form of `ros2 component load`. `name`/`namespace` empty fall back to the
kind's defaults; `parameters` is a `run` `overrides` NamedTuple (mixin-local or
member-prefixed keys). Throws `ArgumentError` if no such kind is registered.
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
    unload_node(c::Container, unique_id) -> Bool

Close and forget the loaded node with `unique_id` (§7) — `ros2 component unload`.
Returns `false` if no node carries that id.
"""
function unload_node(c::Container, unique_id::Integer)
    uid = UInt64(unique_id)
    # Pop-and-own under a single critical section, so two concurrent unloads of the
    # same id can't both claim it (and double-close). The `close` runs outside the lock
    # (it tears down entities + member cleanup — not something to hold the lock across).
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

The loaded nodes as `(unique_id, full_node_name)` pairs, ascending by id (§7) —
`ros2 component list`.
"""
function list_nodes(c::Container)
    @lock c.lock begin
        ids = sort!(collect(keys(c.loaded)))
        return Tuple{UInt64, String}[(uid, String(c.loaded[uid].node.fqn)) for uid in ids]
    end
end

# ── the wire control surface: ~/_container/{load,unload,list}_node (§7) ──────────

const _COMP_SRV = Interfaces.composition_interfaces.srv

# A LoadNode `log_level` (an `rcl_interfaces/msg/Log` severity byte: 10/20/30/40/50,
# 0 = unset) → a Julia `LogLevel` for `set_logger_level!`, or `nothing` to leave the
# node's default.
_loglevel_from_ros(b::Integer) =
    b == 0    ? nothing :
    b >= 0x32 ? Fatal :
    b >= 0x28 ? Logging.Error :
    b >= 0x1e ? Logging.Warn :
    b >= 0x14 ? Logging.Info : Logging.Debug

"""
    _wire_container_services!(c::Container) -> c

Declare the three `composition_interfaces` services under the container node's
`~/_container/` namespace (§7), so `ros2 component load/unload/list` (and the
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
        try
            nm = isempty(req.node_name) ? _kind_default_name(K) : req.node_name
            ns = isempty(req.node_namespace) ? nothing : req.node_namespace
            ov = _overrides_from_wire(req.parameters)
            cn = run(K; ctx = c.ctx, block = false, name = nm, namespace = ns, overrides = ov)
            uid = _track_loaded!(c, cn)
            lvl = _loglevel_from_ros(req.log_level)
            lvl === nothing || set_logger_level!(cn.node, lvl)
            isempty(req.remap_rules) ||
                @warn "load_node: remap_rules not yet applied (§4.4 remap is a follow-up)" rules = req.remap_rules node = cn.node.fqn
            isempty(req.extra_arguments) ||
                @debug "load_node: extra_arguments ignored" node = cn.node.fqn
            return _COMP_SRV.LoadNode_Response(; success = true, error_message = "",
                full_node_name = String(cn.node.fqn), unique_id = uid)
        catch err
            err isa ShutdownException && rethrow()
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
