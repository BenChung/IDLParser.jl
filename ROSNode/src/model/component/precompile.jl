# Type-agnostic scaffolding precompile anchors. The non-endpoint component control flow —
# assembly planning, the lifecycle fan-out, the run/teardown entry, the node-kind registry,
# DI, and the load hook — is parameterised on `Entity`/`NodeKind`/`ComponentNode`/`Symbol`/
# `Dict`, not on any user mixin type, so baking each specialisation once helps every node.
# These are bare `precompile` anchors (compile without running). `_component_precompile_specs`
# is the single source for both the `@compile_workload` (which bakes them into ROSNode's
# pkgimage) and the drift-guard test (which asserts each still resolves to a real method).
#
# Type-specific per-`(M, T)` handler/dispatch/codec leaves belong to the consumer: they are
# warmed at node bring-up (`_warm_member_reactions!`) or baked into the user's own package by
# `@precompile_nodes`.

# (callable, argtypes) pairs. The `Core.kwcall` entries target the *keyword* method of
# `_assemble`/`load_node` — the real entry point (reached via `invokelatest` / the container
# service) — not the positional method, which would read green while the kw path stays unbaked.
function _component_precompile_specs()
    specs = Tuple{Any, Tuple}[
        # assembly planning (the composed-`NodeKind` path)
        (_members_plan,   (NodeKind,)),
        (_resolve_di,     (Vector{NodeMember},)),
        (_toposort,       (Vector{NodeMember}, Dict{Symbol, Vector{Symbol}})),
        (_resolve_wires,  (NodeKind,)),
        (_check_clobbers, (NodeKind, Node, Dict{Symbol, Dict{Symbol, String}}, Set{Tuple{Symbol, Symbol}})),
        # lifecycle fan-out (stops at the `Any` member dispatch — the user-code boundary)
        (_member_materialize!, (ComponentNode, Symbol)),
        (_member_configure!,   (ComponentNode, Symbol)),
        (_member_activate!,    (ComponentNode, Symbol)),
        (_member_cleanup!,     (ComponentNode, Symbol)),
        (_members_on_error!,   (ComponentNode, Vector{Symbol})),
        # run entry, over arg1 = the Context source × namespace variant × log-level variant.
        # arg1 `Nothing` is the common path (`run(K; …)` opens+owns its own Context); `Context` is
        # the caller-supplied one (`run(K; ctx=…)` / `load_node`). `K` is `@nospecialize`d in `_run`,
        # so each anchor bakes the node-kind-agnostic MI. (`LogLevel` arg10 = `ros2 component load
        # --log-level`; bare run = `Nothing`.) positional tail: …, log_level, warmup, warmup_sync, block
        (_run, (Nothing, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Symbol, Bool, Bool)),
        (_run, (Nothing, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Symbol, Bool, Bool)),
        (_run, (Nothing, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Symbol, Bool, Bool)),
        (_run, (Nothing, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Symbol, Bool, Bool)),
        (_run, (Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Symbol, Bool, Bool)),
        (_run, (Context, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Symbol, Bool, Bool)),
        (_run, (Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Symbol, Bool, Bool)),
        (_run, (Context, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Symbol, Bool, Bool)),
        # the public `run(k::NodeKind; …)` entry. `k` is `@nospecialize`d, so its kwarg body is ONE
        # node-kind-agnostic MI — bake it here (abstract `NodeKind`, not per-`@node`, so we don't
        # re-overspecialize the very entry we just collapsed). The per-kwarg-combination `Core.kwcall`
        # sorter still JITs, but it is the cheap arg-reorder shell; the body + `_run`/`_assemble` are baked.
        (Base.run, (NodeKind,)),
        (Base.close, (ComponentNode,)),
        (parameters, (ComponentNode,)),
        (entities,   (ComponentNode,)),
        # Entity creation is msgtype-INDEPENDENT (it takes a `TypeInfo`, not the message type), so its
        # three concrete-kind forms bake ONCE here for every node. `make_entity(node, ::EndpointDesc)`
        # dispatches dynamically on the descriptor's abstract `kind` field, so the per-kind kwcall
        # target is not a static callee of the patterns' `_make_*` (anchored per-mixin) nor of the
        # a-priori graph enumeration — anchor each kind plus the descriptor entry.
        (make_entity, (Node, EndpointDesc)),
        (Core.kwcall, (NamedTuple{(:qos,), Tuple{QosProfile}}, typeof(make_entity), Node, PublisherKind,    String, TypeInfo)),
        (Core.kwcall, (NamedTuple{(:qos,), Tuple{QosProfile}}, typeof(make_entity), Node, SubscriptionKind, String, TypeInfo)),
        (Core.kwcall, (NamedTuple{(:qos,), Tuple{QosProfile}}, typeof(make_entity), Node, ServiceKind,      String, TypeInfo)),
        # the default `~/get_type_description` service descriptor (fixed request type), enumerated for
        # every node's a-priori local graph (`_base_node_descs`).
        (_service_desc, (Node, String, Type{GetTypeDescription_Request})),
        # node-assembly field writes on fixed (msgtype-independent) types: the node's parameter facade
        # and an endpoint's consumer task, both built dynamically during `_assemble`/materialise.
        (setproperty!, (Node, Symbol, CompositeParameterServer)),
        (setproperty!, (Entity, Symbol, Task)),
        # node-kind registry
        (node_kind,           (String,)),
        (node_kinds,          ()),
        (register_node_kind!, (String, NodeKind)),
        # the module load hook (the name-threaded `construct` default is parametric on the member
        # name, so it is left to compile on first use rather than anchored generically here)
        (ros_init!,           (Module,)),
        # kwarg entry points via `Core.kwcall` (the real entry, not the positional method).
        # `_assemble` in both log-level variants; `load_node` with its actual keywords
        # (`name`/`namespace`/`parameters`) — its `#load_node#` body delegates to `run`.
        (Core.kwcall, (NamedTuple{(:managed, :autostart, :log_level, :warmup, :warmup_sync), Tuple{Bool, Bool, Nothing, Symbol, Bool}},
                       typeof(_assemble), Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}})),
        (Core.kwcall, (NamedTuple{(:managed, :autostart, :log_level, :warmup, :warmup_sync), Tuple{Bool, Bool, LogLevel, Symbol, Bool}},
                       typeof(_assemble), Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}})),
        (Core.kwcall, (NamedTuple{(:name, :namespace, :parameters), Tuple{String, String, NamedTuple{(), Tuple{}}}},
                       typeof(load_node), Container, String, String)),
    ]
    # Every `@node` wires a `CompositeParameterServer` (a fixed, non-parametric type), so its six
    # parameter services' construction + handler bodies + the `/parameter_events` publisher bake
    # ONCE here for every composed node — the per-node param wiring was the largest launch phase the
    # per-mixin bake couldn't touch. The named `_ParamSvcHandler` (not a `do`-block) is what makes
    # them precompilable by name.
    append!(specs, _parameter_service_specs(CompositeParameterServer))
    # The timer tick gate/dispatch wrapper (fixed type), so the first `@every` fire doesn't JIT it.
    push!(specs, (_fire!, (Timer{ROS},)))
    return specs
end

@compile_workload begin
    for (f, ts) in _component_precompile_specs()
        precompile(f, ts)
    end
    # Bring-up's first `maxlog=`-bearing log (the no-`home` hint) drives the *global*
    # `ConsoleLogger`'s `message_limits::Dict{Any,Int}` accounting — a Base path we can't
    # narrow but can bake by exercising it once here (captures the real MIs, incl. the
    # closure `get!` form, more robustly than naming Base internals). Routed to `devnull`.
    Base.CoreLogging.with_logger(Base.CoreLogging.ConsoleLogger(devnull)) do
        @info "ROSNode precompile: warm maxlog path" maxlog = 1
    end
end
