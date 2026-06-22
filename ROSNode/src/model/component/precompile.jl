# Type-agnostic scaffolding precompile anchors. The non-component-specific control flow — the
# per-member lifecycle teardown fan-out, the teardown entry, the node-kind registry, entity
# creation, and the load hook — is parameterised on `Entity`/`ComponentNode`/`NodeSchema`/`Symbol`/
# `Dict`, not on any user component type, so baking each specialisation once helps every node.
# These are bare `precompile` anchors (compile without running). `_component_precompile_specs`
# is the single source for both the `@compile_workload` (which bakes them into ROSNode's
# pkgimage) and the drift-guard test (which asserts each still resolves to a real method).
#
# Component-specific per-`(M, T)` handler/dispatch/codec leaves belong to the consumer: a node's
# full first-`run` path is baked by `precompile_node(schema)` (the functor bake below, and the
# consumer's own `@compile_workload precompile_node(...)`).

# (callable, argtypes) pairs. The `Core.kwcall` entry targets the *keyword* method of `load_node`
# — the real entry point (reached via the container service) — not the positional method, which
# would read green while the kw path stays unbaked.
function _component_precompile_specs()
    specs = Tuple{Any, Tuple}[
        # lifecycle fan-out (stops at the `Any` member dispatch — the user-code boundary)
        (_member_cleanup!,   (ComponentNode, Symbol)),
        (_members_on_error!, (ComponentNode, Vector{Symbol})),
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
        (register_node_kind!, (String, NodeSchema)),
        # the module load hook
        (ros_init!,           (Module,)),
        # `load_node`'s kwarg entry (the real entry via the container service); its `#load_node#`
        # body delegates to `run` (functor `run(::Type{M})` / `run(::NodeSchema)`).
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

# ── functor-node bake ───────────────────────────────────────────────────────────
# The functor `run(::NodeSchema)` specializes per concrete schema type (no `@nospecialize` collapse),
# but every GENERIC callee it pulls in — `node`/DI/wire resolution, the `@generated` carrier builders,
# `construct_port` per port kind, the lifecycle fan-out, a-priori priming, and (via `precompile_schema`)
# the spawned-task codec anchors — is shared with every consumer schema. We bake them once here through
# a representative two-member node (pub + timer; sub + service) over vendored message/service types.
# The per-consumer schema's own `run`/builder specialization is the consumer's `@compile_workload` job.
const _PcTime   = Interfaces.builtin_interfaces.msg.Time
const _PcGPReq  = Interfaces.rcl_interfaces.srv.GetParameters_Request
const _PcGPResp = Interfaces.rcl_interfaces.srv.GetParameters_Response

mutable struct _PcSource{Name} <: Component{Name} end
_pc_tick(node, m::_PcSource) = nothing
member_schema(::Type{_PcSource}) = component(_PcSource, publishes(:p, _PcTime), every(:t, 10, _pc_tick))

mutable struct _PcSink{Name} <: Component{Name} end
_pc_hear(node, m::_PcSink, msg) = nothing
_pc_serve(node, m::_PcSink, req) = _default_msg(_PcGPResp)
member_schema(::Type{_PcSink}) = component(_PcSink, hears(:p, _PcTime, _pc_hear), serves(:s, _PcGPReq, _pc_serve))

@compile_workload begin
    for (f, ts) in _component_precompile_specs()
        precompile(f, ts)
    end
    # Build a representative schema (compiles node()/component/_to_port/DI/wire for the String=>Type
    # member form), then bake its full first-`run` path (run specialization + lifecycle fan-out on the
    # concrete ComponentNode + spawned consume/serve/dispatch bodies) — the same call a consumer makes.
    precompile_node(node("src" => _PcSource, "snk" => _PcSink; register = false))
    # Bring-up's first `maxlog=`-bearing log (the no-`home` hint) drives the *global*
    # `ConsoleLogger`'s `message_limits::Dict{Any,Int}` accounting — a Base path we can't
    # narrow but can bake by exercising it once here (captures the real MIs, incl. the
    # closure `get!` form, more robustly than naming Base internals). Routed to `devnull`.
    Base.CoreLogging.with_logger(Base.CoreLogging.ConsoleLogger(devnull)) do
        @info "ROSNode precompile: warm maxlog path" maxlog = 1
    end
end
