# Tier-2 scaffolding precompile anchors (DESIGN: G4). The non-endpoint, type-AGNOSTIC
# component control flow — assembly planning, the lifecycle fan-out, the run/teardown
# entry, the node-kind registry, DI, and the load hook — is parameterised on
# `Entity`/`NodeKind`/`ComponentNode`/`Symbol`/`Dict`, NOT on any user mixin type, so
# baking each specialisation once helps *every* node. These are bare `precompile` anchors
# (compile without running). `_component_precompile_specs` is the single source for both the
# `@compile_workload` (which bakes them into ROSNode's pkgimage) and the drift-guard test
# (which asserts each still resolves to a real method).
#
# Type-*specific* per-`(M, T)` handler/dispatch/codec leaves do NOT belong here — they are
# per-user-type and warmed at node bring-up (`_warm_member_reactions!`) or baked into the
# user's own package.

# (callable, argtypes) pairs. The `Core.kwcall` entries target the *keyword* method of
# `_assemble`/`load_node` — the real entry point (reached via `invokelatest` / the container
# service) — not the positional method, which would read green while the kw path stays unbaked.
function _component_precompile_specs()
    return Tuple{Any, Tuple}[
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
        # run entry (both namespace variants × both log-level variants — the `ros2 component
        # load --log-level` path passes a `LogLevel`, the bare run/load a `Nothing`) + teardown
        # + node-level aggregate views
        (_run, (Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Bool)),
        (_run, (Context, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, Nothing,  Bool)),
        (_run, (Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Bool)),
        (_run, (Context, NodeKind, String, String,  NamedTuple{(), Tuple{}}, Vector{String}, Bool, Bool, Bool, LogLevel, Bool)),
        (Base.close, (ComponentNode,)),
        (parameters, (ComponentNode,)),
        (entities,   (ComponentNode,)),
        # node-kind registry
        (node_kind,           (String,)),
        (node_kinds,          ()),
        (register_node_kind!, (String, NodeKind)),
        # DI zero-dep fallback + the module load hook
        (_zero_dep_construct, (DataType,)),
        (_zero_dep_construct, (UnionAll,)),
        (ros_init!,           (Module,)),
        # kwarg entry points via `Core.kwcall` (the real entry, not the positional method).
        # `_assemble` in both log-level variants; `load_node` with its actual keywords
        # (`name`/`namespace`/`parameters`) — its `#load_node#` body delegates to `run`.
        (Core.kwcall, (NamedTuple{(:managed, :autostart, :log_level), Tuple{Bool, Bool, Nothing}},
                       typeof(_assemble), Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}})),
        (Core.kwcall, (NamedTuple{(:managed, :autostart, :log_level), Tuple{Bool, Bool, LogLevel}},
                       typeof(_assemble), Context, NodeKind, String, Nothing, NamedTuple{(), Tuple{}})),
        (Core.kwcall, (NamedTuple{(:name, :namespace, :parameters), Tuple{String, String, NamedTuple{(), Tuple{}}}},
                       typeof(load_node), Container, String, String)),
    ]
end

@compile_workload begin
    for (f, ts) in _component_precompile_specs()
        precompile(f, ts)
    end
end
