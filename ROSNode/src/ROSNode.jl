"""
    ROSNode

A Julian ROS 2 client built on Zenoh, implementing the node-centric concepts of
the ROS 2 graph: https://docs.ros.org/en/rolling/Concepts/Basic.html

The module body includes the component files in bottom-up dependency order.
"""
module ROSNode

# ── component includes (bottom-up) ───────────────────────────────────────
# Order matters: primitives → data plane → object model → patterns → graph/lifecycle.

include("interfaces/interfaces.jl")     # @ros_msgs over the vendored tree → Interfaces.<pkg>… (first: every file references it)
include("base/core.jl")                 # shared seams: re-exports, exceptions, status tokens
include("base/time.jl")                 # clocks, RTime/Duration, Timer/Rate
include("base/settlement.jl")           # write-once ResultCell + fail-safe settle
include("base/serialization.jl")        # message ↔ ZBytes bridge + attachment
include("model/context.jl")             # Context: session, registry, graph, shutdown
include("model/node/node.jl")           # Node type + concurrency policy
include("model/node/entity.jl")         # generic Entity, data routes, declare pub/sub (+ transient-local re-latch)
include("model/node/dispatch.jl")       # static subscription dispatch runtime
include("model/node/dynamic.jl")        # dynamic keyexpr dispatch + manifest warm
include("model/node/teardown.jl")       # entity/node teardown
include("patterns/pubsub.jl")           # Publisher/Subscription data plane
include("patterns/service.jl")          # Service/Client request-reply
include("patterns/action/support.jl")       # action type support, goal id, state machine, responses
include("patterns/action/goal_handle.jl")   # GoalHandle, checkpoints, feedback, settle verbs
include("patterns/action/server.jl")        # ActionServer + service handlers + publication + wire framing
include("patterns/action/client.jl")        # ActionClient / ClientGoal
include("patterns/action/orchestration.jl") # SingleFlight (optional helper)
include("patterns/parameters/types.jl")     # value-type tags, descriptors, readonly
include("patterns/parameters/macro.jl")      # @parameters macro + validate fallbacks + setproperties
include("patterns/parameters/server.jl")     # ParameterServer, reads, transactions
include("patterns/parameters/events.jl")     # on-change events
include("patterns/parameters/services.jl")   # standard parameter services + wire marshalling
include("patterns/parameters/composite.jl")   # multi-schema façade (member-prefixed node-level params)
include("patterns/parameters/client.jl")      # remote ParameterClient (the async/fallible dual)
include("discovery/graph.jl")           # graph introspection
include("typesupport/registry.jl")      # registry entry shape + codegen pipeline + realize
include("typesupport/acquire.jl")       # dynamic (TypeDescription) + ament/colcon acquisition + resolve_type
include("typesupport/cache.jl")         # content-addressed cache + TypeDescription JSON
include("typesupport/export.jl")        # export_typesupport + type_info specialization + service-level identity
include("typesupport/static_types.jl")  # @ros_import / @ros_cache static-type registration
include("typesupport/resolution.jl")    # per-module resolution tables
include("typesupport/manifest.jl")      # per-node dynamic-interaction manifest (after typesupport: needs the cache helpers)
include("interfaces/wellknown.jl")      # statically-compiled bootstrap interfaces
include("performance/warmup.jl")        # precompilation / warm-up (after wellknown: needs Interfaces types)
include("discovery/introspection.jl")   # message introspection
include("typesupport/staticgen.jl")     # @ros_import / @ros_cache static-gen + flush
include("typesupport/authored.jl")      # authored Julia types → ROS (@ros_package / @ros_message)
include("lifecycle.jl")                 # managed lifecycle nodes
include("performance/intraprocess.jl")  # intra-process transport
include("model/component/component.jl") # mixins/components: @mixin, HAS/DOES ports, lifecycle
include("model/component/run.jl")       # node assembly: @node, run, port materialization
include("model/component/composition.jl") # node-kind registry + container LoadNode/ros2 component services
include("model/component/precompile.jl") # tier-2 scaffolding precompile anchors (after the above: names them)

end # module ROSNode
