"""
    ROSNode

A Julian ROS2 client built on Zenoh. See `ARCHITECTURE.md` for the design.

This file wires the component modules together in bottom-up dependency order.
Components are included as they are implemented.
"""
module ROSNode

# ── component includes (bottom-up) ───────────────────────────────────────
# Populated by the implementation as each component lands. Order matters:
# primitives → data plane → object model → patterns → graph/lifecycle.

include("interfaces/interfaces.jl")     # @ros_msgs over the vendored tree → Interfaces.<pkg>… (first: every file references it)
include("base/core.jl")                 # shared seams: re-exports, exceptions, status tokens (§6/§8/§9)
include("base/time.jl")                 # §7 clocks, RTime/Duration, Timer/Rate
include("base/settlement.jl")           # §8/§9 write-once ResultCell + fail-safe settle
include("base/serialization.jl")        # §2.1/§3 message ↔ ZBytes bridge + attachment
include("model/context.jl")             # §5 Context: session, registry, graph, shutdown
include("model/node/node.jl")           # §6 Node type + §4 concurrency policy
include("model/node/entity.jl")         # §6 generic Entity, data routes, declare pub/sub (+ D4 re-latch)
include("model/node/dispatch.jl")       # §4 static subscription dispatch runtime
include("model/node/dynamic.jl")        # §11/D5 dynamic keyexpr dispatch + §D9 manifest warm
include("model/node/teardown.jl")       # §6/§14 entity/node teardown
include("patterns/pubsub.jl")           # §4 Publisher/Subscription data plane
include("patterns/service.jl")          # §10 Service/Client request-reply
include("patterns/action/support.jl")       # §9 action type support, goal id, state machine, responses
include("patterns/action/goal_handle.jl")   # §9 GoalHandle, checkpoints, feedback, settle verbs
include("patterns/action/server.jl")        # §9 ActionServer + service handlers + publication + wire framing
include("patterns/action/client.jl")        # §9 ActionClient / ClientGoal
include("patterns/action/orchestration.jl") # §9 SingleFlight (optional helper)
include("patterns/parameters/types.jl")     # §10 value-type tags, descriptors, readonly
include("patterns/parameters/macro.jl")      # §10 @parameters macro + validate fallbacks + setproperties
include("patterns/parameters/server.jl")     # §10 ParameterServer, reads, transactions
include("patterns/parameters/events.jl")     # §10 on-change events
include("patterns/parameters/services.jl")   # §10 standard parameter services + wire marshalling
include("discovery/graph.jl")           # §13 graph introspection
include("typesupport/registry.jl")      # §11 registry entry shape + codegen pipeline + realize
include("typesupport/acquire.jl")       # §11 dynamic (TypeDescription) + ament/colcon acquisition + resolve_type
include("typesupport/cache.jl")         # §11 content-addressed cache + TypeDescription JSON
include("typesupport/export.jl")        # §11 export_typesupport + type_info specialization + service-level identity
include("typesupport/static_types.jl")  # §11/D5 @ros_import / @ros_cache static-type registration
include("typesupport/resolution.jl")    # D10B per-module resolution tables
include("typesupport/manifest.jl")      # §D9 per-node dynamic-interaction manifest (after typesupport: needs the cache helpers)
include("interfaces/wellknown.jl")      # §11/§13 statically-compiled bootstrap interfaces (D5 S1)
include("performance/warmup.jl")        # §D8 precompilation / warm-up (after wellknown: needs Interfaces types)
include("discovery/introspection.jl")   # message introspection
include("typesupport/staticgen.jl")     # §11/D5 @ros_import / @ros_cache static-gen + flush
include("lifecycle.jl")                 # managed lifecycle nodes
include("performance/intraprocess.jl")  # intra-process transport

end # module ROSNode
