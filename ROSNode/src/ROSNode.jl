"""
    ROSNode

A Julian ROS 2 client built on Zenoh, implementing the node-centric concepts of
the ROS 2 graph: https://docs.ros.org/en/rolling/Concepts/Basic.html
"""
module ROSNode

# Includes are ordered strictly bottom-up: each file follows its transitive deps,
# since Julia resolves no forward references. interfaces.jl is first because every
# file references Interfaces.<pkg>; component/precompile.jl is last because its
# @compile_workload anchors precompile() on names the earlier includes define.

include("interfaces/interfaces.jl")
include("base/core.jl")
include("base/time.jl")
include("base/settlement.jl")
include("base/serialization.jl")
include("model/context.jl")
include("model/node/node.jl")
include("model/node/entity.jl")
include("model/node/dispatch.jl")
include("model/node/dynamic.jl")
include("model/node/teardown.jl")
include("patterns/pubsub.jl")
include("patterns/service.jl")
include("patterns/action/support.jl")
include("patterns/action/goal_handle.jl")
include("patterns/action/server.jl")
include("patterns/action/client.jl")
include("patterns/action/orchestration.jl")
include("patterns/parameters/types.jl")
include("patterns/parameters/macro.jl")
include("patterns/parameters/server.jl")
include("patterns/parameters/events.jl")
include("patterns/parameters/services.jl")
include("patterns/parameters/composite.jl")
include("patterns/parameters/client.jl")
include("discovery/graph.jl")
include("typesupport/registry.jl")
include("typesupport/acquire.jl")
include("typesupport/cache.jl")
include("typesupport/export.jl")
include("typesupport/static_types.jl")
include("typesupport/resolution.jl")
include("typesupport/manifest.jl")
include("interfaces/wellknown.jl")
include("performance/warmup.jl")
include("discovery/introspection.jl")
include("typesupport/staticgen.jl")
include("typesupport/authored.jl")
include("lifecycle.jl")
include("performance/intraprocess.jl")
include("model/component/component.jl")
include("model/component/run.jl")
include("model/component/composition.jl")
include("model/component/precompile.jl")

end # module ROSNode
