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

include("interfaces.jl")      # @ros_msgs over the vendored tree → Interfaces.<pkg>… (first: every file references it)
include("core.jl")            # shared seams: re-exports, exceptions, status tokens (§6/§8/§9)
include("time.jl")            # §7 clocks, RTime/Duration, Timer/Rate
include("settlement.jl")      # §8/§9 write-once ResultCell + fail-safe settle
include("serialization.jl")   # §2.1/§3 message ↔ ZBytes bridge + attachment
include("context.jl")         # §5 Context: session, registry, graph, shutdown
include("node.jl")            # §6 Node: namespacing, entity registration
include("pubsub.jl")          # §4 Publisher/Subscription data plane
include("service.jl")         # §10 Service/Client request-reply
include("action.jl")          # §11 Action server/client
include("parameters.jl")      # §12 Parameters
include("graph.jl")           # §13 graph introspection
include("typesupport.jl")     # type support / runtime type registry
include("wellknown.jl")       # §11/§13 statically-compiled bootstrap interfaces (D5 S1)
include("warmup.jl")          # §D8 precompilation / warm-up (after wellknown: needs Interfaces types)
include("introspection.jl")   # message introspection
include("staticgen.jl")       # §11/D5 @ros_import / @ros_cache static-gen + flush
include("lifecycle.jl")       # managed lifecycle nodes
include("intraprocess.jl")    # intra-process transport

end # module ROSNode
