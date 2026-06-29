module ReROS

# Record a live ROS 2 (rmw_zenoh) network into Rerun for post-hoc analysis.
# Design: DESIGN.md (architecture) + DESIGN-MAPPING.md (the mapper subsystem).
#
# Implemented so far:
#   Phase 1  — the Rerun sink + single-writer log pump (DESIGN.md §5)
#   Mapping  — map_into!/manifest/structural + declarative specs + std/geometry mappers
#              (DESIGN-MAPPING.md §2-§5, §12-§13; Rerun-only, no ROS deps yet)

import Rerun
import ROSReach
import ROSNode
import ROSMessages

include("sink.jl")

# Mapper subsystem.
include("mapping/il.jl")
include("mapping/grammar.jl")
include("mapping/conform.jl")
include("mapping/manifest.jl")
include("mapping/map.jl")
include("mapping/codegen.jl")
include("mapping/std_msgs.jl")
include("mapping/geometry_msgs.jl")
include("mapping/builtins.jl")

# Recorder (discovery + graph recording; data subscriptions attach in a later phase).
include("config.jl")
include("graph_log.jl")
include("recorder.jl")
include("subscribe.jl")
include("toml.jl")

# Sink. `Sink` (not `LogSink`): Rerun already exports `LogSink`.
export Sink, LogItem, TimeStamps, Timelines, SinkSpec, Save, Spawn, Grpc, emit!, dropped, write_errors

# Mapping API.
export map_into!, map_struct!, MapContext, MapOpts, child,
       MapperManifest, MapperEntry, NameMatch, TopicMatch, StructMatch,
       FieldShape, shape, archetype, BUILTIN_MAPPERS, ros_type_name

# Recorder API.
export Recorder, RecorderConfig, reconcile!, start!, stop!, attach_recorder!,
       Rule, Selector, rule, exclude, topic, type, namespace, kind, node,
       load_config, load_config_string

end # module ReROS
