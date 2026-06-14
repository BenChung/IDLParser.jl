# Canonical home for every vendored ROS type: `@ros_import` aliases a hit to
# `Interfaces.<pkg>.<qual>.<Name>`, so one wire type (name + RIHS01) maps to exactly
# one Julia struct process-wide (`canonical_type`). The wire types parameters,
# lifecycle, actions, and /rosout marshal live here, as does the
# `type_description_interfaces` subtree that bootstraps type support.
#
# `@ros_msgs` over the vendored tree `Base.include_dependency`s each source, so
# editing a vendored interface invalidates ROSNode's precompile. Generated code
# splices StaticArrays / CDRSerialization in as module objects, so it resolves
# regardless of ROSNode's own deps; depending only on `@ros_msgs`, this is included
# first and every later component file can reference it.
module Interfaces
    using ROSMessages: @ros_msgs
    @ros_msgs "../../vendor"
end
