# The canonical generated interface types (static), included first so every
# component file can reference `Interfaces.<pkg>.<qual>.<Name>` — the wire types
# the §11/§13 bootstrap, parameters, lifecycle, actions, and /rosout marshal.
#
# `@ros_msgs` over the vendored tree emits the nested
# `<pkg>.{msg,srv,action}.<Name>` modules and `Base.include_dependency`s each
# source, so editing any vendored interface invalidates ROSNode's precompile. The
# generated code references StaticArrays / CDRSerialization via spliced module
# objects (no `import`), so it resolves here regardless of ROSNode's deps.
# Self-contained (only `@ros_msgs`), so it can be included first.
#
# `Interfaces` is the canonical home for every vendored ROS type: `@ros_import`
# aliases to `Interfaces.<pkg>.<qual>.<Name>`, so one wire type (name + RIHS01)
# maps to exactly one Julia struct (see `canonical_type` in wellknown.jl). The
# `type_description_interfaces` subtree also backs the §11/§13 bootstrap — the
# wire⇄internal bridge in wellknown.jl converts between these generated types and
# ROSMessages' internal hashing forms.
module Interfaces
    using ROSMessages: @ros_msgs
    @ros_msgs "../../vendor"
end
