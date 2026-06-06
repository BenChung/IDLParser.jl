"""
    IDLParser

Parse ROS 2 interface definitions (`.idl`/`.msg`) into Julia struct definitions.

The pipeline runs in three stages, each a submodule: [`Parse`](@ref) builds the
specification AST, [`ConstResolution`](@ref) resolves constant expressions and
scopes, and [`Generation`](@ref) emits the Julia types.

See the ROS 2 interface model at
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html and the IDL
type mapping at https://design.ros2.org/articles/idl_interface_definition.html.
"""
module IDLParser

module Parse
    include("parsing/types.jl")
    include("parsing/parse.jl")
    include("parsing/unparse.jl")
end

module ConstResolution
    include("const_resolution/types.jl")
    include("const_resolution/const_resolve.jl")
end

module Generation
    include("generation/gen.jl")
end

# No precompile workload by design. ROSNode's `@ros_msgs` path uses only
# `ConstResolution.resolve_constants` and `Generation.generate_code`, so ROSMessages
# bakes their coverage via its own parse_msg → resolve → generate workload. The IDL
# text parser (`Parse.specification`) goes unbaked; a standalone `.idl` consumer that
# wants it should add a workload exercising `open_idl`/`parse_whole`.

end
