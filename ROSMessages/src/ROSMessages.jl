"""
    ROSMessages

Translate ROS 2 `.msg`/`.srv`/`.action` interface definitions into Julia structs
and compute their RIHS01 type hashes. Text parses into an intermediate language
(`IL`), lowers to IDLParser's `Parse.Decl` AST for codegen, and lifts back to text
for round-tripping. The `@ros_msg`/`@ros_msgs` macros drive the forward pipeline.

ROS 2 interface concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html
"""
module ROSMessages

using IDLParser
import IDLParser: Parse, ConstResolution, Generation

include("il.jl")
include("ros2.jl")
include("rihs01.jl")
include("macros.jl")

export @ros_msg, @ros_msgs
export to_ros, lift, lower, IL

# Exercise the full `@ros_msgs` pipeline so its CodeInstances — including IDLParser's
# `resolve_constants`/`generate_code`, which are external to this package — bake into
# this pkgimage, sparing downstream consumers (e.g. ROSNode's vendored `Interfaces`)
# the JIT cost.
using PrecompileTools: @setup_workload, @compile_workload
@setup_workload begin
    msg = """
    int32 FOO=7
    int32 a
    float64 b
    bool flag
    string<=16 label
    float64[3] arr
    int32[] seq
    builtin_interfaces/Time stamp
    """
    srv = msg * "---\n" * msg
    act = msg * "---\n" * msg * "---\n" * msg
    @compile_workload begin
        # Mirrors `_expand_msg_files`.
        decls = Parse.Decl[]
        append!(decls, parse_msg(msg; name="M", package="p"))
        append!(decls, parse_srv(srv; name="S", package="p"))
        append!(decls, parse_action(act; name="A", package="p"))
        append!(decls, action_protocol_decls("A"; package="p"))
        resolved = ConstResolution.resolve_constants(decls)
        Generation.generate_code(resolved)
        # RIHS01 hash path.
        d = lower(message_il(msg; name="M"); package="")
        td = type_description_from_struct(d[end], "p/msg/M"; package="p", qualifier="msg")
        tdmsg = TypeDescriptionMsg(td, TypeDescription[])
        calculate_rihs01_hash(tdmsg)
        lift(tdmsg)
    end
end

end # module ROSMessages
