module ROSMessages

using IDLParser
import IDLParser: Parse, ConstResolution, Generation

include("il.jl")
include("ros2.jl")
include("rihs01.jl")
include("macros.jl")

export @ros_msg, @ros_msgs
export to_ros, lift, lower, IL

# Bake the ROS2 `.msg`/`.srv`/`.action` pipeline into the pkgimage: the ros2.jl
# PEG grammar (`message_il`/`service_il`/`action_il`), `lower`, the RIHS01 hashing
# (`type_description_from_struct` → `calculate_rihs01_hash`), and `lift` — exactly
# the path `@ros_msgs` drives at a consumer's precompile (e.g. ROSNode's vendored
# `Interfaces`). Without this each consumer re-JITs that grammar (~several seconds).
# The IDL parse/resolve/generate layer is baked by IDLParser's own workload.
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
        m = message_il(msg; name="M")
        s = service_il(srv; name="S")
        a = action_il(act; name="A")
        lower(m; package="p")
        lower(s; package="p")
        lower(a; package="p")
        # RIHS01 path: lower → struct AST → TypeDescription → hash → lift.
        decls = lower(m; package="")
        ast = decls[end]                       # the StructDecl (after any _Constants)
        td = type_description_from_struct(ast, "p/msg/M"; package="p", qualifier="msg")
        tdmsg = TypeDescriptionMsg(td, TypeDescription[])
        calculate_rihs01_hash(tdmsg)
        lift(tdmsg)
    end
end

end # module ROSMessages
