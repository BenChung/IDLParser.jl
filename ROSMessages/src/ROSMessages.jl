module ROSMessages

using IDLParser
import IDLParser: Parse, ConstResolution, Generation

include("il.jl")
include("ros2.jl")
include("rihs01.jl")
include("macros.jl")

export @ros_msg, @ros_msgs
export to_ros, lift, lower, IL

# Bake the entire `@ros_msgs` pipeline into the pkgimage so consumers (e.g. ROSNode's
# vendored `Interfaces`) don't re-JIT it at *their* precompile (~6s saved there):
#   • ros2.jl PEG grammar — `parse_msg`/`parse_srv`/`parse_action` (→ `message_il`/
#     `service_il`/`action_il` + `lower`) and `action_protocol_decls`;
#   • IDLParser's `resolve_constants` + `generate_code` — the resolve/codegen half of
#     the chain. Exercising them here caches those *external* CodeInstances into this
#     pkgimage (so IDLParser needs no workload of its own, and we never compile its
#     unused IDL-text parser);
#   • the RIHS01 path — `type_description_from_struct` → `calculate_rihs01_hash` + `lift`.
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
        # Full parse → resolve → generate chain, mirroring `_expand_msg_files`.
        decls = Parse.Decl[]
        append!(decls, parse_msg(msg; name="M", package="p"))
        append!(decls, parse_srv(srv; name="S", package="p"))
        append!(decls, parse_action(act; name="A", package="p"))
        append!(decls, action_protocol_decls("A"; package="p"))
        resolved = ConstResolution.resolve_constants(decls)
        Generation.generate_code(resolved)
        # RIHS01 path: lower → struct AST → TypeDescription → hash → lift.
        d = lower(message_il(msg; name="M"); package="")
        td = type_description_from_struct(d[end], "p/msg/M"; package="p", qualifier="msg")
        tdmsg = TypeDescriptionMsg(td, TypeDescription[])
        calculate_rihs01_hash(tdmsg)
        lift(tdmsg)
    end
end

end # module ROSMessages
