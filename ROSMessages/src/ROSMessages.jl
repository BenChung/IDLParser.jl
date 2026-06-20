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
include("reflect.jl")
include("macros.jl")

export @ros_msg, @ros_msgs
export to_ros, lift, lower, IL
export il_from_type, il_from_fields, il_field_diff

# Exercise the full `@ros_msgs` pipeline so its CodeInstances — including IDLParser's
# `resolve_constants`/`generate_code`, which are external to this package — bake into
# this pkgimage, sparing downstream consumers (e.g. ROSNode's vendored `Interfaces`)
# the JIT cost.
using PrecompileTools: @setup_workload, @compile_workload
using StaticArrays: SVector
# A small struct to bake the reflection path (primitive + static-array + sequence fields).
struct _ReflectWL; a::Int32; v::SVector{3, Float64}; n::Vector{Int32}; end
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
        # ROSNode calls these IL entry points DIRECTLY (not via `parse_*`): `_parse_interface`,
        # `_build_canonical_entries`, `_build_wellknown_entries`. `parse_*` inline `*_il`, so the
        # workload's `parse_*` calls leave no reusable standalone CI for the direct path; bake
        # each entry point's own native code. (The shared `_parse_section`/PEG combinator tree
        # dispatches dynamically through IDLParser's packrat memo, so its native code isn't
        # serializable here — the first direct parse of a session still pays that codegen once.)
        precompile(Core.kwcall, (NamedTuple{(:name,), Tuple{String}}, typeof(message_il), String))
        precompile(Core.kwcall, (NamedTuple{(:name,), Tuple{String}}, typeof(service_il), String))
        precompile(Core.kwcall, (NamedTuple{(:name,), Tuple{String}}, typeof(action_il), String))
        # RIHS01 hash path.
        d = lower(message_il(msg; name="M"); package="")
        td = type_description_from_struct(d[end], "p/msg/M"; package="p", qualifier="msg")
        tdmsg = TypeDescriptionMsg(td, TypeDescription[])
        calculate_rihs01_hash(tdmsg)
        lift(tdmsg)
        # Reflection path (reflect.jl): Julia struct → IL → TD → hash.
        ril = il_from_type(_ReflectWL; name_of = _ -> "p/msg/X")
        rd  = lower(ril; package="")
        rtd = type_description_from_struct(rd[end], "p/msg/X"; package="p", qualifier="msg")
        calculate_rihs01_hash(TypeDescriptionMsg(rtd, TypeDescription[]))
    end
end

end # module ROSMessages
