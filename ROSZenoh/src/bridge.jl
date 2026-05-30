# Bridge between the IDL AST (parsed by ROSMessages) and ROSZenoh's TypeInfo.
# This lets callers go .msg → keyexpr without manually computing the RIHS01
# hash or assembling the qualified ROS2 name.

using ROSMessages: ROSMessages, TypeDescription, TypeDescriptionMsg,
                   type_description_from_struct, calculate_rihs01_hash

"""
    type_info_from_struct(struct_ast, name; package="", qualifier="msg",
                          references=TypeDescription[]) -> TypeInfo

Build a `TypeInfo` (qualified ROS2 name + RIHS01 hash) from a parsed struct
AST. `name` is either a bare name (assembled as `\"<package>/<qualifier>/<name>\"`)
or already fully qualified.

`references` should contain a `TypeDescription` for every nested type the
message refers to; sort it deterministically if cross-implementation hash
parity matters (the RIHS01 hash does not auto-sort).
"""
function type_info_from_struct(struct_ast,
                               name::AbstractString;
                               package::AbstractString="",
                               qualifier::AbstractString="msg",
                               references::AbstractVector{TypeDescription}=TypeDescription[])
    td = type_description_from_struct(struct_ast, name;
                                       package=package, qualifier=qualifier)
    rihs = calculate_rihs01_hash(TypeDescriptionMsg(td, collect(references)))
    hash = type_hash_from_rihs_string(rihs)
    hash === nothing && error("calculate_rihs01_hash produced unparseable output: $rihs")
    return TypeInfo(td.type_name, hash)
end
