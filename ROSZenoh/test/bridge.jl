using ROSMessages: parse_msg

@testset "bridge — type_info_from_struct" begin
    src = "string data\n"
    decls = parse_msg(src; name="String")
    struct_ast = decls[1]
    ti = type_info_from_struct(struct_ast, "String"; package="std_msgs", qualifier="msg")
    @test ti.name == "std_msgs/msg/String"
    @test ti.hash.version == 0x01
    # The hash should be deterministic and non-zero for a real struct.
    @test ti.hash != TypeHash()
    # Re-running gives the same hash.
    ti2 = type_info_from_struct(struct_ast, "String"; package="std_msgs", qualifier="msg")
    @test ti.hash == ti2.hash
end
