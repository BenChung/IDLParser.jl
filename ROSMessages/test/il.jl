module ILTests
using IDLParser
using ROSMessages
using Test
using Moshi.Match: @match
import ROSMessages: parse_msg, parse_srv, parse_action, to_ros, lift, lower,
    message_il, service_il, IL

# `lower ∘ lift` must be the identity on the IDL `Parse.Decl` a forward parse
# produced — the IL captures everything the lowering needs. `Parse` types
# `@derive[Eq]`, so `==` is a deep structural comparison.
roundtrips(decls; package="") = lower(lift(decls); package=package) == decls

@testset "IL" begin

@testset "lower∘lift identity — messages" begin
    @test roundtrips(parse_msg("int32 x\nfloat32 y"; name="Point"))
    @test roundtrips(parse_msg("int32[3] a\nint32[] b\nint32[<=5] c"; name="A"))
    @test roundtrips(parse_msg("string<=64 name\nstring desc"; name="A"))
    @test roundtrips(parse_msg("string<=64[] names"; name="A"))
    @test roundtrips(parse_msg("geometry_msgs/Pose pose\nPose rel"; name="A"))
    # legacy aliases collapse to refs at parse time, so the IDL round-trips.
    @test roundtrips(parse_msg("time t\nduration d\nHeader h"; name="A"))
    # char/byte collapse to uint8/octet at parse time — still IDL-stable.
    @test roundtrips(parse_msg("char c\nbyte b\nuint8 u"; name="A"))
end

@testset "lower∘lift identity — constants & defaults" begin
    @test roundtrips(parse_msg("uint32 VERSION=1\nuint8 STATUS_OK=0\nint32 x"; name="Foo"))
    @test roundtrips(parse_msg("int32 HX=0xFF\nint32 BN=0b1010\nint32 OC=0o17"; name="X"))
    @test roundtrips(parse_msg("bool active true\nfloat64 d 3.14\nfloat32 f 1.5"; name="X"))
    @test roundtrips(parse_msg("string<=64 name \"default name\""; name="X"))
    @test roundtrips(parse_msg("int32[3] foo [1, 2, 3]"; name="X"))
end

@testset "lower∘lift identity — packages" begin
    @test roundtrips(parse_msg("int32 x"; name="Pose", package="geometry_msgs");
        package="geometry_msgs")
end

@testset "lower∘lift identity — services & actions" begin
    srv = parse_srv("int32 a\nuint8 K=3\n---\nint32 b"; name="Compute")
    @test roundtrips(srv)
    @test roundtrips(parse_srv("int32 a\n---\nint32 b"; name="Compute", package="my_pkg");
        package="my_pkg")

    act = parse_action("int32 g\n---\nint32 r\n---\nint32 f"; name="Do")
    @test roundtrips(act)
    # empty section survives the round trip
    @test roundtrips(parse_action("int32 g\n---\n---\nint32 f"; name="Do"))
end

@testset "to_ros text" begin
    decls = parse_msg("int32 x\nfloat32[] ys\ngeometry_msgs/Pose pose"; name="P")
    txt = to_ros(decls)
    @test occursin("int32 x", txt)
    @test occursin("float32[] ys", txt)
    @test occursin("geometry_msgs/Pose pose", txt)

    # constants render as `<type> NAME=value`, defaults as `<type> name value`
    decls = parse_msg("uint32 VERSION=7\nbool active true"; name="C")
    txt = to_ros(decls)
    @test occursin("uint32 VERSION=7", txt)
    @test occursin("bool active true", txt)

    # services emit the `---` separator
    txt = to_ros(parse_srv("int32 a\n---\nint32 b"; name="S"))
    @test occursin("int32 a", txt)
    @test occursin("---", txt)
    @test occursin("int32 b", txt)
end

@testset "to_ros reparses to the same IDL" begin
    # text → IDL → text → IDL is stable when the name/package are supplied
    # again (they don't live in the interface text).
    src = """
    uint32 VERSION=1
    uint8 STATE_IDLE=0
    uint64 timestamp
    float64 latitude
    bool valid true
    string<=64 frame_id
    int32[3] checksum
    float32[] history
    geometry_msgs/Pose pose
    """
    decls = parse_msg(src; name="Sample", package="example_msgs")
    decls2 = parse_msg(to_ros(decls); name="Sample", package="example_msgs")
    @test decls2 == decls
end

@testset "IL value round-trips through text" begin
    # message_il(to_ros(...)) reproduces the IL field/constant shape directly.
    il = message_il("int32 a\nstring<=8[] b\nuint16 K=5"; name="M")
    txt = IL.unparse(il)
    il2 = message_il(txt; name="M")
    @test length(il2.fields) == 2
    @test length(il2.constants) == 1
    @test il2.constants[1].name === :K
    @test il2.fields[1].name === :a
    @test il2.fields[2].name === :b
    @test il2.fields[2].type.base == IL.RBase.RStr(8)
    @test il2.fields[2].type.array == IL.ArraySpec.AUnbounded()
end

end # @testset IL
end # module
