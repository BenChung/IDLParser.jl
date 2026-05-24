module TypeDescriptionInterfacesTests
using IDLParser
using CDRSerialization: CDRReader, CDRWriter
using Test

const FILES_DIR = joinpath(@__DIR__, "files/type_description_interfaces")
const MSG_NAMES = ("FieldType", "Field", "IndividualTypeDescription",
                   "TypeDescription", "TypeSource", "KeyValue")

# Run the full pipeline once at module load: parse all six .msg sources, merge
# the AST under the shared `type_description_interfaces::msg` module, resolve
# constants, generate Julia code, and evaluate it into `Tester`.
module Tester
end

let combined = IDLParser.Parse.Decl[]
    for name in MSG_NAMES
        src = read(joinpath(FILES_DIR, name * ".msg"), String)
        decls = IDLParser.ROS2.parse_msg(src; name=name,
            package="type_description_interfaces")
        append!(combined, decls)
    end
    resolved = IDLParser.ConstResolution.resolve_constants(combined)
    for c in IDLParser.Generation.generate_code(resolved)
        Tester.eval(c)
    end
end

const TDI = Tester.type_description_interfaces.msg

@testset "type_description_interfaces" begin

@testset "generation defines all six types" begin
    for name in MSG_NAMES
        @test isdefined(TDI, Symbol(name))
    end
end

@testset "FieldType constants" begin
    C = TDI.FieldType_Constants
    @test C.FIELD_TYPE_NOT_SET   == 0
    @test C.FIELD_TYPE_NESTED_TYPE == 1
    @test C.FIELD_TYPE_STRING    == 17
    @test C.FIELD_TYPE_INT32     == 6
    @test C.FIELD_TYPE_UINT32    == 7
    # The high-end constants matter for the offset arithmetic used in RIHS01.
    @test C.FIELD_TYPE_INT32_ARRAY                == 54
    @test C.FIELD_TYPE_INT32_BOUNDED_SEQUENCE     == 102
    @test C.FIELD_TYPE_INT32_UNBOUNDED_SEQUENCE   == 150
    @test C.FIELD_TYPE_BOUNDED_WSTRING_UNBOUNDED_SEQUENCE == 166
end

@testset "scalar round-trip — FieldType" begin
    ft = TDI.FieldType(
        type_id = 0x11,
        capacity = UInt64(0),
        string_capacity = UInt64(0),
        nested_type_name = "")
    buf = IOBuffer()
    w = CDRWriter(buf)
    write(w, ft)
    seekstart(buf)
    r = CDRReader(buf)
    back = read(r, TDI.FieldType)
    @test back == ft
end

@testset "string + sequence round-trip — IndividualTypeDescription" begin
    f = TDI.Field(
        name = "data",
        type = TDI.FieldType(
            type_id = TDI.FieldType_Constants.FIELD_TYPE_STRING,
            capacity = UInt64(0),
            string_capacity = UInt64(0),
            nested_type_name = ""),
        default_value = "")
    itd = TDI.IndividualTypeDescription(
        type_name = "std_msgs/msg/String",
        fields = [f])

    buf = IOBuffer()
    w = CDRWriter(buf)
    write(w, itd)
    seekstart(buf)
    r = CDRReader(buf)
    back = read(r, TDI.IndividualTypeDescription)
    @test back.type_name == "std_msgs/msg/String"
    @test length(back.fields) == 1
    @test back.fields[1].name == "data"
    @test back.fields[1].type.type_id == TDI.FieldType_Constants.FIELD_TYPE_STRING
    @test back.fields[1].default_value == ""
end

@testset "nested + sequence-of-struct round-trip — TypeDescription" begin
    # std_msgs/msg/String shape — same one we hash in the RIHS01 tests.
    main_itd = TDI.IndividualTypeDescription(
        type_name = "std_msgs/msg/String",
        fields = [TDI.Field(
            name = "data",
            type = TDI.FieldType(
                type_id = TDI.FieldType_Constants.FIELD_TYPE_STRING,
                capacity = UInt64(0),
                string_capacity = UInt64(0),
                nested_type_name = ""),
            default_value = "")])
    td = TDI.TypeDescription(
        type_description = main_itd,
        referenced_type_descriptions = TDI.IndividualTypeDescription[])

    buf = IOBuffer()
    w = CDRWriter(buf)
    write(w, td)
    seekstart(buf)
    r = CDRReader(buf)
    back = read(r, TDI.TypeDescription)
    @test back == td
end

@testset "multiple referenced types round-trip" begin
    time_itd = TDI.IndividualTypeDescription(
        type_name = "builtin_interfaces/msg/Time",
        fields = [
            TDI.Field(name="sec",
                type=TDI.FieldType(type_id=UInt8(6),
                    capacity=UInt64(0), string_capacity=UInt64(0),
                    nested_type_name=""),
                default_value=""),
            TDI.Field(name="nanosec",
                type=TDI.FieldType(type_id=UInt8(7),
                    capacity=UInt64(0), string_capacity=UInt64(0),
                    nested_type_name=""),
                default_value=""),
        ])
    header_itd = TDI.IndividualTypeDescription(
        type_name = "std_msgs/msg/Header",
        fields = [
            TDI.Field(name="stamp",
                type=TDI.FieldType(type_id=UInt8(1),
                    capacity=UInt64(0), string_capacity=UInt64(0),
                    nested_type_name="builtin_interfaces/msg/Time"),
                default_value=""),
            TDI.Field(name="frame_id",
                type=TDI.FieldType(type_id=UInt8(17),
                    capacity=UInt64(0), string_capacity=UInt64(0),
                    nested_type_name=""),
                default_value=""),
        ])
    td = TDI.TypeDescription(
        type_description = header_itd,
        referenced_type_descriptions = [time_itd])

    buf = IOBuffer()
    w = CDRWriter(buf)
    write(w, td)
    seekstart(buf)
    r = CDRReader(buf)
    back = read(r, TDI.TypeDescription)
    @test back == td
    @test length(back.referenced_type_descriptions) == 1
    @test back.referenced_type_descriptions[1].type_name == "builtin_interfaces/msg/Time"
    @test length(back.referenced_type_descriptions[1].fields) == 2
end

end
end
