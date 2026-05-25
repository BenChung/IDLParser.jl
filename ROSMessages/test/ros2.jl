module ROS2Tests
using IDLParser
using ROSMessages
using Test, PEG
using Moshi.Match: @match
import IDLParser.Parse: specification, preprocessor, parse_whole, unparse, open_idl
import IDLParser.Parse: Literal, ScopedName, ConstExpr, Annotated, Declarator,
    TypeSpec, TypeDecl, ConstDecl, ModuleDecl
import ROSMessages: parse_msg, parse_srv, parse_action, parse_file

# Variant discrimination helpers — Moshi @data variants need pattern matching
# (or `isa T.Variant.Type` from inside @match) rather than plain `isa`.
_is_struct(d) = d isa TypeDecl.Type && @match d begin
    TypeDecl.StructDecl(_, _, _) => true
    _ => false
end
_is_module(d) = d isa ModuleDecl.Type
_is_const(d) = d isa ConstDecl.Type
_is_annotated(d) = d isa Annotated.Type
_struct_name(d) = (@match d begin
    TypeDecl.StructDecl(n, _, _) => n
end)
_struct_members(d) = (@match d begin
    TypeDecl.StructDecl(_, _, ms) => ms
end)
_module_name(m) = (@match m begin
    ModuleDecl.MDecl(n, _) => n
end)
_module_decls(m) = (@match m begin
    ModuleDecl.MDecl(_, ds) => ds
end)
_const_name(c) = (@match c begin
    ConstDecl.CDecl(_, n, _) => n
end)
_const_val(c) = (@match c begin
    ConstDecl.CDecl(_, _, v) => v
end)

# Strip leading Annotated wrappers and return the inner member pair.
_unwrap(m) = m isa Annotated.Type ? _unwrap((@match m begin
    Annotated.Annotation(_, _, inner) => inner
end)) : m

function _find_struct(decls, name::Symbol)
    for d in decls
        if _is_struct(d) && _struct_name(d) == name
            return d
        elseif _is_module(d)
            found = _find_struct(_module_decls(d), name)
            found !== nothing && return found
        elseif _is_annotated(d)
            inner = @match d begin
                Annotated.Annotation(_, _, x) => x
            end
            found = _find_struct([inner], name)
            found !== nothing && return found
        end
    end
    return nothing
end

function _reparse(decls)
    txt = unparse(decls)
    return parse_whole(specification, strip(preprocessor(txt)))
end

# Returns the Bool, Intg, F32, F64, or St payload from a ConstExpr.Lit, or
# throws if the shape is wrong.
function _lit_value(expr)
    @match expr begin
        ConstExpr.Lit(Literal.Bl(v)) => v
        ConstExpr.Lit(Literal.Intg(v)) => v
        ConstExpr.Lit(Literal.F32(v)) => v
        ConstExpr.Lit(Literal.F64(v)) => v
        ConstExpr.Lit(Literal.St(v)) => v
    end
end

@testset "ROS2" begin

@testset "Primitive field round-trip" begin
    decls = parse_msg("int32 x\nfloat32 y"; name="Point")
    @test length(decls) == 1
    s = decls[1]
    @test _is_struct(s)
    @test _struct_name(s) == :Point
    ms = _struct_members(s)
    @test length(ms) == 2
    m1 = _unwrap(ms[1])
    m2 = _unwrap(ms[2])
    @test m1.first == TypeSpec.TInt(32)
    @test m2.first == TypeSpec.TFloat(32)
    @test m1.second == [Declarator.DIdent(:x)]
    @test m2.second == [Declarator.DIdent(:y)]

    reparsed = _reparse(decls)
    @test !isempty(reparsed)
    s2 = _find_struct(reparsed, :Point)
    @test s2 !== nothing
    @test length(_struct_members(s2)) == 2
end

@testset "Array forms" begin
    decls = parse_msg("int32[3] xs"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TInt(32)
    @test m.second == [Declarator.DArray(:xs,
        [ConstExpr.Lit(Literal.Intg(3))])]

    decls = parse_msg("int32[] xs"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TSeq(TypeSpec.TInt(32), nothing)
    @test m.second == [Declarator.DIdent(:xs)]

    decls = parse_msg("int32[<=5] xs"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TSeq(TypeSpec.TInt(32),
        ConstExpr.Lit(Literal.Intg(5)))
    @test m.second == [Declarator.DIdent(:xs)]

    decls = parse_msg("int32[3] a\nint32[] b\nint32[<=5] c"; name="A")
    reparsed = _reparse(decls)
    s = _find_struct(reparsed, :A)
    @test length(_struct_members(s)) == 3
end

@testset "Bounded strings" begin
    decls = parse_msg("string<=64 name"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TString(ConstExpr.Lit(Literal.Intg(64)))
    @test m.second == [Declarator.DIdent(:name)]

    decls = parse_msg("string<=64[] names"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TSeq(
        TypeSpec.TString(ConstExpr.Lit(Literal.Intg(64))), nothing)
    @test m.second == [Declarator.DIdent(:names)]
end

@testset "Type references" begin
    decls = parse_msg("Pose pose"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TRef(
        ScopedName.Name(Symbol[], :Pose, true))

    decls = parse_msg("geometry_msgs/Pose pose"; name="A")
    m = _unwrap(_struct_members(decls[1])[1])
    @test m.first == TypeSpec.TRef(
        ScopedName.Name([:geometry_msgs, :msg], :Pose, true))

    out = unparse(decls)
    @test occursin("geometry_msgs::msg::Pose", out)
end

@testset "Constants hoisting" begin
    src = """
    uint32 VERSION=1
    uint8 STATUS_OK=0
    int32 x
    """
    decls = parse_msg(src; name="Foo")
    @test length(decls) == 2
    @test _is_module(decls[1])
    @test _module_name(decls[1]) == :Foo_Constants
    inner = _module_decls(decls[1])
    @test length(inner) == 2
    @test _is_const(inner[1])
    @test _const_name(inner[1]) == :VERSION
    @test _const_name(inner[2]) == :STATUS_OK
    @test _is_struct(decls[2])
    @test _struct_name(decls[2]) == :Foo

    decls = parse_msg("int32 x"; name="Bar")
    @test length(decls) == 1
    @test _is_struct(decls[1])
end

@testset "Integer literal bases" begin
    src = """
    int32 HX=0xFF
    int32 BN=0b1010
    int32 OC=0o17
    int32 DC=42
    """
    decls = parse_msg(src; name="X")
    cmod = decls[1]
    vals = Dict{Symbol, Int64}()
    for c in _module_decls(cmod)
        @test _is_const(c)
        vals[_const_name(c)] = _lit_value(_const_val(c))
    end
    @test vals[:HX] == 0xFF
    @test vals[:BN] == 0b1010
    @test vals[:OC] == 0o17
    @test vals[:DC] == 42
end

@testset "Defaults" begin
    decls = parse_msg("bool active true"; name="X")
    s = decls[1]
    ms = _struct_members(s)
    @test length(ms) == 1
    m = ms[1]
    @test _is_annotated(m)
    @match m begin
        Annotated.Annotation(name, params, _) => begin
            @test name == ScopedName.Name(Symbol[], :default, true)
            @test length(params) == 1
            @test params[1].first === :value
            @test params[1].second == ConstExpr.Lit(Literal.Bl(true))
        end
    end

    out = unparse(decls)
    @test occursin("@default(TRUE)", out)

    decls = parse_msg("float64 d 3.14"; name="X")
    m = _struct_members(decls[1])[1]
    @match m begin
        Annotated.Annotation(_, params, _) => begin
            @test params[1].second == ConstExpr.Lit(Literal.F64(3.14))
        end
    end

    decls = parse_msg("float32 f 1.5"; name="X")
    m = _struct_members(decls[1])[1]
    @match m begin
        Annotated.Annotation(_, params, _) => begin
            @test params[1].second == ConstExpr.Lit(Literal.F32(1.5f0))
        end
    end
end

@testset "Package wrapping" begin
    decls = parse_msg("int32 x"; name="Pose", package="geometry_msgs")
    @test length(decls) == 1
    outer = decls[1]
    @test _is_module(outer)
    @test _module_name(outer) == :geometry_msgs
    outer_inner = _module_decls(outer)
    @test length(outer_inner) == 1
    sub = outer_inner[1]
    @test _is_module(sub)
    @test _module_name(sub) == :msg
    @test any(d -> _is_struct(d) && _struct_name(d) == :Pose, _module_decls(sub))

    out = unparse(decls)
    @test occursin("module geometry_msgs", out)
    @test occursin("module msg", out)
end

@testset ".srv" begin
    src = """
    int32 a
    ---
    int32 b
    """
    decls = parse_srv(src; name="Compute")
    @test length(decls) == 2
    @test _struct_name(decls[1]) == :Compute_Request
    @test _struct_name(decls[2]) == :Compute_Response

    decls = parse_srv(src; name="Compute", package="my_pkg")
    outer = decls[1]
    @test _module_name(outer) == :my_pkg
    sub = _module_decls(outer)[1]
    @test _module_name(sub) == :srv
    inner = _module_decls(sub)
    @test any(d -> _is_struct(d) && _struct_name(d) == :Compute_Request, inner)
    @test any(d -> _is_struct(d) && _struct_name(d) == :Compute_Response, inner)
end

@testset ".action" begin
    src = """
    int32 g
    ---
    int32 r
    ---
    int32 f
    """
    decls = parse_action(src; name="Do")
    @test length(decls) == 3
    @test _struct_name(decls[1]) == :Do_Goal
    @test _struct_name(decls[2]) == :Do_Result
    @test _struct_name(decls[3]) == :Do_Feedback

    src_empty = """
    int32 g
    ---
    ---
    int32 f
    """
    decls = parse_action(src_empty; name="Do")
    @test length(decls) == 3
    s_result = _find_struct(decls, :Do_Result)
    @test s_result !== nothing
    @test isempty(_struct_members(s_result))
end

@testset "File dispatch with comments and blank lines" begin
    src = """
    # Header comment
    uint32 VERSION=1   # trailing comment
    string<=64 name "default name"  # comment after default

    int32 count
    """
    decls = parse_msg(src; name="Thing")
    @test length(decls) == 2
    @test _module_name(decls[1]) == :Thing_Constants
    s = decls[2]
    @test length(_struct_members(s)) == 2
end

@testset "End-to-end round-trip" begin
    src = """
    # A realistic-ish message
    uint32 VERSION=1
    uint8 STATE_IDLE=0
    uint8 STATE_RUNNING=1
    int32 STATE_MAX=0xFF

    uint64 timestamp
    uint64 timestamp_sample
    int32 sequence_no
    float64 latitude
    float64 longitude
    float32 altitude
    bool valid true
    uint8 state
    string<=64 frame_id
    int32[3] checksum
    float32[] history
    geometry_msgs/Pose pose
    """
    decls = parse_msg(src; name="Sample", package="example_msgs")
    idl_text = unparse(decls)
    path, io = mktemp()
    try
        write(io, idl_text)
        close(io)
        reparsed = open_idl(path)
        @test !isempty(reparsed)
        s = _find_struct(reparsed, :Sample)
        @test s !== nothing
        @test length(_struct_members(s)) == 12
    finally
        rm(path; force=true)
    end
end

@testset "parse_file dispatch" begin
    mktempdir() do dir
        msg_path = joinpath(dir, "Point.msg")
        write(msg_path, "int32 x\nint32 y\n")
        d = parse_file(msg_path)
        @test length(d) == 1
        @test _is_struct(d[1])
        @test _struct_name(d[1]) == :Point

        srv_path = joinpath(dir, "Echo.srv")
        write(srv_path, "string<=64 in\n---\nstring<=64 out\n")
        d = parse_file(srv_path)
        @test length(d) == 2
        @test _struct_name(d[1]) == :Echo_Request

        action_path = joinpath(dir, "Move.action")
        write(action_path, "int32 a\n---\nint32 b\n---\nint32 c\n")
        d = parse_file(action_path)
        @test length(d) == 3
        @test _struct_name(d[1]) == :Move_Goal
    end
end

@testset "RIHS01" begin
    import ROSMessages: rihs01_hash, calculate_rihs01_hash, type_description_from_struct,
        TypeDescriptionMsg, TypeDescription, FieldDescription, FieldTypeDescription,
        TYPE_ID_STRING, TYPE_ID_INT32, TYPE_ID_UINT32, TYPE_ID_NESTED_TYPE,
        TYPE_ID_BOUNDED_STRING, ARRAY_OFFSET, BOUNDED_SEQUENCE_OFFSET,
        UNBOUNDED_SEQUENCE_OFFSET
    import ROSMessages: to_ros2_json

    # std_msgs/msg/String — single STRING field. Published hash from ROS2.
    @testset "std_msgs/msg/String" begin
        decls = parse_msg("string data"; name="String")
        h = rihs01_hash(decls[1], "std_msgs/msg/String")
        @test h == "RIHS01_df668c740482bbd48fb39d76a70dfd4bd59db1288021743503259e948f6b1a18"
    end

    # builtin_interfaces/msg/Time — INT32 sec + UINT32 nanosec.
    @testset "builtin_interfaces/msg/Time" begin
        decls = parse_msg("int32 sec\nuint32 nanosec"; name="Time")
        h = rihs01_hash(decls[1], "builtin_interfaces/msg/Time")
        @test h == "RIHS01_b106235e25a4c5ed35098aa0a61a3ee9c9b18d197f398b0e4206cea9acf9c197"
    end

    # `package=` builds the fully-qualified name with the rosidl `/msg/` segment.
    @testset "package-qualified name" begin
        decls = parse_msg("string data"; name="String")
        h = rihs01_hash(decls[1], "String"; package="std_msgs")
        @test h == "RIHS01_df668c740482bbd48fb39d76a70dfd4bd59db1288021743503259e948f6b1a18"
    end

    @testset "JSON shape" begin
        td = type_description_from_struct(
            parse_msg("string data"; name="String")[1],
            "std_msgs/msg/String")
        msg = TypeDescriptionMsg(td, TypeDescription[])
        json = to_ros2_json(msg)
        @test json == """{"type_description": {"type_name": "std_msgs/msg/String", "fields": [{"name": "data", "type": {"type_id": 17, "capacity": 0, "string_capacity": 0, "nested_type_name": ""}}]}, "referenced_type_descriptions": []}"""
    end

    # Defaults must be excluded from the hash per the spec — two messages
    # differing only in a `@default(...)` annotation should hash equal.
    @testset "defaults excluded from hash" begin
        h_no_default = rihs01_hash(parse_msg("int32 value"; name="Test")[1], "test/msg/Test")
        h_with_default = rihs01_hash(parse_msg("int32 value 42"; name="Test")[1], "test/msg/Test")
        @test h_no_default == h_with_default
    end

    # Empty message — no fields. Just needs to produce a well-formed hash.
    @testset "empty message" begin
        # parse_msg with an empty body — the parser should produce a struct with zero members.
        decls = parse_msg(""; name="Empty")
        h = rihs01_hash(decls[1], "test_msgs/msg/Empty")
        @test startswith(h, "RIHS01_")
        @test length(h) == 7 + 64
    end

    # Type-id offsets for arrays / bounded sequences / unbounded sequences.
    @testset "type_id offsets" begin
        # int32[3] → INT32 + ARRAY_OFFSET = 6 + 48 = 54, capacity=3
        td = type_description_from_struct(parse_msg("int32[3] xs"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_INT32 + ARRAY_OFFSET
        @test ft.capacity == 3
        @test ft.string_capacity == 0

        # int32[]  → INT32 + UNBOUNDED_SEQUENCE_OFFSET, capacity=0
        td = type_description_from_struct(parse_msg("int32[] xs"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_INT32 + UNBOUNDED_SEQUENCE_OFFSET
        @test ft.capacity == 0

        # int32[<=5] → INT32 + BOUNDED_SEQUENCE_OFFSET, capacity=5
        td = type_description_from_struct(parse_msg("int32[<=5] xs"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_INT32 + BOUNDED_SEQUENCE_OFFSET
        @test ft.capacity == 5

        # string<=64 → BOUNDED_STRING (21), string_capacity=64
        td = type_description_from_struct(parse_msg("string<=64 s"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_BOUNDED_STRING
        @test ft.string_capacity == 64

        # string<=64[] → BOUNDED_STRING + UNBOUNDED_SEQUENCE_OFFSET, string_capacity=64
        td = type_description_from_struct(parse_msg("string<=64[] s"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_BOUNDED_STRING + UNBOUNDED_SEQUENCE_OFFSET
        @test ft.string_capacity == 64
        @test ft.capacity == 0

        # geometry_msgs/Pose → NESTED_TYPE, nested_type_name expanded via /msg/
        td = type_description_from_struct(parse_msg("geometry_msgs/Pose p"; name="A")[1], "a/msg/A")
        ft = td.fields[1].field_type
        @test ft.type_id == TYPE_ID_NESTED_TYPE
        @test ft.nested_type_name == "geometry_msgs/msg/Pose"
    end

    # Nested types: the referenced TypeDescription must be passed in by the caller
    # (and pre-sorted alphabetically if the caller wants cross-impl-stable hashes).
    @testset "nested references" begin
        time_td = type_description_from_struct(
            parse_msg("int32 sec\nuint32 nanosec"; name="Time")[1],
            "builtin_interfaces/msg/Time")
        header_struct = parse_msg(
            "builtin_interfaces/Time stamp\nstring frame_id"; name="Header")[1]
        h = rihs01_hash(header_struct, "std_msgs/msg/Header"; references=[time_td])
        @test startswith(h, "RIHS01_")
        # Caller-order sensitivity: same data, different reference order → different hash.
        # Single ref here can't test ordering — but two refs in opposite order should differ.
        type_a = TypeDescription("pkg/msg/A", [FieldDescription("v", FieldTypeDescription(TYPE_ID_INT32))])
        type_b = TypeDescription("pkg/msg/B", [FieldDescription("v", FieldTypeDescription(0x0B))])
        main_struct = parse_msg("pkg/A a\npkg/B b"; name="Main")[1]
        h_ab = rihs01_hash(main_struct, "pkg/msg/Main"; references=[type_a, type_b])
        h_ba = rihs01_hash(main_struct, "pkg/msg/Main"; references=[type_b, type_a])
        @test h_ab != h_ba
    end
end

end # @testset ROS2
end # module
