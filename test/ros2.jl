module ROS2Tests
using IDLParser
using Test, PEG
using Moshi.Match: @match
import IDLParser.Parse: specification, preprocessor, parse_whole, unparse, open_idl
import IDLParser.Parse: Literal, ScopedName, ConstExpr, Annotated, Declarator,
    TypeSpec, TypeDecl, ConstDecl, ModuleDecl
import IDLParser.ROS2: parse_msg, parse_srv, parse_action, parse_file

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

end # @testset ROS2
end # module
