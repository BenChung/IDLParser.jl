module ParsingTests
using IDL
using Test, PEG
import IDL.Parse: preprocessor, scoped_name,escape,character_literal,integer_literal,
    floating_pt_literal,literal,primary_expr,unary_expr,const_expr,simple_type_spec,template_type_spec,
    array_declarator,struct_def,struct_forward_dcl,const_dcl,union_forward_dcl,union_def,enum_dcl,
    simple_declarator,typedef_dcl,bitset_dcl,bitmask_dcl,maybe_annotated,struct_def,module_dcl,string_literal,include_rgx,open_idl
import IDL.Parse: Binop, Unop, Literal, ScopedName, ConstExpr, Annotated, CanAnnotate, Decl, ModuleDecl, 
    TypeSpec, ConstDecl, Declarator, UnionCaseLabel, UnionElement, BitfieldSpec, TypeDecl
@testset "Parsing" begin 
@test preprocessor("""
//hello world
me
""") == "\nme\n"
@test preprocessor("""
foo /*
test
test
*/bar
""") == "foo bar\n"
@test preprocessor("""
#include<testme>
""", (fname, preproc) -> begin match(include_rgx, fname)[1] == "testme" ? "hello" : "" end) == "hello\n"
@test preprocessor("""
#include"testme"
""", (fname, preproc) -> begin match(include_rgx, fname)[1] == "testme" ? "hello" : "" end) == "hello\n"
@test_throws "directives not supported" preprocessor("""
#other
""", (fname, preproc) -> begin match(include_rgx, fname)[1] == "testme" ? "hello" : "" end)


@test parse_whole(scoped_name, "hello") == ScopedName.Name(Symbol[], :hello, true)
@test parse_whole(scoped_name, "px4::hello") == ScopedName.Name(Symbol[:px4], :hello, true)
@test parse_whole(scoped_name, "::px4::hello") == ScopedName.Name(Symbol[:px4], :hello, false)
@test parse_whole(scoped_name, "ref::px4::hello") == ScopedName.Name(Symbol[:ref, :px4], :hello, true)
@test parse_whole(scoped_name, "ref::   px4::hello") == ScopedName.Name(Symbol[:ref, :px4], :hello, true)


@test parse_whole(escape, "\\033") == "\e"
@test parse_whole(escape, "\\03") == string(Char(0o3))
@test parse_whole(escape, "\\333") == string(Char(0o333))
@test parse_whole(escape, "\\xF3") == "ó"
@test parse_whole(escape, "\\xf3") == "ó"
@test parse_whole(escape, "\\u3BC") == "μ"
@test parse_whole(escape, "\\t") == "\t"
@test parse_whole(escape, "\\?") == "?"

@test parse_whole(character_literal, "'c'") == 'c'
@test parse_whole(character_literal, "'\\xF3'") == 'ó'
@test parse_whole(character_literal, "'\\n'") == '\n'
@test parse_whole(string_literal, "\"char\"") == "char"
@test parse_whole(string_literal, "\"hi\\xF3\"") == "hió"
@test parse_whole(string_literal, "\"hello\\n\"") == "hello\n"


@test parse_whole(integer_literal, "19") == 19
@test parse_whole(integer_literal, "012") == 0o12
@test parse_whole(integer_literal, "0x19") == 0x19



@test parse_whole(floating_pt_literal, "32.1") == 32.1f0
@test parse_whole(floating_pt_literal, "32.1f") == 32.1f0
@test parse_whole(floating_pt_literal, "32.1d") == 32.1
@test parse_whole(floating_pt_literal, ".1d") == 0.1
@test parse_whole(floating_pt_literal, ".1f") == 0.1f0
@test parse_whole(floating_pt_literal, ".1") == 0.1f0
@test parse_whole(floating_pt_literal, ".1e2d") == 0.1e2
@test parse_whole(floating_pt_literal, "3f") == 3.0f0


@test parse_whole(literal, "22") == Literal.Intg(22)
@test parse_whole(literal, "22.0") == Literal.F32(22.0f0)
@test parse_whole(literal, "22.0d") == Literal.F64(22.0f0)
@test parse_whole(literal, "'h'") == Literal.Ch('h')
@test parse_whole(literal, "\"hello\"") == Literal.St("hello")
@test parse_whole(literal, "TRUE") == Literal.Bl(true)
@test parse_whole(literal, "FALSE") == Literal.Bl(false)

@test parse_whole(primary_expr, "px4::testme") == ConstExpr.Var(ScopedName.Name([:px4], :testme, true))
@test parse_whole(primary_expr, "32") == ConstExpr.Lit(Literal.Intg(32))
@test parse_whole(primary_expr, "FALSE") == ConstExpr.Lit(Literal.Bl(false))
@test parse_whole(primary_expr, "(-32+32*2)") == ConstExpr.BinApp(Binop.Add(), ConstExpr.UnApp(Unop.Neg(), ConstExpr.Lit(Literal.Intg(32))), ConstExpr.BinApp(Binop.Mul(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(2))))

@test parse_whole(unary_expr, "-32") == ConstExpr.UnApp(Unop.Neg(), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(unary_expr, "-   32") == ConstExpr.UnApp(Unop.Neg(), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(unary_expr, "+32") == ConstExpr.UnApp(Unop.Plus(), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(unary_expr, "~FALSE") == ConstExpr.UnApp(Unop.Inv(), ConstExpr.Lit(Literal.Bl(false)))
@test parse_whole(unary_expr, "32") == ConstExpr.Lit(Literal.Intg(32))
@test parse_whole(unary_expr, "FALSE") == ConstExpr.Lit(Literal.Bl(false))

@test parse_whole(const_expr, "-32") == ConstExpr.UnApp(Unop.Neg(), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32|32") == ConstExpr.BinApp(Binop.Or(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32^32") == ConstExpr.BinApp(Binop.Xor(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32&32") == ConstExpr.BinApp(Binop.And(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32<<32") == ConstExpr.BinApp(Binop.Lshift(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32>>32") == ConstExpr.BinApp(Binop.Rshift(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32+32") == ConstExpr.BinApp(Binop.Add(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32-32") == ConstExpr.BinApp(Binop.Sub(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32*32") == ConstExpr.BinApp(Binop.Mul(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32/32") == ConstExpr.BinApp(Binop.Div(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))
@test parse_whole(const_expr, "32%32") == ConstExpr.BinApp(Binop.Mod(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(32)))

@test parse_whole(const_expr, "32+32*2") == ConstExpr.BinApp(Binop.Add(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.BinApp(Binop.Mul(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(2)))) 
@test parse_whole(const_expr, "-32+32*2") == ConstExpr.BinApp(Binop.Add(), ConstExpr.UnApp(Unop.Neg(), ConstExpr.Lit(Literal.Intg(32))), ConstExpr.BinApp(Binop.Mul(), ConstExpr.Lit(Literal.Intg(32)), ConstExpr.Lit(Literal.Intg(2))))



@test parse_whole(simple_type_spec, "float") == TypeSpec.TFloat(32)
@test parse_whole(simple_type_spec, "double") == TypeSpec.TFloat(64)
@test parse_whole(simple_type_spec, "long double") == TypeSpec.TFloat(128)
@test parse_whole(simple_type_spec, "short") == TypeSpec.TInt(16)
@test parse_whole(simple_type_spec, "long") == TypeSpec.TInt(32)
@test parse_whole(simple_type_spec, "long long") == TypeSpec.TInt(64)
@test parse_whole(simple_type_spec, "unsigned short") == TypeSpec.TUInt(16)
@test parse_whole(simple_type_spec, "unsigned long") == TypeSpec.TUInt(32)
@test parse_whole(simple_type_spec, "unsigned long long") == TypeSpec.TUInt(64)
@test parse_whole(simple_type_spec, "int16") == TypeSpec.TInt(16)
@test parse_whole(simple_type_spec, "int32") == TypeSpec.TInt(32)

@test parse_whole(simple_type_spec, "int64") == TypeSpec.TInt(64)
@test parse_whole(simple_type_spec, "uint16") == TypeSpec.TUInt(16)
@test parse_whole(simple_type_spec, "uint32") == TypeSpec.TUInt(32)
@test parse_whole(simple_type_spec, "uint32 ") == TypeSpec.TUInt(32)
@test parse_whole(simple_type_spec, "uint64") == TypeSpec.TUInt(64)
@test parse_whole(simple_type_spec, "char") == TypeSpec.TChar()
@test parse_whole(simple_type_spec, "wchar") == TypeSpec.TWChar()
@test parse_whole(simple_type_spec, "boolean") == TypeSpec.TBool()
@test parse_whole(simple_type_spec, "octet") == TypeSpec.TOctet()
@test parse_whole(simple_type_spec, "any") == TypeSpec.TAny()



@test parse_whole(template_type_spec, "sequence<int16, 12>") == TypeSpec.TSeq(TypeSpec.TInt(16), ConstExpr.Lit(Literal.Intg(12)))
@test parse_whole(template_type_spec, "sequence<int16>") == TypeSpec.TSeq(TypeSpec.TInt(16), nothing)
@test parse_whole(template_type_spec, "string<12>") == TypeSpec.TString(ConstExpr.Lit(Literal.Intg(12)))
@test parse_whole(template_type_spec, "string") == TypeSpec.TString(nothing)
@test parse_whole(template_type_spec, "wstring<12>") == TypeSpec.TWString(ConstExpr.Lit(Literal.Intg(12)))
@test parse_whole(template_type_spec, "wstring") == TypeSpec.TWString(nothing)
@test parse_whole(template_type_spec, "fixed<12,7>") == TypeSpec.TSpecifiedFixedPoint(ConstExpr.Lit(Literal.Intg(12)), ConstExpr.Lit(Literal.Intg(7)))
@test parse_whole(template_type_spec, "wstring") == TypeSpec.TWString(nothing)
@test parse_whole(template_type_spec, "map<int16, int16>") == TypeSpec.TMap(TypeSpec.TInt(16), TypeSpec.TInt(16), nothing)
@test parse_whole(template_type_spec, "map<int16, int16, 32>") == TypeSpec.TMap(TypeSpec.TInt(16), TypeSpec.TInt(16), ConstExpr.Lit(Literal.Intg(32)))



@test parse_whole(array_declarator, "hi[23]") == Declarator.DArray(:hi, [ConstExpr.Lit(Literal.Intg(23))])
@test parse_whole(array_declarator, "hi[23][32]") == Declarator.DArray(:hi, [ConstExpr.Lit(Literal.Intg(23)), ConstExpr.Lit(Literal.Intg(32))])
@test parse_whole(array_declarator, "hi[23]  [32]") == Declarator.DArray(:hi, [ConstExpr.Lit(Literal.Intg(23)), ConstExpr.Lit(Literal.Intg(32))])
@test parse_whole(array_declarator, "hi\t[23]  [32]") == Declarator.DArray(:hi, [ConstExpr.Lit(Literal.Intg(23)), ConstExpr.Lit(Literal.Intg(32))])

@test parse_whole(struct_def, "struct testme { }") == TypeDecl.StructDecl(:testme, nothing, [])
@test parse_whole(struct_def, "struct testme : super { }") == TypeDecl.StructDecl(:testme, ScopedName.Name(Symbol[], :super, true), [])
@test parse_whole(struct_def, "struct testme : super::baz { }") == TypeDecl.StructDecl(:testme, ScopedName.Name([:super], :baz, true), [])
@test parse_whole(struct_def, "struct testme \n{ \n}") == TypeDecl.StructDecl(:testme, nothing, [])
@test parse_whole(struct_def, "struct testme \n{\n\t \n}") == TypeDecl.StructDecl(:testme, nothing, [])
@test parse_whole(struct_def, "struct testme { string hi; }") == TypeDecl.StructDecl(:testme, nothing, [TypeSpec.TString(nothing) => [Declarator.DIdent(:hi)]])
@test parse_whole(struct_def, "struct testme { string hi; string world; }") == TypeDecl.StructDecl(:testme, nothing, [
    TypeSpec.TString(nothing) => [Declarator.DIdent(:hi)],
    TypeSpec.TString(nothing) => [Declarator.DIdent(:world)]
])
@test parse_whole(struct_def, "struct testme { string hi, world; }") == TypeDecl.StructDecl(:testme, nothing, [
    TypeSpec.TString(nothing) => [Declarator.DIdent(:hi), Declarator.DIdent(:world)]
])
@test parse_whole(struct_def, "struct testme { string hi,\n world; }") == TypeDecl.StructDecl(:testme, nothing, [
    TypeSpec.TString(nothing) => [Declarator.DIdent(:hi), Declarator.DIdent(:world)]
])
@test parse_whole(struct_def, "struct testme { string hi,world; }") == TypeDecl.StructDecl(:testme, nothing, [
    TypeSpec.TString(nothing) => [Declarator.DIdent(:hi), Declarator.DIdent(:world)]
])
@test parse_whole(struct_def, "struct testme { \nstring hi,world;\n }") == TypeDecl.StructDecl(:testme, nothing, [
    TypeSpec.TString(nothing) => [Declarator.DIdent(:hi), Declarator.DIdent(:world)]
])
@test parse_whole(struct_def, "struct testme { string<31> hi; }") == TypeDecl.StructDecl(:testme, nothing, [TypeSpec.TString(ConstExpr.Lit(Literal.Intg(31))) => [Declarator.DIdent(:hi)]])
@test parse_whole(struct_def, "struct testme { fixed<31, 17> hi; }") == TypeDecl.StructDecl(:testme, nothing, [TypeSpec.TSpecifiedFixedPoint(ConstExpr.Lit(Literal.Intg(31)), ConstExpr.Lit(Literal.Intg(17))) => [Declarator.DIdent(:hi)]])
@test parse_whole(struct_forward_dcl, "struct testme") == TypeDecl.StructFwdDecl(:testme)

@test parse_whole(const_dcl, """
const uint32 MESSAGE_VERSION = 0
""") == ConstDecl.CDecl(TypeSpec.TUInt(32), :MESSAGE_VERSION, ConstExpr.Lit(Literal.Intg(0)))

@test parse_whole(union_forward_dcl, "union testme") == TypeDecl.UnionFwdDecl(:testme)
@test parse_whole(union_def, "union testme switch (octet) { default: uint8 test; }") == TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [[UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))])
@test parse_whole(union_def, """
union testme switch (octet) { 
    default: uint8 test; 
}""") == TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [[UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))])
@test parse_whole(union_def, """
union testme switch (octet) { 
    case 3: string<3> me;
    default: uint8 test; 
}""") == TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [
    [UnionCaseLabel.UCLCase(ConstExpr.Lit(Literal.Intg(3)))] => UnionElement.UElem(TypeSpec.TString(ConstExpr.Lit(Literal.Intg(3))), Declarator.DIdent(:me)), 
    [UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))])
@test parse_whole(union_def, """
union testme switch (octet) { 
    case px4::non: string<3> me;
    default: uint8 test; 
}""") == TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [
    [UnionCaseLabel.UCLCase(ConstExpr.Var(ScopedName.Name([:px4], :non, true)))] => UnionElement.UElem(TypeSpec.TString(ConstExpr.Lit(Literal.Intg(3))), Declarator.DIdent(:me)), 
    [UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))])
@test parse_whole(union_def, """
union testme switch (octet) { 
    case px4::non: string<3> me;
    case px4::non: string<3> me;
    default: uint8 test; 
}""") == TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [
    [UnionCaseLabel.UCLCase(ConstExpr.Var(ScopedName.Name([:px4], :non, true)))] => UnionElement.UElem(TypeSpec.TString(ConstExpr.Lit(Literal.Intg(3))), Declarator.DIdent(:me)), 
    [UnionCaseLabel.UCLCase(ConstExpr.Var(ScopedName.Name([:px4], :non, true)))] => UnionElement.UElem(TypeSpec.TString(ConstExpr.Lit(Literal.Intg(3))), Declarator.DIdent(:me)), 
    [UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))])

    
@test parse_whole(enum_dcl, """enum testme { example }""") == TypeDecl.EnumDecl(:testme, [:example])
@test parse_whole(enum_dcl, """enum testme { example, example }""") == TypeDecl.EnumDecl(:testme, [:example, :example])
@test parse_whole(enum_dcl, """enum testme { example, example, }""") == TypeDecl.EnumDecl(:testme, [:example, :example])
@test parse_whole(enum_dcl, """enum testme { example,
example }""") == TypeDecl.EnumDecl(:testme, [:example, :example])


@test parse_whole(simple_declarator, "hi") == Declarator.DIdent(:hi) 

    
@test parse_whole(typedef_dcl, "typedef uint8 foo") == TypeDecl.TypedefDecl(TypeSpec.TUInt(8), [Declarator.DIdent(:foo)])
@test parse_whole(typedef_dcl, "typedef float float__4[4]") == TypeDecl.TypedefDecl(TypeSpec.TFloat(32), [Declarator.DArray(:float__4, [ConstExpr.Lit(Literal.Intg(4))])])
@test parse_whole(typedef_dcl, "typedef sequence<int16, 12> foo") == TypeDecl.TypedefDecl(TypeSpec.TSeq(TypeSpec.TInt(16), ConstExpr.Lit(Literal.Intg(12))), [Declarator.DIdent(:foo)])
@test parse_whole(typedef_dcl, "typedef struct testme { } foo") == TypeDecl.TypedefDecl(TypeDecl.StructDecl(:testme, nothing, []), [Declarator.DIdent(:foo)])
@test parse_whole(typedef_dcl, "typedef union testme switch (octet) { default: uint8 test; } foo") == 
    TypeDecl.TypedefDecl(TypeDecl.UnionDecl(:testme, TypeSpec.TOctet(), [[UnionCaseLabel.UCLDefault()] => UnionElement.UElem(TypeSpec.TUInt(8), Declarator.DIdent(:test))]), [Declarator.DIdent(:foo)])
@test parse_whole(typedef_dcl, "typedef enum testme { example } foo") == TypeDecl.TypedefDecl(TypeDecl.EnumDecl(:testme, [:example]), [Declarator.DIdent(:foo)])
@test parse_whole(typedef_dcl, "typedef px4::test foo") == TypeDecl.TypedefDecl(TypeSpec.TRef(ScopedName.Name([:px4], :test, true)), [Declarator.DIdent(:foo)])


@test parse_whole(bitset_dcl, "bitset foo { }") == TypeDecl.BitsetDecl(:foo, nothing, [])
@test parse_whole(bitset_dcl, "bitset foo : bar { }") == TypeDecl.BitsetDecl(:foo, ScopedName.Name(Symbol[], :bar, true), [])
@test parse_whole(bitset_dcl, "bitset foo { bitfield<3> a; }") == TypeDecl.BitsetDecl(:foo, nothing, [BitfieldSpec.BSSpec(ConstExpr.Lit(Literal.Intg(3)), nothing, [:a])])
@test parse_whole(bitset_dcl, "bitset foo { bitfield<3, short> a; }") == TypeDecl.BitsetDecl(:foo, nothing, [BitfieldSpec.BSSpec(ConstExpr.Lit(Literal.Intg(3)), TypeSpec.TInt(16), [:a])])
@test parse_whole(bitset_dcl, "bitset foo { bitfield<3> a; bitfield<3, short> a; }") == TypeDecl.BitsetDecl(:foo, nothing, [BitfieldSpec.BSSpec(ConstExpr.Lit(Literal.Intg(3)), nothing, [:a]), BitfieldSpec.BSSpec(ConstExpr.Lit(Literal.Intg(3)), TypeSpec.TInt(16), [:a])])


@test parse_whole(bitmask_dcl, "bitmask foo { flag0 }") == TypeDecl.BitmaskDecl(:foo, [:flag0])
@test parse_whole(bitmask_dcl, "bitmask foo { flag0, flag1 }") == TypeDecl.BitmaskDecl(:foo, [:flag0, :flag1])


@test parse_whole(maybe_annotated, "")("hi") == "hi"
@test parse_whole(maybe_annotated, "@foobar(baz=2)")("hi") == Annotated.Annotation(ScopedName.Name(Symbol[], :foobar, true), [:baz => ConstExpr.Lit(Literal.Intg(2))], "hi")
@test parse_whole(maybe_annotated, "@foobar(baz=2)\n@bing(bar=3)")("hi") == 
    Annotated.Annotation(ScopedName.Name(Symbol[], :foobar, true), [:baz => ConstExpr.Lit(Literal.Intg(2))], 
    Annotated.Annotation(ScopedName.Name(Symbol[], :bing, true), [:bar => ConstExpr.Lit(Literal.Intg(3))], "hi"))
@test parse_whole(maybe_annotated, "@foobar(baz=2, bar=3)")("hi") == 
    Annotated.Annotation(ScopedName.Name(Symbol[], :foobar, true), [:baz => ConstExpr.Lit(Literal.Intg(2)), :bar => ConstExpr.Lit(Literal.Intg(3))], "hi")
@test parse_whole(maybe_annotated, """@verbatim (language="comment", text= "Vehicle odometry data. Fits ROS REP 147 for aerial vehicles")""")("hi") == 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [:language => ConstExpr.Lit(Literal.St("comment")), :text => ConstExpr.Lit(Literal.St("Vehicle odometry data. Fits ROS REP 147 for aerial vehicles"))], "hi")

@test parse_whole(struct_def, """
struct VehicleOdometry {
    @verbatim (language="comment", text=
    "time since system start (microseconds)")
    uint64 timestamp;

    uint64 timestamp_sample;

    @verbatim (language="comment", text=
    "Position and orientation frame of reference")
    uint8 pose_frame;

    @verbatim (language="comment", text=
    "Position in meters. Frame of reference defined by local_frame. NaN if invalid/unknown")
    float__3 position;

    @verbatim (language="comment", text=
    "Quaternion rotation from FRD body frame to reference frame. First value NaN if invalid/unknown")
    float__4 q;

    @verbatim (language="comment", text=
    "Reference frame of the velocity data")
    uint8 velocity_frame;

    @verbatim (language="comment", text=
    "Velocity in meters/sec. Frame of reference defined by velocity_frame variable. NaN if invalid/unknown")
    float__3 velocity;

    @verbatim (language="comment", text=
    "Angular velocity in body-fixed frame (rad/s). NaN if invalid/unknown")
    float__3 angular_velocity;

    float__3 position_variance;

    float__3 orientation_variance;

    float__3 velocity_variance;

    uint8 reset_counter;

    int8 quality;
}
""") == TypeDecl.StructDecl(:VehicleOdometry, nothing, [
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("time since system start (microseconds)"))], 
    TypeSpec.TUInt(64) => [Declarator.DIdent(:timestamp)]), 
    TypeSpec.TUInt(64) => [Declarator.DIdent(:timestamp_sample)], 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Position and orientation frame of reference"))], 
    TypeSpec.TUInt(8) => [Declarator.DIdent(:pose_frame)]), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Position in meters. Frame of reference defined by local_frame. NaN if invalid/unknown"))], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:position)]), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Quaternion rotation from FRD body frame to reference frame. First value NaN if invalid/unknown"))], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__4, true)) => [Declarator.DIdent(:q)]), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Reference frame of the velocity data"))], 
    TypeSpec.TUInt(8) => [Declarator.DIdent(:velocity_frame)]), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Velocity in meters/sec. Frame of reference defined by velocity_frame variable. NaN if invalid/unknown"))], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:velocity)]), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("Angular velocity in body-fixed frame (rad/s). NaN if invalid/unknown"))], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:angular_velocity)]), 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:position_variance)], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:orientation_variance)], 
    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:velocity_variance)], 
    TypeSpec.TUInt(8) => [Declarator.DIdent(:reset_counter)], 
    TypeSpec.TInt(8) => [Declarator.DIdent(:quality)]])

@test parse_whole(module_dcl, """
module VehicleOdometry_Constants {
        const uint32 MESSAGE_VERSION = 0;
        const uint8 POSE_FRAME_UNKNOWN = 0;
        @verbatim (language="comment", text=
        "NED earth-fixed frame")
        const uint8 POSE_FRAME_NED = 1;
        @verbatim (language="comment", text=
        "FRD world-fixed frame, arbitrary heading reference")
        const uint8 POSE_FRAME_FRD = 2;
        const uint8 VELOCITY_FRAME_UNKNOWN = 0;
        @verbatim (language="comment", text=
        "NED earth-fixed frame")
        const uint8 VELOCITY_FRAME_NED = 1;
        @verbatim (language="comment", text=
        "FRD world-fixed frame, arbitrary heading reference")
        const uint8 VELOCITY_FRAME_FRD = 2;
        @verbatim (language="comment", text=
        "FRD body-fixed frame")
        const uint8 VELOCITY_FRAME_BODY_FRD = 3;
    }
""") == ModuleDecl.MDecl(:VehicleOdometry_Constants, [
    ConstDecl.CDecl(TypeSpec.TUInt(32), :MESSAGE_VERSION, ConstExpr.Lit(Literal.Intg(0))), 
    ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_UNKNOWN, ConstExpr.Lit(Literal.Intg(0))), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("NED earth-fixed frame"))], 
        ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_NED, ConstExpr.Lit(Literal.Intg(1)))), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("FRD world-fixed frame, arbitrary heading reference"))], 
        ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_FRD, ConstExpr.Lit(Literal.Intg(2)))), 
    ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_UNKNOWN, ConstExpr.Lit(Literal.Intg(0))), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("NED earth-fixed frame"))], 
        ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_NED, ConstExpr.Lit(Literal.Intg(1)))), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("FRD world-fixed frame, arbitrary heading reference"))], 
        ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_FRD, ConstExpr.Lit(Literal.Intg(2)))), 
    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
        :language => ConstExpr.Lit(Literal.St("comment")), 
        :text => ConstExpr.Lit(Literal.St("FRD body-fixed frame"))], 
    ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_BODY_FRD, ConstExpr.Lit(Literal.Intg(3))))])

end

@testset "Files" begin 
    @test open_idl("files/VehicleOdometry.idl") == [ModuleDecl.MDecl(:px4, [
        ModuleDecl.MDecl(:msg, [
            TypeDecl.TypedefDecl(TypeSpec.TFloat(32), [Declarator.DArray(:float__3, [ConstExpr.Lit(Literal.Intg(3))])]), 
            TypeDecl.TypedefDecl(TypeSpec.TFloat(32), [Declarator.DArray(:float__4, [ConstExpr.Lit(Literal.Intg(4))])]), 
            ModuleDecl.MDecl(:VehicleOdometry_Constants, [
                ConstDecl.CDecl(TypeSpec.TUInt(32), :MESSAGE_VERSION, ConstExpr.Lit(Literal.Intg(0))), 
                ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_UNKNOWN, ConstExpr.Lit(Literal.Intg(0))), 
                Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("NED earth-fixed frame"))], 
                    ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_NED, ConstExpr.Lit(Literal.Intg(1)))), 
                Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("FRD world-fixed frame, arbitrary heading reference"))], 
                    ConstDecl.CDecl(TypeSpec.TUInt(8), :POSE_FRAME_FRD, ConstExpr.Lit(Literal.Intg(2)))), 
                ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_UNKNOWN, ConstExpr.Lit(Literal.Intg(0))), 
                Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("NED earth-fixed frame"))], 
                    ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_NED, ConstExpr.Lit(Literal.Intg(1)))), 
                Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("FRD world-fixed frame, arbitrary heading reference"))], 
                    ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_FRD, ConstExpr.Lit(Literal.Intg(2)))), 
                Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("FRD body-fixed frame"))], 
                    ConstDecl.CDecl(TypeSpec.TUInt(8), :VELOCITY_FRAME_BODY_FRD, ConstExpr.Lit(Literal.Intg(3))))]),
            Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                :language => ConstExpr.Lit(Literal.St("comment")), 
                :text => ConstExpr.Lit(Literal.St("Vehicle odometry data. Fits ROS REP 147 for aerial vehicles"))], 
                TypeDecl.StructDecl(:VehicleOdometry, nothing, [
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("time since system start (microseconds)"))], 
                    TypeSpec.TUInt(64) => [Declarator.DIdent(:timestamp)]), TypeSpec.TUInt(64) => [Declarator.DIdent(:timestamp_sample)], 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Position and orientation frame of reference"))], 
                    TypeSpec.TUInt(8) => [Declarator.DIdent(:pose_frame)]), 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Position in meters. Frame of reference defined by local_frame. NaN if invalid/unknown"))], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:position)]), 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Quaternion rotation from FRD body frame to reference frame. First value NaN if invalid/unknown"))], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__4, true)) => [Declarator.DIdent(:q)]), 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Reference frame of the velocity data"))], 
                    TypeSpec.TUInt(8) => [Declarator.DIdent(:velocity_frame)]), 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Velocity in meters/sec. Frame of reference defined by velocity_frame variable. NaN if invalid/unknown"))], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:velocity)]), 
                    Annotated.Annotation(ScopedName.Name(Symbol[], :verbatim, true), [
                    :language => ConstExpr.Lit(Literal.St("comment")), 
                    :text => ConstExpr.Lit(Literal.St("Angular velocity in body-fixed frame (rad/s). NaN if invalid/unknown"))], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:angular_velocity)]), 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:position_variance)], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:orientation_variance)], 
                    TypeSpec.TRef(ScopedName.Name(Symbol[], :float__3, true)) => [Declarator.DIdent(:velocity_variance)], 
                    TypeSpec.TUInt(8) => [Declarator.DIdent(:reset_counter)], 
                    TypeSpec.TInt(8) => [Declarator.DIdent(:quality)]]))])])]
end
end