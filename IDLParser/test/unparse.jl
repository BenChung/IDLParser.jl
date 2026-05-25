module UnparseTests
using IDLParser
using Test, PEG
import IDLParser.Parse: literal, scoped_name, const_expr, array_declarator, simple_declarator,
    maybe_annotated, character_literal, string_literal, unparse,
    type_spec, struct_def, struct_forward_dcl, union_def, union_forward_dcl, enum_dcl,
    typedef_dcl, bitset_dcl, bitmask_dcl, const_dcl, module_dcl, specification,
    open_idl, preprocessor, parse_whole
import IDLParser.Parse: Binop, Unop, Literal, ScopedName, ConstExpr, Annotated, Declarator,
    TypeSpec, TypeDecl, ConstDecl, ModuleDecl, UnionCaseLabel, UnionElement, BitfieldSpec

@testset "Unparse" begin

@testset "Literal round-trip" begin
    @test parse_whole(literal, unparse(Literal.Intg(42))) == Literal.Intg(42)
    @test parse_whole(literal, unparse(Literal.F32(3.14f0))) == Literal.F32(3.14f0)
    @test parse_whole(literal, unparse(Literal.F32(1.5f10))) == Literal.F32(1.5f10)
    @test parse_whole(literal, unparse(Literal.F64(3.14))) == Literal.F64(3.14)
    @test parse_whole(literal, unparse(Literal.F64(1.5e10))) == Literal.F64(1.5e10)
    @test parse_whole(literal, unparse(Literal.Bl(true))) == Literal.Bl(true)
    @test parse_whole(literal, unparse(Literal.Bl(false))) == Literal.Bl(false)

    s = Literal.St("hello \"world\"\n\u3BC")
    @test parse_whole(literal, unparse(s)) == s

    c = Literal.Ch('\n')
    @test parse_whole(literal, unparse(c)) == c
    @test parse_whole(literal, unparse(Literal.Ch('\''))) == Literal.Ch('\'')
    @test parse_whole(literal, unparse(Literal.Ch('a'))) == Literal.Ch('a')
    @test parse_whole(literal, unparse(Literal.Ch('\u3BC'))) == Literal.Ch('\u3BC')

    # Confirm the F32/F64 suffixes are present so dispatch into the right variant
    @test endswith(unparse(Literal.F32(3.14f0)), "f")
    @test endswith(unparse(Literal.F64(3.14)), "d")
end

@testset "ScopedName round-trip" begin
    n1 = ScopedName.Name([:px4, :msg], :Foo, true)
    @test parse_whole(scoped_name, unparse(n1)) == n1

    n2 = ScopedName.Name([:px4], :Bar, false)
    @test parse_whole(scoped_name, unparse(n2)) == n2
    @test startswith(unparse(n2), "::")

    n3 = ScopedName.Name(Symbol[], :Foo, true)
    @test parse_whole(scoped_name, unparse(n3)) == n3
    @test unparse(n3) == "Foo"

    n4 = ScopedName.Name(Symbol[], :Foo, false)
    @test parse_whole(scoped_name, unparse(n4)) == n4
    @test unparse(n4) == "::Foo"
end

@testset "ConstExpr round-trip" begin
    e_var = ConstExpr.Var(ScopedName.Name(Symbol[], :foo, true))
    @test parse_whole(const_expr, unparse(e_var)) == e_var

    e_lit = ConstExpr.Lit(Literal.Intg(7))
    @test parse_whole(const_expr, unparse(e_lit)) == e_lit

    # Left-associative chain
    e_chain = parse_whole(const_expr, "8 - 3 - 2")
    @test parse_whole(const_expr, unparse(e_chain)) == e_chain

    # Mixed precedence not over-parenthesized
    e_mixed = parse_whole(const_expr, "1 + 2 * 3")
    out_mixed = unparse(e_mixed)
    @test parse_whole(const_expr, out_mixed) == e_mixed
    @test !occursin("(", out_mixed)

    # Parens required around lower-precedence left child
    e_lhs = parse_whole(const_expr, "(1 + 2) * 3")
    @test parse_whole(const_expr, unparse(e_lhs)) == e_lhs

    # Parens required around equal-precedence right child (left-associativity)
    e_sub = parse_whole(const_expr, "8 - (3 - 2)")
    @test parse_whole(const_expr, unparse(e_sub)) == e_sub
    @test occursin("(", unparse(e_sub))

    e_div = parse_whole(const_expr, "8 / (4 / 2)")
    @test parse_whole(const_expr, unparse(e_div)) == e_div

    # UnApp inside BinApp
    e_unbin = parse_whole(const_expr, "-1 + 2")
    @test parse_whole(const_expr, unparse(e_unbin)) == e_unbin

    # UnApp around BinApp needs parens
    e_unparen = ConstExpr.UnApp(Unop.Neg(),
        ConstExpr.BinApp(Binop.Add(),
            ConstExpr.Lit(Literal.Intg(1)),
            ConstExpr.Lit(Literal.Intg(2))))
    @test parse_whole(const_expr, unparse(e_unparen)) == e_unparen

    # All binop variants round-trip
    for opstr in ("|", "^", "&", "<<", ">>", "+", "-", "*", "/", "%")
        e = parse_whole(const_expr, "5 $opstr 3")
        @test parse_whole(const_expr, unparse(e)) == e
    end
end

@testset "Declarator round-trip" begin
    d1 = Declarator.DIdent(:foo)
    @test parse_whole(simple_declarator, unparse(d1)) == d1

    d2 = Declarator.DArray(:x, [ConstExpr.Lit(Literal.Intg(3)), ConstExpr.Lit(Literal.Intg(5))])
    @test parse_whole(array_declarator, unparse(d2)) == d2
end

# Strip the trailing " hi" subject from an unparsed annotation so we can re-parse
# the annotation prefix with `maybe_annotated`, which returns a wrapper function.
_strip_subject(out::AbstractString) = (@assert endswith(out, " hi"); out[1:end-3])

@testset "Annotated round-trip" begin
    # Named-arg
    ann_named = parse_whole(maybe_annotated, "@foo(bar=2)")("hi")
    out_named = unparse(ann_named)
    @test parse_whole(maybe_annotated, _strip_subject(out_named))("hi") == ann_named

    # Positional: no `value=` in output
    ann_pos = parse_whole(maybe_annotated, "@key(42)")("hi")
    out_pos = unparse(ann_pos)
    @test !occursin("value=", out_pos)
    @test parse_whole(maybe_annotated, _strip_subject(out_pos))("hi") == ann_pos

    # No-arg: no parens
    ann_marker = parse_whole(maybe_annotated, "@marker")("hi")
    out_marker = unparse(ann_marker)
    @test !occursin("(", out_marker)
    @test parse_whole(maybe_annotated, _strip_subject(out_marker))("hi") == ann_marker

    # Multiple named args
    ann_multi = parse_whole(maybe_annotated, "@foo(a=1, b=2)")("hi")
    out_multi = unparse(ann_multi)
    @test parse_whole(maybe_annotated, _strip_subject(out_multi))("hi") == ann_multi

    # Chained annotations
    ann_chain = parse_whole(maybe_annotated, "@a @b(c=1)")("hi")
    out_chain = unparse(ann_chain)
    @test parse_whole(maybe_annotated, _strip_subject(out_chain))("hi") == ann_chain
end

@testset "TypeSpec round-trip" begin
    for src in ("float", "double", "long double",
                "int8", "int16", "int32", "int64",
                "uint8", "uint16", "uint32", "uint64",
                "char", "wchar", "boolean", "octet", "any", "Object", "ValueBase",
                "sequence<int16>", "sequence<int16, 12>",
                "string", "string<64>", "wstring", "wstring<32>",
                "fixed", "fixed<10, 2>",
                "map<int16, int32>", "map<int16, int32, 32>",
                "px4::msg::Foo")
        ts = parse_whole(type_spec, src)
        @test parse_whole(type_spec, unparse(ts)) == ts
    end

    nested = parse_whole(type_spec, "sequence<sequence<int16, 4>, 8>")
    @test parse_whole(type_spec, unparse(nested)) == nested
end

@testset "TypeDecl: StructDecl round-trip" begin
    s_empty = parse_whole(struct_def, "struct E {}")
    @test parse_whole(struct_def, unparse(s_empty)) == s_empty

    s_simple = parse_whole(struct_def, "struct S { int32 a; float b; }")
    @test parse_whole(struct_def, unparse(s_simple)) == s_simple

    s_super = parse_whole(struct_def, "struct C : px4::Base { uint8 x; }")
    @test parse_whole(struct_def, unparse(s_super)) == s_super

    s_multi_decl = parse_whole(struct_def, "struct M { int32 a, b, c; }")
    @test parse_whole(struct_def, unparse(s_multi_decl)) == s_multi_decl

    s_ann = parse_whole(struct_def, "struct A { @key uint8 x; @verbatim(language=\"comment\", text=\"hi\") int32 y; }")
    @test parse_whole(struct_def, unparse(s_ann)) == s_ann

    s_arr = parse_whole(struct_def, "struct AR { float xs[3]; }")
    @test parse_whole(struct_def, unparse(s_arr)) == s_arr
end

@testset "TypeDecl: StructFwdDecl round-trip" begin
    fwd = parse_whole(struct_forward_dcl, "struct Fwd")
    @test parse_whole(struct_forward_dcl, unparse(fwd)) == fwd
end

@testset "TypeDecl: UnionDecl round-trip" begin
    u = parse_whole(union_def, "union U switch (int32) { case 1: int32 a; case 2: case 3: float b; default: uint8 c; }")
    @test parse_whole(union_def, unparse(u)) == u

    u_fwd = parse_whole(union_forward_dcl, "union Fwd")
    @test parse_whole(union_forward_dcl, unparse(u_fwd)) == u_fwd
end

@testset "TypeDecl: EnumDecl round-trip" begin
    e = parse_whole(enum_dcl, "enum Color { RED, GREEN, BLUE }")
    @test parse_whole(enum_dcl, unparse(e)) == e
    @test !endswith(unparse(e), ",}")
end

@testset "TypeDecl: TypedefDecl round-trip" begin
    t1 = parse_whole(typedef_dcl, "typedef int32 MyInt")
    @test parse_whole(typedef_dcl, unparse(t1)) == t1

    t_multi = parse_whole(typedef_dcl, "typedef float Vec3[3], Vec4[4]")
    @test parse_whole(typedef_dcl, unparse(t_multi)) == t_multi

    t_seq = parse_whole(typedef_dcl, "typedef sequence<int16, 12> Ints")
    @test parse_whole(typedef_dcl, unparse(t_seq)) == t_seq

    t_nested = parse_whole(typedef_dcl, "typedef struct X { int32 a; } alias")
    @test parse_whole(typedef_dcl, unparse(t_nested)) == t_nested
end

@testset "TypeDecl: BitsetDecl round-trip" begin
    b = parse_whole(bitset_dcl, "bitset B { bitfield<3> a b; bitfield<5, uint8> c; }")
    @test parse_whole(bitset_dcl, unparse(b)) == b

    b_super = parse_whole(bitset_dcl, "bitset C : Base { bitfield<2> x; }")
    @test parse_whole(bitset_dcl, unparse(b_super)) == b_super
end

@testset "TypeDecl: BitmaskDecl round-trip" begin
    bm = parse_whole(bitmask_dcl, "bitmask Flags { READ, WRITE, EXEC }")
    @test parse_whole(bitmask_dcl, unparse(bm)) == bm
end

@testset "ConstDecl round-trip" begin
    c1 = parse_whole(const_dcl, "const uint32 X = 1 + 2 * 3")
    @test parse_whole(const_dcl, unparse(c1)) == c1

    c2 = parse_whole(const_dcl, "const string<32> NAME = \"hello\"")
    @test parse_whole(const_dcl, unparse(c2)) == c2
end

@testset "ModuleDecl round-trip" begin
    m = parse_whole(module_dcl, """
        module outer {
          module inner {
            const uint8 X = 1;
          };
          struct S { int32 a; };
        }""")
    @test parse_whole(module_dcl, unparse(m)) == m
end

@testset "VehicleOdometry.idl integration round-trip" begin
    path = joinpath(@__DIR__, "files/VehicleOdometry.idl")
    original = open_idl(path)
    unparsed = sprint(io -> unparse(io, original))
    reparsed = parse_whole(specification, strip(preprocessor(unparsed)))
    @test reparsed == original
end

end # @testset Unparse
end # module
