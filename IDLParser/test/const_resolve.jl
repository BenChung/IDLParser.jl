module ConstResolveTests
using IDLParser
using Test, PEG
using Moshi.Match: @match
import IDLParser.Parse: const_expr, type_spec, module_dcl, union_def, enum_dcl, struct_def, const_dcl
import IDLParser.ConstResolution: resolve_constants, Scope
import IDLParser.ConstResolution as CR

@testset "Const resolution" begin
empty_scope = Scope()

@test resolve_constants(parse_whole(const_expr, "32*2"), empty_scope, empty_scope) == 64
@test resolve_constants(parse_whole(const_expr, "32+32*2"), empty_scope, empty_scope) == 96
@test resolve_constants(parse_whole(const_expr, "32/2"), empty_scope, empty_scope) == 16
@test resolve_constants(parse_whole(const_expr, "32-2"), empty_scope, empty_scope) == 30
@test resolve_constants(parse_whole(const_expr, "32>>2"), empty_scope, empty_scope) == 8
@test resolve_constants(parse_whole(const_expr, "32%2"), empty_scope, empty_scope) == 0
@test resolve_constants(parse_whole(const_expr, "TRUE & TRUE"), empty_scope, empty_scope) == true
@test resolve_constants(parse_whole(const_expr, "TRUE & FALSE"), empty_scope, empty_scope) == false
@test resolve_constants(parse_whole(const_expr, "TRUE | FALSE"), empty_scope, empty_scope) == true
@test resolve_constants(parse_whole(const_expr, "FALSE | FALSE"), empty_scope, empty_scope) == false
@test resolve_constants(parse_whole(const_expr, "9 ^ 3"), empty_scope, empty_scope) == 10

@test resolve_constants(parse_whole(const_expr, "-2"), empty_scope, empty_scope) == -2
@test resolve_constants(parse_whole(const_expr, "+2"), empty_scope, empty_scope) == 2
@test resolve_constants(parse_whole(const_expr, "~TRUE"), empty_scope, empty_scope) == false

scope_with(pairs...) = Scope(Dict{Symbol, Any}(pairs...))
@test resolve_constants(parse_whole(const_expr, "hello"), scope_with(:hello => 2), empty_scope) == 2
@test resolve_constants(parse_whole(const_expr, "hello::world"), scope_with(:hello => scope_with(:world => 2)), empty_scope) == 2
@test resolve_constants(parse_whole(const_expr, "::hello"), empty_scope, scope_with(:hello => 2)) == 2
@test resolve_constants(parse_whole(const_expr, "::hello::world"), empty_scope, scope_with(:hello => scope_with(:world => 2))) == 2
@test resolve_constants(parse_whole(const_expr, "2+hello::world"), scope_with(:hello => scope_with(:world => 2)), empty_scope) == 4
@test resolve_constants(parse_whole(const_expr, "2-hello::world"), scope_with(:hello => scope_with(:world => 2)), empty_scope) == 0


@test resolve_constants(parse_whole(type_spec, "string<32*2>"), empty_scope, empty_scope) == IDLParser.ConstResolution.TypeSpec.TString(64)
@test resolve_constants(parse_whole(type_spec, "wstring<32*2>"), empty_scope, empty_scope) == IDLParser.ConstResolution.TypeSpec.TWString(64)
@test resolve_constants(parse_whole(type_spec, "sequence<int16,3+3>"), empty_scope, empty_scope) == IDLParser.ConstResolution.TypeSpec.TSeq(IDLParser.ConstResolution.TypeSpec.TInt(16), 6)
@test resolve_constants(parse_whole(type_spec, "fixed<32*2, 12>"), empty_scope, empty_scope) == IDLParser.ConstResolution.TypeSpec.TSpecifiedFixedPoint(64,12)
@test resolve_constants(parse_whole(type_spec, "map<int16, int16, 64>"), empty_scope, empty_scope) == IDLParser.ConstResolution.TypeSpec.TMap(IDLParser.ConstResolution.TypeSpec.TInt(16), IDLParser.ConstResolution.TypeSpec.TInt(16), 64)

resolve_constants(parse_whole(module_dcl, """
module M {
    const uint32 A = 3;
    const uint32 B = A;
}
"""), Scope(), Scope()) == IDLParser.ConstResolution.ModuleDecl.MDecl(:M, [
    IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :A, 3),
    IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

    resolve_constants(parse_whole(module_dcl, """
    module M {
        module D {
            const uint32 A = 3;
        };
        const uint32 B = D::A;
    }
    """), Scope(), Scope()) == IDLParser.ConstResolution.ModuleDecl.MDecl(:M, [
        IDLParser.ConstResolution.ModuleDecl.MDecl(:D, [
            IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :A, 3)]),
        IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

ctx = Scope()
resolve_constants(parse_whole(module_dcl, """
module M {
    module D {
        const uint32 A = 3;
    };
    const uint32 B = ::M::D::A;
}
"""), ctx, ctx) == IDLParser.ConstResolution.ModuleDecl.MDecl(:M, [
    IDLParser.ConstResolution.ModuleDecl.MDecl(:D, [
        IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :A, 3)]),
    IDLParser.ConstResolution.ConstDecl.CDecl(IDLParser.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

# Integer bitwise ops (regression: Or/And/Inv were lowered to logical operators)
@test resolve_constants(parse_whole(const_expr, "32|32"), empty_scope, empty_scope) == 32
@test resolve_constants(parse_whole(const_expr, "12&10"), empty_scope, empty_scope) == 8
@test resolve_constants(parse_whole(const_expr, "~0"), empty_scope, empty_scope) == ~0

# Enum resolution (regression: emitted UnionFwdDecl)
@test resolve_constants(parse_whole(enum_dcl, "enum E { a, b }"), empty_scope, empty_scope) ==
    CR.TypeDecl.EnumDecl(:E, [:a, :b])

# Struct with inheritance (regression: super was not converted to CR.ScopedName)
@test resolve_constants(parse_whole(struct_def, "struct S : Base { }"), empty_scope, empty_scope) ==
    CR.TypeDecl.StructDecl(:S, CR.ScopedName.Name(Symbol[], :Base, true), [])

# Operator precedence (regression: all binops were the same precedence)
@test resolve_constants(parse_whole(const_expr, "1 & 2 | 3"), empty_scope, empty_scope) == 3
@test resolve_constants(parse_whole(const_expr, "1 | 2 & 0"), empty_scope, empty_scope) == 1
@test resolve_constants(parse_whole(const_expr, "2 * 3 + 1"), empty_scope, empty_scope) == 7
@test resolve_constants(parse_whole(const_expr, "1 + 2 * 3"), empty_scope, empty_scope) == 7
@test resolve_constants(parse_whole(const_expr, "1 + 2 << 3"), empty_scope, empty_scope) == (1 + 2) << 3
@test resolve_constants(parse_whole(const_expr, "1 ^ 2 & 3"), empty_scope, empty_scope) == 1 ⊻ (2 & 3)

# Left-associativity (regression: const_expr was right-recursive)
@test resolve_constants(parse_whole(const_expr, "10 - 3 - 2"), empty_scope, empty_scope) == 5
@test resolve_constants(parse_whole(const_expr, "100 % 30 % 7"), empty_scope, empty_scope) == 3
@test resolve_constants(parse_whole(const_expr, "32 >> 1 >> 1"), empty_scope, empty_scope) == 8

# Integer division (regression: `7/2` returned 3.5::Float64)
let v = resolve_constants(parse_whole(const_expr, "7 / 2"), empty_scope, empty_scope)
    @test v === 3
end
# Float operands still produce Float results
@test resolve_constants(parse_whole(const_expr, "7.0 / 2"), empty_scope, empty_scope) === 3.5f0

# 64-bit hex literals no longer overflow
@test resolve_constants(parse_whole(const_expr, "0xFFFFFFFFFFFFFFFF"), empty_scope, empty_scope) == -1
@test resolve_constants(parse_whole(const_expr, "0x8000000000000000"), empty_scope, empty_scope) == typemin(Int64)

# The decimal uint64 max parses without throwing (mirrors the hex path:
# Int64-overflowing values reinterpret through UInt64).
@test parse_whole(IDLParser.Parse.integer_literal, "18446744073709551615") == -1
@test reinterpret(UInt64, parse_whole(IDLParser.Parse.integer_literal, "18446744073709551615")) == typemax(UInt64)

# A const binds at the width its declared type names. A float literal defaults
# to Float32 at parse time, so without coercion a `double` const would bind a
# Float32; the declared type must win.
cval(src) = @match resolve_constants(parse_whole(const_dcl, src), empty_scope, empty_scope) begin
    CR.ConstDecl.CDecl(_, _, v) => v
end
@test cval("const double D = 1234.5678") isa Float64
# Integer-valued doubles bind losslessly at full Float64 width.
@test cval("const double E = 1234.0") === 1234.0
# A `float` const stays Float32.
@test cval("const float F = 1.5") === 1.5f0
@test cval("const float G = 1.5f") === 1.5f0
# A `double` written with the `d` suffix is lossless to the last digit.
@test cval("const double H = 1234.5678d") === 1234.5678

# Enclosing-scope lookup walks outward through parent modules
let resolved = resolve_constants(parse_whole(module_dcl, """
        module M {
            const uint32 X = 5;
            module Inner {
                const uint32 Y = X;
                const uint32 Z = X * 2;
            };
        }
        """), Scope(), Scope())
    inner_decls = @match resolved begin
        CR.ModuleDecl.MDecl(_, ds) => @match ds[2] begin
            CR.ModuleDecl.MDecl(_, inner_ds) => inner_ds
        end
    end
    y_val = @match inner_decls[1] begin
        CR.ConstDecl.CDecl(_, _, v) => v
    end
    z_val = @match inner_decls[2] begin
        CR.ConstDecl.CDecl(_, _, v) => v
    end
    @test y_val == 5
    @test z_val == 10
end

# Union case label resolution (regression: emitted UnionElement.UCLCase/UCLDefault, used undefined `cases`)
@test resolve_constants(parse_whole(union_def, """
union U switch (octet) {
    case 1: uint8 a;
    default: uint8 b;
}"""), empty_scope, empty_scope) == CR.TypeDecl.UnionDecl(:U, CR.TypeSpec.TOctet(), [
        [CR.UnionCaseLabel.UCLCase(1)] => CR.UnionElement.UElem(CR.TypeSpec.TUInt(8), CR.Declarator.DIdent(:a)),
        [CR.UnionCaseLabel.UCLDefault()] => CR.UnionElement.UElem(CR.TypeSpec.TUInt(8), CR.Declarator.DIdent(:b))])

end
end