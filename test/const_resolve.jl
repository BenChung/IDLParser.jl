module ConstResolveTests
using IDL
using Test, PEG
import IDL.Parse: const_expr, type_spec, module_dcl
import IDL.ConstResolution: resolve_constants

@testset "Const resolution" begin
empty_scope = Dict{Symbol, Any}()

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

@test resolve_constants(parse_whole(const_expr, "hello"), Dict{Symbol, Any}(:hello => 2), empty_scope) == 2
@test resolve_constants(parse_whole(const_expr, "hello::world"), Dict{Symbol, Any}(:hello => Dict{Symbol, Any}(:world => 2)), empty_scope) == 2
@test resolve_constants(parse_whole(const_expr, "::hello"), empty_scope, Dict{Symbol, Any}(:hello => 2)) == 2
@test resolve_constants(parse_whole(const_expr, "::hello::world"), empty_scope, Dict{Symbol, Any}(:hello => Dict{Symbol, Any}(:world => 2))) == 2
@test resolve_constants(parse_whole(const_expr, "2+hello::world"), Dict{Symbol, Any}(:hello => Dict{Symbol, Any}(:world => 2)), empty_scope) == 4
@test resolve_constants(parse_whole(const_expr, "2-hello::world"), Dict{Symbol, Any}(:hello => Dict{Symbol, Any}(:world => 2)), empty_scope) == 0


@test resolve_constants(parse_whole(type_spec, "string<32*2>"), empty_scope, empty_scope) == IDL.ConstResolution.TypeSpec.TString(64)
@test resolve_constants(parse_whole(type_spec, "wstring<32*2>"), empty_scope, empty_scope) == IDL.ConstResolution.TypeSpec.TWString(64)
@test resolve_constants(parse_whole(type_spec, "sequence<int16,3+3>"), empty_scope, empty_scope) == IDL.ConstResolution.TypeSpec.TSeq(IDL.ConstResolution.TypeSpec.TInt(16), 6)
@test resolve_constants(parse_whole(type_spec, "fixed<32*2, 12>"), empty_scope, empty_scope) == IDL.ConstResolution.TypeSpec.TSpecifiedFixedPoint(64,12)
@test resolve_constants(parse_whole(type_spec, "map<int16, int16, 64>"), empty_scope, empty_scope) == IDL.ConstResolution.TypeSpec.TMap(IDL.ConstResolution.TypeSpec.TInt(16), IDL.ConstResolution.TypeSpec.TInt(16), 64)

resolve_constants(parse_whole(module_dcl, """
module M {
    const uint32 A = 3;
    const uint32 B = A;
}
"""), Dict{Symbol, Any}(), Dict{Symbol, Any}()) == IDL.ConstResolution.ModuleDecl.MDecl(:M, [
    IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :A, 3), 
    IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

    resolve_constants(parse_whole(module_dcl, """
    module M {
        module D {
            const uint32 A = 3;
        };
        const uint32 B = D::A;
    }
    """), Dict{Symbol, Any}(), Dict{Symbol, Any}()) == IDL.ConstResolution.ModuleDecl.MDecl(:M, [
        IDL.ConstResolution.ModuleDecl.MDecl(:D, [
            IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :A, 3)]), 
        IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

ctx = Dict{Symbol, Any}()
resolve_constants(parse_whole(module_dcl, """
module M {
    module D {
        const uint32 A = 3;
    };
    const uint32 B = ::M::D::A;
}
"""), ctx, ctx) == IDL.ConstResolution.ModuleDecl.MDecl(:M, [
    IDL.ConstResolution.ModuleDecl.MDecl(:D, [
        IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :A, 3)]), 
    IDL.ConstResolution.ConstDecl.CDecl(IDL.ConstResolution.TypeSpec.TUInt(32), :B, 3)])

end
end