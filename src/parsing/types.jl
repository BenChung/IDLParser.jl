
using Moshi.Derive: @derive
using Moshi.Data: @data

@data Binop begin 
    Or
    Xor
    And
    Lshift
    Rshift
    Add
    Sub
    Mul
    Div
    Mod
end
@derive Binop[Eq, Hash, Show]
@data Unop begin 
    Plus # +
    Neg # -
    Inv # ~
end
@derive Unop[Eq, Hash, Show]
@data Literal begin 
    F32(Float32)
    F64(Float64)
    Intg(Int64)
    Ch(Char)
    St(String)
    Bl(Bool)
end
@derive Literal[Eq, Hash, Show]

@data ScopedName begin
    Name(Vector{Symbol}, Symbol, Bool)
end
@derive ScopedName[Eq, Hash, Show]

@data ConstExpr begin
    BinApp(Binop.Type, ConstExpr, ConstExpr)
    UnApp(Unop.Type, ConstExpr)
    Var(ScopedName.Type)
    Lit(Literal.Type)
end
@derive ConstExpr[Eq, Hash, Show]

@data Annotated{T} begin 
    Annotation(ScopedName.Type, Vector{Pair{Symbol, ConstExpr.Type}}, Union{Annotated{T}, T})
end
@derive Annotated[Eq, Hash, Show]
const CanAnnotate{T} = Union{T, Annotated.Type{<:T}}

abstract type Decl end

@data ModuleDecl <: Decl begin 
    MDecl(Symbol, Vector{CanAnnotate{Decl}})
end
@derive ModuleDecl[Eq, Hash, Show]

@data TypeSpec begin
    TRef(ScopedName.Type)
    TFloat(Int)
    TInt(Int)
    TUInt(Int)
    TChar
    TWChar
    TBool
    TOctet
    TAny
    TSeq(TypeSpec, Union{Nothing, ConstExpr.Type})
    TString(Union{Nothing, ConstExpr.Type})
    TWString(Union{Nothing, ConstExpr.Type})
    TSpecifiedFixedPoint(ConstExpr.Type, ConstExpr.Type)
    TFixedPoint
    TMap(TypeSpec, TypeSpec, Union{Nothing, ConstExpr.Type})
    TObject
    TValueBase
end
@derive TypeSpec[Eq, Hash, Show]

@data ConstDecl <: Decl begin 
    CDecl(TypeSpec.Type, Symbol, ConstExpr.Type)
end
@derive ConstDecl[Eq, Hash, Show]

@data Declarator begin
    DArray(Symbol, Vector{ConstExpr.Type})
    DIdent(Symbol)
end 
@derive Declarator[Eq, Hash, Show]

@data UnionCaseLabel begin 
    UCLCase(ConstExpr.Type)
    UCLDefault
end
@derive UnionCaseLabel[Eq, Hash, Show]

@data UnionElement begin 
    UElem(TypeSpec.Type, Declarator.Type)
end
@derive UnionElement[Eq, Hash, Show]

@data BitfieldSpec begin 
    BSSpec(ConstExpr.Type, Union{Nothing, TypeSpec.Type}, Vector{Symbol})
end
@derive BitfieldSpec[Eq, Hash, Show]

@data TypeDecl <: Decl begin
    StructDecl(Symbol, Union{Nothing, ScopedName.Type}, Vector{CanAnnotate{Pair{TypeSpec.Type, Vector{Declarator.Type}}}})
    StructFwdDecl(Symbol)
    UnionDecl(Symbol, TypeSpec.Type, Vector{Pair{Vector{UnionCaseLabel.Type}, UnionElement.Type}})
    UnionFwdDecl(Symbol)
    EnumDecl(Symbol, Vector{Symbol})
    TypedefDecl(Union{TypeSpec.Type, TypeDecl}, Vector{Declarator.Type})
    BitsetDecl(Symbol, Union{Nothing, ScopedName.Type}, Vector{BitfieldSpec.Type})
    BitmaskDecl(Symbol, Vector{Symbol})
end
@derive TypeDecl[Eq, Hash, Show]
