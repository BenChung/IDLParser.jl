
using Moshi.Derive: @derive
using Moshi.Data: @data

const IDLValue = Any

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
@data Annotated{T} begin 
    Annotation(ScopedName.Type, Vector{Pair{Symbol, IDLValue}}, Union{Annotated{T}, T})
end
@derive Annotated[Eq, Hash, Show]
const CanAnnotate{T} = Union{T, Annotated.Type{<:T}}

abstract type Decl end

@data ModuleDecl <: Decl begin 
    MDecl(Symbol, Vector{CanAnnotate{Decl}})
end
@derive ModuleDecl[Eq, Hash, Show]

@data TypeSpec begin
    TFloat(Int)
    TInt(Int)
    TUInt(Int)
    TChar
    TWChar
    TBool
    TOctet
    TAny
    TSeq(TypeSpec, Union{Nothing, Int})
    TString(Union{Nothing, Int})
    TWString(Union{Nothing, Int})
    TSpecifiedFixedPoint(Int, Int)
    TFixedPoint
    TMap(TypeSpec, TypeSpec, Union{Nothing, IDLValue})
    TObject
    TValueBase
end
@derive TypeSpec[Eq, Hash, Show]

@data ConstDecl <: Decl begin 
    CDecl(TypeSpec.Type, Symbol, IDLValue)
end
@derive ConstDecl[Eq, Hash, Show]

@data Declarator begin
    DArray(Symbol, Vector{Int})
    DIdent(Symbol)
end 
@derive Declarator[Eq, Hash, Show]

@data UnionCaseLabel begin 
    UCLCase(IDLValue)
    UCLDefault
end
@derive UnionCaseLabel[Eq, Hash, Show]

@data UnionElement begin 
    UElem(TypeSpec.Type, Declarator.Type)
end
@derive UnionElement[Eq, Hash, Show]

@data BitfieldSpec begin 
    BSSpec(IDLValue, Union{Nothing, TypeSpec.Type}, Vector{Symbol})
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
