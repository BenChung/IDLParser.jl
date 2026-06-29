# Mapper IL — the normalized form both the Julia DSL and TOML lower to
# (DESIGN-MAPPING.md §12.2). Plain abstract+struct sum types (no Moshi dependency).

# ── 12.2.1 Structural IL: FieldShape ──────────────────────────────────────────
"""A field's expected primitive family (a pattern over a ROS `RBase`)."""
@enum ScalarKind SBool SInt SUInt SFloat SReal SString SChar SAny

abstract type Kind end                         # a field's expected kind

"""`T[]`/`T[<=N]` → `Vector`, or `T[N]` → fixed `SArray`."""
abstract type ArrayBound end
struct ASeq <: ArrayBound end
struct AFixed <: ArrayBound; n::Int; end

struct KScalar <: Kind; sk::ScalarKind; end

# FieldReq/FieldShape are mutually recursive with KNested; Kind is abstract so the
# forward reference in FieldReq.kind is fine.
struct FieldReq
    name::Symbol
    kind::Kind
    optional::Bool
end
FieldReq(name::Symbol, kind::Kind; optional::Bool=false) = FieldReq(name, kind, optional)

struct FieldShape
    fields::Vector{FieldReq}                   # names unique; order irrelevant for matching
    allow_extra::Bool
end
FieldShape(fields::Vector{FieldReq}; allow_extra::Bool=true) = FieldShape(fields, allow_extra)

struct KArray <: Kind; elem::Kind; bound::ArrayBound; end   # elem ∈ {KScalar, KNested}
struct KNested <: Kind; shape::FieldShape; end

# ── 12.2.2 Emission IL: Source + ArchetypeIL ──────────────────────────────────
const Path = Vector{Symbol}                    # field chain [:position, :x]; element-relative under `each`
const Lit  = Union{Bool, Int64, Float64, String}

abstract type Source end
struct SrcPath  <: Source; path::Path; end
struct SrcTuple <: Source; parts::Vector{Source}; end       # fixed-arity group, e.g. a point
struct SrcList  <: Source; items::Vector{Source}; end       # variable list of sources
struct SrcLit   <: Source; value::Lit; end

abstract type Rotation end
struct RQuat      <: Rotation; src::Source; end             # (x,y,z,w)
struct RAxisAngle <: Rotation; src::Source; end             # (x,y,z,angle)
struct RMat3x3    <: Rotation; src::Source; end             # 9 values, row-major

abstract type PointSet end
struct PFixed <: PointSet; points::Vector{Source}; end                  # explicit point tuples
struct PEach  <: PointSet; array::Path; point::Source; end              # iterate `array`; relative point

abstract type ArchetypeIL end
struct EScalars     <: ArchetypeIL; value::Source; end                  # one scalar real
struct EScalarGroup <: ArchetypeIL; series::Vector{Pair{Symbol,Source}}; end  # the `scalars` form
struct ETextLog     <: ArchetypeIL; text::Source; level::Union{Source,Nothing}; end
struct EPoints2D    <: ArchetypeIL; positions::PointSet; end
struct EPoints3D    <: ArchetypeIL; positions::PointSet; end
struct ETransform3D <: ArchetypeIL
    translation::Union{Source,Nothing}
    rotation::Union{Rotation,Nothing}
    scale::Union{Source,Nothing}
end

# ── 12.2.3 Selection IL: Matcher + Glob ───────────────────────────────────────
# A '/'-segmented glob: '*' matches within a segment, '**' spans segments. Carries a
# precompiled regex for matching plus literal-length / wildcard-count for specificity.
struct Glob
    pattern::String
    re::Regex
    litlen::Int      # total literal (non-wildcard) characters — higher = more specific
    nwild::Int       # number of wildcards — fewer = more specific
end

abstract type Matcher end
struct NameMatch   <: Matcher; glob::Glob; end  # over ROS type name "pkg/msg/Name"
struct TopicMatch  <: Matcher; glob::Glob; end  # over ROS topic name "/a/b"
struct StructMatch <: Matcher; shape::FieldShape; end

# ── 12.2.4 Body, entry, manifest ──────────────────────────────────────────────
abstract type FnRef end
struct FByName  <: FnRef; qualname::String; end     # "Mod.func"
struct FByValue <: FnRef; f; end                     # a callable (Julia DSL)
struct FInline  <: FnRef; src::Expr; end             # parsed Julia source (TOML [mappers.julia])

abstract type Body end
struct BFn   <: Body; ref::FnRef; end
struct BSpec <: Body
    emit::ArchetypeIL
    subpath::String
    also_structural::Bool
end
BSpec(emit::ArchetypeIL; subpath::AbstractString="", also_structural::Bool=false) =
    BSpec(emit, String(subpath), also_structural)

struct MapperEntry
    matcher::Matcher
    body::Body
    requires::Union{FieldShape,Nothing}
    desc::String
end

struct MapperManifest
    entries::Vector{MapperEntry}
end
