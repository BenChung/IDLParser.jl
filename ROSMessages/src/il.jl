# Intermediate language for ROS2 interfaces. Sits between the textual
# `.msg`/`.srv`/`.action` form and IDLParser's `Parse.Decl` IDL AST, so the
# two translations (text↔IL and IL↔IDL) stay independent and round-trippable.
#
# The IL deliberately models ROS concepts directly (a message is constants +
# fields, a field is a typed name with an optional default) rather than IDL
# ones (modules, structs, annotations). `ros2.jl` owns the IL↔IDL lowering and
# lifting; this file owns the IL data types and IL→text unparsing.

module IL

using Moshi.Data: @data
using Moshi.Derive: @derive
using Moshi.Match: @match

# A ROS base (element) type — what precedes any `[...]` array suffix. `RChar`
# and `RByte` are kept distinct from the integer types even though ROS treats
# them as 8-bit ints, so text round-trips preserve the original spelling.
@data RBase begin
    RBool
    RByte
    RChar
    RInt(Int)    # bit width: 8/16/32/64
    RUInt(Int)
    RFloat(Int)  # 32/64
    RStr(Union{Nothing, Int})   # nothing = unbounded; Int = upper bound
    RWStr(Union{Nothing, Int})
    # package = nothing for a same-package (relative) reference, else the
    # owning package name. `time`/`duration`/`Header` aliases lift to refs.
    RRef(Union{Nothing, String}, String)
end
@derive RBase[Eq, Hash, Show]

# The array modifier on a field's type.
@data ArraySpec begin
    AScalar             # no suffix
    AStatic(Int)        # [N]
    ABounded(Int)       # [<=N]
    AUnbounded          # []
end
@derive ArraySpec[Eq, Hash, Show]

# A constant or default value. ROS only permits literals; `VRaw` carries an
# array default verbatim (e.g. `[1, 2, 3]`) since neither the IL nor IDL has a
# structured array-literal form.
@data RValue begin
    VBool(Bool)
    VInt(Int64)
    VFloat(Float64)
    VString(String)
    VRaw(String)
end
@derive RValue[Eq, Hash, Show]

# A fully-specified field/constant type: base plus array modifier.
struct RType
    base::RBase.Type
    array::ArraySpec.Type
end

struct RField
    type::RType
    name::Symbol
    default::Union{Nothing, RValue.Type}
end

# Constants never carry an array suffix in valid ROS2, so the type is a bare
# `RBase` rather than an `RType`.
struct RConstant
    type::RBase.Type
    name::Symbol
    value::RValue.Type
end

struct RMessage
    name::Symbol
    constants::Vector{RConstant}
    fields::Vector{RField}
end

# Services and actions hold their sections as `RMessage`s whose `name`s are the
# fully-suffixed struct names (`Foo_Request`, `Foo_Goal`, …) so the IL→IDL
# lowering can treat each section uniformly.
struct RService
    name::Symbol
    request::RMessage
    response::RMessage
end

struct RAction
    name::Symbol
    goal::RMessage
    result::RMessage
    feedback::RMessage
end

# ---- Referenced types (for transitive resolution) --------------------------

"""
    referenced_refs(x) -> Vector{Tuple{Union{Nothing, String}, String}}

The nested-type references an IL interface (`RMessage`/`RService`/`RAction`) makes:
`(package, name)` pairs, with `package === nothing` for a same-package (relative)
reference. Primitive and string fields contribute nothing. A field's *element* base
carries the ref (arrays/sequences flatten to `RType{base, ArraySpec}`), so walking
`RField`s covers `T`, `T[]`, and `T[N]` alike. ROSNode's `@ros_import` uses this to
resolve a type's transitive definition closure to generate.
"""
function referenced_refs(fields::Vector{RField})
    out = Tuple{Union{Nothing, String}, String}[]
    for f in fields
        @match f.type.base begin
            RBase.RRef(pkg, name) => push!(out, (pkg, name))
            _ => nothing
        end
    end
    return out
end
referenced_refs(m::RMessage) = referenced_refs(m.fields)
referenced_refs(s::RService) = vcat(referenced_refs(s.request), referenced_refs(s.response))
referenced_refs(a::RAction)  =
    vcat(referenced_refs(a.goal), referenced_refs(a.result), referenced_refs(a.feedback))

# ---- Unparsing IL → ROS interface text -------------------------------------

unparse(node) = sprint(io -> unparse(io, node))

# Escape a string value for emission inside double quotes; the textual parser
# (`_string_lit`) understands these escapes.
function _escape_string(s::AbstractString)
    io = IOBuffer()
    for c in s
        if c == '\\'
            print(io, "\\\\")
        elseif c == '"'
            print(io, "\\\"")
        elseif c == '\n'
            print(io, "\\n")
        elseif c == '\t'
            print(io, "\\t")
        elseif c == '\r'
            print(io, "\\r")
        else
            print(io, c)
        end
    end
    return String(take!(io))
end

unparse(io::IO, b::RBase.Type) = @match b begin
    RBase.RBool() => print(io, "bool")
    RBase.RByte() => print(io, "byte")
    RBase.RChar() => print(io, "char")
    RBase.RInt(w) => print(io, "int", w)
    RBase.RUInt(w) => print(io, "uint", w)
    RBase.RFloat(w) => print(io, "float", w)
    RBase.RStr(b) => begin
        print(io, "string")
        b === nothing || print(io, "<=", b)
    end
    RBase.RWStr(b) => begin
        print(io, "wstring")
        b === nothing || print(io, "<=", b)
    end
    RBase.RRef(pkg, name) => begin
        pkg === nothing || print(io, pkg, "/")
        print(io, name)
    end
end

unparse(io::IO, a::ArraySpec.Type) = @match a begin
    ArraySpec.AScalar() => nothing
    ArraySpec.AStatic(n) => print(io, "[", n, "]")
    ArraySpec.ABounded(n) => print(io, "[<=", n, "]")
    ArraySpec.AUnbounded() => print(io, "[]")
end

function unparse(io::IO, t::RType)
    unparse(io, t.base)
    unparse(io, t.array)
end

# Floats print with Julia's default `Float64` formatting, which always carries
# a `.` or exponent — so they re-parse as floats, not ints.
unparse(io::IO, v::RValue.Type) = @match v begin
    RValue.VBool(b) => print(io, b ? "true" : "false")
    RValue.VInt(n) => print(io, n)
    RValue.VFloat(x) => print(io, x)
    RValue.VString(s) => print(io, '"', _escape_string(s), '"')
    RValue.VRaw(s) => print(io, s)
end

function unparse(io::IO, c::RConstant)
    unparse(io, c.type)
    print(io, ' ', c.name, '=')
    unparse(io, c.value)
end

function unparse(io::IO, f::RField)
    unparse(io, f.type)
    print(io, ' ', f.name)
    if f.default !== nothing
        print(io, ' ')
        unparse(io, f.default)
    end
end

# Constants first (matching the usual ROS layout), then fields, one per line.
function unparse(io::IO, m::RMessage)
    first = true
    for c in m.constants
        first || print(io, '\n')
        first = false
        unparse(io, c)
    end
    for f in m.fields
        first || print(io, '\n')
        first = false
        unparse(io, f)
    end
end

function unparse(io::IO, s::RService)
    unparse(io, s.request)
    print(io, "\n---\n")
    unparse(io, s.response)
end

function unparse(io::IO, a::RAction)
    unparse(io, a.goal)
    print(io, "\n---\n")
    unparse(io, a.result)
    print(io, "\n---\n")
    unparse(io, a.feedback)
end

end # module IL
