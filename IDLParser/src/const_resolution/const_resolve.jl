# Lowers parsed IDL declarations: evaluates const expressions and converts
# expression-shaped type/length fields into resolved values.
using Moshi.Derive: @derive
using Moshi.Data: @data
using Moshi.Match: @match
using ..Parse: Parse

# A lexical scope with a parent pointer so unqualified lookups walk outward
# (per OMG IDL v4 name lookup rules). Each module declaration creates a child
# scope; resolved constants are stored in `bindings`, and nested modules are
# stored as child Scope values for qualified `M::X` lookups.
mutable struct Scope
    bindings::Dict{Symbol, Any}
    parent::Union{Scope, Nothing}
end
Scope() = Scope(Dict{Symbol, Any}(), nothing)
Scope(parent::Union{Scope, Nothing}) = Scope(Dict{Symbol, Any}(), parent)
Scope(bindings::Dict{Symbol, Any}) = Scope(bindings, nothing)

apply_op(op::Parse.Binop.Type, lhs, rhs) = @match op begin
        Parse.Binop.Or() => lhs | rhs
        Parse.Binop.Xor() => lhs ⊻ rhs
        Parse.Binop.And() => lhs & rhs
        Parse.Binop.Lshift() => lhs << rhs
        Parse.Binop.Rshift() => lhs >> rhs
        Parse.Binop.Add() => lhs + rhs
        Parse.Binop.Sub() => lhs - rhs
        Parse.Binop.Mul() => lhs * rhs
        Parse.Binop.Div() => (lhs isa Integer && rhs isa Integer) ? div(lhs, rhs) : lhs / rhs
        Parse.Binop.Mod() => lhs % rhs
    end


apply_op(op::Parse.Unop.Type, val) = @match op begin
        Parse.Unop.Plus() => val
        Parse.Unop.Neg() => -val
        Parse.Unop.Inv() => ~val
    end

lit_to_julia(v) = @match v begin
        Parse.Literal.F32(v) => v
        Parse.Literal.F64(v) => v
        Parse.Literal.Intg(v) => v
        Parse.Literal.Ch(v) => v
        Parse.Literal.St(v) => v
        Parse.Literal.Bl(v) => v
    end

# Walks the parent chain looking for `name` in any enclosing scope.
function lookup_enclosing(scope::Scope, name::Symbol)
    if haskey(scope.bindings, name)
        return scope.bindings[name]
    elseif scope.parent !== nothing
        return lookup_enclosing(scope.parent, name)
    else
        error("$name not available in IDL scope")
    end
end

# For `M::N::X`: the first path component is found via enclosing-scope lookup,
# then each subsequent component must be a direct child of the previous Scope.
function resolve_name(path::Vector{Symbol}, v::Symbol, scope::Scope)
    if isempty(path)
        return lookup_enclosing(scope, v)
    end
    sub = lookup_enclosing(scope, path[1])
    for n in path[2:end]
        sub isa Scope || error("$(path[1]) is not a module/scope")
        haskey(sub.bindings, n) || error("$n not in scope")
        sub = sub.bindings[n]
    end
    sub isa Scope || error("$(isempty(path[2:end]) ? path[1] : path[end]) is not a module/scope")
    haskey(sub.bindings, v) || error("$v not in scope")
    return sub.bindings[v]
end

resolve_name(v::Parse.ScopedName.Type, local_scope::Scope, file_scope::Scope) = @match v begin
    Parse.ScopedName.Name(path, name, true) => resolve_name(path, name, local_scope)
    Parse.ScopedName.Name(path, name, false) => resolve_name(path, name, file_scope)
end
convert_name(v::Parse.ScopedName.Type) = @match v begin
    Parse.ScopedName.Name(path, name, islocal) => ScopedName.Name(path, name, islocal)
end

resolve_constants(expr::Parse.ConstExpr.Type, local_scope::Scope, file_scope::Scope) = @match expr begin
        Parse.ConstExpr.BinApp(op, l, r) => apply_op(op,
            resolve_constants(l, local_scope, file_scope),
            resolve_constants(r, local_scope, file_scope))
        Parse.ConstExpr.UnApp(op, v) => apply_op(op,
            resolve_constants(v, local_scope, file_scope))
        Parse.ConstExpr.Var(v) => resolve_name(v, local_scope, file_scope)
        Parse.ConstExpr.Lit(v) => lit_to_julia(v)
    end

resolve_constants(definition::Parse.Annotated.Type, local_scope::Scope, file_scope::Scope) = @match definition begin
        Parse.Annotated.Annotation(name, params, defn) => begin
            new_params = Pair{Symbol, Any}[pname => resolve_constants(pvalue, local_scope, file_scope) for (pname, pvalue) in params]
            return Annotated.Annotation(convert_name(name), new_params, resolve_constants(defn, local_scope, file_scope))
        end
    end

resolve_constants(definition::Parse.ModuleDecl.Type, local_scope::Scope, file_scope::Scope) = @match definition begin
    Parse.ModuleDecl.MDecl(name, decls) => begin
        new_scope = Scope(local_scope)
        local_scope.bindings[name] = new_scope
        new_decls = [resolve_constants(decl, new_scope, file_scope) for decl in decls]
        return ModuleDecl.MDecl(name, new_decls)
    end
end

# Bind a float const at the width its declared type names: a suffix-less literal
# parses as Float32, so a `const double` must be widened to Float64 to match its
# 8-byte wire field. Integer widths come from the literal/expression result
# (their range is already guarded at parse time); non-`Real` values
# (string/char/bool/enum ref) pass through.
_coerce_const(::Parse.TypeSpec.Type, val) = val
_coerce_const(typ::Parse.TypeSpec.Type, val::Real) = @match typ begin
    Parse.TypeSpec.TFloat(16) => Float16(val)
    Parse.TypeSpec.TFloat(32) => Float32(val)
    Parse.TypeSpec.TFloat(64) => Float64(val)
    _ => val
end

resolve_constants(definition::Parse.ConstDecl.Type, local_scope::Scope, file_scope::Scope) = @match definition begin
    Parse.ConstDecl.CDecl(typ, name, val) => begin
        # Coerce while `typ` is still a `Parse.TypeSpec`: the float-width match keys on `Parse.TypeSpec.TFloat`.
        val = _coerce_const(typ, resolve_constants(val, local_scope, file_scope))
        typ = resolve_constants(typ, local_scope, file_scope)
        local_scope.bindings[name] = val
        return ConstDecl.CDecl(typ, name, val)
    end
end

require_positive_int(val::Nothing) = val # unbounded length (no upper bound)
require_positive_int(val::Integer) = val < 0 ? error("$val must be a non-negative integer") : val
require_positive_int(val) = error("$val must be a non-negative integer")

resolve_constants(::Nothing, local_scope::Scope, file_scope::Scope) = nothing
resolve_constants(definition::Parse.TypeSpec.Type, local_scope::Scope, file_scope::Scope) = @match definition begin
    Parse.TypeSpec.TRef(name) => TypeSpec.TRef(convert_name(name))
    Parse.TypeSpec.TFloat(v) => TypeSpec.TFloat(v)
    Parse.TypeSpec.TInt(v) => TypeSpec.TInt(v)
    Parse.TypeSpec.TUInt(v) => TypeSpec.TUInt(v)
    Parse.TypeSpec.TChar() => TypeSpec.TChar()
    Parse.TypeSpec.TWChar() => TypeSpec.TWChar()
    Parse.TypeSpec.TBool() => TypeSpec.TBool()
    Parse.TypeSpec.TOctet() => TypeSpec.TOctet()
    Parse.TypeSpec.TAny() => TypeSpec.TAny()
    Parse.TypeSpec.TSeq(ts, val) => TypeSpec.TSeq(
        resolve_constants(ts, local_scope, file_scope),
        require_positive_int(resolve_constants(val, local_scope, file_scope)))
    Parse.TypeSpec.TString(len) => TypeSpec.TString(
        require_positive_int(resolve_constants(len, local_scope, file_scope)))
    Parse.TypeSpec.TWString(len) =>
        TypeSpec.TWString(require_positive_int(resolve_constants(len, local_scope, file_scope)))
    Parse.TypeSpec.TSpecifiedFixedPoint(a, b) => TypeSpec.TSpecifiedFixedPoint(
        require_positive_int(resolve_constants(a, local_scope, file_scope)),
        require_positive_int(resolve_constants(b, local_scope, file_scope)))
    Parse.TypeSpec.TFixedPoint() => TypeSpec.TFixedPoint()
    Parse.TypeSpec.TMap(a, b, len) => TypeSpec.TMap(
        resolve_constants(a, local_scope, file_scope),
        resolve_constants(b, local_scope, file_scope),
        require_positive_int(resolve_constants(len, local_scope, file_scope)))
    Parse.TypeSpec.TObject() => TypeSpec.TObject()
    Parse.TypeSpec.TValueBase() => TypeSpec.TValueBase()
    _ => error("Unhandled case $(definition)")
end
resolve_constants((ts, decls)::Pair{Parse.TypeSpec.Type, Vector{Parse.Declarator.Type}}, local_scope::Scope, file_scope::Scope) =
    resolve_constants(ts, local_scope, file_scope) => [resolve_constants(decl, local_scope, file_scope) for decl in decls]


resolve_constants(decl::Parse.Declarator.Type, local_scope::Scope, file_scope::Scope) = @match decl begin
    Parse.Declarator.DArray(name, dims) => Declarator.DArray(name, resolve_constants.(dims, (local_scope, ), (file_scope, )))
    Parse.Declarator.DIdent(s) => Declarator.DIdent(s)
end
resolve_constants(uc::Parse.UnionCaseLabel.Type, local_scope::Scope, file_scope::Scope) = @match uc begin
    Parse.UnionCaseLabel.UCLCase(ce) => UnionCaseLabel.UCLCase(resolve_constants(ce, local_scope, file_scope))
    Parse.UnionCaseLabel.UCLDefault() => UnionCaseLabel.UCLDefault()
end
resolve_constants(uc::Parse.UnionElement.Type, local_scope::Scope, file_scope::Scope) = @match uc begin
    Parse.UnionElement.UElem(ts, decl) => UnionElement.UElem(resolve_constants(ts, local_scope, file_scope), resolve_constants(decl, local_scope, file_scope))
end
resolve_constants(uc::Parse.BitfieldSpec.Type, local_scope::Scope, file_scope::Scope) = @match uc begin
    Parse.BitfieldSpec.BSSpec(ce, ts, vs) => BitfieldSpec.BSSpec(resolve_constants(ce, local_scope, file_scope), resolve_constants(ts, local_scope, file_scope), vs)
end
resolve_constants((uc, elem)::Pair{Vector{Parse.UnionCaseLabel.Type}, Parse.UnionElement.Type}, local_scope::Scope, file_scope::Scope) =
    [resolve_constants(case, local_scope, file_scope) for case in uc] => resolve_constants(elem, local_scope, file_scope)

convert_super(::Nothing) = nothing
convert_super(super::Parse.ScopedName.Type) = convert_name(super)

resolve_constants(definition::Parse.TypeDecl.Type, local_scope::Scope, file_scope::Scope) = @match definition begin
    Parse.TypeDecl.StructDecl(name, super, decls) => TypeDecl.StructDecl(name, convert_super(super), let ls = Scope(local_scope); [resolve_constants(decl, ls, file_scope) for decl in decls] end)
    Parse.TypeDecl.StructFwdDecl(name) => TypeDecl.StructFwdDecl(name)
    Parse.TypeDecl.UnionDecl(name, disc, cases) =>
        TypeDecl.UnionDecl(name, resolve_constants(disc, local_scope, file_scope), [resolve_constants(case, local_scope, file_scope) for case in cases])
    Parse.TypeDecl.UnionFwdDecl(name) => TypeDecl.UnionFwdDecl(name)
    Parse.TypeDecl.EnumDecl(name, cases) => TypeDecl.EnumDecl(name, cases)
    Parse.TypeDecl.TypedefDecl(def, decls) => TypeDecl.TypedefDecl(resolve_constants(def, local_scope, file_scope), [resolve_constants(decl, local_scope, file_scope) for decl in decls])
    Parse.TypeDecl.BitsetDecl(name, super, bfs) => TypeDecl.BitsetDecl(name, convert_super(super), resolve_constants.(bfs, (local_scope, ), (file_scope, )))
    Parse.TypeDecl.BitmaskDecl(name, cases) => TypeDecl.BitmaskDecl(name, cases)
end

function resolve_constants(definitions::Vector{<:Parse.CanAnnotate{Parse.Decl}})
    file_scope = Scope()
    return [resolve_constants(def, file_scope, file_scope) for def in definitions]
end
