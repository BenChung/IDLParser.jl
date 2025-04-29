# resolves const value_base_type
using Moshi.Derive: @derive
using Moshi.Data: @data
using Moshi.Match: @match
using ..Parse: Parse


apply_op(op::Parse.Binop.Type, lhs, rhs) = @match op begin 
        Parse.Binop.Or() => lhs || rhs
        Parse.Binop.Xor() => lhs ⊻ rhs
        Parse.Binop.And() => lhs && rhs
        Parse.Binop.Lshift() => lhs << rhs
        Parse.Binop.Rshift() => lhs >> rhs
        Parse.Binop.Add() => lhs + rhs
        Parse.Binop.Sub() => lhs - rhs
        Parse.Binop.Mul() => lhs * rhs
        Parse.Binop.Div() => lhs / rhs
        Parse.Binop.Mod() => lhs % rhs
    end


apply_op(op::Parse.Unop.Type, val) = @match op begin 
        Parse.Unop.Plus() => val # ????
        Parse.Unop.Neg() => -val
        Parse.Unop.Inv() => !val
    end

lit_to_julia(v) = @match v begin # not sure this is actually a good idea?
        Parse.Literal.F32(v) => v
        Parse.Literal.F64(v) => v
        Parse.Literal.Intg(v) => v
        Parse.Literal.Ch(v) => v
        Parse.Literal.St(v) => v
        Parse.Literal.Bl(v) => v
    end

function resolve_name(path::Vector{Symbol}, v::Symbol, scope::Dict{Symbol, Any})
    if isempty(path)
        if !(v ∈ keys(scope))
            throw("$v not available in IDL scope")
        end
        return scope[v]
    else
        if !(path[1] ∈ keys(scope))
            throw("$(path[1]) not in scope")
        end
        return resolve_name(path[2:end], v, scope[path[1]])
    end
end

resolve_name(v::Parse.ScopedName.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match v begin 
    Parse.ScopedName.Name(path, name, true) => resolve_name(path, name, local_scope)
    Parse.ScopedName.Name(path, name, false) => resolve_name(path, name, file_scope)
end
convert_name(v::Parse.ScopedName.Type) = @match v begin 
    Parse.ScopedName.Name(path, name, islocal) => ScopedName.Name(path, name, islocal)
end

resolve_constants(expr::Parse.ConstExpr.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match expr begin 
        Parse.ConstExpr.BinApp(op, l, r) => apply_op(op, # todo: typecheck op
            resolve_constants(l, local_scope, file_scope), 
            resolve_constants(r, local_scope, file_scope))
        Parse.ConstExpr.UnApp(op, v) => apply_op(op, 
            resolve_constants(v, local_scope, file_scope))
        Parse.ConstExpr.Var(v) => resolve_name(v, local_scope, file_scope)
        Parse.ConstExpr.Lit(v) => lit_to_julia(v)
    end

resolve_constants(definition::Parse.Annotated.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match definition begin 
        Parse.Annotated.Annotation(name, params, defn) => begin 
            new_params = Pair{Symbol, Any}[name => resolve_constants(value, local_scope, file_scope) for (name, value) in params]
            return Annotated.Annotation(convert_name(name), new_params, resolve_constants(defn, local_scope, file_scope))
        end
    end

resolve_constants(definition::Parse.ModuleDecl.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match definition begin 
    Parse.ModuleDecl.MDecl(name, decls) => begin
        new_local_scope = Dict{Symbol, Any}()
        local_scope[name] = new_local_scope
        new_decls = [resolve_constants(decl, new_local_scope, file_scope) for decl in decls]
        return ModuleDecl.MDecl(name, new_decls)
    end
end

resolve_constants(definition::Parse.ConstDecl.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match definition begin 
    Parse.ConstDecl.CDecl(typ, name, val) => begin
        typ = resolve_constants(typ, local_scope, file_scope)
        val = resolve_constants(val, local_scope, file_scope)
        local_scope[name] = val
        return ConstDecl.CDecl(typ, name, val)
    end
end

require_positive_int(val::Nothing) = val # allow nothings
require_positive_int(val::Int) = if val < 0 throw("$val must be a positive integer!") else val end
require_positive_int(val) = throw("$val must be a positive integer!")

resolve_constants(::Nothing, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = nothing
resolve_constants(definition::Parse.TypeSpec.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match definition begin 
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
    _ => throw("Unhandled case $(definition)")
end
resolve_constants((ts, decls)::Pair{Parse.TypeSpec.Type, Vector{Parse.Declarator.Type}}, local_scope, file_scope) = 
    resolve_constants(ts, local_scope, file_scope) => [resolve_constants(decl, local_scope, file_scope) for decl in decls]

    
resolve_constants(decl::Parse.Declarator.Type, local_scope, file_scope) = @match decl begin 
    Parse.Declarator.DArray(name, dims) => Declarator.DArray(name, resolve_constants.(dims, (local_scope, ), (file_scope, )))
    Parse.Declarator.DIdent(s) => Declarator.DIdent(s)
end
resolve_constants(uc::Parse.UnionCaseLabel.Type, local_scope, file_scope) = @match uc begin 
    Parse.UnionCaseLabel.UCLCase(ce) => UnionElement.UCLCase(resolve_constants(ce, local_scope, file_scope))
    Parse.UnionCaseLabel.UCLDefault() => UnionElement.UCLDefault()
end
resolve_constants(uc::Parse.UnionElement.Type, local_scope, file_scope) = @match uc begin 
    Parse.UnionElement.UElem(ts, decl) => UnionElement.UElem(resolve_constants(ts, local_scope, file_scope), resolve_constants(decl, local_scope, file_scope))
end
resolve_constants(uc::Parse.BitfieldSpec.Type, local_scope, file_scope) = @match uc begin 
    Parse.BitfieldSpec.BSSpec(ce, ts, vs) => BitfieldSpec.BSSpec(resolve_constants(ce, local_scope, file_scope), resolve_constants(ts, local_scope, file_scope), vs)
end
resolve_constants((uc, elem)::Pair{Vector{UnionCaseLabel.Type}, UnionElement.Type}, local_scope, file_scope) = 
    [resolve_constants(case, local_scope, file_scope) for case in cases] => resolve_constants(elem, local_scope, file_scope)

resolve_constants(definition::Parse.TypeDecl.Type, local_scope::Dict{Symbol, Any}, file_scope::Dict{Symbol, Any}) = @match definition begin 
    Parse.TypeDecl.StructDecl(name, super, decls) => TypeDecl.StructDecl(name, super, let ls = Dict{Symbol, Any}(); [resolve_constants(decl, ls, file_scope) for decl in decls] end)
    Parse.TypeDecl.StructFwdDecl(name) => TypeDecl.StructFwdDecl(name)
    Parse.TypeDecl.UnionDecl(name, disc, cases) => 
        TypeDecl.UnionDecl(name, resolve_constants(disc, local_scope, file_scope), [resolve_constants(case, local_scope, file_scope) for case in cases])
    Parse.TypeDecl.UnionFwdDecl(name) => TypeDecl.UnionFwdDecl(name)
    Parse.TypeDecl.EnumDecl(name, cases) => TypeDecl.UnionFwdDecl(name, cases)
    Parse.TypeDecl.TypedefDecl(def, decls) => TypeDecl.TypedefDecl(resolve_constants(def, local_scope, file_scope), [resolve_constants(decl, local_scope, file_scope) for decl in decls])
    Parse.TypeDecl.BitsetDecl(name, super, bfs) => TypeDecl.BitsetDecl(name, super, resolve_constants.(bfs, (local_scope, ), (file_scope, )))
    Parse.TypeDecl.BitmaskDecl(name, cases) => TypeDecl.BitmaskDecl(name, cases)
end

function resolve_constants(definitions::Vector{<:Parse.CanAnnotate{Parse.Decl}})
    file_scope = Dict{Symbol, Any}()
    return [resolve_constants(def, file_scope, file_scope) for def in definitions]
end