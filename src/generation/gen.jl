import IDLParser.ConstResolution as CR
import IDLParser.Parse as Parse
using OrderedCollections
using Moshi.Match: @match

generate_code(definition::CR.Annotated.Type, genmod) = @match definition begin 
    CR.Annotated.Annotation(name, params, defn) => generate_code(defn, genmod) # ignore them for now
end
generate_code(definition::CR.ModuleDecl.Type, genmod) = @match definition begin 
    CR.ModuleDecl.MDecl(name, decls) => begin
        mod = get!(genmod, name, LittleDict{Symbol, Any}())
        for decl in decls 
            generate_code(decl, mod)
        end
    end
end
generate_code(definition::CR.ConstDecl.Type, genmod) = @match definition begin 
    CR.ConstDecl.CDecl(typ, name, val) => begin
        genmod[name] = :(const $name = $val)
    end
end

resolve_type(typ::CR.TypeSpec.Type) = @match typ begin 
    CR.TypeSpec.TRef(CR.ScopedName.Name([], name, true)) => name
    CR.TypeSpec.TRef(CR.ScopedName.Name(path, name, true)) => foldl((a,n)->:($a.$n), [path; name])
    CR.TypeSpec.TRef(CR.ScopedName.Name(path, name, false)) => throw("Global type references not supported")
    CR.TypeSpec.TFloat(16) => Float16
    CR.TypeSpec.TFloat(32) => Float32
    CR.TypeSpec.TFloat(64) => Float64
    CR.TypeSpec.TInt(8) => Int8
    CR.TypeSpec.TInt(16) => Int16
    CR.TypeSpec.TInt(32) => Int32
    CR.TypeSpec.TInt(64) => Int64
    CR.TypeSpec.TUInt(8) => UInt8
    CR.TypeSpec.TUInt(16) => UInt16
    CR.TypeSpec.TUInt(32) => UInt32
    CR.TypeSpec.TUInt(64) => UInt64
    CR.TypeSpec.TChar() => Char
    CR.TypeSpec.TWChar() => Cwchar_t
    CR.TypeSpec.TBool() => Bool
    CR.TypeSpec.TOctet() => UInt8
    CR.TypeSpec.TString(nothing) => String
    _ => throw("unsupported type $typ")

end

resolve_declarator(decl::CR.Declarator.Type, jltype) = @match decl begin 
    CR.Declarator.DArray(name, dims) => (name, :(StaticArrays.SArray{Tuple{$(dims...),}, $jltype, $(length(dims)), $(prod(dims))}))
    CR.Declarator.DIdent(name) => (name, jltype)
end
function generate_struct_field(ts::CR.TypeSpec.Type, decls::CR.Declarator.Type, f, s, des)
    jltype = resolve_type(ts)
    (name, jltype) = resolve_declarator(decls, jltype)
    push!(f, :($name::$jltype))
    push!(s, :(write(dst, o.$name)))
    push!(des, Expr(:kw, name, :(read(rdr, $jltype))))
end
function generate_struct_field((ts, decls)::Pair{CR.TypeSpec.Type, Vector{CR.Declarator.Type}}, f, s, des)
    for decl in decls 
        generate_struct_field(ts, decl, f, s, des)
    end
end
generate_struct_field(d::CR.Annotated.Type, f, s, des) = @match d begin 
    CR.Annotated.Annotation(name, params, defn) => generate_struct_field(defn, f, s, des) # ignore them for now
end
generate_struct(definition::CR.TypeDecl.Type, genmod) = @match definition begin 
    CR.TypeDecl.StructDecl(name, nothing, decls) => begin
        fields = []
        serializer_body = []
        deserializer_body = []
        for decl in decls 
            generate_struct_field(decl, fields, serializer_body, deserializer_body)
        end
        genmod[name] = quote 
            @kwdef struct $name
                $(fields...)
            end
            Base.read(rdr::CDRSerialization.CDRReader, ::Type{$name}) = $name(; $(deserializer_body...))
            Base.write(dst::CDRSerialization.CDRWriter, o::$name) = begin $(serializer_body...) end
        end
    end
end
generate_typedef(definition::CR.TypeDecl.Type, genmod) = @match definition begin 
    CR.TypeDecl.TypedefDecl(ts::CR.TypeSpec.Type, declarators) => begin 
        jltype = resolve_type(ts)
        for decl in declarators 
            (name, jltype) = resolve_declarator(decl, jltype)
            genmod[name] = :(const $name = $jltype)
        end
    end
    _ => throw("Anonymous typedefs not supported")
end

generate_code(definition::CR.TypeDecl.Type, genmod) = @match definition begin 
    CR.TypeDecl.StructDecl(name, nothing, decls) => generate_struct(definition, genmod)
    CR.TypeDecl.StructFwdDecl(name) => throw("forward declarations not supported")
    CR.TypeDecl.UnionDecl(name, disc, cases) => throw("unions not supported")
    CR.TypeDecl.UnionFwdDecl(name) => throw("forward declarations not supported")
    CR.TypeDecl.EnumDecl(name, cases) => throw("enums not supported")
    CR.TypeDecl.TypedefDecl(def, decls) => generate_typedef(definition, genmod)
    CR.TypeDecl.BitsetDecl(name, super, bfs) => throw("bitsets not supported")
    CR.TypeDecl.BitmaskDecl(name, cases) => throw("bitmasks not supported")
end

function build_modules(name, mod_dict)
    out = []
    push!(out, :(import StaticArrays, CDRSerialization))
    for (k,v) in mod_dict
        if v isa AbstractDict
            push!(out, build_modules(k, v))
        else 
            push!(out, v)
        end
    end
    return :(module $name $(out...) end)
end
function generate_code(definitions::Vector{<:CR.CanAnnotate{CR.Decl}})
    generated_modules = LittleDict{Symbol, Any}()
    for def in definitions
        generate_code(def, generated_modules) 
    end
    out = []
    for (k, v) in generated_modules
        if v isa AbstractDict
            push!(out, build_modules(k, v))
        else 
            push!(out, v)
        end
    end
    return out
end