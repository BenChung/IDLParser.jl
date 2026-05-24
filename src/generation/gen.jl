import IDLParser.ConstResolution as CR
import IDLParser.Parse as Parse
using OrderedCollections
using Moshi.Match: @match

# A pending declaration. The Expr is built lazily by `render(enclosing)` so
# that type references resolve into their canonical form for the scope they
# end up in — a `pkg/Foo` ref from a file in `pkg` becomes a bare `Foo`,
# while a `other_pkg/Foo` ref becomes `other_pkg.msg.Foo` plus a sibling
# `import ...other_pkg`. The raw `refs` (path, name pairs) are used for
# topo-sorting and import collection without needing to inspect Exprs.
struct ModEntry
    render::Function   # (enclosing::Union{Symbol, Nothing}) -> Expr
    refs::Vector{Pair{Vector{Symbol}, Symbol}}
end

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
        # Const RHS is already a resolved Julia literal — no further refs.
        genmod[name] = ModEntry(_ -> :(const $name = $val), Pair{Vector{Symbol}, Symbol}[])
    end
end

# Resolve a TypeSpec into the Julia expression to splice into the generated
# code. `enclosing` is the package being generated; when a TRef's first path
# segment matches it, we emit the bare name (the canonical form inside the
# emitted `module msg`). Cross-package refs keep their full `pkg.ns.Name`
# path — `pkg` is brought into scope by the `import ...pkg` we emit at the
# top of the module.
function resolve_type(typ::CR.TypeSpec.Type; enclosing::Union{Symbol, Nothing}=nothing)
    @match typ begin
        CR.TypeSpec.TRef(CR.ScopedName.Name([], name, true)) => name
        CR.TypeSpec.TRef(CR.ScopedName.Name(path, name, true)) =>
            (enclosing !== nothing && path[1] == enclosing) ?
                name :
                foldl((a,n)->:($a.$n), [path; name])
        CR.TypeSpec.TRef(CR.ScopedName.Name(path, name, false)) => error("Global type references not supported")
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
        # Bounded/unbounded strings share a wire format; the bound is documentation.
        # `_` (wildcard) avoids the Moshi gotcha where a bare `nothing` in a pattern
        # would be treated as a variable binding and match anything.
        CR.TypeSpec.TString(_) => String
        CR.TypeSpec.TWString(_) => String
        CR.TypeSpec.TSeq(elt, _) => :(Vector{$(resolve_type(elt; enclosing=enclosing))})
        _ => error("unsupported type $typ")
    end
end

resolve_declarator(decl::CR.Declarator.Type, jltype) = @match decl begin
    CR.Declarator.DArray(name, dims) => (name, :(StaticArrays.SArray{Tuple{$(dims...),}, $jltype, $(length(dims)), $(prod(dims))}))
    CR.Declarator.DIdent(name) => (name, jltype)
end

# Recursive walk of a TypeSpec, accumulating every TRef as a (path, name)
# pair. Resolution of "same-module vs cross-package" happens later in
# `build_modules`, where the enclosing package is known.
function _collect_refs_ts!(refs::Vector{Pair{Vector{Symbol}, Symbol}},
                            ts::CR.TypeSpec.Type)
    @match ts begin
        CR.TypeSpec.TRef(CR.ScopedName.Name(path, n, is_local)) =>
            is_local && push!(refs, copy(path) => n)
        CR.TypeSpec.TSeq(elt, _) => _collect_refs_ts!(refs, elt)
        _ => nothing
    end
end

# Collect refs from a struct-member declarator list. Unwraps any leading
# Annotated wrappers (e.g. `@default(...)`), then peeks at the type half of
# each `Pair{TypeSpec, [Declarator]}`.
function _collect_refs_members(decls)
    refs = Pair{Vector{Symbol}, Symbol}[]
    for d in decls
        node = d
        while node isa CR.Annotated.Type
            node = @match node begin
                CR.Annotated.Annotation(_, _, inner) => inner
            end
        end
        if node isa Pair
            _collect_refs_ts!(refs, node.first)
        end
    end
    return unique!(refs)
end

# Given the enclosing package symbol (or nothing if we're outside any package
# context), partition a refs list into bare same-module names and the set of
# sibling-package names that need `import ...pkg` statements. A ref of the
# form `pkg/Foo` where `pkg == enclosing` is treated as same-module — it is
# semantically identical to a bare `Foo`.
function _classify_refs(refs::Vector{Pair{Vector{Symbol}, Symbol}},
                        enclosing::Union{Symbol, Nothing})
    same_mod = Symbol[]
    cross_pkg = Symbol[]
    for (path, n) in refs
        if isempty(path)
            push!(same_mod, n)
        elseif enclosing !== nothing && path[1] == enclosing
            push!(same_mod, n)
        else
            push!(cross_pkg, path[1])
        end
    end
    return (unique!(sort!(same_mod)), unique!(sort!(cross_pkg)))
end
function generate_struct_field(ts::CR.TypeSpec.Type, decls::CR.Declarator.Type, f, s, des;
                                enclosing::Union{Symbol, Nothing}=nothing)
    # Sequences need an explicit length-prefix + element loop on the wire;
    # CDRSerialization only has a generic `read(::Vector{T})` for primitive `T`.
    # Always emit the loop for uniformity.
    seq_elt = @match ts begin
        CR.TypeSpec.TSeq(elt, _) => elt
        _ => nothing
    end
    if seq_elt !== nothing
        name = @match decls begin
            CR.Declarator.DIdent(n) => n
            _ => error("static array of sequence not supported")
        end
        elt_jltype = resolve_type(seq_elt; enclosing=enclosing)
        push!(f, :($name::Vector{$elt_jltype}))
        push!(s, :(CDRSerialization.sequenceLength(dst, length(o.$name))))
        push!(s, :(for elem in o.$name; write(dst, elem); end))
        push!(des, Expr(:kw, name, :(
            let n = CDRSerialization.sequenceLength(rdr)
                $elt_jltype[read(rdr, $elt_jltype) for _ in 1:n]
            end)))
        return
    end
    jltype = resolve_type(ts; enclosing=enclosing)
    (name, jltype) = resolve_declarator(decls, jltype)
    push!(f, :($name::$jltype))
    push!(s, :(write(dst, o.$name)))
    push!(des, Expr(:kw, name, :(read(rdr, $jltype))))
end
function generate_struct_field((ts, decls)::Pair{CR.TypeSpec.Type, Vector{CR.Declarator.Type}}, f, s, des;
                                enclosing::Union{Symbol, Nothing}=nothing)
    for decl in decls
        generate_struct_field(ts, decl, f, s, des; enclosing=enclosing)
    end
end
generate_struct_field(d::CR.Annotated.Type, f, s, des;
                       enclosing::Union{Symbol, Nothing}=nothing) = @match d begin
    CR.Annotated.Annotation(name, params, defn) =>
        generate_struct_field(defn, f, s, des; enclosing=enclosing) # ignore the annotation itself
end
function _render_struct(name::Symbol, decls, enclosing::Union{Symbol, Nothing})
    fields = []
    serializer_body = []
    deserializer_body = []
    for decl in decls
        generate_struct_field(decl, fields, serializer_body, deserializer_body;
                              enclosing=enclosing)
    end
    # Julia's default `==` only deep-compares struct fields when every field
    # is a bits type; once a field is a String or Vector, equality falls
    # back to `===`. Emit an explicit field-wise `==` so generated message
    # types behave as plain data — which they are.
    field_names = [f.args[1] for f in fields]
    eq_body = isempty(field_names) ? true :
        foldl((acc, n) -> :($acc && a.$n == b.$n),
              field_names[2:end];
              init = :(a.$(field_names[1]) == b.$(field_names[1])))
    return quote
        @kwdef struct $name
            $(fields...)
        end
        Base.:(==)(a::$name, b::$name) = $eq_body
        Base.read(rdr::CDRSerialization.CDRReader, ::Type{$name}) = $name(; $(deserializer_body...))
        Base.write(dst::CDRSerialization.CDRWriter, o::$name) = begin $(serializer_body...) end
    end
end

generate_struct(definition::CR.TypeDecl.Type, genmod) = @match definition begin
    CR.TypeDecl.StructDecl(name, nothing, decls) => begin
        refs = _collect_refs_members(decls)
        genmod[name] = ModEntry(enc -> _render_struct(name, decls, enc), refs)
    end
end
_declarator_name(decl::CR.Declarator.Type) = @match decl begin
    CR.Declarator.DArray(n, _) => n
    CR.Declarator.DIdent(n)    => n
end

generate_typedef(definition::CR.TypeDecl.Type, genmod) = @match definition begin
    CR.TypeDecl.TypedefDecl(ts::CR.TypeSpec.Type, declarators) => begin
        refs = Pair{Vector{Symbol}, Symbol}[]
        _collect_refs_ts!(refs, ts)
        unique!(refs)
        for decl in declarators
            # `decl` and `ts` are captured by the closure so the alias-type
            # expression is rebuilt with the right enclosing-pkg context.
            sym = _declarator_name(decl)
            genmod[sym] = ModEntry(
                enc -> begin
                    base_jltype = resolve_type(ts; enclosing=enc)
                    (n, decl_jltype) = resolve_declarator(decl, base_jltype)
                    :(const $n = $decl_jltype)
                end,
                refs)
        end
    end
    _ => error("Anonymous typedefs not supported")
end

generate_code(definition::CR.TypeDecl.Type, genmod) = @match definition begin
    CR.TypeDecl.StructDecl(name, nothing, decls) => generate_struct(definition, genmod)
    CR.TypeDecl.StructDecl(name, super, decls) => error("struct inheritance not supported")
    CR.TypeDecl.StructFwdDecl(name) => error("forward declarations not supported")
    CR.TypeDecl.UnionDecl(name, disc, cases) => error("unions not supported")
    CR.TypeDecl.UnionFwdDecl(name) => error("forward declarations not supported")
    CR.TypeDecl.EnumDecl(name, cases) => error("enums not supported")
    CR.TypeDecl.TypedefDecl(def, decls) => generate_typedef(definition, genmod)
    CR.TypeDecl.BitsetDecl(name, super, bfs) => error("bitsets not supported")
    CR.TypeDecl.BitmaskDecl(name, cases) => error("bitmasks not supported")
end

# Construct a relative-import expression `import .....name` with `dots`
# leading dots. `dots=1` is `import .name` (sibling), `dots=2` is `import ..name`
# (uncle), `dots=N` climbs N-1 module levels before resolving `name`.
function _dotted_import(dots::Int, name::Symbol)
    args = Any[Symbol(".") for _ in 1:dots]
    push!(args, name)
    return Expr(:import, Expr(:., args...))
end


# Kahn's algorithm over the decl entries given a precomputed same-module
# dep list per entry. Ties broken alphabetically for reproducible output.
function _topo_sort(entries::Vector{Tuple{Symbol, ModEntry}},
                    same_mod_for::Dict{Symbol, Vector{Symbol}})
    names = Set(name for (name, _) in entries)
    succ = Dict{Symbol, Vector{Symbol}}()
    indeg = Dict{Symbol, Int}(name => 0 for (name, _) in entries)
    for (name, _) in entries
        for dep in get(same_mod_for, name, Symbol[])
            dep == name && continue
            dep in names || continue
            push!(get!(succ, dep, Symbol[]), name)
            indeg[name] += 1
        end
    end
    ready = sort!([name for (name, _) in entries if indeg[name] == 0])
    by_name = Dict(name => entry for (name, entry) in entries)
    sorted = Tuple{Symbol, ModEntry}[]
    while !isempty(ready)
        n = popfirst!(ready)
        push!(sorted, (n, by_name[n]))
        for m in get(succ, n, Symbol[])
            indeg[m] -= 1
            if indeg[m] == 0
                pos = searchsortedfirst(ready, m)
                insert!(ready, pos, m)
            end
        end
    end
    length(sorted) == length(entries) ||
        error("circular dependency among $(setdiff(keys(indeg), Set(n for (n, _) in sorted)))")
    return sorted
end

# Recursively walk a module dict to gather the set of sibling packages any
# decl inside it references. Self-package refs (where the first path segment
# matches `enclosing`) are not counted. Used to topo-sort outer packages so
# `Big.std_msgs` exists before `Big.geometry_msgs` (which imports it) loads.
function _all_cross_pkgs(mod_dict::AbstractDict, enclosing::Union{Symbol, Nothing})
    pkgs = Set{Symbol}()
    for (_, v) in mod_dict
        if v isa AbstractDict
            union!(pkgs, _all_cross_pkgs(v, enclosing))
        elseif v isa ModEntry
            for (path, _) in v.refs
                isempty(path) && continue
                enclosing !== nothing && path[1] == enclosing && continue
                push!(pkgs, path[1])
            end
        end
    end
    return pkgs
end

# Build an `import ..<sibling>: a, b, c` expression. Used so that a sub-module
# like `srv` can write a bare `Transition` and have it resolve to the type
# defined in the sibling `msg` sub-module.
function _named_import_sibling(sibling::Symbol, names::Vector{Symbol})
    # AST: import ..sibling: name1, name2, ...
    return Expr(:import,
        Expr(:(:),
            Expr(:., Symbol("."), Symbol("."), sibling),
            (Expr(:., n) for n in names)...))
end

# `depth` is the module nesting level of the module currently being built,
# measured from the eval site (depth=1 is the outermost emitted `module`,
# depth=2 a sub-module, etc). A cross-package `using ..pkg` at this level
# uses depth+1 leading dots to reach the eval site's parent and find the
# sibling package.
#
# `siblings` is supplied by the *parent* `build_modules` call: a map from
# each sibling sub-module's name to its decl-entry symbols, so this sub-
# module can pull the right names into scope. A srv sub-module gets a
# `siblings = Dict(:msg => [...])` entry and emits `import ..msg: ...`.
function build_modules(name, mod_dict; depth::Int=1,
                       enclosing::Union{Symbol, Nothing}=nothing,
                       siblings::Dict{Symbol, Vector{Symbol}}=Dict{Symbol, Vector{Symbol}}())
    # At depth=1 we are the package (the outermost module being emitted),
    # so this name becomes the "enclosing" context all the way down. Once
    # set on the first call, the package context is carried unchanged
    # through recursion into sub-modules (msg, srv, …).
    enclosing_pkg = enclosing === nothing ? name : enclosing

    submods = Tuple{Symbol, AbstractDict}[]
    decl_entries = Tuple{Symbol, ModEntry}[]
    for (k, v) in mod_dict
        if v isa AbstractDict
            push!(submods, (k, v))
        else
            push!(decl_entries, (k, v::ModEntry))
        end
    end

    # Resolve every entry's refs in the current package context so we know
    # which deps are same-module (just names) and which are foreign (need
    # an explicit `import ...pkg` for the bare path to resolve).
    same_mod_for = Dict{Symbol, Vector{Symbol}}()
    cross_pkgs = Set{Symbol}()
    for (k, entry) in decl_entries
        same_mod, cp = _classify_refs(entry.refs, enclosing_pkg)
        same_mod_for[k] = same_mod
        union!(cross_pkgs, cp)
    end

    body = Any[]
    push!(body, :(import StaticArrays, CDRSerialization))
    for pkg in sort!(collect(cross_pkgs))
        push!(body, _dotted_import(depth + 1, pkg))
    end
    # Pull names from sibling sub-modules (e.g. msg → srv) so bare refs
    # like `Transition` inside a srv definition resolve.
    for sib in sort!(collect(keys(siblings)))
        names = siblings[sib]
        isempty(names) && continue
        push!(body, _named_import_sibling(sib, names))
    end

    # Within a ROS2 package, srv definitions reference msg types and
    # action definitions reference msg (and rarely srv) types. Emit sub-
    # modules in dependency order — msg first, then srv, then action,
    # then anything else alphabetically — so that each sub-module can
    # `import ..<earlier-kind>:...` from a sibling that has already been
    # defined. Imports of a sibling that comes *later* in the same outer
    # module would fail at load time because Julia evaluates modules in
    # textual order.
    #
    # Sibling cross-importing is a *package-level* feature only: it shares
    # names across the `msg`/`srv`/`action` sub-modules of one package.
    # Deeper structures (e.g. per-message `<Foo>_Constants` sub-modules
    # inside `msg`) must not inherit it, or their inner `const` names
    # collide with imports of identically-named constants in siblings.
    sub_priority = (:msg, :srv, :action)
    submod_order = sort(submods; by = ((sn, _),) ->
        (something(findfirst(==(sn), sub_priority), length(sub_priority)+1), sn))
    is_pkg_level = depth == 1
    sub_entry_names = is_pkg_level ? Dict{Symbol, Vector{Symbol}}(
        sn => sort!([k for (k, v) in sd if v isa ModEntry])
        for (sn, sd) in submod_order) : Dict{Symbol, Vector{Symbol}}()
    seen_subs = Symbol[]
    for (sub_name, sub_dict) in submod_order
        child_siblings = is_pkg_level ?
            Dict{Symbol, Vector{Symbol}}(
                k => sub_entry_names[k] for k in seen_subs if k != sub_name) :
            Dict{Symbol, Vector{Symbol}}()
        push!(body, build_modules(sub_name, sub_dict;
                                  depth=depth+1, enclosing=enclosing_pkg,
                                  siblings=child_siblings))
        push!(seen_subs, sub_name)
    end

    for (_, entry) in _topo_sort(decl_entries, same_mod_for)
        push!(body, entry.render(enclosing_pkg))
    end

    return :(module $name $(body...) end)
end

function generate_code(definitions::Vector{<:CR.CanAnnotate{CR.Decl}})
    generated_modules = LittleDict{Symbol, Any}()
    for def in definitions
        generate_code(def, generated_modules)
    end

    # Separate top-level outer modules from any flat (non-module) decls.
    top_modules = Tuple{Symbol, AbstractDict}[]
    top_entries = Tuple{Symbol, ModEntry}[]
    for (k, v) in generated_modules
        if v isa AbstractDict
            push!(top_modules, (k, v))
        else
            push!(top_entries, (k, v::ModEntry))
        end
    end

    # Topo-sort outer modules by their cross-package deps so that, when the
    # caller eval's our output in order, every module's `import ...pkg` finds
    # an already-defined sibling. Each outer module's deps are the union of
    # cross-package refs in all its sub-decls — with self-refs (matching the
    # outer module name) excluded.
    mod_index = Dict(name => i for (i, (name, _)) in enumerate(top_modules))
    mod_deps_for = Dict{Symbol, Vector{Symbol}}()
    entries_for_sort = Tuple{Symbol, ModEntry}[]
    for (name, dict) in top_modules
        deps = sort!(collect(_all_cross_pkgs(dict, name)))
        filter!(p -> haskey(mod_index, p), deps)
        mod_deps_for[name] = deps
        push!(entries_for_sort,
              (name, ModEntry(_ -> :(module $name end), Pair{Vector{Symbol}, Symbol}[])))
    end
    sorted_order = [n for (n, _) in _topo_sort(entries_for_sort, mod_deps_for)]

    out = Any[]
    by_name = Dict(name => d for (name, d) in top_modules)
    for name in sorted_order
        push!(out, build_modules(name, by_name[name]; depth=1))
    end
    # Flat (non-module) decls — topo-sort and emit after modules.
    flat_same_mod = Dict{Symbol, Vector{Symbol}}(
        name => first(_classify_refs(entry.refs, nothing))
        for (name, entry) in top_entries)
    for (_, entry) in _topo_sort(top_entries, flat_same_mod)
        push!(out, entry.render(nothing))
    end
    return out
end