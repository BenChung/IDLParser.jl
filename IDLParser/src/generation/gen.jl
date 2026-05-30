import IDLParser.ConstResolution as CR
import IDLParser.Parse as Parse
using OrderedCollections
using Moshi.Match: @match

# `render(enclosing) -> Expr` is deferred so type refs can resolve into their
# canonical form once the enclosing package is known. `refs` is kept raw so
# topo-sorting and import collection don't have to inspect Exprs.
struct ModEntry
    render::Function
    refs::Vector{Pair{Vector{Symbol}, Symbol}}
end

# --- Per-type layout requirement plumbing ----------------------------------
#
# A struct can be pinned to a CDR layout tier — `:compact` (in-memory bytes ==
# wire, single load / zero-copy alias), `:fixed` (no variable-length fields), or
# `:any` (default) — either with an `@compact` / `@fixed` annotation in the IDL
# or via the `require` option to `generate_code` (which wins on conflict). The
# generator asserts the tier at build time: an impossible request (`:compact` on
# a struct with a string/sequence field) errors during generation with the
# offending field named, and a request that depends on field layout (padding)
# emits a precompile `@assert` against the library's `iscompact` — so a layout
# regression fails the build instead of silently dropping to a slower tier.
struct LayoutCtx
    require::Dict{Vector{Symbol}, Symbol}   # full scoped path => tier
    scope::Vector{Symbol}                   # enclosing module path
    anno_tier::Union{Symbol, Nothing}       # tier from an enclosing annotation
end
LayoutCtx() = LayoutCtx(Dict{Vector{Symbol}, Symbol}(), Symbol[], nothing)

# Descend into a sub-module: extend the scope, drop any pending annotation tier
# (an annotation binds only to the single decl it precedes).
_descend(lc::LayoutCtx, name::Symbol) = LayoutCtx(lc.require, [lc.scope; name], nothing)
# Apply an annotation in a chain: a recognised layout annotation sets the tier;
# any other annotation keeps whatever an outer one set.
_anno(lc::LayoutCtx, tier) = LayoutCtx(lc.require, lc.scope,
                                       tier === nothing ? lc.anno_tier : tier)

# `@compact` / `@fixed` (bare, no params) are the recognised layout annotations;
# everything else (`@verbatim`, …) returns nothing and is ignored for layout.
function _layout_tier_from_anno(annotation_name::CR.ScopedName.Type)
    nm = @match annotation_name begin
        CR.ScopedName.Name(_, n, _) => n
    end
    nm === :compact && return :compact
    nm === :fixed   && return :fixed
    return nothing
end

# Effective tier for a struct: the `require` option (by full scoped path) wins,
# else the annotation tier, else `:any`.
function _effective_tier(lc::LayoutCtx, name::Symbol)
    fullpath = [lc.scope; name]
    haskey(lc.require, fullpath) && return lc.require[fullpath]
    return lc.anno_tier === nothing ? :any : lc.anno_tier
end

# Normalise `require` keys ("pkg/sub/Name") to scoped-symbol paths; validate tiers.
function _normalize_require(require)
    out = Dict{Vector{Symbol}, Symbol}()
    for (k, v) in require
        tier = Symbol(v)
        tier in (:compact, :fixed, :any) ||
            error("generate_code: unknown layout tier `:$tier` for `$k` (use :compact, :fixed, or :any)")
        out[Symbol.(split(String(k), '/'))] = tier
    end
    return out
end

generate_code(definition::CR.Annotated.Type, genmod, registry, lc::LayoutCtx) = @match definition begin
    CR.Annotated.Annotation(name, params, defn) =>
        generate_code(defn, genmod, registry, _anno(lc, _layout_tier_from_anno(name)))
end
generate_code(definition::CR.ModuleDecl.Type, genmod, registry, lc::LayoutCtx) = @match definition begin
    CR.ModuleDecl.MDecl(name, decls) => begin
        mod = get!(genmod, name, LittleDict{Symbol, Any}())
        child = _descend(lc, name)
        for decl in decls
            generate_code(decl, mod, registry, child)
        end
    end
end
generate_code(definition::CR.ConstDecl.Type, genmod, registry, lc::LayoutCtx) = @match definition begin
    CR.ConstDecl.CDecl(typ, name, val) => begin
        genmod[name] = ModEntry(_ -> :(const $name = $val), Pair{Vector{Symbol}, Symbol}[])
    end
end

# A TRef whose first path segment is the enclosing package emits the bare
# name (its canonical form inside the package's module); cross-package refs
# emit the full `pkg.ns.Name` and rely on the matching `import ...pkg` at
# the top of the enclosing module.
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
        # Bounded and unbounded strings share a wire format. Moshi @match
        # treats `nothing` in a pattern as a variable binding, so the only
        # safe way to "match any inner" is the `_` wildcard.
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

function _collect_refs_ts!(refs::Vector{Pair{Vector{Symbol}, Symbol}},
                            ts::CR.TypeSpec.Type)
    @match ts begin
        CR.TypeSpec.TRef(CR.ScopedName.Name(path, n, is_local)) =>
            is_local && push!(refs, copy(path) => n)
        CR.TypeSpec.TSeq(elt, _) => _collect_refs_ts!(refs, elt)
        _ => nothing
    end
end

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

# `pkg/Foo` from inside `pkg` is identical in meaning to a bare `Foo`, so
# treat it as same-module rather than emitting a self-import.
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
# --- Compact-eligibility classification -----------------------------------
#
# A struct can be emitted via `@cdr1_compat` (a single plain concrete CDR1
# struct) only when every field is flat: a primitive, an SArray of primitives,
# a typedef resolving to those, or a nested struct that is
# itself all-fixed (`@cdr1_compat` validates nesting recursively). A string,
# sequence, or non-flat nested struct disqualifies it, so it falls back to a
# plain `@kwdef` struct decoded field-by-field by CDRSerialization's generic
# `read` (owned) / `read_view` (zero-copy `CDRString`/`CDRArray`). The registry
# lets the classifier resolve a `TRef` (typedef vs struct vs enum) and recurse
# into nested struct definitions without re-walking the AST for every field.
struct TypeRegistry
    structs::Set{Symbol}
    enums::Set{Symbol}
    typedefs::Dict{Symbol, Tuple{CR.TypeSpec.Type, CR.Declarator.Type}}
    struct_decls::Dict{Symbol, Any}
end

_collect_registry!(reg::TypeRegistry, def::CR.Annotated.Type) = @match def begin
    CR.Annotated.Annotation(_, _, inner) => _collect_registry!(reg, inner)
end
_collect_registry!(reg::TypeRegistry, def::CR.ModuleDecl.Type) = @match def begin
    CR.ModuleDecl.MDecl(_, decls) => for d in decls; _collect_registry!(reg, d); end
end
_collect_registry!(::TypeRegistry, ::CR.ConstDecl.Type) = nothing
_collect_registry!(reg::TypeRegistry, def::CR.TypeDecl.Type) = @match def begin
    CR.TypeDecl.StructDecl(name, _, decls) => (push!(reg.structs, name); reg.struct_decls[name] = decls)
    CR.TypeDecl.StructFwdDecl(name)    => push!(reg.structs, name)
    CR.TypeDecl.EnumDecl(name, _)      => push!(reg.enums, name)
    CR.TypeDecl.TypedefDecl(target, declarators) => begin
        # Only `target isa TypeSpec` typedefs (the alias forms) are tracked;
        # anonymous nested-type typedefs aren't compact-eligible references.
        if target isa CR.TypeSpec.Type
            for decl in declarators
                reg.typedefs[_declarator_name(decl)] = (target, decl)
            end
        end
    end
    _ => nothing
end

function _build_registry(definitions)
    reg = TypeRegistry(Set{Symbol}(), Set{Symbol}(),
                       Dict{Symbol, Tuple{CR.TypeSpec.Type, CR.Declarator.Type}}(),
                       Dict{Symbol, Any}())
    for def in definitions
        _collect_registry!(reg, def)
    end
    return reg
end

_is_primitive_spec(ts::CR.TypeSpec.Type) = @match ts begin
    CR.TypeSpec.TFloat(_) => true
    CR.TypeSpec.TInt(_)   => true
    CR.TypeSpec.TUInt(_)  => true
    CR.TypeSpec.TChar()   => true
    CR.TypeSpec.TWChar()  => true
    CR.TypeSpec.TBool()   => true
    CR.TypeSpec.TOctet()  => true
    _ => false
end

# Classify a typedef alias chain as :primitive_scalar, :primitive_array,
# :other, or :none (name not a tracked typedef).
function _typedef_kind(name::Symbol, registry::TypeRegistry)
    info = get(registry.typedefs, name, nothing)
    info === nothing && return :none
    (tts, tdecl) = info
    isarray = @match tdecl begin
        CR.Declarator.DArray(_, _) => true
        CR.Declarator.DIdent(_)    => false
    end
    isarray && return _spec_resolves_to_primitive(tts, registry) ? :primitive_array : :other
    _is_primitive_spec(tts) && return :primitive_scalar
    return @match tts begin
        CR.TypeSpec.TRef(CR.ScopedName.Name(_, n2, _)) => _typedef_kind(n2, registry)
        _ => :other
    end
end

# Does `ts`, used as a scalar field, resolve to a primitive (directly or via a
# scalar typedef alias)?
function _spec_resolves_to_primitive(ts::CR.TypeSpec.Type, registry::TypeRegistry)
    _is_primitive_spec(ts) && return true
    @match ts begin
        CR.TypeSpec.TRef(CR.ScopedName.Name(_, name, _)) =>
            _typedef_kind(name, registry) === :primitive_scalar
        _ => false
    end
end

# Is a single field (its TypeSpec + declarator) flat — i.e. a primitive, an
# SArray of primitives, or a nested struct that is itself all-fixed — so the
# whole struct can be a single `@cdr1_compat` value? `seen` guards against a
# (degenerate) cyclic struct reference.
function _field_is_fixed(ts::CR.TypeSpec.Type, decl::CR.Declarator.Type,
                         registry::TypeRegistry, seen::Set{Symbol})
    @match decl begin
        # `T name[N]` → SArray; eligible iff the element is a primitive
        # (`@cdr1_compat` rejects SArrays of structs).
        CR.Declarator.DArray(_, _) => _spec_resolves_to_primitive(ts, registry)
        # `T name` → primitive, a typedef to a primitive/primitive-array
        # (e.g. `float[3]`), or a nested all-fixed struct.
        CR.Declarator.DIdent(_) => begin
            _is_primitive_spec(ts) && return true
            @match ts begin
                CR.TypeSpec.TRef(CR.ScopedName.Name(_, name, _)) => begin
                    _typedef_kind(name, registry) in (:primitive_scalar, :primitive_array) && return true
                    name in registry.structs && return _struct_all_fixed(name, registry, seen)
                    false
                end
                _ => false
            end
        end
    end
end

# Is every field of struct `name` flat (recursively)? Unknown / forward-declared
# structs (no stored decls) and cycles are treated as not-fixed.
function _struct_all_fixed(name::Symbol, registry::TypeRegistry, seen::Set{Symbol})
    name in seen && return false
    decls = get(registry.struct_decls, name, nothing)
    decls === nothing && return false
    seen2 = push!(copy(seen), name)
    for d in decls
        member = _unwrap_member(d)
        member isa Pair || return false
        ts, declarators = member
        for decl in declarators
            _field_is_fixed(ts, decl, registry, seen2) || return false
        end
    end
    return true
end

# Strip any `@annotation` layers down to the underlying member `Pair`.
function _unwrap_member(d)
    while d isa CR.Annotated.Type
        d = @match d begin
            CR.Annotated.Annotation(_, _, inner) => inner
        end
    end
    return d
end

# (name, julia_type_expr, is_fixed) for every declared field, in order. `seen`
# seeds the cycle guard with the struct being rendered.
function _struct_fields(decls, enclosing::Union{Symbol, Nothing}, registry::TypeRegistry,
                        seen::Set{Symbol})
    fields = Tuple{Symbol, Any, Bool}[]
    for d in decls
        member = _unwrap_member(d)
        member isa Pair || error("unexpected struct member: $member")
        ts, declarators = member
        for decl in declarators
            jltype = resolve_type(ts; enclosing=enclosing)
            (fname, ftype) = resolve_declarator(decl, jltype)
            push!(fields, (fname, ftype, _field_is_fixed(ts, decl, registry, seen)))
        end
    end
    return fields
end

# An all-fixed struct (every field a primitive or an SArray of primitives) is
# emitted via `@cdr1_compat`: a single plain concrete struct whose wire format
# is standard CDR1 — exactly what a ROS2 publisher emits, trailing pad omitted.
# Unlike `@cdr_compact` it produces no `Name{V}` CDR1/CDR2 variant wrapper
# (ROS2 only needs CDR1), so `fieldnames`/`fieldtype` reflect the real fields
# and the value nests cleanly. `@cdr1_compat` supplies `propertynames`, `==`,
# and `show`; serialization flows through the library's generic `write`/`read`.
#
# A struct with a string/sequence/non-flat-nested field is a plain `@kwdef`
# struct. Serialization is entirely the library's: generic `read` (owned),
# `read_view` (zero-copy `CDRView{T}` with `CDRString`/`CDRArray` fields), and
# generic `write` (a field-by-field inverse of `read`). The generator emits no
# read/write — only the struct, a value `==`, and `@kwdef`'s keyword ctor.
function _render_struct(name::Symbol, decls, enclosing::Union{Symbol, Nothing},
                        registry::TypeRegistry, tier::Symbol=:any)
    fields = _struct_fields(decls, enclosing, registry, Set{Symbol}((name,)))
    field_decls = [Expr(:(::), n, t) for (n, t, _) in fields]
    field_names = Symbol[n for (n, _, _) in fields]
    all_fixed = !isempty(fields) && all(f -> f[3], fields)

    # A requested compact/fixed layout that's structurally impossible (a
    # variable-length field) is an error now, with the offending field named —
    # no homogeneous representation exists, and deferring to precompile would
    # only obscure the cause.
    if (tier === :compact || tier === :fixed) && !all_fixed
        bad = first(f[1] for f in fields if !f[3])
        error("layout: message `$name` is required to be `:$tier`, but field `$bad` " *
              "is variable-length (string/sequence/non-flat-nested); a homogeneous " *
              "in-memory representation is impossible. Drop the requirement or change the schema.")
    end

    if all_fixed
        structdef = Expr(:struct, false, name, Expr(:block, field_decls...))
        cdr1_compat = Expr(:macrocall,
            Expr(:., :CDRSerialization, QuoteNode(Symbol("@cdr1_compat"))),
            LineNumberNode(@__LINE__, Symbol(@__FILE__)),
            structdef)
        # Keyword constructor preserving the old `@kwdef` ergonomics: convert
        # each field so callers can pass plain `Int`/`Float64` literals, then
        # forward to the positional constructor the plain struct provides.
        conv_args = [:(convert($t, $n)) for (n, t, _) in fields]
        kw_ctor = Expr(:(=),
            Expr(:call, name, Expr(:parameters, field_names...)),
            Expr(:call, name, conv_args...))
        # `@cdr1_compat` emits `propertynames`/`==`/`show`; the library's
        # generic `write`/`read` handle serialization. We only add the keyword
        # constructor (the macro's own constructor is positional).
        body = Any[cdr1_compat, kw_ctor]
        # `:fixed` is already guaranteed by `@cdr1_compat` (it rejects any
        # non-flat field at macro-expansion). `:compact` additionally requires a
        # padding-free layout, which depends on the Julia field layout — assert
        # it at precompile so a regression fails the build with the library's
        # explanation rather than silently losing the zero-copy path.
        if tier === :compact
            push!(body, quote
                @assert CDRSerialization.iscompact($name) string(
                    "layout: message `", $(string(name)),
                    "` is required to be compact (in-memory layout identical to the CDR1 ",
                    "wire — single load / zero-copy) but is not — ",
                    CDRSerialization.cdr_layout($name).why)
            end)
        end
        return Expr(:block, body...)
    end

    # Julia's default `==` only field-compares bits types; with `String`/
    # `Vector` fields it falls back to `===`. Generated messages are pure data,
    # so force value equality.
    eq_body = isempty(field_names) ? true :
        foldl((acc, n) -> :($acc && a.$n == b.$n),
              field_names[2:end];
              init = :(a.$(field_names[1]) == b.$(field_names[1])))
    return quote
        @kwdef struct $name
            $(field_decls...)
        end
        Base.:(==)(a::$name, b::$name) = $eq_body
    end
end

generate_struct(definition::CR.TypeDecl.Type, genmod, registry::TypeRegistry, lc::LayoutCtx) = @match definition begin
    CR.TypeDecl.StructDecl(name, nothing, decls) => begin
        tier = _effective_tier(lc, name)
        refs = _collect_refs_members(decls)
        genmod[name] = ModEntry(enc -> _render_struct(name, decls, enc, registry, tier), refs)
    end
end
_declarator_name(decl::CR.Declarator.Type) = @match decl begin
    CR.Declarator.DArray(n, _) => n
    CR.Declarator.DIdent(n)    => n
end

generate_typedef(definition::CR.TypeDecl.Type, genmod, registry) = @match definition begin
    CR.TypeDecl.TypedefDecl(ts::CR.TypeSpec.Type, declarators) => begin
        refs = Pair{Vector{Symbol}, Symbol}[]
        _collect_refs_ts!(refs, ts)
        unique!(refs)
        for decl in declarators
            sym = _declarator_name(decl)
            # Closure captures `decl` and `ts` so the alias is rebuilt with
            # the enclosing-pkg context known.
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

generate_code(definition::CR.TypeDecl.Type, genmod, registry, lc::LayoutCtx) = @match definition begin
    CR.TypeDecl.StructDecl(name, nothing, decls) => generate_struct(definition, genmod, registry, lc)
    CR.TypeDecl.StructDecl(name, super, decls) => error("struct inheritance not supported")
    CR.TypeDecl.StructFwdDecl(name) => error("forward declarations not supported")
    CR.TypeDecl.UnionDecl(name, disc, cases) => error("unions not supported")
    CR.TypeDecl.UnionFwdDecl(name) => error("forward declarations not supported")
    CR.TypeDecl.EnumDecl(name, cases) => error("enums not supported")
    CR.TypeDecl.TypedefDecl(def, decls) => generate_typedef(definition, genmod, registry)
    CR.TypeDecl.BitsetDecl(name, super, bfs) => error("bitsets not supported")
    CR.TypeDecl.BitmaskDecl(name, cases) => error("bitmasks not supported")
end

# `import .....name`: `dots` leading dots climb `dots-1` module levels.
function _dotted_import(dots::Int, name::Symbol)
    args = Any[Symbol(".") for _ in 1:dots]
    push!(args, name)
    return Expr(:import, Expr(:., args...))
end

# Kahn's algorithm; ties broken alphabetically for reproducible output.
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

# Outer packages need topo-sorting too — `std_msgs` must exist before
# `geometry_msgs` (which imports it) loads. Self-package refs are ignored
# so a package isn't counted as depending on itself.
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

# `import ..sibling: a, b, c` lets a sub-module like `srv` write bare
# `Transition` and have it resolve to the sibling `msg`'s type.
function _named_import_sibling(sibling::Symbol, names::Vector{Symbol})
    return Expr(:import,
        Expr(:(:),
            Expr(:., Symbol("."), Symbol("."), sibling),
            (Expr(:., n) for n in names)...))
end

# `depth=1` is the outermost emitted module (the package). A cross-package
# import uses `depth + 1` leading dots to reach the eval site's parent and
# find the sibling package.
#
# `siblings` is set by the parent call so a sub-module can pull names from
# already-defined sibling sub-modules — typically `srv`/`action` pulling
# from `msg`.
function build_modules(name, mod_dict; depth::Int=1,
                       enclosing::Union{Symbol, Nothing}=nothing,
                       siblings::Dict{Symbol, Vector{Symbol}}=Dict{Symbol, Vector{Symbol}}())
    # First call's `name` is the package; carry it as the enclosing context
    # for every recursive call so refs resolve consistently.
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
    for sib in sort!(collect(keys(siblings)))
        names = siblings[sib]
        isempty(names) && continue
        push!(body, _named_import_sibling(sib, names))
    end

    # Emit sub-modules in dependency order — msg, then srv, then action —
    # so each one's `import ..<earlier-kind>:...` lands on an already-
    # defined sibling. Julia evaluates module bodies top-down, so a later
    # sibling can't be imported from an earlier one.
    #
    # Sibling cross-imports apply only at the package level. Inside a `msg`
    # sub-module, the per-message `_Constants` modules must not inherit
    # them or their inner consts collide with identically-named consts
    # imported from siblings.
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

function generate_code(definitions::Vector{<:CR.CanAnnotate{CR.Decl}};
                       require::AbstractDict = Dict{String, Symbol}())
    registry = _build_registry(definitions)
    lc = LayoutCtx(_normalize_require(require), Symbol[], nothing)
    generated_modules = LittleDict{Symbol, Any}()
    for def in definitions
        generate_code(def, generated_modules, registry, lc)
    end

    top_modules = Tuple{Symbol, AbstractDict}[]
    top_entries = Tuple{Symbol, ModEntry}[]
    for (k, v) in generated_modules
        if v isa AbstractDict
            push!(top_modules, (k, v))
        else
            push!(top_entries, (k, v::ModEntry))
        end
    end

    # Each outer module's deps are the union of cross-package refs in its
    # sub-decls; topo-sorting ensures every `import ...pkg` lands on an
    # already-evaluated sibling.
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
    flat_same_mod = Dict{Symbol, Vector{Symbol}}(
        name => first(_classify_refs(entry.refs, nothing))
        for (name, entry) in top_entries)
    for (_, entry) in _topo_sort(top_entries, flat_same_mod)
        push!(out, entry.render(nothing))
    end
    return out
end