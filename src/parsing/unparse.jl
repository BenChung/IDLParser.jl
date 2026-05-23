using Moshi.Match: @match

unparse(node) = sprint(io -> unparse(io, node))

const _STRING_ESCAPES = Dict{Char, String}(
    '\\' => "\\\\",
    '\n' => "\\n",
    '\t' => "\\t",
    '\r' => "\\r",
    '\b' => "\\b",
    '\f' => "\\f",
    '\a' => "\\a",
    '\v' => "\\v",
)

function _escape_codepoint(c::Char)
    cp = UInt32(c)
    if cp <= 0xFF
        return string("\\x", uppercase(string(cp, base=16, pad=2)))
    else
        return string("\\u", uppercase(string(cp, base=16, pad=4)))
    end
end

function _escape_for_literal(c::Char, quote_char::Char)
    if c == quote_char
        return string('\\', quote_char)
    elseif haskey(_STRING_ESCAPES, c)
        return _STRING_ESCAPES[c]
    else
        cp = UInt32(c)
        if cp >= 0x20 && cp <= 0x7E
            return string(c)
        else
            return _escape_codepoint(c)
        end
    end
end

escape_string_literal(s::AbstractString) =
    sprint(io -> for c in s; print(io, _escape_for_literal(c, '"')); end)

escape_char_literal(c::Char) = _escape_for_literal(c, '\'')

unparse(io::IO, lit::Literal.Type, indent::Int=0) = @match lit begin
    Literal.F32(v) => print(io, v, "f")
    Literal.F64(v) => print(io, v, "d")
    Literal.Intg(v) => print(io, v)
    Literal.Bl(true) => print(io, "TRUE")
    Literal.Bl(false) => print(io, "FALSE")
    Literal.Ch(c) => print(io, '\'', escape_char_literal(c), '\'')
    Literal.St(s) => print(io, '"', escape_string_literal(s), '"')
end

unparse(io::IO, name::ScopedName.Type, indent::Int=0) = @match name begin
    ScopedName.Name(path, n, is_local) => begin
        is_local || print(io, "::")
        for p in path
            print(io, p, "::")
        end
        print(io, n)
    end
end

op_string(op::Binop.Type)::String = @match op begin
    Binop.Or() => "|"
    Binop.Xor() => "^"
    Binop.And() => "&"
    Binop.Lshift() => "<<"
    Binop.Rshift() => ">>"
    Binop.Add() => "+"
    Binop.Sub() => "-"
    Binop.Mul() => "*"
    Binop.Div() => "/"
    Binop.Mod() => "%"
end

op_string(op::Unop.Type)::String = @match op begin
    Unop.Plus() => "+"
    Unop.Neg() => "-"
    Unop.Inv() => "~"
end

prec(op::Binop.Type)::Int = @match op begin
    Binop.Mul() => 5
    Binop.Div() => 5
    Binop.Mod() => 5
    Binop.Add() => 4
    Binop.Sub() => 4
    Binop.Lshift() => 3
    Binop.Rshift() => 3
    Binop.And() => 2
    Binop.Xor() => 1
    Binop.Or() => 0
end

# Left children only need parens when strictly lower-precedence than the parent;
# right children also need parens at equal precedence, because IDL binops are
# left-associative and `a - (b - c)` must round-trip differently from `a - b - c`.
function unparse_in_context(io::IO, expr::ConstExpr.Type, parent_prec::Int, on_right::Bool)
    @match expr begin
        ConstExpr.BinApp(op, _, _) => begin
            p = prec(op)
            needs_parens = on_right ? (p <= parent_prec) : (p < parent_prec)
            if needs_parens
                print(io, '(')
                unparse(io, expr)
                print(io, ')')
            else
                unparse(io, expr)
            end
        end
        _ => unparse(io, expr)
    end
end

unparse(io::IO, expr::ConstExpr.Type, indent::Int=0) = @match expr begin
    ConstExpr.BinApp(op, l, r) => begin
        p = prec(op)
        unparse_in_context(io, l, p, false)
        print(io, ' ', op_string(op), ' ')
        unparse_in_context(io, r, p, true)
    end
    ConstExpr.UnApp(op, v) => begin
        print(io, op_string(op))
        @match v begin
            ConstExpr.BinApp(_, _, _) => begin
                print(io, '(')
                unparse(io, v)
                print(io, ')')
            end
            _ => unparse(io, v)
        end
    end
    ConstExpr.Var(name) => unparse(io, name)
    ConstExpr.Lit(l) => unparse(io, l)
end

unparse(io::IO, decl::Declarator.Type, indent::Int=0) = @match decl begin
    Declarator.DArray(name, dims) => begin
        print(io, name)
        for d in dims
            print(io, '[')
            unparse(io, d)
            print(io, ']')
        end
    end
    Declarator.DIdent(name) => print(io, name)
end

# Plain strings as the wrapped subject of an Annotation are used in tests
# (chunk 1 doesn't unparse Decls yet); chunk 2's Decl methods don't conflict
# with this since Decls aren't AbstractStrings.
unparse(io::IO, s::AbstractString, indent::Int=0) = print(io, s)

unparse(io::IO, ann::Annotated.Type, indent::Int=0) = @match ann begin
    Annotated.Annotation(name, params, inner) => begin
        print(io, '@')
        unparse(io, name)
        if isempty(params)
            # no parens
        elseif length(params) == 1 && params[1].first === :value
            print(io, '(')
            unparse(io, params[1].second)
            print(io, ')')
        else
            print(io, '(')
            first = true
            for (k, v) in params
                first || print(io, ", ")
                first = false
                print(io, k, '=')
                unparse(io, v)
            end
            print(io, ')')
        end
        print(io, ' ')
        unparse(io, inner, indent)
    end
end

_pad(indent::Int) = " " ^ (4 * indent)

unparse(io::IO, ts::TypeSpec.Type, indent::Int=0) = @match ts begin
    TypeSpec.TRef(name) => unparse(io, name)
    TypeSpec.TFloat(32) => print(io, "float")
    TypeSpec.TFloat(64) => print(io, "double")
    TypeSpec.TFloat(128) => print(io, "long double")
    TypeSpec.TInt(n) => print(io, "int", n)
    TypeSpec.TUInt(n) => print(io, "uint", n)
    TypeSpec.TChar() => print(io, "char")
    TypeSpec.TWChar() => print(io, "wchar")
    TypeSpec.TBool() => print(io, "boolean")
    TypeSpec.TOctet() => print(io, "octet")
    TypeSpec.TAny() => print(io, "any")
    TypeSpec.TObject() => print(io, "Object")
    TypeSpec.TValueBase() => print(io, "ValueBase")
    TypeSpec.TSeq(elt, len) => begin
        print(io, "sequence<")
        unparse(io, elt)
        if len !== nothing
            print(io, ", ")
            unparse(io, len)
        end
        print(io, '>')
    end
    TypeSpec.TString(len) => begin
        print(io, "string")
        if len !== nothing
            print(io, '<')
            unparse(io, len)
            print(io, '>')
        end
    end
    TypeSpec.TWString(len) => begin
        print(io, "wstring")
        if len !== nothing
            print(io, '<')
            unparse(io, len)
            print(io, '>')
        end
    end
    TypeSpec.TSpecifiedFixedPoint(d, fd) => begin
        print(io, "fixed<")
        unparse(io, d)
        print(io, ", ")
        unparse(io, fd)
        print(io, '>')
    end
    TypeSpec.TFixedPoint() => print(io, "fixed")
    TypeSpec.TMap(k, v, len) => begin
        print(io, "map<")
        unparse(io, k)
        print(io, ", ")
        unparse(io, v)
        if len !== nothing
            print(io, ", ")
            unparse(io, len)
        end
        print(io, '>')
    end
end

unparse(io::IO, lbl::UnionCaseLabel.Type, indent::Int=0) = @match lbl begin
    UnionCaseLabel.UCLCase(expr) => begin
        print(io, "case ")
        unparse(io, expr)
        print(io, ':')
    end
    UnionCaseLabel.UCLDefault() => print(io, "default:")
end

unparse(io::IO, elt::UnionElement.Type, indent::Int=0) = @match elt begin
    UnionElement.UElem(ts, decl) => begin
        unparse(io, ts, indent)
        print(io, ' ')
        unparse(io, decl, indent)
    end
end

unparse(io::IO, bf::BitfieldSpec.Type, indent::Int=0) = @match bf begin
    BitfieldSpec.BSSpec(len, dest, idents) => begin
        print(io, "bitfield<")
        unparse(io, len)
        if dest !== nothing
            print(io, ", ")
            unparse(io, dest)
        end
        print(io, '>')
        for id in idents
            print(io, ' ', id)
        end
    end
end

# Struct/typedef member body: `<ts> <decl1>, <decl2>, ...`. Enclosing context
# (struct body, typedef) supplies the trailing `;` and any newline.
function unparse(io::IO, member::Pair{<:TypeSpec.Type, Vector{Declarator.Type}}, indent::Int=0)
    unparse(io, member.first, indent)
    print(io, ' ')
    first = true
    for d in member.second
        first || print(io, ", ")
        first = false
        unparse(io, d, indent)
    end
end

# Union case: label lines then the element line, all at `indent`. The trailing
# `;` follows the element on its own line per the parser's `case` rule.
function unparse(io::IO, c::Pair{Vector{UnionCaseLabel.Type}, <:UnionElement.Type}, indent::Int=0)
    pad = _pad(indent)
    first = true
    for lbl in c.first
        first || print(io, '\n')
        first = false
        print(io, pad)
        unparse(io, lbl, indent)
    end
    print(io, '\n', pad)
    unparse(io, c.second, indent)
    print(io, ';')
end

unparse(io::IO, td::TypeDecl.Type, indent::Int=0) = @match td begin
    TypeDecl.StructDecl(name, super, members) => begin
        print(io, "struct ", name)
        if super !== nothing
            print(io, " : ")
            unparse(io, super)
        end
        print(io, " {")
        inner_pad = _pad(indent + 1)
        for m in members
            print(io, '\n', inner_pad)
            unparse(io, m, indent + 1)
            print(io, ';')
        end
        if !isempty(members)
            print(io, '\n', _pad(indent))
        end
        print(io, '}')
    end
    TypeDecl.StructFwdDecl(name) => print(io, "struct ", name)
    TypeDecl.UnionDecl(name, switch, cases) => begin
        print(io, "union ", name, " switch (")
        unparse(io, switch, indent)
        print(io, ") {")
        for c in cases
            print(io, '\n')
            unparse(io, c, indent + 1)
        end
        if !isempty(cases)
            print(io, '\n', _pad(indent))
        end
        print(io, '}')
    end
    TypeDecl.UnionFwdDecl(name) => print(io, "union ", name)
    TypeDecl.EnumDecl(name, cases) => begin
        print(io, "enum ", name, " { ")
        first = true
        for c in cases
            first || print(io, ", ")
            first = false
            print(io, c)
        end
        print(io, " }")
    end
    TypeDecl.TypedefDecl(def, decls) => begin
        print(io, "typedef ")
        unparse(io, def, indent)
        print(io, ' ')
        first = true
        for d in decls
            first || print(io, ", ")
            first = false
            unparse(io, d, indent)
        end
    end
    TypeDecl.BitsetDecl(name, super, bfs) => begin
        print(io, "bitset ", name)
        if super !== nothing
            print(io, " : ")
            unparse(io, super)
        end
        print(io, " {")
        inner_pad = _pad(indent + 1)
        for bf in bfs
            print(io, '\n', inner_pad)
            unparse(io, bf, indent + 1)
            print(io, ';')
        end
        if !isempty(bfs)
            print(io, '\n', _pad(indent))
        end
        print(io, '}')
    end
    TypeDecl.BitmaskDecl(name, cases) => begin
        print(io, "bitmask ", name, " { ")
        first = true
        for c in cases
            first || print(io, ", ")
            first = false
            print(io, c)
        end
        print(io, " }")
    end
end

unparse(io::IO, c::ConstDecl.Type, indent::Int=0) = @match c begin
    ConstDecl.CDecl(typ, name, val) => begin
        print(io, "const ")
        unparse(io, typ, indent)
        print(io, ' ', name, " = ")
        unparse(io, val)
    end
end

unparse(io::IO, m::ModuleDecl.Type, indent::Int=0) = @match m begin
    ModuleDecl.MDecl(name, decls) => begin
        print(io, "module ", name, " {")
        inner_pad = _pad(indent + 1)
        for d in decls
            print(io, '\n', inner_pad)
            unparse(io, d, indent + 1)
            print(io, ';')
        end
        if !isempty(decls)
            print(io, '\n', _pad(indent))
        end
        print(io, '}')
    end
end

# Top-level entry for a parsed `specification` — every definition gets a
# trailing `;` (per the `definition` rule), one per line. Accepts AbstractVector
# because PEG.jl hands back `Vector{Any}` rather than a typed decl vector.
function unparse(io::IO, decls::AbstractVector, indent::Int=0)
    pad = _pad(indent)
    for d in decls
        print(io, pad)
        unparse(io, d, indent)
        print(io, ";\n")
    end
end
