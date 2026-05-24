# ROS2 interface-file parser. Reads `.msg` / `.srv` / `.action` text and emits
# the existing IDL AST (`Parse.Decl` types), so that piping the result through
# `Parse.unparse` produces valid OMG IDL v4. Implements the rosidl projection:
# cross-package message references resolve to `<package>::msg::Type`, and
# constants are hoisted out of structs into a sibling `<Name>_Constants` module
# (IDL doesn't allow `const` inside `struct`).
#
# Line-level structure (blank lines as separators, `#` line comments, `---`
# section markers in .srv/.action) is handled by a small driver; the per-line
# grammar — types, array suffixes, field names, scalar values — is a PEG.jl
# grammar reusing the same parser-combinator dialect as `src/parsing/parse.jl`.

using PEG
using Moshi.Match: @match

# ---- PEG grammar for a single ROS2 line ------------------------------------

# Whitespace-eater. ROS2 lines may end in trailing spaces/tabs; consume those.
@rule _ws = r"[ \t]*"p
@rule _ws1 = r"[ \t]+"p

# Identifiers and unsigned integer literals (for use in array bounds, etc.)
@rule _ident = r"[a-zA-Z_][a-zA-Z0-9_]*"p
@rule _uint_dec = r"[0-9]+"p |> s -> parse(Int, s)

# Primitive type names. Ordering matters: longer names ("float32") must precede
# shorter names ("float") if there's a prefix relationship — there isn't here,
# but PEG is ordered choice so list them explicitly.
@rule _prim_bool    = r"bool\b"p     |> _ -> Parse.TypeSpec.TBool()
@rule _prim_byte    = r"byte\b"p     |> _ -> Parse.TypeSpec.TOctet()
# `char` per ROS2 spec is an unsigned 8-bit integer ([0,255]).
@rule _prim_char    = r"char\b"p     |> _ -> Parse.TypeSpec.TUInt(8)
@rule _prim_int8    = r"int8\b"p     |> _ -> Parse.TypeSpec.TInt(8)
@rule _prim_uint8   = r"uint8\b"p    |> _ -> Parse.TypeSpec.TUInt(8)
@rule _prim_int16   = r"int16\b"p    |> _ -> Parse.TypeSpec.TInt(16)
@rule _prim_uint16  = r"uint16\b"p   |> _ -> Parse.TypeSpec.TUInt(16)
@rule _prim_int32   = r"int32\b"p    |> _ -> Parse.TypeSpec.TInt(32)
@rule _prim_uint32  = r"uint32\b"p   |> _ -> Parse.TypeSpec.TUInt(32)
@rule _prim_int64   = r"int64\b"p    |> _ -> Parse.TypeSpec.TInt(64)
@rule _prim_uint64  = r"uint64\b"p   |> _ -> Parse.TypeSpec.TUInt(64)
@rule _prim_f32     = r"float32\b"p  |> _ -> Parse.TypeSpec.TFloat(32)
@rule _prim_f64     = r"float64\b"p  |> _ -> Parse.TypeSpec.TFloat(64)

@rule _prim_scalar = (
    _prim_bool, _prim_byte, _prim_char,
    _prim_int8, _prim_uint8, _prim_int16, _prim_uint16,
    _prim_int32, _prim_uint32, _prim_int64, _prim_uint64,
    _prim_f32, _prim_f64)

# `string<=N` and `wstring<=N` (and the unbounded forms).
@rule _string_bounded  = r"string<="p   & _uint_dec  > (_, n) ->
    Parse.TypeSpec.TString(Parse.ConstExpr.Lit(Parse.Literal.Intg(n)))
@rule _wstring_bounded = r"wstring<="p  & _uint_dec  > (_, n) ->
    Parse.TypeSpec.TWString(Parse.ConstExpr.Lit(Parse.Literal.Intg(n)))
@rule _string_unbounded  = r"string\b"p   |> _ -> Parse.TypeSpec.TString(nothing)
@rule _wstring_unbounded = r"wstring\b"p  |> _ -> Parse.TypeSpec.TWString(nothing)

# `pkg/Name` is the rosidl convention for a cross-package message reference —
# the projection routes through `::msg::`, since cross-package refs in
# .msg/.srv/.action always point to message types living under
# `<package>::msg::Type` in the IDL projection.
@rule _absolute_ref = _ident & r"/"p & _ident > (pkg, _, name) ->
    Parse.TypeSpec.TRef(Parse.ScopedName.Name(
        [Symbol(pkg), :msg], Symbol(name), true))

# Legacy aliases preserved from ROS1. `rosidl_adapter` (the upstream tool
# that normalizes .msg files into .idl) unconditionally rewrites these,
# regardless of any same-package type with the same name; if a user wants
# their own `Header` they must write `<pkg>/Header` explicitly. We mirror
# that behavior verbatim.
@rule _builtin_time     = r"time\b"p     |> _ ->
    Parse.TypeSpec.TRef(Parse.ScopedName.Name([:builtin_interfaces, :msg], :Time, true))
@rule _builtin_duration = r"duration\b"p |> _ ->
    Parse.TypeSpec.TRef(Parse.ScopedName.Name([:builtin_interfaces, :msg], :Duration, true))
@rule _builtin_header   = r"Header\b"p   |> _ ->
    Parse.TypeSpec.TRef(Parse.ScopedName.Name([:std_msgs, :msg], :Header, true))

@rule _relative_ref = _ident |> s ->
    Parse.TypeSpec.TRef(Parse.ScopedName.Name(Symbol[], Symbol(s), true))

# Element type — primitive, string-with-bound, or reference. Ordered so that
# bounded forms win over unbounded, absolute over relative, and the legacy
# `time`/`duration` builtins are tried before the generic relative_ref
# (which would otherwise swallow them as plain identifier refs).
@rule _element_type = (
    _prim_scalar,
    _string_bounded, _wstring_bounded,
    _string_unbounded, _wstring_unbounded,
    _absolute_ref,
    _builtin_time, _builtin_duration, _builtin_header,
    _relative_ref)

# Array suffixes attached to the type token: `[N]`, `[]`, `[<=N]`.
# Each evaluates to a tagged tuple consumed by `_apply_array`. Use regex
# literals (with `p` to eat trailing whitespace) since PEG.jl's plain-string
# matchers don't anchor cleanly on the boundary between `int32` and `[3]`.
@rule _suffix_static    = r"\["p & _uint_dec & r"\]"p   > (_, n, _) -> (:static, n)
@rule _suffix_bounded   = r"\[<="p & _uint_dec & r"\]"p > (_, n, _) -> (:bounded, n)
@rule _suffix_unbounded = r"\[\]"p                      |> _ -> :unbounded
@rule _array_suffix = (_suffix_bounded, _suffix_static, _suffix_unbounded)

@rule _field_type = _element_type & _array_suffix[:?] > (base, suf) ->
    (base, isempty(suf) ? nothing : suf[1])

# `_field_type` (just the type token, used by constants which never have suffixes
# in valid ROS2). We still allow the suffix to parse so we don't fail; ignore it.
@rule _const_type = _element_type & _array_suffix[:?] > (base, _) -> base

# Combine a base type with the array suffix into the final
# `(TypeSpec, Declarator)` for a field.
function _apply_array(base_ts, suffix, field_name::Symbol)
    if suffix === nothing
        return (base_ts, Parse.Declarator.DIdent(field_name))
    elseif suffix === :unbounded
        return (Parse.TypeSpec.TSeq(base_ts, nothing),
                Parse.Declarator.DIdent(field_name))
    elseif suffix isa Tuple && suffix[1] === :bounded
        n = Parse.ConstExpr.Lit(Parse.Literal.Intg(suffix[2]))
        return (Parse.TypeSpec.TSeq(base_ts, n),
                Parse.Declarator.DIdent(field_name))
    elseif suffix isa Tuple && suffix[1] === :static
        n = Parse.ConstExpr.Lit(Parse.Literal.Intg(suffix[2]))
        return (base_ts, Parse.Declarator.DArray(field_name, [n]))
    else
        error("unknown array suffix: $suffix")
    end
end

# ---- Value literals (constants and default values) -------------------------
# The IDL parser has rich expression rules but ROS2 only allows literals (or
# array literals — see _wrap_default). These rules cover scalar cases.

@rule _bool_lit = (
    r"true\b"i  |> _ -> Parse.Literal.Bl(true),
    r"True\b"   |> _ -> Parse.Literal.Bl(true),
    r"false\b"i |> _ -> Parse.Literal.Bl(false),
    r"False\b"  |> _ -> Parse.Literal.Bl(false))

@rule _int_hex = r"[+-]?0[xX][0-9a-fA-F]+"p |> s -> begin
    m = match(r"^([+-]?)0[xX]([0-9a-fA-F]+)$", strip(s))
    (m[1] == "-" ? -1 : 1) * parse(Int, m[2]; base=16)
end
@rule _int_bin = r"[+-]?0[bB][01]+"p |> s -> begin
    m = match(r"^([+-]?)0[bB]([01]+)$", strip(s))
    (m[1] == "-" ? -1 : 1) * parse(Int, m[2]; base=2)
end
@rule _int_oct = r"[+-]?0[oO][0-7]+"p |> s -> begin
    m = match(r"^([+-]?)0[oO]([0-7]+)$", strip(s))
    (m[1] == "-" ? -1 : 1) * parse(Int, m[2]; base=8)
end
@rule _int_dec = r"[+-]?[0-9]+(?![0-9eE.])"p |> s -> parse(Int, strip(s))

@rule _int_lit = (_int_hex, _int_bin, _int_oct, _int_dec) |> n -> Parse.Literal.Intg(n)

# Float syntax requires either a decimal point or an exponent to distinguish
# from an integer; otherwise `42` would parse as a float too. Parse as F64 so
# precision survives when the declared field type is float64 — the coercion in
# `_parse_scalar_value` narrows to F32 (which is exact) when the field is
# float32.
@rule _float_raw = r"[+-]?(?:[0-9]+\.[0-9]*|\.[0-9]+|[0-9]+)(?:[eE][+-]?[0-9]+)"p
@rule _float_dot = r"[+-]?(?:[0-9]+\.[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?"p
@rule _float_lit = (_float_raw, _float_dot) |> s -> Parse.Literal.F64(parse(Float64, s))

# String literals: single- or double-quoted, with the usual escape sequences.
# PEG.jl wraps each regex with `^(...)\s*`, so a user pattern ending in `\`
# becomes `^(\)\s*` — the `\)` looks escaped to PCRE and the open paren has no
# match. Use `\\\\` (= regex `\\` = literal backslash) to avoid this trap.
@rule _esc_pair = r"\\\\"p & r"."s > (_, c) -> c == "n" ? "\n" :
                                                c == "t" ? "\t" :
                                                c == "r" ? "\r" :
                                                c == "\\" ? "\\" :
                                                c == "\"" ? "\"" :
                                                c == "'" ? "'" : "\\" * c
@rule _dquoted_body = ((-r"\""p & (_esc_pair, r"."s)) |> x -> x[2])[*] |> xs -> join(xs)
@rule _squoted_body = ((-r"'"p  & (_esc_pair, r"."s)) |> x -> x[2])[*] |> xs -> join(xs)
@rule _string_lit = (r"\""p & _dquoted_body & r"\""p > (_, b, _) -> b,
                     r"'"p  & _squoted_body & r"'"p  > (_, b, _) -> b) |>
                     s -> Parse.Literal.St(s)

# Generic scalar literal — order matters: try float first (with dot/exponent)
# before integer, since `3.14` should not be matched as `3` followed by `.14`.
@rule _scalar_lit = (_bool_lit, _string_lit, _float_lit, _int_lit)

# Wrap a raw `Literal` in `ConstExpr.Lit`. For float values we may need to
# coerce F32 → F64 if the declared type is `float64`; that fixup happens in
# the caller (after the value has been parsed).
_lit_to_expr(lit) = Parse.ConstExpr.Lit(lit)

# Strip an unquoted `#` line comment off a source line. The `#` inside a string
# literal is literal text, not a comment.
function _strip_comment(line::AbstractString)
    in_single = false
    in_double = false
    escape = false
    for (i, c) in pairs(line)
        if escape
            escape = false
            continue
        end
        if c == '\\' && (in_single || in_double)
            escape = true
            continue
        end
        if c == '"' && !in_single
            in_double = !in_double
        elseif c == '\'' && !in_double
            in_single = !in_single
        elseif c == '#' && !in_single && !in_double
            return line[1:prevind(line, i)]
        end
    end
    return line
end

# Parse a scalar value with the declared ROS2 type known. Picks the right
# Literal variant (F32 vs F64 for floats; Intg for any int base; Bl for bools;
# St for quoted strings). For unquoted bare strings (allowed by the spec for
# simple string defaults) and for anything not matching the grammar, falls
# back to a Literal.St of the raw token.
function _parse_scalar_value(raw::AbstractString, decl_type::Parse.TypeSpec.Type)
    s = strip(raw)
    # bool special-case: ROS2 accepts 0/1 as bool defaults too
    is_bool = @match decl_type begin
        Parse.TypeSpec.TBool() => true
        _ => false
    end
    if is_bool && s in ("0", "1")
        return Parse.ConstExpr.Lit(Parse.Literal.Bl(s == "1"))
    end
    parsed = try
        parse_whole(_scalar_lit, s)
    catch
        nothing
    end
    if parsed === nothing
        # Unquoted bare string fallback (per spec for simple string defaults).
        return Parse.ConstExpr.Lit(Parse.Literal.St(s))
    end
    # Coerce float width to match the declared field type — the literal parser
    # produces F32 by default, but a `float64` field needs F64. Also accept
    # an integer literal (`1`) as a default for a float-typed field.
    float_width = @match decl_type begin
        Parse.TypeSpec.TFloat(w) => w
        _ => 0
    end
    if float_width != 0
        parsed = @match parsed begin
            Parse.Literal.F32(v) => float_width == 64 ? Parse.Literal.F64(Float64(v)) : parsed
            Parse.Literal.F64(v) => float_width == 32 ? Parse.Literal.F32(Float32(v)) : parsed
            Parse.Literal.Intg(v) => float_width == 64 ?
                Parse.Literal.F64(Float64(v)) :
                Parse.Literal.F32(Float32(v))
            _ => parsed
        end
    end
    return Parse.ConstExpr.Lit(parsed)
end

# ---- Line-level parsing ----------------------------------------------------

# A line is either a constant (`<type> NAME=value`), a field with default
# (`<type> name value`), or a bare field (`<type> name`). We parse each
# variant separately and try them in order.

# Helper: parse the type portion of a line. PEG.jl's `p` suffix on the inner
# regex rules already consumes trailing whitespace, so no extra `_ws` is needed.
@rule _type_token = _field_type
@rule _const_type_token = _const_type

# Capture-all rules: the value half of constants and defaults can hold
# arbitrary text (with embedded spaces — e.g. `"hello world"` or `[1, 2, 3]`),
# so consume the rest of the line as an opaque string and parse it separately
# with `_parse_scalar_value`.
@rule _rest_of_line = r"[^\n\r]*"p
# Like `_rest_of_line` but requires at least one non-space character so the
# parser commits to "this is a default" only when there's actually a value.
@rule _rest_of_line_nonempty = r"[^\s][^\n\r]*"p

# Constant: `<type> NAME = <value>`. The `=` may or may not have surrounding
# whitespace; the `p` suffix on `_ident` already eats trailing spaces.
@rule _const_line = _const_type_token & _ident & r"="p & _rest_of_line >
    (ts, name, _, val) -> (:const, ts, Symbol(name), strip(val))

# Field with default: `<type> name <value>` — the value is non-empty.
@rule _field_with_default_line = _type_token & _ident & _rest_of_line_nonempty >
    (ft, name, val) -> (:field_default, ft, Symbol(name), strip(val))

# Field without default: `<type> name`
@rule _field_bare_line = _type_token & _ident > (ft, name) -> (:field, ft, Symbol(name))

# Try const first (the `=` disambiguates), then field-with-default, then bare.
@rule _line = (_const_line, _field_with_default_line, _field_bare_line)

# Parse a single non-comment, non-blank line into an intermediate tag tuple.
function _parse_line(line::AbstractString)
    stripped = strip(_strip_comment(line))
    isempty(stripped) && return nothing
    try
        return parse_whole(_line, stripped)
    catch e
        e isa Base.Meta.ParseError || rethrow()
        error("ROS2 parse error on line `$stripped`: " * e.msg)
    end
end

# Build the `@default(value=...)` annotation wrapping a field member. Array
# defaults — e.g. `int32[3] foo [1, 2, 3]` — don't have a corresponding IDL
# expression form (no array-literal `ConstExpr` variant), so as a tactical
# workaround we emit the raw source text as a string literal. Documented as
# such inline.
function _wrap_default(member, default_raw::AbstractString, base_ts, suffix)
    expr = if suffix === nothing
        _parse_scalar_value(default_raw, base_ts)
    else
        # Array default: pass through raw source as a string. See note above.
        Parse.ConstExpr.Lit(Parse.Literal.St(strip(default_raw)))
    end
    ann_name = Parse.ScopedName.Name(Symbol[], :default, true)
    return Parse.Annotated.Annotation(ann_name,
        [:value => expr],
        member)
end

# Parse a section's source text into `(constants, members)`.
function _parse_section(src::AbstractString)
    constants = Parse.ConstDecl.Type[]
    members = Any[]
    for line in split(src, '\n')
        parsed = _parse_line(line)
        parsed === nothing && continue
        tag = parsed[1]
        if tag === :const
            _, ts, name, val = parsed
            push!(constants,
                Parse.ConstDecl.CDecl(ts, name, _parse_scalar_value(val, ts)))
        elseif tag === :field
            _, (base_ts, suffix), name = parsed
            ts, decl = _apply_array(base_ts, suffix, name)
            push!(members, ts => Parse.Declarator.Type[decl])
        elseif tag === :field_default
            _, (base_ts, suffix), name, default = parsed
            ts, decl = _apply_array(base_ts, suffix, name)
            member = ts => Parse.Declarator.Type[decl]
            push!(members, _wrap_default(member, default, base_ts, suffix))
        else
            error("unexpected line tag: $tag")
        end
    end
    return (constants, members)
end

# Assemble the IDL decls for a single message: a `<Name>_Constants` module if
# there are any constants, then the struct.
function _build_message_decls(name::AbstractString, constants, members)
    decls = Any[]
    if !isempty(constants)
        constants_name = Symbol(string(name, "_Constants"))
        push!(decls, Parse.ModuleDecl.MDecl(constants_name,
            Vector{Parse.CanAnnotate{Parse.Decl}}(constants)))
    end
    sname = Symbol(name)
    members_typed = Vector{Parse.CanAnnotate{
        Pair{Parse.TypeSpec.Type, Vector{Parse.Declarator.Type}}}}(members)
    push!(decls, Parse.TypeDecl.StructDecl(sname, nothing, members_typed))
    return decls
end

# Wrap inner decls in `module <package> { module <submodule> { ... } }` if
# `package` is non-empty. `submodule` is `:msg` / `:srv` / `:action`.
function _wrap_package(inner_decls, package::AbstractString, submodule::Symbol)
    isempty(package) && return inner_decls
    inner_typed = Vector{Parse.CanAnnotate{Parse.Decl}}(inner_decls)
    sub = Parse.ModuleDecl.MDecl(submodule, inner_typed)
    outer = Parse.ModuleDecl.MDecl(Symbol(package),
        Vector{Parse.CanAnnotate{Parse.Decl}}([sub]))
    return Any[outer]
end

"""
    parse_msg(source; name, package="")

Parse the contents of a ROS2 `.msg` file into a vector of IDL declarations.
"""
function parse_msg(source::AbstractString; name::AbstractString,
                   package::AbstractString="")
    constants, members = _parse_section(source)
    inner = _build_message_decls(name, constants, members)
    return _wrap_package(inner, package, :msg)
end

# Split a `.srv` / `.action` source on lines that are exactly `---` (after
# stripping comments + whitespace).
function _split_on_marker(source::AbstractString)
    sections = String[]
    current = IOBuffer()
    for line in split(source, '\n')
        if strip(_strip_comment(line)) == "---"
            push!(sections, String(take!(current)))
            current = IOBuffer()
        else
            println(current, line)
        end
    end
    push!(sections, String(take!(current)))
    return sections
end

"""
    parse_srv(source; name, package="")

Parse a ROS2 `.srv` source into request and response struct decls.
"""
function parse_srv(source::AbstractString; name::AbstractString,
                   package::AbstractString="")
    sections = _split_on_marker(source)
    length(sections) == 2 || error(".srv source must have exactly one `---` separator")
    req_consts, req_members = _parse_section(sections[1])
    res_consts, res_members = _parse_section(sections[2])
    inner = Any[]
    append!(inner, _build_message_decls(string(name, "_Request"),
        req_consts, req_members))
    append!(inner, _build_message_decls(string(name, "_Response"),
        res_consts, res_members))
    return _wrap_package(inner, package, :srv)
end

"""
    parse_action(source; name, package="")

Parse a ROS2 `.action` source into goal, result, and feedback struct decls.
"""
function parse_action(source::AbstractString; name::AbstractString,
                      package::AbstractString="")
    sections = _split_on_marker(source)
    length(sections) == 3 || error(".action source must have exactly two `---` separators")
    parts = [_parse_section(s) for s in sections]
    inner = Any[]
    for (suffix, (consts, members)) in zip(("Goal", "Result", "Feedback"), parts)
        append!(inner, _build_message_decls(string(name, "_", suffix),
            consts, members))
    end
    return _wrap_package(inner, package, :action)
end

"""
    parse_file(path; package="")

Dispatch on file extension (`.msg`, `.srv`, `.action`) and parse accordingly.
The message name is taken from the basename.
"""
function parse_file(path::AbstractString; package::AbstractString="")
    src = read(path, String)
    base = basename(path)
    name, ext = splitext(base)
    if ext == ".msg"
        return parse_msg(src; name=name, package=package)
    elseif ext == ".srv"
        return parse_srv(src; name=name, package=package)
    elseif ext == ".action"
        return parse_action(src; name=name, package=package)
    else
        error("unknown ROS2 interface extension: $ext")
    end
end
