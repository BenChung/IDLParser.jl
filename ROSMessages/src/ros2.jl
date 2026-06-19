# ROS2 `.msg` / `.srv` / `.action` interfaces, in three independent steps:
#
#   text  ──parse──▶  IL  ──lower──▶  Parse.Decl   (forward; `parse_msg` etc.)
#   text  ◀─unparse─  IL  ◀──lift───  Parse.Decl   (backward; `to_ros`)
#
# The IL (`il.jl`) models ROS concepts directly; the two ends of the pipe only
# ever speak to it, never to each other. Forward and backward are exact
# inverses except where ROS and IDL genuinely disagree (the `char`/`uint8` and
# `byte`/`octet` spellings collapse, and the `time`/`duration`/`Header` aliases
# come back as their fully-qualified references — all semantically identical).
#
# IDL lowering: constants are hoisted into a sibling `<Name>_Constants` module
# because IDL forbids `const` inside `struct`; cross-package refs (`pkg/Name`)
# project to `pkg::msg::Name`.

using PEG
using Moshi.Match: @match

@rule _ws = r"[ \t]*"p
@rule _ws1 = r"[ \t]+"p
@rule _ident = r"[a-zA-Z_][a-zA-Z0-9_]*"p
@rule _uint_dec = r"[0-9]+"p |> s -> parse(Int, s)

@rule _prim_bool    = r"bool\b"p     |> _ -> IL.RBase.RBool()
@rule _prim_byte    = r"byte\b"p     |> _ -> IL.RBase.RByte()
@rule _prim_char    = r"char\b"p     |> _ -> IL.RBase.RChar()
@rule _prim_int8    = r"int8\b"p     |> _ -> IL.RBase.RInt(8)
@rule _prim_uint8   = r"uint8\b"p    |> _ -> IL.RBase.RUInt(8)
@rule _prim_int16   = r"int16\b"p    |> _ -> IL.RBase.RInt(16)
@rule _prim_uint16  = r"uint16\b"p   |> _ -> IL.RBase.RUInt(16)
@rule _prim_int32   = r"int32\b"p    |> _ -> IL.RBase.RInt(32)
@rule _prim_uint32  = r"uint32\b"p   |> _ -> IL.RBase.RUInt(32)
@rule _prim_int64   = r"int64\b"p    |> _ -> IL.RBase.RInt(64)
@rule _prim_uint64  = r"uint64\b"p   |> _ -> IL.RBase.RUInt(64)
@rule _prim_f32     = r"float32\b"p  |> _ -> IL.RBase.RFloat(32)
@rule _prim_f64     = r"float64\b"p  |> _ -> IL.RBase.RFloat(64)

@rule _prim_scalar = (
    _prim_bool, _prim_byte, _prim_char,
    _prim_int8, _prim_uint8, _prim_int16, _prim_uint16,
    _prim_int32, _prim_uint32, _prim_int64, _prim_uint64,
    _prim_f32, _prim_f64)

@rule _string_bounded  = r"string<="p   & _uint_dec  > (_, n) -> IL.RBase.RStr(n)
@rule _wstring_bounded = r"wstring<="p  & _uint_dec  > (_, n) -> IL.RBase.RWStr(n)
@rule _string_unbounded  = r"string\b"p   |> _ -> IL.RBase.RStr(nothing)
@rule _wstring_unbounded = r"wstring\b"p  |> _ -> IL.RBase.RWStr(nothing)

@rule _absolute_ref = _ident & r"/"p & _ident > (pkg, _, name) ->
    IL.RBase.RRef(String(pkg), String(name), "msg")

# Legacy ROS1 aliases. `rosidl_adapter` rewrites these unconditionally even
# in the presence of a same-package type with the same name, so we do too;
# a user wanting their own `Header` must spell it `<pkg>/Header`.
@rule _builtin_time     = r"time\b"p     |> _ -> IL.RBase.RRef("builtin_interfaces", "Time", "msg")
@rule _builtin_duration = r"duration\b"p |> _ -> IL.RBase.RRef("builtin_interfaces", "Duration", "msg")
@rule _builtin_header   = r"Header\b"p   |> _ -> IL.RBase.RRef("std_msgs", "Header", "msg")

@rule _relative_ref = _ident |> s -> IL.RBase.RRef(nothing, String(s), "msg")

# Order matters: PEG alternation is first-match. Bounded forms must precede
# unbounded, absolute_ref must precede relative_ref, and the legacy builtin
# aliases must precede relative_ref or it'd swallow `time`/`duration`/`Header`
# as plain identifier refs.
@rule _element_type = (
    _prim_scalar,
    _string_bounded, _wstring_bounded,
    _string_unbounded, _wstring_unbounded,
    _absolute_ref,
    _builtin_time, _builtin_duration, _builtin_header,
    _relative_ref)

@rule _suffix_static    = r"\["p & _uint_dec & r"\]"p   > (_, n, _) -> IL.ArraySpec.AStatic(n)
@rule _suffix_bounded   = r"\[<="p & _uint_dec & r"\]"p > (_, n, _) -> IL.ArraySpec.ABounded(n)
@rule _suffix_unbounded = r"\[\]"p                      |> _ -> IL.ArraySpec.AUnbounded()
@rule _array_suffix = (_suffix_bounded, _suffix_static, _suffix_unbounded)

@rule _field_type = _element_type & _array_suffix[:?] > (base, suf) ->
    IL.RType(base, isempty(suf) ? IL.ArraySpec.AScalar() : suf[1])

# Constants never carry an array suffix in valid ROS2; accept and ignore one
# so a malformed line errors at the value-side rules instead of the type-side.
@rule _const_type = _element_type & _array_suffix[:?] > (base, _) -> base

# ---- Value literals (constants and default values) -------------------------
# The IL only carries literals (or, for array defaults, opaque source text);
# these rules cover the scalar cases. Float width is *not* resolved here — the
# IL keeps the natural literal and IDL lowering narrows it to the field type.

@rule _bool_lit = (
    r"true\b"i  |> _ -> IL.RValue.VBool(true),
    r"True\b"   |> _ -> IL.RValue.VBool(true),
    r"false\b"i |> _ -> IL.RValue.VBool(false),
    r"False\b"  |> _ -> IL.RValue.VBool(false))

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

@rule _int_lit = (_int_hex, _int_bin, _int_oct, _int_dec) |> n -> IL.RValue.VInt(n)

# Float syntax requires either a decimal point or an exponent to distinguish
# from an integer; otherwise `42` would parse as a float too. The IL stores it
# as a Float64; lowering picks F32 vs F64 from the declared field type.
@rule _float_raw = r"[+-]?(?:[0-9]+\.[0-9]*|\.[0-9]+|[0-9]+)(?:[eE][+-]?[0-9]+)"p
@rule _float_dot = r"[+-]?(?:[0-9]+\.[0-9]*|\.[0-9]+)(?:[eE][+-]?[0-9]+)?"p
@rule _float_lit = (_float_raw, _float_dot) |> s -> IL.RValue.VFloat(parse(Float64, s))

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
                     s -> IL.RValue.VString(s)

# Generic scalar literal — order matters: try float first (with dot/exponent)
# before integer, since `3.14` should not be matched as `3` followed by `.14`.
@rule _scalar_lit = (_bool_lit, _string_lit, _float_lit, _int_lit)

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

# Parse a scalar value into an IL value, given the declared base type. Bools
# accept `0`/`1`; anything that doesn't match the literal grammar falls back to
# a bare string (per the ROS2 spec for simple string defaults).
function _parse_scalar_value(raw::AbstractString, base::IL.RBase.Type)
    s = strip(raw)
    is_bool = @match base begin
        IL.RBase.RBool() => true
        _ => false
    end
    if is_bool && s in ("0", "1")
        return IL.RValue.VBool(s == "1")
    end
    parsed = try
        parse_whole(_scalar_lit, s)
    catch
        nothing
    end
    parsed === nothing && return IL.RValue.VString(s)
    return parsed
end

# ---- Line-level parsing ----------------------------------------------------

# A line is either a constant (`<type> NAME=value`), a field with default
# (`<type> name value`), or a bare field (`<type> name`).

@rule _type_token = _field_type
@rule _const_type_token = _const_type

# Capture-all rules: the value half of constants and defaults can hold
# arbitrary text (with embedded spaces — e.g. `"hello world"` or `[1, 2, 3]`),
# so consume the rest of the line as an opaque string and parse it separately.
@rule _rest_of_line = r"[^\n\r]*"p
# Like `_rest_of_line` but requires at least one non-space character so the
# parser commits to "this is a default" only when there's actually a value.
@rule _rest_of_line_nonempty = r"[^\s][^\n\r]*"p

@rule _const_line = _const_type_token & _ident & r"="p & _rest_of_line >
    (base, name, _, val) -> (:const, base, Symbol(name), strip(val))

@rule _field_with_default_line = _type_token & _ident & _rest_of_line_nonempty >
    (ft, name, val) -> (:field_default, ft, Symbol(name), strip(val))

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

_is_scalar(a::IL.ArraySpec.Type) = (@match a begin
    IL.ArraySpec.AScalar() => true
    _ => false
end)

# Parse a section's source text into an `(constants, fields)` IL pair.
function _parse_section(src::AbstractString)
    constants = IL.RConstant[]
    fields = IL.RField[]
    for line in split(src, '\n')
        parsed = _parse_line(line)
        parsed === nothing && continue
        tag = parsed[1]
        if tag === :const
            _, base, name, val = parsed
            push!(constants, IL.RConstant(base, name, _parse_scalar_value(val, base)))
        elseif tag === :field
            _, rtype, name = parsed
            push!(fields, IL.RField(rtype, name, nothing))
        elseif tag === :field_default
            _, rtype, name, raw = parsed
            # Array defaults — e.g. `int32[3] foo [1, 2, 3]` — have no
            # structured literal form, so carry the source text verbatim.
            default = _is_scalar(rtype.array) ?
                _parse_scalar_value(raw, rtype.base) : IL.RValue.VRaw(raw)
            push!(fields, IL.RField(rtype, name, default))
        else
            error("unexpected line tag: $tag")
        end
    end
    return (constants, fields)
end

# ---- Building IL interfaces from source ------------------------------------

# An empty (fieldless) message gets the rosidl synthetic member via
# `IL.nonempty_fields` (the shared convention; see il.jl), so the parse path matches
# the reflection path and rosidl.
build_message(name, constants, fields) =
    IL.RMessage(Symbol(name), constants, IL.nonempty_fields(fields))

"""
    message_il(source; name) -> IL.RMessage

Parse a ROS2 `.msg` source into the interface IL.
"""
function message_il(source::AbstractString; name::AbstractString)
    constants, fields = _parse_section(source)
    return build_message(name, constants, fields)
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
    service_il(source; name) -> IL.RService

Parse a ROS2 `.srv` source into the interface IL.
"""
function service_il(source::AbstractString; name::AbstractString)
    sections = _split_on_marker(source)
    length(sections) == 2 || error(".srv source must have exactly one `---` separator")
    req_consts, req_fields = _parse_section(sections[1])
    res_consts, res_fields = _parse_section(sections[2])
    return IL.RService(Symbol(name),
        build_message(string(name, "_Request"), req_consts, req_fields),
        build_message(string(name, "_Response"), res_consts, res_fields))
end

"""
    action_il(source; name) -> IL.RAction

Parse a ROS2 `.action` source into the interface IL.
"""
function action_il(source::AbstractString; name::AbstractString)
    sections = _split_on_marker(source)
    length(sections) == 3 || error(".action source must have exactly two `---` separators")
    cf = [_parse_section(s) for s in sections]
    return IL.RAction(Symbol(name),
        build_message(string(name, "_Goal"), cf[1]...),
        build_message(string(name, "_Result"), cf[2]...),
        build_message(string(name, "_Feedback"), cf[3]...))
end

# ---- Lowering: IL → Parse.Decl ---------------------------------------------

_base_to_ts(base::IL.RBase.Type) = @match base begin
    IL.RBase.RBool() => Parse.TypeSpec.TBool()
    IL.RBase.RByte() => Parse.TypeSpec.TOctet()
    # `char` per ROS2 spec is an unsigned 8-bit integer.
    IL.RBase.RChar() => Parse.TypeSpec.TUInt(8)
    IL.RBase.RInt(w) => Parse.TypeSpec.TInt(w)
    IL.RBase.RUInt(w) => Parse.TypeSpec.TUInt(w)
    IL.RBase.RFloat(w) => Parse.TypeSpec.TFloat(w)
    IL.RBase.RStr(b) => Parse.TypeSpec.TString(
        b === nothing ? nothing : Parse.ConstExpr.Lit(Parse.Literal.Intg(b)))
    IL.RBase.RWStr(b) => Parse.TypeSpec.TWString(
        b === nothing ? nothing : Parse.ConstExpr.Lit(Parse.Literal.Intg(b)))
    IL.RBase.RRef(pkg, name, qual) => Parse.TypeSpec.TRef(
        pkg === nothing ?
            Parse.ScopedName.Name(Symbol[], Symbol(name), true) :
            Parse.ScopedName.Name([Symbol(pkg), Symbol(qual)], Symbol(name), true))
end

# Lower an IL value to a `ConstExpr`. Float-typed targets narrow the value's
# width (and accept an integer literal as a float default), matching the spec.
function _value_to_expr(v::IL.RValue.Type, base::IL.RBase.Type)
    fw = @match base begin
        IL.RBase.RFloat(w) => w
        _ => 0
    end
    @match v begin
        IL.RValue.VBool(b) => Parse.ConstExpr.Lit(Parse.Literal.Bl(b))
        IL.RValue.VString(s) => Parse.ConstExpr.Lit(Parse.Literal.St(s))
        IL.RValue.VRaw(s) => Parse.ConstExpr.Lit(Parse.Literal.St(s))
        IL.RValue.VInt(n) =>
            fw == 64 ? Parse.ConstExpr.Lit(Parse.Literal.F64(Float64(n))) :
            fw == 32 ? Parse.ConstExpr.Lit(Parse.Literal.F32(Float32(n))) :
                       Parse.ConstExpr.Lit(Parse.Literal.Intg(n))
        IL.RValue.VFloat(x) =>
            fw == 32 ? Parse.ConstExpr.Lit(Parse.Literal.F32(Float32(x))) :
                       Parse.ConstExpr.Lit(Parse.Literal.F64(x))
    end
end

function _apply_array(base_ts, arr::IL.ArraySpec.Type, name::Symbol)
    @match arr begin
        IL.ArraySpec.AScalar() => (base_ts, Parse.Declarator.DIdent(name))
        IL.ArraySpec.AUnbounded() =>
            (Parse.TypeSpec.TSeq(base_ts, nothing), Parse.Declarator.DIdent(name))
        IL.ArraySpec.ABounded(n) =>
            (Parse.TypeSpec.TSeq(base_ts, Parse.ConstExpr.Lit(Parse.Literal.Intg(n))),
             Parse.Declarator.DIdent(name))
        IL.ArraySpec.AStatic(n) =>
            (base_ts, Parse.Declarator.DArray(name,
                [Parse.ConstExpr.Lit(Parse.Literal.Intg(n))]))
    end
end

function _wrap_default(member, expr)
    ann_name = Parse.ScopedName.Name(Symbol[], :default, true)
    return Parse.Annotated.Annotation(ann_name, [:value => expr], member)
end

function _lower_field(f::IL.RField)
    base_ts = _base_to_ts(f.type.base)
    ts, decl = _apply_array(base_ts, f.type.array, f.name)
    member = ts => Parse.Declarator.Type[decl]
    f.default === nothing && return member
    return _wrap_default(member, _value_to_expr(f.default, f.type.base))
end

_lower_constant(c::IL.RConstant) =
    Parse.ConstDecl.CDecl(_base_to_ts(c.type), c.name, _value_to_expr(c.value, c.type))

# A `<Name>_Constants` module (if there are any constants) then the struct.
function _lower_message_decls(msg::IL.RMessage)
    decls = Any[]
    if !isempty(msg.constants)
        constants_name = Symbol(string(msg.name, "_Constants"))
        consts = Parse.ConstDecl.Type[_lower_constant(c) for c in msg.constants]
        push!(decls, Parse.ModuleDecl.MDecl(constants_name,
            Vector{Parse.CanAnnotate{Parse.Decl}}(consts)))
    end
    members = Any[_lower_field(f) for f in msg.fields]
    members_typed = Vector{Parse.CanAnnotate{
        Pair{Parse.TypeSpec.Type, Vector{Parse.Declarator.Type}}}}(members)
    push!(decls, Parse.TypeDecl.StructDecl(msg.name, nothing, members_typed))
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
    lower(interface; package="") -> Vector

Lower an IL interface (`RMessage`/`RService`/`RAction`) into the IDL
`Parse.Decl` vector, optionally wrapped in a `<package>` module.
"""
lower(msg::IL.RMessage; package::AbstractString="") =
    _wrap_package(_lower_message_decls(msg), package, :msg)

function lower(srv::IL.RService; package::AbstractString="")
    inner = Any[]
    append!(inner, _lower_message_decls(srv.request))
    append!(inner, _lower_message_decls(srv.response))
    return _wrap_package(inner, package, :srv)
end

function lower(act::IL.RAction; package::AbstractString="")
    inner = Any[]
    append!(inner, _lower_message_decls(act.goal))
    append!(inner, _lower_message_decls(act.result))
    append!(inner, _lower_message_decls(act.feedback))
    return _wrap_package(inner, package, :action)
end

# ---- Public forward API ----------------------------------------------------

"""
    parse_msg(source; name, package="")

Parse the contents of a ROS2 `.msg` file into a vector of IDL declarations.

ROS2 interface definitions: https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html
"""
parse_msg(source::AbstractString; name::AbstractString, package::AbstractString="") =
    lower(message_il(source; name=name); package=package)

"""
    parse_srv(source; name, package="")

Parse a ROS2 `.srv` source into request and response struct decls.
"""
parse_srv(source::AbstractString; name::AbstractString, package::AbstractString="") =
    lower(service_il(source; name=name); package=package)

"""
    parse_action(source; name, package="")

Parse a ROS2 `.action` source into goal, result, and feedback struct decls.
"""
parse_action(source::AbstractString; name::AbstractString, package::AbstractString="") =
    lower(action_il(source; name=name); package=package)

# The implicit interface types rosidl derives from every `.action` — the wire
# types an action client and server actually exchange, on top of the user's
# Goal/Result/Feedback. Each goal carries a `unique_identifier_msgs/UUID`; the
# accept reply stamps a `builtin_interfaces/Time`. Field names match
# rosidl_adapter so the generated structs (and their RIHS01) are wire-compatible.
function _action_protocol_messages(name::AbstractString)
    uuid    = IL.RType(IL.RBase.RRef("unique_identifier_msgs", "UUID", "msg"), IL.ArraySpec.AScalar())
    time    = IL.RType(IL.RBase.RRef("builtin_interfaces", "Time", "msg"), IL.ArraySpec.AScalar())
    boolean = IL.RType(IL.RBase.RBool(), IL.ArraySpec.AScalar())
    int8    = IL.RType(IL.RBase.RInt(8), IL.ArraySpec.AScalar())
    # A same-package (relative) ref to one of the three sections (action-qualified; the relative
    # form lowers to a bare name resolved within the surrounding `action` package).
    section(sec) = IL.RType(IL.RBase.RRef(nothing, string(name, sec), "action"), IL.ArraySpec.AScalar())
    fld(t, n) = IL.RField(t, n, nothing)
    msg(suffix, fields) = IL.RMessage(Symbol(name, suffix), IL.RConstant[], fields)
    return IL.RMessage[
        msg("_SendGoal_Request",   [fld(uuid, :goal_id), fld(section("_Goal"), :goal)]),
        msg("_SendGoal_Response",  [fld(boolean, :accepted), fld(time, :stamp)]),
        msg("_GetResult_Request",  [fld(uuid, :goal_id)]),
        msg("_GetResult_Response", [fld(int8, :status), fld(section("_Result"), :result)]),
        msg("_FeedbackMessage",    [fld(uuid, :goal_id), fld(section("_Feedback"), :feedback)]),
    ]
end

"""
    action_protocol_decls(name; package="") -> Vector

The IDL decls for the implicit action-protocol types rosidl derives from a
`<name>.action`: `<name>_SendGoal_{Request,Response}`,
`<name>_GetResult_{Request,Response}`, and `<name>_FeedbackMessage`. These are
emitted alongside the goal/result/feedback sections (e.g. by `@ros_msgs`) so a
generated action is usable end-to-end. They reference `unique_identifier_msgs/
UUID` and `builtin_interfaces/Time`, which must be in the same generation set.

Kept separate from [`parse_action`](@ref) — which is the faithful text↔IDL
projection of the three `.action` sections — because these are codegen
artifacts, not part of the source text.
"""
function action_protocol_decls(name::AbstractString; package::AbstractString="")
    inner = Any[]
    for m in _action_protocol_messages(name)
        append!(inner, _lower_message_decls(m))
    end
    return _wrap_package(inner, package, :action)
end

"""
    InterfaceMarker

Supertype of the empty tag struct generated for each service/action (`Foo`). The short-name
section aliases (`Foo.Request`, `Foo.Goal`, …) resolve through the *single*
`Base.getproperty(::Type{<:InterfaceMarker}, ::Symbol)` method below — NOT a per-marker
overload.

This is a precompile fix: a `getproperty(::Type{Foo}, ::Symbol)` method makes the generic
`getproperty(::DataType, ::Symbol)` non-exhaustive, invalidating its compiled callers. Generated
*per* interface (in ROSNode's own type generation), each one re-invalidates the `getproperty(::
DataType,::Symbol)` callers on the node bring-up path — measured at ~58 ms of forced recompilation
(`precompile_blockers`, the #1 startup-path invalidation blocker). One method, defined here in
ROSMessages (loaded before ROSNode), is present when ROSNode's image is built, so those callers
bake accounting for it and are not invalidated at load.
"""
abstract type InterfaceMarker end

# `Foo.Request`/`Foo.Goal`/… → the sibling section struct `Foo_<short>` in `Foo`'s own module,
# reconstructed by name (constant-folds when `M`/`s` are literals — the common case, `Fibonacci.
# Goal`). Falls back to `getfield` so DataType introspection (`Foo.name`/`Foo.parameters`) works.
# Name/module read via `getfield` on the `Core.TypeName`, NOT `nameof`/`parentmodule`: those go
# through `M.name` (getproperty), which would re-enter this method and stack-overflow (every
# introspection of a marker type dispatches here).
@inline function Base.getproperty(::Type{M}, s::Symbol) where {M <: InterfaceMarker}
    tn = getfield(M, :name)::Core.TypeName
    sect = Symbol(getfield(tn, :name), :_, s)
    pm = getfield(tn, :module)
    return isdefined(pm, sect) ? getfield(pm, sect) : getfield(M, s)
end

"""
    namespace_alias_decls(kind, name) -> Vector{Expr}

Julia-side namespacing for a service/action: a `<name>` tag struct (subtyping
[`InterfaceMarker`](@ref)) whose short-name properties map to the generated section structs, so
`Foo.Request`/`Foo.Goal` read better than the `Foo_Request`/`Foo_Goal` structs. The ROS/wire type
names stay unchanged. The `.Request`/`.Goal` resolution is the single shared `getproperty` method
on `InterfaceMarker` (this only emits the tag struct). Splice the returned exprs *inside* the
generated `<pkg>.<kind>` module — see `_inject_namespaces!`. A `msg` yields an empty vector.
"""
function namespace_alias_decls(kind::AbstractString, name::AbstractString)
    (kind == "srv" || kind == "action") || return Expr[]
    # `struct Foo <: InterfaceMarker end` — still a *type* (action handle / type parameter); the
    # supertype routes `Foo.Request`/`Foo.Goal` through the one shared `getproperty` (no per-marker
    # overload → no per-interface invalidation of `getproperty(::DataType,::Symbol)`). Spliced as a
    # GlobalRef so `InterfaceMarker` resolves regardless of the generated module's imports.
    return Expr[Expr(:struct, false,
                     Expr(:(<:), Symbol(name), GlobalRef(@__MODULE__, :InterfaceMarker)),
                     Expr(:block))]
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

# ---- Lifting: Parse.Decl → IL ----------------------------------------------

# `(name, decls)` if `d` is a module, else `nothing`.
function _as_module(d)
    d isa Parse.ModuleDecl.Type || return nothing
    @match d begin
        Parse.ModuleDecl.MDecl(n, ds) => (n, ds)
    end
end

# `(name, members)` if `d` is a struct decl, else `nothing`.
function _as_struct(d)
    d isa Parse.TypeDecl.Type || return nothing
    @match d begin
        Parse.TypeDecl.StructDecl(n, _, ms) => (n, ms)
        _ => nothing
    end
end

_struct_names(decls) = Symbol[s[1] for s in (_as_struct(d) for d in decls) if s !== nothing]

# Decide whether a bare (un-package-wrapped) decl list is a msg/srv/action,
# from the suffixes of its struct names.
function _infer_kind(decls)
    names = Set(string.(_struct_names(decls)))
    has(suf) = any(endswith(n, suf) for n in names)
    if has("_Goal") && has("_Result") && has("_Feedback")
        return :action
    elseif has("_Request") && has("_Response")
        return :srv
    else
        return :msg
    end
end

# Peel an optional `module <pkg> { module <msg|srv|action> { ... } }` wrapper,
# returning `(package, kind, inner_decls)`.
function _unwrap_package(decls)
    if length(decls) == 1
        m = _as_module(decls[1])
        if m !== nothing && length(m[2]) == 1
            sm = _as_module(m[2][1])
            if sm !== nothing && sm[1] in (:msg, :srv, :action)
                return (string(m[1]), sm[1], sm[2])
            end
        end
    end
    return (nothing, _infer_kind(decls), decls)
end

_scoped_name(name) = (@match name begin
    Parse.ScopedName.Name(path, n, _) => (path, n)
end)

# Walk `Annotated` wrappers, returning `(inner, default_expr)` where
# `default_expr` is the `@default(value=...)` argument if present.
function _peel_annotated(x)
    default = nothing
    while x isa Parse.Annotated.Type
        x = @match x begin
            Parse.Annotated.Annotation(name, params, inner) => begin
                if _scoped_name(name)[2] === :default
                    for (k, v) in params
                        k === :value && (default = v)
                    end
                end
                inner
            end
        end
    end
    return (x, default)
end

_expr_int(expr) = (@match expr begin
    Parse.ConstExpr.Lit(Parse.Literal.Intg(n)) => Int(n)
    _ => error("expected integer literal, got $(expr)")
end)

function _ref_to_base(scoped)
    path, n = _scoped_name(scoped)
    if isempty(path)
        return IL.RBase.RRef(nothing, string(n), "msg")
    elseif length(path) >= 2
        # `pkg/<qual>/Name`: the qualifier is the last path segment, the owning
        # package the one before it — preserved so action-section refs survive.
        return IL.RBase.RRef(string(path[end-1]), string(n), string(path[end]))
    else
        return IL.RBase.RRef(string(path[1]), string(n), "msg")
    end
end

_ts_to_base(ts) = @match ts begin
    Parse.TypeSpec.TBool() => IL.RBase.RBool()
    Parse.TypeSpec.TOctet() => IL.RBase.RByte()
    Parse.TypeSpec.TChar() => IL.RBase.RChar()
    Parse.TypeSpec.TWChar() => IL.RBase.RChar()
    Parse.TypeSpec.TInt(w) => IL.RBase.RInt(w)
    Parse.TypeSpec.TUInt(w) => IL.RBase.RUInt(w)
    Parse.TypeSpec.TFloat(w) => IL.RBase.RFloat(w)
    Parse.TypeSpec.TString(b) => IL.RBase.RStr(b === nothing ? nothing : _expr_int(b))
    Parse.TypeSpec.TWString(b) => IL.RBase.RWStr(b === nothing ? nothing : _expr_int(b))
    Parse.TypeSpec.TRef(name) => _ref_to_base(name)
    _ => error("unsupported ROS type spec: $(ts)")
end

function _expr_to_value(expr, is_array::Bool)
    @match expr begin
        Parse.ConstExpr.Lit(lit) => (@match lit begin
            Parse.Literal.Bl(b) => IL.RValue.VBool(b)
            Parse.Literal.Intg(n) => IL.RValue.VInt(n)
            Parse.Literal.F32(v) => IL.RValue.VFloat(Float64(v))
            Parse.Literal.F64(v) => IL.RValue.VFloat(v)
            Parse.Literal.Ch(c) => IL.RValue.VString(string(c))
            Parse.Literal.St(s) => is_array ? IL.RValue.VRaw(s) : IL.RValue.VString(s)
        end)
        # ROS only emits literals; anything richer round-trips as opaque text.
        _ => IL.RValue.VRaw(Parse.unparse(expr))
    end
end

function _lift_field(member)
    pair, default_expr = _peel_annotated(member)
    ts = pair.first
    decl = pair.second[1]
    base, array, name = @match decl begin
        Parse.Declarator.DArray(nm, dims) =>
            (_ts_to_base(ts), IL.ArraySpec.AStatic(_expr_int(dims[1])), nm)
        Parse.Declarator.DIdent(nm) => (@match ts begin
            Parse.TypeSpec.TSeq(elt, bound) => (_ts_to_base(elt),
                bound === nothing ? IL.ArraySpec.AUnbounded() :
                    IL.ArraySpec.ABounded(_expr_int(bound)), nm)
            _ => (_ts_to_base(ts), IL.ArraySpec.AScalar(), nm)
        end)
    end
    default = default_expr === nothing ? nothing :
        _expr_to_value(default_expr, !_is_scalar(array))
    return IL.RField(IL.RType(base, array), name, default)
end

function _lift_constant(d)
    d, _ = _peel_annotated(d)
    @match d begin
        Parse.ConstDecl.CDecl(ts, name, val) =>
            IL.RConstant(_ts_to_base(ts), name, _expr_to_value(val, false))
    end
end

# Reconstruct one `RMessage` (the named struct plus its `<Name>_Constants`
# module, if any) out of a flat decl list.
function _lift_message(decls, name::Symbol)
    members = nothing
    for d in decls
        s = _as_struct(d)
        s !== nothing && s[1] == name && (members = s[2])
    end
    members === nothing && error("struct $name not found")
    consts_name = Symbol(string(name, "_Constants"))
    constants = IL.RConstant[]
    for d in decls
        m = _as_module(d)
        if m !== nothing && m[1] == consts_name
            append!(constants, (_lift_constant(c) for c in m[2]))
        end
    end
    fields = IL.RField[_lift_field(m) for m in members]
    return IL.RMessage(name, constants, fields)
end

function _first_with_suffix(names, suf)
    for n in names
        endswith(string(n), suf) && return n
    end
    error("no struct ending in `$suf` found")
end

_strip_suffix(s, suf) = endswith(s, suf) ? s[1:end-length(suf)] : s

function _lift_service(decls)
    names = _struct_names(decls)
    req = _first_with_suffix(names, "_Request")
    res = _first_with_suffix(names, "_Response")
    base = Symbol(_strip_suffix(string(req), "_Request"))
    return IL.RService(base, _lift_message(decls, req), _lift_message(decls, res))
end

function _lift_action(decls)
    names = _struct_names(decls)
    goal = _first_with_suffix(names, "_Goal")
    result = _first_with_suffix(names, "_Result")
    feedback = _first_with_suffix(names, "_Feedback")
    base = Symbol(_strip_suffix(string(goal), "_Goal"))
    return IL.RAction(base, _lift_message(decls, goal),
        _lift_message(decls, result), _lift_message(decls, feedback))
end

"""
    lift(decls) -> IL interface

Reconstruct the ROS interface IL (`RMessage`/`RService`/`RAction`) from a
`Parse.Decl` vector — the inverse of [`lower`](@ref). The kind is taken from a
`module msg|srv|action` wrapper if present, else inferred from struct-name
suffixes (`_Request`/`_Response`, `_Goal`/`_Result`/`_Feedback`).
"""
function lift(decls)
    _, kind, inner = _unwrap_package(decls)
    if kind === :srv
        return _lift_service(inner)
    elseif kind === :action
        return _lift_action(inner)
    else
        names = _struct_names(inner)
        isempty(names) && error("no message struct found in decls")
        return _lift_message(inner, names[1])
    end
end

# ---- Public backward API ---------------------------------------------------

"""
    to_ros(decls) -> String
    to_ros(io, decls)

Render a `Parse.Decl` vector back to ROS2 interface text (`.msg`/`.srv`/
`.action`), going `Parse.Decl → IL → text`. The inverse of [`parse_msg`](@ref)
and friends, up to spellings that ROS and IDL share (`char`↔`uint8`,
`byte`↔`octet`, and the `time`/`duration`/`Header` aliases).
"""
to_ros(decls) = IL.unparse(lift(decls))
to_ros(io::IO, decls) = IL.unparse(io, lift(decls))
