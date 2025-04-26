using PEG

include_rgx = r"#include(?|<([^>\"]+)>|\"([^>\"]+)\")"
function resolve_file(file, preproc)
    contents = read(match(include_rgx, file)[1], String)
    return preproc(contents)
end
function preprocessor(idl, include_resolver=resolve_file, directive_resolver=x->throw("directives not supported"))
    no_comments = replace(idl,
        r"(//[^\n\r]*)(\r{0,1}\n)" => s"\2",
        r"/\*.*\*/"s => "")
    resolve_includes = replace(no_comments, include_rgx => inc -> include_resolver(inc, idl -> preprocessor(idl, include_resolver, directive_resolver)))
    resolve_directives = replace(resolve_includes, r"#[^\n\r]*" => inc -> directive_resolver(inc))
    return resolve_directives
end

function open_idl(file)
    inp = read(file, String)
    preproc = preprocessor(inp)
    return parse_whole(specification, strip(preproc))
end

# 7.2 lexical conventions
sjoin = x->join(x, "")
lit(c) = x->c
nth(n) = (args) -> args[n]
drop(x) = []

@rule alpha = r"[a-zA-Z]"
@rule digit = r"[0-9]"
@rule octal_digit = r"[0-7]"
@rule hex_digit = r"[0-9a-fA-F]"
@rule octal_escape = ("\\" & r"[0-7]{1,3}") |> x->string(Char(parse(Int, x[2]; base=8)))
@rule hex_escape = ("\\x" & r"[0-9a-fA-F]{1,}") |> x->string(Char(parse(Int, x[2]; base=16)))
@rule unicode_escape = ("\\u" &  r"[0-9a-fA-F]{1,4}") |> x-> string(Char(parse(Int, x[2]; base=16)))

@rule escape_chars = (
    ("n" |> lit("\n")), 
    ("t" |> lit("\t")), 
    ("v" |> lit("\v")), 
    ("b" |> lit("\b")), 
    ("r" |> lit("\r")), 
    ("f" |> lit("\f")), 
    ("a" |> lit("\a")), 
    ("?" |> lit("?")),
    "'",
    "\"")
@rule escape = ((("\\" & escape_chars) |> nth(2), octal_escape, hex_escape, unicode_escape))

@rule identifier = r"[a-zA-Z_][_a-zA-Z0-9]*"p
@rule character_literal = "'" & (-"'" & (escape, r".") |> nth(2)) & r"'"p |> x -> x[2][1]
@rule string_literal = "\"" & ((-"\"" & (escape, r".") |> nth(2))[*] |> sjoin) & r"\""p |> x -> x[2]
@rule wide_character_literal = "L'" & (-"'" & (escape, r".") |> nth(2)) & r"'"p |> x -> x[2][1]
@rule wide_string_literal = "L\"" & ((-"\"" & (escape, r".") |> nth(2))[*] |> sjoin) & r"\""p |> x -> x[2]

@rule integer_literal = (hex_integer_literal, octal_integer_literal, decimal_integer_literal)
@rule decimal_integer_literal = r"[0-9]+"p |> x -> parse(Int, x)
@rule octal_integer_literal = r"0[0-7]+"p |> x -> parse(Int, x; base=8)
@rule hex_integer_literal = r"(?:0x)([0-9a-fA-F]+)"ip |> x -> parse(Int, x[3:end]; base=16)

@rule fixed_pt_literal = floating_pt_literal
parse_idl_float(f) = 
    if endswith(f, r"f"i) parse(Float32, f[1:end-1])
    elseif endswith(f, r"d"i) parse(Float64, f[1:end-1])
    else parse(Float32, f)
    end
@rule floating_pt_literal = 
    (r"[0-9]+\.[0-9]+(e[+-]?[0-9]+)?[fd]?"ip,
     r"\.[0-9]+(e[+-]?[0-9]+)?[fd]?"ip,
     r"[0-9]+(e[+-]?[0-9]+)[fd]?"ip,
     r"[0-9]+[fd]"ip) |> parse_idl_float
# From Annex: Consolidated IDL Grammar

@rule specification = annotated_definition[*]
@rule annotated_definition = maybe_annotated & definition > (f, d) -> f(d)
@rule definition = (
    module_dcl & r";"p,
    const_dcl & r";"p,
    type_dcl & r";"p,
    #except_dcl & r";"p,
    #interface_dcl & r";"p,
    #value_dcl & r";"p,
    #type_id_dcl & r";"p,
    #type_prefix_dcl & r";"p,
    #import_dcl & r";"p,
    #component_dcl & r";"p,
    #home_dcl & r";"p,
    #event_dcl & r";"p,
    #porttype_dcl & r";"p,
    #connector_dcl & r";"p,
    #template_module_dcl & r";"p,
    #template_module_inst & r";"p,
    #annotation_dcl & r";"p
) |> nth(1)
@rule module_dcl = r"module"w & identifier & r"{"p & annotated_definition[*] & r"}"p > (_, i, _, defs, _) -> ModuleDecl.MDecl(Symbol(i), defs)
@rule const_dcl = r"const"w & const_type & identifier & r"="p & const_expr > (_, ct, id, _, ce) -> ConstDecl.CDecl(ct, Symbol(id), ce)
@rule const_type = (
    floating_pt_type,
    fixed_pt_const_type,
    integer_type,
    char_type,
    wide_char_type,
    boolean_type,
    octet_type,
    string_type,
    wide_string_type,
    scoped_name
)

mklit(f::Float32) = Literal.F32(f)
mklit(f::Float64) = Literal.F64(f)
mklit(f::Int64) = Literal.Intg(f)
mklit(f::Char) = Literal.Ch(f)
mklit(f::String) = Literal.St(f)
mklit(f::Bool) = Literal.Bl(f)

@rule scoped_name = (r"::"p[:?] |> isempty) & identifier & (r"::"p & identifier |> nth(2))[*] > (is_global, prefix, path) -> let fullpath = [prefix ; path]; ScopedName.Name(Symbol.(fullpath[1:end-1]), Symbol(fullpath[end]), is_global) end

@rule maybe_annotated = annotated[:?] |> f -> isempty(f) ? x->x : f[1]
@rule annotated = (annotation_appl & annotated > (fun, next) -> (subject) -> fun(next(subject))), (annotation_appl)
@rule annotation_appl = ( r"@" & scoped_name & (r"\("p & annotation_appl_params & r"\)"p |> nth(2))[:?] ) > 
    (_, annotation_name, annotation_params) -> (subject -> Annotated.Annotation(annotation_name, isempty(annotation_params) ? Pair{Symbol, ConstExpr.Type}[] : annotation_params[1], subject))
@rule annotation_appl_params = ((annotation_appl_param & (r","p & annotation_appl_param |> nth(2))[*]) > (a,b) -> Pair{Symbol, ConstExpr.Type}[a;b], const_expr)
@rule annotation_appl_param = ( identifier & r"="p & const_expr ) > (n,_,e) -> Symbol(n)=>e

@rule const_expr = unary_expr & (or_expr, xor_expr, and_expr, lshift_expr, rshift_expr, add_expr, sub_expr, mul_expr, div_expr, mod_expr)[:?] > (uex, binop) -> isempty(binop) ? uex : binop[1](uex)
@rule or_expr = r"\|"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Or(), l, r))
@rule xor_expr = r"\^"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Xor(), l, r))
@rule and_expr = r"\&"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.And(), l, r))
@rule lshift_expr = r"<<"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Lshift(), l, r))
@rule rshift_expr = r">>"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Rshift(), l, r))
@rule add_expr = r"\+"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Add(), l, r))
@rule sub_expr = r"\-"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Sub(), l, r))
@rule mul_expr = r"\*"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Mul(), l, r))
@rule div_expr = r"/"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Div(), l, r))
@rule mod_expr = r"%"p & const_expr > (_, r) -> ((l) -> ConstExpr.BinApp(Binop.Mod(), l, r))
@rule unary_expr = ((unary_operator & primary_expr > (op, pex) -> ConstExpr.UnApp(op, pex)), primary_expr)
@rule unary_operator = (r"\-"p > lit(Unop.Neg())), (r"\+"p > lit(Unop.Plus())), (r"~"p > lit(Unop.Inv()))
@rule primary_expr = (literal |> ConstExpr.Lit, scoped_name |> ConstExpr.Var, r"\("p & const_expr & r"\)"p |> nth(2))

@rule literal = ((floating_pt_literal, fixed_pt_literal, integer_literal, character_literal, wide_character_literal, boolean_literal) |> mklit, (string_literal, wide_string_literal) |> s -> Literal.St(s))
@rule boolean_literal = (r"TRUE"p |> lit(true), r"FALSE"p |> lit(false))

@rule positive_int_const = const_expr
@rule type_dcl = (constr_type_dcl, native_dcl, typedef_dcl)
@rule type_spec = (template_type_spec, simple_type_spec)

@rule simple_type_spec = (base_type_spec, scoped_name |> TypeSpec.TRef)
@rule base_type_spec = (floating_pt_type, integer_type, char_type, wide_char_type, boolean_type, octet_type, any_type, object_type, value_base_type)
@rule floating_pt_type = (r"float"w |> lit(TypeSpec.TFloat(32)), r"double"w |> lit(TypeSpec.TFloat(64)), r"long"w & r"double"w |> lit(TypeSpec.TFloat(128)))
@rule integer_type = (signed_int, unsigned_int)
@rule signed_int = (
    (r"int8"w |> lit(TypeSpec.TInt(8))), 
    ((r"short"w, r"int16"w) |> lit(TypeSpec.TInt(16))), 
    ((r"long"w & r"long"w, r"int64"w) |> lit(TypeSpec.TInt(64))), 
    ((r"long"w, r"int32"w) |> lit(TypeSpec.TInt(32))))
@rule unsigned_int = (
    (r"uint8"w |> lit(TypeSpec.TUInt(8))), 
    ((r"unsigned"w & r"short"w, r"uint16"w) |> lit(TypeSpec.TUInt(16))), 
    ((r"unsigned"w & r"long"w & r"long"w, r"uint64"w) |> lit(TypeSpec.TUInt(64))), 
    ((r"unsigned"w & r"long"w, r"uint32"w) |> lit(TypeSpec.TUInt(32))))
@rule char_type = r"char"w |> lit(TypeSpec.TChar())
@rule wide_char_type = r"wchar"w |> lit(TypeSpec.TWChar())
@rule boolean_type = r"boolean"w |> lit(TypeSpec.TBool())
@rule octet_type = r"octet"w |> lit(TypeSpec.TOctet())
@rule any_type = r"any"w |> lit(TypeSpec.TAny())
@rule object_type = r"Object"w |> lit(TypeSpec.TObject())
@rule value_base_type = r"ValueBase"w |> lit(TypeSpec.TValueBase())

@rule template_type_spec = (sequence_type, string_type, wide_string_type, fixed_pt_type, map_type)
@rule sequence_type = r"sequence"p & r"<"p & type_spec & ((r","p & positive_int_const)[:?] |> x -> isempty(x) ? nothing : x[1][2]) & r">"p > (_, _, eltype, len, _) -> TypeSpec.TSeq(eltype, len)
@rule string_type = r"string"p & ((r"<"p & positive_int_const & r">"p)[:?] |> x -> isempty(x) ? nothing : x[1][2]) > (_, len) -> TypeSpec.TString(len)
@rule wide_string_type = r"wstring"p & ((r"<"p & positive_int_const & r">"p)[:?] |> x -> isempty(x) ? nothing : x[1][2]) > (_, len) -> TypeSpec.TWString(len)
@rule fixed_pt_type = r"fixed"p & r"<"p & positive_int_const & r","p & positive_int_const & r">"p > (_, _, digits, _, fdigits, _) -> TypeSpec.TSpecifiedFixedPoint(digits, fdigits)
@rule fixed_pt_const_type = r"fixed"p |> lit(TypeSpec.TFixedPoint())
@rule map_type = (r"map"p & r"<"p & type_spec & r","p & type_spec & ((r","p & positive_int_const)[:?] |> x -> isempty(x) ? nothing : x[1][2]) & r">"p) > (_, _, kty, _, ety, len, _) -> TypeSpec.TMap(kty, ety, len)


@rule constr_type_dcl = (struct_dcl, union_dcl, enum_dcl, bitset_dcl, bitmask_dcl)
@rule struct_dcl = (struct_def, struct_forward_dcl)
@rule struct_def = r"struct"p & identifier & ((r":"p & scoped_name)[:?] |> x -> isempty(x) ? nothing : x[1][2]) & r"{"p & annotated_member[*] & r"}"p > (_, name, super, _, members, _) -> TypeDecl.StructDecl(Symbol(name), super, members)
@rule annotated_member = maybe_annotated & member > (f, d) -> f(d)
@rule member = type_spec & declarators & r";"p > (ts, decls, _) -> (ts => decls)
@rule struct_forward_dcl = r"struct"w & identifier > (_, n) -> TypeDecl.StructFwdDecl(Symbol(n))
@rule declarators = ( declarator & (r","p & declarator |> nth(2))[:*] ) > (a, b) -> Declarator.Type[a ; b]
@rule declarator = ( array_declarator, simple_declarator )
@rule fixed_array_size = ( r"\["p & positive_int_const & r"\]"p ) > (_, c, _) -> c
@rule array_declarator = ( identifier & fixed_array_size[:+] ) |> x -> Declarator.DArray(Symbol(x[1]), x[2])
@rule simple_declarator = ( identifier ) |> x -> Declarator.DIdent(Symbol(x))



@rule union_dcl = (union_def, union_forward_dcl)
@rule union_def = r"union"w & identifier & r"switch"p & r"\("p & switch_type_spec & r"\)"p & r"{"p & switch_body & r"}"p > (_, name, _, _, switch_type, _, _, switch_body, _) -> TypeDecl.UnionDecl(Symbol(name), switch_type, switch_body)
@rule switch_type_spec = (integer_type, char_type, boolean_type, wide_char_type, octet_type, scoped_name)
@rule switch_body = case[+]
@rule case = case_label[+] & element_spec & r";"p > (labels, espec, _) -> labels => espec
@rule case_label = (r"case"w & const_expr & r":"p > (_, c, _) -> UnionCaseLabel.UCLCase(c), r"default"p & r":"p |> lit(UnionCaseLabel.UCLDefault()))
@rule element_spec = type_spec & declarator > UnionElement.UElem
@rule union_forward_dcl = r"union"w & identifier > (_, n) -> TypeDecl.UnionFwdDecl(Symbol(n))

@rule enum_dcl = r"enum"w & identifier & r"{"p & (enumerator & (r","p & enumerator |> nth(2))[*] > (p, f) -> [p; f]) & r","p[:?] & r"}"p > (_, name, _, cases, _...) -> TypeDecl.EnumDecl(Symbol(name), Symbol.(cases))
@rule enumerator = identifier

@rule native_dcl = ( r"native"w & simple_declarator )

@rule typedef_dcl = ( r"typedef"p & type_declarator ) |> nth(2)
@rule type_declarator = ( (template_type_spec, constr_type_dcl, simple_type_spec) & any_declarators ) > (def, declarators) -> TypeDecl.TypedefDecl(def, declarators)
@rule any_declarators = ( any_declarator & (r","w & any_declarator |> nth(2))[:*] ) > (a,b) -> [a ; b]
@rule any_declarator = ( array_declarator, simple_declarator )

@rule bitset_dcl = ( r"bitset"p & identifier & ((r":"p & scoped_name |> nth(2))[:?] |> (x -> isempty(x) ? nothing : x[1])) & r"{"p & bitfield[*] & r"}"p ) > (_, id, super, _, fields, _) -> TypeDecl.BitsetDecl(Symbol(id), super, fields) 
@rule bitfield = ( bitfield_spec & identifier[*] & r";"p ) > ((len, dt), idents, _) -> BitfieldSpec.BSSpec(len, dt, Symbol.(idents))
@rule bitfield_spec = ( r"bitfield"p & r"<"p & positive_int_const & (r","p & destination_type |> nth(2))[:?] & r">"p ) > (_, _, len, dt, _) -> (len, isempty(dt) ? nothing : dt[1])
@rule destination_type = ( boolean_type, octet_type, integer_type )

@rule bitmask_dcl = ( r"bitmask"p & identifier & r"{"p & bit_value & (r","p & bit_value |> nth(2))[*] & r","p[:?] & r"}"p ) > (_, name, _, bv, bvs, _, _) -> TypeDecl.BitmaskDecl(Symbol(name), Symbol.([bv; bvs]))
@rule bit_value = ( identifier )


#=
///////////////////////////////////////////////////////////////////////////////
// From Building Block Interfaces – Basic:
///////////////////////////////////////////////////////////////////////////////
// (71) merged to (2)
// (72)
except_dcl = { "exception" ~ identifier ~ "{" ~ member* ~ "}" }
// (73)
interface_dcl = {
    interface_def
    | interface_forward_dcl
}
// (74)
interface_def = { interface_header ~ "{" ~ interface_body ~ "}" }
// (75)
interface_forward_dcl = { interface_kind ~ identifier }
// (76)
interface_header = { interface_kind ~ identifier ~ interface_inheritance_spec? }
// (77) (119) (129)
interface_kind = { "interface" | "local" ~ "interface" | "abstract" ~ "interface" }
// (78)
interface_inheritance_spec = { ":" ~ interface_name ~ ("," ~ interface_name)* }
// (79)
interface_name = { scoped_name }
// (80)
interface_body = { export* }
// (81) (97) (112)
export = {
    op_dcl ~ ";"
    | attr_dcl ~ ";"
    | type_dcl ~ ";"
    | const_dcl ~ ";"
    | except_dcl ~ ";"
    | type_id_dcl ~ ";"
    | type_prefix_dcl ~ ";"
    | import_dcl ~ ";"
    | op_with_context ~ ";"
    | op_oneway_dcl ~ ";"
}
// (82)
op_dcl = { op_type_spec ~ identifier ~ "(" ~ parameter_dcls? ~ ")" ~ raises_expr? }
// (83)
op_type_spec = {
    type_spec
    | "void"
}
// (84)
parameter_dcls = { param_dcl ~ ("," ~ param_dcl)* }
// (85)
param_dcl = { param_attribute ~ type_spec ~ simple_declarator }
// (86)
param_attribute = {
    "inout"
    | "in"
    | "out"
}
// (87)
raises_expr = { "raises" ~ "(" ~ scoped_name ~ ("," ~ scoped_name)* ~ ")" }
// (88)
attr_dcl = {
    readonly_attr_spec
    | attr_spec
}
// (89)
readonly_attr_spec = { "readonly" ~ "attribute" ~ type_spec ~ readonly_attr_declarator }
// (90)
readonly_attr_declarator = {
    simple_declarator ~ raises_expr
    | simple_declarator ~ ("," ~ simple_declarator)*
}
// (91)
attr_spec = { "attribute" ~ type_spec ~ attr_declarator }
// (92)
attr_declarator = {
    simple_declarator ~ attr_raises_expr
    | simple_declarator ~ ("," ~ simple_declarator)*
}
// (93)
attr_raises_expr = {
    get_excep_expr ~ set_excep_expr?
    | set_excep_expr
}
// (94)
get_excep_expr = { "getraises" ~ exception_list }
// (95)
set_excep_expr = { "setraises" ~ exception_list }
// (96)
exception_list = { "(" ~ scoped_name ~ ("," ~ scoped_name)* ~ ")" }

///////////////////////////////////////////////////////////////////////////////
// From Building Block Value Types:
///////////////////////////////////////////////////////////////////////////////
// (98) merged to (2)
// (99) (125)
value_dcl = {
    value_def
    | value_forward_dcl
    | value_box_def
    | value_abs_def
}
// (100)
value_def = { value_header ~ "{" ~ value_element* ~ "}" }
// (101)
value_header = { value_kind ~ identifier ~ value_inheritance_spec? }
// (102) (128)
value_kind = { "valuetype" | "custom" ~ "valuetype" }
// (103) (130)
value_inheritance_spec = {
    (":" ~ value_name)? ~ ("supports" ~ interface_name)?
    | ":" ~ "truncatable"? ~ value_name ~ ("," ~ value_name)* ~ ("supports" ~ interface_name ~ ("," ~ interface_name)* )?

}
// (104)
value_name = { scoped_name }
// (105)
value_element = {
    export
    | state_member
    | init_dcl
}
// (106)
state_member = { ( "public" | "private" ) ~ type_spec ~ declarators ~ ";" }
// (107)
init_dcl = { "factory" ~ identifier ~ "(" ~ init_param_dcls? ~ ")" ~ raises_expr? ~ ";" }
// (108)
init_param_dcls = { init_param_dcl ~ ("," ~ init_param_dcl)* }
// (109)
init_param_dcl = { "in" ~ type_spec ~ simple_declarator }
// (110)
value_forward_dcl = { value_kind ~ identifier }



///////////////////////////////////////////////////////////////////////////////
// From Building Block CORBA-Specific – Interfaces:
///////////////////////////////////////////////////////////////////////////////
// (111) merged to (2)
// (112) merged to (81)
// (113)
type_id_dcl = { "typeid" ~ scoped_name ~ string_literal }
// (114)
type_prefix_dcl = { "typeprefix" ~ scoped_name ~ string_literal }
// (115)
import_dcl = { "import" ~ imported_scope }
// (116)
imported_scope = { scoped_name | string_literal }
// (119) merged to (77)
// (120)
op_oneway_dcl = { "oneway" ~ "void" ~ identifier ~ "(" ~ in_parameter_dcls? ~ ")" }
// (121)
in_parameter_dcls = { in_param_dcl ~ ("," ~ in_param_dcl)* }
// (122)
in_param_dcl = { "in" ~ type_spec ~ simple_declarator }
// (123)
op_with_context = { (op_dcl | op_oneway_dcl) ~ context_expr }
// (124)
context_expr = { "context" ~ "(" ~ string_literal ~ ("," ~ string_literal)* ~ ")" }



///////////////////////////////////////////////////////////////////////////////
// From Building Block CORBA-Specific – Value Types:
///////////////////////////////////////////////////////////////////////////////
// (125) merged to (99)
// (126)
value_box_def = { "valuetype" ~ identifier ~ type_spec }
// (127)
value_abs_def = { "abstract" ~ "valuetype" ~ identifier ~ value_inheritance_spec? ~ "{" ~ export* ~ "}" }



///////////////////////////////////////////////////////////////////////////////
// From Building Block Components – Basic:
///////////////////////////////////////////////////////////////////////////////
// (133) merged to (2)
// (134)
component_dcl = { component_def | component_forward_dcl }
// (135)
component_forward_dcl = { "component" ~ identifier }
// (136)
component_def = { component_header ~ "{" ~ component_body ~ "}" }
// (137) (154)
component_header = { "component" ~ identifier ~ component_inheritance_spec? ~ supported_interface_spec? }
// (138)
component_inheritance_spec = { ":" ~ scoped_name }
// (139)
component_body = { component_export* }
// (140) (156) (179)
component_export = {
    provides_dcl ~ ";"
    | uses_dcl ~ ";"
    | attr_dcl ~ ";"
    | emits_dcl ~ ";"
    | publishes_dcl ~ ";"
    | consumes_dcl ~ ";"
    | port_dcl ~ ";"
}
// (141)
provides_dcl = { "provides" ~ interface_type ~ identifier }
// (142) (157)
interface_type = { scoped_name | "Object" }
// (143) (158)
uses_dcl = { "uses" ~ "multiple"? ~ interface_type ~ identifier }



///////////////////////////////////////////////////////////////////////////////
// From Building Block Components – Homes:
///////////////////////////////////////////////////////////////////////////////
// (144) merged to (2)
// (145)
home_dcl = { home_header ~ "{" ~ home_body ~ "}" }
// (146) (162)
home_header = { "home" ~ identifier ~ home_inheritance_spec? ~ supported_interface_spec? ~ "manages" ~ scoped_name ~ primary_key_spec? }
// (147)
home_inheritance_spec = { ":" ~ scoped_name }
// (148)
home_body = { home_export* }
// (149) (164)
home_export = {
    export
    | factory_dcl ~ ";"
    | finder_dcl ~ ";"
}
// (150)
factory_dcl = { "factory" ~ identifier ~ "(" ~ factory_param_dcls? ~ ")" ~ raises_expr? }
// (151)
factory_param_dcls = { factory_param_dcl ~ ("," ~ factory_param_dcl)* }
// (152)
factory_param_dcl = { "in" ~ type_spec ~ simple_declarator }



///////////////////////////////////////////////////////////////////////////////
// From Building Block CCM-Specific:
///////////////////////////////////////////////////////////////////////////////
// (153) merged to (2)
// (154) merged to (137)
// (155)
supported_interface_spec = { "supports" ~ scoped_name ~ ("," ~ scoped_name)* }
// (156) merged to (140)
// (157) merged to (142)
// (158) merged to (143)
// (159)
emits_dcl = { "emits" ~ scoped_name ~ identifier }
// (160)
publishes_dcl = { "publishes" ~ scoped_name ~ identifier }
// (161)
consumes_dcl = { "consumes" ~ scoped_name ~ identifier }
// (162) merged to (146)
// (163)
primary_key_spec = { "primarykey" ~ scoped_name }
// (164) merged to (149)
// (165)
finder_dcl = { "finder" ~ identifier ~ "(" ~ init_param_dcls* ~ ")" ~ raises_expr? }
// (166)
event_dcl = {
    event_def
    | event_abs_def
    | event_forward_dcl
}
// (167)
event_forward_dcl = { "abstract"? ~ "eventtype" ~ identifier }
// (168)
event_abs_def = { "abstract" ~ "eventtype" ~ identifier ~ value_inheritance_spec? ~ "{" ~ export* ~ "}" }
// (169)
event_def = { event_header ~ "{" ~ value_element* ~ "}" }
// (170)
event_header = { "custom"? ~ "eventtype" ~ identifier ~ value_inheritance_spec? }



///////////////////////////////////////////////////////////////////////////////
// From Building Block Components – Ports and Connectors:
///////////////////////////////////////////////////////////////////////////////
// (171) merged to (2)
// (172)
porttype_dcl = { porttype_def | porttype_forward_dcl }
// (173)
porttype_forward_dcl = { "porttype" ~ identifier }
// (174)
porttype_def = { "porttype" ~ identifier ~ "{ " ~ port_body ~ "}" }
// (175)
port_body = { port_ref ~ port_export* }
// (176)
port_ref = {
    provides_dcl ~ ";"
    | uses_dcl ~ ";"
    | port_dcl ~ ";"
}
// (177)
port_export = {
    port_ref
    | attr_dcl ~ ";"
}
// (178)
port_dcl = { ("port" | "mirrorport") ~ scoped_name ~ identifier }
// (179) merged to (140)
// (180)
connector_dcl = { connector_header ~ "{" ~ connector_export+ ~ "}" }
// (181)
connector_header = { "connector" ~ identifier ~ connector_inherit_spec? }
// (182)
connector_inherit_spec = { ":" ~ scoped_name }
// (183)
connector_export = {
    port_ref
    | attr_dcl ~ ";"
}



///////////////////////////////////////////////////////////////////////////////
// From Building Block Template Modules:
///////////////////////////////////////////////////////////////////////////////
// (184) merged to (2)
// (185)
template_module_dcl = { "module" ~ identifier ~ "<" ~ formal_parameters ~ ">" ~ "{" ~ tpl_definition+ ~ "}" }
// (186)
formal_parameters = { formal_parameter ~ ("," ~ formal_parameter)* }
// (187)
formal_parameter = { formal_parameter_type ~ identifier }
// (188)
formal_parameter_type = {
    "typename"
    | "interface"
    | "valuetype"
    | "eventtype"
    | "struct"
    | "union"
    | "exception"
    | "enum"
    | "const" ~ const_type
    | sequence_type
    | "sequence"
}
// (189)
tpl_definition = {
    definition
    | template_module_ref ~ ";"
}
// (190)
template_module_inst = { "module" ~ scoped_name ~ "<" ~ actual_parameters ~ ">" ~ identifier }
// (191)
actual_parameters = { actual_parameter ~ ("," ~ actual_parameter)* }
// (192)
actual_parameter = {
    type_spec
    | const_expr
}
// (193)
template_module_ref = { "alias" ~ scoped_name ~ "<" ~ formal_parameter_names ~ ">" ~ identifier }
// (194)
formal_parameter_names = { identifier ~ ("," ~ identifier)* }



///////////////////////////////////////////////////////////////////////////////
// From Building Block Anonymous Types:
///////////////////////////////////////////////////////////////////////////////
// (206) merged to (21)
// (207) merged to (68)



///////////////////////////////////////////////////////////////////////////////
// From Building Block Annotations:
///////////////////////////////////////////////////////////////////////////////
// (208) merged to (2)
// (209)
annotation_dcl = { annotation_header ~ "{" ~ annotation_body ~ "}" }
// (210)
annotation_header = { "@annotation" ~ identifier }
// (211)
annotation_body = {
    (
        annotation_member
        | enum_dcl ~ ";"
        | const_dcl ~ ";"
        | typedef_dcl ~ ";"
    )*
}
// (212)
annotation_member = { annotation_member_type ~ simple_declarator ~ ( "default" ~ const_expr )? ~ ";" }
// (213)
annotation_member_type = { const_type | any_const_type | scoped_name }
// (214)
any_const_type = { "any" }
=#