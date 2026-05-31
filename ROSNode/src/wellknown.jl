# §11/§13 — the statically-compiled bootstrap interfaces (PLAN-D5 S1).
#
# Dynamic discovery fetches an unknown type's definition by *calling* a remote's
# `~/get_type_description` (`type_description_interfaces/srv/GetTypeDescription`).
# That handshake needs the GetTypeDescription request/response types and the
# `type_description_interfaces/msg/*` vocabulary they carry — themselves ROS types.
# So we vendor those `.msg`/`.srv` and generate them *statically* (compiled into
# ROSNode's image), the bootstrap §11/§13 assume: a known type used to fetch
# unknown ones.
#
# Two halves live here:
#   • `WellKnown` — the generated wire structs (CDR-codable), via `@ros_msgs` over
#     `../vendor/type_description_interfaces`.
#   • the wire ⇄ internal bridge: ROSMessages' *internal* hashing `TypeDescription`
#     (rihs01.jl) and the generated *wire* `type_description_interfaces/msg/
#     TypeDescription` are distinct types with the same shape; `to_wire_td` /
#     `from_wire_td` convert between them losslessly (verified: RIHS01 is preserved
#     across the round-trip). The registry/serving side speaks the internal form;
#     the client/server marshal speaks the wire form.
#
# The well-known types are also registered into every Context's registry on
# creation (`_register_wellknown_types!`) so the §13 server can answer for them and
# `type_info_of` reports their real RIHS01.

using ROSMessages: message_il, service_il

# ── the generated wire types (static) ─────────────────────────────────────────
# Generated at ROSNode precompile time. `@ros_msgs` over the vendored directory
# emits the nested `type_description_interfaces.{msg,srv}.<Name>` modules and
# `Base.include_dependency`s each source, so editing a vendored `.msg` invalidates
# ROSNode's precompile. The generated `import StaticArrays, CDRSerialization`
# resolve against ROSNode's deps (both are direct).
module WellKnown
    using ROSMessages: @ros_msgs
    @ros_msgs "../vendor/type_description_interfaces"
end

const _TDI = WellKnown.type_description_interfaces

# The wire structs (distinct from ROSMessages' internal hashing structs of the
# same conceptual shape — see the module banner).
const WireTypeDescription            = _TDI.msg.TypeDescription
const WireIndividualTypeDescription  = _TDI.msg.IndividualTypeDescription
const WireField                      = _TDI.msg.Field
const WireFieldType                  = _TDI.msg.FieldType
const WireTypeSource                 = _TDI.msg.TypeSource
const WireKeyValue                   = _TDI.msg.KeyValue
const GetTypeDescription_Request     = _TDI.srv.GetTypeDescription_Request
const GetTypeDescription_Response    = _TDI.srv.GetTypeDescription_Response

# Vendored source dir (shipped with the package); read at runtime to recover the
# well-known types' IL + RIHS01 for registry registration.
const _VENDOR_DIR = normpath(joinpath(@__DIR__, "..", "vendor"))

# Generated-type lookup by fully-qualified ROS2 name, for registration.
const _WELLKNOWN_TYPES = Dict{String, Any}(
    "type_description_interfaces/msg/FieldType"                 => WireFieldType,
    "type_description_interfaces/msg/Field"                     => WireField,
    "type_description_interfaces/msg/IndividualTypeDescription" => WireIndividualTypeDescription,
    "type_description_interfaces/msg/TypeDescription"           => WireTypeDescription,
    "type_description_interfaces/msg/KeyValue"                  => WireKeyValue,
    "type_description_interfaces/msg/TypeSource"                => WireTypeSource,
    "type_description_interfaces/srv/GetTypeDescription_Request"  => GetTypeDescription_Request,
    "type_description_interfaces/srv/GetTypeDescription_Response" => GetTypeDescription_Response,
)

# ── wire ⇄ internal TypeDescription bridge ────────────────────────────────────
# ROSMessages' internal `TypeDescription`/`FieldDescription`/`FieldTypeDescription`
# (rihs01.jl, the form `calculate_rihs01_hash`/`lift` consume) ⇄ the generated wire
# `type_description_interfaces/msg/*`. Field-for-field; `default_value` is dropped
# (RIHS01 excludes it, and `lift` never carries it). Verified hash-preserving.

to_wire_field_type(ft::FieldTypeDescription) =
    WireFieldType(; type_id = ft.type_id, capacity = ft.capacity,
                  string_capacity = ft.string_capacity,
                  nested_type_name = ft.nested_type_name)

to_wire_field(fd::FieldDescription) =
    WireField(; name = fd.name, type = to_wire_field_type(fd.field_type),
              default_value = "")

to_wire_individual(td::TypeDescription) =
    WireIndividualTypeDescription(; type_name = td.type_name,
        fields = WireField[to_wire_field(f) for f in td.fields])

"""
    to_wire_td(m::TypeDescriptionMsg) -> WireTypeDescription

Convert ROSMessages' internal hashing `TypeDescriptionMsg` (main + referenced
closure) into the generated wire `type_description_interfaces/msg/TypeDescription`
for serving over `~/get_type_description` (§13). Field-for-field; lossless w.r.t.
RIHS01 (constants/defaults are RIHS-excluded anyway).
"""
to_wire_td(m::TypeDescriptionMsg) =
    WireTypeDescription(; type_description = to_wire_individual(m.type_description),
        referenced_type_descriptions =
            WireIndividualTypeDescription[to_wire_individual(t)
                                          for t in m.referenced_type_descriptions])

from_wire_field_type(ft) =
    FieldTypeDescription(UInt8(ft.type_id), UInt64(ft.capacity),
                         UInt64(ft.string_capacity), String(ft.nested_type_name))

from_wire_field(f) = FieldDescription(String(f.name), from_wire_field_type(f.type))

from_wire_individual(itd) =
    TypeDescription(String(itd.type_name),
                    FieldDescription[from_wire_field(f) for f in itd.fields])

"""
    from_wire_td(w) -> TypeDescriptionMsg

Convert a decoded wire `type_description_interfaces/msg/TypeDescription` (e.g. from
a `GetTypeDescription` reply) back into ROSMessages' internal `TypeDescriptionMsg`,
the form the integrity gate (`calculate_rihs01_hash`), `lift`, and the registry
consume. Inverse of [`to_wire_td`](@ref).
"""
from_wire_td(w) =
    TypeDescriptionMsg(from_wire_individual(w.type_description),
        TypeDescription[from_wire_individual(t)
                        for t in w.referenced_type_descriptions])

# ── well-known registry entries (bootstrap, §11) ──────────────────────────────
# Build the registry entries for the vendored types: parse each source → IL, recover
# its RIHS01 (with the referenced closure, so referencing types hash exactly), and
# point the entry's `mod`/`type` at the *already-compiled* WellKnown struct so
# `realize!` is a no-op (these are static, never re-codegen'd).
#
# Source preference: when running inside a sourced ROS2 env that ships
# `type_description_interfaces`, read each interface from there (so the bootstrap
# types' own RIHS01 matches the environment exactly); else the vendored copy. The
# generated *Julia* type stays the vendored WellKnown one either way — the standard
# definitions are structurally stable, so an env hash keys the same wire layout.

# Read a well-known interface source, preferring an ament copy when present.
function _wellknown_source(name::AbstractString, fallback_path::AbstractString)
    ament = _find_ament_file(name)
    ament === nothing || return read(ament, String)
    return read(fallback_path, String)
end

# Transitively collect the referenced `TypeDescription`s for `main` from `pool`
# (keyed by qualified name), sorted by `type_name` — the canonical closure §13
# requires for hash parity and for a peer to codegen a referencing type.
function _collect_td_closure(main::TypeDescription,
                             pool::AbstractDict{String, TypeDescription})
    seen = Set{String}()
    out = TypeDescription[]
    function visit(td::TypeDescription)
        for f in td.fields
            nt = f.field_type.nested_type_name
            (isempty(nt) || nt in seen) && continue
            ref = get(pool, nt, nothing)
            ref === nothing && continue          # external ref not in pool — skip
            push!(seen, nt); push!(out, ref); visit(ref)
        end
    end
    visit(main)
    sort!(out; by = t -> t.type_name)
    return out
end

# Parse the vendored (or ament) sources into internal main `TypeDescription`s +
# their IL, build per-type entries with closures, and bind each to its compiled
# WellKnown type. Memoized — computed once, copied into each Context's registry.
function _build_wellknown_entries()
    pkg = "type_description_interfaces"
    pool = Dict{String, TypeDescription}()      # qualified name → main internal TD
    ilmap = Dict{String, Any}()                 # qualified name → IL.RMessage

    msg_dir = joinpath(_VENDOR_DIR, pkg, "msg")
    for bare in ("FieldType", "Field", "IndividualTypeDescription",
                 "TypeDescription", "KeyValue", "TypeSource")
        qn = "$pkg/msg/$bare"
        src = _wellknown_source(qn, joinpath(msg_dir, bare * ".msg"))
        il = message_il(src; name = bare)
        ast = _scan_for_struct(lower(il; package = pkg))
        ast === nothing && error("wellknown: no struct AST for $qn")
        pool[qn] = type_description_from_struct(ast, bare; package = pkg, qualifier = "msg")
        ilmap[qn] = il
    end

    srv_qn = "$pkg/srv/GetTypeDescription"
    srv_src = _wellknown_source(srv_qn, joinpath(_VENDOR_DIR, pkg, "srv", "GetTypeDescription.srv"))
    sil = service_il(srv_src; name = "GetTypeDescription")
    for (sec, secil) in (("GetTypeDescription_Request", sil.request),
                         ("GetTypeDescription_Response", sil.response))
        qn = "$pkg/srv/$sec"
        ast = _scan_for_struct(lower(secil; package = pkg))
        ast === nothing && error("wellknown: no struct AST for $qn")
        # The section's own name is the srv-qualified `qn` (passed fully-qualified so
        # it's used as-is); `qualifier="msg"` then governs only how its *relative*
        # refs resolve — a bare `TypeDescription` in a .srv is the same-package *msg*
        # `…/msg/TypeDescription`, not `…/srv/…`, so the closure finds it in the pool.
        pool[qn] = type_description_from_struct(ast, qn; package = pkg, qualifier = "msg")
        ilmap[qn] = secil
    end

    entries = RegistryEntry[]
    for (qn, main) in pool
        closure = _collect_td_closure(main, pool)
        tdmsg = TypeDescriptionMsg(main, closure)
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(tdmsg))
        hash === nothing && error("wellknown: unparseable RIHS01 for $qn")
        T = _WELLKNOWN_TYPES[qn]
        e = RegistryEntry(TypeInfo(qn, hash), ilmap[qn]; td = tdmsg, provenance = :static)
        e.mod = parentmodule(T)
        e.type = T
        _record_type_entry!(e)                  # type_info_of(T) → the real hash
        push!(entries, e)
    end
    return entries
end

const _WELLKNOWN_ENTRIES = Ref{Union{Nothing, Vector{RegistryEntry}}}(nothing)
const _WELLKNOWN_LOCK = ReentrantLock()

# The memoized well-known entries (computed once per process). Shared, read-only.
function _wellknown_entries()
    @lock _WELLKNOWN_LOCK begin
        _WELLKNOWN_ENTRIES[] === nothing && (_WELLKNOWN_ENTRIES[] = _build_wellknown_entries())
        return _WELLKNOWN_ENTRIES[]::Vector{RegistryEntry}
    end
end

"""
    _register_wellknown_types!(ctx) -> ctx

Register the bootstrap `type_description_interfaces` types into `ctx`'s registry
(§11 S1) so the §13 `~/get_type_description` server can answer for them and the
client can resolve them. Best-effort: a failure is logged, never fatal (the
generated types are already compiled and usable directly).
"""
function _register_wellknown_types!(ctx)
    try
        reg = registry(ctx)
        for e in _wellknown_entries()
            register_type!(reg, e.info, e)
        end
    catch err
        @error "typesupport: registering well-known bootstrap types failed" exception=(err, catch_backtrace())
    end
    return ctx
end
