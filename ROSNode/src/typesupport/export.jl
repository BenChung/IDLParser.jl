# ── export_typesupport: write a discovered type into durable form ────────────
# Writes a registered type to disk in one of three formats: `:msg`
# (interface-package layout from `IL.unparse`), `:julia` (`generate_code` output
# as static `.jl` source), and `:typedesc` (the raw wire blob, language-agnostic).
# RIHS01 is the roundtrip invariant across all three.

"""
    export_typesupport(node_or_ctx, names; to=pwd(), format=:msg) -> Vector{String}

Write registered (typically discovered) types out of the in-memory registry
into durable, user-owned files, returning the paths written. `names` is a
single type name or any iterable of names; `format` selects the layout (the
ROS 2 interface concept:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html):

- `:msg` (default) / `:julia_text` — `IL.unparse` the type into ROS interface
  text at `<to>/<package>/<qualifier>/<Name>.<ext>` (`.msg` / `.srv` /
  `.action` per the IL kind). The most portable form: `colcon build` turns the
  tree into a real interface package. A discovered type's text carries exactly
  the field definitions RIHS01 covers, so it is wire-faithful; constants and
  defaults are absent (`lift` drops them), so it is lossy as documentation.
- `:julia` — emit `Generation.generate_code`'s output as `.jl` source at
  `<to>/<package>.jl` (or `<to>/<Name>.jl` for a package-less name) to check in
  and `include`. The strongest port-forward: the once-dynamic type becomes
  static, precompilable, and back on the min-copy fast path. The file carries
  inline `import StaticArrays, CDRSerialization` so it reparses standalone, and
  emits the entry's referenced types alongside it (the same closure `realize!`
  generates from), so nested cross-package fields resolve on `include`.
- `:typedesc` — the raw wire `TypeDescription` JSON bundle at
  `<to>/<Name>.typedesc.json`; language-agnostic and reloadable by ROSNode.

Throws `ArgumentError` when a requested name is not registered (discover or
[`load_ament_type`](@ref) it first), when an unknown `format` is given, or when
`:typedesc` is requested for an `:ament`-acquired entry — one parsed from local
interface text, which carries no wire `TypeDescription`. The `:wire`, `:cache`,
`:static`, and `:authored` entries all carry one and export as `:typedesc`
fine. Name resolution ignores the hash version — the first registry entry
matching the name is exported.
"""
function export_typesupport(ctxlike, names; to::AbstractString=pwd(),
                            format::Symbol=:msg)
    reg = registry(_ctx(ctxlike))
    namelist = names isa AbstractString ? (names,) : names
    written = String[]
    for name in namelist
        entry = _registered_entry_by_name(reg, String(name))
        entry === nothing &&
            throw(ArgumentError("export_typesupport: type $(name) is not registered \
                                 (discover or load it before exporting)"))
        append!(written, _export_one(entry, to, format))
    end
    return written
end

# Export is name-addressed: return any entry matching the name, ignoring hash
# version (`Dict` iteration order is unspecified). `nothing` if unregistered.
function _registered_entry_by_name(reg::TypeRegistry, name::AbstractString)
    @lock reg.lock begin
        for ((n, _), v) in reg.entries
            n == name && v isa RegistryEntry && return v
        end
    end
    return nothing
end

function _export_one(entry::RegistryEntry, to::AbstractString, format::Symbol)
    if format === :typedesc
        return [_export_typedesc(entry, to)]
    elseif format === :julia
        return [_export_julia(entry, to)]
    elseif format === :msg || format === :julia_text
        return [_export_interface_text(entry, to)]
    else
        throw(ArgumentError("export_typesupport: unknown format $(repr(format)) \
                             (expected :msg, :julia, or :typedesc)"))
    end
end

# `:typedesc` — the raw wire blob as canonical JSON. Requires the entry to carry a
# `TypeDescriptionMsg`; an `:ament` entry (parsed from local interface text) has none.
function _export_typedesc(entry::RegistryEntry, to::AbstractString)
    entry.td === nothing &&
        throw(ArgumentError("type $(entry.info.name) has no TypeDescription to \
                             export as :typedesc (an :ament entry parsed from \
                             local interface text carries none — export it as \
                             :julia or :msg instead)"))
    _, _, bare = split_ros_name(entry.info.name)
    isdir(to) || mkpath(to)
    path = joinpath(to, bare * ".typedesc.json")
    open(path, "w") do io
        println(io, entry.info.name)
        print(io, to_ros2_json(entry.td))
    end
    return path
end

# `:msg`/`:srv`/`:action` — `IL.unparse` into a ROS interface-package layout;
# qualifier and extension follow the IL kind. A discovered type emits fields
# only (RIHS01/`lift` drops constants and defaults), wire-faithful but lossy.
function _export_interface_text(entry::RegistryEntry, to::AbstractString)
    package, _, _ = split_ros_name(entry.info.name)
    qualifier, ext = _il_qualifier_ext(entry.il)
    _, _, bare = split_ros_name(entry.info.name)
    dir = joinpath(to, package, qualifier)
    isdir(dir) || mkpath(dir)
    path = joinpath(dir, bare * ext)
    open(path, "w") do io
        IL.unparse(io, entry.il)
    end
    return path
end

# `:julia` — the generated `.jl` source for the whole package, the strongest
# port-forward (the type becomes static/fast). Emits `generate_code`'s `Expr`s as
# pretty-printed source so the file is human-readable and `include`-able.
function _export_julia(entry::RegistryEntry, to::AbstractString)
    package, _, _ = split_ros_name(entry.info.name)
    # Exported file is reparsed standalone, so each module emits its own imports.
    exprs = _generate_exprs_multi(_closure_ils(entry, package); emit_imports=true)
    isdir(to) || mkpath(to)
    fname = isempty(package) ? split_ros_name(entry.info.name)[3] : package
    path = joinpath(to, fname * ".jl")
    open(path, "w") do io
        println(io, "# Generated by ROSNode export_typesupport(:julia) for ",
                entry.info.name)
        println(io, "# RIHS01: ", to_rihs_string(entry.info.hash))
        for ex in exprs
            println(io, ex)
            println(io)
        end
    end
    return path
end

# Map an IL interface kind to its (qualifier, file-extension) for `:msg` export.
_il_qualifier_ext(::IL.RMessage) = ("msg", ".msg")
_il_qualifier_ext(::IL.RService) = ("srv", ".srv")
_il_qualifier_ext(::IL.RAction)  = ("action", ".action")
_il_qualifier_ext(_)             = ("msg", ".msg")

# ── type_info specialization for registered (dynamic) types ─────────────────
# serialization.jl's `type_info(::Type{T})` returns the zero hash for a generated
# type (the Humble placeholder): correct for keyexpr structure, wrong for
# cross-version matching. A registered type knows its real hash (its registry
# key), so this exposes a lookup keyed by the generated type itself — `realize!`
# stamps the entry and `type_info_of` recovers the real `TypeInfo`. Static types
# keep the reflective default; dynamic ones carry the verified hash.

# Reverse map: generated `Type` → its `RegistryEntry`, populated at `realize!`.
# Kept as an IdDict so identity (not name) keys it — distinct hash-versions are
# distinct generated types.
const _TYPE_TO_ENTRY = IdDict{Type, RegistryEntry}()
const _TYPE_TO_ENTRY_LOCK = ReentrantLock()

# Hook realize! to record the reverse mapping once a type exists.
function _record_type_entry!(entry::RegistryEntry)
    entry.type isa Type || return entry
    @lock _TYPE_TO_ENTRY_LOCK _TYPE_TO_ENTRY[entry.type] = entry
    entry
end

"""
    type_info_of(::Type{T}) -> TypeInfo

The `TypeInfo` (name + RIHS01) for a type. A dynamically-generated type returns
its registry-verified hash; a statically-included type falls back to
the reflective [`type_info`](@ref) (zero-hash placeholder). This is
the registry's specialization of type identity: a registered type carries its true hash,
so its keyexprs and liveliness match the wire across versions.
"""
function type_info_of(::Type{T}) where {T}
    entry = @lock _TYPE_TO_ENTRY_LOCK get(_TYPE_TO_ENTRY, T, nothing)
    entry === nothing ? type_info(T) : entry.info
end

# The registry entry bound to a generated type (`.td` is the wire TypeDescription),
# or `nothing` if the type was never registered.
_entry_of(::Type{T}) where {T} =
    @lock _TYPE_TO_ENTRY_LOCK get(_TYPE_TO_ENTRY, T, nothing)

# ── service-level type identity (rmw_zenoh service/client keyexpr) ──────────────
# rmw_zenoh keys a service/client off the service type (`pkg::srv::dds_::Base_`
# plus the service RIHS01); a keyexpr built from the request message's info never
# matches a peer. `service_type_info_of` synthesizes the rosidl service type
# description (ROSMessages.service_rihs01) and returns the service-level
# `TypeInfo`. The synthesized service-event closure references
# `service_msgs/msg/ServiceEventInfo`, read from the vendored canonical set.

const _SERVICE_EVENT_INFO_NAME = "service_msgs/msg/ServiceEventInfo"
const _SEI_TD = Ref{Union{Nothing, TypeDescriptionMsg}}(nothing)
const _SEI_LOCK = ReentrantLock()

# The vendored `service_msgs/msg/ServiceEventInfo` type description (memoized).
function _service_event_info_td()
    @lock _SEI_LOCK begin
        _SEI_TD[] === nothing || return _SEI_TD[]
        for e in _canonical_entries()
            if e.info.name == _SERVICE_EVENT_INFO_NAME && e.td !== nothing
                return _SEI_TD[] = e.td
            end
        end
        return nothing
    end
end

"""
    service_type_info_of(Req, Resp) -> Union{TypeInfo, Nothing}

The rmw_zenoh *service-level* `TypeInfo` for a service whose request /
response message types are `Req` / `Resp`: the service type name (`pkg/srv/Base`,
keyed on the wire as `pkg::srv::dds_::Base_`) and the ROS2 service RIHS01 the
service/client keyexpr must carry to match a peer. Built by stripping the
`_Request` suffix off the request's qualified name and hashing the synthesized
service type description. Returns `nothing` unless both message types are
registered with their wire `TypeDescription`s and the vendored
`service_msgs/msg/ServiceEventInfo` is available — the caller then falls back to
the request type's own info (self-consistent locally, but not peer-compatible).
"""
function service_type_info_of(::Type{Req}, ::Type{Resp}) where {Req, Resp}
    re = _entry_of(Req)
    rs = _entry_of(Resp)
    (re === nothing || rs === nothing || re.td === nothing || rs.td === nothing) &&
        return nothing
    sei = _service_event_info_td()
    sei === nothing && return nothing
    name = re.td.type_description.type_name
    sname = endswith(name, "_Request") ? name[1:end-length("_Request")] : name
    h = type_hash_from_rihs_string(ROSMessages.service_rihs01(sname, re.td, rs.td, sei))
    h === nothing && return nothing
    return TypeInfo(sname, h)
end

