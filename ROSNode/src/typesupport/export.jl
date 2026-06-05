# ── export_typesupport: graduate a discovered type into durable form ─────────
# The explicit "port forward" half (vs. the automatic cache). Three formats per
# §11: `:msg` (an interface-package layout from `IL.unparse`), `:julia` (the
# `generate_code` output as `.jl` source — the type goes static/fast again), and
# `:typedesc` (the raw wire blob, language-agnostic). RIHS01 is the roundtrip
# invariant across all three.

"""
    export_typesupport(node_or_ctx, names; to=pwd(), format=:msg) -> Vector{String}

Graduate discovered (or any registered) types out of the ephemeral registry into
durable, user-owned form (§11). `names` is a type name (or iterable of them);
`format` selects the output:

- `:msg` / `:julia_text` — `IL.unparse` the IL into ROS interface text
  (`<to>/<package>/<qualifier>/<Name>.<ext>`). Most portable: `colcon build` turns
  it into a real package. (A *discovered* type lost its constants/defaults through
  RIHS01/`lift`, so the emitted text carries fields only — wire-faithful, lossy as
  docs.)
- `:julia` — emit `Generation.generate_code`'s output as `.jl` source
  (`<to>/<package>.jl`) to check in and `include`. The strongest port-forward: the
  once-dynamic type becomes static, precompilable, and back on the min-copy fast
  path.
- `:typedesc` — the raw `TypeDescription` JSON bundle (`<to>/<Name>.typedesc.json`);
  language-agnostic and reloadable by us.

Returns the paths written. Errors if a requested name isn't registered (resolve it
first via discovery/ament).
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

# Find a registered entry by name alone (export is name-addressed; if multiple
# hash versions coexist, the newest-registered wins — `Dict` insertion order
# isn't guaranteed, so we just take any match and note it). Returns `nothing` if
# unregistered.
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
# `TypeDescriptionMsg` (wire/cache provenance); a purely-static entry has none.
function _export_typedesc(entry::RegistryEntry, to::AbstractString)
    entry.td === nothing &&
        throw(ArgumentError("type $(entry.info.name) has no TypeDescription to \
                             export as :typedesc (it was acquired statically)"))
    _, _, bare = split_ros_name(entry.info.name)
    isdir(to) || mkpath(to)
    path = joinpath(to, bare * ".typedesc.json")
    open(path, "w") do io
        println(io, entry.info.name)
        print(io, to_ros2_json(entry.td))
    end
    return path
end

# `:msg`/`:srv`/`:action` — `IL.unparse` into a ros interface-package layout. The
# qualifier/extension follow the IL kind. Constants/defaults are absent on a
# discovered type (RIHS01/`lift` dropped them); the text is fields-only but
# wire-faithful.
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
    # `emit_imports=true`: the exported file is reparsed source, so each generated
    # module carries its own `import StaticArrays, CDRSerialization` and is
    # self-contained / `include`-able.
    exprs = _generate_exprs(entry.il, package; emit_imports=true)
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

# ── type_info specialization for registered (dynamic) types (§11) ───────────
# serialization.jl's `type_info(::Type{T})` defaults to the zero hash for a
# generated type (the Humble placeholder) — correct for keyexpr structure, wrong
# for cross-version matching. A registered type *does* know its real hash (it's
# the registry key), so we expose a lookup keyed by the generated type itself:
# realize-time stamps the entry, and `type_info_of` recovers the real `TypeInfo`.
# Static types keep the reflective default; dynamic ones carry the verified hash.

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

The `TypeInfo` (name + *real* RIHS01) for a type — the registry's verified hash
for a dynamically-generated type, falling back to serialization.jl's reflective
[`type_info`](@ref) (zero-hash placeholder) for a statically-included one. This is
the §11 specialization of type identity: a registered type carries its true hash,
so keyexprs and liveliness for it match the wire across versions.
"""
function type_info_of(::Type{T}) where {T}
    entry = @lock _TYPE_TO_ENTRY_LOCK get(_TYPE_TO_ENTRY, T, nothing)
    entry === nothing ? type_info(T) : entry.info
end

# The registry entry bound to a generated type (`.td` is the wire TypeDescription),
# or `nothing` if the type was never registered.
_entry_of(::Type{T}) where {T} =
    @lock _TYPE_TO_ENTRY_LOCK get(_TYPE_TO_ENTRY, T, nothing)

# ── service-level type identity (rmw_zenoh service/client keyexpr, §8) ──────────
# rmw_zenoh keys a service/client off the SERVICE type (`pkg::srv::dds_::Base_` +
# the service RIHS01), not the request message — so a service keyexpr built from
# the request type's info never matches a peer. `service_type_info_of` synthesizes
# the rosidl service type description (ROSMessages.service_rihs01) and returns the
# service-level `TypeInfo`. The synthesized service-event closure references
# `service_msgs/msg/ServiceEventInfo`, which we read from the vendored canonical set.

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

The rmw_zenoh *service-level* [`TypeInfo`](@ref) for a service whose request /
response message types are `Req` / `Resp`: the service type name (`pkg/srv/Base`,
keyed on the wire as `pkg::srv::dds_::Base_`) and the ROS2 service RIHS01 the
service/client keyexpr must carry to match a peer (§8). Built by stripping the
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

