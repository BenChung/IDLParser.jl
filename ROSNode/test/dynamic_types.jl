# D5 dynamic-type support — pure-logic unit tests (no live Zenoh session). The
# session-based round-trips (GetTypeDescription over the wire, keyexpr-only sub,
# ROSNode↔ROSNode discovery) live in `dynamic_live.jl`, run separately.

using ROSNode
using ROSNode: to_wire_td, from_wire_td, WireField, WireFieldType, WireKeyValue,
               WireTypeDescription, GetTypeDescription_Request, GetTypeDescription_Response,
               describe_type, type_info_of, encode, decode, _wellknown_entries,
               _scan_for_struct, _cache_store, _cache_load, _cache_enabled,
               enable_project_cache!, disable_project_cache!, flush_type_cache,
               absorb_static_types!, _STATIC_TYPES
using ROSMessages: message_il, lower, type_description_from_struct, calculate_rihs01_hash,
                   to_ros2_json, TypeDescription, TypeDescriptionMsg, FieldDescription,
                   FieldTypeDescription
using ROSZenoh: TypeInfo, TypeHash, to_rihs_string, type_hash_from_rihs_string
import Zenoh

# Build an internal TypeDescriptionMsg from a .msg source (no refs ⇒ leaf).
function _internal_td(src, name, pkg)
    il = message_il(src; name = name)
    ast = _scan_for_struct(lower(il; package = pkg))
    main = type_description_from_struct(ast, name; package = pkg, qualifier = "msg")
    return TypeDescriptionMsg(main, TypeDescription[])
end

# Minimal CDR round-trip (mirrors serialization.jl) for the mock marshal tests.
function _cdr_roundtrip(msg, ::Type{T}) where {T}
    bytes = encode(msg)
    mem = Zenoh.as_memory(bytes, UInt8)
    return decode(mem, T; view = false)
end

# Flushed progress marker — these tests run runtime codegen (`@ros_import`, eval'd
# type generation), which is slow on first JIT; the banner shows which step is live.
_dt(msg::AbstractString) = (@info "  · dynamic_types: $msg"; flush(stdout); flush(stderr))

# A module exercising @ros_import (vendored, name-resolved, with transitive closure).
# This generates code at module-definition time (first-JIT heavy) — announce it.
_dt("expanding @ros_import module (codegen)")
module _D5Import
    using ROSNode
    @ros_import "type_description_interfaces/msg/Field"   # → pulls FieldType transitively
end
_dt("@ros_import module ready")

# A *stray* copy of a vendored type — what `@ros_msgs` on the vendored sources
# produces (no aliasing). Used to exercise the single-copy convert guard.
module _StrayTime
    using ROSMessages: @ros_msgs
    @ros_msgs "../vendor/builtin_interfaces"
end

@testset "D5 dynamic type support" begin

    @testset "wire ⇄ internal TypeDescription preserves RIHS01" begin
        _dt("wire ⇄ internal RIHS01")
        # leaf (no refs)
        kv = _internal_td("string key\nstring value\n", "KeyValue", "type_description_interfaces")
        @test calculate_rihs01_hash(from_wire_td(to_wire_td(kv))) == calculate_rihs01_hash(kv)
        # referencing type (Field → FieldType), with a real closure
        ft = FieldTypeDescription(0x01, UInt64(0), UInt64(0), "type_description_interfaces/msg/FieldType")
        fmain = TypeDescription("type_description_interfaces/msg/Field",
                    [FieldDescription("name", FieldTypeDescription(0x11)),
                     FieldDescription("type", ft),
                     FieldDescription("default_value", FieldTypeDescription(0x11))])
        ftd = _internal_td("uint8 type_id\nuint64 capacity\nuint64 string_capacity\nstring nested_type_name\n",
                           "FieldType", "type_description_interfaces").type_description
        f = TypeDescriptionMsg(fmain, [ftd])
        wire = to_wire_td(f)
        @test wire isa WireTypeDescription
        @test calculate_rihs01_hash(from_wire_td(wire)) == calculate_rihs01_hash(f)
    end

    @testset "well-known bootstrap entries (hashes match real ROS2)" begin
        _dt("well-known bootstrap entries")
        es = Dict(e.info.name => e for e in _wellknown_entries())
        @test length(es) == 8
        # closures are the canonical sorted referenced sets
        @test length(es["type_description_interfaces/msg/Field"].td.referenced_type_descriptions) == 1
        @test length(es["type_description_interfaces/msg/TypeDescription"].td.referenced_type_descriptions) == 3
        @test length(es["type_description_interfaces/srv/GetTypeDescription_Response"].td.referenced_type_descriptions) == 6
        # exact RIHS01 (computed identically to real ROS2's tooling)
        @test to_rihs_string(es["type_description_interfaces/msg/Field"].info.hash) ==
              "RIHS01_c0b01379cd4226281285ccaf6be46653968f855f7c5e41614ff5d7a854efef7c"
        @test to_rihs_string(es["type_description_interfaces/msg/FieldType"].info.hash) ==
              "RIHS01_a70b6dd919645a03a3586f7f821defbc886ea3e531a1d95cc0f380a3973ccaa6"
        # every well-known entry is bound to its compiled type (no codegen needed)
        @test all(e -> e.type !== nothing && e.mod !== nothing, values(es))
    end

    @testset "GetTypeDescription request/response CDR marshal" begin
        _dt("GetTypeDescription CDR marshal")
        req = GetTypeDescription_Request(type_name = "std_msgs/msg/String",
                                         type_hash = "RIHS01_" * repeat("ab", 32),
                                         include_type_sources = false)
        r2 = _cdr_roundtrip(req, GetTypeDescription_Request)
        @test r2.type_name == "std_msgs/msg/String"
        @test r2.type_hash == req.type_hash
        @test r2.include_type_sources == false

        # a Response carrying a real TypeDescription (Field + its FieldType closure)
        fld = _wellknown_entries()[1]  # any; use the registry-built Field below instead
        fentry = only(filter(e -> endswith(e.info.name, "/msg/Field"), _wellknown_entries()))
        resp = GetTypeDescription_Response(successful = true, failure_reason = "",
                    type_description = to_wire_td(fentry.td),
                    type_sources = ROSNode.WireTypeSource[],
                    extra_information = ROSNode.WireKeyValue[])
        r3 = _cdr_roundtrip(resp, GetTypeDescription_Response)
        @test r3.successful
        @test r3.type_description.type_description.type_name == "type_description_interfaces/msg/Field"
        @test length(r3.type_description.referenced_type_descriptions) == 1
        # the decoded reply round-trips back to the advertised hash (the integrity gate)
        @test calculate_rihs01_hash(from_wire_td(r3.type_description)) ==
              to_rihs_string(fentry.info.hash)
    end

    @testset "content-addressed cache is opt-in + self-validating" begin
        _dt("content-addressed cache opt-in")
        td = _internal_td("string data\n", "String", "std_msgs")
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(td))
        info = TypeInfo("std_msgs/msg/String", hash)
        disable_project_cache!()
        try
            # off by default → store/load are no-ops (no surprise files)
            @test !_cache_enabled()
            _cache_store(info, td)
            @test _cache_load(info) === nothing
            # enabled → store/load/self-validate
            tmp = mktempdir()
            enable_project_cache!(tmp)
            @test _cache_enabled()
            _cache_store(info, td)
            loaded = _cache_load(info)
            @test loaded !== nothing
            @test calculate_rihs01_hash(loaded) == to_rihs_string(hash)
            # a corrupt/mismatched blob is discarded on load
            badinfo = TypeInfo("std_msgs/msg/String", TypeHash(0x01, ntuple(i -> UInt8(i), 32)))
            write(joinpath(tmp, bytes2hex(collect(badinfo.hash.value)) * ".json"),
                  "std_msgs/msg/String\n" * to_ros2_json(td))
            @test _cache_load(badinfo) === nothing   # hash gate rejects it
        finally
            disable_project_cache!()
        end
    end

    @testset "@ros_import: canonical types alias to the single Interfaces copy" begin
        _dt("@ros_import aliasing")
        @test isdefined(_D5Import.type_description_interfaces.msg, :Field)
        @test isdefined(_D5Import.type_description_interfaces.msg, :FieldType)   # transitive
        # A vendored (canonical) type is aliased to the one compiled `Interfaces`
        # struct, not re-generated — one wire type (name + RIHS01) ⇒ one Julia type.
        @test _D5Import.type_description_interfaces.msg.Field ===
              ROSNode.Interfaces.type_description_interfaces.msg.Field
        @test _D5Import.type_description_interfaces.msg.FieldType ===
              ROSNode.Interfaces.type_description_interfaces.msg.FieldType
        # carries the real RIHS01 (identical to real ROS2), recorded for the canonical
        # type (so `type_info_of` recovers it through the alias)
        ti = type_info_of(_D5Import.type_description_interfaces.msg.Field)
        @test ti.name == "type_description_interfaces/msg/Field"
        @test to_rihs_string(ti.hash) ==
              "RIHS01_c0b01379cd4226281285ccaf6be46653968f855f7c5e41614ff5d7a854efef7c"
    end

    @testset "@ros_import action closure pulls vendored UUID + Time" begin
        _dt("@ros_import action closure")
        # An action's generated protocol types (SendGoal/GetResult/FeedbackMessage)
        # reference unique_identifier_msgs/UUID and builtin_interfaces/Time, so the
        # import closure must pull them — resolved here from ROSNode's vendored dir
        # (the action itself comes from a throwaway ament prefix).
        amroot = mktempdir()
        actdir = joinpath(amroot, "share", "example_msgs", "action")
        mkpath(actdir)
        write(joinpath(actdir, "Fibonacci.action"),
              "int32 order\n---\nint32[] sequence\n---\nint32[] partial_sequence\n")
        withenv("AMENT_PREFIX_PATH" => amroot) do
            specs = ROSNode._resolve_import_closure(["example_msgs/action/Fibonacci"])
            names = Set("$(s.package)/$(s.qualifier)/$(s.bare)" for s in specs)
            @test "example_msgs/action/Fibonacci" in names
            @test "unique_identifier_msgs/msg/UUID" in names
            @test "builtin_interfaces/msg/Time" in names
            # the pulled deps resolve to the shipped vendor copies, not ament
            uuid = only(filter(s -> s.bare == "UUID", specs))
            @test startswith(uuid.path, ROSNode._VENDOR_DIR)
        end
    end

    @testset "single-copy convert guard rejects a stray duplicate" begin
        _dt("single-copy convert guard")
        T = ROSNode.Interfaces.builtin_interfaces.msg.Time
        canon = T(sec = Int32(1), nanosec = UInt32(2))
        @test convert(T, canon) === canon                       # identity unaffected
        stray = _StrayTime.builtin_interfaces.msg.Time(sec = Int32(1), nanosec = UInt32(2))
        @test typeof(stray) !== T                                # genuinely distinct
        err = try; convert(T, stray); nothing; catch e; e; end
        @test err isa ArgumentError
        msg = sprint(showerror, err)
        @test occursin("builtin_interfaces/msg/Time", msg)
        @test occursin("as <Alias>", msg)
    end

    @testset "flush_type_cache exports the cache" begin
        _dt("flush_type_cache export")
        tmp = mktempdir()
        td = _internal_td("string data\n", "String", "std_msgs")
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(td))
        write(joinpath(tmp, bytes2hex(collect(hash.value)) * ".json"),
              "std_msgs/msg/String\n" * to_ros2_json(td))
        out = mktempdir()
        written = flush_type_cache(out; dir = tmp, format = :msg)
        @test any(p -> endswith(p, joinpath("msg", "String.msg")), written)
        @test occursin("string data", read(only(written), String))
    end
end
