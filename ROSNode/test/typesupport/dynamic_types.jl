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
    @ros_msgs "../../vendor/builtin_interfaces"
end

# `@ros_import … as Alias` on a (provided) vendored type binds straight to the
# canonical struct under the chosen name.
module _AsImport
    using ROSNode
    @ros_import "builtin_interfaces/msg/Time" as Stamp
end

# Hybrid binding: a fully-qualified type binds its bare leaf; a bare package binds
# only the module (no leaf flood).
module _LeafImport
    using ROSNode
    @ros_import "builtin_interfaces/msg/Time"    # → leaf `Time` (+ nested path)
    @ros_import "rcl_interfaces"                  # → module `rcl_interfaces` only
end

# A service import binds the `Foo` namespace module (→ `Foo.Request`/`Foo.Response`).
module _SrvImport
    using ROSNode
    @ros_import "rcl_interfaces/srv/GetParameters"
end

# BYO interfaces: `from=` adds a local search root, types referenced by name and run
# through the full registration pipeline (unlike raw `@ros_msgs`). Fixtures under
# `test/fixtures/byo`; the relative root resolves against this source file.
_dt("expanding @ros_import from= (BYO codegen)")
module _ByoImport
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/msg/Widget" "robot_msgs/srv/DoThing" "robot_msgs/action/Process"
end
module _ByoAlias
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/msg/Widget" as W
end
module _ByoPkg
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs"           # bare package → all interfaces
end
_dt("@ros_import from= modules ready")

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

    @testset "dynamic codegen resolves a nested message ref (closure)" begin
        _dt("dynamic codegen with nested ref")
        # The wire shape a type-less subscriber gets: Chatter{header: demo_msgs/msg/Header, …}
        # plus the referenced Header. realize! must generate Header into the SAME gensym module,
        # else `header::demo_msgs.msg.Header` is an UndefVarError and codegen leaves type=nothing.
        htd   = _internal_td("float64 stamp\nstring frame\n", "Header", "demo_msgs").type_description
        cmain = _internal_td("demo_msgs/Header header\nstring data\nint32 seq\n",
                             "Chatter", "demo_msgs").type_description
        tdm   = TypeDescriptionMsg(cmain, [htd])
        info  = TypeInfo("demo_msgs/msg/Chatter", type_hash_from_rihs_string(calculate_rihs01_hash(tdm)))
        entry = ROSNode.realize!(ROSNode.entry_from_type_description(tdm, info; verify = false))
        @test entry.mod !== nothing && entry.type !== nothing        # codegen succeeded (was nothing)
        @test fieldnames(entry.type) === (:header, :data, :seq)
        htype = fieldtype(entry.type, :header)                       # nested type, generated alongside
        @test nameof(htype) === :Header && fieldnames(htype) === (:stamp, :frame)
    end

    @testset "type_sources regenerate from registry IL (include_type_sources)" begin
        _dt("type_sources from registry IL")
        # Session-free: build a real TypeRegistry from the well-known entries (a bare
        # registry suffices — `_type_sources` takes `reg` directly, never `_ctx`, which
        # has no method for a TypeRegistry). Serve the multi-source TypeDescription
        # closure (Field, FieldType, IndividualTypeDescription) off its stored `.td`.
        reg = ROSNode.TypeRegistry()
        for e in _wellknown_entries()
            ROSNode.register_type!(reg, e.info, e)
        end
        fentry = only(filter(e -> endswith(e.info.name, "/msg/TypeDescription"),
                             _wellknown_entries()))
        tdmsg = fentry.td                                # well-known entries carry `.td`
        srcs = ROSNode._type_sources(reg, tdmsg)
        @test eltype(srcs) === ROSNode.WireTypeSource
        @test !isempty(srcs)
        @test length(srcs) > 1                           # main + referenced closure
        # main first; .msg encoding; every name fully-qualified
        @test srcs[1].type_name == "type_description_interfaces/msg/TypeDescription"
        @test srcs[1].encoding == "msg"
        @test all(s -> occursin("/", s.type_name), srcs)
        # regenerated source text — assert a token unique to a field line (the
        # `referenced_type_descriptions` field), not a short token (`name`/`type`/
        # `description`) that also appears in nested type-name lines.
        @test !isempty(srcs[1].raw_file_contents)
        @test occursin("referenced_type_descriptions", srcs[1].raw_file_contents)
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

    @testset "@ros_cache tolerates a not-yet-created cache dir (first run)" begin
        _dt("@ros_cache first-run missing dir")
        dir = joinpath(mktempdir(), "ros_typesupport")     # the first-run case: absent
        @test !isdir(dir)
        saved = copy(_STATIC_TYPES.cache_dirs)              # restore: the macro pushes the opt-in dir
        try
            # Expanding/running `@ros_cache` on a missing dir must not throw — its emitted
            # `include_dependency(dir)` errors on a missing path. The macro now creates the
            # dir so the precompile dependency is trackable from run 1 (D5/D9 convergence).
            @eval module _D9CacheFirstRun
                using ROSNode
                @ros_cache $dir
            end
            @test isdir(dir)                                # created at expansion
            @test dir in _STATIC_TYPES.cache_dirs           # opt-in absorbed
        finally
            empty!(_STATIC_TYPES.cache_dirs); union!(_STATIC_TYPES.cache_dirs, saved)
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

    @testset "@ros_import ... as binds the canonical copy under the alias" begin
        _dt("@ros_import as")
        @test _AsImport.Stamp === ROSNode.Interfaces.builtin_interfaces.msg.Time
        @test !isdefined(_AsImport, :builtin_interfaces)    # `as` doesn't leak the bare path
    end

    @testset "@ros_import binds leaf for a type, module for a package" begin
        _dt("@ros_import hybrid binding")
        # fully-qualified type → bare leaf, same identity as the nested path + canonical
        @test isdefined(_LeafImport, :Time)
        @test _LeafImport.Time === ROSNode.Interfaces.builtin_interfaces.msg.Time
        @test _LeafImport.builtin_interfaces.msg.Time === _LeafImport.Time
        # bare package → module bound, but no leaf flood
        @test isdefined(_LeafImport, :rcl_interfaces)
        @test !isdefined(_LeafImport, :Log)
        @test isdefined(_LeafImport.rcl_interfaces.msg, :Log)
    end

    @testset "service/action sections get a Foo.Request namespace alias" begin
        _dt("Foo.Request namespacing")
        G = ROSNode.Interfaces.rcl_interfaces.srv
        # `Foo` is a tag *type* (usable as a handle/type-param) exposing the sections
        # by short name via getproperty; wire names/structs unchanged.
        @test G.GetParameters isa Type && !(G.GetParameters isa Module)
        @test G.GetParameters.Request === G.GetParameters_Request
        @test G.GetParameters.Response === G.GetParameters_Response
        @test nameof(G.GetParameters) === :GetParameters     # getfield fallback intact
        # `@ros_import` of a service binds the bare `Foo` tag
        @test _SrvImport.GetParameters isa Type
        @test _SrvImport.GetParameters.Request === G.GetParameters_Request
    end

    @testset "@ros_import from=: BYO interfaces generate + leaf-bind" begin
        _dt("BYO from= generation")
        W = _ByoImport.robot_msgs.msg.Widget
        @test _ByoImport.Widget === W                       # message leaf-binds
        @test fieldnames(W) == (:label, :count, :samples)
        w = W(label = "x", count = Int32(3), samples = Float64[1.0, 2.0])
        @test w.label == "x" && w.count == Int32(3) && w.samples == [1.0, 2.0]
        # service: sibling Request/Response + the Foo.Request namespace binding
        @test isdefined(_ByoImport.robot_msgs.srv, :DoThing_Request)
        @test _ByoImport.DoThing.Request === _ByoImport.robot_msgs.srv.DoThing_Request
        @test _ByoImport.DoThing.Response === _ByoImport.robot_msgs.srv.DoThing_Response
        # action: the three sibling sections
        A = _ByoImport.robot_msgs.action
        @test isdefined(A, :Process_Goal) && isdefined(A, :Process_Result) &&
              isdefined(A, :Process_Feedback)
    end

    @testset "@ros_import from=: BYO types reach the registry (≠ @ros_msgs)" begin
        _dt("BYO from= registration")
        # The crux: `@ros_import` records `(type, TypeDescription-JSON)` for Context to
        # register; raw `@ros_msgs` (see `_StrayTime`) emits structs but records nothing.
        @test isdefined(_ByoImport, :__ros_static_types__)
        @test !isdefined(_StrayTime, :__ros_static_types__)
        pairs = _ByoImport.__ros_static_types__
        @test any(p -> p[1] === _ByoImport.robot_msgs.msg.Widget, pairs)
        # Absorbing interns the entry under its real wire name (what keyexpr-only
        # resolution and the §13 server look up) — the registry coverage @ros_msgs lacks.
        absorb_static_types!(_ByoImport)
        @test any(e -> e.info.name == "robot_msgs/msg/Widget", _STATIC_TYPES.entries)
        @test any(e -> e.info.name == "robot_msgs/srv/DoThing_Request", _STATIC_TYPES.entries)
    end

    @testset "@ros_import from=: as-alias and bare-package forms" begin
        _dt("BYO from= as + bare package")
        @test fieldnames(_ByoAlias.W) == (:label, :count, :samples)   # `as` → the struct
        # bare package binds all interfaces in robot_msgs
        @test isdefined(_ByoPkg.robot_msgs.msg, :Widget)
        @test isdefined(_ByoPkg.robot_msgs.srv, :DoThing_Request)
        @test isdefined(_ByoPkg.robot_msgs.action, :Process_Goal)
        # `_ByoImport` already registered these, so the bare import *aliases* the
        # provided msg/srv (single-copy) while still generating the action — the two
        # must coexist under one `module robot_msgs` (a second would clobber the first).
        @test _ByoPkg.robot_msgs.msg.Widget === _ByoImport.robot_msgs.msg.Widget
        @test _ByoPkg.robot_msgs.srv.DoThing_Request === _ByoImport.robot_msgs.srv.DoThing_Request
    end

    @testset "@ros_import from=: search order is from-roots → vendored → ament" begin
        _dt("BYO from= precedence")
        byo    = abspath(joinpath(@__DIR__, "..", "fixtures", "byo"))
        shadow = abspath(joinpath(@__DIR__, "..", "fixtures", "shadow"))   # a divergent Time copy
        # a name only in the BYO root resolves there
        @test ROSNode._resolve_one_iface("robot_msgs", "msg", "Widget", [byo]) ==
              joinpath(byo, "robot_msgs", "msg", "Widget.msg")
        # a name in BOTH a root and vendored resolves to the root (root shadows vendored)
        @test ROSNode._resolve_one_iface("builtin_interfaces", "msg", "Time", [shadow]) ==
              joinpath(shadow, "builtin_interfaces", "msg", "Time.msg")
        # with no roots, the same name falls through to vendored
        @test ROSNode._resolve_one_iface("builtin_interfaces", "msg", "Time") ==
              joinpath(ROSNode._VENDOR_DIR, "builtin_interfaces", "msg", "Time.msg")
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
