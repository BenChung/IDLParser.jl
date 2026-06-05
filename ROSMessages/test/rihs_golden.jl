module RIHSGoldenTests
using IDLParser
using ROSMessages
using Test
using Moshi.Match: @match
import IDLParser.Parse: TypeDecl
import ROSMessages: parse_msg
import ROSMessages: rihs01_hash, type_description_from_struct, TypeDescription,
    TypeDescriptionMsg, service_rihs01,
    send_goal_service_rihs01, get_result_service_rihs01, cancel_goal_service_rihs01,
    TYPE_ID_UINT8, TYPE_ID_BYTE, TYPE_ID_INT32, TYPE_ID_NESTED_TYPE, ARRAY_OFFSET,
    UNBOUNDED_SEQUENCE_OFFSET, FieldDescription, FieldTypeDescription

# RIHS01 golden coverage for the parts `ros2.jl`'s flat single-field cases
# don't reach: a 3+-deep transitive reference closure (`referenced_type_
# descriptions` sorted by `type_name`, ROS2 JSON shape) and the char→uint8 /
# byte→octet primitive collapse.

# The first struct decl in a parsed message — messages with constants hoist a
# `<Name>_Constants` module ahead of the struct, so we can't just take `[1]`.
function _first_struct(decls)
    i = findfirst(decls) do d
        d isa TypeDecl.Type && @match d begin
            TypeDecl.StructDecl(_, _, _) => true
            _ => false
        end
    end
    return decls[i]
end

# Build a referenced `TypeDescription` from a message's source body + its
# fully-qualified `pkg/msg/Name`, exactly as a caller assembling a closure
# would. The leading package segment resolves the body's *relative* field refs
# (e.g. `Pose`'s `Point position` → `geometry_msgs/msg/Point`).
function _ref(name, body)
    parts = split(name, '/')
    pkg = String(parts[1])
    short = String(parts[end])
    type_description_from_struct(_first_struct(parse_msg(body; name=short)), name; package=pkg)
end

# The standard-interface bodies these tests reference, by fully-qualified name.
# Verbatim field lines from the vendored ROS2 .msg sources (defaults dropped —
# RIHS01 excludes them, and they don't affect the parse here either).
const TIME_BODY  = "int32 sec\nuint32 nanosec"
const POINT_BODY = "float64 x\nfloat64 y\nfloat64 z"
const QUAT_BODY  = "float64 x\nfloat64 y\nfloat64 z\nfloat64 w"
const POSE_BODY  = "Point position\nQuaternion orientation"
const HEADER_BODY = "builtin_interfaces/Time stamp\nstring frame_id"
const POSESTAMPED_BODY = "std_msgs/Header header\nPose pose"

# service_msgs/msg/ServiceEventInfo body — constants get hoisted out of the
# field set, leaving event_type / stamp / client_gid / sequence_number.
const SEI_BODY = """
    uint8 REQUEST_SENT = 0
    uint8 REQUEST_RECEIVED = 1
    uint8 RESPONSE_SENT = 2
    uint8 RESPONSE_RECEIVED = 3
    uint8 event_type
    builtin_interfaces/Time stamp
    char[16] client_gid
    int64 sequence_number
    """

@testset "RIHS01 golden — multi-ref + char/byte" begin

    # ---- geometry_msgs/msg/PoseStamped — a 5-type transitive closure -------
    #
    # PoseStamped → Header, Pose; Pose → Point, Quaternion; Header → Time.
    # The caller flattens the whole graph into `references` and sorts by
    # `type_name` (the cross-impl-stable order RIHS01 hashes over).
    ps_refs() = sort!(TypeDescription[
            _ref("std_msgs/msg/Header", HEADER_BODY),
            _ref("geometry_msgs/msg/Pose", POSE_BODY),
            _ref("geometry_msgs/msg/Point", POINT_BODY),
            _ref("geometry_msgs/msg/Quaternion", QUAT_BODY),
            _ref("builtin_interfaces/msg/Time", TIME_BODY),
        ]; by = t -> t.type_name)

    @testset "transitive closure is complete + sorted" begin
        refs = ps_refs()
        names = [t.type_name for t in refs]
        # Every transitively-reachable type is present (none dropped, none extra).
        @test Set(names) == Set([
            "builtin_interfaces/msg/Time", "geometry_msgs/msg/Point",
            "geometry_msgs/msg/Pose", "geometry_msgs/msg/Quaternion",
            "std_msgs/msg/Header"])
        # …and in `type_name` order — the key the hash depends on.
        @test names == sort(names)
        @test names == [
            "builtin_interfaces/msg/Time", "geometry_msgs/msg/Point",
            "geometry_msgs/msg/Pose", "geometry_msgs/msg/Quaternion",
            "std_msgs/msg/Header"]
    end

    @testset "PoseStamped golden hash" begin
        ps = _first_struct(parse_msg(POSESTAMPED_BODY; name="PoseStamped"))
        # `package` resolves the relative `Pose pose` field → geometry_msgs/msg/Pose.
        h = rihs01_hash(ps, "geometry_msgs/msg/PoseStamped";
                        package="geometry_msgs", references=ps_refs())
        # Regression anchor: SHA-256 of the spec-shaped JSON, computed by hand
        # from `rihs01.jl`'s documented encoding (the same method reproduces the
        # published std_msgs/msg/String hash byte-for-byte). The reference order
        # is the sorted closure above.
        # TODO: verify against real ROS2-published RIHS01 once a sourced env is
        # available — regression-anchor only.
        @test h == "RIHS01_10f3786d7d40fd2b54367835614bff85d4ad3b5dab62bf8bca0cc232d73b4cd8"
    end

    # Reference order must not leak into the hash beyond the documented sort:
    # the same closure declared in a different order, then sorted, hashes equal;
    # an unsorted (mis-ordered) closure hashes differently.
    @testset "sort makes declaration order irrelevant; mis-sort diverges" begin
        ps = _first_struct(parse_msg(POSESTAMPED_BODY; name="PoseStamped"))
        forward = sort!(TypeDescription[
                _ref("builtin_interfaces/msg/Time", TIME_BODY),
                _ref("geometry_msgs/msg/Point", POINT_BODY),
                _ref("geometry_msgs/msg/Pose", POSE_BODY),
                _ref("geometry_msgs/msg/Quaternion", QUAT_BODY),
                _ref("std_msgs/msg/Header", HEADER_BODY),
            ]; by = t -> t.type_name)
        reversed = sort!(TypeDescription[
                _ref("std_msgs/msg/Header", HEADER_BODY),
                _ref("geometry_msgs/msg/Quaternion", QUAT_BODY),
                _ref("geometry_msgs/msg/Pose", POSE_BODY),
                _ref("geometry_msgs/msg/Point", POINT_BODY),
                _ref("builtin_interfaces/msg/Time", TIME_BODY),
            ]; by = t -> t.type_name)
        @test rihs01_hash(ps, "geometry_msgs/msg/PoseStamped";
                          package="geometry_msgs", references=forward) ==
              rihs01_hash(ps, "geometry_msgs/msg/PoseStamped";
                          package="geometry_msgs", references=reversed)

        # A deliberately mis-sorted (reverse-`type_name`) closure must hash
        # differently — proving the order is load-bearing and the sort matters.
        missorted = sort!(copy(forward); by = t -> t.type_name, rev = true)
        @test rihs01_hash(ps, "geometry_msgs/msg/PoseStamped";
                          package="geometry_msgs", references=missorted) !=
              rihs01_hash(ps, "geometry_msgs/msg/PoseStamped";
                          package="geometry_msgs", references=forward)
    end

    # ---- char / byte primitive collapse ------------------------------------
    #
    # ROS2's `char` is an unsigned 8-bit int (TYPE_ID_UINT8 = 3); `byte` is an
    # opaque octet (TYPE_ID_BYTE = 16). Both collapse on the way through the IL,
    # so a `char`/`byte` field is indistinguishable from the equivalent
    # `uint8`/`byte` field in the hash.
    @testset "char → uint8 collapse (std_msgs/msg/Char)" begin
        td = type_description_from_struct(
            _first_struct(parse_msg("char data"; name="Char")), "std_msgs/msg/Char")
        @test td.fields[1].field_type.type_id == TYPE_ID_UINT8
        h = rihs01_hash(_first_struct(parse_msg("char data"; name="Char")), "std_msgs/msg/Char")
        # Regression anchor (hand-computed spec JSON). TODO: verify against real
        # ROS2-published RIHS01 once a sourced env is available.
        @test h == "RIHS01_3ad2d04dd29ba19d04b16659afa3ccaedd691914b02a64e82e252f2fa6a586a9"
        # `char data` and `uint8 data` collapse to the same shape → same hash.
        @test h == rihs01_hash(_first_struct(parse_msg("uint8 data"; name="Char")), "std_msgs/msg/Char")
    end

    @testset "byte → octet collapse (std_msgs/msg/Byte)" begin
        td = type_description_from_struct(
            _first_struct(parse_msg("byte data"; name="Byte")), "std_msgs/msg/Byte")
        @test td.fields[1].field_type.type_id == TYPE_ID_BYTE
        h = rihs01_hash(_first_struct(parse_msg("byte data"; name="Byte")), "std_msgs/msg/Byte")
        # Regression anchor (hand-computed spec JSON). TODO: verify against real
        # ROS2-published RIHS01 once a sourced env is available.
        @test h == "RIHS01_41e1a3345f73fe93ede006da826a6ee274af23dd4653976ff249b0f44e3e798f"
        # `byte` is OCTET, distinct from `uint8` — the two must NOT collide.
        @test h != rihs01_hash(_first_struct(parse_msg("uint8 data"; name="Byte")), "std_msgs/msg/Byte")
    end

    # ---- service_msgs/msg/ServiceEventInfo — char[16] array + one ref ------
    #
    # Exercises char-in-an-array (uint8 + ARRAY_OFFSET, capacity 16) alongside a
    # nested ref, with constants hoisted out of the field set.
    @testset "ServiceEventInfo — char array + nested ref" begin
        td = type_description_from_struct(
            _first_struct(parse_msg(SEI_BODY; name="ServiceEventInfo")),
            "service_msgs/msg/ServiceEventInfo")
        # Constants are hoisted out; only the four real fields remain.
        @test [f.name for f in td.fields] ==
            ["event_type", "stamp", "client_gid", "sequence_number"]
        # client_gid is `char[16]` → uint8 + static-array offset, capacity 16.
        client_gid = td.fields[3].field_type
        @test client_gid.type_id == TYPE_ID_UINT8 + ARRAY_OFFSET
        @test client_gid.capacity == 16
        # stamp is a nested ref to builtin_interfaces/msg/Time.
        @test td.fields[2].field_type.type_id == TYPE_ID_NESTED_TYPE
        @test td.fields[2].field_type.nested_type_name == "builtin_interfaces/msg/Time"

        h = rihs01_hash(_first_struct(parse_msg(SEI_BODY; name="ServiceEventInfo")),
            "service_msgs/msg/ServiceEventInfo"; package="service_msgs",
            references=[_ref("builtin_interfaces/msg/Time", TIME_BODY)])
        # Regression anchor (hand-computed spec JSON). TODO: verify against real
        # ROS2-published RIHS01 once a sourced env is available.
        @test h == "RIHS01_41bcbbe07a75c9b52bc96bfd5c24d7f0fc0a08c0cb7921b3373c5732345a6f45"
    end

    # ---- example_interfaces/srv/AddTwoInts — SERVICE-level RIHS01 ----------
    #
    # rmw_zenoh keys a service/client off the service type hash (not the request
    # message). rosidl synthesizes a `Base_Event` message and a service umbrella;
    # `service_rihs01` reproduces it. This is a *real* ROS2-published hash (from
    # rosidl-generated hiroz-msgs), so it pins the synthesis + closure ordering.
    @testset "AddTwoInts — service-level RIHS01" begin
        tdm(td, refs=TypeDescription[]) = TypeDescriptionMsg(td, refs)
        req  = tdm(_ref("example_interfaces/srv/AddTwoInts_Request", "int64 a\nint64 b"))
        resp = tdm(_ref("example_interfaces/srv/AddTwoInts_Response", "int64 sum"))
        sei  = tdm(_ref("service_msgs/msg/ServiceEventInfo", SEI_BODY),
                   TypeDescription[_ref("builtin_interfaces/msg/Time", TIME_BODY)])
        h = service_rihs01("example_interfaces/srv/AddTwoInts", req, resp, sei)
        @test h == "RIHS01_e118de6bf5eeb66a2491b5bda11202e7b68f198d6f67922cf30364858239c81a"
    end

    # ---- action-protocol service hashes (example_interfaces/action/Fibonacci) ---
    #
    # Real ROS2-published values (from rosidl-generated hiroz-msgs). Pins the
    # hardcoded action service synthesis (action path, fixed request/response
    # shapes, reference-impl-incomplete closures). CancelGoal is a per-distro
    # constant shared by every action.
    @testset "action services — Fibonacci SendGoal/GetResult + CancelGoal" begin
        order(id) = FieldTypeDescription(UInt8(id))
        # Fibonacci_Goal { int32 order }; Fibonacci_Result { int32[] sequence }.
        goal_td = TypeDescription("example_interfaces/action/Fibonacci_Goal",
            [FieldDescription("order", FieldTypeDescription(TYPE_ID_INT32))])
        result_td = TypeDescription("example_interfaces/action/Fibonacci_Result",
            [FieldDescription("sequence",
                FieldTypeDescription(TYPE_ID_INT32 + UNBOUNDED_SEQUENCE_OFFSET, UInt64(0), UInt64(0), ""))])

        @test send_goal_service_rihs01("example_interfaces", "Fibonacci", goal_td) ==
            "RIHS01_d1a57fb2a4afe8c21e34fb10db206f16ce6729b28531141472df92277c55b557"
        @test get_result_service_rihs01("example_interfaces", "Fibonacci", result_td) ==
            "RIHS01_1b0de0d5d29dc955d92f546706568428632771db13ec84c15ec1c1a59f424a57"
        @test cancel_goal_service_rihs01() ==
            "RIHS01_022cb8eb7bbae4dedde27098187e94f435faa23942695d7fbc7335182d10baf1"
    end

end

end # module
