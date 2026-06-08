module ReflectTests
using IDLParser
using ROSMessages
using Test
using Moshi.Match: @match
import IDLParser.Parse: TypeDecl
import ROSMessages: parse_msg, rihs01_hash, type_description_from_struct, TypeDescription,
    TypeDescriptionMsg, service_rihs01, send_goal_service_rihs01, get_result_service_rihs01,
    lower, il_from_type, il_from_fields, il_service_from_fields, il_action_from_fields,
    il_field_diff, nt_pairs, struct_from_nt, nt_from_struct, _default_name_of

# reflect.jl is the only new ROSMessages translation (Julia type → IL); everything
# downstream is golden-tested. This proves the reflected IL hashes to the *same*
# published ROS2 RIHS01 as the text path — equal hash ⇒ a real ROS2 peer agrees.

# Plain Julia structs mirroring the standard interfaces (no ROS registry here, so
# nested names come from an injected `name_of`).
struct Point;       x::Float64; y::Float64; z::Float64;          end
struct Quaternion;  x::Float64; y::Float64; z::Float64; w::Float64; end
struct Pose;        position::Point; orientation::Quaternion;    end
struct TimeMsg;     sec::Int32; nanosec::UInt32;                 end
struct Header;      stamp::TimeMsg; frame_id::String;            end
struct PoseStamped; header::Header; pose::Pose;                  end
struct EmptyAuthored;                                            end   # a fieldless authored type

const _NAMES = IdDict{Any,String}(
    Point       => "geometry_msgs/msg/Point",
    Quaternion  => "geometry_msgs/msg/Quaternion",
    Pose        => "geometry_msgs/msg/Pose",
    TimeMsg     => "builtin_interfaces/msg/Time",
    Header      => "std_msgs/msg/Header",
    PoseStamped => "geometry_msgs/msg/PoseStamped",
)
NAMEOF(S) = _NAMES[S]

# Same StructDecl-finder rihs_golden.jl uses (constants would hoist a module first).
function _first_struct(decls)
    i = findfirst(decls) do d
        d isa TypeDecl.Type && @match d begin
            TypeDecl.StructDecl(_, _, _) => true
            _ => false
        end
    end
    return decls[i]
end

# Reflect a struct → IL → lower → the section's StructDecl AST.
_ast(S) = _first_struct(lower(il_from_type(S; name_of=NAMEOF); package=""))
# A nested TypeDescription, from reflection, for the closure.
_td(S, fqn) = type_description_from_struct(_ast(S), fqn; package=String(split(fqn, '/')[1]))

@testset "reflect.jl — RIHS01 golden oracle (reflected == published)" begin

    # ---- message: geometry_msgs/msg/PoseStamped (5-type transitive closure) ----
    @testset "PoseStamped golden hash, fully reflected" begin
        refs = sort!(TypeDescription[
                _td(Header,     "std_msgs/msg/Header"),
                _td(Pose,       "geometry_msgs/msg/Pose"),
                _td(Point,      "geometry_msgs/msg/Point"),
                _td(Quaternion, "geometry_msgs/msg/Quaternion"),
                _td(TimeMsg,    "builtin_interfaces/msg/Time"),
            ]; by = t -> t.type_name)
        h = rihs01_hash(_ast(PoseStamped), "geometry_msgs/msg/PoseStamped";
                        package="geometry_msgs", references=refs)
        # Same published value asserted in rihs_golden.jl from the text path.
        @test h == "RIHS01_10f3786d7d40fd2b54367835614bff85d4ad3b5dab62bf8bca0cc232d73b4cd8"
    end

    # ---- service: example_interfaces/srv/AddTwoInts (SERVICE-level, real hash) --
    @testset "AddTwoInts service-level RIHS01, reflected sections" begin
        req  = TypeDescriptionMsg(type_description_from_struct(
                   _first_struct(lower(il_from_fields("AddTwoInts_Request", [(:a, Int64), (:b, Int64)]); package="")),
                   "example_interfaces/srv/AddTwoInts_Request"), TypeDescription[])
        resp = TypeDescriptionMsg(type_description_from_struct(
                   _first_struct(lower(il_from_fields("AddTwoInts_Response", [(:sum, Int64)]); package="")),
                   "example_interfaces/srv/AddTwoInts_Response"), TypeDescription[])
        # ServiceEventInfo is the fixed vendored event message (text, as the real path supplies it).
        sei_body = """
            uint8 REQUEST_SENT = 0
            uint8 event_type
            builtin_interfaces/Time stamp
            char[16] client_gid
            int64 sequence_number
            """
        time_td = type_description_from_struct(_first_struct(parse_msg("int32 sec\nuint32 nanosec"; name="Time")),
                                               "builtin_interfaces/msg/Time"; package="builtin_interfaces")
        sei = TypeDescriptionMsg(
            type_description_from_struct(_first_struct(parse_msg(sei_body; name="ServiceEventInfo")),
                                         "service_msgs/msg/ServiceEventInfo"; package="service_msgs"),
            TypeDescription[time_td])
        h = service_rihs01("example_interfaces/srv/AddTwoInts", req, resp, sei)
        @test h == "RIHS01_e118de6bf5eeb66a2491b5bda11202e7b68f198d6f67922cf30364858239c81a"
    end

    # ---- action: example_interfaces/action/Fibonacci endpoint hashes -----------
    @testset "Fibonacci action endpoint hashes, reflected Goal/Result" begin
        goal_td = type_description_from_struct(
            _first_struct(lower(il_from_fields("Fibonacci_Goal", [(:order, Int32)]); package="")),
            "example_interfaces/action/Fibonacci_Goal")
        result_td = type_description_from_struct(
            _first_struct(lower(il_from_fields("Fibonacci_Result", [(:sequence, Vector{Int32})]); package="")),
            "example_interfaces/action/Fibonacci_Result")
        @test send_goal_service_rihs01("example_interfaces", "Fibonacci", goal_td) ==
            "RIHS01_d1a57fb2a4afe8c21e34fb10db206f16ce6729b28531141472df92277c55b557"
        @test get_result_service_rihs01("example_interfaces", "Fibonacci", result_td) ==
            "RIHS01_1b0de0d5d29dc955d92f546706568428632771db13ec84c15ec1c1a59f424a57"
    end

    # ---- reflection mapping edges ----------------------------------------------
    @testset "static array + sequence shapes" begin
        m = il_from_type(Header; name_of=NAMEOF)
        @test ROSMessages.IL.unparse(m) == "builtin_interfaces/Time stamp\nstring frame_id"
        # SVector → [N], Vector → [], UInt8 → uint8 (never byte).
        struct_local = il_from_fields("X", [(:fixed, ROSMessages.SVector{3,Float64}),
                                            (:seq, Vector{Int32}), (:flag, Bool), (:raw, UInt8)])
        @test ROSMessages.IL.unparse(struct_local) == "float64[3] fixed\nint32[] seq\nbool flag\nuint8 raw"
    end

    @testset "il_from_type rejects @NamedTuple; il_from_fields takes it via nt_pairs" begin
        @test_throws ErrorException il_from_type(@NamedTuple{a::Int64})
        @test nt_pairs(@NamedTuple{a::Int64, b::Float64}) == Tuple{Symbol,Type}[(:a, Int64), (:b, Float64)]
    end

    # An empty authored type (a fieldless struct via il_from_type, or an empty section
    # via il_from_fields — e.g. an empty `@NamedTuple{}` service response) must get the
    # rosidl synthetic member, so its RIHS01 matches the parse path + a real ROS2 peer
    # (not the degenerate zero-field hash).
    @testset "empty authored type gets the synthetic member (== parse path)" begin
        from_type   = il_from_type(EmptyAuthored; name_of=NAMEOF)
        from_fields = il_from_fields("EmptyAuthored", Tuple{Symbol,Type}[])
        @test ROSMessages.IL.unparse(from_type)   == "uint8 structure_needs_at_least_one_member"
        @test ROSMessages.IL.unparse(from_fields) == "uint8 structure_needs_at_least_one_member"
        h_type  = rihs01_hash(_first_struct(lower(from_type; package="")), "pkg/msg/EmptyAuthored")
        h_parse = rihs01_hash(parse_msg(""; name="EmptyAuthored")[1], "pkg/msg/EmptyAuthored")
        @test h_type == h_parse
    end

    @testset "_default_name_of throws off-shape (no silent bare-name fallback)" begin
        @test_throws ErrorException _default_name_of(Point)   # Point is 1-deep in this module
    end

    @testset "il_field_diff names the divergence" begin
        a = il_from_fields("X", [(:sum, Int64)])
        b = il_from_fields("X", [(:sum, Int32)])
        d = il_field_diff(a, b)
        @test length(d) == 1
        @test d[1].name == :sum && d[1].kind == :changed && d[1].a == "int64" && d[1].b == "int32"
        @test isempty(il_field_diff(a, a))   # identical ⇒ no diff ⇒ same RIHS
    end

    @testset "struct_from_nt / nt_from_struct round-trip (one level)" begin
        @test nt_from_struct(Point(1.0, 2.0, 3.0)) == (x=1.0, y=2.0, z=3.0)
        q = struct_from_nt(Point, (z=3.0, x=1.0, y=2.0))   # order-independent
        @test (q.x, q.y, q.z) == (1.0, 2.0, 3.0)
    end
end

end # module
