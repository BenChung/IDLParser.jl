using ReROS
using Rerun.Archetypes
using Test

# Fixture message types, nested as package.msg.Type so `ros_type_name` yields the
# canonical "package/msg/Type" (it walks two module levels).
module Fix
    module geometry_msgs; module msg
        struct Point;      x::Float64; y::Float64; z::Float64; end
        struct Quaternion; x::Float64; y::Float64; z::Float64; w::Float64; end
        struct Pose;       position::Point; orientation::Quaternion; end
        struct Vector3;    x::Float64; y::Float64; z::Float64; end
        struct Twist;      linear::Vector3; angular::Vector3; end
    end; end
    module my_pkg; module msg
        struct Inner; a::Int32; b::Float64; end
        struct Wide
            flag::Bool
            name::String
            letter::Char
            vals::Vector{Float64}
            bytes::Vector{UInt8}
            inner::Inner
            pts::Vector{Inner}
        end
    end; end
    module vendor_msgs; module msg
        struct Pt3;     x::Float64; y::Float64; z::Float64; end
        struct Orient3; x::Float64; y::Float64; z::Float64; end   # no `w`
        struct RobotPose; position::Pt3; orientation::Orient3; end
    end; end
    module arr_pkg; module msg
        struct Scan; ranges::Vector{Float64}; label::String; end  # for conform array tests (no Char — see below)
    end; end
end
import .Fix.geometry_msgs.msg as G
import .Fix.my_pkg.msg as M
import .Fix.vendor_msgs.msg as V
import .Fix.arr_pkg.msg as AR

_ctx(sink, topic; manifest = ReROS.MapperManifest(), opts = ReROS.MapOpts()) =
    ReROS.MapContext(topic, "/topics/" * lstrip(topic, '/'),
                     ReROS.TimeStamps(ReROS._now_ns(), nothing, ReROS.next_seq!(sink));
                     manifest = manifest, opts = opts)

# Map a few values through a temp-file sink; succeeds iff nothing errors and the .rrd is real.
function _roundtrip(f; manifest = ReROS.MapperManifest())
    mktempdir() do dir
        path = joinpath(dir, "m.rrd")
        sink = Sink("reros_map"; sinks = [Save(path)])
        f(sink, manifest)
        close(sink)
        return isfile(path) && filesize(path) > 0
    end
end

@testset "ReROS mapping subsystem" begin

    @testset "ros_type_name" begin
        @test ros_type_name(G.Pose) == "geometry_msgs/msg/Pose"
        @test ros_type_name(M.Wide) == "my_pkg/msg/Wide"
    end

    @testset "glob matching & specificity" begin
        @test ReROS.glob_match(ReROS.parse_glob("std_msgs/msg/Float*"), "std_msgs/msg/Float64")
        @test ReROS.glob_match(ReROS.parse_glob("*/msg/Pose"), "geometry_msgs/msg/Pose")
        @test ReROS.glob_match(ReROS.parse_glob("/sensors/**"), "/sensors/a/b/c")
        @test !ReROS.glob_match(ReROS.parse_glob("std_msgs/msg/Int*"), "std_msgs/msg/Float64")
        # exact (no wildcard) outranks a glob
        @test ReROS._specificity(NameMatch("geometry_msgs/msg/Pose")) >
              ReROS._specificity(NameMatch("*/msg/Pose"))
    end

    @testset "fits (reflection) & requirement derivation" begin
        @test ReROS.fits(shape(x = :float, y = :float, z = :float), G.Point)
        @test ReROS.fits(shape(x = :real, y = :real, z = :real), G.Point)
        @test !ReROS.fits(shape(x = :float, y = :float, z = :float, w = :float), G.Point)  # no w
        @test ReROS.fits(shape(position = shape(x = :float, y = :float, z = :float),
                               orientation = shape(x = :float, y = :float, z = :float, w = :float)), G.Pose)
        @test ReROS.fits(shape(name = :string, flag = :bool, letter = :char, vals = :(float[])), M.Wide)

        # a Transform3D spec's auto-derived requirement fits Pose but not RobotPose (no w)
        spec = archetype(:Transform3D;
            translation = ("position.x", "position.y", "position.z"),
            quaternion  = ("orientation.x", "orientation.y", "orientation.z", "orientation.w"))
        req = ReROS.requirement(ReROS.BSpec(spec))
        @test req isa FieldShape
        @test ReROS.fits(req, G.Pose)
        @test !ReROS.fits(req, V.RobotPose)
    end

    @testset "manifest selection precedence" begin
        m = merge(BUILTIN_MAPPERS,
                  ReROS.MapperManifest(MapperEntry(TopicMatch("/special/pose"), ReROS.map_pose!; desc = "topic")))
        # exact NameMatch for Pose
        e = ReROS.select_entry(m, "geometry_msgs/msg/Pose", "/x", G.Pose)
        @test e !== nothing && ReROS._matcher_label(e.matcher) == "geometry_msgs/msg/Pose"
        # topic selection wins for the special topic even with a non-matching type name
        et = ReROS.select_entry(m, "unknown/msg/Thing", "/special/pose", G.Pose)
        @test et !== nothing && et.desc == "topic"
        # nothing matches an unknown type on an ordinary topic
        @test ReROS.select_entry(BUILTIN_MAPPERS, "my_pkg/msg/Wide", "/x", M.Wide) === nothing
    end

    @testset "structural fallback round-trips an arbitrary message" begin
        @test _roundtrip() do sink, _
            w = M.Wide(true, "hi", 'q', [1.0, 2.0, 3.0], UInt8[0x1, 0x2],
                       M.Inner(Int32(7), 2.5), [M.Inner(Int32(1), 1.0), M.Inner(Int32(2), 2.0)])
            map_into!(sink, w, _ctx(sink, "/things"))
        end
    end

    @testset "built-in geometry mappers round-trip" begin
        @test _roundtrip() do sink, _
            ctx = _ctx(sink, "/robot/pose"; manifest = BUILTIN_MAPPERS)
            map_into!(sink, G.Pose(G.Point(1.0, 2.0, 3.0), G.Quaternion(0.0, 0.0, 0.0, 1.0)), ctx)
            map_into!(sink, G.Twist(G.Vector3(0.1, 0.0, 0.0), G.Vector3(0.0, 0.0, 0.5)),
                      _ctx(sink, "/cmd_vel"; manifest = BUILTIN_MAPPERS))
            map_into!(sink, G.Point(4.0, 5.0, 6.0), _ctx(sink, "/pt"; manifest = BUILTIN_MAPPERS))
        end
    end

    @testset "declarative spec via TopicMatch round-trips" begin
        spec = archetype(:Transform3D;
            translation = ("position.x", "position.y", "position.z"),
            quaternion  = ("orientation.x", "orientation.y", "orientation.z", "orientation.w"))
        man = ReROS.MapperManifest(MapperEntry(TopicMatch("/robot/pose"), spec; desc = "pose-like → Transform3D"))
        @test _roundtrip() do sink, _
            map_into!(sink, G.Pose(G.Point(1.0, 2.0, 3.0), G.Quaternion(0.0, 0.0, 0.0, 1.0)),
                      _ctx(sink, "/robot/pose"; manifest = man))
        end
    end

    @testset "spec that doesn't fit falls back (no throw)" begin
        spec = archetype(:Transform3D;
            translation = ("position.x", "position.y", "position.z"),
            quaternion  = ("orientation.x", "orientation.y", "orientation.z", "orientation.w"))
        man = ReROS.MapperManifest(MapperEntry(TopicMatch("/robot/pose"), spec))
        @test _roundtrip() do sink, _
            # RobotPose lacks orientation.w → bind fails → structural fallback, still logs.
            map_into!(sink, V.RobotPose(V.Pt3(1.0, 2.0, 3.0), V.Orient3(0.0, 0.0, 0.0)),
                      _ctx(sink, "/robot/pose"; manifest = man))
        end
    end

    @testset "manifest show lists what's available" begin
        s = sprint(show, MIME"text/plain"(), BUILTIN_MAPPERS)
        @test occursin("geometry_msgs/msg/Pose", s)
        @test occursin("Transform3D", s)
    end

    @testset "conform (precise IL-vs-RMessage, §12.4)" begin
        poseshape = ReROS.requirement(ReROS.BSpec(archetype(:Transform3D;
            translation = ("position.x", "position.y", "position.z"),
            quaternion  = ("orientation.x", "orientation.y", "orientation.z", "orientation.w"))))
        @test ReROS.conform(poseshape, G.Pose).ok
        r = ReROS.conform(poseshape, V.RobotPose)              # RobotPose.orientation has no `w`
        @test !r.ok
        @test any(m -> occursin("orientation.w", m.path), r.mismatches)

        @test ReROS.conform(shape(x = :float, y = :float, z = :float), G.Point).ok
        @test !ReROS.conform(shape(x = :string), G.Point).ok   # x is float, not string
        @test ReROS.conform(shape(ranges = :(float[])), AR.Scan).ok          # ranges::Vector{Float64}
        @test ReROS.conform(shape(label = :string, ranges = :(float[])), AR.Scan).ok
        @test !ReROS.conform(shape(label = :(float[])), AR.Scan).ok          # label is a scalar string, not an array

        # A ROS `char` field reflects to RChar (ROSMessages reflect.jl fix), so conform now
        # validates char-bearing types instead of erroring on them.
        @test ReROS.conform(shape(letter = :char), M.Wide).ok            # M.Wide.letter::Char
        @test !ReROS.conform(shape(letter = :string), M.Wide).ok         # letter is char, not string
        # bound_mapper's try/catch still guards the remaining unreflectable fields (Dict/abstract).
    end

    @testset "Transform3D rotation_axis_angle round-trips (axisangle-1)" begin
        # RotationAxisAngle is an Arrow struct {axis: f32[3], angle: f32}; the old codegen
        # emitted a flat 4-tuple, which crashes the writer at log time. Treat orientation
        # (x,y,z,w) as (axis, angle) just to exercise the component shape end-to-end.
        spec = archetype(:Transform3D;
            translation = ("position.x", "position.y", "position.z"),
            rotation_axis_angle = ("orientation.x", "orientation.y", "orientation.z", "orientation.w"))
        man = ReROS.MapperManifest(MapperEntry(TopicMatch("/aa"), spec))
        mktempdir() do dir
            path = joinpath(dir, "aa.rrd")
            sink = Sink("reros_aa"; sinks = [Save(path)])
            map_into!(sink, G.Pose(G.Point(1.0, 2.0, 3.0), G.Quaternion(0.1, 0.2, 0.3, 1.5)),
                      _ctx(sink, "/aa"; manifest = man))
            close(sink)
            @test write_errors(sink) == 0            # the flat-4-tuple shape used to crash here
            @test isfile(path) && filesize(path) > 0
        end
    end

    @testset "TopicMatch outranks NameMatch and binds top-level only (sel-2/sel-3)" begin
        @test ReROS._specificity(TopicMatch("/odom")) >
              ReROS._specificity(NameMatch("geometry_msgs/msg/Pose"))
        man = ReROS.MapperManifest(MapperEntry(TopicMatch("/robot/pose"), ReROS.map_pose!))
        @test ReROS.select_entry(man, "geometry_msgs/msg/Pose", "/robot/pose", G.Pose; toplevel = true) !== nothing
        # a nested struct (depth > 0) on the same topic must NOT re-fire the TopicMatch
        @test ReROS.select_entry(man, "geometry_msgs/msg/Point", "/robot/pose", G.Point; toplevel = false) === nothing
    end

    @testset "binding iterates to a fitting lower-specificity candidate (bind-1)" begin
        # exact NameMatch for Point, but its spec requires a `w` Point lacks → must fall
        # through to the fitting StructMatch rather than dropping straight to map_struct!.
        badspec = archetype(:Transform3D; translation = ("x", "y", "z"),
                                          quaternion = ("x", "y", "z", "w"))
        structfn = (sink, x, ctx) -> nothing
        man = ReROS.MapperManifest(
            MapperEntry(NameMatch("geometry_msgs/msg/Point"), badspec),
            MapperEntry(StructMatch(shape(x = :float, y = :float, z = :float)), structfn; desc = "struct"))
        ctx = ReROS.MapContext("/x", "/topics/x", ReROS.TimeStamps(0, nothing, 0); manifest = man)
        @test ReROS.bound_mapper(ctx, G.Point) === structfn
    end

    @testset "per-topic mapper override (sel-1)" begin
        times = ReROS.TimeStamps(0, nothing, 0)
        # :structural forces map_struct! even though a builtin Pose mapper would match
        ctx_s = ReROS.MapContext("/p", "/topics/p", times; manifest = BUILTIN_MAPPERS, mapper = :structural)
        @test ReROS.bound_mapper(ctx_s, G.Pose) === ReROS.map_struct!
        # a Function override is used directly
        f = (sink, x, c) -> nothing
        ctx_f = ReROS.MapContext("/p", "/topics/p", times; manifest = BUILTIN_MAPPERS, mapper = f)
        @test ReROS.bound_mapper(ctx_f, G.Pose) === f
        # the override applies at the top level only — a child drops it
        @test ReROS.child(ctx_s, :position).mapper === :auto
    end

    @testset "array-of-strings expands instead of dropping (leaf-1)" begin
        @test _roundtrip() do sink, _
            map_into!(sink, ["alpha", "beta", "gamma"], _ctx(sink, "/labels"))
        end
    end

end
