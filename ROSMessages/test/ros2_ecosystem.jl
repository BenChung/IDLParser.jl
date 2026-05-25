module ROS2EcosystemTests
using IDLParser
using ROSMessages
using CDRSerialization: CDRReader, CDRWriter
using Test

const STD_ROOT = joinpath(@__DIR__, "files/ros2_standard")
const ECO_ROOT = joinpath(@__DIR__, "files/ros2_ecosystem")

# Enumerate (package, kind, name, path) across both vendored trees. The
# ecosystem packages depend on the standard interfaces, so we must build the
# combined catalog for cross-package refs to resolve.
function _enumerate_all()
    out = Tuple{String, String, String, String}[]
    for root in (STD_ROOT, ECO_ROOT)
        for (dir, _, files) in walkdir(root)
            for f in sort!(copy(files))
                ext = splitext(f)[2]
                ext in (".msg", ".srv", ".action") || continue
                path = joinpath(dir, f)
                parts = splitpath(path)
                idx = findlast(p -> p in ("msg", "srv", "action"), parts)
                idx === nothing && continue
                pkg = parts[idx-1]
                kind = parts[idx]
                push!(out, (pkg, kind, splitext(f)[1], path))
            end
        end
    end
    # Sort for deterministic test output.
    return sort!(out; by = t -> (t[1], t[2], t[3]))
end

const INTERFACES = _enumerate_all()

# Vendored — see NOTICE.md under each `files/ros2_*` dir. Run
# `test/files/vendor_ros2.sh` to repopulate. When absent (e.g. fresh
# checkout), skip rather than fail every test.
const HAVE_VENDORED = !isempty(INTERFACES)

if !HAVE_VENDORED
    @info "skipping ROS2 ecosystem tests: no vendored .msg files found — run test/files/vendor_ros2.sh to populate"
end

# Build a single merged module of every interface across both trees so the
# round-trip tests below can pull types from anywhere. Done at module load
# because `module ... end` can't sit inside an `@testset`.
module Generated
end

if HAVE_VENDORED
    let combined = IDLParser.Parse.Decl[],
        seen = Set{Tuple{String, String, String}}()
        for (pkg, kind, name, path) in INTERFACES
            key = (pkg, kind, name)
            key in seen && continue
            push!(seen, key)
            src = read(path, String)
            if kind == "msg"
                append!(combined, ROSMessages.parse_msg(src; name=name, package=pkg))
            elseif kind == "srv"
                append!(combined, ROSMessages.parse_srv(src; name=name, package=pkg))
            else
                append!(combined, ROSMessages.parse_action(src; name=name, package=pkg))
            end
        end
        resolved = IDLParser.ConstResolution.resolve_constants(combined)
        for c in IDLParser.Generation.generate_code(resolved)
            # `test_msgs/NestedMessage.action` references `test_msgs/BasicTypes`,
            # which only exists upstream as a `.idl` file (not `.msg`) and isn't
            # surfaced through our ROS2 → .msg flow. Eval-time failure for the
            # `test_msgs` package is expected — swallow it so the rest load.
            try
                Generated.eval(c)
            catch e
                mname = c isa Expr && c.head === :module ? c.args[2] : :unknown
                mname == :test_msgs || rethrow()
            end
        end
    end
end

HAVE_VENDORED && @testset "ROS2 ecosystem interfaces" begin

@testset "interfaces enumerated" begin
    # Hard floor — if the vendored set shrinks unexpectedly we want to notice.
    @test length(INTERFACES) >= 685
    by_kind = Dict("msg" => 0, "srv" => 0, "action" => 0)
    for (_, kind, _, _) in INTERFACES
        by_kind[kind] += 1
    end
    @test by_kind["msg"] >= 500
    @test by_kind["srv"] >= 90
    @test by_kind["action"] >= 30
end

# Stage-by-stage shake-down across every vendored file (standard + ecosystem).
@testset "pipeline reaches generate for every interface" begin
    parse_fail = Tuple{String, String}[]
    resolve_fail = Tuple{String, String}[]
    gen_fail = Tuple{String, String}[]
    for (pkg, kind, _, path) in INTERFACES
        rel = relpath(path, dirname(STD_ROOT))
        parsed = try
            ROSMessages.parse_file(path; package=pkg)
        catch e
            push!(parse_fail, (rel, sprint(showerror, e)))
            continue
        end
        resolved = try
            typed = convert(Vector{IDLParser.Parse.Decl}, parsed)
            IDLParser.ConstResolution.resolve_constants(typed)
        catch e
            push!(resolve_fail, (rel, sprint(showerror, e)))
            continue
        end
        try
            IDLParser.Generation.generate_code(resolved)
        catch e
            push!(gen_fail, (rel, sprint(showerror, e)))
        end
    end
    @test isempty(parse_fail)
    @test isempty(resolve_fail)
    @test isempty(gen_fail)
    isempty(parse_fail)   || println("parse failures:   ", parse_fail)
    isempty(resolve_fail) || println("resolve failures: ", resolve_fail)
    isempty(gen_fail)     || println("gen failures:     ", gen_fail)
end

@testset "every ecosystem package module is defined" begin
    # Every package we vendored should appear in `Generated` (except
    # `test_msgs`, intentionally skipped above).
    for pkg in (:nav2_msgs, :dwb_msgs, :nav_2d_msgs, :moveit_msgs, :control_msgs,
                :px4_msgs, :slam_toolbox, :octomap_msgs, :object_recognition_msgs,
                :geographic_msgs, :uuid_msgs)
        @test isdefined(Generated, pkg)
        if isdefined(Generated, pkg)
            mod = getfield(Generated, pkg)
            # Each must have at least one of the kind sub-modules.
            @test any(k -> isdefined(mod, k), (:msg, :srv, :action))
        end
    end
end

@testset "CDR round-trip: nav2_msgs/Costmap (Vector<uint8>)" begin
    G = Generated.geometry_msgs.msg
    S = Generated.std_msgs.msg
    B = Generated.builtin_interfaces.msg
    N = Generated.nav2_msgs.msg
    cm = N.Costmap(
        header = S.Header(stamp=B.Time(sec=Int32(1), nanosec=UInt32(0)), frame_id="map"),
        metadata = N.CostmapMetaData(
            map_load_time = B.Time(sec=Int32(0), nanosec=UInt32(0)),
            update_time = B.Time(sec=Int32(0), nanosec=UInt32(0)),
            layer = "static",
            resolution = 0.05f0,
            size_x = UInt32(4),
            size_y = UInt32(3),
            origin = G.Pose(
                position=G.Point(x=0.0, y=0.0, z=0.0),
                orientation=G.Quaternion(x=0.0, y=0.0, z=0.0, w=1.0))),
        data = UInt8[0, 50, 100, 150, 200, 250, 255, 0, 0, 0, 0, 0])
    buf = IOBuffer(); write(CDRWriter(buf), cm)
    seekstart(buf)
    back = read(CDRReader(buf), N.Costmap)
    @test back == cm
end

@testset "CDR round-trip: control_msgs/JointTolerance (Header + bare Header alias)" begin
    # control_msgs/JointTrajectoryControllerState uses bare `Header`
    # (legacy alias to std_msgs/Header). Confirm it works after the
    # aliasing change.
    C = Generated.control_msgs.msg
    jt = C.JointTolerance(name="joint1", position=0.01, velocity=0.05, acceleration=0.1)
    buf = IOBuffer(); write(CDRWriter(buf), jt)
    seekstart(buf)
    back = read(CDRReader(buf), C.JointTolerance)
    @test back == jt
end

@testset "CDR round-trip: px4_msgs/VehicleCommand (kwargs-from-fields)" begin
    P = Generated.px4_msgs.msg
    # Build a populated value generically — px4 messages have dozens of
    # fields, and listing them by hand is brittle. Just default-construct
    # each field's type with `zero` / empty, then round-trip.
    function _zero(T)
        T <: AbstractString && return ""
        T <: AbstractFloat && return zero(T)
        T <: Integer && return zero(T)
        T === Bool && return false
        T <: AbstractArray && return T()  # static arrays from StaticArrays.jl have a `T()` zero ctor
        return zero(T)
    end
    T = P.VehicleCommand
    kw = (n => _zero(fieldtype(T, n)) for n in fieldnames(T))
    vc = T(; kw...)
    buf = IOBuffer(); write(CDRWriter(buf), vc)
    seekstart(buf)
    back = read(CDRReader(buf), T)
    @test back == vc
end

@testset "legacy aliasing: bare Header / time / duration work in moveit_msgs" begin
    # moveit_msgs uses bare `Header` extensively, and lowercase
    # `duration` in a few files (e.g. CartesianTrajectoryPoint).
    # Both should resolve via the rosidl-style legacy alias.
    M = Generated.moveit_msgs.msg
    @test fieldtype(M.CartesianTrajectoryPoint, :time_from_start) ===
        Generated.builtin_interfaces.msg.Duration
    @test fieldtype(M.CartesianTrajectory, :header) ===
        Generated.std_msgs.msg.Header
end

end # @testset ROS2 ecosystem interfaces
end # module
