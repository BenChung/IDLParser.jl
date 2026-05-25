module ROS2StandardTests
using IDLParser
using ROSMessages
using CDRSerialization: CDRReader, CDRWriter
using Test

# Vendored ROS2 standard interfaces — see test/files/ros2_standard/NOTICE.md.
const ROOT = joinpath(@__DIR__, "files/ros2_standard")

# Enumerate every (package, kind, name, path) tuple under ROOT, where `kind`
# is one of "msg" / "srv" / "action" and the path layout is
# `<pkg>/<kind>/<Name>.<kind>`. Returned sorted by (pkg, kind, name) for
# stable test output.
function _enumerate_interfaces()
    out = Tuple{String, String, String, String}[]
    for pkg in sort!(readdir(ROOT))
        pkg_dir = joinpath(ROOT, pkg)
        isdir(pkg_dir) || continue
        for kind in ("msg", "srv", "action")
            kind_dir = joinpath(pkg_dir, kind)
            isdir(kind_dir) || continue
            for f in sort!(readdir(kind_dir))
                endswith(f, "." * kind) || continue
                push!(out, (pkg, kind, splitext(f)[1], joinpath(kind_dir, f)))
            end
        end
    end
    return out
end

const INTERFACES = _enumerate_interfaces()

# The interface files are vendored — see test/files/ros2_standard/NOTICE.md.
# A fresh checkout won't have them; `test/files/vendor_ros2.sh` populates the
# tree. When absent, skip this testset rather than failing every test.
const HAVE_VENDORED = !isempty(INTERFACES)

if !HAVE_VENDORED
    @info "skipping ROS2 standard-interface tests: no vendored .msg files found at $ROOT — run test/files/vendor_ros2.sh to populate"
end

# Build every .msg type into one merged Julia module at module-load time.
# `module` cannot appear inside an `@testset` (which expands to `begin ... end`),
# so the eval happens here at the outer module scope; the testsets below then
# inspect / round-trip the populated `Generated`.
module Generated
end

if HAVE_VENDORED
    let combined = IDLParser.Parse.Decl[],
        seen = Set{Tuple{String, String}}()
        for (pkg, kind, name, path) in INTERFACES
            kind == "msg" || continue
            (pkg, name) in seen && continue
            push!(seen, (pkg, name))
            append!(combined, ROSMessages.parse_msg(read(path, String);
                name=name, package=pkg))
        end
        resolved = IDLParser.ConstResolution.resolve_constants(combined)
        for c in IDLParser.Generation.generate_code(resolved)
            Generated.eval(c)
        end
    end
end

HAVE_VENDORED && @testset "ROS2 standard interfaces" begin

@testset "192 interfaces exist" begin
    by_kind = Dict("msg" => 0, "srv" => 0, "action" => 0)
    for (_, kind, _, _) in INTERFACES
        by_kind[kind] += 1
    end
    @test by_kind["msg"]    == 163
    @test by_kind["srv"]    == 28
    @test by_kind["action"] == 1
    @test length(INTERFACES) == 192
end

# Stage-by-stage shake-down: every vendored file must make it through
# parse → const-resolve → code-gen without errors. Each stage records
# offending file/error so a failure points at the exact spec corner.
@testset "pipeline reaches generate for every interface" begin
    parse_fail = Tuple{String, String}[]
    resolve_fail = Tuple{String, String}[]
    gen_fail = Tuple{String, String}[]
    for (pkg, kind, _, path) in INTERFACES
        rel = relpath(path, ROOT)
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

@testset "every package module is defined" begin
    expected_pkgs = (
        :builtin_interfaces, :std_msgs, :geometry_msgs, :sensor_msgs,
        :nav_msgs, :diagnostic_msgs, :shape_msgs, :stereo_msgs,
        :trajectory_msgs, :visualization_msgs, :unique_identifier_msgs,
        :action_msgs, :lifecycle_msgs, :rcl_interfaces, :rosgraph_msgs,
        :service_msgs, :statistics_msgs, :test_msgs,
        :type_description_interfaces,
    )
    for pkg in expected_pkgs
        @test isdefined(Generated, pkg)
        if isdefined(Generated, pkg)
            @test isdefined(getfield(Generated, pkg), :msg)
        end
    end
end

@testset "CDR round-trip: sensor_msgs/Imu" begin
    M = Generated.sensor_msgs.msg
    S = Generated.std_msgs.msg
    G = Generated.geometry_msgs.msg
    B = Generated.builtin_interfaces.msg
    imu = M.Imu(
        header = S.Header(stamp=B.Time(sec=Int32(1), nanosec=UInt32(2)),
                          frame_id="base_link"),
        orientation = G.Quaternion(x=0.0, y=0.0, z=0.0, w=1.0),
        orientation_covariance = ntuple(_ -> 0.0, 9),
        angular_velocity = G.Vector3(x=0.1, y=0.2, z=0.3),
        angular_velocity_covariance = ntuple(_ -> 0.0, 9),
        linear_acceleration = G.Vector3(x=0.0, y=0.0, z=9.81),
        linear_acceleration_covariance = ntuple(_ -> 0.0, 9))
    buf = IOBuffer()
    write(CDRWriter(buf), imu)
    seekstart(buf)
    back = read(CDRReader(buf), M.Imu)
    @test back == imu
end

@testset "CDR round-trip: nav_msgs/Path with sequence-of-stamped" begin
    G = Generated.geometry_msgs.msg
    S = Generated.std_msgs.msg
    B = Generated.builtin_interfaces.msg
    ps = G.PoseStamped(
        header = S.Header(stamp=B.Time(sec=Int32(0), nanosec=UInt32(0)),
                          frame_id="map"),
        pose = G.Pose(position = G.Point(x=1.0, y=2.0, z=3.0),
                      orientation = G.Quaternion(x=0.0, y=0.0, z=0.0, w=1.0)))
    path = Generated.nav_msgs.msg.Path(
        header = S.Header(stamp=B.Time(sec=Int32(0), nanosec=UInt32(0)),
                          frame_id="map"),
        poses = [ps, ps, ps])
    buf = IOBuffer()
    write(CDRWriter(buf), path)
    seekstart(buf)
    back = read(CDRReader(buf), Generated.nav_msgs.msg.Path)
    @test back == path
end

@testset "CDR round-trip: diagnostic_msgs/DiagnosticArray (Vector{KeyValue})" begin
    D = Generated.diagnostic_msgs.msg
    S = Generated.std_msgs.msg
    B = Generated.builtin_interfaces.msg
    da = D.DiagnosticArray(
        header = S.Header(stamp=B.Time(sec=Int32(0), nanosec=UInt32(0)),
                          frame_id=""),
        status = [D.DiagnosticStatus(
            level=UInt8(0), name="sensor", message="ok", hardware_id="hw01",
            values=[D.KeyValue(key="rate", value="100Hz"),
                    D.KeyValue(key="errors", value="0")])])
    buf = IOBuffer()
    write(CDRWriter(buf), da)
    seekstart(buf)
    back = read(CDRReader(buf), D.DiagnosticArray)
    @test back == da
end

@testset "constants exposed under <Type>_Constants" begin
    @test Generated.sensor_msgs.msg.NavSatStatus_Constants.STATUS_FIX == 0
    @test Generated.diagnostic_msgs.msg.DiagnosticStatus_Constants.ERROR == 2
    @test Generated.visualization_msgs.msg.Marker_Constants.SPHERE == 2
end

end # @testset ROS2 standard interfaces
end # module
