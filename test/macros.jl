module MacroTests
using IDLParser
using CDRSerialization: CDRReader, CDRWriter
using Test

const HAVE_VENDORED = isdir(joinpath(@__DIR__, "files/ros2_standard/std_msgs/msg"))

if !HAVE_VENDORED
    @info "skipping macro tests: no vendored ROS2 files — run test/files/vendor_ros2.sh"
end

# Single-file macro: pull in `std_msgs/String` and confirm the type plus
# its CDR codec are spliced into this module.
module SingleFile
    using IDLParser
    if isfile(joinpath(@__DIR__, "files/ros2_standard/std_msgs/msg/String.msg"))
        IDLParser.@ros_msg "files/ros2_standard/std_msgs/msg/String.msg"
    end
end

# Multi-directory macro: pull in the geometry_msgs cluster (which needs
# std_msgs and builtin_interfaces too) and round-trip a Pose.
module MultiDir
    using IDLParser
    if isdir(joinpath(@__DIR__, "files/ros2_standard/geometry_msgs"))
        IDLParser.@ros_msgs(
            "files/ros2_standard/builtin_interfaces",
            "files/ros2_standard/std_msgs",
            "files/ros2_standard/geometry_msgs",
        )
    end
end

HAVE_VENDORED && @testset "@ros_msg / @ros_msgs macros" begin

@testset "@ros_msg single file" begin
    T = SingleFile.std_msgs.msg.String
    s = T(data = "hello")
    buf = IOBuffer(); write(CDRWriter(buf), s); seekstart(buf)
    back = read(CDRReader(buf), T)
    @test back == s
    @test fieldnames(T) == (:data,)
    @test fieldtype(T, :data) === String
end

@testset "@ros_msgs directory traversal" begin
    # All three packages should have a `msg` sub-module
    for pkg in (:builtin_interfaces, :std_msgs, :geometry_msgs)
        @test isdefined(MultiDir, pkg)
        if isdefined(MultiDir, pkg)
            @test isdefined(getfield(MultiDir, pkg), :msg)
        end
    end

    # Round-trip a Pose (cross-pkg refs into geometry_msgs internals)
    G = MultiDir.geometry_msgs.msg
    p = G.Pose(
        position    = G.Point(x = 1.0, y = 2.0, z = 3.0),
        orientation = G.Quaternion(x = 0.0, y = 0.0, z = 0.0, w = 1.0))
    buf = IOBuffer(); write(CDRWriter(buf), p); seekstart(buf)
    back = read(CDRReader(buf), G.Pose)
    @test back == p

    # PoseStamped pulls in std_msgs/Header + builtin_interfaces/Time
    ps = G.PoseStamped(
        header = MultiDir.std_msgs.msg.Header(
            stamp = MultiDir.builtin_interfaces.msg.Time(sec = Int32(1), nanosec = UInt32(2)),
            frame_id = "map"),
        pose = p)
    buf = IOBuffer(); write(CDRWriter(buf), ps); seekstart(buf)
    back_ps = read(CDRReader(buf), G.PoseStamped)
    @test back_ps == ps
end

@testset "macros register include_dependency for precompile invalidation" begin
    # Verify the expansion includes `Base.include_dependency(<path>)` calls.
    ex = @macroexpand IDLParser.@ros_msg "files/ros2_standard/std_msgs/msg/String.msg"
    inner = ex.head === :escape ? ex.args[1] : ex
    @test inner.head === :toplevel
    # The expansion uses `Base.include_dependency(path)` which serializes as
    # `Expr(:call, Expr(:., :Base, QuoteNode(:include_dependency)), path)`.
    has_dep = any(inner.args) do a
        a isa Expr && a.head === :call && occursin("include_dependency", string(a.args[1]))
    end
    @test has_dep
end

end
end
