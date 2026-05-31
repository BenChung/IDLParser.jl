module MacroTests
using IDLParser
using ROSMessages
using CDRSerialization: CDRReader, CDRWriter
using Test

const HAVE_VENDORED = isdir(joinpath(@__DIR__, "files/ros2_standard/std_msgs/msg"))

if !HAVE_VENDORED
    @info "skipping macro tests: no vendored ROS2 files — run test/files/vendor_ros2.sh"
end

# Single-file macro: pull in `std_msgs/String` and confirm the type plus
# its CDR codec are spliced into this module.
module SingleFile
    using ROSMessages
    if isfile(joinpath(@__DIR__, "files/ros2_standard/std_msgs/msg/String.msg"))
        ROSMessages.@ros_msg "files/ros2_standard/std_msgs/msg/String.msg"
    end
end

# Multi-directory macro: pull in the geometry_msgs cluster (which needs
# std_msgs and builtin_interfaces too) and round-trip a Pose.
module MultiDir
    using ROSMessages
    if isdir(joinpath(@__DIR__, "files/ros2_standard/geometry_msgs"))
        ROSMessages.@ros_msgs(
            "files/ros2_standard/builtin_interfaces",
            "files/ros2_standard/std_msgs",
            "files/ros2_standard/geometry_msgs",
        )
    end
end

# Action codegen: a `.action` expands to its three sections plus the implicit
# SendGoal / GetResult / FeedbackMessage protocol types, which reference
# unique_identifier_msgs/UUID and builtin_interfaces/Time (supplied alongside).
module ActionProto
    using ROSMessages
    if isdir(joinpath(@__DIR__, "files/ros2_standard/unique_identifier_msgs"))
        ROSMessages.@ros_msgs(
            "files/ros2_standard/builtin_interfaces",
            "files/ros2_standard/unique_identifier_msgs",
            "files/action_protocol/example_msgs",
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

@testset "@ros_msgs .action expands to full protocol" begin
    A = ActionProto.example_msgs.action

    # The three user sections.
    @test fieldnames(A.Fibonacci_Goal) == (:order,)
    @test fieldnames(A.Fibonacci_Result) == (:sequence,)
    @test fieldnames(A.Fibonacci_Feedback) == (:partial_sequence,)

    # The implicit protocol types rosidl derives.
    @test fieldnames(A.Fibonacci_SendGoal_Request) == (:goal_id, :goal)
    @test fieldnames(A.Fibonacci_SendGoal_Response) == (:accepted, :stamp)
    @test fieldnames(A.Fibonacci_GetResult_Request) == (:goal_id,)
    @test fieldnames(A.Fibonacci_GetResult_Response) == (:status, :result)
    @test fieldnames(A.Fibonacci_FeedbackMessage) == (:goal_id, :feedback)

    # Field types: cross-package refs and section refs resolve.
    UUID = ActionProto.unique_identifier_msgs.msg.UUID
    Time = ActionProto.builtin_interfaces.msg.Time
    @test fieldtype(A.Fibonacci_SendGoal_Request, :goal_id) === UUID
    @test fieldtype(A.Fibonacci_SendGoal_Request, :goal) === A.Fibonacci_Goal
    @test fieldtype(A.Fibonacci_SendGoal_Response, :accepted) === Bool
    @test fieldtype(A.Fibonacci_SendGoal_Response, :stamp) === Time
    @test fieldtype(A.Fibonacci_GetResult_Response, :status) === Int8
    @test fieldtype(A.Fibonacci_GetResult_Response, :result) === A.Fibonacci_Result
    @test fieldtype(A.Fibonacci_FeedbackMessage, :feedback) === A.Fibonacci_Feedback

    # The generated protocol structs are CDR-codable end-to-end.
    req = A.Fibonacci_SendGoal_Request(
        goal_id = UUID(uuid = ntuple(i -> UInt8(i), 16)),
        goal = A.Fibonacci_Goal(order = Int32(7)))
    buf = IOBuffer(); write(CDRWriter(buf), req); seekstart(buf)
    @test read(CDRReader(buf), A.Fibonacci_SendGoal_Request) == req
end

@testset "macros register include_dependency for precompile invalidation" begin
    # Verify the expansion includes `Base.include_dependency(<path>)` calls.
    ex = @macroexpand ROSMessages.@ros_msg "files/ros2_standard/std_msgs/msg/String.msg"
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
