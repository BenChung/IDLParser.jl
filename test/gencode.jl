module Gencode
using IDLParser
using CDRSerialization: CDRReader, CDRWriter
using StaticArrays
using Test, PEG
import IDLParser.Parse: open_idl
import IDLParser.ConstResolution: resolve_constants
import IDLParser.Generation: generate_code

module Tester
end
@testset "vehicle odometry" begin
    code = generate_code(resolve_constants(convert(Vector{IDLParser.Parse.Decl}, open_idl(joinpath(@__DIR__, "files/VehicleOdometry.idl")))))[1]
    Tester.eval(code)

    data = Gencode.Tester.px4.msg.VehicleOdometry(
        timestamp = 1,
        timestamp_sample = 2,
        pose_frame = 1,
        position = SVector(1.0, 2.0, 3.0),
        q = SVector(0.0, 0.5, 0.75, 0.85),
        velocity_frame = 1,
        velocity = SVector(4.0, 5.0, 6.0),
        angular_velocity = SVector(7.0, 8.0, 9.0),
        position_variance = SVector(10.0, 11, 12),
        orientation_variance = SVector(13.0, 14, 15),
        velocity_variance = SVector(16.0, 17, 18),
        reset_counter = 16,
        quality = 17
    )
    buf = IOBuffer()
    w = CDRWriter(buf)
    write(w, data)
    seekstart(buf)
    r = CDRReader(buf)
    readback = read(r, Gencode.Tester.px4.msg.VehicleOdometry)
    @test readback == data
end
end