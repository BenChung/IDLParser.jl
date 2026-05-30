module Gencode
using IDLParser
using CDRSerialization: CDRReader, CDRWriter, iscompact
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

# Regression: gen.jl reassigned `jltype` inside the typedef loop, so a typedef
# with multiple declarators corrupted the base type after the first one.
module TypedefMulti
    import StaticArrays, CDRSerialization
end
@testset "typedef with multiple declarators" begin
    import IDLParser.Parse: specification
    parsed = parse_whole(specification, "typedef float a[3], b[5];")
    resolved = resolve_constants(convert(Vector{IDLParser.Parse.Decl}, parsed))
    for c in generate_code(resolved)
        TypedefMulti.eval(c)
    end
    @test TypedefMulti.a == SVector{3, Float32}
    @test TypedefMulti.b == SVector{5, Float32}
end

module LayoutGood end
module LayoutPadded end
module LayoutOverride end
@testset "layout requirements (@compact annotation + require option)" begin
    import IDLParser.Parse: specification
    p(src) = resolve_constants(convert(Vector{IDLParser.Parse.Decl}, parse_whole(specification, src)))

    # `@compact` on a genuinely-compact struct builds; the layout holds.
    for c in generate_code(p("module m { @compact struct Good { double a; double b; }; };"))
        LayoutGood.eval(c)
    end
    @test iscompact(LayoutGood.m.Good)

    # `require => :compact` on a padded struct (sizeof 16 ≠ wire 12) fails when the
    # generated code loads — the precompile-time `@assert iscompact` fires.
    padded = generate_code(p("module m { struct Padded { double a; unsigned long b; }; };");
                           require = Dict("m/Padded" => :compact))
    @test_throws Exception begin
        for c in padded
            LayoutPadded.eval(c)
        end
    end

    # `:compact` on a variable-length struct is impossible → errors during generation.
    @test_throws ErrorException generate_code(
        p("module m { struct Dyn { string name; double v; }; };");
        require = Dict("m/Dyn" => :compact))

    # The `require` option overrides the IDL annotation (:any lets a padded struct build).
    for c in generate_code(p("module m { @compact struct P2 { double a; unsigned long b; }; };");
                           require = Dict("m/P2" => :any))
        LayoutOverride.eval(c)
    end
    @test isdefined(LayoutOverride.m, :P2)
end
end