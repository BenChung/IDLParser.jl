using ReROS
using Rerun.Archetypes
using Test

# A loaded module the TOML can reference by name (fn= / mapper_use=).
module TM
    using ReROS
    using Rerun.Archetypes
    telem(sink, msg, ctx) = ReROS.emit!(sink, ctx, Scalars([Float64(msg.battery)]))
    const MAPPERS = ReROS.MapperManifest(
        ReROS.MapperEntry(ReROS.NameMatch("vendor/msg/Foo"), (s, m, c) -> nothing; desc = "vendor-foo"))
end

module CfgFix; module geom; module msg
    struct Point; x::Float64; y::Float64; z::Float64; end
    struct Quat;  x::Float64; y::Float64; z::Float64; w::Float64; end
    struct Pose;  position::Point; orientation::Quat; end
end; end; end
import .CfgFix.geom.msg as CG

const _CFG_TOML = """
app_id = "cfgtest"
sinks = [{ save = "x.rrd" }]
mapper_use = ["TM.MAPPERS"]

[[record]]
topic = "/turtle1/**"
data = true

[[exclude]]
namespace = "/_"

[[mapper]]
type = "robot/msg/Telemetry"
fn = "TM.telem"

[[mapper]]
topic = "/robot/pose"
[mapper.emit]
archetype = "Transform3D"
translation = ["position.x", "position.y", "position.z"]
quaternion = ["orientation.x", "orientation.y", "orientation.z", "orientation.w"]

[[mapper]]
[mapper.match]
x = "float"
y = "float"
z = "float"
[mapper.emit]
archetype = "Points3D"
positions = [["x", "y", "z"]]
"""

_pub(t; ns = "/", nm = "n") = (topic = t, type = nothing, namespace = ns, node_name = nm,
                               kind = ReROS.ROSReach.Publisher)

@testset "TOML config loader" begin
    cfg = load_config_string(_CFG_TOML)

    @test cfg.app_id == "cfgtest"
    @test length(cfg.sinks) == 1 && cfg.sinks[1] isa Save

    @testset "record/exclude rules → wants_data" begin
        @test ReROS.wants_data(cfg, _pub("/turtle1/cmd_vel"; ns = "/turtle1"))
        @test !ReROS.wants_data(cfg, _pub("/other"))
    end

    @testset "mappers: use + fn + emit + match, over builtins" begin
        m = cfg.mappers
        @test ReROS.select_entry(m, "robot/msg/Telemetry", "/x", CG.Pose) !== nothing   # fn entry
        @test ReROS.select_entry(m, "unknown/msg/T", "/robot/pose", CG.Pose) !== nothing # topic entry
        @test ReROS.select_entry(m, "vendor/msg/Foo", "/x", CG.Pose) !== nothing         # mapper_use
        @test ReROS.select_entry(m, "any/msg/Pt", "/x", CG.Point) !== nothing            # StructMatch x/y/z
        @test ReROS.select_entry(m, "geometry_msgs/msg/Pose", "/x", CG.Pose) !== nothing # builtin survives
    end

    @testset "declarative Transform3D emit (from TOML) round-trips" begin
        mktempdir() do dir
            rrd = joinpath(dir, "m.rrd")
            sink = Sink("cfgmap"; sinks = [Save(rrd)])
            e = ReROS.select_entry(cfg.mappers, "x/msg/y", "/robot/pose", CG.Pose)
            ctx = ReROS.MapContext("/robot/pose", "/topics/robot/pose",
                                   ReROS.TimeStamps(ReROS._now_ns(), nothing, ReROS.next_seq!(sink)))
            ReROS._entry_callable(e)(sink, CG.Pose(CG.Point(1.0, 2.0, 3.0), CG.Quat(0.0, 0.0, 0.0, 1.0)), ctx)
            close(sink)
            @test filesize(rrd) > 0
        end
    end

    @testset "allow_julia gates inline/included Julia" begin
        jl = """
        [[mapper]]
        type = "p/msg/q"
        [mapper.julia]
        body = "emit!(sink, ctx, Scalars([1.0]))"
        """
        @test_throws Exception load_config_string(jl)                       # gated off by default
        cfg2 = load_config_string(jl; allow_julia = true)                   # allowed → builds a fn mapper
        @test ReROS.select_entry(cfg2.mappers, "p/msg/q", "/x", CG.Point) !== nothing
    end
end
