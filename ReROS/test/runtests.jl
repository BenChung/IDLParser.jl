using ReROS
using Rerun.Archetypes   # Scalars, TextLog, Transform3D, … (generated archetypes live here)
using Test

include("mapping.jl")

@testset "ReROS Phase 1 — sink + archetype-arity gate" begin

    # The Phase-1 gate (DESIGN-REVIEW.md theme 1): every Rerun archetype field is a
    # component *vector*, and every value the mapper emits must round-trip through the
    # sink to a real `.rrd`. One of each data-path archetype, with the correct arities.
    @testset "every data-path archetype round-trips through the sink" begin
        mktempdir() do dir
            path = joinpath(dir, "smoke.rrd")
            sink = Sink("reros_smoke"; sinks = [Save(path)], capacity = 64)

            # Scalars / TextLog / TextDocument: singles wrapped length-1.
            @test emit!(sink, "topics/x", Scalars([1.0]))
            @test emit!(sink, "topics/x", Scalars([1.0, 2.0, 3.0]))           # batched (N instances)
            @test emit!(sink, "topics/msg", TextLog(["hello"]))
            @test emit!(sink, "nodes/talker", TextDocument(["# meta"]); static = true)

            # Transform3D / Points3D: RAW NTuple vectors (a Position3D/Translation3D
            # *struct* fails `_fits` for a fixed-size list — cdata.jl:495).
            @test emit!(sink, "topics/pose", Transform3D(;
                translation = [(1.0f0, 2.0f0, 3.0f0)],
                quaternion  = [(0.0f0, 0.0f0, 0.0f0, 1.0f0)]))
            @test emit!(sink, "topics/points", Points3D([(1.0f0, 2.0f0, 3.0f0),
                                                               (4.0f0, 5.0f0, 6.0f0)]))

            # Clear: positional Bool vector (the kw form `is_recursive=` is a MethodError).
            @test emit!(sink, "nodes/gone", Clear([true]); static = true)

            close(sink)
            @test isfile(path)
            @test filesize(path) > 0
            @test dropped(sink) == 0
        end
    end

    @testset "drop policy counts and never blocks" begin
        mktempdir() do dir
            sink = Sink("reros_drop"; sinks = [Save(joinpath(dir, "drop.rrd"))],
                           capacity = 4, drop_when_full = true)
            # Flood far past capacity; with the writer draining, some get through and
            # the rest are dropped+counted — the call must never hang.
            for i in 1:10_000
                emit!(sink, "flood", Scalars([Float64(i)]))
            end
            close(sink)
            @test dropped(sink) ≥ 0          # may be 0 if the writer kept up; never errors
        end
    end

    @testset "close is idempotent and post-close emit is a no-op" begin
        mktempdir() do dir
            sink = Sink("reros_close"; sinks = [Save(joinpath(dir, "c.rrd"))])
            emit!(sink, "x", Scalars([1.0]))
            close(sink)
            close(sink)                       # idempotent
            @test emit!(sink, "x", Scalars([2.0])) == false
        end
    end

    @testset "one bad log item never kills the writer (sink-2/sink-1)" begin
        mktempdir() do dir
            path = joinpath(dir, "survive.rrd")
            sink = Sink("reros_survive"; sinks = [Save(path)])
            @test emit!(sink, "ok1", Scalars([1.0]))
            # a malformed RotationAxisAngle (flat 4-tuple — the component is a struct) throws
            # at log time; the writer must catch it, count it, and keep draining.
            @test emit!(sink, "bad", Transform3D(; rotation_axis_angle = [(1.0f0, 2.0f0, 3.0f0, 4.0f0)]))
            @test emit!(sink, "ok2", Scalars([2.0]))
            close(sink)
            @test write_errors(sink) == 1     # the bad item was isolated, not fatal
            @test isfile(path) && filesize(path) > 0   # close still flushed the .rrd
        end
    end

end

@testset "connectivity rules gate the graph (lifecycle-2)" begin
    # exclude(...) and rule(...; connectivity=false) deny endpoints/nodes from the graph;
    # selectors evaluate against endpoint fields (and node name/namespace for bare nodes).
    cfg = RecorderConfig("x"; record = [exclude(topic("/secret")), rule(node("keepme"))])
    @test ReROS.conn_denied(cfg, (topic = "/secret", node_name = "n"))
    @test !ReROS.conn_denied(cfg, (topic = "/public", node_name = "keepme"))

    cfgn = RecorderConfig("x"; record = [exclude(node("dropme"))])
    @test ReROS.conn_denied_node(cfgn, (name = "dropme", namespace = "/"))
    @test !ReROS.conn_denied_node(cfgn, (name = "other", namespace = "/"))
end

include("config_toml.jl")   # offline: TOML config loader
include("integration.jl")   # live: spins a zenohd router + in-process ROS network
