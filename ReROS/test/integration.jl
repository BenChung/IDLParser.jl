# Live integration: spin up a small in-process ROS 2 (rmw_zenoh) network with ROSNode
# and record its graph. Two Contexts (system + recorder) discover each other through a
# private zenohd router on a random port (the ROSNode test pattern: peer-to-peer is off).

using ReROS
using ROSNode
using ROSReach
using Test

# Zenohd_jll is a test-only dep (the router binary). It's available under `Pkg.test` / when
# on the load path; a bare `julia test/runtests.jl` without it skips the live suite cleanly.
const _ZENOHD = try
    @eval import Zenohd_jll
    Zenohd_jll
catch
    nothing
end

if _ZENOHD === nothing
    @info "ReROS: Zenohd_jll unavailable — skipping live integration tests (run via `Pkg.test`)"
else

_PORT = rand(20000:39999)
_EP   = "tcp/localhost:$_PORT"
_LOG  = joinpath(tempdir(), "zenohd-reros-$_PORT.log")
@info "ReROS integration router" _EP log = _LOG
_ROUTER = run(pipeline(`$(_ZENOHD.zenohd()) -l $_EP`, stdout = _LOG, stderr = _LOG), wait = false)
sleep(0.8)   # let the router bind

_lctx() = ROSNode.Context(; peers = [_EP], localhost_only = true, drain_timeout = 1.0)

# Poll until `pred()` or timeout; returns whether it held.
function _until(pred; tries = 60, pause = 0.2)
    for _ in 1:tries
        pred() && return true
        sleep(pause)
    end
    return pred()
end

try
    @testset "ReROS graph recorder — live in-process network" begin

        @testset "discovers a publisher and logs the graph" begin
            mktempdir() do dir
                rrd = joinpath(dir, "net.rrd")
                sysctx = _lctx()
                recctx = _lctx()
                try
                    talker = ROSNode.Node(sysctx, "talker")
                    pub = ROSNode.Publisher(talker, "/chatter", ROSNode.WireKeyValue)

                    rec = Recorder(RecorderConfig("reros_live"; sinks = [Save(rrd)]); ctx = recctx)

                    # the recorder (a separate session) must discover the talker's publisher
                    @test _until() do
                        any(e -> occursin("chatter", e.topic) && !e.is_local,
                            ROSReach.snapshot_endpoints(rec.idx))
                    end

                    reconcile!(rec)

                    @test any(n -> n.name == "talker", ROSReach.snapshot_nodes(rec.idx))
                    @test dropped(rec.sink) == 0

                    close(rec.sink)
                    try; close(rec.idx); catch; end
                    @test isfile(rrd) && filesize(rrd) > 0
                    close(pub)
                finally
                    close(sysctx); close(recctx)
                end
            end
        end

        @testset "start!/stop! loop records appearance + teardown" begin
            mktempdir() do dir
                rrd = joinpath(dir, "loop.rrd")
                recctx = _lctx()
                rec = Recorder(RecorderConfig("reros_loop"; sinks = [Save(rrd)]); ctx = recctx)
                start!(rec; interval = 0.1)
                try
                    sysctx = _lctx()
                    talker = ROSNode.Node(sysctx, "looptalker")
                    pub = ROSNode.Publisher(talker, "/loopchatter", ROSNode.WireKeyValue)
                    # the reconcile loop should pick up the new publisher via on_change
                    @test _until() do
                        any(p -> occursin("loopchatter", last(p).topic), rec.prev_eps)
                    end
                    close(pub); close(sysctx)
                    # teardown should be observed too (endpoint removed on a later pass)
                    @test _until(tries = 50) do
                        !any(p -> occursin("loopchatter", last(p).topic), rec.prev_eps)
                    end
                finally
                    stop!(rec)            # final reconcile + drain + close sink/idx
                    close(recctx)
                end
                @test isfile(rrd) && filesize(rrd) > 0
            end
        end

        @testset "records topic data through dynamic subscription + mapper" begin
            mktempdir() do dir
                rrd = joinpath(dir, "data.rrd")
                recctx = _lctx()
                cfg = RecorderConfig("reros_data"; sinks = [Save(rrd)],
                                     record = [rule(topic("/data_topic"), data = true)])
                rec = Recorder(cfg; ctx = recctx)
                start!(rec; interval = 0.1)
                try
                    sysctx = _lctx()
                    talker = ROSNode.Node(sysctx, "datatalker")
                    pub = ROSNode.Publisher(talker, "/data_topic", ROSNode.WireKeyValue)

                    # the recorder discovers the publisher and opens a data subscription
                    @test _until() do; haskey(rec.subs, "/data_topic"); end

                    for i in 1:8
                        ROSNode.publish(pub, ROSNode.WireKeyValue(key = "k$i", value = "v$i"))
                        sleep(0.1)
                    end

                    @test _until(tries = 60) do; rec.data_count[] >= 1; end
                    @test rec.data_errors[] == 0
                    close(pub); close(sysctx)
                finally
                    stop!(rec); close(recctx)
                end
                @test isfile(rrd) && filesize(rrd) > 0
            end
        end

        @testset "captures a transient_local latched value (subscribed AFTER publish)" begin
            mktempdir() do dir
                rrd = joinpath(dir, "latched.rrd")
                sysctx = _lctx()
                talker = ROSNode.Node(sysctx, "latchtalker")
                tlqos = ROSReach.QosProfile(reliability = :reliable, durability = :transient_local,
                                            history = :keep_last, depth = 1)
                pub = ROSNode.Publisher(talker, "/latched", ROSNode.WireKeyValue; qos = tlqos)
                ROSNode.publish(pub, ROSNode.WireKeyValue(key = "state", value = "latched_v1"))
                sleep(0.5)   # let the latched sample settle into the advanced-publisher cache

                recctx = _lctx()
                cfg = RecorderConfig("reros_latched"; sinks = [Save(rrd)],
                                     record = [rule(topic("/latched"), data = true)])
                rec = Recorder(cfg; ctx = recctx)
                start!(rec; interval = 0.1)
                try
                    # No live publish after the sub opens: data can ONLY arrive via the
                    # transient_local join-time history replay (the cross-repo ROSNode change).
                    @test _until(tries = 80) do; rec.data_count[] >= 1; end
                    @test rec.data_errors[] == 0
                finally
                    stop!(rec); close(recctx)
                    close(pub); close(sysctx)
                end
                @test isfile(rrd) && filesize(rrd) > 0
            end
        end

        @testset "attach_recorder! embeds in a host node (shared Context)" begin
            mktempdir() do dir
                rrd = joinpath(dir, "embed.rrd")
                ctx = _lctx()
                try
                    host = ROSNode.Node(ctx, "host_node")
                    pub = ROSNode.Publisher(host, "/embed_topic", ROSNode.WireKeyValue)
                    cfg = RecorderConfig("reros_embed"; sinks = [Save(rrd)],
                                         record = [rule(topic("/embed_topic"), data = true)])
                    rec = ReROS.attach_recorder!(host, cfg)     # shares the host's Context

                    # node-granular self-exclusion: the host node (a same-session sibling) IS
                    # recorded, even though it's is_local — only the recorder's own node is dropped.
                    @test _until() do; haskey(rec.subs, "/embed_topic"); end
                    @test _until() do; any(n -> n.name == "host_node", ROSReach.snapshot_nodes(rec.idx)); end
                    for i in 1:6
                        ROSNode.publish(pub, ROSNode.WireKeyValue(key = "k$i", value = "v$i")); sleep(0.1)
                    end
                    @test _until(tries = 60) do; rec.data_count[] >= 1; end
                    @test rec.data_errors[] == 0
                    stop!(rec); close(pub)
                finally
                    close(ctx)
                end
                @test isfile(rrd) && filesize(rrd) > 0
            end
        end

    end
finally
    # zenohd ignores SIGTERM (wedged libzenohc) → SIGKILL to avoid a lingering child.
    try; kill(_ROUTER, Base.SIGKILL); catch; end
end

end  # if _ZENOHD !== nothing
