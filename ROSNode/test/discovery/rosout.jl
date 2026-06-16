# D7 — unified logging: Julia's `@info` *is* the ROS logging API. Two layers:
#   • pure-logic (no session): level mapping, console format, structured render,
#     maxlog gating — the reliable core.
#   • live (private router): the dispatcher wraps every handler in the node logger,
#     so a plain `@info` inside a subscription handler surfaces on `/rosout`; plus
#     the node-owned shared publisher and the per-logger level table.
#
# Standalone (outside the suite): run a router on :7447 (or set ROS_TEST_EP), then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/rosout.jl
#
# The Zenoh-transport-log bridge (`bridge_zenoh_logs!`, §8) is NOT exercised here:
# Zenoh's logger is process-global + one-shot, so a live test would poison every
# other section's logging in the shared test process — it needs a dedicated
# subprocess (follow-up). The bridge's pure pieces (`_julia_log_level`) are covered.

using ROSNode
using ROSNode: Context, Node, Subscription, Publisher, publish, QosProfile,
               logger, set_logger_level!, Fatal, WireKeyValue,
               Service, ServiceClient, call, wait_for_service, Serial, Parallel
import Logging
using Logging: LogLevel, @logmsg
using Test

const _RLogMsg = ROSNode.LogMsg

# Self-contained echo service for the service-handler /rosout test (no sourced ROS2).
# The handler does a plain @info, so serving it must route that to the node's /rosout.
module _RosoutSvc
    using ROSNode
    @ros_package "rosout_test_msgs"
    @ros_service function Echo(key::String, value::String)::@NamedTuple{key::String, value::String}
        @info "in handler" key = key
        (key = key, value = value)
    end
end

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_rosctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# Block up to `secs` for the next value matching `pred` on `ch`; `nothing` on timeout.
# Distinct name from dynamic_live.jl's `_recv` (both files include into `Main`) so the
# two don't overwrite each other.
function _recv_match(ch::Channel, secs::Real; pred = _ -> true)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint = 0.02) do
        while isready(ch)
            v = take!(ch)
            if pred(v); ref[] = v; return true; end
        end
        false
    end === :ok ? ref[] : nothing
end

@testset "D7 unified logging" begin

    # ── pure logic (no Zenoh session) ─────────────────────────────────────────
    @testset "level mapping + Fatal" begin
        @test ROSNode._rosout_level(Logging.Debug) == 0x0a
        @test ROSNode._rosout_level(Logging.Info)  == 0x14
        @test ROSNode._rosout_level(Logging.Warn)  == 0x1e
        @test ROSNode._rosout_level(Logging.Error) == 0x28
        @test ROSNode._rosout_level(Fatal)         == 0x32   # FATAL=50 round-trips
        @test Fatal > Logging.Error
        @test ROSNode._console_level_label(Fatal) == "FATAL"
        @test ROSNode._console_level_label(Logging.Info) == "INFO"
    end

    @testset "structured-kwarg render" begin
        @test ROSNode._render_message("m", pairs((a = 1, b = 2))) == "m (a=1, b=2)"
        @test ROSNode._render_message("m", pairs(NamedTuple())) == "m"
        # internal kwargs (:maxlog, :_id) are not user payload — skipped.
        @test ROSNode._render_message("m", pairs((maxlog = 1, a = 2))) == "m (a=2)"
        # an :exception renders as its message, never throws out of logging.
        r = ROSNode._render_message("boom", pairs((exception = ErrorException("nope"),)))
        @test occursin("boom", r) && occursin("nope", r)
    end

    @testset "zenoh severity → Julia level" begin
        @test ROSNode._julia_log_level(ROSNode.Zenoh.LogSeverities.ERROR) == Logging.Error
        @test ROSNode._julia_log_level(ROSNode.Zenoh.LogSeverities.WARN)  == Logging.Warn
        @test ROSNode._julia_log_level(ROSNode.Zenoh.LogSeverities.INFO)  == Logging.Info
    end

    @testset "ROS console format + maxlog (sessionless tee)" begin
        # node=nothing → no /rosout sink, console only; exercises format + maxlog.
        lg = ROSNode.RosoutLogger(nothing, "/talker", nothing, true,
                                  Logging.BelowMinLevel, nothing, Dict{Any, Int}())
        path = tempname()
        open(path, "w") do f
            redirect_stderr(f) do
                Logging.with_logger(lg) do
                    @info "armed" speed = 3
                    @logmsg Fatal "meltdown" temp = 9000
                    for _ in 1:5
                        @info "spam" maxlog = 2
                    end
                end
            end
        end
        out = read(path, String)
        @test occursin("[INFO] [", out)
        @test occursin("[/talker]: armed (speed=3)", out)
        @test occursin("[FATAL] ", out) && occursin("meltdown (temp=9000)", out)
        @test length(collect(eachmatch(r"\[/talker\]: spam", out))) == 2   # maxlog=2
    end

    # ── live: /rosout round-trip + shared publisher + level table ─────────────
    @testset "live /rosout (Zenoh session)" begin
        _rosctx() do ctx
            talker   = Node(ctx, "talker")
            listener = Node(ctx, "listener")
            logs = Channel{Any}(32)

            # Listener subscribes to /rosout (reliable; live samples, published after join).
            rsub = Subscription(listener, "/rosout", _RLogMsg;
                                qos = QosProfile(reliability = :reliable, depth = 50)) do m
                put!(logs, m)
            end

            # Talker subscribes to /trigger; its handler does a plain @info — the
            # dispatcher wrap (§4) must route it to the talker's /rosout.
            tsub = Subscription(talker, "/trigger", WireKeyValue) do m
                @info "handled" key = m.key
            end
            trig = Publisher(listener, "/trigger", WireKeyValue)

            sleep(1.0)   # discovery: /rosout + /trigger routes must be up

            publish(trig, WireKeyValue(key = "go", value = ""))

            log = _recv_match(logs, 4.0; pred = m -> occursin("handled", m.msg))
            @test log !== nothing
            if log !== nothing
                @test log.name == talker.fqn          # logger name = node FQN
                @test log.level == 0x14               # INFO
                @test occursin("handled (key=go)", log.msg)
            end

            # Node-owned publisher is declared once and shared by node + child loggers.
            @test talker._rosout_pub !== nothing
            @test logger(talker) === logger(talker)                     # cached
            @test logger(talker, "child")._pub === talker._rosout_pub   # shared
            @test logger(talker, "child").name == talker.fqn * ".child"

            # Per-logger level table (§7): set a floor, observe shouldlog; children inherit.
            lg = logger(talker)
            @test Logging.shouldlog(lg, Logging.Info, Main, :g, :id)
            set_logger_level!(talker, Logging.Warn)
            @test !Logging.shouldlog(lg, Logging.Info, Main, :g, :id)   # Info now gated
            @test Logging.shouldlog(lg, Logging.Warn, Main, :g, :id)
            @test !Logging.shouldlog(logger(talker, "child"), Logging.Info, Main, :g, :id)  # inherits /talker
        end
    end

    # ── live: a service handler's plain @info routes to /rosout ────────────────
    # The consumer task installs the node logger once (not per request); a Serial
    # handler runs inline under it, a Parallel handler is spawned from within its
    # scope and must inherit the logstate. Verify both reach /rosout.
    @testset "service handler /rosout ($(nameof(typeof(conc))))" for conc in (Serial(), Parallel(2))
        _rosctx() do ctx
            srvnode  = Node(ctx, "srvlog")
            listener = Node(ctx, "srvlog_listener")
            logs = Channel{Any}(32)

            rsub = Subscription(listener, "/rosout", _RLogMsg;
                                qos = QosProfile(reliability = :reliable, depth = 50)) do m
                put!(logs, m)
            end

            # 3-arg authored form: the @ros_service function is marker + handler.
            srv = Service(srvnode, "/logsvc", _RosoutSvc.Echo; concurrency = conc)
            cli = ServiceClient(srvnode, "/logsvc", _RosoutSvc.Echo)
            @test wait_for_service(cli; timeout = 5)
            sleep(0.5)   # /rosout route up

            resp = call(cli; key = "k", value = "v")
            @test resp.key == "k"

            log = _recv_match(logs, 4.0; pred = m -> occursin("in handler", m.msg))
            @test log !== nothing
            if log !== nothing
                @test log.name == srvnode.fqn
                @test log.level == 0x14                       # INFO
                @test occursin("in handler (key=k)", log.msg)
            end
            close(cli); close(srv)
        end
    end
end
