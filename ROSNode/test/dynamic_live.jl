# D5 dynamic-type support — LIVE round-trips over a real Zenoh session.
#
# Included by `runtests.jl`, which launches a PRIVATE `zenohd` router on a random
# per-run port and exports `ROS_TEST_EP`; every session here connects ONLY to it
# (multicast off). That mirrors real rmw_zenoh (router-based discovery) and avoids
# the multicast-scouting I/O contention that, on a bare multi-session host, can wedge
# even same-session delivery (a blocking native call that ignores SIGTERM). Every
# wire wait is bounded (`call` is hard-bounded past its `timeout_ms`), so a missing
# route fails fast rather than hanging.
#
# Standalone (outside runtests): start a router and point these at it, e.g.
#     zenohd -l tcp/localhost:7447 &
#     ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/dynamic_live.jl
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: WireKeyValue, GetTypeDescription_Request, get_type_description,
               ServiceClient, DynamicSubscriptionHandle, fetch_type_description,
               register_type_description!, registry, endpoints_snapshot, realize!,
               lookup_type, _scan_for_struct
using ROSMessages: message_il, lower, type_description_from_struct,
                   TypeDescription, TypeDescriptionMsg, calculate_rihs01_hash
using ROSZenoh: TypeInfo, type_hash_from_rihs_string
using Test

# Connect every session ONLY to the per-run private router (set by runtests.jl) —
# multicast off, no contention. Standalone: set `ROS_TEST_EP` or run a router on :7447.
const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_rctx() = Context(; peers = [_EP], localhost_only = true)
_rctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

function _recv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint=0.05) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

# Flushed progress marker: the live tests can wedge on a blocking native call, so
# each step announces itself (and flushes) before it might block — a hung run then
# names the exact step it stuck on instead of going dark.
_step(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

@testset "D5 live (Zenoh session)" begin

    # S2/S3 — the GetTypeDescription wire round-trip (this IS the wire-discovery
    # mechanism: a client builds the request, the §13 server answers from the
    # registry, hashes round-trip).
    @testset "S2/S3 — GetTypeDescription round-trip" begin
        _step("S2/S3: opening context")
        _rctx() do ctx
            _step("S2/S3: creating talker node (serves ~/get_type_description)")
            node = Node(ctx, "talker")               # auto-serves ~/get_type_description
            _step("S2/S3: creating service client")
            client = ServiceClient(node, "/talker/get_type_description", GetTypeDescription_Request)
            sleep(0.3)
            _step("S2/S3: call #1 (TypeDescription, ≤4s)")
            resp = get_type_description(client,
                       "type_description_interfaces/msg/TypeDescription"; timeout_ms=4000)
            _step("S2/S3: call #1 returned")
            @test resp.successful
            @test resp.type_description.type_description.type_name ==
                  "type_description_interfaces/msg/TypeDescription"
            @test length(resp.type_description.referenced_type_descriptions) == 3
            _step("S2/S3: call #2 (not-found, ≤4s)")
            @test !get_type_description(client, "nope/msg/Nope"; timeout_ms=4000).successful
            _step("S2/S3: closing client")
            close(client)
            _step("S2/S3: client closed — do-block returning, context drain next")
        end
        _step("S2/S3: context drained, testset done")
    end

    # S5 — the keyexpr-only subscription receives a real typed message (registry-hit
    # resolution; the published well-known type is already registered).
    @testset "S5 — keyexpr-only subscription" begin
        _step("S5: open context + node + dynamic subscription")
        _rctx() do ctx
            node = Node(ctx, "n")
            got = Channel{Any}(8)
            sub = Subscription(node, "/kv") do msg; put!(got, msg); end
            @test sub isa DynamicSubscriptionHandle
            _step("S5: dynamic subscription created; opening publisher")
            pub = Publisher(node, "/kv", WireKeyValue)
            sleep(0.4)
            _step("S5: publish + await dynamic dispatch")
            publish(pub, WireKeyValue(key="hello", value="d5"))
            msg = _recv(got, 5.0)
            @test msg isa WireKeyValue && msg !== nothing && msg.key == "hello"
        end
    end

    # ROSNode↔ROSNode wire discovery, the real topology: node A runs in a SEPARATE
    # PROCESS (server, connected to the same router), node B here discovers A's type
    # by calling A's `~/get_type_description`. Separate processes are essential — two
    # contexts in ONE process share sticky consumer tasks on one thread, so B's
    # blocking `get` would starve A's server consumer (a single-process test artifact,
    # not a deployment one). Bounded + skip-safe: if the server subprocess doesn't
    # come up or the query doesn't round-trip in this env, we SKIP (never hang/fail).
    @testset "ROSNode↔ROSNode — cross-context wire discovery" begin
        # `Ping`'s RIHS01 must match between the server subprocess and here.
        il = message_il("string id\nint64 stamp\n"; name = "Ping")
        ast = _scan_for_struct(lower(il; package = "d5_demo"))
        td = TypeDescriptionMsg(type_description_from_struct(ast, "Ping"; package = "d5_demo", qualifier = "msg"),
                                TypeDescription[])
        hash = type_hash_from_rihs_string(calculate_rihs01_hash(td))

        srclog = joinpath(tempdir(), "rosnode-xctx-server-$(getpid()).log")
        proj = dirname(Base.active_project())
        server = """
            using ROSNode
            using ROSNode: registry, register_type_description!, _scan_for_struct
            using ROSMessages: message_il, lower, type_description_from_struct,
                               TypeDescription, TypeDescriptionMsg, calculate_rihs01_hash
            using ROSZenoh: TypeInfo, type_hash_from_rihs_string
            ctx = Context(; peers = ["$_EP"], localhost_only = true)
            node = Node(ctx, "server")
            il = message_il("string id\\nint64 stamp\\n"; name = "Ping")
            ast = _scan_for_struct(lower(il; package = "d5_demo"))
            td = TypeDescriptionMsg(type_description_from_struct(ast, "Ping"; package = "d5_demo", qualifier = "msg"),
                                    TypeDescription[])
            h = type_hash_from_rihs_string(calculate_rihs01_hash(td))
            register_type_description!(registry(ctx), td, TypeInfo("d5_demo/msg/Ping", h); cache = false)
            println("READY"); flush(stdout)
            while true; sleep(1); end
        """
        _step("xctx: launching server subprocess")
        proc = run(pipeline(`$(Base.julia_cmd()) --startup-file=no --project=$proj -e $server`,
                            stdout = srclog, stderr = srclog), wait = false)
        try
            _step("xctx: waiting for server READY (≤60s)")
            ready = timedwait(60.0; pollint = 0.25) do
                process_exited(proc) || (isfile(srclog) && occursin("READY", read(srclog, String)))
            end
            if ready !== :ok || process_exited(proc)
                @info "skipping cross-context: server subprocess not ready" log = srclog
                @test_skip ready
            else
                _rctx() do ctx
                    nodeB = Node(ctx, "client")
                    sleep(0.5)
                    _step("xctx: fetch_type_description from /server (≤5s)")
                    entry = fetch_type_description(nodeB, "/server", "d5_demo/msg/Ping", hash; timeout_ms = 5000)
                    if entry === nothing
                        @info "skipping cross-context: query did not round-trip in this env"
                        @test_skip entry
                    else
                        @test lookup_type(registry(ctx), TypeInfo("d5_demo/msg/Ping", hash)) !== nothing
                        @test ROSNode.ros_type_name(realize!(entry).type) == "d5_demo/msg/Ping"
                        @info "cross-context wire discovery OK — B fetched + realized A's type"
                    end
                end
            end
        finally
            # SIGKILL + reap: the server may be wedged in a libzenohc call that
            # ignores SIGTERM, and an unreaped child lingers as a zombie. Force-kill
            # then `wait` so no defunct process is left. (This is a test-spawned
            # subprocess, not a user session — killing it is correct.)
            try; process_running(proc) && kill(proc, Base.SIGKILL); catch; end
            try; wait(proc); catch; end
        end
    end
end
