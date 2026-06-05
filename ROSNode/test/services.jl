# §8 services — LIVE request/reply round-trips over a real Zenoh session, the
# end-to-end counterpart of settlement.jl (which only exercises the abstract cell
# with a fake handle). Every case proves the §8 fail-safe contract on the wire: a
# handler return is a reply-ok, an explicit `failed`/`respond!`-failure or a thrown
# handler is a query *error* reply (the client's `call` raises `ServiceError`), and
# no path can hang a blocked caller — a no-server `call` is hard-bounded.
#
# Included by `runtests.jl`, which launches a PRIVATE `zenohd` router on a random
# per-run port and exports `ROS_TEST_EP`; every session connects ONLY to it
# (multicast off), mirroring rmw_zenoh router discovery. Standalone (outside the
# suite): run a router on :7447 (or set ROS_TEST_EP), then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/services.jl
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, Service, ServiceClient, call, respond!,
               ServiceError, failed, LifecycleNode, configure!, activate!,
               inner_node, GetTypeDescription_Request,
               wait_for_service, service_matched
using Test

# BYO service type: `robot_msgs/srv/DoThing` (string command → bool ok, string
# detail). `@ros_import from=` resolves against this source file, so the fixture
# path is relative to test/. Re-importing the already-registered package aliases
# the single-copy types (same identity dynamic_types.jl proved), so client and
# server speak the *same* struct in-process.
module _SrvTypes
    using ROSNode
    @ros_import from="fixtures/byo" "robot_msgs/srv/DoThing"
end
const DoThing_Request  = _SrvTypes.robot_msgs.srv.DoThing_Request
const DoThing_Response = _SrvTypes.robot_msgs.srv.DoThing_Response

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_sctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# Flushed progress marker: a live test can wedge on a blocking native call, so each
# step announces itself before it might block — a hung run then names the step.
_sstep(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

# Run `f` on a spawned task and assert it finishes within `secs` — the bounded-wait
# harness for "must not hang" assertions. Returns the task (done) or fails the test.
function _within(f::Function, secs::Real)
    t = Threads.@spawn f()
    done = timedwait(() -> istaskdone(t), Float64(secs); pollint = 0.02) === :ok
    @test done                                   # never hangs past the deadline
    return t
end

@testset "services (Zenoh session)" begin

    # The happy path: a handler that returns its Response is a reply-ok; the client's
    # synchronous `call` decodes it. Request fields round-trip into the handler.
    @testset "reply-ok round trip" begin
        _sstep("reply-ok: server + client")
        _sctx() do ctx
            node = Node(ctx, "srv_ok")
            seen = Channel{String}(4)
            Service(node, "/do", DoThing_Request) do req
                put!(seen, req.command)                  # request reaches the handler
                return DoThing_Response(; ok = true, detail = "did " * req.command)
            end
            client = ServiceClient(node, "/do", DoThing_Request)
            sleep(0.4)                                    # let the route match
            _sstep("reply-ok: call")
            resp = call(client, DoThing_Request(; command = "go"); timeout_ms = 4000)
            @test resp isa DoThing_Response
            @test resp.ok
            @test resp.detail == "did go"
            @test isready(seen) && take!(seen) == "go"    # request correlated to handler
            close(client)
        end
    end

    # An explicit `respond!(req, failed, msg)` settles the request as a failure → a
    # Zenoh query *error* reply, so the client's `call` raises `ServiceError` (it
    # must NOT get a plausible zeroed Response). The user's message surfaces.
    # An explicit `respond!(req, failed, msg)` settles the request as a failure → a
    # Zenoh query *error* reply, so the client's `call` raises `ServiceError` (it must
    # NOT get a plausible zeroed Response). The user's message surfaces.
    @testset "explicit respond! failure ⇒ ServiceError" begin
        _sstep("fail: server respond!(failed) + client")
        _sctx() do ctx
            node = Node(ctx, "srv_fail")
            Service(node, "/fail", DoThing_Request) do req
                respond!(req, failed, "refused: " * req.command)
            end
            client = ServiceClient(node, "/fail", DoThing_Request)
            sleep(0.4)
            _sstep("fail: call expects ServiceError")
            err = nothing
            try
                call(client, DoThing_Request(; command = "x"); timeout_ms = 4000)
            catch e
                err = e
            end
            @test err isa ServiceError
            @test occursin("refused: x", err.msg)         # user's failure text surfaces
            close(client)
        end
    end

    # A throwing handler is the fail-safe case: the framework synthesizes an error
    # reply so the caller raises rather than hanging on a never-filled cell.
    @testset "throwing handler ⇒ ServiceError (fail-safe, no hang)" begin
        _sstep("throw: server throws + client")
        _sctx() do ctx
            node = Node(ctx, "srv_throw")
            Service(node, "/boom", DoThing_Request) do req
                error("handler blew up")                  # uncaught throw
            end
            client = ServiceClient(node, "/boom", DoThing_Request)
            sleep(0.4)
            _sstep("throw: call bounded, expects ServiceError")
            err = Ref{Any}(nothing)
            # Bounded: the throw must surface as a ServiceError within the deadline,
            # never hang the caller (the swallowed handler error is logged by design).
            _within(8.0) do
                try
                    call(client, DoThing_Request(; command = "x"); timeout_ms = 4000)
                catch e
                    err[] = e
                end
            end
            @test err[] isa ServiceError
            close(client)
        end
    end

    # No server: `call` must return/raise within its bounded deadline (timeout_ms +
    # slack), NOT block forever. A blocked caller without a reply is a ServiceError.
    @testset "no-server call is bounded (no hang)" begin
        _sstep("no-server: client with no matching service")
        _sctx() do ctx
            node = Node(ctx, "srv_none")
            client = ServiceClient(node, "/nobody/home", DoThing_Request)
            sleep(0.2)
            _sstep("no-server: call must return within deadline")
            err = Ref{Any}(nothing)
            # timeout_ms=1000 ⇒ the client hard-bounds at ~1s + 2s slack; 6s leaves
            # head-room while still catching a regression to the indefinite block.
            _within(6.0) do
                try
                    call(client, DoThing_Request(; command = "x"); timeout_ms = 1000)
                catch e
                    err[] = e
                end
            end
            @test err[] isa ServiceError                  # no reply ⇒ raises, doesn't hang
            close(client)
        end
    end

    # Request-id correlation: a burst of distinct requests each gets its own reply
    # (the handler echoes the command into the detail), so a reply can't be crossed
    # with another in-flight call. Async `call` lets several be outstanding at once.
    @testset "request correlation across concurrent calls" begin
        _sstep("correlate: server echoes command")
        _sctx() do ctx
            node = Node(ctx, "srv_corr")
            Service(node, "/echo", DoThing_Request; concurrency = Parallel(4)) do req
                return DoThing_Response(; ok = true, detail = req.command)
            end
            client = ServiceClient(node, "/echo", DoThing_Request)
            sleep(0.4)
            _sstep("correlate: fire concurrent async calls")
            cmds = ["a", "b", "c", "d", "e"]
            futs = [call(client, DoThing_Request(; command = c); async = true,
                         timeout_ms = 5000) for c in cmds]
            for (c, f) in zip(cmds, futs)
                resp = fetch(f)                            # async ⇒ fetchable
                @test resp.detail == c                     # each reply matches its request
            end
            close(client)
        end
    end

    # §14.2 service gate (Tier-1 #3): an application Service on `inner_node(ln)` of an
    # Inactive LifecycleNode error-replies "node inactive" (the handler never runs),
    # then serves normally once `activate!`d. The control surface stays exempt; this
    # is an application service, so it follows the gate.
    @testset "lifecycle gate: inactive service error-replies, active serves" begin
        _sstep("gate: lifecycle node (Inactive) + application service")
        _sctx() do ctx
            ln = LifecycleNode(ctx, "managed_srv")
            configure!(ln)                                 # Unconfigured → Inactive
            ran = Threads.Atomic{Int}(0)
            Service(inner_node(ln), "/work", DoThing_Request) do req
                Threads.atomic_add!(ran, 1)                # only runs while Active
                return DoThing_Response(; ok = true, detail = "ran " * req.command)
            end
            client = ServiceClient(inner_node(ln), "/work", DoThing_Request)
            sleep(0.4)

            _sstep("gate: call while Inactive ⇒ node-inactive error reply")
            err = Ref{Any}(nothing)
            _within(8.0) do
                try
                    call(client, DoThing_Request(; command = "x"); timeout_ms = 4000)
                catch e
                    err[] = e
                end
            end
            @test err[] isa ServiceError                   # gated ⇒ error reply, not a fake ok
            @test occursin("inactive", err[].msg)          # the §14.2 "node inactive" note
            @test ran[] == 0                               # handler did NOT run while gated

            # rcl-level infrastructure (get_type_description) sits BELOW the managed-node
            # abstraction and must serve in EVERY lifecycle state — it is NOT gated like an
            # application service (the Tier-1 #3 exemption). A query while Inactive must come
            # back as a normal reply (here a not-found Response), never a "node inactive" error.
            _sstep("gate: get_type_description still serves while Inactive")
            gtd = ServiceClient(inner_node(ln), "~/get_type_description",
                                GetTypeDescription_Request)
            sleep(0.4)
            gtd_inactive_err = false
            try
                call(gtd, GetTypeDescription_Request(; type_name = "", type_hash = "",
                                                     include_type_sources = false);
                     timeout_ms = 4000)
            catch err
                gtd_inactive_err = err isa ServiceError && occursin("inactive", err.msg)
            end
            @test !gtd_inactive_err                        # infra service exempt from the §14.2 gate
            close(gtd)

            _sstep("gate: activate ⇒ service serves normally")
            activate!(ln)                                  # Inactive → Active
            sleep(0.3)
            resp = call(client, DoThing_Request(; command = "go"); timeout_ms = 4000)
            @test resp isa DoThing_Response && resp.ok
            @test resp.detail == "ran go"
            @test ran[] == 1                               # now the handler ran
            close(client); close(ln)
        end
    end

    # `wait_for_service(client)` blocks on a real *routing* match (the client's
    # Querier matched the server's queryable), so the first `call` needs no sleep —
    # even in-process, where graph liveliness is authoritative before Zenoh routing
    # settles. This is the idiom that replaces the example's `sleep`.
    @testset "wait_for_service routing-match ⇒ no sleep before first call" begin
        _sstep("matched: server + client, await routing match")
        _sctx() do ctx
            node = Node(ctx, "srv_matched")
            Service(node, "/await", DoThing_Request) do req
                return DoThing_Response(; ok = true, detail = "ack " * req.command)
            end
            client = ServiceClient(node, "/await", DoThing_Request)

            matched = Ref(false)
            _within(8.0) do                                # must resolve, never hang
                matched[] = wait_for_service(client; timeout = 5)
            end
            @test matched[]                                # routing match observed
            @test service_matched(client)                  # predicate agrees

            _sstep("matched: call succeeds immediately (no sleep)")
            resp = call(client, DoThing_Request(; command = "now"); timeout_ms = 4000)
            @test resp isa DoThing_Response && resp.ok
            @test resp.detail == "ack now"
            @test service_matched(client)                  # still matched after the call
            close(client)
            @test !service_matched(client)                 # closed ⇒ not matched (graceful, no throw)
        end
    end

    # No server: wait_for_service returns `false` at its timeout — bounded, no hang.
    @testset "wait_for_service times out cleanly with no server" begin
        _sstep("matched-timeout: client, no matching server")
        _sctx() do ctx
            node = Node(ctx, "srv_await_none")
            client = ServiceClient(node, "/await/nobody", DoThing_Request)
            ready = Ref(true)
            _within(5.0) do
                ready[] = wait_for_service(client; timeout = 0.5)
            end
            @test ready[] == false                         # timed out → false, didn't hang
            @test !service_matched(client)
            close(client)
        end
    end
end
