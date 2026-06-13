# §D1 detached service settlement — LIVE round-trips proving a handler can hand its
# request off to another task. The companion to services.jl: there every handler
# settles within its own extent, here `detach!(req)` suppresses the at-return
# force-abort and transfers the owned Query + cell to a spawned task, which settles
# later via `respond!(handle, …)`. The fail-safe contract still holds end-to-end:
# a settled detached request replies for real, but a *forgotten* one can't hang the
# client — a per-service `detach_timeout` sweeper or a `close`-drain force-aborts it
# into a ServiceError.
#
# Included by `runtests.jl` (private per-run `zenohd`, `ROS_TEST_EP` exported,
# multicast off). Standalone: start a router, then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… test/patterns/service_detach.jl
# Always under a hard force-kill: `timeout -k 5 180 julia …`.

using ROSNode
using ROSNode: Context, Node, Service, ServiceClient, call, respond!, detach!,
               ServiceRequestHandle, ServiceError, success, failed
using Test

# Same BYO service type as services.jl (`robot_msgs/srv/DoThing`: string command →
# bool ok, string detail). Re-importing the registered package aliases the
# single-copy types, so client and server speak the same struct in-process.
module _DetachSrvTypes
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/srv/DoThing"
end
const DoThing_Request  = _DetachSrvTypes.robot_msgs.srv.DoThing_Request
const DoThing_Response = _DetachSrvTypes.robot_msgs.srv.DoThing_Response

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_sctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# Flushed progress marker: a hung native call then names the step it wedged on.
_sstep(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

# Run `f` on a spawned task; assert it finishes within `secs` — the bounded-wait
# harness for the "must not hang" assertions.
function _within(f::Function, secs::Real)
    t = Threads.@spawn f()
    done = timedwait(() -> istaskdone(t), Float64(secs); pollint = 0.02) === :ok
    @test done
    return t
end

@testset "service detach (Zenoh session)" begin

    # Happy path: TRUE fire-and-forget. The handler detaches, spawns a settler, and
    # returns `nothing` immediately — it does NOT wait for the settler. The detached
    # flag suppresses the at-return return-value fill (settlement.jl gates both the
    # normal-exit `fill!` and the fail-safe `force_abort!` on `cell.detached`), so the
    # empty cell is not settled at handler return; the spawned task settles it off the
    # handler frame via the first-class handle a beat later. The client's `call` must
    # get that real reply (not an at-return abort), with the handler's fields.
    @testset "detached happy path ⇒ real reply" begin
        _sstep("detach-ok: server detaches + spawns settler, returns immediately")
        _sctx() do ctx
            node = Node(ctx, "srv_detach_ok")
            Service(node, "/detach/ok", DoThing_Request) do req
                h = detach!(req)
                Threads.@spawn begin
                    sleep(0.3)                       # off-frame: handler already returned
                    respond!(h, success, DoThing_Response(; ok = true, detail = "done"))
                end
                nothing                              # fire-and-forget: no wait
            end
            client = ServiceClient(node, "/detach/ok", DoThing_Request)
            sleep(0.4)
            _sstep("detach-ok: call awaits the spawned settlement")
            resp = call(client, DoThing_Request(; command = "go"); timeout_ms = 5000)
            @test resp isa DoThing_Response
            @test resp.ok
            @test resp.detail == "done"
            close(client)
        end
    end

    # Forgotten respond! after detach: the handler detaches and never settles. A
    # short `detach_timeout` arms the sweeper, which force-aborts the orphaned cell
    # into an error reply — the client's `call` raises ServiceError within roughly
    # the deadline, never hangs.
    @testset "forgotten respond ⇒ swept to ServiceError" begin
        _sstep("detach-sweep: server detaches, never settles (detach_timeout=1.0)")
        _sctx() do ctx
            node = Node(ctx, "srv_detach_sweep")
            Service(node, "/detach/sweep", DoThing_Request; detach_timeout = 1.0) do req
                detach!(req)                         # transfer ownership, then drop it
                nothing
            end
            client = ServiceClient(node, "/detach/sweep", DoThing_Request)
            sleep(0.4)
            _sstep("detach-sweep: call must raise within the sweep deadline")
            err = Ref{Any}(nothing)
            # detach_timeout=1.0 + sweep interval (~1s) + transport slack; 12s leaves
            # head-room while still catching a regression to an indefinite block.
            _within(12.0) do
                try
                    call(client, DoThing_Request(; command = "x"); timeout_ms = 8000)
                catch e
                    err[] = e
                end
            end
            @test err[] isa ServiceError             # swept ⇒ error reply, not a fake ok
            close(client)
        end
    end

    # Detach-then-drain: the handler detaches and never settles, but the deadline is
    # far off (default 60s). Closing the service before then drains the outstanding
    # detached cell, force-aborting it — the client's `call` still terminates with a
    # ServiceError rather than waiting out the timeout.
    @testset "drain before deadline ⇒ ServiceError" begin
        _sstep("detach-drain: server detaches, far deadline; close drains it")
        _sctx() do ctx
            node = Node(ctx, "srv_detach_drain")
            entered = Channel{Nothing}(1)            # gate the close on the handler firing
            srv = Service(node, "/detach/drain", DoThing_Request; detach_timeout = 60.0) do req
                detach!(req)
                put!(entered, nothing)
                nothing
            end
            client = ServiceClient(node, "/detach/drain", DoThing_Request)
            sleep(0.4)
            _sstep("detach-drain: fire call, then close once the handler has detached")
            err = Ref{Any}(nothing)
            t = Threads.@spawn try
                call(client, DoThing_Request(; command = "x"); timeout_ms = 30000)
            catch e
                err[] = e
            end
            take!(entered)                           # handler has detached the request
            close(srv)                               # drain force-aborts the detached cell
            done = timedwait(() -> istaskdone(t), 10.0; pollint = 0.02) === :ok
            @test done                               # drain unblocks the caller well before 30s
            @test err[] isa ServiceError             # force-abort ⇒ error reply
            close(client)
        end
    end

    # Detached failure: TRUE fire-and-forget. The handler detaches, spawns a settler,
    # and returns `nothing` immediately without waiting. The spawned task settles with
    # `failed` + a message off-frame, which is a Zenoh query error reply — the client's
    # `call` raises ServiceError carrying that message.
    @testset "detached failure ⇒ ServiceError with message" begin
        _sstep("detach-fail: server detaches + spawns a failed settlement, returns immediately")
        _sctx() do ctx
            node = Node(ctx, "srv_detach_fail")
            Service(node, "/detach/fail", DoThing_Request) do req
                h = detach!(req)
                Threads.@spawn begin
                    sleep(0.2)                       # off-frame: handler already returned
                    respond!(h, failed, "nope")
                end
                nothing                              # fire-and-forget: no wait
            end
            client = ServiceClient(node, "/detach/fail", DoThing_Request)
            sleep(0.4)
            _sstep("detach-fail: call must raise the failure message")
            err = Ref{Any}(nothing)
            _within(8.0) do
                try
                    call(client, DoThing_Request(; command = "x"); timeout_ms = 5000)
                catch e
                    err[] = e
                end
            end
            @test err[] isa ServiceError
            @test occursin("nope", err[].msg)        # the settler's failure text surfaces
            close(client)
        end
    end
end
