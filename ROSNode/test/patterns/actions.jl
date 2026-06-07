# §9 actions — LIVE goal lifecycle over a real Zenoh session, the end-to-end
# counterpart of settlement.jl (which only exercises the abstract cell). Covers the
# full protocol: accept / reject / defer, a goal run to SUCCEEDED with a fetchable
# result, structured cancellation (handler observes `Cancelled` at a checkpoint →
# terminal CANCELED, not stuck CANCELING), `get_result` replay for a late fetch,
# feedback filtered by goal id, and the §9 fail-safe (Tier-1 #4): a goal whose
# RESULT TYPE is non-defaultable that aborts must let the client `fetch` raise/return
# WITHIN A BOUNDED TIME, never hang.
#
# Included by `runtests.jl`, which launches a PRIVATE `zenohd` router on a random
# per-run port and exports `ROS_TEST_EP`; every session connects ONLY to it
# (multicast off), mirroring rmw_zenoh router discovery. Standalone (outside the
# suite): run a router on :7447 (or set ROS_TEST_EP), then
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/actions.jl
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ActionServer, ActionClient, GoalHandle,
               send, cancel, state, feedback, feedback!, checkpoint, succeed,
               abort, execute, accept, reject, defer, respond!, succeeded,
               aborted, canceled, Cancelled, wait_for_service
using Test

# BYO action type: `robot_msgs/action/Process` (int32 total → int32[] results
# feedback → int32 done result). `@ros_import from=` resolves against this source
# file; re-importing the already-registered package aliases the single-copy
# msg/srv types and generates the action wrappers, so client and server speak the
# same structs in-process.
module _ActTypes
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/action/Process"
end
const Process          = _ActTypes.robot_msgs.action.Process
const Process_Goal     = _ActTypes.robot_msgs.action.Process_Goal
const Process_Result   = _ActTypes.robot_msgs.action.Process_Result
const Process_Feedback = _ActTypes.robot_msgs.action.Process_Feedback

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_actx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

_astep(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

# Block up to `secs` for the next value on `ch`; `nothing` on timeout.
function _arecv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint = 0.02) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

# Run `f` on a spawned task; assert it finishes within `secs` (the "must not hang"
# harness). Returns the (done-or-not) task; the caller fetches its value/raise.
function _within(f::Function, secs::Real)
    t = Threads.@spawn f()
    @test (timedwait(() -> istaskdone(t), Float64(secs); pollint = 0.02) === :ok)
    return t
end

# Wait until the action server's services are discoverable before driving it. `send`,
# `fetch` (get_result) and `cancel` each one-shot-`get` their OWN service, and the five
# endpoints discover independently — so waiting on send_goal alone still races fetch's
# get_result. Wait on all three the tests touch.
function _wait_action(c::ActionClient)
    for sfx in ("send_goal", "get_result", "cancel_goal")
        wait_for_service(c.node, string(c.name, "/_action/", sfx); timeout = 6.0)
    end
    sleep(0.5)   # liveliness (the graph) leads Zenoh queryable route-matching; let it settle
end

# Regression (real-peer interop): every action service `get` (send_goal/get_result/cancel)
# must carry an rmw_zenoh request attachment. A C++/Rust rmw_zenoh peer reads it from
# `query.attachment()` to stamp its reply and PANICS if absent (observed: hiroz's accept()
# `.unwrap()` on `None`). Julia↔Julia tolerates a missing one — the server mints its own reply
# attachment — so the live lifecycle tests below don't catch it; this guards the client side.
@testset "action client request attachment" begin
    _actx() do ctx
        client = ActionClient(Node(ctx, "att_probe"), "/att_probe", Process)
        @test client._gid != ntuple(_ -> 0x00, 16)        # ctor derived a real source gid
        s0 = @atomic client._seq
        @test ROSNode._request_attachment(client) !== nothing
        @test (@atomic client._seq) == s0 + 1             # per-request seq advanced
        close(client)
    end
end

@testset "actions (Zenoh session)" begin

    # on_goal accept / reject / defer: the three acceptance decisions surface on the
    # client's returned handle state, and `defer` accepts but does NOT begin execution.
    @testset "on_goal accept / reject / defer" begin
        _astep("decisions: server keyed on goal.total")
        _actx() do ctx
            server_node = Node(ctx, "act_dec_srv")
            client_node = Node(ctx, "act_dec_cli")
            executed = Channel{Int}(8)
            # total==1 ⇒ accept (run now); ==2 ⇒ reject; ==3 ⇒ defer (accept, no run).
            ActionServer(server_node, "/dec", Process;
                         on_goal = req -> req.total == 2 ? reject() :
                                          req.total == 3 ? defer()  : accept()) do g
                put!(executed, Int(g.request.total))           # only accepted-now goals run
                succeed(g, Process_Result(; done = Int32(1)))
            end
            client = ActionClient(client_node, "/dec", Process)
            _wait_action(client)

            _astep("decisions: accept")
            ga = send(client, Process_Goal(; total = Int32(1)))
            @test state(ga) === :accepted
            _astep("decisions: reject")
            gr = send(client, Process_Goal(; total = Int32(2)))
            @test state(gr) === :rejected
            _astep("decisions: defer")
            gd = send(client, Process_Goal(; total = Int32(3)))
            @test state(gd) === :accepted                       # accepted…
            # …but a deferred goal does not execute until `execute(goal)`; only the
            # accept-now goal (total 1) should have run.
            sleep(0.4)
            ran = Int[]
            while isready(executed); push!(ran, take!(executed)); end
            @test 1 in ran
            @test !(3 in ran)                                   # deferred ⇒ not run
            close(client)
        end
    end

    # A normal goal runs to SUCCEEDED and the result is fetchable with the value the
    # handler computed.
    @testset "goal runs to SUCCEEDED, result fetchable" begin
        _astep("success: server doubles total into done")
        _actx() do ctx
            server_node = Node(ctx, "act_ok_srv")
            client_node = Node(ctx, "act_ok_cli")
            ActionServer(server_node, "/sum", Process) do g
                succeed(g, Process_Result(; done = g.request.total * Int32(2)))
            end
            client = ActionClient(client_node, "/sum", Process)
            _wait_action(client)
            _astep("success: send + fetch")
            g = send(client, Process_Goal(; total = Int32(21)))
            @test state(g) === :accepted
            res = Ref{Any}(nothing)
            _within(10.0) do
                res[] = fetch(g)                                # fetch must not hang
            end
            @test res[] isa Process_Result
            @test res[].done == Int32(42)                       # the handler's computed result
            @test state(g) === :succeeded
            close(client)
        end
    end

    # Structured cancellation: the handler loops on `checkpoint`/`feedback!`; a client
    # `cancel` moves the goal to CANCELING, the next checkpoint throws `Cancelled`, and
    # fail-safe settlement lands the goal in CANCELED (NOT stuck at CANCELING).
    @testset "cancel ⇒ handler observes Cancelled ⇒ terminal CANCELED" begin
        _astep("cancel: server loops on checkpoint")
        _actx() do ctx
            server_node = Node(ctx, "act_cancel_srv")
            client_node = Node(ctx, "act_cancel_cli")
            entered = Channel{Bool}(1)
            ActionServer(server_node, "/long", Process) do g
                put!(entered, true)
                # A long loop whose only exit (besides completion) is the cancellation
                # throw at the checkpoint — the structured-cancel contract.
                for _ in 1:10_000
                    checkpoint(g)                                # throws Cancelled once CANCELING
                    sleep(0.02)
                end
                succeed(g, Process_Result(; done = Int32(0)))    # not reached under cancel
            end
            client = ActionClient(client_node, "/long", Process)
            _wait_action(client)

            _astep("cancel: send + wait for handler entry")
            g = send(client, Process_Goal(; total = Int32(100)))
            @test state(g) === :accepted
            @test _arecv(entered, 5.0) === true                  # handler is running
            _astep("cancel: request cancel")
            cancel(g)
            res = Ref{Any}(nothing)
            _within(10.0) do
                res[] = fetch(g)                                 # fetch must not hang
            end
            @test state(g) === :canceled                         # terminal CANCELED, not CANCELING
            close(client)
        end
    end

    # get_result replay: after a goal settles, a *late* fetch (issued well after the
    # goal finished) still recovers the cached result — the server holds it for replay.
    @testset "get_result replay for a late fetch" begin
        _astep("replay: server settles fast, client fetches late")
        _actx() do ctx
            server_node = Node(ctx, "act_replay_srv")
            client_node = Node(ctx, "act_replay_cli")
            ActionServer(server_node, "/quick", Process) do g
                succeed(g, Process_Result(; done = g.request.total))
            end
            client = ActionClient(client_node, "/quick", Process)
            _wait_action(client)
            g = send(client, Process_Goal(; total = Int32(7)))
            @test state(g) === :accepted
            _astep("replay: let the goal settle before fetching")
            sleep(1.0)                                           # goal long since terminal
            res = Ref{Any}(nothing)
            _within(10.0) do
                res[] = fetch(g)                                 # cached result replays
            end
            @test res[] isa Process_Result && res[].done == Int32(7)
            @test state(g) === :succeeded
            close(client)
        end
    end

    # Feedback filtered by goal id: two concurrent goals each publish distinct
    # feedback; a client streaming one goal's feedback must see only ITS messages
    # (the feedback topic carries the goal id and the stream drops other goals').
    @testset "feedback filtered by goal id" begin
        _astep("feedback: server emits goal-specific feedback")
        _actx() do ctx
            server_node = Node(ctx, "act_fb_srv")
            client_node = Node(ctx, "act_fb_cli")
            # Each goal emits feedback whose `results` payload encodes its own `total`,
            # so a leak across goals would show the wrong tag.
            ActionServer(server_node, "/fb", Process; concurrency = Parallel(4)) do g
                # `feedback(g)` opens the client's subscription lazily (on first call,
                # after `send`), so emit steadily for a few seconds rather than a short
                # burst the client would miss before it has subscribed.
                for _ in 1:30
                    feedback!(g, Process_Feedback(; results = Int32[g.request.total]))
                    sleep(0.1)
                end
                succeed(g, Process_Result(; done = g.request.total))
            end
            client = ActionClient(client_node, "/fb", Process)
            _wait_action(client)

            _astep("feedback: two goals in flight, stream goal A only")
            gA = send(client, Process_Goal(; total = Int32(11)))
            gB = send(client, Process_Goal(; total = Int32(22)))
            @test state(gA) === :accepted && state(gB) === :accepted
            fbA = feedback(gA)                                   # stream A's feedback only

            seen = Int32[]
            timedwait(8.0; pollint = 0.02) do
                while isready(fbA)
                    fb = take!(fbA)
                    isempty(fb.results) || push!(seen, fb.results[1])
                end
                length(seen) >= 3
            end
            @test !isempty(seen)                                 # A's feedback arrived
            @test all(==(Int32(11)), seen)                       # NONE of B's (22) leaked in
            _within(10.0) do
                fetch(gA)
            end
            _within(10.0) do
                fetch(gB)
            end
            close(client)
        end
    end

    # §9 fail-safe (Tier-1 #4): a goal whose RESULT TYPE is non-defaultable — its
    # fields have NO keyword defaults, so a no-arg `Process_Result()` throws — that
    # ABORTS via a throwing handler must NOT hang the client. Fail-safe settlement
    # force-aborts the goal with no result payload; the server then SYNTHESIZES a
    # zero-filled result (or, if even that is impossible, error-replies the status),
    # so the client's `fetch` finishes — returning a fail-safe result OR raising —
    # within a bounded time rather than blocking forever on a reply that never comes.
    #
    # `Process_Result(; done = …)` is the only ctor with a value; `Process_Result()`
    # raises (required `done`). The regression this guards is the server throwing
    # while building the fallback result inside its queryable → swallowed → no reply →
    # client hangs. The bounded-fetch assertion below fails on that hang.
    @testset "non-defaultable result + abort ⇒ fetch bounded (no hang)" begin
        # Sanity: the result type really is non-defaultable (no all-defaults ctor).
        @test_throws Exception Process_Result()

        _astep("faildef: aborting goal w/ non-defaultable result")
        _actx() do ctx
            server_node = Node(ctx, "act_faildef_srv")
            client_node = Node(ctx, "act_faildef_cli")
            # A handler that always throws (before producing a result) ⇒ fail-safe
            # force-abort with a nothing payload, exercising the server's
            # non-defaultable-result fallback on the get_result reply path.
            ActionServer(server_node, "/faildef", Process) do g
                error("handler blew up before producing a result")
            end
            client = ActionClient(client_node, "/faildef", Process)
            _wait_action(client)
            _astep("faildef: send + bounded fetch")
            g = send(client, Process_Goal(; total = Int32(1)))
            @test state(g) === :accepted
            # The crux: fetch must finish (raise or return a fail-safe result) within a
            # bounded time — a regression to the old hang fails THIS deadline.
            outcome = Ref{Symbol}(:pending)
            _within(15.0) do
                try
                    fetch(g)
                    outcome[] = :returned                        # fail-safe result is acceptable
                catch
                    outcome[] = :raised                          # error reply ⇒ raise is acceptable
                end
            end
            @test outcome[] !== :pending                         # did NOT hang
            close(client)
        end
    end
end
