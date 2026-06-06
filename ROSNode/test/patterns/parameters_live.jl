# §10 parameters — LIVE round-trips of the remote `ParameterClient` over a real
# Zenoh session, the end-to-end counterpart of patterns/parameters.jl (which covers
# the server transaction core + the client's pure helpers in-process). Each case
# drives a `Node(ctx, name, Schema)` server — which auto-wires the six standard
# `rcl_interfaces` parameter services + `/parameter_events` — through a
# `ParameterClient`, proving the wire contract a ROS2/hiroz client speaks and the
# local↔remote uniformity of the six verbs, `fetch`/`transaction`, the indexing
# sugar, `/parameter_events`, and `ParameterRejection`.
#
# Included by `runtests.jl` (private per-run router, ROS_TEST_EP). Standalone:
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/patterns/parameters_live.jl
# Always under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ParameterClient, wait_for_service,
               get_parameters, get_parameter_types, set_parameters, set_parameters_atomically,
               list_parameters, describe_parameters, parameter, set_parameter!,
               transaction, on_parameter_event, ParameterRejection,
               ParameterDescriptor, PARAMETER_INTEGER, PARAMETER_STRING, PARAMETER_NOT_SET
using Test

# The shared schema the server bakes in and the typed client lenses onto. `frame` is
# read-only, so a runtime set is rejected over the wire (the §10 read-only gate).
@parameters struct LiveParams
    "maximum speed"  max_speed::Int64 = 50 ∈ 0..100
    "planner mode"   mode::String = "auto" ∈ ("auto", "manual")
    "fixed frame"    frame::String = "map" |> readonly
end

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_pctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)
_pstep(msg::AbstractString) = (@info "  · $msg"; flush(stdout); flush(stderr))

# A typed server + a typed client routing-matched to it; runs `f(server, client)`.
# Distinct `server_name` per case keeps discovery clean across testsets (the router
# persists for the run). The server node is closed by the Context drain on exit.
function _server_client(f; server_name::AbstractString)
    _pctx() do ctx
        server = Node(ctx, server_name, LiveParams)
        node   = Node(ctx, "pclient")
        client = ParameterClient(node, "/" * server_name, LiveParams)
        try
            @test wait_for_service(client; timeout = 5)
            f(server, client)
        finally
            close(client); close(node)
        end
    end
end

@testset "parameters (Zenoh session)" begin

    # L0: the six verbs over the wire, 1:1 with a ROS2/hiroz client — and the set
    # lands on the server's typed struct (the wire contract a foreign client speaks).
    @testset "L0 round-trip + server reflects" begin
        _pstep("L0: get / get_types / set_atomically")
        _server_client(server_name = "psrv_l0") do server, client
            @test get_parameters(client, [:max_speed]) == [50]
            @test get_parameter_types(client, [:max_speed]) == [PARAMETER_INTEGER]
            @test get_parameter_types(client, [:mode]) == [PARAMETER_STRING]
            ok, reason = set_parameters_atomically(client, [:max_speed => 80])
            @test ok === true && reason == ""
            @test server.parameters.max_speed == 80            # landed on the server's typed struct
        end
    end

    # `set_parameters` (the per-item verb the file otherwise never drives): a
    # `Vector{Tuple{Bool,String}}`, one `(true, "")` per pair, and the change is on
    # the server (proves it crossed the wire + committed, not echoed by the client).
    @testset "L0 set_parameters (per-item verb)" begin
        _pstep("L0: set_parameters batch")
        _server_client(server_name = "psrv_set") do server, client
            results = set_parameters(client, [:max_speed => 60, :mode => "manual"])
            @test results isa Vector{Tuple{Bool, String}}
            @test results == [(true, ""), (true, "")]
            @test server.parameters.max_speed == 60            # Int64 value crossed the wire
            @test server.parameters.max_speed isa Int64
            @test server.parameters.mode == "manual"
            @test get_parameters(client, [:max_speed, :mode]) == [60, "manual"]
        end
    end

    # An unknown name reads back unset over the wire: `nothing` for the value and
    # `PARAMETER_NOT_SET` for the type — the §10 sentinel a foreign client relies on.
    @testset "L0 NOT_SET for unknown" begin
        _pstep("L0: unknown name → nothing / NOT_SET")
        _server_client(server_name = "psrv_unk") do server, client
            @test get_parameters(client, [:nope]) == [nothing]
            @test get_parameter_types(client, [:nope]) == [PARAMETER_NOT_SET]
        end
    end

    # L2: the Julian structure — `fetch` materializes a typed `LiveParams`, and
    # `transaction` mutates the remote with the same do-block as the local server.
    @testset "L2 typed snapshot + transaction" begin
        _pstep("L2: fetch(::LiveParams) + transaction push")
        _server_client(server_name = "psrv_l2") do server, client
            p = fetch(client)
            @test p isa LiveParams
            @test p.max_speed == 50 && p.mode == "auto"
            transaction(client) do d
                d.max_speed = 8
                d.mode      = "manual"
            end
            @test server.parameters.max_speed == 8             # the atomic push applied
            @test server.parameters.mode == "manual"
            @test fetch(client).max_speed == 8                 # a re-fetch sees it
        end
    end

    # Rejection is the same `ParameterRejection` whether the remote refuses (a wire
    # `set_parameter!`) or the typed transaction's client-side pre-check catches it
    # first — uniform with the local path, nothing applied either way.
    @testset "rejection is uniform (remote + client-side pre-check)" begin
        _pstep("reject: out-of-range set + transaction")
        _server_client(server_name = "psrv_rej") do server, client
            @test occursin("ParameterClient{LiveParams}", sprint(show, client))
            @test_throws ParameterRejection set_parameter!(client, :max_speed, 999)
            @test server.parameters.max_speed == 50            # remote rejected → nothing applied
            @test_throws ParameterRejection transaction(client) do d
                d.max_speed = 999                              # fails the client-side pre-check
            end
            @test server.parameters.max_speed == 50
        end
    end

    # A read-only field rejected *over the wire*: `set_parameter!` goes through
    # `set_parameters_atomically` (no client-side validate), so the rejection is the
    # server's read-only gate replying `(false, reason)`, re-raised as the same
    # `ParameterRejection` — uniform with the local path, the field left untouched.
    @testset "read-only rejected over the wire" begin
        _pstep("reject: read-only field set over the wire")
        _server_client(server_name = "psrv_ro") do server, client
            @test get_parameters(client, [:frame]) == ["map"]
            @test_throws ParameterRejection set_parameter!(client, :frame, "odom")
            @test server.parameters.frame == "map"             # read-only → nothing applied
            @test get_parameters(client, [:frame]) == ["map"]  # and the remote still reads "map"
            d = describe_parameters(client, [:frame])[1]
            @test d.read_only === true                         # the descriptor carries read-only on the wire
        end
    end

    # describe decodes the wire descriptor back into our `ParameterDescriptor`; list
    # names the parameters; `c[:name]` / `c[:name] = v` is the indexing sugar (L1).
    @testset "describe + list + indexing" begin
        _pstep("describe / list / getindex / setindex!")
        _server_client(server_name = "psrv_desc") do server, client
            d = describe_parameters(client, [:max_speed])[1]
            @test d.name === :max_speed
            @test d.type === Int64
            @test d.ptype == PARAMETER_INTEGER
            @test d.read_only === false
            @test d.constraint === nothing                     # the 0..100 range is dropped on the wire
            names = list_parameters(client)
            @test :max_speed in names && :mode in names
            @test client[:max_speed] == 50                     # L1 read
            client[:mode] = "manual"                            # L1 write
            @test server.parameters.mode == "manual"
        end
    end

    # The remote dual of `on_parameter_event`: subscribe the remote's
    # `/parameter_events`, observe a committed change (value coerced to its field type).
    @testset "remote /parameter_events" begin
        _pstep("events: subscribe remote, observe a change")
        _server_client(server_name = "psrv_evt") do server, client
            seen = Channel{Any}(4)
            on_parameter_event(client) do batch
                put!(seen, batch.changed)
            end
            sleep(0.6)                                         # let the events sub route
            _pstep("events: trigger a transaction")
            transaction(client) do d; d.max_speed = 7 end
            got = (timedwait(() -> isready(seen), 5.0; pollint = 0.05) === :ok) ? take!(seen) : nothing
            @test got !== nothing
            @test get(got, :max_speed, nothing) == 7           # the changed value
            @test got[:max_speed] isa Int64                    # coerced to the field type
        end
    end

    # No schema: the client still drives the six verbs, and `fetch` returns a dynamic
    # `NamedTuple`; `transaction` requires a typed client (there is no struct to draft).
    @testset "schemaless (dynamic) client" begin
        _pstep("dynamic: ParameterClient with no schema")
        _pctx() do ctx
            Node(ctx, "pdyn", LiveParams)
            node   = Node(ctx, "pdyn_client")
            client = ParameterClient(node, "/pdyn")            # no schema ⇒ ParameterClient{Nothing}
            try
                @test wait_for_service(client; timeout = 5)
                @test get_parameters(client, [:max_speed]) == [50]
                snap = fetch(client)                           # dynamic ⇒ NamedTuple
                @test snap isa NamedTuple
                @test snap.max_speed == 50 && snap.mode == "auto"
                @test_throws ArgumentError transaction(client) do d; d.max_speed = 1 end
            finally
                close(client); close(node)
            end
        end
    end

    # No server: `wait_for_service` returns `false` at its timeout — bounded, no hang.
    @testset "wait_for_service times out with no server" begin
        _pstep("no-server: client targets a nonexistent node")
        _pctx() do ctx
            node = Node(ctx, "p_none")
            client = ParameterClient(node, "/nobody", LiveParams)
            @test wait_for_service(client; timeout = 0.5) == false
            close(client)
        end
    end
end

# ── §7 sim-time: use_sim_time reroutes the node's ROS clock to /clock ─────────────
import Dates
using ROSNode: Publisher, publish, clock, register!, JumpCallback, ROS, System, nanoseconds

@parameters struct SimParams
    use_sim_time::Bool = false
end

const _ClockMsg = ROSNode.Interfaces.rosgraph_msgs.msg.Clock
const _TimeMsg  = ROSNode.Interfaces.builtin_interfaces.msg.Time
function _clockmsg(ns::Integer)
    s, n = divrem(Int64(ns), 1_000_000_000)
    _ClockMsg(clock = _TimeMsg(sec = Int32(s), nanosec = UInt32(n)))
end

@testset "sim-time: use_sim_time reroutes now(node, ROS) to /clock (§7)" begin
    _pctx() do ctx
        node = Node(ctx, "simnode", SimParams)
        clk  = clock(node, ROS())                  # stable handle; register a jump callback on it
        jumps = Channel{Any}(32)
        register!(clk, JumpCallback(j -> put!(jumps, j);
                                    on_clock_change = true, min_forward = Dates.Millisecond(1)))

        # Before activation the ROS clock is system time.
        @test abs(nanoseconds(Dates.now(node, ROS())) - nanoseconds(Dates.now(node, System()))) < 1_000_000_000

        _pstep("sim: activate use_sim_time, publish two /clock values")
        node.parameters.use_sim_time = true        # single-statement transaction → hook → set_use_sim_time!
        cpub = Publisher(node, "/clock", _ClockMsg)
        sleep(0.5)                                  # let the Context /clock sub + this route match

        t1 = 1_000_000_000                          # 1.0 s sim — baseline (no jump)
        publish(cpub, _clockmsg(t1))
        @test timedwait(() -> nanoseconds(Dates.now(node, ROS())) == t1, 5.0; pollint = 0.02) === :ok

        t2 = 5_000_000_000                          # 5.0 s sim — a forward discontinuity
        publish(cpub, _clockmsg(t2))
        @test timedwait(() -> nanoseconds(Dates.now(node, ROS())) == t2, 5.0; pollint = 0.02) === :ok

        # Jump callbacks fired on the node's clock: sim_activated (opt-in) + time_forward (t1→t2).
        @test timedwait(() -> isready(jumps), 5.0; pollint = 0.02) === :ok
        sleep(0.2)
        kinds = Symbol[]
        while isready(jumps); push!(kinds, Symbol(take!(jumps).kind)); end
        @test :sim_activated in kinds
        @test :time_forward in kinds

        _pstep("sim: deactivate → ROS reverts to system, sim_deactivated fires")
        node.parameters.use_sim_time = false        # synchronous: tears down + fires the jump
        @test abs(nanoseconds(Dates.now(node, ROS())) - nanoseconds(Dates.now(node, System()))) < 1_000_000_000
        sawdeact = false
        while isready(jumps); Symbol(take!(jumps).kind) === :sim_deactivated && (sawdeact = true); end
        @test sawdeact
    end
end
