# Action server + client for example_interfaces/action/Fibonacci.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/action.jl           # both, one process (default)
#     julia --project=. ROSNode/examples/action.jl server    # serve only (Ctrl-C to stop)
#     julia --project=. ROSNode/examples/action.jl client    # send a goal (needs a server)
#
# Fibonacci is the canonical demo action: a Goal `order`, streamed Feedback
# `sequence`, and a final Result `sequence`. The `client` role interoperates
# with `ros2 action send_goal /fibonacci example_interfaces/action/Fibonacci "{order: 10}"`
# (or a C++/Python server) given a matching router and domain.

using ROSNode

# `@ros_import` of a fully-qualified action binds the bare `Fibonacci` namespace — a
# tag type that both *is* the action handle (pass it to the constructors) and exposes
# the sections by short name: `Fibonacci.Goal`, `Fibonacci.Feedback`, `Fibonacci.Result`
# (plus the protocol wrappers the runtime uses internally). Wire names are unchanged
# (`Fibonacci_Goal`, …).
@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"

# Which half(s) to run: the `ROLE` env var or the first CLI arg, defaulting to `both`.
const ROLE = lowercase(get(ENV, "ROLE", isempty(ARGS) ? "both" : ARGS[1]))
ROLE in ("both", "server", "client") ||
    error("usage: action.jl [both|server|client] (got $(repr(ROLE)))")
const RUN_SERVER = ROLE in ("both", "server")
const RUN_CLIENT = ROLE in ("both", "client")

# `localhost_only` keeps discovery on the configured router (no multicast); drop it
# to reach peers on other hosts.
@context(peers = ["tcp/localhost:7447"], localhost_only = true) do ctx
    node = Node(ctx, "fibonacci")

    # ── Server (high-level do-block) ──────────────────────────────────────────
    # The handler runs once per accepted goal. `goal` is a GoalHandle: `goal.request`
    # is the decoded Goal. `feedback!` publishes a Feedback message AND checkpoints
    # cancellation — if a client cancels, it throws `Cancelled` here and the goal
    # settles CANCELED automatically. Returning a Result settles the goal SUCCEEDED.
    server = nothing
    if RUN_SERVER
        server = ActionServer(node, "/fibonacci", Fibonacci) do goal
            seq = Int32[0, 1]
            for i in 1:goal.request.order
                push!(seq, seq[end] + seq[end-1])
                feedback!(goal, Fibonacci.Feedback(sequence = copy(seq)))
                sleep(0.3)  # pretend the step is expensive; also a cancellation yield point
            end
            @info "goal complete" seq
            Fibonacci.Result(sequence = seq)
        end
    end

    # ── Client ────────────────────────────────────────────────────────────────
    if RUN_CLIENT
        client = ActionClient(node, "/fibonacci", Fibonacci)
        # Block until routing-matched to a server — no sleep. Returns `false` on
        # timeout (e.g. no server up), so we don't send a goal into the void.
        if wait_for_action_server(client; timeout = 5)
            # `send` blocks until the server accepts/rejects, then returns a handle.
            gh = send(client, Fibonacci.Goal(order = Int32(8)))
            @info "goal" state = state(gh)   # :accepted (or :rejected)

            # Stream feedback as it arrives; the loop ends when the goal settles — the
            # framework drives the result behind the scenes, so there's no task to juggle.
            for fb in feedback(gh)
                @info "feedback" fb.sequence
            end

            # The goal has settled; `fetch` returns its (now cached) result.
            result = fetch(gh)
            @info "result" sequence = result.sequence
            @info "final state" state = state(gh)   # :succeeded

            # To cancel a running goal instead: `cancel(gh)` — the server's `feedback!`/
            # `checkpoint` then throws `Cancelled` and the goal settles :canceled.
        else
            @warn "no /fibonacci action server within 5s — start one (e.g. `action.jl server`)"
        end
        close(client)
    end

    # Server-only: keep serving until interrupted. (In `both`, the client already ran.)
    if RUN_SERVER && !RUN_CLIENT
        @info "serving /fibonacci — Ctrl-C to stop"
        spin(ctx; handle_signals = true)
    end

    server === nothing || close(server)
end
