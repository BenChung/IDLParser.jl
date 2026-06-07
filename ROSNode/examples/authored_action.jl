# Authored-type action for example_interfaces/action/Fibonacci.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/authored_action.jl           # both, one process (default)
#     julia --project=. ROSNode/examples/authored_action.jl server    # serve only (Ctrl-C to stop)
#     julia --project=. ROSNode/examples/authored_action.jl client    # send a goal (needs a server)
#
# The authored counterpart to action.jl. The action IS a Julia function: goal fields are
# parameters, a `FeedbackSink{@NamedTuple{…}}` parameter is the feedback channel, and the
# `@NamedTuple` return type is the result. `@ros_package "example_interfaces"` gives it the
# wire identity `example_interfaces/action/Fibonacci` (matching the published RIHS, sections
# and endpoints alike) — so it interoperates with action.jl, a C++/Python server, or
# `ros2 action send_goal /fibonacci example_interfaces/action/Fibonacci "{order: 8}"`.

using ROSNode

# The function body IS the handler, running once per accepted goal. Calling the feedback
# sink `fb((sequence = …,))` publishes a Feedback message AND checkpoints cancellation: if a
# client cancels, it throws here and the goal settles CANCELED. Returning the result
# NamedTuple settles SUCCEEDED; throwing settles ABORTED.
module Act
    using ROSNode
    @ros_package "example_interfaces"
    @ros_action function Fibonacci(order::Int32,
            fb::FeedbackSink{@NamedTuple{sequence::Vector{Int32}}})::@NamedTuple{sequence::Vector{Int32}}
        seq = Int32[0, 1]
        for _ in 1:order
            push!(seq, seq[end] + seq[end-1])
            fb((sequence = copy(seq),))   # publish feedback + cancellation yield point
            sleep(0.3)                    # pretend the step is expensive
        end
        @info "goal complete" seq
        (sequence = seq,)
    end
end

# Which half(s) to run: the `ROLE` env var or the first CLI arg, defaulting to `both`.
const ROLE = lowercase(get(ENV, "ROLE", isempty(ARGS) ? "both" : ARGS[1]))
ROLE in ("both", "server", "client") ||
    error("usage: authored_action.jl [both|server|client] (got $(repr(ROLE)))")
const RUN_SERVER = ROLE in ("both", "server")
const RUN_CLIENT = ROLE in ("both", "client")

# `localhost_only` keeps discovery on the configured router (no multicast); drop it
# to reach peers on other hosts.
@context(peers = ["tcp/localhost:7447"], localhost_only = true) do ctx
    node = Node(ctx, "fibonacci")

    # ── Server ──────────────────────────────────────────────────────────────
    # Pass the authored function as the marker; ROSNode runs its body per goal and drives
    # the accept/feedback/result protocol around it.
    server = RUN_SERVER ? ActionServer(node, "/fibonacci", Act.Fibonacci) : nothing

    # ── Client ────────────────────────────────────────────────────────────────
    if RUN_CLIENT
        client = ActionClient(node, "/fibonacci", Act.Fibonacci)
        # Block until routing-matched to a server — returns `false` on timeout.
        if wait_for_action_server(client; timeout = 5)
            gh = send(client; order = Int32(8))       # goal fields as kwargs; blocks until accepted
            @info "goal" state = state(gh)            # :accepted (or :rejected)

            # Stream feedback as NamedTuples; the loop ends when the goal settles (the
            # framework drives the result behind the scenes).
            for fb in feedback(gh)
                @info "feedback" fb.sequence
            end

            result = fetch(gh)                        # settled — result is a NamedTuple
            @info "result" result.sequence
            @info "final state" state = state(gh)     # :succeeded

            # To cancel a running goal instead: `cancel(gh)` — the server's `fb(…)` call
            # then throws and the goal settles :canceled.
        else
            @warn "no /fibonacci action server within 5s — start one (e.g. `authored_action.jl server`)"
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
