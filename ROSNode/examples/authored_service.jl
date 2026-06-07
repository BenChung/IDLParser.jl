# Authored-type service for example_interfaces/srv/AddTwoInts.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/authored_service.jl           # both, one process (default)
#     julia --project=. ROSNode/examples/authored_service.jl server    # serve only (Ctrl-C to stop)
#     julia --project=. ROSNode/examples/authored_service.jl client    # call only (needs a server)
#
# The authored counterpart to service.jl. Rather than `@ros_import`ing the .srv and writing
# a do-block server, we write the service AS a Julia function: parameters are the request
# fields, the `@NamedTuple` return type is the response. `@ros_package "example_interfaces"`
# gives it the wire identity `example_interfaces/srv/AddTwoInts` — same RIHS as the real one —
# so this interoperates with service.jl, a C++/Python `add_two_ints`, or `ros2 service call`.

using ROSNode

# The function body IS the handler. Args in, NamedTuple out; throw to fail the call (the
# client's `call` then raises). Registration (RIHS01 + keyexpr) happens at module load.
module Srv
    using ROSNode
    @ros_package "example_interfaces"
    @ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        @info "request" a b
        (sum = a + b,)
    end
end

# Which half(s) to run: the `ROLE` env var or the first CLI arg, defaulting to `both`.
const ROLE = lowercase(get(ENV, "ROLE", isempty(ARGS) ? "both" : ARGS[1]))
ROLE in ("both", "server", "client") ||
    error("usage: authored_service.jl [both|server|client] (got $(repr(ROLE)))")
const RUN_SERVER = ROLE in ("both", "server")
const RUN_CLIENT = ROLE in ("both", "client")

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")

    # ── Server ──────────────────────────────────────────────────────────────
    # Pass the authored function itself as the marker; ROSNode wires up the request
    # decode → handler → response encode from the registered service type.
    srv = RUN_SERVER ? Service(node, "/add_two_ints", Srv.AddTwoInts) : nothing

    # ── Client ──────────────────────────────────────────────────────────────
    if RUN_CLIENT
        client = ServiceClient(node, "/add_two_ints", Srv.AddTwoInts)
        # Block until routing-matched, so calls only fire once a server is reachable.
        if wait_for_service(client; timeout = 5)
            for b in (40, 41, 42, 43)
                resp = call(client; a = Int64(2), b = Int64(b))   # request fields as kwargs
                @info "response" sum = resp.sum                   # NamedTuple out → 42, 43, 44, 45
            end
        else
            @warn "no /add_two_ints server within 5s — start one (e.g. `authored_service.jl server`)"
        end
        close(client)
    end

    # Server-only role keeps serving until interrupted; other roles fall through to shutdown.
    if RUN_SERVER && !RUN_CLIENT
        @info "serving /add_two_ints — Ctrl-C to stop"
        spin(ctx; handle_signals = true)
    end

    srv === nothing || close(srv)
end
