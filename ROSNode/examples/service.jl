# Service server + client for example_interfaces/srv/AddTwoInts.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/service.jl           # both, one process (default)
#     julia --project=. ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
#     julia --project=. ROSNode/examples/service.jl client    # call only (needs a server)
#
# Run `server` and `client` in two processes — or point `client` at a real C++/Python
# `add_two_ints` node — and they interoperate given a matching router and domain.

using ROSNode

# `@ros_import` of a fully-qualified service binds the bare `AddTwoInts` namespace —
# a tag type exposing the generated sections by short name: `AddTwoInts.Request` and
# `AddTwoInts.Response` (the wire types stay `AddTwoInts_Request`/`_Response`).
# Reference the service by its `.Request` type; the Response is resolved automatically.
@ros_import "example_interfaces/srv/AddTwoInts" from="interfaces"

# Which half(s) to run: the `ROLE` env var or the first CLI arg, defaulting to `both`.
const ROLE = lowercase(get(ENV, "ROLE", isempty(ARGS) ? "both" : ARGS[1]))
ROLE in ("both", "server", "client") ||
    error("usage: service.jl [both|server|client] (got $(repr(ROLE)))")
const RUN_SERVER = ROLE in ("both", "server")
const RUN_CLIENT = ROLE in ("both", "client")

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")

    # ── Server ──────────────────────────────────────────────────────────────
    # The handler receives the decoded request and returns the response message.
    # That return becomes a reply-ok. To fail explicitly, `respond!(req, failed, msg)`
    # — the client's `call` then raises. A throw or a return-without-responding is
    # caught and turned into an error reply, so a buggy handler can't hang a caller.
    srv = nothing
    if RUN_SERVER
        srv = Service(node, "/add_two_ints", AddTwoInts.Request) do req
            @info "request" a = req.a b = req.b
            AddTwoInts.Response(sum = req.a + req.b)
        end
    end

    # ── Client ──────────────────────────────────────────────────────────────
    if RUN_CLIENT
        client = ServiceClient(node, "/add_two_ints", AddTwoInts.Request)
        # Block until the client↔server are routing-matched — no sleep. Returns
        # `false` on timeout (e.g. no server up), so we don't fire calls into the void.
        if wait_for_service(client; timeout = 5)
            for b in (40, 41, 42, 43)
                resp = call(client, AddTwoInts.Request(a = 2, b = b); timeout_ms = 5000)
                @info "response" sum = resp.sum            # → 42, 43, 44, 45
            end
        else
            @warn "no /add_two_ints server within 5s — start one (e.g. `service.jl server`)"
        end
        # `call(client, req; async=true)` instead returns a Task you `fetch` later, so
        # several requests can be in flight at once.
        close(client)
    end

    # Server-only: keep serving until interrupted. (In `both`, the client already ran,
    # so fall through and shut down; in `client`, there's nothing to keep alive.)
    if RUN_SERVER && !RUN_CLIENT
        @info "serving /add_two_ints — Ctrl-C to stop"
        spin(ctx; handle_signals = true)
    end

    srv === nothing || close(srv)
end
