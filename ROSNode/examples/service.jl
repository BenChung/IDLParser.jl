# Service server + client in one process: example_interfaces/srv/AddTwoInts.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/service.jl
#
# Split across processes (or talk to a real ROS2 node) by keeping only the server
# or only the client half. A C++/Python `add_two_ints` server/client interoperates
# given a matching router and domain.

using ROSNode

# `@ros_import` of a fully-qualified service binds the bare `AddTwoInts` namespace —
# a tag type exposing the generated sections by short name: `AddTwoInts.Request` and
# `AddTwoInts.Response` (the wire types stay `AddTwoInts_Request`/`_Response`).
# Reference the service by its `.Request` type; the Response is resolved automatically.
@ros_import "example_interfaces/srv/AddTwoInts"

Context(; peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")

    # ── Server ──────────────────────────────────────────────────────────────
    # The handler receives the decoded request and returns the response message.
    # That return becomes a reply-ok. To fail explicitly, `respond!(req, failed, msg)`
    # — the client's `call` then raises. A throw or a return-without-responding is
    # caught and turned into an error reply, so a buggy handler can't hang a caller.
    srv = Service(node, "/add_two_ints", AddTwoInts.Request) do req
        @info "request" a = req.a b = req.b
        AddTwoInts.Response(sum = req.a + req.b)
    end

    # ── Client ──────────────────────────────────────────────────────────────
    client = ServiceClient(node, "/add_two_ints", AddTwoInts.Request)
    sleep(0.3)  # let discovery match client ↔ server before the first call

    resp = call(client, AddTwoInts.Request(a = 2, b = 40); timeout_ms = 5000)
    @info "response" sum = resp.sum   # → 42

    # `call(client, req; async=true)` instead returns a Task you `fetch` later, so
    # several requests can be in flight at once.

    close(client)
    close(srv)
end
