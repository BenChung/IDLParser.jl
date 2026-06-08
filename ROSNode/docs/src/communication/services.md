# Services

A service is a request/response call: the client blocks for the reply. The handler settles each request once. A normal return replies success; a failure surfaces to the client as a `ServiceError`.

## Imported interface

`@ros_import` of a fully-qualified service binds the bare `AddTwoInts` namespace, exposing `AddTwoInts.Request` and `AddTwoInts.Response`. Reference the service by its `.Request` type; the Response resolves automatically. See [Interface Types](../foundations/interface-types.md) for how the import binds. The handler receives the decoded request and returns the response message:

```julia
using ROSNode

@ros_import "example_interfaces/srv/AddTwoInts" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")

    srv = Service(node, "/add_two_ints", AddTwoInts.Request) do req
        AddTwoInts.Response(sum = req.a + req.b)
    end

    spin(ctx; handle_signals = true)   # serve until Ctrl-C
end
```

The client constructs a `ServiceClient` over the same request type, waits for a server to route-match, then calls. `call` blocks for the reply:

```julia
using ROSNode

@ros_import "example_interfaces/srv/AddTwoInts" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")
    client = ServiceClient(node, "/add_two_ints", AddTwoInts.Request)

    if wait_for_service(client; timeout = 5)
        resp = call(client, AddTwoInts.Request(a = 2, b = 40); timeout_ms = 5000)
        @info "response" sum = resp.sum   # → 42
    end
    close(client)
end
```

`wait_for_service` returns `true` once a server route-matches within the timeout, gating the call on a reachable server. For concurrent requests, `call(client, req; async = true)` returns a Task you `fetch` later, so several requests stay in flight at once.

## Error replies

The server settles every request. A normal return replies success. `respond!(req, failed, msg)` sends an explicit failure, which raises a `ServiceError` at the client's `call`, surfacing the error where the request was made. A throw inside the handler also becomes an error reply, so every call resolves.

## Next

[Actions](actions.md) extend the request/response shape with progress feedback and cancellation.
