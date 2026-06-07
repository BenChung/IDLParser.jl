# Writing a Service and Client

A service is a request/response call: the client blocks for the reply. The handler settles each request once. A normal return replies success; a failure surfaces to the client as a `ServiceError`.

## Imported interface

`@ros_import` of a fully-qualified service binds the bare `AddTwoInts` namespace, exposing `AddTwoInts.Request` and `AddTwoInts.Response`. Reference the service by its `.Request` type; the Response resolves automatically. The handler receives the decoded request and returns the response message.

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

The client constructs a `ServiceClient` over the same request type, waits for a server to route-match, then calls. `call` blocks for the reply.

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

`wait_for_service` returns `true` once a server route-matches within the timeout, gating the call on a reachable server. For concurrent requests, `call(client, req; async = true)` returns a Task you `fetch` later.

## Authoring the service in Julia

Write the service as a Julia function: the parameters are the request fields, and the `@NamedTuple` return type is the response. `@ros_package` gives it the wire identity `example_interfaces/srv/AddTwoInts` — the same RIHS01 as the imported `.srv` — so an authored server interoperates with an imported client, a C++/Python `add_two_ints`, or `ros2 service call`. The function body is the handler; throw to fail the call.

```julia
using ROSNode

module Srv
    using ROSNode
    @ros_package "example_interfaces"
    @ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b,)
    end
end

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "add_two_ints")

    srv = Service(node, "/add_two_ints", Srv.AddTwoInts)   # the function is the marker

    spin(ctx; handle_signals = true)
end
```

The client passes the authored function as the marker and supplies request fields as keyword arguments; `call` returns a NamedTuple.

```julia
client = ServiceClient(node, "/add_two_ints", Srv.AddTwoInts)

if wait_for_service(client; timeout = 5)
    resp = call(client; a = Int64(2), b = Int64(40))   # request fields as kwargs
    @info "response" sum = resp.sum                     # NamedTuple → 42
end
close(client)
```

## Discovering the service at runtime

The request and response sections resolve off the wire like message types. The home module supplies the resolution context that lets a client reach a service whose type it learned from discovery. See [Interface Types](../interfaces.md) for how that resolution works.

## Running it and handling errors

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is **router-based**. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router. `localhost_only = true` confines discovery to the configured router; the default reaches peers on other hosts. Set `domain_id` to match the ROS domain.

Run the server and client in separate Julia processes pointed at the same router:

```sh
julia --project=. ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
julia --project=. ROSNode/examples/service.jl client    # call only (needs a server)
```

The server settles every request: a normal return replies success, and `respond!(req, failed, msg)` sends an explicit failure. A failed reply raises a `ServiceError` at the client's `call`, surfacing the error where the request was made.

See [Interface Types](../interfaces.md) for authoring and discovering service types.
