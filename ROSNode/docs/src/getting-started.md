# Getting Started

ROSNode brings ROS 2 publish/subscribe, services, and actions to Julia over Zenoh, mirroring `rmw_zenoh` on the wire. This page sets up the package, starts a router, and runs the bundled examples.

## Adding ROSNode

ROSNode ships as part of this workspace. Activate the workspace project and load the package:

```julia
using ROSNode
```

Run any example with the workspace project active so ROSNode and its dependencies resolve:

```sh
julia --project=. ROSNode/examples/publisher.jl
```

## Starting a router

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is **router-based**. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router. Omit `peers` (and set `localhost_only` / `domain_id` as needed) to use environment discovery. `localhost_only = true` confines discovery to the configured router; omit it to reach peers on other hosts.

## Running the examples

Start a router, then run the two halves in separate Julia processes pointed at the same router:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/publisher.jl     # in one terminal
julia --project=. ROSNode/examples/subscriber.jl    # in another
```

The publisher logs `published "hello world N"` once a second; the subscriber logs a matching `heard "hello world N"` for each one as it arrives.

Examples that bundle both halves (`service.jl`, `action.jl`, `parameters.jl`) take a role argument to split across two processes — the `ROLE` env var or the first CLI arg, defaulting to `both`:

```sh
julia --project=. ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
julia --project=. ROSNode/examples/service.jl client    # call only (needs a server)
```

Run each with no argument to drive both halves in one self-contained process: point it at the router and go.

## The runtime model

Every program opens a Context, groups entities under a Node, then spins. See [The Runtime Model](foundations/runtime-model.md) for the full lifecycle.

## Interoperating with ROS 2

ROSNode entities are wire-compatible with real ROS 2 C++/Python nodes. See [Interoperating with ROS 2](interop/ros2.md).

## Next

- [The Runtime Model](foundations/runtime-model.md)
- [Interface Types](foundations/interface-types.md)
