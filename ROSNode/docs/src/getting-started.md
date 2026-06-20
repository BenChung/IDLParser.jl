# Getting Started

ROSNode brings ROS 2 publish/subscribe, services, and actions to Julia over Zenoh, mirroring `rmw_zenoh` on the wire. This page sets up the package, starts a router, and runs the bundled examples.

## Adding ROSNode

ROSNode is a member of this workspace. Import it once its project is active:

```julia
using ROSNode
```

From the workspace root, run any example against the `ROSNode` member project:

```sh
julia --project=ROSNode ROSNode/examples/publisher.jl
```

## Starting a router

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is **router-based**. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

`@context` takes three discovery knobs; see the [`Context`](@ref) docstring for the full catalog:

- `peers = ["tcp/localhost:7447"]` adds connect endpoints, dialing that router.
- `localhost_only = true` disables multicast scouting and defaults the connect endpoint to the loopback router. The default `false` reaches peers on other hosts.
- `domain_id` selects the ROS 2 domain.

Omit all three to discover from the environment.

## Running the examples

Start a router, then run the two halves in separate Julia processes pointed at the same router:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=ROSNode ROSNode/examples/publisher.jl     # in one terminal
julia --project=ROSNode ROSNode/examples/subscriber.jl    # in another
```

The publisher emits a `published` `@info` record once a second, carrying `msg.data = "hello world N"`; the subscriber emits a matching `heard` record for each one as it arrives, carrying `msg.data` and its `String` type.

Examples that bundle both halves (`service.jl`, `action.jl`, `parameters.jl`) take a role argument to split across two processes — the `ROLE` env var or the first CLI arg, defaulting to `both`:

```sh
julia --project=ROSNode ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
julia --project=ROSNode ROSNode/examples/service.jl client    # call only (needs a server)
```

Run each with no argument to drive both halves in one self-contained process: point it at the router and go.

## The runtime model

Every program opens a Context, groups entities under a Node, then spins. See [The Runtime Model](foundations/runtime-model.md) for the full lifecycle.

## Interoperating with ROS 2

ROSNode entities are wire-compatible with real ROS 2 C++/Python nodes. See [Interoperating with ROS 2](interop/ros2.md).

## Next

- [The Runtime Model](foundations/runtime-model.md)
- [Interface Types](foundations/interface-types.md)
