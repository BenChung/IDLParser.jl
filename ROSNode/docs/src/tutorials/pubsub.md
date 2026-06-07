# Writing a Publisher and Subscriber

A topic is a named channel. Publishers send messages on it, and subscribers receive every message that arrives.

This page builds a talker that publishes a `std_msgs/String` on `/chatter` and a listener that prints each one, then authors the message type in Julia and discovers a type at runtime.

## Imported interfaces

`@ros_import` bakes a standard interface by name and lands it at `std_msgs.msg.String` in the calling module. The publisher opens a context, creates a node, and publishes a fresh `String` each second:

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "talker")
    pub  = Publisher(node, "/chatter", std_msgs.msg.String)

    for i in 0:typemax(Int)
        isopen(ctx) || break
        publish(pub, std_msgs.msg.String(data = "hello world $i"))
        sleep(1.0)
    end
end
```

The subscriber runs as a separate process. The do-block handler runs once per message, receiving an owned, decoded `std_msgs.msg.String`. The type-less `Subscription(node, "/chatter")` resolves the concrete type at runtime against the calling module's `@ros_import` closure, so a single import serves both halves:

```julia
using ROSNode

@ros_import "std_msgs/msg/String" from="interfaces"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "listener")

    sub = Subscription(node, "/chatter") do msg
        @info "heard" msg.data
    end

    spin(ctx; handle_signals = true)
end
```

The default `concurrency = Serial()` delivers messages in order on one task; pass `Parallel(n)` to fan out across threads. `spin` parks the main task to keep the process alive while the scheduler delivers each message on its own task, and `handle_signals = true` turns Ctrl-C into a graceful drain.

## Authoring the message in Julia

A hand-written Julia struct marked `@ros_message` becomes a fully registered ROS type, carrying a real RIHS01 identity and keyexpr resolution, with no `.msg` file and no codegen step. `@ros_package` names the package, and authored types compose: `Chatter` references `Header` by its Julia type, and ROSNode hashes that nested ref into `Chatter`'s identity exactly as a `.msg` nested field would be:

```julia
using ROSNode

module Msgs
    using ROSNode
    @ros_package "demo_msgs"
    @ros_message struct Header;  stamp::Float64; frame::String;        end
    @ros_message struct Chatter; header::Header; data::String; seq::Int32; end
end
```

The talker publishes the authored type by its Julia name, passing the type explicitly to resolve it straight from the registry:

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "authored_talker")
    pub  = Publisher(node, "/chatter", Msgs.Chatter)
    sleep(0.4)   # let discovery match sub ↔ pub before the first publish

    for i in 0:4
        publish(pub, Msgs.Chatter(
            header = Msgs.Header(stamp = 1.0 * i, frame = "demo"),
            data   = "hello world $i",
            seq    = Int32(i)))
        sleep(0.5)
    end
end
```

The listener subscribes to the same authored type and reads its nested fields:

```julia
@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "authored_listener")

    sub = Subscription(node, "/chatter", Msgs.Chatter) do msg
        @info "heard" msg.data seq = msg.seq frame = msg.header.frame
    end

    spin(ctx; handle_signals = true)
end
```

The authored RIHS01 matches the equivalent `.msg`, so the type is wire-compatible with imported twins and with C++/Python ROS 2 nodes.

## Discovering the type at runtime

A type-less subscription learns its message type off the wire, so a generic recorder handles a stream whose headers it has never seen. `@ros_cache` adds project-local persistence and static baking, so each run warms the codec faster than the last:

```julia
using ROSNode

@ros_cache

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "recorder")

    # type-less: resolves each sample's type at runtime; `warmup = :precompile`
    # warms the decode→handler chain so a repeat run pre-warms it at startup
    sub = Subscription(node, "/readings"; warmup = :precompile) do msg
        @info "recorded" sensor = msg.sensor_id value = msg.value
    end

    spin(ctx; handle_signals = true)
end
```

Run 1 discovers the type and warms its codec on first sight, recording every type it handles into a per-node manifest under `ros_typesupport/manifests/`. Run 2 replays that manifest at startup and warms the codec, so the first sample arrives hot.

## Running it

ROSNode is built on Zenoh and mirrors `rmw_zenoh`, whose discovery is **router-based**. Start a router once, then point examples at it:

```sh
zenohd -l tcp/localhost:7447 &
```

The `peers = ["tcp/localhost:7447"]` argument to `@context` selects that router. Omit it (and set `localhost_only` / `domain_id` as needed) for environment discovery. `localhost_only = true` confines discovery to the configured router; omit it to reach peers on other hosts.

Run a router, then start the two halves in separate Julia processes pointed at the same router:

```sh
zenohd -l tcp/localhost:7447 &

julia --project=. ROSNode/examples/publisher.jl     # in one terminal
julia --project=. ROSNode/examples/subscriber.jl    # in another
```

Examples that bundle both halves (`service.jl`, `action.jl`) take a role argument to split across two processes — the `ROLE` env var, then the first CLI arg, then `both` by default:

```sh
julia --project=. ROSNode/examples/service.jl server    # serve only (Ctrl-C to stop)
julia --project=. ROSNode/examples/service.jl client    # call only (needs a server)
```

`service.jl`, `action.jl`, and `parameters.jl` each also run both halves in one process, so they're self-contained — just point them at the router.

## Next steps

Define and customize your own messages in [Interface Types](../interfaces.md). Set up a project and router in [Getting Started](../getting-started.md).
