# Runtime Type Discovery

A type-less `Subscription` resolves each sample's message type at runtime, so a single subscriber handles a stream whose definition it learns over the wire. This powers generic tooling — recorders, bridges, introspection — over types the tool never imported.

## Resolving a type off the wire

A `Subscription` written without a type argument resolves each sample's type as it arrives:

```julia
sub = Subscription(node, "/chatter") do msg
    @info "heard" typeof(msg.data) msg.data
end
```

`msg` is the real decoded struct with type-stable field access, so `msg.data` is fast and concrete. The framework resolves the wire type in order: the home table, then the type registry, then the project cache, then ament, then a wire `GetTypeDescription` query to the publisher.

The home table is the entry point. `@context` binds the calling module as the Context's resolution home (see [The Runtime Model](../foundations/runtime-model.md)), and a wire type resolves against that module's imported structs first. A module that has imported `Reading` lands *its* `Reading` deterministically; the later stages cover types the home has never seen.

## Caching discoveries across runs

`@ros_cache`, written once in a module, opts into project-local persistence:

```julia
@ros_cache
```

It stores each discovered type description under `<project>/ros_typesupport/`, keyed by RIHS01, records a per-node warm-up manifest under `ros_typesupport/manifests/`, and bakes wire-discovered types into static ones at precompile. Discovery works without it; `@ros_cache` is what makes a discovery stick, so the next run starts from the cached definition.

## Warm-up

The first sample of a new type pays codegen and JIT for the decode→handler chain, a latency spike mid-stream. Warm-up drives that cost off the hot path.

`warmup = :precompile` compiles the decode→handler chain. The node default is `:off`, so opt in either on the node (`Node(...; warmup = :precompile)`) or per-subscription (as on line 66). With `@ros_cache` recording the manifest, a repeat run pre-warms it at startup:

- **Run 1** discovers the type and warms it on first sight.
- **Run 2+** warms at startup from the manifest, so the first sample arrives hot.

`warmup = :execute` additionally runs the handler once on a fabricated sample. A dynamic subscription degrades it to `:precompile` and stays compile-only: synthesizing a sample needs the static type that a discovered handler resolves at runtime.

`warmup_sync = true` blocks construction until the codec is warm — the choice for hard real-time, where the first sample must already be hot.

`@effectful expr` marks a genuine side effect. An `:execute` warm-up run skips the marked expression while still compiling it, so warming a handler with `:execute` records no fabricated data. On a dynamic subscription, which stays compile-only, the guard is inert; marking effects this way keeps them safe the moment a static subscription enables `:execute`.

## A recorder

A generic recorder buffers readings off `/readings` without importing the type. The publisher half stands in for some other node on the graph, publishing `sensor_demo/msg/Reading`; split it into a second process (or a real ROS 2 node) and the subscriber discovers the type over the wire with the same code. This example ships runnable as `examples/warming.jl`. Run it from the ROSNode project so `--project=.` activates the ROSNode environment; the `from = "interfaces"` root resolves against the script's directory, `examples/interfaces/`. Start a router first so both processes meet:

```sh
zenohd -l tcp/localhost:7447 &
julia --project=. examples/warming.jl   # then run it again to see the startup warm
```

```julia
using ROSNode

@ros_cache

@ros_import from = "interfaces" "sensor_demo/msg/Reading"

@context(peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "recorder")

    # type-less: resolves each sample's type at runtime; `warmup = :precompile`
    # warms the decode→handler chain so a repeat run pre-warms it at startup
    buffer = Any[]
    sub = Subscription(node, "/readings"; warmup = :precompile) do msg
        @effectful push!(buffer, (msg.sensor_id, msg.value))
        @info "recorded" sensor = msg.sensor_id value = msg.value n = length(buffer)
    end

    pub = Publisher(node, "/readings", sensor_demo.msg.Reading)
    sleep(0.4)   # let discovery match sub ↔ pub before the first publish
    for i in 1:5
        publish(pub, sensor_demo.msg.Reading(
            sensor_id = "imu_$i", value = 9.8 + i, history = Float64[i, i + 0.5]))
        sleep(0.3)
    end
    sleep(0.5)
    close(sub); close(pub)
end
```

With `@ros_import` present, Run 1 resolves `Reading` from the home table and warms its codec on first sight, recording the type into `ros_typesupport/manifests/_recorder.tsv`. Drop the `@ros_import` and split the publisher into a second process, and Run 1 instead fetches `Reading` over the wire on first sight. Either way, Run 2 replays that manifest at startup and warms the codec, so the first publish lands on a hot path.

## Graduating to a static type

Once a topic's type is known, the framework logs a one-time hint naming the discovered type and the static spelling to graduate to:

```julia
sub = Subscription(node, "/readings", Reading) do msg
    @info "recorded" sensor = msg.sensor_id value = msg.value
end
```

Naming the type drops the per-message `invokelatest` hop and unlocks the zero-copy `view = true` fast path (see [Message Delivery](delivery.md)).

## See also

- [Topics](../communication/topics.md) — publishers and subscribers end to end.
- [Message Delivery](delivery.md) — view modes and concurrency for a typed subscription.

## API reference

```@meta
CurrentModule = ROSNode
```

### Static generation and caching

```@docs
@ros_import
@ros_cache
flush_type_cache
```

### Warm-up

```@docs
@effectful
is_warming
```

### Type description and introspection

```@docs
describe_type
get_type_description
fetch_type_description
resolve_or_discover
wire_get_type_description!
```

### Type support and resolution

```@docs
TypeRegistry
RegistryEntry
register_type!
lookup_type
resolve_type
absorb_static_types!
export_typesupport
enable_project_cache!
disable_project_cache!
ament_prefix_paths
search_prefixes
add_search_path!
discover_ament_packages
load_ament_type
```

### Logging

```@docs
RosoutLogger
logger
Fatal
set_logger_level!
with_rosout
bridge_zenoh_logs!
```
