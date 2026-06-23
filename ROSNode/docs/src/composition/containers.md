# Containers & Dynamic Composition

A node kind runs at two scales, and its code is identical at both. Deploy-time wiring picks the scale.

## Two scales

Run a node **standalone** or inside a **container**. Both share sessions, discovery, and registry; they differ only in namespacing: a standalone node exposes its entities under its own name, while a container namespaces each node it holds.

**Standalone** nodes run directly — `run(Vehicle; name = "vehicle")` — exposing their entities under the given name.

Several nodes can share one process through a **container**. Each [`add!`](@ref) instantiates a node on the container's single Context, so the nodes share one session, discovery, and registry, plus a direct in-process delivery path between them:

```julia
container("fleet") do c
    add!(c, Vehicle;       name = "vehicle")
    add!(c, GroundStation; name = "ground")
end
```

The same `Vehicle` schema runs either way, its code unchanged.

## Loading a node by name

`node(…; name = "Vehicle")` registers the kind in a process-global registry under that name. A container then loads it by name through [`load_node`](@ref) — the path a `ros2 component load` request drives over the container's `~/_container/load_node` service:

```julia
container("fleet") do c
    load_node(c, "drone", "Vehicle"; name = "vehicle",
              parameters = (var"sensor.rate" = 10,))
end
```

`load_node(c, package, plugin; …)` resolves the kind registered as `plugin`, instantiates it on the container, and returns the node with its unique id:

- `package` — scopes the lookup; may be empty.
- `plugin` — the registered kind name.
- `name` — instance name; defaults to the registered kind name.
- `parameters` — overrides member parameters by their composite `<member>.<field>` names.

The three container operations map onto the ROS2 component verbs:

| Function | Behavior | ROS2 path |
| --- | --- | --- |
| [`load_node`](@ref) | instantiates a registered kind, returns the node and its id | `ros2 component load` |
| [`unload_node`](@ref) | closes a loaded node and drops it from the container | `ros2 component unload` |
| [`list_nodes`](@ref) | returns the loaded `(id, name)` pairs | `ros2 component list` |

### Registration in a precompiled package

`node(…; name=)` registers the kind by one of two paths, because a build-time registration cannot reach the loaded image:

- REPL, script, or runtime: it registers the kind immediately.
- Precompiled package: it skips registration at build time, so the package re-registers its kinds at load, from its `__init__`:

```julia
const Vehicle = node("sensor" => Sensor, "guard" => Guard; name = "Vehicle")

__init__() = register_node_kinds!(@__MODULE__, Vehicle)
```

`@register_nodes Vehicle …` expands to that `register_node_kinds!` call, and a package with its own `__init__` calls `ros_init!(@__MODULE__)` to fold the registration into existing load-time setup.

## Inspecting a composed node

[`describe_wiring`](@ref) (see [Components](components.md)) prints a built node's resolved wiring. The schema-introspection accessors read a `member_schema` or a built node programmatically — the members of a node, the ports a member declares, the message type a port carries:

```julia
ms = member_schema(Sensor)
port_names(ms)                        # [:telemetry, :tick]
message_type(port(ms, :telemetry))   # Telemetry
member_names(Vehicle)                 # [:sensor, :guard]
```

## API reference

```@meta
CurrentModule = ROSNode
```

### Containers

```@docs
Container
container
add!
```

### Node-kind registry

```@docs
register_node_kind!
register_node_kinds!
@register_nodes
ros_init!
node_kind
node_kinds
load_node
unload_node
list_nodes
```

### Schema introspection

```@docs
message_type
port_name
port_names
param_names
port
member_names
```
