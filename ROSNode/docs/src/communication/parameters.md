# Parameters

A parameter is a named, typed, live configuration value a node exposes. Peers read it and set it at runtime, and the node sees each change applied to its own typed state.

A `@parameters` schema is the shared contract across the two ends, the way a message or service type is shared across its endpoints. The server bakes the schema in; a client uses it as a typed lens onto the remote.

## The schema

`@parameters` declares the schema as a struct. Each field carries a type, a default, and optional metadata:

```julia
using ROSNode

@parameters struct DemoParams
    "maximum speed"  max_speed::Int64 = 50 ∈ 0..100
    "planner mode"   mode::String = "auto" ∈ ("auto", "manual")
end
```

The annotations on each field shape how peers may drive it:

- A leading string literal is the field's description, surfaced to parameter tooling.
- `∈ lo..hi` constrains a numeric field to that range.
- `∈ (a, b, c)` constrains a field to that choice set.
- `|> readonly` blocks runtime sets; startup overrides still apply.

Legal field types are the ROS 2 parameter value set — `Bool`, `Int64`, `Float64`, `String`, `Symbol`, and `Vector`s of those. The macro generates an immutable struct, so reads stay type-stable.

## The server

`Node(ctx, name, Schema)` attaches a typed parameter server at `node.parameters` and wires the six standard `rcl_interfaces` parameter services plus `/parameter_events`. The node is driveable by any ROS 2 parameter client. `overrides` overlays startup values (CLI, launch, or YAML) onto the schema defaults:

```julia
server = Node(ctx, "client_demo_server", DemoParams; overrides = (max_speed = 50,))
@info "serving" max_speed = server.parameters.max_speed mode = server.parameters.mode
```

`server.parameters` is the live `DemoParams` value; field access reads the current setting, typed.

## The client

`ParameterClient` drives a node's parameters from another node. Pass the schema to share it as a typed lens, so `fetch` and `transaction` are type-stable; drop it to drive a node by its parameter services alone, where values come back dynamically as `Any`/`NamedTuple`:

```julia
node   = Node(ctx, "client_demo")
client = ParameterClient(node, "/client_demo_server", DemoParams)

wait_for_service(client; timeout = 5) || return   # false on timeout, e.g. no server up
```

`wait_for_service` returns once the parameter services are routing-matched, gating the calls below to a live server.

### The low-level verbs

Three verbs map one-to-one onto the `rcl_interfaces` services:

```julia
get_parameters(client, [:max_speed])                       # → Any[50]
get_parameter_types(client, [:max_speed])                  # → [PARAMETER_INTEGER]
ok, reason = set_parameters_atomically(client, [:max_speed => 80])
```

`set_parameters_atomically` returns `(ok::Bool, reason::String)`: the server applies the whole batch together, and `reason` carries the explanation behind a rejection.

### The Julian structure

`fetch` materializes the whole remote into the shared schema in one round trip:

```julia
p = fetch(client)        # ::DemoParams
@info "snapshot" max_speed = p.max_speed mode = p.mode
```

`transaction` mutates the remote with a do-block over a local draft. ROSNode validates the draft locally, then pushes the whole set atomically:

```julia
transaction(client) do d
    d.max_speed = 8
    d.mode      = "manual"
end
```

`set_parameter!` sets a single field. An out-of-range value raises `ParameterRejection`, the same exception a local server set raises, so the failure contract is uniform across local and remote:

```julia
set_parameter!(client, :max_speed, 999)   # out of [0, 100] → raises ParameterRejection
```

`on_parameter_event` watches the remote's changes, firing the do-block on each batch the server publishes to `/parameter_events`:

```julia
on_parameter_event(client) do batch
    @info "changed" batch.changed
end
```

## Cross-field validation

Per-field constraints cover one field at a time. For rules spanning several fields, extend `ROSNode.validate` on the schema and throw `ParameterRejection` when a candidate violates them:

```julia
ROSNode.validate(p::DemoParams) =
    p.mode == "manual" && p.max_speed > 50 &&
        throw(ParameterRejection("manual mode caps max_speed at 50"))
```

The hook runs on every set, server-side and through a client transaction, mirroring ROS 2's `add_on_set_parameters_callback`. A throw rejects the whole atomic set.

## Running it

The `rcl_interfaces` parameter types are vendored, so a parameter example is self-contained: it needs only a router and a stock Julia environment. See [Getting Started](../getting-started.md) for the router. A live `server` is also driveable by `ros2 param get`/`set` against a matching router; see [Interoperating with ROS 2](../interop/ros2.md).

A parameter also attaches to a component: `@param` declares a live, `ros2 param`-driveable parameter on a mixin. See [Components](../composition/components.md).

## See also

- [Interoperating with ROS 2](../interop/ros2.md) — drive a node's parameters from the `ros2 param` CLI.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
@parameters
ParameterServer
ParameterClient
CompositeParameterServer
ParameterDescriptor
ParameterType
parameter_type
descriptors
validate
transaction
setproperties
setproperties!
declared_names
parameter_names
parameter
dynamic_parameters
set_parameter!
get_parameters
get_parameter_types
set_parameters
set_parameters_atomically
list_parameters
describe_parameters
on_parameter_event
ParameterEventBatch
readonly
ParameterRejection
```
