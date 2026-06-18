# Interface Types

Every topic, service, and action carries a typed message. That type fixes the field layout on the wire and gives the endpoint its identity, so a publisher and subscriber agree on exactly what crosses the channel. The fastest way to get a type is to import an existing interface by name.

## Importing existing interfaces

`@ros_import` bakes an existing interface by its fully-qualified name. The `from="…"` argument adds a local source root, so your own `.msg`/`.srv`/`.action` definitions resolve alongside the vendored tree and a sourced ROS 2 environment.

```julia
@ros_import "std_msgs/msg/String" from="interfaces"
@ros_import "example_interfaces/srv/AddTwoInts" from="interfaces"
@ros_import "action_tutorials_interfaces/action/Fibonacci" from="interfaces"
```

The binding follows the interface kind:

- A **message** binds the bare leaf type, e.g. `std_msgs.msg.String`. Construct it by that name: `std_msgs.msg.String(data = "hello")`.
- A **service** binds the bare namespace, exposing `.Request` and `.Response`. Reference it by `.Request`; ROSNode resolves the Response automatically.
- An **action** binds the bare namespace, which serves as the action handle and exposes `.Goal`, `.Result`, and `.Feedback`.

```julia
# message: bare leaf type
pub = Publisher(node, "/chatter", std_msgs.msg.String)

# service: reference by .Request
srv = Service(node, "/add_two_ints", AddTwoInts.Request) do req
    AddTwoInts.Response(sum = req.a + req.b)
end

# action: bare namespace is the handle; sections by short name
server = ActionServer(node, "/fibonacci", Fibonacci) do goal
    seq = Int32[0, 1]   # elides the per-step extension loop
    feedback!(goal, Fibonacci.Feedback(partial_sequence = seq))
    Fibonacci.Result(sequence = seq)
end
```

One `@ros_import` call handles several interfaces, with these knobs:

- `from=` accepts one directory or a vector of source roots.
- `as Alias` renames a message binding when leaf names would clash. It applies to messages only; a service or action has no single struct to bind, so an `as` on either errors.

Drop `from=` to resolve a name from the vendored tree and the local ROS 2 environment:

```julia
@ros_import "std_msgs/msg/String"   # from a locally installed ROS 2 distro
```

ROSNode searches three sources for each name, in order:

1. the `from=` source roots, when given;
2. ROSNode's vendored interface tree;
3. the local ROS 2 install, scanned in turn: roots registered with [`add_search_path!`](@ref) (highest precedence), then `AMENT_PREFIX_PATH` (a sourced workspace), then `CMAKE_PREFIX_PATH`, then the `/opt/ros/<distro>` system installs (`rolling`, `jazzy`, `kilted`, `lyrical`, `humble`).

Because `from=` roots come first, a local package can supply a new type or override a vendored or environment one.

## Two more ways to get a type

Beyond importing, two paths give you a usable type:

- Author one directly in Julia source with `@ros_message`/`@ros_service`/`@ros_action` — see [Authoring Interfaces in Julia](../advanced/authoring.md).
- Discover one over the wire and resolve it at runtime — see [Runtime Type Discovery](../advanced/discovery.md).

## Wire identity (RIHS01)

A ROS interface's wire type is identified by its name plus its RIHS01 hash, derived from the field layout. Two definitions sharing both the interface name and the RIHS01 hash are the same wire type. This identity is what lets independently defined types interoperate: an authored `@ros_message`/`@ros_service`/`@ros_action` carries the same RIHS01 as the equivalent `.msg`/`.srv`/`.action`, so it speaks the same wire type as its imported twin and as C++/Python ROS 2 nodes.

That shared identity is what makes ROSNode wire-compatible with ROS 2 — see [Interoperating with ROS 2](../interop/ros2.md) for the `ros2` CLI commands that exercise it.

`as` casts a value to a sibling type that shares its RIHS01, copying the field values into the target type:

```@meta
CurrentModule = ROSNode
```

```@docs
as
```

## Next

Carry one of these types onto a channel in [Topics](../communication/topics.md). For how a type's value reaches your handler, see [The Runtime Model](runtime-model.md).
