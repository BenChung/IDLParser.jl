# Addressing & Key Expressions

By default, ROSNode mirrors `rmw_zenoh` on the wire: every topic, service, and action maps to a Zenoh **key expression** (keyexpr), and every node and endpoint announces itself with a **liveliness token** that peers read to build their discovery graph. This page covers the machinery behind [`Context`](@ref) and the topic, service, and action constructors — how a name becomes a keyexpr, how the domain scopes it, and which wire dialect spells it.

## Name resolution

[`resolve_name`](@ref) turns a user-supplied name into the fully-qualified name (FQN) a keyexpr is built from, applying ROS 2 name resolution to the three name forms:

- absolute `"/chatter"` — used as-is.
- relative `"chatter"` — prepended with the node's namespace.
- private `"~/chatter"` — prepended with the node's FQN (`namespace` then `name`).

It then normalizes the namespace (a leading slash, no trailing slash; the empty namespace becomes `/`), applies an ordered exact-match `from => to` remap table (empty by default), and validates the result. A valid FQN:

- is absolute — starts with `/`;
- contains no empty `//` token;
- has no trailing slash;
- spells each token with a leading letter or underscore, then letters, digits, and underscores.

## Domain scoping

`domain_id` (the [`Context`](@ref) keyword, falling back to `ROS_DOMAIN_ID`, then `0`) scopes both topic traffic and graph discovery to one domain: two contexts on different domains share a router yet never see each other's endpoints. Under the default `RmwZenoh` dialect it leads every topic keyexpr and liveliness token, so the `@ros2_lv/<domain>/**` discovery subscription itself stays in-domain. Reaching a peer means matching its `domain_id` — including the `ROS_DOMAIN_ID` of stock ROS 2 nodes (see [Interoperating with ROS 2](../interop/ros2.md)).

## Topic key expressions

Under the default `RmwZenoh` dialect, a topic's data keyexpr is four slash-separated components:

```
<domain>/<topic>/<type>/<hash>
```

| Component | Value |
|-----------|-------|
| `<domain>` | the integer `domain_id` |
| `<topic>` | the resolved FQN with its leading slash stripped, internal slashes preserved (`/ns/chatter` becomes `ns/chatter`) |
| `<type>` | the message type in DDS form — `std_msgs/msg/String` becomes the single component `std_msgs::msg::dds_::String_` |
| `<hash>` | the type's RIHS01 identity (see [Interface Types](interface-types.md)) |

A type-less subscription routes on a wildcard data keyexpr (`<domain>/<topic>/**`), so every type published on the topic reaches it; its own liveliness token carries an empty-type placeholder in the `<type>`/`<hash>` slots. See [Runtime Type Discovery](../advanced/discovery.md).

## Discovery: liveliness tokens

Every node and endpoint declares a Zenoh liveliness token under the admin space `@ros2_lv/<domain>/`, and one `@ros2_lv/<domain>/**` subscriber on the [`Context`](@ref) feeds the discovery graph as tokens arrive and depart. The token is slash-structured, so it escapes any `/` inside a component (`<ns>`, `<name>`, `<topic>`, `<type>`) to `%`:

```
@ros2_lv/<domain>/<zid>/<nid>/<eid>/<kind>/<enclave>/<ns>/<name>/<topic>/<type>/<hash>/<qos>
```

The `<enclave>` slot is always `%` (rmw_zenoh does not transmit it), and the QoS profile rides in the final component as a colon-separated field set. ROSNode reads these tokens to answer graph queries — which publishers and subscribers exist on a topic — and to gate type matching.

## Format dialects

`format` (the [`Context`](@ref) keyword) selects the keyexpr dialect:

- `RmwZenoh()` — the default and primary target, matching the native `rmw_zenoh_cpp` RMW (the shapes above).
- `Ros2DDS()` — the `zenoh-plugin-ros2dds` DDS bridge: bare topic-name keyexprs with no type or hash, and `§` as the liveliness slash-escape.

Each dialect interoperates with stock C++/Python ROS 2 nodes running the matching middleware; see [Interoperating with ROS 2](../interop/ros2.md). The ROS 2 middleware-vendor concept is documented upstream at [About Different Middleware Vendors](https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Different-Middleware-Vendors.html).
