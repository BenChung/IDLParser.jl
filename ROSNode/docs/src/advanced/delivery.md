# Message Delivery

When a sample arrives, a subscription decodes it and runs your handler. Two keyword options control that handoff: `view` chooses how much of the message you own, and `concurrency` chooses how many handlers run at once.

## View modes

`view` chooses how your handler receives the message — a ladder from full safety to maximum throughput on `Subscription`. A `Service` takes a two-rung Boolean form:

- `view = false` (the default) owns the request.
- `view = true` borrows it zero-copy for the handler's duration.

The snippets below assume a live `node` and the generated message types (`std_msgs.msg.String`, `sensor_msgs.msg.LaserScan`) already in scope.

```julia
Subscription(node, "/chatter", std_msgs.msg.String; view = Owned()) do msg
    @info "heard" msg.data
end
```

- `Owned()` (the default) decodes a fully-owned message — storable, forwardable to other tasks, spawnable, with no lifetime caveats. Pick this when the message outlives the handler: you keep it, hand it to another task, or `@spawn` work on it.
- `Checked()` (also written `view = true`) exposes a zero-copy `CDRView` aliasing the payload, valid for the handler's duration, with a runtime guard that throws `BorrowError` when the view escapes the handler. Pick this for high-rate streams you consume in place, while you still want the safety net.
- `Unchecked()` exposes the same zero-copy view and runs the handler without the borrow guard, for maximum throughput. Pick this once `Checked()` has proven the view stays put. An escaping view is undefined behavior.

The usual progression is to validate under `Checked()`, then switch to `Unchecked()` for the hot path.

## Concurrency

`concurrency` controls how many handlers run at a time.

- `Serial()` (the default) delivers messages in order on one task, so a handler touches shared state without locks.
- `Parallel(n)` fans delivery across `n` tasks; handlers run concurrently and synchronize shared state themselves.

```julia
Subscription(node, "/chatter", std_msgs.msg.String; concurrency = Parallel(4)) do msg
    process(msg)
end
```

## Matching

`match` decides which publishers a subscription accepts by type hash. It is a `Subscription` keyword.

- `ExactMatch()` (the default, also written `match = :exact`) admits only publishers whose type hash matches the subscription's.
- `WeakMatch()` (also written `match = :weak`) additionally admits a publisher advertising the topic under a different or absent type hash, decoding best-effort.

```julia
Subscription(node, "/chatter", std_msgs.msg.String; match = WeakMatch()) do msg
    @info "heard" msg.data
end
```

## Combining them

The two options compose. A hot, lock-free fan-out owns its own synchronization and reads each sample in place:

```julia
Subscription(node, "/scan", sensor_msgs.msg.LaserScan;
             view = Unchecked(), concurrency = Parallel(4)) do msg
    accumulate!(msg)
end
```

## Quality of service

Both `Publisher` and `Subscription` accept a `qos = default_qos()` profile that governs reliability and history. `default_qos()` carries the settings that match a stock ROS 2 endpoint, which suits most topics.

## Static subscriptions and the fast path

Both subscription flavors take the same `view` modes through the identical delivery leaf; they differ only in when the type is resolved:

- a **static** subscription, `Subscription(node, "/topic", T)`, resolves its type once at declaration, so the delivery leaf carries no per-sample type lookup;
- a **dynamic** subscription resolves the type per sample — see [Runtime Type Discovery](discovery.md).

## See also

- [Topics](../communication/topics.md)
- [Runtime Type Discovery](discovery.md)

## API reference

```@meta
CurrentModule = ROSNode
```

### View modes, concurrency, and matching

```@docs
ViewMode
Owned
Checked
Unchecked
Concurrency
Serial
Parallel
MatchPolicy
ExactMatch
WeakMatch
```

### Graph introspection

```@docs
endpoints
publishers_info
subscriptions_info
count_publishers
count_subscribers
topic_names_and_types
node_names
service_is_ready
wait_for_service
wait_for_action_server
wait_for_graph_change
on_graph_change
```

### Delivery events

```@docs
on_type_mismatch
on_qos_incompatible
on_message_lost
TypeMismatch
QosIncompatible
MessageLost
```
