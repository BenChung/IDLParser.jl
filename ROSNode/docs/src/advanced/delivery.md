# Message Delivery

When a sample arrives, a subscription decodes it and runs your handler. Two keyword options control that handoff: `view` chooses how much of the message you own, and `concurrency` chooses how many handlers run at once.

## View modes

`view` chooses how your handler receives the message — a ladder from full safety to maximum throughput. It is available on `Subscription` and on `Service`.

```julia
Subscription(node, "/chatter", std_msgs.msg.String; view = Owned()) do msg
    @info "heard" msg.data
end
```

- `Owned()` (the default) decodes a fully-owned message — storable, forwardable to other tasks, spawnable, with no lifetime caveats. Pick this when the message outlives the handler: you keep it, hand it to another task, or `@spawn` work on it.
- `Checked()` (also written `view = true`) exposes a zero-copy `CDRView` aliasing the payload, valid for the handler's duration, with a runtime guard that throws when the view escapes the handler. Pick this for high-rate streams you consume in place, while you still want the safety net.
- `Unchecked()` exposes the same zero-copy view with the guard removed, for maximum throughput. Pick this once `Checked()` has proven the view stays put. An escaping view is undefined behavior.

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

## Combining them

The two options compose. A hot, lock-free fan-out owns its own synchronization and reads each sample in place:

```julia
Subscription(node, "/scan", sensor_msgs.msg.LaserScan;
             view = Unchecked(), concurrency = Parallel(4)) do msg
    accumulate!(msg)   # you synchronize shared state across the 4 tasks
end
```

## Quality of service

Both `Publisher` and `Subscription` accept a `qos = default_qos()` profile that governs reliability and history. `default_qos()` carries the settings that match a stock ROS 2 endpoint, which suits most topics.

## Static subscriptions and the fast path

The zero-copy `view` modes pair with a static typed subscription, `Subscription(node, "/topic", T)`, whose known type lets decode alias the payload directly. A dynamic subscription resolves its type per sample and materializes the message; graduating it to a static one unlocks the fast path — see [Runtime Type Discovery](discovery.md).

## See also

- [Topics](../communication/topics.md)
- [Runtime Type Discovery](discovery.md)
