# Warm-up with a *dynamic* (runtime-discovered) message type — the D8/D9 story.
#
#     zenohd -l tcp/localhost:7447 &
#     julia --project=. ROSNode/examples/warming.jl      # then run it AGAIN
#
# A type-less subscription resolves its message type at runtime from discovery, so the
# first message of each new type pays codegen + JIT — a latency spike, mid-stream.
# This example shows how ROSNode drives that cost toward zero *across runs*:
#
#   • `@ros_cache` turns on project-local persistence (and static baking).
#   • The dynamic subscription records every type it actually handles into a per-node
#     interaction manifest (under `ros_typesupport/manifests/`).
#   • On the NEXT run that manifest is replayed at startup: each recorded type is
#     resolved and its codec warmed *before the first message arrives*, moving the JIT
#     off the hot path.
#
#   Run 1 — reactive:  discover + warm on first sight (a blip mid-stream).
#   Run 2+ — proactive: warmed at startup from the manifest; the first message is hot.
#
# Self-contained: the publisher half stands in for "some other node" whose headers you
# don't have. Split it into a second process (or a real ROS2 node) and the subscriber
# discovers the type over the wire instead — same subscriber code — and `@ros_cache`
# additionally caches + bakes the wire-fetched definition into a static type for a
# no-codegen next run.

using ROSNode

# Opt into the converge-to-fast loop: persist discovered type descriptions + the
# per-node warm-up manifest under `<project>/ros_typesupport`, and (at precompile) bake
# any cached wire-discovered types into static ones. Without this, discovery still
# works but every run re-pays it — `@ros_cache` is what makes warm-up *stick*.
@ros_cache

# The PUBLISHER half only — imagine this lives in another process. A BYO interface
# (no ROS2 install needed): examples/interfaces/sensor_demo/msg/Reading.msg. The
# type-less subscriber below does NOT import it — that's the point.
@ros_import from = "interfaces" "sensor_demo/msg/Reading"

Context(; peers = ["tcp/localhost:7447"]) do ctx
    node = Node(ctx, "recorder")

    # ── Dynamic subscriber: no compile-time type ────────────────────────────────
    # The type-less `Subscription` resolves each sample's type at runtime (registry →
    # cache → ament → wire `GetTypeDescription`). `msg` is the REAL decoded `Reading`,
    # not a boxed `Any`, so field access is type-stable and fast. `warmup = :precompile`
    # (the node default) warms the decode→handler chain; on a repeat run the manifest
    # pre-warms it at startup. (`:execute` degrades to `:precompile` here — a discovered
    # handler can't be run on a synthesized sample, so dynamic warm-up is compile-only.
    # Pass `warmup_sync = true` to block construction until warm — hard real-time.)
    buffer = Any[]   # a generic recorder doesn't know the message type ahead of time
    sub = Subscription(node, "/readings"; warmup = :precompile) do msg
        # `@effectful` marks the genuine side effect (committing the reading): skipped
        # during a `:execute` warm-up run, but still compiled — so warming never records
        # a fabricated reading. On this dynamic (compile-only) path the handler isn't run
        # at warm-up, so the guard is a no-op; writing effects this way keeps them safe
        # the moment you add `:execute` (e.g. after graduating to a static subscription).
        @effectful push!(buffer, (msg.sensor_id, msg.value))
        @info "recorded" sensor = msg.sensor_id value = msg.value n = length(msg.history) total = length(buffer)
    end

    # ── Publisher (the stand-in remote node) ────────────────────────────────────
    pub = Publisher(node, "/readings", sensor_demo.msg.Reading)
    sleep(0.4)   # let discovery match sub ↔ pub before the first publish

    for i in 1:5
        publish(pub, sensor_demo.msg.Reading(
            sensor_id = "imu_$i", value = 9.8 + i, history = Float64[i, i + 0.5]))
        sleep(0.3)
    end
    sleep(0.5)   # let the dynamic consumer drain the last sample

    @info "done — run this example again to see the type warmed at startup" recorded = length(buffer)
    close(sub); close(pub)
end  # ctx drains: undeclares entities, closes the node and session.

# What landed on disk, under `<project>/ros_typesupport/`:
#   • manifests/_recorder.tsv — the `recorder` node's used (role, type, topic) records.
# Re-run: the dynamic subscription replays that manifest at startup and warms `Reading`'s
# codec before the first publish — the first-message JIT is gone.
#
# The framework also logs a one-time hint naming the discovered type and the static
# spelling to graduate to — `Subscription(node, "/readings", Reading)` — which drops the
# per-message `invokelatest` hop and unlocks the zero-copy `view = true` fast path.
