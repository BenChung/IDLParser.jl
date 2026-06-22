# Component Lifecycle

A component that wraps an external resource — a device, a file, a connection — authors its setup and the matching release as lifecycle hooks. Every node runs the same lifecycle; the `managed` keyword on `run` chooses who drives it.

## The hooks

The five hooks are plain methods on the component type, each node-first `(node, m)` and defaulting to a no-op. Each fires on one transition:

| Hook | Transition | Acquires / releases |
|------|------------|---------------------|
| `configure` | `Unconfigured → Inactive` | acquire resources into the component's state |
| `activate` | `Inactive → Active` | open the working edge (managed nodes gate dispatch automatically) |
| `deactivate` | `Active → Inactive` | close the working edge |
| `cleanup` | `Inactive → Unconfigured` | release what `configure` acquired |
| `on_error` | a thrown transition | recover the state a throwing hook left behind |

The framework brackets the hooks with its own bookkeeping: a member's entities materialise immediately before its `configure`, its (paused) timers start immediately after a successful `activate`, and its entities close immediately after `cleanup`. A hook covers only the state the framework cannot see for you:

```julia
import ROSNode: configure, cleanup, member_schema

mutable struct Recorder{Name} <: Component{Name}
    io::Any                                  # configure opens it, cleanup closes it
end
Recorder{Name}() where {Name} = Recorder{Name}(nothing)
@parameters struct RecorderParams
    path::String = "flight.log"
end

configure(node, r::Recorder) = r.io = open(parameters(node, r).path, "a")
cleanup(node, r::Recorder)   = r.io === nothing || close(r.io)

function record(node, r::Recorder, msg::Telemetry)
    println(r.io, msg.battery)
end
member_schema(::Type{Recorder}) = component(Recorder, RecorderParams,
    hears(:record, Telemetry, record; on = "/vehicle/telemetry"))
```

## Managed and unmanaged nodes

`managed` selects who drives the transitions:

- **Unmanaged (the default).** `run` / `add!` brings the node straight up: construction runs every member's `configure`, then every member's `activate`. `cleanup` runs at teardown — an explicit `close(node)`, a container's [`unload_node`](@ref) (see [Containers & Dynamic Composition](containers.md)), or the Context drain when the node's Context closes.
- **Managed (`managed = true`).** The node declares the lifecycle control surface (the five `lifecycle_msgs` services plus the `~/transition_event` topic) and starts `Unconfigured`. An external orchestrator (`ros2 lifecycle set …`) or in-process calls drive the transitions; `autostart = true` runs `configure!` then `activate!` during construction.

```julia
vehicle = run(Vehicle; ctx = ctx, name = "vehicle", managed = true, block = false)
ln = ROSNode.lifecycle(vehicle)
configure!(ln)      # → Inactive: entities materialise, each member's configure runs
activate!(ln)       # → Active: timers start, dispatch gating lifts
deactivate!(ln)     # → Inactive: members deactivate in reverse order, gating drops back
cleanup!(ln)        # → Unconfigured: each cleanup runs, entities close — a configure! starts fresh
```

These calls and a `ros2 lifecycle` orchestrator drive one machine over `~/change_state`. Each transition runs the members' hooks under the [settlement three-way](../communication/services.md), so it lands its target, declines back to its origin, or diverts into error processing. Click a transition to step through it:

```@raw html
<div class="rosnode-statechart" data-machine="lifecycle"></div>
```

## The dispatch gate

A managed node holds each port shut at dispatch until it reaches `Active`:

| Port | Behavior while not `Active` |
|------|-----------------------------|
| Publisher | the publish drops |
| Subscription, timer | the reaction stays quiet |
| Service | the request gets an error reply |
| Action server | goal, cancel, and result requests get an error reply; feedback and status publications drop |
| Action client (`send`/`fetch`/`cancel`) | the call raises `NodeInactiveError` — probe `isactive(node)` first |

The gate is why most components leave `activate`/`deactivate` as no-ops: those hooks carry only work beyond the gating — pre-rolling a device, flushing a buffer. `deactivate` goes one step further and cooperatively cancels any goals still in flight: each executing body sees the cancel signal and settles `CANCELED`, and a body that ignores it runs detached after a bounded wait, so a node bounced to `Inactive` stops working against state the orchestrator may have invalidated. The control surface, the parameter services, and `~/get_type_description` stay live throughout, so an orchestrator drives transitions and tunes parameters on an inactive node.

Two ports sit outside the gate:

- The **service client** issues every `call` regardless of state and blocks to its own timeout; guard it on `isactive(node)` when that matters.
- An **unmanaged node** has no gate at all: it delivers from the moment a subscription materialises. A message can reach `record` before `configure` assigns `r.io`, so guard the field in the handler, or run the node managed, when that window matters.

## Failure and recovery

A hook signals failure two ways:

- **Returning the `failure` token** rolls the transition back to its pre-transition state. The fan-out stops at the failing member, and the members that already ran the forward hook unwind in reverse dependency order: a failed `activate!` deactivates them and lands `Inactive`; a failed `configure!` cleans them up, closes their ports, and lands `Unconfigured`. The transition reports `:failure`, and error processing is left untouched.
- **Throwing** diverts a managed node into error processing:

```@raw html
<div class="rosnode-statechart" data-machine="recovery"></div>
```

`on_error` runs across the members in dependency order, then:

- every member returns cleanly → the node recovers to `Unconfigured`;
- an `on_error` throws → the node drops to `Finalized` (the throw is logged and the remaining members still run).

Error processing then runs the guarded member-cleanup fan-out — each member's `cleanup` plus its port close — so recovery to `Unconfigured` is a true reset: a later `configure!` reacquires from scratch, with no stale entities still dispatching and no double-acquire. An unmanaged node has no error-processing state — `run` / `add!` tears the partial node down (every member that reached `configure` cleans up against its partial state, entities close) and the throw propagates.

## Ordering and idempotent cleanup

In a multi-member node the hooks fan out in dependency order: `configure`, `activate`, and `on_error` run providers first; `deactivate` and `cleanup` run in reverse. The `Sensor` in [Components](components.md) configures before the `Guard` it feeds, and cleans up after it.

`cleanup` runs at most once per `configure`. Teardown has several triggers — explicit `close`, `unload_node`, the shutdown transition, the Context drain — and a node can see more than one. The member's materialised entities are the guard: the first trigger runs `cleanup`, later ones are no-ops. The framework logs a throwing `cleanup` and continues, so the member's entities still close and the remaining members still clean up.

## API reference

```@meta
CurrentModule = ROSNode
```

```@docs
configure
activate
deactivate
cleanup
on_error
```
