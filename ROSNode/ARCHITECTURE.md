# ROSNode — Architecture

A Julian ROS2 client built on Zenoh. Goals: keep Julian semantics throughout
(handlers are plain `do`-blocks, blocking calls block the *task* not the thread,
cancellation is structured), present multithreaded/async operation elegantly, and
stay byte-compatible with `rmw_zenoh_cpp` (the native ROS2 Zenoh RMW; the hiroz
Rust implementation is the reference). `zenoh-plugin-ros2dds` is a secondary
bridge-compat path.

This document is the settled architecture, told bottom-up. `DESIGN.md` is the
design-discussion history that produced it.

---

## 1. The stack

```
ROSNode          ← this layer: Context, Node, pub/sub, Service, Action, Parameters
ROSZenoh         ← key expressions, liveliness tokens, QoS, entity model, attachment
ROSMessages      ← interface IL, @ros_msg structs, RIHS01 hashes, TypeInfo
CDRSerialization ← CDR codec (owned `read` + zero-copy `read_view`), @cdr1_compat
Zenoh.jl         ← zenoh-c session, payloads, structured serializer, SHM
IDLParser        ← .idl / .msg → AST
```

ROSNode is the top layer: it turns the wire model and transport below it into a
Julian node API. Everything below `ROSNode` already exists.

---

## 2. Primitives

### 2.1 Messages & types (ROSMessages / CDRSerialization)

`ROSMessages.IL` is the canonical interface IR (`RMessage`/`RService`/`RAction`),
above IDLParser's AST. `@ros_msg`/`@ros_msgs` generate Julia types + CDR codec.
Two generated shapes:

- **all-fixed messages** → `@cdr1_compat` structs: isbits, layout == CDR wire, so
  they decode/encode as a single load/store.
- **string/sequence messages** → plain structs decodable two ways:
  `read(r, T)` (owned) or `read_view(r, T)` (zero-copy — `Vector`-of-POD fields
  become `CDRArray` views, strings `CDRString` views, aliasing the buffer).

Every type has a RIHS01 hash; `TypeInfo` = qualified ROS2 name + hash. The IL also
converts to/from `type_description_interfaces/TypeDescription` (`lift` /
`type_description_from_struct`), which is the basis for dynamic type support (§11).

### 2.2 Wire model (ROSZenoh)

- **Key expressions:** `topic_keyexpr` (`<domain>/<topic>/<type>/<hash>` for data)
  and `liveliness_keyexpr` (the `@ros2_lv/**` discovery token), each with inverse
  parsers (`parse_topic_keyexpr`, `parse_liveliness`).
- **Entities:** `NodeEntity` (domain, z_id, id, name, namespace, enclave) and
  `EndpointEntity` (id, node, kind ∈ {Publisher,Subscription,Service,Client},
  topic, `TypeInfo`, `QosProfile`). These are exactly a node's identity.
- **`QosProfile`:** reliability, durability, history, depth, plus deadline,
  lifespan, liveliness (kind + lease) (§12).
- **Attachment:** per-message metadata `(sequence_number::Int64,
  source_timestamp::Int64, source_gid::NTuple{16,UInt8})` carried on every
  `put`/request/reply (§3.4).

### 2.3 Transport & memory (Zenoh.jl)

- **Session** per process region (one per Context, §5); gives the shared `z_id`.
- **Foreign-thread marshalling, already solved:** zenoh-c delivers samples on its
  I/O thread; Zenoh.jl's trampoline stashes the (refcounted) sample and wakes a
  **Julia consumer task** via `uv_async_send`. Handlers run in normal Julia
  context — we never re-solve this.
- **Subscriber forms:** callback (latest-wins single slot, drops under
  backpressure) vs FIFO channel (buffers N samples). Maps to QoS:
  `KeepLast(1)`→callback, `KeepLast(N)`→FIFO capacity N, `KeepAll`→unbounded.
- **Publish/query:** `put`/`reply`/`get` with an `attach=`; SHM via `zref`.
- **Memory primitives** (`Zenoh.jl/src/types/zref.jl`): `zref(sample, T)` (typed
  isbits view, zero-copy tiers + aligned-copy fallback), `Borrowed{T}` +
  `with_memory`/`borrow` (scope-validated views; `BorrowError` on escape),
  `as_memory`/`collect` (owned copy-out), and the `ZSerializer`/`ZDeserializer`
  structured codec used for the attachment.

---

## 3. Data plane: minimum-copy I/O

Both directions avoid the intermediate full-payload `Vector`. The one copy is the
CDR decode into fields (receive) or the single serialization pass (publish).

### 3.1 Receive — owned by default, view opt-in

A handler gets a fully-materialized **owned** message by default — storable,
forwardable, `Threads.@spawn`-able with no lifetime caveats. Correctness first.

```julia
Subscription(node, "/odom", Odometry) do msg … end                 # owned (default)
Subscription(node, "/scan", LaserScan; view = true) do msg … end   # zero-copy view
```

| Message | owned (`read`) | view (`read_view` / `zref`) |
|---|---|---|
| all-fixed (`@cdr1_compat`) | `zref(sample,T)[]` — already zero-copy & escapable | same |
| string/sequence | every field materialized (one copy) | `CDRArray`/`CDRString` alias the payload |

`view = true` is the measured opt-in for large arrays (point clouds, images),
where it aliases the dominant array instead of copying it.

### 3.2 Ownership & escape (`Borrowed` / `with_memory`)

The view path rides Zenoh's `Borrowed{T}`: the dispatcher runs a `view=true`
handler inside `with_memory`, so `msg` aliases the payload for the call. Escaping
the scope is a runtime-checked `BorrowError` (not a use-after-free); to keep data,
`collect`/`as_memory` copies it out. The default owned path needs none of this.

### 3.3 Publish — heap-borrow or SHM-move

```
size (CDRSizeCalculator) → exact buffer → serialize → Zenoh.put
```

`put` **borrows** a heap `Vector` (zero copy into transport; Zenoh pins it until
the deleter fires) or **moves** an SHM segment (`zref(session, T)` makes SHM-vs-heap
transparent). Compact messages skip the field walk (a single store). A buffer pool
is a later zero-steady-state upgrade.

### 3.4 Per-message attachment

`Zenoh.serialize((seq, ts, gid::NTuple{16,UInt8}))` on every `put`/request/reply;
`Zenoh.deserialize(Tuple{Int64,Int64,NTuple{16,UInt8}}, sample)` on receive. The
gid is the **fixed `[u8;16]`** form (no length prefix), matching hiroz. Roles:
`(gid, seq)` is the **service request id** (async reply correlation); `gid` drives
ignore-own-messages; all three populate message-info; and `seq`/`ts` feed
message-lost / lifespan (§12). Encoding + gid-derivation live in ROSZenoh
(`ROSZenoh/TODO.md`).

---

## 4. Runtime model

Julia's scheduler *is* the executor — there's no separate spin loop to pump.

- The consumer task (§2.3) deserializes and runs the user handler as a task.
- **Concurrency is per-endpoint:** `concurrency = :serial` (one at a time,
  preserves order — default) or `:parallel` (`Threads.@spawn` per message).
- A blocking handler never stalls the receive path (it's a task; the FIFO form
  keeps buffering).
- Mutual exclusion *across* endpoints (a sub + timer sharing state) is a shared
  `ReentrantLock` today; a callback-group abstraction is deferred (§15).

---

## 5. Contexts

A `Context` is the process-level container — modeled on hiroz's `ZContext`, on top
of Zenoh.jl's `Config`/`Session`. **One Context = one session; N nodes share it.**
It owns: the session (and `z_id`), an atomic entity-id counter, domain id, default
namespace/enclave, the discovery listener + endpoint index (the graph, §12), the
type registry (§11), and the clock (§7).

**Configuration** is two layers: the Zenoh session config *is* Zenoh.jl's `Config`
(file/env/str + `c[key]=value` overrides), and ROS config (domain, namespace,
remaps, localhost-only, peers) comes from kwargs → env → `--ros-args`, with some of
it translated into the Zenoh `Config` (localhost/peers → scouting/connect).
`domain_id` stays in the keyexprs, not the transport.

**Name resolution** (the ROS-specific layer) turns a user name into an FQN before
it reaches ROSZenoh: absolute `/foo` as-is; relative `foo` → prepend namespace;
private `~/foo` → prepend node FQN; then apply remap rules; validate. ROSZenoh
stays naming-agnostic.

`Context` is the RAII/shutdown root (§14).

---

## 6. Nodes & entities

`Node(ctx, "name"; namespace=…)` inherits the Context's shared state. Entities are
created with **type-constructors, not `open`** — following the `Timer(f,…)` /
`Channel(f,…)` precedent (a registered callback that lives until close, the
opposite of `open`'s scoped RAII):

```julia
pub = Publisher(node, "/cmd_vel", Twist)
sub = Subscription(node, "/odom", Odometry) do msg … end
srv = Service(node, "/arm", SetBool) do req; …; resp end
act = ActionServer(node, "/fly_to", FlyTo) do goal; …; result end
```

The kind is inferred from the type argument. Each handle is `close`-able and dies
with the node; the function is stored and invoked per-event (per the `concurrency`
policy). Constructing an entity materializes a `ROSZenoh.EndpointEntity` and
declares both its liveliness token and its data route.

A convenience `Node("name")` lazily uses a process-default Context (the
`rclcpp::init` analog); explicit `Context` is primary.

---

## 7. Time & clocks

Clock source is a discriminated type set; the discriminator doubles as the time
tag, so cross-clock mixing is a compile-time `MethodError`:

```julia
abstract type ClockSource end
struct System <: ClockSource end   # wall
struct Steady <: ClockSource end   # monotonic
struct ROS    <: ClockSource end   # system, or sim via /clock under use_sim_time
```

`RTime{C}` (named `RTime`, not `Time`, to avoid the `Dates.Time` clash) wraps an
`Int64` ns count; `Duration` is clock-agnostic. `now(node)` → `RTime{ROS}`;
`now(node, Steady())` etc.; `Dates.Period` interoperates.

- `Timer(node, period; clock=ROS()) do … end`, `Rate(node, hz)` + `sleep(rate)`,
  `sleep(clock, dur)` — all clock-aware and **interruptible on shutdown**.
- **Sim time:** `use_sim_time` (a node parameter) routes the ROS clock to a
  Context-hosted `/clock` subscription; a 1 s `ROS()` timer then tracks sim time.
  Jump callbacks fire on activation/discontinuity.
- `header.stamp = now(node)` is explicit and distinct from the attachment's
  `source_timestamp` (publish-time, transport metadata).

---

## 8. Services

A service is one callback with a **write-once result cell**. The common path is
`return` the response; `respond!` is the explicit settle verb (its status token
selects success vs failure). Under rmw_zenoh a failure is a **query error reply**,
so the client's `call` raises rather than getting a plausible zeroed response.

**Fail-safe settlement (the invariant):** a client blocks on the result, so there
is *no non-terminal exit*. Every handler exit fills the cell exactly once — normal
return → success; thrown / `return`-without-responding → a synthesized error reply,
logged. A buggy handler can never hang a caller.

```julia
client = ServiceClient(node, "/arm", SetBool)
resp   = call(client, SetBool.Request(data=true))            # blocks this task; raises on error
fut    = call(client, SetBool.Request(data=true); async=true)
```

---

## 9. Actions

Two layers, mirroring `rclcpp_action::Server`:

- **Low-level primitive** — three callbacks (`on_goal` accept/reject/defer,
  `on_cancel`, `on_accepted`) handing you a per-goal `GoalHandle`. The framework
  owns the goal state machine, result cache, status publication, and fail-safe
  settlement; **you own execution & scheduling**.
- **High-level `do`-block** — sugar for "accept every goal, run to completion": an
  `on_accepted` that spawns the body wrapped in cancellation + settlement,
  scheduled by `concurrency`.

```julia
ActionServer(node, "/fly_to", FlyTo) do goal
    for wp in goal.request.waypoints
        fly_to(wp)
        feedback!(goal, FlyTo.Feedback(...))   # also a checkpoint: throws Cancelled if canceling
    end
    FlyTo.Result(arrived = true)                # → SUCCEEDED (fail-safe settlement as in §8)
end
```

**Cancellation is structured:** `feedback!`/`checkpoint(goal)` throw `Cancelled`
when the goal is canceling — no manual polling, no "stuck in CANCELING". `respond!`
can settle early (release the client) and run detached cleanup.

**Orchestration is a helper, not a server feature.** Queue / pause / single-flight
/ `queued_goals` are components built on `on_accepted` (e.g. a `SingleFlight`
helper), not uniform `ActionServer` API — because no single "queued goals" notion
fits concurrent/serial/preempt at once. Client side: `send` returns a goal handle
that's iterable over feedback, `fetch`-able for the result, and `cancel`-able.

---

## 10. Parameters

Declared parameters are **typed and baked into the node**: `Node("name",
PlannerParams)`. A `@parameters` macro generates an immutable struct (type-stable
reads) + descriptors. Mutation is transactional:

```julia
transaction(node.parameters) do p
    p.max_speed = 9.0
    p.mode      = "manual"
end
node.parameters.max_speed = 9.0                 # sugar: single-statement transaction
```

The live struct sits behind an atomic `Ref`; a transaction builds a candidate,
validates the whole thing, swaps once, then publishes one `/parameter_events` batch
— so readers always see a complete value and rollback is free. Undeclared/dynamic
params live in a separate `node.dynamic_parameters` dict (gated by
`allow_undeclared`, default off). The six standard parameter services +
`/parameter_events` are generic over the struct via reflection.

---

## 11. Type support

**The IL is the hub.** Every way a type enters normalizes into `ROSMessages.IL`;
codegen, hashing, and persistence read from it.

| Direction | Function |
|---|---|
| text ↔ IL | `message_il` / `IL.unparse` |
| IL → AST → code | `lower` → `resolve_constants` → `generate_code` |
| AST → `TypeDescription` | `type_description_from_struct` (+ `calculate_rihs01_hash`) |
| `TypeDescription` → IL | `lift` |

**Acquisition:** colcon/ament (parse installed `share/<pkg>/...`), direct files
(`@ros_msgs`), or **dynamic over the wire**. Dynamic discovery: liveliness gives
`(name, RIHS01)`; on an unknown type, call the remote's `~/get_type_description`,
verify `calculate_rihs01_hash(td) == wire hash`, `lift` the closure, codegen, and
register under `(name, hash)`.

**Runtime registry** keyed by `(name, hash)` (versions coexist). Runtime codegen
runs through the macro pipeline programmatically and is `eval`'d once per type;
because the type is born at runtime it's reached via `invokelatest` — so
**static types are fast, dynamic types are correct-but-boxed**.

**Persist & port forward:** an automatic content-addressed cache (the
`TypeDescription` blob, hash-verified on load) and explicit
`export_typesupport(node, names; format = :msg | :julia | :typedesc)` to graduate a
discovered type into durable form — `:julia` makes it static/fast again. RIHS01 is
the roundtrip invariant.

---

## 12. Discovery, the graph & QoS

One discovery index (the Context's liveliness listener + our injected local
entities) underlies everything here; the detectors and waits are **views over one
change stream**.

### 12.1 Graph query & wait

Symmetric self+remote queries over `EndpointInfo` (node, kind, topic, `TypeInfo`
incl. hash, QoS, gid, `is_local`):

```julia
endpoints(node; topic, kind, …)   # + publishers_info / count_publishers / …
topic_names_and_types(node)       # topic → Vector{TypeInfo}  (>1 ⇒ a type mismatch)
node_names(node)
service_is_ready(client);  wait_for_service(client; timeout)   # graph predicate + wait-on-change
on_graph_change(node) do change; change.added / change.removed end
```

Consistency: our entities are injected immediately (authoritative); remotes are
eventually-consistent via liveliness — which is *why* `wait_for_service` exists.
Symmetry holds at this graph layer only; deeper introspection (a remote's params /
type description) is intrinsically asymmetric — a local accessor for us vs. an
async, fallible client for a remote.

### 12.2 Mismatch detection (views on the change stream)

- **Type mismatch:** compare a matched endpoint's `TypeInfo` to ours — name differs
  (wrong type) or hash differs (wrong *version*, decode-unsafe). Primary signal is
  the graph; a per-sample backstop (`parse_topic_keyexpr`) catches wildcard subs.
- **QoS incompatibility (RxO):** offered must be ≥ requested — `best_effort` pub vs
  `reliable` sub, `volatile` pub vs `transient_local` sub, etc.
- Both dedupe by signature and **throttle** their logs; both also fire programmatic
  events (`on_type_mismatch` / `on_qos_incompatible`) with a fix suggestion.

### 12.3 QoS completeness & events

`QosProfile` carries the full policy set (deadline, lifespan, liveliness + lease;
defaults = ∞/automatic). Events use the uniform `on_*` surface and are sourced from
pieces we already have:

| Event | Sourced from |
|---|---|
| liveliness-changed / -lost | the graph-change stream |
| message-lost | gaps in the attachment `sequence_number` |
| lifespan (enforced: drop expired) | attachment `source_timestamp` + clock |
| deadline-missed (notify only) | a per-endpoint `Timer` |

**Caveat — Zenoh is not DDS:** these are our implementations over
attachment+clock+liveliness generating the ROS2 *event contract*, not inherited DDS
enforcement (deadline notify-only matches ROS2; lifespan we enforce; liveliness
rides the Zenoh tokens). QoS values still travel in the liveliness token for
cross-vendor compatibility. (`encode_qos` completion is a ROSZenoh TODO.)

---

## 13. Introspection (serving ourselves)

The dual of discovery — we *serve* the interfaces others use to inspect us:

- **`~/get_type_description`** answers from the registry (every registered type has
  its `TypeDescription`); the served closure is canonicalized so its RIHS01 matches
  what we advertise.
- **Graph visibility** is the liveliness tokens we already emit (incl. for the
  hidden services).
- **`/rosout`** bridges Julia logging to `rcl_interfaces/msg/Log`.
- **Parameter services** (§10).

---

## 14. Lifecycle & shutdown

### 14.1 Process lifecycle

`spin(ctx)` / `wait(ctx)` **park** the main task until shutdown — they don't pump
callbacks (the scheduler does). One **drain path** handles every shutdown trigger
(`close(ctx)`, signal, `request_shutdown`): stop new work and wake blocked waits
with `ShutdownException` → run `on_shutdown` hooks → await in-flight tasks (incl.
detached action cleanup) up to `drain_timeout` → undeclare entities → close session.
Signals are **opt-in** (`spin(ctx; handle_signals=true)`) so a library embedding us
doesn't steal them; first signal = graceful, second SIGINT = hard exit.

### 14.2 Managed (lifecycle) nodes

A distinct `LifecycleNode` provides the ROS2 managed-node contract: state machine
(`Unconfigured`/`Inactive`/`Active`/`Finalized` as discriminated types), transition
callbacks (`on_configure`/`on_activate`/…), the five `lifecycle_msgs` control
services + `~/transition_event` topic (always live), and an `autostart` convenience.
Transition results reuse the action three-way (return = SUCCESS, `failure()` =
revert, throw = ERROR → `on_error`).

**Gating:** while not `Active`, *all* the node's application entities are gated **at
dispatch** — publishers drop, subscriptions don't fire, timers don't fire, services
error-reply "inactive" — automatically, since every entity knows its node. This
beats rclcpp (which gates only publishers): `on_activate`/`on_deactivate` are
usually empty.

---

## 15. Performance & deferred

### 15.1 Intra-process short-circuit

**On by default within a single Context:** a publisher hands the message directly
to same-Context subscribers, skipping serialize/Zenoh/deserialize (mixed
local+remote → direct to local *and* serialize-once for remote). It reuses existing
knobs — `Locality` (`allowed_origin = remote`) suppresses the Zenoh loopback, and
the **`view` flag is the share/copy choice** (owned default → a copy preserving
mutate-freely; `view=true` → shared read-only). Safer than the cross-process borrow
(GC-backed, so escape is fine; only in-place mutation aliases). Cross-Context
same-process is deferred.

### 15.2 Deferred

- **Callback groups** — cross-entity mutual exclusion is a shared `ReentrantLock`
  today; a thin `CallbackGroup` (membership + shared mutex + dispatch-gating for
  freshest keep-last) is deferred until experience shows the freshness behavior
  matters. Default stays per-endpoint-independent.
- **Cross-Context intra-process** (§15.1).
- **Buffer pool** for zero-steady-state publish (§3.3).

### 15.3 ROSZenoh build tasks

Tracked in `ROSZenoh/TODO.md`: the per-message **attachment** (gid derivation +
wiring) and **QoS encoding** completion (deadline/lifespan/liveliness in
`encode_qos`, byte-for-byte with rmw_zenoh).
