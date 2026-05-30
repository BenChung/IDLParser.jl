# ROSNode — design notes

The top layer of the stack: a Julian ROS2 client API built on the live Zenoh
session, sitting above `ROSZenoh` (key expressions + entity/liveliness model),
`ROSMessages` (generated structs + RIHS01 hashes + CDR codec), and `IDLParser`.

The goal is to keep Julian semantics throughout while presenting multithreaded
and async operation elegantly — handlers are plain `do`-blocks, blocking calls
block the *task* (not the thread), and cancellation is structured.

This document covers the **node / pub-sub / service / action** surface.
Parameters are designed separately (see "Parameters" below — in progress).

## Layering

```
ROSNode        ← this layer: Node, Publisher/Subscription, Service, Action
ROSZenoh       ← key expressions, liveliness tokens, QoS, entity model
ROSMessages    ← @ros_msg structs, RIHS01 hashes, CDR codec
IDLParser      ← .idl / .msg → AST
CDRSerialization (external) ← CDRReader / CDRWriter
```

Each `open`-equivalent constructor materializes a `ROSZenoh.EndpointEntity`
(allocating an entity id, attaching the node's `z_id`, computing `TypeInfo`
via the `ROSMessages` bridge), then declares both its **liveliness token**
(`liveliness_keyexpr`) and its **data route** (`topic_keyexpr`). Primary target
is `rmw_zenoh_cpp`; `ros2dds` is the secondary bridge-compat path.

## The verb: type-constructors, not `open`

We do **not** overload `Base.open`. Its `open(f, args...)` convention is scoped
RAII (run `f`, then close), but ROS entities are the opposite: a registered
callback that lives until the node dies and fires many times. That clash is a
footgun.

Instead we follow the `Timer(f, …)` / `Channel(f, …)` precedent — constructors
that take a function, return a `close`-able handle, and do **not** auto-close:

```julia
pub = Publisher(node, "/cmd_vel", Twist)                         # no callback
sub = Subscription(node, "/odom", Odometry) do msg ... end
srv = Service(node, "/arm", SetBool) do req; ...; resp end
act = ActionServer(node, "/fly_to", FlyTo) do goal; ...; result end
```

The kind is inferred from the type argument (message vs `Srv` vs `Action`,
which `ROSMessages` distinguishes structurally). Every handle is `close`-able
and also dies with the node. `Node` *itself* is the one place RAII fits:
`Node(f, "name") do n ... end` closes the session and undeclares all entities.

The function passed to a constructor is **stored and invoked per-event,
possibly concurrently** — not run once at construction. This ties into the
`concurrency` policy (callback groups) below.

## Runtime model

The transport is `Zenoh.jl` (zenoh-c via `libzenohc_jll`), and it already owns
the foreign-thread problem so we don't re-solve it: its `@cfunction` trampoline
runs on libzenohc's I/O thread, stashes the (refcounted) sample, and wakes a
**Julia consumer task via `uv_async_send`** — the user callback runs in normal
Julia context. We build the dispatcher on top of that task. So our layer is:

- **Delivery semantics come from the subscriber form.** `open(f, s, k)` is a
  *latest-wins single slot* (drops under backpressure); `open(s, k;
  channel=:fifo, capacity=N)` buffers `N` owned samples. This maps cleanly onto
  QoS: `KeepLast(1)`/best-effort → callback form; `KeepLast(N)` → FIFO capacity
  `N`; `KeepAll` → unbounded channel. So `QosProfile.depth` *is* the capacity.
- **Concurrency policy** layers on the consumer task: `concurrency = :serial`
  (handle inline, preserves per-topic ordering — default) or `:parallel`
  (`Threads.@spawn` per message). The default owned message is independent, so it
  parallelizes/defers freely. Only a `view = true` handler that defers must
  **retain the refcounted `Sample`** (cheap clone, no data copy) for the view's
  lifetime — never copy the bytes out. See min-copy I/O below.
- A blocking handler never stalls the receive path (it's a task, and the FIFO
  form keeps buffering). Shared state across tasks (a goal's status cell) is
  synchronized — see fail-safe settlement.
- Concurrency is **per-endpoint and independent**; mutual exclusion *across*
  endpoints (a sub + timer touching one state) is a shared `ReentrantLock` today
  (a ported callback-group abstraction is deferred — see Open design areas #8).

## Minimum-copy I/O

Built on `Zenoh.jl` + `CDRSerialization.jl`. Both sides avoid the intermediate
full-payload `Vector` copy; the default receive path decodes an owned message
(one copy of variable fields), and the **opt-in view path** (`view = true`) aliases
even a large array field rather than copying it. Publish is always a single CDR
serialization pass.
The wire body is the 4-byte CDR encapsulation header (`CDR_LE`, which `CDRWriter`
emits) + CDR payload; rmw_zenoh metadata (sequence number, source timestamp, GID)
rides in the Zenoh **attachment** (`put`/`reply` `attach=`).

> **Zero-copy large arrays — landed.** The earlier limitation (a big
> variable-length POD array — point cloud / image `data` — having to be copied
> into an owned `Vector`) is **resolved** by the IDLParser/ROSMessages codegen:
> all-fixed messages emit via `@cdr1_compat` (whole-struct compact), and
> string/sequence messages are plain structs that CDRSerialization can decode
> **owned (`read`) or zero-copy (`read_view`)** — the latter returns the field
> set with `Vector`-of-POD fields as `CDRArray` views and strings as `CDRString`
> views aliasing the buffer. So the dominant array is no longer copied; the
> only remaining caveat is the *lifetime* of those views (next section), not a
> copy.

### Receive — view Zenoh memory in place

Zenoh.jl provides the receive primitives (`src/types/zref.jl`); we don't build a
bridge type. The message types and their CDR codec are
`IDLParser`/`ROSMessages`/`CDRSerialization`'s concern and out of scope here.
Two cases:

- **All-fixed (`@cdr1_compat`) message `T`** — `r = zref(sample, T)`; `r[]` *is*
  the message. Zero-copy when the payload is SHM-backed (tier 1) or a single
  contiguous, `T`-aligned network slice (tier 2); otherwise one aligned copy
  (tier 3). `isborrowed(r)` asserts the fast path was taken. The value is plain
  bits — inherently escapable, no lifetime ceremony.
- **String/sequence message** — wrap the (contiguous/SHM) payload as
  `Memory{UInt8}` (`as_memory`/`unsafe_memory`, a `DenseVector{UInt8}`) → `MemBuf`
  → `CDRReader`, then choose per the handler's escape needs:
  - **`read_view(r, T)`** — zero-copy: `Vector`-of-POD fields become `CDRArray`
    views and strings `CDRString` views aliasing the payload. The dominant array
    is never copied. The views **alias the buffer**, so they're valid only while
    the backing owner (`Borrowed`/`Sample`/SHM) is alive — exactly the ownership
    model below governs this.
  - **`read(r, T)`** — owned: every field materialized (the array is copied into
    its `Vector`). Escapable anywhere, at the cost of that copy.

  **The default is owned (`read`)** — a handler gets a fully-materialized message
  it can store, forward, or `Threads.@spawn` over with zero lifetime caveats;
  correctness and ergonomics first. A handler **opts into the zero-copy view**
  (`Subscription(node, topic, T; view = true) do msg … end`) when it has *measured*
  it needs it — large arrays, high rate — and then `msg` aliases the payload and is
  governed by the ownership model below. (Compact `@cdr1_compat` messages are the
  happy middle: `zref`'s `r[]` is owned-by-value *and* zero-copy, so fixed messages
  pay nothing on either path. Sequence elements that can't be aliased — `string[]`,
  jagged structs — fall back to owned decode *inside* `read_view` automatically.)

### Ownership & escape: this is what `Borrowed` / `with_memory` are for

This machinery backs the **opt-in view path** (`view = true`); the default owned
path needs none of it. The borrowed bytes are valid only for a scope, and Julia
has no borrow checker — so Zenoh's `Borrowed{T}` makes the scope a runtime-checked,
value-first view. The verbs:

- `with_memory(sample, T; writable) do b … end` — calls the body with a
  scope-validated `Borrowed{T}`, then `close`s it. Zero-copy (tiers 1/2) or a
  one-time copy (tier 3). The body *is* the scope.
- `borrow(sample, T; writable)` — the manual (un-scoped) form; you `close(b)`.
- `as_memory(z)` / `collect(b)` — copy out into owned memory that may escape.
- `writable=true` — `b` owns a private mutable copy (`b.field = v`, `b[i] = v`).
- `unsafe_with_memory` / `unsafe_memory(b)` — uninstrumented raw `Memory{T}`, no
  per-access checks, for hot paths once the access pattern is proven correct.

How the **view path** uses them, realizing "in-scope ⇒ no copy; escaping ⇒ copy":

1. **The `view=true` handler body is the `with_memory` scope.** The dispatcher
   runs the user's `do msg … end` inside `with_memory`; read-compute-republish
   never copies. (The default owned handler skips this entirely — it just gets a
   `read`-decoded message.)
2. **`Borrowed` gives the loud-error escape trap for free.** A `Borrowed` (or a
   pointer from it) that escapes the body is `close`d on exit; any later use throws
   `BorrowError` instead of reading freed memory — no flag of our own to build.
   The safety net for capturing the *borrowed* value into an untyped escape
   (`Ref{Any}`, a global).
3. **Escape = copy, explicitly.** To keep data past the call, `collect(b)` /
   `as_memory` (whole-payload) or materialize the message into its owned form.
   The copy is incurred at that call, while the borrow is still live. Typed
   boundaries (a `Channel{Msg}`, a service `Response` return, a node-state field
   typed `Msg`) route through `convert` to make this automatic at exactly the
   escape point.

So the view path is scoped + zero-copy with `BorrowError` guarding escapes — all
on Zenoh's `Borrowed`. **You reach for it only after measuring;** the default
owned message is the everyday path and carries none of these caveats.

*Big POD arrays:* zero-copy receipt of a large array field (point cloud `data`,
image `data`) is the motivating reason to opt into `view = true` — the field comes
back as a `CDRArray` aliasing the payload, governed by the `Borrowed` lifetime;
`collect`/`read` materialize it to escape. The default still copies it once into
an owned `Vector`, which is the right call until profiling says otherwise.

### Publish

Zenoh's `ZRef{T}` is the send handle, and `zref(s, T)` makes SHM-vs-heap
**transparent**: it allocates from the session's SHM provider when available and
falls back to a Julia box otherwise — same handle either way, `put` borrows the
box (`z_bytes_from_buf`, pinned until the deleter fires) or *moves* the SHM
segment (`z_bytes_from_shm_mut`). Zero copy into the transport on both paths.

- **Compact (isbits) message** — `r = zref(s, WireOf{T}); r[] = wire; put(pub, r)`,
  where the wire value is the 4-byte CDR encapsulation header + the struct
  (`r[] = …` is a single `unsafe_store!`/memcpy; `pointer(r)` writes the SHM
  segment in place). No CDR field walk.
- **Variable message** — size pass (`CDRSizeCalculator`, exact length) →
  serialize header + body once with `CDRWriter` into a session buffer →
  `put`/`ZBytes`. The size pass means one exact allocation, no IOBuffer growth or
  `take!`.

Because the heap path *borrows*, the buffer must outlive transmission — a fresh
exact-size buffer per publish is correct (Zenoh pins it; the deleter unpins). A
per-publisher **buffer pool** reclaiming in the deleter is the zero-steady-state
upgrade, deferred until profiling asks for it. (The earlier open question about
writable in-place SHM serialization is resolved: `zref(provider, T)` writes
straight into the segment via `pointer`.)

### Services & queries — same escape model

Same owned-by-default rule as a subscription: the request is `read`-decoded
(owned) unless the service opts into `view = true`, where it's borrowed from the
`Query` for the handler scope. A synchronous handler that reads `req`, computes,
and returns a `Response` is the common case. A **detached/async response** (settle
the write-once cell from another task) is an escape — with the default owned `req`
it just escapes freely; with a view it must `collect`/`as_memory` first — and
either way the framework retains the owned `Query` until `reply` (filling the
result cell triggers it). Response: size → serialize → `reply` borrows the buffer,
same as `put`.

### Per-message attachment

rmw_zenoh's per-sample metadata — `(sequence_number::Int64, source_timestamp::Int64,
source_gid::NTuple{16,UInt8})` — rides in the Zenoh **attachment** on every
`put`/request/reply, a separate channel from the liveliness/discovery tokens. With
Zenoh.jl's structured serializer (`src/types/serializer.jl`, landed) it's a
one-liner each way:

```julia
attach = Zenoh.serialize((seq, ts, gid))                                  # gid::NTuple{16,UInt8}
put(pub, payload; attach)                                                 # or reply(q, …; attach)
(seq, ts, gid) = Zenoh.deserialize(Tuple{Int64,Int64,NTuple{16,UInt8}}, sample)
```

**Byte-exactness pivot — the gid is `NTuple{16,UInt8}`, not `Vector{UInt8}`.**
Zenoh.jl distinguishes the two zenoh shapes: `Vector{UInt8}` is length-prefixed
(`serialize_buf` / `Vec<u8>`), `NTuple{N,UInt8}` is fixed-width with **no** length
prefix (`[u8; N]`). hiroz serializes `source_gid: [u8; 16]` — the fixed form — so
the attachment is 8 + 8 + 16 = 32 bytes with no prefix on the gid; using
`Vector{UInt8}` would inject a spurious length byte and break parity. (Worth a
byte-diff against a real rmw_zenoh capture, since this is *the* exactness point.)

Signaling roles: `(source_gid, sequence_number)` is the **service request id** that
correlates async replies; `source_gid` also drives ignore-own-messages; all three
populate `rmw_message_info`. With the serializer landed, the only remaining
ROSZenoh work for #3 is the **gid derivation** (`z_id` + entity id → 16 bytes,
matching hiroz) and wiring these two calls into the publish/receive/service paths.

## Services

A service is the degenerate case: one callback, a write-once **result cell**,
no feedback. The common path is `return`:

```julia
arm = Service(node, "/arm", std_srvs.srv.SetBool) do req
    req.data ? engage_motors() : disengage_motors()
    SetBool.Response(success = true, message = req.data ? "armed" : "disarmed")
end
```

Explicit `respond!` is for settling a *failure*. Under rmw_zenoh the service is
a Zenoh queryable, so a failure becomes a **query error reply** — the client's
`call` raises rather than receiving a plausible zeroed `Response`:

```julia
takeoff = Service(node, "/takeoff", drone_srvs.srv.Takeoff) do req
    if !is_armed()
        respond!(req, failed, "cannot take off while disarmed")   # → query error reply
        return                                                     # cell full; return ignored
    end
    climb_to(req.altitude)
    Takeoff.Response(reached = current_altitude())                 # → reply-ok
end
```

Client side blocks the *calling task* (it yields); a failed reply raises:

```julia
client = ServiceClient(node, "/arm", std_srvs.srv.SetBool)
resp   = call(client, SetBool.Request(data = true))                # raises on error reply
fut    = call(client, SetBool.Request(data = true); async = true)  # Future-like
resp   = fetch(fut)
```

## Actions

An action server has **two layers**, mirroring `rclcpp_action::Server`:

- a **low-level primitive** — three callbacks (`on_goal` / `on_cancel` /
  `on_accepted`) that hand you a per-goal `GoalHandle` you drive yourself. The
  framework owns the goal **state machine**, the **result cache** (so result
  requests are answered and status is published), and **fail-safe settlement** —
  but **not execution or scheduling**. This is rclcpp's shape exactly: its
  `handle_accepted` gives you the handle and *you* decide whether to run it now,
  queue it, or hand it to a scheduler; the canonical example just spawns a thread.
- a **high-level `do`-block** — sugar over the primitive for the overwhelmingly
  common "accept every goal and run it to completion" case (what the examples
  below use).

It's also **two objects with two lifetimes**: the long-lived `close`-able
*server*, and a per-goal *`GoalHandle`* — the thing you `feedback!`, settle, and
observe cancellation on.

ROS2's goal state machine has three decision points, not one:

```
                on_goal(req)
   (arrives) ─────────────────┐
                │ reject()     │ accept()
                ▼              ▼
            [rejected]      ACCEPTED ──────────────┐
                              │  │                 │ cancel + on_cancel→accept()
                              │  ▼                 ▼
                              │ EXECUTING ───► CANCELING
        do-block returns ─────┘  │  │                │
        Result(...)→ SUCCEEDED    │  │ abort()/throw → ABORTED
                                  │  └ canceled()    → CANCELED
```

`on_goal` returns accept / reject / **defer** (accept but don't start executing
yet — the low-level path uses defer to queue), `on_cancel` returns accept/reject.
In the high-level form both **default to accept-all**, appearing only when you
need to *deny* something — and then a synchronous decision point is honest
because the protocol requires a prompt reply.

Crucially, `on_cancel` accepting a cancel does **not** stop anything — it moves
the goal to `CANCELING` and arms the cancellation token. The execute block must
observe it. We make that observation structured (next section), so the classic
"stuck in CANCELING forever" bug can't happen on the default path.

### Cancellation: thrown `Cancelled`

`feedback!` and `checkpoint(goal)` are yield points that **throw `Cancelled`**
when the goal has been moved to `CANCELING`. No manual polling, no forgetting:

```julia
fly = ActionServer(node, "/fly_to", drone_actions.action.FlyTo;
    on_goal     = req  -> is_armed() ? accept() : reject(),   # defaults shown
    on_cancel   = goal -> accept(),
    concurrency = :serial,
) do goal
    wps   = goal.request.waypoints
    total = path_length(wps)
    try
        for (i, wp) in enumerate(wps)
            fly_segment_to(wp)
            feedback!(goal, FlyTo.Feedback(                    # also a checkpoint
                remaining = total - path_length(@view wps[1:i]),
                current   = position(),
            ))
        end
        FlyTo.Result(arrived = true, where = position())       # → SUCCEEDED
    catch e
        e isa Cancelled || rethrow()                           # real errors → fail-safe ABORTED
        respond!(goal, canceled, FlyTo.Result(arrived = false, where = position()))
        descend_and_disarm()    # detached: client already released; checkpoint() now a no-op
    end
end
```

### `respond!` and the write-once cell

`respond!` is the single settlement verb across services and actions. Its
status token selects between two outputs with different cardinality:

- the **result** — a write-once *cell* (`succeeded` / `canceled` / `aborted`)
- **feedback** — a *stream*, zero-or-more (`respond!(goal, feedback, fb)`;
  `feedback!(goal, fb)` is sugar)

The return value is a *second syntax* for filling the cell, disambiguated by:

> `respond!` is authoritative. The return value fills the cell **only if it is
> still empty.** Once filled, the return value is *ignored* (not an error).

That "ignored, not error" choice is deliberate: it lets detached cleanup end on
an incidental expression (`descend_and_disarm()`'s return) without blowing up.
The only hard error is an *explicit* second terminal `respond!`.

### Fail-safe settlement (the key invariant)

A client blocks on the result, so **there is no non-terminal exit.** The
framework guarantees the cell is filled exactly once before the handler Task is
reaped — "forgot to respond" and "threw an exception" collapse into one
fail-safe `ABORTED`:

| Handler exit | Cell empty → framework fills | Log |
|---|---|---|
| `return payload` | `succeeded(payload)` | — |
| threw `Cancelled` | `canceled(default)` | — |
| threw anything else | `aborted(default)` + exception | `@error` + backtrace |
| `return nothing`, never responded | `aborted(default)` | `@error "exited without responding"` |
| cell already full | — (ignored) | cleanup errors logged, can't re-settle |

The wrapper is layered so the guarantee holds even if recovery itself fails:

```julia
try
    v = userblock(goal)
    isfilled(goal) || fill!(goal, succeeded, v)
catch e
    isfilled(goal) || fill!(goal, e isa Cancelled ? canceled : aborted, default_result(goal))
    e isa Cancelled || @error "goal handler failed" exception=(e, catch_backtrace()) goal.id
finally
    isfilled(goal) || force_abort!(goal)   # infallible: bare ABORTED + zeroed result bytes
end
```

`force_abort!` must not throw — it sends the status enum + zero-filled result
bytes directly, not via the user message constructor, so even a non-defaultable
result type can't wedge the client.

For services the same applies, but the wire shape differs: `aborted`/`failed`
maps to a **query error reply**, so a buggy handler surfaces as a failed `call`
on the client, not a plausible empty success.

This draws a clean line:

- **Settlement failure** (handler *exits* unsettled) — solved here; cannot hang
  a client.
- **Liveness failure** (handler *never exits*) — a watchdog/deadline concern,
  not settlement. We do **not** default an execution timeout (long missions are
  normal). Guards: client-side result timeout + cooperative `cancel`. An
  optional per-goal `deadline=` on the server stays opt-in.

### Low-level: the goal-handle primitive

The high-level `do`-block is *defined in terms of* the primitive — it's an
`on_accepted` that spawns the body wrapped in the `Cancelled` + fail-safe
machinery, scheduled by `concurrency`:

```julia
# LOW-LEVEL — you own execution & scheduling (rclcpp's three-callback shape).
server = ActionServer(node, "/fly_to", FlyTo;
    on_goal     = req  -> accept(),          # accept / reject / defer
    on_cancel   = goal -> accept(),
    on_accepted = goal -> begin              # you take the GoalHandle; run it now,
        …                                    # queue it, hand to a scheduler — your call
    end)
# GoalHandle verbs: feedback!(goal, …), checkpoint(goal) / iscancelled(goal),
#   succeed/abort/canceled (= respond!), goal/status queries, execute(goal).

# HIGH-LEVEL — sugar over the above:
ActionServer(node, "/fly_to", FlyTo; concurrency = :serial) do goal … end
```

Who owns what: the **framework** owns the goal state machine, result cache, status
publication, and the fail-safe-settlement backstop (a `GoalHandle` dropped
unsettled still aborts, so the client never hangs even on the low-level path).
**You** own when/whether/where each accepted goal runs. `concurrency = :serial |
:parallel` is the *only* scheduling the high level bakes in (one-at-a-time vs
task-per-goal — rclcpp's two common cases); anything richer is the next rung.

### Orchestration is a helper, not a server feature

Queue / pause / single-flight / preempt / `queued_goals` are **not** uniform
`ActionServer` features — and rclcpp doesn't provide them either, for the reason
you'd expect: there's no single notion of "queued goals" that fits concurrent,
serial, preempt, *and* custom scheduling at once. They're components built on
`on_accepted`:

```julia
sched  = SingleFlight(queue = 4)             # an orchestrator over the low-level API
server = ActionServer(node, "/fly_to", FlyTo; on_accepted = g -> submit!(sched, g))
execute!(sched) do goal
    …                                        # checkpoint(goal) also blocks while sched is paused
end
pause(sched; reason = "low battery"); resume(sched)
queued_goals(sched)                          # introspection belongs to the orchestrator
```

`SingleFlight` (one active goal + bounded queue — the common one-actuator pattern)
can ship as an *optional* helper, but it's *a* policy, not *the* server. Even
**pause reuses `checkpoint`**: the orchestrator flips its own paused flag and
`checkpoint(goal)` blocks on it, so pause too is the helper's feature, not the
server's. `queued_goals` / `active_goal` are the orchestrator's, with whatever
semantics that orchestrator defines — which is exactly why they don't belong on a
universal `ActionServer`.

### Action client

The goal handle is iterable over feedback and awaitable for the result:

```julia
client = ActionClient(node, "/fly_to", drone_actions.action.FlyTo)
goal   = send(client, FlyTo.Goal(waypoints = mission))    # returns once accepted/rejected
state(goal) === :rejected && error("controller refused the goal")

@async for fb in feedback(goal)        # streams until terminal
    @info "remaining $(round(fb.remaining; digits=1)) m"
end

Timer(30.0) do _                       # cancel if it runs long
    state(goal) === :executing && cancel(goal)
end

result = fetch(goal)                   # blocks this task until terminal; reflects final state
```

## Shutdown / drain

Because `respond!` can release a client while the Task keeps running (detached
cleanup), `close(server)` and `close(node)` must **await outstanding goal Tasks**
— including post-cancel cleanup — up to a `drain_timeout`, then hard-interrupt.
The server tracks live goal Tasks specifically so it can join them; otherwise a
node shutdown mid-"descend and disarm" would drop the vehicle.

## Parameters

Declared parameters are **typed and baked into the node**: `Node("name",
PlannerParams)`. The schema is an immutable struct behind an atomic `Ref`;
dynamic (undeclared) parameters live in a separate, explicit side dict.

### Declaration: the `@parameters` macro

One annotated declaration generates the immutable struct (plain field types →
type-stable reads), a `descriptors(::Type)` method, and the reflection glue.
Consistent with `ROSMessages`' `@ros_msg`:

```julia
@parameters struct PlannerParams
    "max horizontal speed (m/s)"
    max_speed::Float64 = 5.0   ∈ 0.0..20.0
    "flight mode"
    mode::String      = "auto" ∈ ("auto", "manual")
    waypoint_topic::String = "/wp" |> readonly
end
```

Legal field types are restricted to what ROS2's `ParameterValue` tagged union
can carry — `Bool`, `Int64`, `Float64`, `String`, and `Vector` of those — plus
`Symbol`/enum as sugar over string-with-choices. Anything else is rejected at
macro-expansion with a clear error, rather than silently coerced.

Construction overlays overrides (CLI `--ros-args -p`, launch, YAML) onto the
struct defaults, coerces to field types, and validates the result. Overriding a
`readonly` param at startup is allowed — read-only only blocks *runtime* sets.

### Mutation: transactions are the primitive

Every mutation is a transaction; the do-block form just lets you batch:

```julia
# Primitive — batch with logic, commits atomically on clean exit.
transaction(node.parameters) do p
    p.max_speed = 9.0
    p.mode      = "manual"
    p.cruise    = p.max_speed * 0.8       # reads see pending values
end

# Sugar — implicit single-statement transactions:
node.parameters.max_speed = 9.0
setproperties!(node.parameters, (max_speed = 9.0, mode = "manual"))   # partial overlay
setproperties!(node.parameters, PlannerParams(max_speed = 9.0))       # full struct → replace-all
```

`setproperty!`/`setproperties!` are effectful setters (idiomatic Julia) routed
through the same commit path. A **namedtuple** is a partial overlay; a **full
`PlannerParams` value** is replace-all (set every field) — dispatched on the
argument type.

Transaction semantics, which fall out of immutable-struct-behind-a-`Ref`:

- Inside the block, `p` is a mutable draft (base + accumulated overrides);
  assignments don't touch the live cell and publish nothing yet.
- **Clean exit** → build candidate via `setproperties(base, overrides)`,
  validate the *whole* candidate (per-field descriptors + the user's
  `validate(::PlannerParams)` for cross-field rules), one atomic swap, one
  batched `/parameter_events` message, fire on-change once.
- **Any throw** (including a raised rejection) → abort. Rollback is free —
  the candidate is dropped; the live cell never held a partial state.

That swap-once discipline is also the **consistency boundary**: a reader racing
a transaction derefs either the whole old struct or the whole new one, never a
half-applied set — which matters under the foreign-thread dispatch. `validate`
is exactly ROS2's `add_on_set_parameters_callback`, expressed as a method on the
candidate.

Rejection asymmetry: an internal Julia caller gets an **exception** (the
`transaction` block throws). An external ROS client's set arrives through the
service, which catches the rejection and returns
`SetParametersResult(successful=false, reason=...)` on the wire — same shape as
service-handler-error → error-reply.

### Dynamic (undeclared) parameters

Kept deliberately separate and explicit, so the typed struct and its reads stay
fully type-stable (no `Any` union leaking into `node.parameters.field`):

```julia
node.dynamic_parameters[:gain] = 2.0     # Any-typed; only when allow_undeclared=true
```

- Gated by `allow_undeclared` (default **off**, per rclcpp). When off, the field
  **vanishes** — `node.dynamic_parameters[:x]` throws "undeclared params
  disabled", so a typed-only node has zero dynamic surface.
- Routing on set: `name ∈ fieldnames(P)` → typed struct path; otherwise → the
  dict. Declared names always win; a dynamic param can't shadow a declared one.
- The two-tier split surfaces the type-stability tradeoff honestly: declare it
  for speed + safety, or accept `Any` in the dict for flexibility.

### Protocol surface

The 6 standard parameter services (`describe` / `get_types` / `get` / `list` /
`set` / `set_atomically`) + `/parameter_events` are **generic over the schema**
via `fieldnames`/`fieldtypes` + `descriptors()`, implemented once. They reflect
over the *union* of declared fields and the dynamic dict — ROS2 presents one
flat namespace, so a client never sees the tiering. Wire-atomic sets that span
both tiers commit struct + dict jointly under the node's single
parameter-mutation lock.
```
## Type support

### The IL is the hub

Everything converges on `ROSMessages.IL` — a ROS-level interface IR
(`RMessage`/`RService`/`RAction` over `RBase`/`ArraySpec`/`RField`/`RConstant`),
distinct from and *above* IDLParser's `Parse.Decl` AST. The conversions all
exist today:

| Direction | Function |
|---|---|
| ROS text → IL | `message_il` / `service_il` / `action_il` |
| IL → ROS text | `IL.unparse` |
| IL → IDL AST | `lower(il; package)` |
| IDL AST → code | `ConstResolution.resolve_constants` → `Generation.generate_code` |
| AST → `TypeDescription` | `type_description_from_struct` (+ `calculate_rihs01_hash`) |
| `TypeDescription` → IL | `lift` |

Every way a type can enter normalizes into the IL; codegen, RIHS01 hashing, and
persistence all read from it. "Where the type came from" stays orthogonal to
"how we use it."

### Acquisition front-ends

Three entries, equivalent once parsed to IL:

1. **colcon / ament (static).** Discover installed interface packages via the
   ament index (`AMENT_PREFIX_PATH` → `share/<pkg>/{msg,srv,action}/*`), feed the
   files to `@ros_msgs`. The "I'm in a sourced workspace" path — resolves a
   package/type name to its definition with no wire traffic.
2. **Direct files / inline (macro).** `@ros_msg` / `@ros_msgs` over
   `.msg`/`.srv`/`.action` files or directories (exists today; `@ros_msgs` already
   walks directories).
3. **Dynamic over the wire.** A type we've never seen, fetched at runtime (below).

### Dynamic discovery

rmw_zenoh liveliness carries only the type **name** + RIHS01 **hash** (ROSZenoh
`parse_liveliness` → `TypeInfo`), never the definition. So:

```
observe endpoint (ROSZenoh) → (type_name, RIHS01)
  ├─ in registry?  → use it
  └─ unknown:
       call <remote>/get_type_description   (type_description_interfaces/srv/GetTypeDescription)
         → TypeDescriptionMsg (main + referenced closure)
         → VERIFY calculate_rihs01_hash(td) == wire hash      ← integrity gate, on the raw td
         → lift main + each referenced → IL.RMessage          ← ROSMessages.lift
         → lower(il; package) → resolve_constants → generate_code → eval   (closure together)
         → register under (name, RIHS01)
         → optionally persist
```

Bootstrapping is clean: `GetTypeDescription` is itself a well-known interface
(`type_description_interfaces`, compiled statically), so we use the known type to
fetch unknown ones. Availability: the type-description service is default-on in
Jazzy+, absent on older distros — fallbacks, in order: (a) ament/static lookup by
name; (b) a definition carried out-of-band (Foxglove-style concatenated `.msg`
text / schema, when present); (c) deliver raw bytes + log "no typesupport for
`<name>`/`<hash>`".

### `lift` (landed) and what it does/doesn't carry

`ROSMessages.lift(::TypeDescription) -> IL.RMessage` is the inverse mapping, and
it's in place. Two properties that shape the runtime:

- **Lossy in exactly the RIHS01-excluded ways:** no constants, no field defaults,
  `wchar`→`char`. That's *fine* for our purposes — the verify-gate hashes the raw
  received `TypeDescriptionMsg` (not the lifted IL), and a lift→lower→`type_-
  description_from_struct` roundtrip reproduces the same RIHS01 precisely *because*
  the hash ignores the dropped parts. So decode correctness and hash parity hold;
  only human-facing constants/defaults are absent from a discovered type.
- **One `RMessage`, refs preserved:** `lift` returns the main type only;
  nested types survive as `RRef`s in its fields. So codegen must `lift` the
  **referenced closure too** (`td.referenced_type_descriptions`), `lower` each with
  its package (recovered from each `type_name`), and `generate_code` the lot
  together so the refs resolve. A **service/action** is several wire types
  (`Foo_Request`/`Foo_Response`/…), each its own `(name, hash)` — discovered
  per-section and assembled.

### Runtime registry & dispatch

A `TypeRegistry` keyed by `(type_name, RIHS01)` — not name alone, so an evolved
type's new hash coexists with the old (different remotes, replayed bags), and the
hash stays the decode-safety key. Each entry holds the IL, the wire
`TypeDescription` blob (source of truth for persistence), a provenance tag
(`:static`/`:ament`/`:wire`/`:cache`), and the generated `Module` once realized.
Guarded by a lock — discovery runs from the dispatch/consumer task.

Generation is the existing chain — `lower(il; package)` →
`ConstResolution.resolve_constants` → `Generation.generate_code` → `eval` the
resulting `Expr`s into a fresh module — run once per `(name, hash)` and cached in
the entry. (This is the same pipeline `@ros_msg` runs at macro time, just called
programmatically, so no new codegen entry point is needed — that earlier fork is
closed.) Because the type is born at runtime, it's in a newer world age than the
compiled dispatcher, so the decode + handler call cross the boundary via
`Base.invokelatest` (one dynamic-dispatch + invokelatest hop per message on
dynamic topics). This is the **static = fast, dynamic = correct-but-boxed** line,
made concrete: a runtime type forgoes the compact/min-copy fast path until it's
ported to static.

**Discovery is proactive.** We already parse liveliness, which announces an
endpoint (with its `name`+`hash`) before/around its data — so we kick off
`GetTypeDescription` on *endpoint sighting*, not on first sample, and the type is
usually registered before data flows. Until an entry is ready, the subscription
buffers up to its QoS depth then drops-with-log (consistent with `KeepLast`), or
delivers raw bytes if the handler opted into that — never blocks the consumer task.

### Persisting & porting forward

Two audiences, two mechanisms — the split is deliberate: **our cache is automatic;
exporting into the user's world is always explicit.**

**Our cache (automatic, machine-local).** A content-addressed store keyed by
RIHS01 (configurable dir, default a Julia scratchspace), holding the wire
`TypeDescription` blob. Discovery checks it before the network; a successful fetch
writes it back. Self-validating on load: recompute `calculate_rihs01_hash` and
discard on mismatch, so a stale/corrupt entry can never decode wrong. Codegen is
cheap, so we cache the *definition*, not generated code, and regenerate on load.

**Porting forward (explicit, user's discretion).** `export_typesupport(node,
names; to, format)` graduates discovered types out of the ephemeral registry into
durable form the user owns:

- `:msg` / `:srv` / `:action` — `IL.unparse` the IL into a ROS interface-package
  layout. Most portable: `colcon build` turns it into a real package, and other
  ROS nodes/tools get it too. (Caveat: a *discovered* type lost its constants and
  defaults through RIHS01/`lift`, so an emitted `.msg` carries fields only — fine
  for wire compatibility, lossy as documentation.)
- `:julia` — emit `Generation.generate_code`'s output as `.jl` source to check in
  and `include`. The strongest "port forward": the once-dynamic type becomes
  static, precompilable, **type-stable, and back on the min-copy fast path**.
- `:typedesc` — the raw `TypeDescription` bundle; language-agnostic, reloadable by
  us, shareable.

RIHS01 is the roundtrip invariant across all three — a type frozen and reloaded
hashes identically (ROSZenoh checks wire parity), so an export reloads as if it
had always been static, and our static types can equally be *emitted* for others.

**The graduation lifecycle** this enables:

```
wire (dynamic, boxed/slow)
   → registry entry (IL + generated module)
   → cache  (our fast restart; still dynamic)
   → export :julia/:idl at user discretion
   → static type next build  (fast, type-stable, no network)
```

So a system is "trained" by running once against the real ROS graph, exporting the
discovered types, and shipping them static — dynamic discovery becomes a
development-time convenience rather than a runtime dependency.

### Forks

- **Until-ready policy** (buffer-to-depth / raw-passthrough / drop) — lean: QoS-
  consistent buffer-then-drop, raw opt-in.
- **Cache scope** — per-user scratchspace (shared across projects) vs project-local
  `.ros_typesupport/` (reproducible, vendorable). Lean: scratchspace default,
  project-local opt-in.
- **Service/action discovery assembly** — each section is its own wire `(name,
  hash)`; lifting them into a single `RService`/`RAction` (vs. registering loose
  section messages) is an assembly step on top of `lift`. Lean: assemble, so the
  ROSNode service/action API sees one type.
- **Discovered-type ergonomics** — `lift` drops constants/defaults, so a runtime
  type has none. Acceptable (they're not on the wire), but worth surfacing if a
  handler expects message constants on a dynamically discovered type.

## Type-mismatch detection

A wrongly-typed peer on one of our topics is detectable because type identity
travels on the wire in two ROSZenoh-modelled places. Two complementary detectors:

### Liveliness/graph — the primary, authoritative detector

Every endpoint's liveliness token carries `(type_name, RIHS01)` (`parse_liveliness`
→ `TypeInfo`). The node's discovery listener already watches `@ros2_lv/**`; we
index endpoints by `(domain, topic, kind)` and, when a complementary remote
endpoint (pub↔sub, client↔service) matches one of ours on the topic, compare its
`TypeInfo` to ours:

- **name differs** → wrong type.
- **name equal, hash differs** → same type, *different version* (fields
  added/reordered) — decoding would be unsafe; this is the case a name-only check
  misses and RIHS01 catches.
- **both equal** → match.

This fires **before any data flows**, mirrors what the ROS2 graph reports, and is
the primary mechanism — because rmw_zenoh's data keyexpr embeds type+hash, a
*concrete-typed* subscription never receives a mismatched publisher's data
(non-intersecting keyexpr), so the graph is the only place the mismatch surfaces.

### Per-sample keyexpr — backstop for wildcard subscriptions

The data keyexpr is `<domain>/<topic>/<type>/<hash>`; `ROSZenoh.parse_topic_keyexpr`
(landed — the inverse of `topic_keyexpr`, returning `(; domain_id, topic,
type_info)`) recovers type+hash from a received sample, which we compare to
expected, and on mismatch **warn and refuse to decode** (foreign bytes decoded as
our type are garbage or a CDR error). This only triggers if a subscription
wildcards the
type/hash segments — see the fork below.

### Reporting

- **Dedupe**: warn once per `(remote endpoint id, observed TypeInfo)` — graph
  churn must not spam.
- **Programmatic, not just logs**: an `on_type_mismatch` event on the
  subscription/node (the offending endpoint + expected vs observed `TypeInfo`), so
  a user can react, plus surfacing in graph introspection.
- **Placeholder hashes**: Humble-era / type-erased peers report the zero /
  `TypeHashNotSupported` placeholders ROSZenoh already models — fall back to
  name-only comparison and flag reduced confidence.

### Fork: concrete vs wildcard subscription keyexpr

- **Concrete (default, ROS2-correct).** Subscribe with our exact type+hash; only
  matching data routes to us (incompatible types simply don't exchange, as ROS2
  intends). Detection is liveliness-only; a mismatched/evolved publisher shows up
  as a graph warning + "no messages."
- **Wildcard/observe (opt-in).** Subscribe `…/*/*` to receive all data on the
  topic; enables per-sample detection and, combined with dynamic discovery, lets
  us decode an *evolved* publisher's version on purpose (cross-version interop).
  Costs the per-sample keyexpr check and slightly more traffic.

Lean: concrete by default with liveliness warnings; wildcard/observe as an opt-in
for diagnostics or deliberate cross-version interop.

## QoS-incompatibility detection

The other classic ROS2 toestub — "best-effort publisher, reliable subscriber, no
data, no error." Same machinery as type-mismatch: QoS rides in the liveliness
token (ROSZenoh `encode_qos`/`decode_qos`), so on an endpoint match in the Context
graph we compare profiles. The rule is DDS **request-vs-offered (RxO)**: a
subscription *requests*, a publisher *offers*, and the **offered must be at least
as strong as the requested** per policy.

### The compatibility rules

| Policy | Order (weak→strong) | Incompatible when |
|---|---|---|
| reliability | `best_effort` < `reliable` | sub requests `reliable`, pub offers `best_effort` |
| durability | `volatile` < `transient_local` | sub requests `transient_local`, pub offers `volatile` |
| history / depth | — | never (local resource policy, not part of RxO) |
| deadline / liveliness | RxO too | modeled later — ROSZenoh currently defaults these |

So we check **reliability** and **durability** (the two ROSZenoh carries and the
two that actually bite). Both match directions:

- our **subscription** ↔ remote **publisher**: requested = ours, offered = theirs.
- our **publisher** ↔ remote **subscription**: offered = ours, requested = theirs.

> **rmw_zenoh nuance.** QoS is in the liveliness token, *not* the data keyexpr —
> so detection is liveliness-only (like type-mismatch's primary path), and a
> mismatched pub/sub still keyexpr-match, so data may actually flow where DDS would
> have stayed silent. We warn on the ROS2 RxO rule regardless, because that's the
> user's mental model and the behavior they'll compare against.

### Make noise — and say how to fix it

Each warning names the topic, the offending policy with offered-vs-requested, the
*consequence*, and a concrete **fix** — leading with the side the user controls
(their own endpoint) and offering the remote-side alternative:

```
[QoS] /scan: this subscription requests reliability=:reliable but publisher
      '/lidar_driver' offers :best_effort → you will receive no messages.
  Fix: relax this subscription to best-effort —
       Subscription(node, "/scan", LaserScan; qos = QosProfile(reliability = :best_effort))
  or configure the publisher to publish reliably.

[QoS] /map: this subscription requests durability=:transient_local but publisher
      '/map_server' offers :volatile → late-joining / latched data won't arrive.
  Fix: set durability = :volatile here, or make the publisher transient_local
       (a latched publisher).
```

Also surfaced programmatically as an `on_qos_incompatible` event (offending
endpoint + the policy diff), symmetric to `on_type_mismatch`.

### Rate-limiting (required)

QoS mismatches churn — a topic with 50 mismatched subscribers, or a flapping peer,
must not emit 50× or spam on every reconnect. Two layers:

- **Dedup by signature** `(local endpoint, remote endpoint, policy, offered,
  requested)` — the same distinct incompatibility never re-warns, as with
  type-mismatch.
- **Throttle window per signature** (à la `RCLCPP_WARN_THROTTLE`, default ~5 s):
  first occurrence logs immediately; further hits in the window are *counted and
  suppressed*; when the window rolls, a one-line "+N suppressed in the last 5 s"
  closes it out. The `on_qos_incompatible` event still fires per occurrence (it's
  the log that's throttled, not the programmatic signal).

## Graph query & wait

The client-facing read side of the discovery graph — `wait_for_service`, counts,
endpoint info, graph-change events. It's a read API over the **Context's discovery
index** (the same one the detectors above use), with our own entities injected. It
is **symmetric**: a query spans our endpoints and discovered remotes uniformly,
and the self/other distinction leaks into the surface only as an `is_local` flag
and a freshness difference. (Symmetry holds at this *graph* layer; deeper
introspection — a remote's parameters or type description — is intrinsically
asymmetric, a local accessor for us vs. an async, fallible client for a remote, and
is *not* papered into this API. Purely-local state like an action orchestrator's
`queued_goals` has no remote analog at all.)

### The atom: `EndpointInfo`

One record per endpoint, from a liveliness token (`parse_liveliness`) or injected
for our own:

```julia
struct EndpointInfo
    node_name :: String
    namespace :: String
    kind      :: ROSZenoh.EndpointKind         # Publisher | Subscription | Service | Client
    topic     :: String                        # resolved FQN
    type      :: Union{TypeInfo, Nothing}      # name + RIHS01 hash; nothing if EMPTY_TOPIC_TYPE
    qos       :: QosProfile
    gid       :: NTuple{16,UInt8}
    is_local  :: Bool                          # injected (ours) vs discovered (remote)
end
```

Carrying the **RIHS01 hash** and **gid** (not just a type-name string, as rclcpp's
`TopicEndpointInfo` does) lets a caller run its own type/QoS compatibility check
with the exact data the auto-detectors use.

### Queries

One filterable primitive, rclcpp-named conveniences over it, and aggregate views:

```julia
endpoints(node; topic=nothing, node_name=nothing, kind=nothing, local=nothing) :: Vector{EndpointInfo}

publishers_info(node, topic)  :: Vector{EndpointInfo}     # + subscribers_info
count_publishers(node, topic) :: Int                      # + count_subscribers / _services / _clients

topic_names_and_types(node)   :: Dict{String, Vector{TypeInfo}}   # + service_names_and_types
node_names(node)              :: Vector{NodeInfo}                 # (name, namespace, enclave)
```

- **A topic → `Vector{TypeInfo}` (not one type) is deliberate**: `length > 1` *is*
  the type-mismatch situation surfaced through the graph. Introspection and the
  mismatch detector are the same data, two views.
- **Topic arguments FQN-resolve** through the node's namespace + remap (the
  Contexts resolver), exactly as `Subscription` resolves them; absolute names are
  used as-is.
- Queries return an **immutable snapshot** (`Vector` copied out under the Context
  lock) — a consistent instant, not a live alias.

### Readiness & waiting

```julia
service_is_ready(client)            :: Bool      # non-blocking graph predicate
wait_for_service(client; timeout)   :: Bool      # block calling task until a matching server appears
wait_for_matched(endpoint; timeout) :: Bool      # generalization (a pub waiting for a sub, …)
```

`wait_for_service` is *graph predicate + wait-on-change*: is there a `Service`
endpoint matching the client's topic **and type hash**? If not, block the calling
task (it yields) on the graph-change condition until it is, or timeout. Instant for
our own server; the real-latency case for a remote. **Interruptible by shutdown**
(`ShutdownException`) so a wait on a server that never appears can't wedge
`close(ctx)`.

### Graph-change events

```julia
ev = on_graph_change(node) do change            # change.added / change.removed :: Vector{EndpointInfo}
    …
end
close(ev)
wait_for_graph_change(node; timeout)             # block until the next change
```

`GraphChange` carries `added`/`removed` `EndpointInfo` (richer than rclcpp's bare
"something changed" tick — free, since the listener already sees each token
appear/disappear). The structural point: **this is the substrate the detectors sit
on.** `on_type_mismatch` and `on_qos_incompatible` are *filters* over this change
stream (a change that forms an incompatible match); `wait_for_service` is a
*predicate-wait* over it. One discovery index, several views — not a parallel
mechanism.

### Consistency model

- **Eventually-consistent for remotes, immediate for self.** Our endpoints are
  injected at creation (authoritative, no liveliness round-trip) and removed at
  `close`; remotes lag by discovery latency. So a point-in-time `count_publishers`
  can under-report a just-started remote — which is *why* `wait_for_service` exists
  rather than trusting a count.
- **Self de-dup.** Our own liveliness loops back through the same channel; entries
  key on `(z_id, entity id)`, so the loopback merges into the injected entry
  (`is_local = true`), never double-counted.
- **`type === nothing` tolerated.** Humble-era / type-erased peers advertise
  `EMPTY_TOPIC_TYPE` → `nothing`; queries degrade to name-only, as mismatch
  detection does.
- Updates run on the discovery consumer task; change callbacks fire as tasks under
  the usual `concurrency` policy.

## Introspection — serving ROS2's interfaces about ourselves

Discovery is symmetric: we built the *consumer* side (read peers' liveliness,
fetch their type descriptions, detect mismatches); being a proper ROS2 node means
*serving* the same interfaces about us. The IL hub and parameter model make most
of it nearly free.

### `~/get_type_description` server — the dual of dynamic discovery

Stand up a per-node service on `<node>/get_type_description`
(`type_description_interfaces/srv/GetTypeDescription`). A peer that sees our topic
with a hash it doesn't know calls it; we answer from the registry, where every
registered type already has its IL and thus its `TypeDescription`
(`type_description_from_struct`). This closes the loop with our consumer path —
the definition we serve hashes to exactly the RIHS01 we advertise.

One correctness point: **hash parity requires a canonical closure.** ROSMessages
notes `referenced_type_descriptions` are not auto-sorted, so the server must
canonicalize (sort) the closure so `calculate_rihs01_hash` of what we serve
matches the hash in our liveliness tokens. Bootstrapped by the statically
compiled `type_description_interfaces`.

### Graph visibility

Already implied by the design: each endpoint declares a liveliness token
(`liveliness_keyexpr`) and the node declares its node token — that's what puts us
in `ros2 node/topic/service list` and rqt_graph. The hidden services (parameter
services, `get_type_description`) must themselves declare endpoint liveliness so
tools see them as services.

### `/rosout` logging

Bridge Julia logging to ROS: install an `AbstractLogger` that tees records to a
node-owned `/rosout` publisher (`rcl_interfaces/msg/Log`), tagged with node name +
stamp + mapped severity. Opt-in by level. Makes our logs visible to
`ros2 topic echo /rosout` / rqt_console alongside the rest of the system.

### Parameter introspection

The six parameter services + `/parameter_events` (see Parameters) are the
parameter half of self-introspection — already designed.

## Open design areas (not yet specified)

Captured so scope is honest. Items marked ✓ have since been designed in their own
sections.

1. **Time & clocks.** ✓ Designed — see **Time & clocks**.
2. **Process lifecycle / spin.** ✓ Designed — see **Process lifecycle &
   shutdown** (`spin` parks, one drain path, opt-in signals, `on_shutdown`).
3. **QoS-incompatibility detection.** ✓ Designed — see **QoS-incompatibility
   detection**.
4. **Multiple nodes per process / context.** ✓ Now designed — see **Contexts**:
   one session per `Context`, N nodes share it (plus `z_id`, domain, entity-id
   counter, discovery listener, type registry, clock).
5. **Managed (lifecycle) nodes.** ✓ Designed — see **Managed (lifecycle) nodes**
   (distinct `LifecycleNode`, gate-everything-at-dispatch, the five control
   services + `transition_event`, autostart convenience).
6. **Intra-process short-circuit.** ✓ Designed — see **Intra-process
   short-circuit** (on by default, same-Context; `view` knob = share/copy,
   `Locality` for dedup; transparent).
7. **Graph query / wait API.** ✓ Designed — see **Graph query & wait** (unified
   `EndpointInfo` queries, `wait_for_service`, graph-change events; subsumes the
   mismatch detectors as views over one change stream).
8. **Callback-group granularity.** ~ Deferred pending experience. `concurrency =
   :serial|:parallel` stays per-endpoint and independent (default unchanged — *not*
   node-wide-serial, to avoid "why is my camera blocked behind my IMU?"). Cross-
   entity mutual exclusion (a sub + timer sharing state) is **served today by a
   shared Julia `ReentrantLock`** — idiomatic and sufficient:
   ```julia
   lk = ReentrantLock()
   Subscription(node, "/imu", Imu) do m; @lock lk update!(filter, m) end
   Timer(node, Dates.Millisecond(20)) do; @lock lk publish(pub, estimate(filter)) end
   ```
   *Porting caveat:* unlike rclcpp's default (one node-wide mutually-exclusive
   group), our endpoints are independent — shared state across endpoints must be
   locked (or grouped) explicitly. A thin `CallbackGroup(node)` (declarative
   membership + shared mutex + **dispatch-gating** — a member doesn't dequeue until
   the group frees, so it processes the *freshest* keep-last sample instead of a
   stale held one) is the only thing it'd add over a raw lock. Build it only if
   that freshness behavior proves to matter in practice; the lock pattern is the
   answer until then. Note: rclcpp's reentrancy-to-avoid-executor-deadlock
   motivation is moot here — blocking calls yield the task.
9. **QoS completeness + events.** ✓ Designed — see **QoS completeness & events**
   (full `QosProfile`; events sourced from the graph stream / attachment `seq` /
   clock; Zenoh-isn't-DDS caveat; `encode_qos` completion is a ROSZenoh TODO).

## Contexts

A `Context` is the process-level container nodes live in — modelled on Hiroz's
`ZContext`, sitting on Zenoh.jl's `Config`/`Session` for transport and adding the
ROS-specific naming layer on top. This is also the answer to "multiple nodes per
process": **one Context = one session; N nodes share it.**

### What a Context owns (and every node inherits)

Mirrors `ZContext` field-for-field, mapped onto our stack:

| Hiroz `ZContext` | Ours | Notes |
|---|---|---|
| `session` | `Zenoh.Session` | one per context; gives the shared `z_id` |
| `counter` | atomic entity-id allocator | nodes *and* endpoints draw ids from it (ROSZenoh `id` fields) |
| `domain_id` | `Int` | goes into keyexprs (ROSZenoh), **not** the Zenoh config |
| `namespace` | `String` | default node namespace |
| `enclave` | `String` | rmw_zenoh writes the `%` placeholder (ROSZenoh models it) |
| `graph` | discovery listener + endpoint index | one `@ros2_lv/**` sub per context; feeds type-mismatch + dynamic discovery |
| `remap_rules` | name remapping | see below |
| `shm_config` | session SHM provider | `zref(s, T)` transparent SHM rides on this |
| `keyexpr_format` | `ROSZenoh.RmwZenoh` (default) / `Ros2DDS` | |
| `clock` | node clock | ties into the (open) time subsystem |
| — | **type registry** | per-context, so all nodes share discovered types |

`Node(ctx, "name"; namespace=…)` clones these into the node (Hiroz `create_node`),
overriding name/namespace as needed. The node's identity (`domain_id`, `z_id`,
`id`, `name`, `namespace`, `enclave`) is exactly `ROSZenoh.NodeEntity` — the
Context populates it.

### Configuration: sit on Zenoh.jl, add ROS on top

Two layers, kept separate:

- **Zenoh session config = Zenoh.jl `Config`.** We reuse it wholesale —
  `Config(; file=… / from_env=… / str=…)` plus the `c["key"] = value` JSON5
  override API. Build priority mirrors Hiroz: explicit `Config` → config file →
  `ZENOH_SESSION_CONFIG_URI` → a **default ROS session config** (peer mode →
  local zenoh router `tcp/localhost:7447`), with `ZENOH_CONFIG_OVERRIDE` merged.
- **ROS config = new, context-level.** `domain_id`, `namespace`, `enclave`,
  `remap_rules`, localhost-only, discovery range, static peers — sourced from
  explicit kwargs, then env (`ROS_DOMAIN_ID`, `ROS_NAMESPACE`,
  `ROS_LOCALHOST_ONLY`, `ROS_AUTOMATIC_DISCOVERY_RANGE`, `ROS_STATIC_PEERS`), then
  `--ros-args`. **Some ROS config maps *into* the Zenoh `Config`** via the same
  `c[key]=value` API: localhost-only / discovery-range → scouting + gossip;
  static peers → `connect/endpoints`. `domain_id` does **not** — it lives in the
  keyexprs (ROSZenoh), not the transport.

So the split is clean: Zenoh.jl owns the transport config object and we drive it
through its existing setter API; the Context adds the ROS naming/identity config
and the small ROS-env→Zenoh-config translation.

### Namespacing & name resolution (the ROS-specific core)

This is the real addition over Hiroz's exact-match `RemapRules`. Names a node
hands us (`"/cmd_vel"`, `"odom"`, `"~/debug"`) are resolved to a fully-qualified
name *before* they reach ROSZenoh:

1. **Category by prefix:**
   - **absolute** `"/foo"` → used as-is (after remap).
   - **relative** `"foo"` → prepend the node's namespace.
   - **private** `"~/foo"` → prepend the node's FQN (`namespace` + `/` + `name`).
2. **Algorithm:** expand `~` → node FQN → if relative, prepend namespace → apply
   remap rules → validate (token charset, no `//`, no trailing `/`) → FQN.
3. **Remap rules** from `--ros-args`: `from:=to`, `__ns:=/new/ns`,
   `__node:=newname`, and node-scoped `nodename:from:=to`.
4. The resolved FQN feeds `ROSZenoh.topic_keyexpr` / `liveliness_keyexpr` — which
   already strip one leading/trailing slash and mangle for liveliness. So **name
   resolution lives entirely in ROSNode; ROSZenoh stays naming-agnostic** (it just
   takes the final topic string + node namespace/name).

Node FQN = `namespace + "/" + name`; every relative/private name resolves against
it. This is the piece Hiroz's minimal `apply()` doesn't cover and that we must
build out for real ROS2 name compatibility.

### Lifecycle

`Context` is the RAII root: `Context(f) do ctx … end`, or explicit
`close(ctx)` → undeclare every node's entities → close the session (Hiroz
`shutdown` = `session.close()`). SIGINT handling belongs here too and reuses
`close`'s drain-timeout (await in-flight goal/handler tasks, then interrupt) —
the process-lifecycle open area is really "what `close(ctx)` does on a signal."

A convenience `Node("name")` (no explicit context) lazily creates/uses a
process-default context — the `rclcpp::init` analog — but the **explicit
`Context` is primary**, which is the more Julian and the multi-node-friendly form.

*Optional:* rclcpp **sub-namespaces** (`create_sub_node`) — deferred.

## Time & clocks

The `Context.clock` field is the hook; this is what hangs off it. Mirrors
rclcpp's time model (three clock types, sim time via `/clock`, `use_sim_time`),
expressed Julian-ly.

### Three clocks

- **system** — wall clock (`CLOCK_REALTIME`); can jump (NTP). For human-facing
  timestamps.
- **steady** — monotonic (`CLOCK_MONOTONIC`); never goes backward. For
  durations/timeouts.
- **ROS** — the abstraction: equals **system** time normally, or **simulated**
  time driven by the `/clock` topic when `use_sim_time` is set. This is what
  `now(node)` returns and what stamps go through.

### Clock sources are a discriminated type set

The clock is selected by a singleton type, not a `Symbol` — type-stable, and the
discriminator doubles as the `RTime` tag (next):

```julia
abstract type ClockSource end
struct System <: ClockSource end    # wall clock
struct Steady <: ClockSource end    # monotonic
struct ROS    <: ClockSource end    # system, or sim via /clock under use_sim_time
```

### `RTime` / `Duration` value types

`RTime{C<:ClockSource}` wraps a clock-tagged `Int64` nanosecond count (rclcpp's
`rcl_time_point_value_t`), *not* the raw `builtin_interfaces/Time` (sec+nanosec)
struct. (Named `RTime`, **not** `Time`, so it can't be mistaken for `Dates.Time` /
`Base` — the name clash that motivated this.)

- `RTime{C} - RTime{C} → Duration`, `RTime{C} ± Duration → RTime{C}`, comparisons
  within a clock. **Mixing clocks is a compile-time `MethodError`**, not a runtime
  check — `RTime{Steady} - RTime{System}` simply has no method. The type
  discriminators turn rclcpp's runtime "clock types differ" throw into a
  dispatch-level guarantee. `Duration` itself is clock-agnostic (rclcpp parity).
- `convert` ↔ `builtin_interfaces/Time` & `Duration` at the wire/`Header.stamp`
  boundary (those types are ROSMessages-generated).
- Accept `Dates.Period` for ergonomics: `now(node) + Dates.Second(1)`.
- Accessors: `now(node)::RTime{ROS}` (the common stamping path);
  `now(node, Steady())` / `now(node, System())` for explicit selection.
  `clock(node)` / `clock(node, Steady())` return a `Clock{C}` to hold (for sleeps
  and jump registration); `now(node, C())` is sugar for `now(clock(node, C()))`.

### Time source (context-level)

When any node sets `use_sim_time`, the **Context** hosts one `/clock` subscription
(`rosgraph_msgs/msg/Clock`, needed statically like `type_description_interfaces`),
holds the current sim time atomically, and wakes time-waiters (timers, rates,
sleeps) on each update via a condition variable. `use_sim_time` is a well-known
**node parameter** (bool, default false) — so it rides the Parameters subsystem,
and toggling it at runtime flips that node's ROS clock between sim and system and
fires a **jump callback**. Jump callbacks (forward leap / backward jump / sim
activate-deactivate) are registerable, because timers must recompute their next
fire on a jump. Sim source at context scope feeds per-node ROS clocks, each
honoring its own `use_sim_time` — one `/clock` sub, per-node opt-in.

### Timers, rate, sleep — all clock-aware

- `Timer(node, period; clock=ROS()) do … end` — fires every `period`, callback as
  a task (handler `concurrency` policy applies). On a `Steady()`/`System()` clock
  it's a `Base.Timer`/task; on the `ROS()` clock **under sim it's driven by
  sim-time advances** (fires when `/clock` crosses the next deadline), so a 1 s ROS
  timer tracks sim time, not wall time. Node-scoped, so it's distinct from
  `Base.Timer` by the node argument.
- `Rate(node, hz)` + `sleep(rate)` — maintain a loop frequency on the node's
  clock; sleeps in sim time under sim.
- `sleep(clock, dur)` / `sleep_until(clock, t)` — sim-aware and **interruptible**:
  a `ROS()`-clock sleep waits for sim time to advance and *blocks indefinitely if
  `/clock` stops* — so it must wake on context shutdown (next section).

### Stamping vs the wire timestamp — two different things

`header.stamp = now(node)` is **explicit and app-level** (ROS never auto-stamps).
Distinct from the rmw_zenoh **attachment** `source_timestamp` (publish-time,
system clock) carried alongside sequence number + GID — that's transport
metadata, set by the publish path, not the message body.

### Shutdown interaction

ROS-clock waits (timers, `sleep`, `Rate`) must be interruptible on `close(ctx)` —
a node blocked on sim time that never advances must not hang shutdown. So every
clock-wait registers with the context's shutdown, which wakes them (early-return
or a `ShutdownException`) as part of the drain. This is the concrete reason the
process-lifecycle/`close` story and the clock are coupled.

### Examples

**1 — Stamp a published message.** `now(node)` is an `RTime{ROS}`; assigning it into
a `builtin_interfaces/Time` field converts at the boundary (the generated kw
ctor's `convert`). Under sim this stamp is sim time, for free:

```julia
posepub = Publisher(node, "/pose", geometry_msgs.msg.PoseStamped)
function report(pose)
    publish(posepub, PoseStamped(
        header = Header(stamp = now(node), frame_id = "map"),   # ROS clock → wire Time
        pose   = pose,
    ))
end
```

**2 — Measure an interval on the steady clock.** Durations come from differencing;
`Dates` periods interoperate. Steady, so an NTP step can't make it negative:

```julia
t0      = now(node, Steady())
plan    = compute_plan()
elapsed = now(node, Steady()) - t0           # Duration
elapsed > Dates.Millisecond(50) && @warn "planning overran: $elapsed"
```

**3 — A control loop as a ROS-clock timer.** 50 Hz *in ROS time* — under sim it
tracks `/clock`, so the same code runs faster/slower than wall with the simulator:

```julia
loop = Timer(node, Dates.Millisecond(20)) do        # clock = ROS() (default)
    publish(cmdpub, compute_command())
end
# ... later, or automatically when the node closes:
close(loop)
```

Need a true wall cadence regardless of sim (e.g. a watchdog blink)? Pin the clock:

```julia
blink = Timer(node, Dates.Second(1); clock = Steady()) do
    toggle_led()
end
```

**4 — Rate-limited loop.** `Rate` sleeps the remainder of each period on the
node's clock:

```julia
rate = Rate(node, 10)            # 10 Hz
while isopen(node)
    step!()
    sleep(rate)                  # sim-aware; interruptible on close(ctx)
end
```

**5 — Sleep / deadline, and the wall-vs-ROS distinction.**

```julia
sleep(node, Dates.Second(1))                       # ROS clock: 1 *sim* second under sim
sleep(clock(node, Steady()), Dates.Second(1))      # always 1 wall second
sleep_until(clock(node), now(node) + Dates.Second(5))
```

All three throw `ShutdownException` if `close(ctx)` fires while waiting — and a
ROS-clock sleep that's waiting on a stalled `/clock` is woken the same way.

**6 — `use_sim_time`: the control code doesn't change.** It's a well-known node
parameter; flipping it (launch override or at runtime) reroutes the node's ROS
clock to `/clock`. Examples 1/3/4/5 above are already sim-correct with no edit:

```julia
# Launch:  ros2 run … --ros-args -p use_sim_time:=true
# Or at runtime, via the Parameters subsystem:
node.parameters.use_sim_time = true     # now() and ROS() timers follow /clock; fires a jump
```

**7 — React to a time jump.** Register on the clock; fires on sim
activate/deactivate and large discontinuities, so stateful estimators can reset:

```julia
on_jump(clock(node)) do jump
    if jump.clock_change || abs(jump.delta) > Dates.Second(1)
        @warn "time jumped by $(jump.delta) — resetting filter"
        reset_estimator!()
    end
end
```

**8 — Cross-clock mixing is an error, by design.** Catches the classic bug of
subtracting wall from sim:

```julia
now(node, Steady()) - now(node, System())   # MethodError: no RTime{Steady} - RTime{System}
```

### Forks

- **Clock scope** — context-level sim source feeding per-node ROS clocks (lean,
  one `/clock` sub) vs a per-node `TimeSource` (rclcpp-exact, more subscriptions).
- **Time representation** — dedicated `RTime{C}` `Int64`-ns clock-tagged value
  types (lean; cross-clock mixing is a compile-time `MethodError`) vs reusing
  `Dates` throughout (familiar, but
  no clock tagging and awkward for the sec+nanosec wire form).
- **ROS-timer-under-sim catch-up** — on a large forward jump, fire once or
  fire-per-missed-period? Lean: fire once + report the overrun (ROS timers report
  missed cycles), never burst.

## Process lifecycle & shutdown

### `spin` parks; it does not process

The one conceptual departure from rclcpp worth stating loudly: callbacks already
run on Julia's scheduler (the Zenoh consumer task + per-handler tasks), so there
is **no executor to pump**. `spin(ctx)` / `wait(ctx)` simply **block the calling
task until the context shuts down**, so a script's main task doesn't fall off the
end and kill the process. In a REPL or when embedded in a larger app, you skip
`spin` entirely — the nodes run on their tasks and you `close(ctx)` when done.

### Shutdown state & triggers

A context has `:running → :shutting_down → :shutdown`. Triggers:

- `close(ctx)` — synchronous: request shutdown, then wait for the drain.
- `request_shutdown(ctx; reason)` — async, **idempotent**; returns immediately.
- a signal (below), or a node deciding to quit (`request_shutdown` from a handler).

Predicates and unwinding: `isopen(node)` / `is_shutdown(ctx)` for loop guards
(`while isopen(node)`), and a `ShutdownException` woken into every blocked wait
(clock `sleep`/`Rate`, a pending service `call`, an action `fetch`) so user code
unwinds cleanly instead of hanging.

### The drain (one path, reused by every trigger)

Ordered so peers see a clean exit and in-flight work finishes:

1. Flip to `:shutting_down` — reject new goals, stop dispatching new samples, wake
   all clock-waits / blocked calls with `ShutdownException`.
2. Run `on_shutdown` hooks (below).
3. **Await in-flight tasks** up to `drain_timeout` — goal/service handler tasks
   *including detached post-cancel cleanup* (the action "descend and disarm"
   case), so we never drop a vehicle mid-safing.
4. Undeclare all entities (liveliness + routes) — clean graph departure.
5. Close the Zenoh session. → `:shutdown`, notify `spin` waiters.

Past `drain_timeout`, remaining tasks are hard-interrupted and the session is
force-closed. This is the same drain the **Contexts** and **Time & clocks**
sections refer back to — signals, `close`, and the action/clock interruption all
funnel through it.

### Signals — minimal handler, scheduler does the work

Consistent with the foreign-thread pattern: the OS signal handler does the one
safe thing — `uv_async_send` a context-owned `AsyncCondition` — and a Julia task
woken by it runs the graceful drain. Handles **SIGINT and SIGTERM**. **First
signal = graceful**; a **second SIGINT while draining = immediate `exit(130)`**,
so a wedged handler can't trap the operator.

Signal handling is **process-global and opt-in**: `Context()` does *not* grab
signals (a library embedding us must not steal the host's handlers); `spin(ctx;
handle_signals = true)` installs them for the duration of the spin and removes
them on return — so signal ownership is scoped to the one call that's acting as
the app entrypoint. With multiple contexts, a process-level shutdown manager fans
the signal out to all that opted in.

### `on_shutdown` hooks

`on_shutdown(ctx) do … end` registers user cleanup — safe the hardware, flush a
log, release a resource — run during the drain (step 2, before the session
closes), each bounded by a timeout so one hook can't stall the rest.

### Putting it together

```julia
Context() do ctx
    node = Node(ctx, "controller")
    Subscription(node, "/odom", nav_msgs.msg.Odometry) do msg
        update_estimate!(msg)
    end
    Timer(node, Dates.Millisecond(20)) do
        publish(cmdpub, compute_command())
    end
    on_shutdown(ctx) do
        disarm!()                  # safe the vehicle on *any* exit path
    end
    spin(ctx; handle_signals = true)   # park until Ctrl-C / SIGTERM / request_shutdown
end                                    # block-exit close(ctx) is then an idempotent no-op
```

### rclcpp mapping

| rclcpp | here |
|---|---|
| `rclcpp::init` | `Context(…)` |
| `rclcpp::shutdown` | `close(ctx)` / `request_shutdown(ctx)` |
| `rclcpp::spin` | `spin(ctx)` (parks, doesn't pump) |
| `rclcpp::ok()` | `isopen(node)` / `!is_shutdown(ctx)` |
| `Context::on_shutdown` | `on_shutdown(ctx) do … end` |

### Forks

- **Signal-handling default** — lean: off at `Context()`, on via
  `spin(; handle_signals=true)`, so libraries stay signal-neutral and only the
  app entrypoint grabs them.
- **`drain_timeout` default** — long enough to safe hardware, short enough not to
  hang a supervisor kill; lean ~5 s, overridable per context.
- **Second-signal behavior** — hard `exit(130)` (lean, simple, operator-friendly)
  vs escalating to a task-interrupt-then-exit.

## Managed (lifecycle) nodes

A `LifecycleNode` — a distinct node type (chosen over a `Node` flag so dispatch
specializes: a plain `Node` is always-active with no gating branch) — gives the
standard ROS2 managed-node contract: a state machine an external orchestrator
drives and observes, so a system can be brought up deterministically and supervised.

### State machine

States as a discriminated type set (consistent with `ClockSource`):

```julia
abstract type LifecycleState end
struct Unconfigured <: LifecycleState end
struct Inactive     <: LifecycleState end
struct Active       <: LifecycleState end
struct Finalized    <: LifecycleState end
```

`state(node)::LifecycleState`; `isactive(node)` is the gate predicate. Transition
states (`Configuring`/…) are internal — we're "in" one only while a callback runs.

Transitions, each running a user callback, driven in-process or via the control
service:

```julia
configure!(node)    # Unconfigured → Inactive   (on_configure)
cleanup!(node)      # Inactive → Unconfigured    (on_cleanup)
activate!(node)     # Inactive → Active          (on_activate)
deactivate!(node)   # Active → Inactive          (on_deactivate)
shutdown!(node)     # any → Finalized            (on_shutdown)
```

**Transition result = the action-handler three-way, reused:**
- callback returns normally ⇒ **SUCCESS** (land in target),
- returns `failure()` ⇒ **FAILURE** (revert to origin — a clean "can't right now"),
- throws ⇒ **ERROR** → `on_error` runs; its SUCCESS recovers to `Unconfigured`,
  else `Finalized`.

Nothing new to learn — same return / sentinel / throw shape as action settlement.

### Two entity classes

- **Control surface — always live, never gated.** Auto-created on every
  `LifecycleNode` (from the well-known `lifecycle_msgs`, riding the normal
  service/publisher machinery): services `~/change_state`, `~/get_state`,
  `~/get_available_states`, `~/get_available_transitions`,
  `~/get_transition_graph`, and a `~/transition_event` topic published on every
  transition. This is how an external manager drives and *asynchronously watches*
  the node (vs polling `get_state`).
- **Application entities — gated by state.** Every `Publisher`/`Subscription`/
  `Timer`/`Service` the user creates on the node.

### Gating — gate everything, at dispatch

While the node is not `Active`, the publish/dispatch paths check `isactive(node)`
(automatic, since every entity knows its node):

| Entity | Inactive behavior |
|---|---|
| Publisher | `publish` → **drop** (no-op, counted) |
| Subscription | handler doesn't fire; sample **dropped** (inactive = not processing) |
| Timer | doesn't fire |
| Service | handler doesn't fire; **error-reply** "node inactive" (caller isn't left hanging — consistent with fail-safe settlement) |

Two deliberate choices: **gate everything**, not just publishers (rclcpp gates only
the publisher and leaves subs/timers to developer discipline — its known footgun),
so `on_activate`/`on_deactivate` are usually *empty*; and **gate at dispatch**, not
declaration, so entities stay visible in the graph while inactive (the orchestrator
sees the node's wiring) — matching rclcpp, at the cost of "present yet silent."

### Composition

`shutdown!` (or `close(ctx)`'s drain reaching a managed node) runs `on_shutdown`
→ `Finalized`, reusing the lifecycle drain. Remote observation is the
`transition_event` topic + `get_state` service — the layer-2 RPC + broadcast from
the introspection model.

### Convenience

Strictly externally-driven by default (the node reacts to `change_state`; it does
not self-transition), with an opt-in for the simple case:

```julia
node = LifecycleNode(ctx, "camera"; autostart = true, on_configure = …, …)
# autostart ⇒ configure! + activate! on construction
```

### Example

```julia
mutable struct CamState; dev; pub; timer; end
st = CamState(nothing, nothing, nothing)

cam = LifecycleNode(ctx, "camera";
    on_configure = node -> begin
        st.dev   = open_camera("/dev/video0")
        st.pub   = Publisher(node, "image", sensor_msgs.msg.Image)   # gated until Active
        st.timer = Timer(node, Dates.Millisecond(33)) do            # fires only when Active
            publish(st.pub, grab_frame(st.dev))
        end
    end,
    on_activate   = node -> nothing,                                # empty — gating is automatic
    on_deactivate = node -> nothing,
    on_cleanup    = node -> (close(st.timer); close(st.pub); close_camera(st.dev)),
    on_error      = node -> reset_device!(st),                      # SUCCESS-return ⇒ recover
)

configure!(cam); activate!(cam)        # or: ros2 lifecycle set /camera activate
```

## Intra-process short-circuit

When a publisher and a subscriber live in the **same Context**, the publisher hands
the message to the subscriber's handler directly — skipping CDR serialize, the
Zenoh hop, and deserialize. **On by default** (within a single Context; a
`intra_process = false` Context flag opts out). The realization that keeps it
API-minimal: *intra-process is the same receive model with an in-heap Julia object
as the buffer instead of a Zenoh payload* — so the existing knobs carry it.

- **Transparent.** `publish(pub, msg)` and `Subscription(...) do msg … end` are
  unchanged. The publisher consults the Context registry for same-Context
  subscribers with compatible type + QoS and delivers to them directly. Mixed
  local+remote: direct to local subs **and** serialize once for the remote ones —
  local subs win regardless.
- **Dedup via `Locality`.** IPC-enabled local subs set `allowed_origin = remote`
  (ROSZenoh already models `Locality`), so the Zenoh loopback of a local
  publication is ignored and the data arrives via the direct path only. No
  double-delivery; reuses an existing primitive.
- **Share vs copy = the `view` flag** (the same one governing cross-process
  aliasing):

  | Local subscriber | gets | cost |
  |---|---|---|
  | default (owned) | a **copy** of the message | one in-heap copy — still skips serialize + Zenoh + deserialize |
  | `view = true` | the publisher's object **shared read-only** | true zero-copy |

**Why owned-copy is the safe default here too.** Sharing one object across N subs
+ the publisher aliases only on *in-place mutation* — and intra-process is *milder*
than the cross-process borrow: the object is GC'd, so **escape is safe** (no
`BorrowError`, no lifetime hazard); only mutation is shared state. Owned-by-default
preserves "mutate freely," so code written for cross-process doesn't silently
corrupt siblings when colocated. Immutable (`@cdr1_compat`) messages have **zero**
hazard and share freely even under `view = true`. The `view = true` IPC contract is
just "don't mutate in place; copy to mutate" — weaker than the cross-process view
(no escape restriction).

**Composition.** Type/QoS matching and lifecycle gating still apply (no delivery to
an inactive `LifecycleNode` sub). **Scope is same-Context**: two Contexts in one OS
process have separate Zenoh sessions, so cross-Context-same-process short-circuiting
is a cross-registry match — noted as a later extension, not built.

## QoS completeness & events

Two halves: completing `QosProfile`, and surfacing the standard rmw events. The
satisfying part is that #9 is **mostly composition** — the attachment, the clock,
and the graph-change stream we already built *are* the event substrate.

### `QosProfile` completeness

Add the three missing policies (ROS2 defaults shown — all "no constraint"):

```julia
deadline         :: Union{Nothing, Duration}   # nothing = ∞ (default)
lifespan         :: Union{Nothing, Duration}   # nothing = ∞
liveliness       :: Symbol                      # :automatic (default) | :manual_by_topic
liveliness_lease :: Union{Nothing, Duration}    # nothing = ∞
```

These feed two existing-but-stubbed paths: **ROSZenoh `encode_qos`/`decode_qos`**
(currently placeholders for these three — completing them byte-for-byte with
rmw_zenoh is a ROSZenoh TODO), and the **RxO compatibility checker** (QoS-incompat
detection extends to deadline/liveliness — same `on_qos_incompatible` machinery,
more policies).

### Events — sourced from pieces we already have

| Event (sub / pub) | Sourced from | New work |
|---|---|---|
| **liveliness-changed** (sub) / **liveliness-lost** (pub) | the **graph-change stream** — a matched publisher's token disappearing *is* liveliness-lost; a *view* on the same stream as type-mismatch/QoS-incompat | ~none (a filter) |
| **message-lost** (sub) | gaps in the attachment **`sequence_number`** per source gid | gap-track per gid |
| **lifespan** (enforce) | drop a sample when `now() - attachment.source_timestamp > lifespan` | sub-side timestamp check |
| **deadline-missed** (req/offered) | a per-endpoint **`Timer`** on the node clock, reset each message/publish | per-endpoint deadline timer |
| **incompatible-qos / -type** | already done | — |

Uniform `on_*` surface, consistent with `on_type_mismatch`/`on_qos_incompatible`:

```julia
Subscription(node, "/scan", LaserScan;
    qos = QosProfile(deadline = Dates.Millisecond(100), lifespan = Dates.Second(1)),
    on_deadline_missed    = ev -> @warn("scan stalled",  missed = ev.count),
    on_liveliness_changed = ev -> @warn("publishers",    alive  = ev.alive_count),
    on_message_lost       = ev -> @warn("dropped",       lost   = ev.lost_count),
) do msg … end
```

So liveliness events are another filter on the graph stream (alongside the
detectors), message-lost falls out of the attachment `seq`, lifespan out of the
attachment `source_timestamp` + clock, and only deadline needs a genuinely new
piece — a per-endpoint `Timer`, which we already have.

### Honest caveat — Zenoh is not DDS

These aren't natively enforced by Zenoh; several are *our* implementations over
attachment + clock + liveliness, generating the ROS2 **event contract** rather
than inheriting DDS enforcement:

- **deadline** is notify-only — which matches ROS2 (deadline never enforces, only
  signals).
- **lifespan** we genuinely enforce (drop expired on the sub side).
- **liveliness** rides the Zenoh liveliness tokens (`:automatic` = token exists;
  `:manual_by_topic` = publisher periodically re-asserts its token).
- regardless of enforcement, QoS values **travel in the liveliness token**, so
  cross-vendor compatibility checking (vs a real DDS node via a bridge) works.

### Phasing

- **Now (nearly free, high value):** full `QosProfile` + carry-in-`encode_qos` +
  compat-check; **liveliness events** (graph-stream filter) and **message-lost**
  (attachment `seq`) — both essentially fall out.
- **Lower-priority slice:** **lifespan** drop and **deadline** timers — real but
  each a small, self-contained add. `:manual_by_topic` liveliness (heartbeat token
  re-assertion) is the only fiddly sub-case; `:automatic` is free.
