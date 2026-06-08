# ROSNode — Mixins, Nodes & Composition (DESIGN)

The authored macros (`@ros_message` / `@ros_service` / `@ros_action`) gave every
*entity* its natural Julian form. The *node* never got one — it stays imperative
glue — and the intra-process short-circuit (ARCHITECTURE §15.1) built the *data plane*
for composition without the *structure*: no reusable unit, no container, no
deploy-time binding.

This document designs both, as one idea:

> **Composition is a construction pattern at every scale.** A node is a
> **collection of mixins**, each a cohesive chunk — its own state, the entities
> authored onto it, its own lifecycle. Mixins compose into a node (build time);
> nodes compose into a process (deploy time). **Execution** — dispatch and the
> Zenoh data plane — is the orthogonal, shared runtime. A *capability* is just a
> reusable mixin; a *container* is just nodes one scale up.

It builds on `ARCHITECTURE.md` §5 (Context), §6 (Node/entities), §10 (parameters),
§14.2 (managed nodes), §15.1 (intra-process), and changes none of them — this is an
authoring + construction layer over the runtime that already exists. The lone
exception is the §15.1 intra-process short-circuit, designed but with wiring deferred
(D6); see §7 and §10.

**Cross-references:** a bare `§N` is a section of **this** document; a section of
`ARCHITECTURE.md` is written `ARCHITECTURE §N`.

ROS 2 composition: https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Composition.html

---

## 1. Why neither a struct nor a function

A node is not data and not a procedure; it is a **live reactive entity** — identity,
private state, reactions that run repeatedly and concurrently, and a lifecycle. The
two obvious Julian shapes each lie about half of that:

- **A `struct` is data.** Modelling a node *as* a struct welds passive config, live
  resource handles, and active identity into one value — the symptoms are entity
  handles `#undef` until configure, a `getproperty` shim to read a live parameter, and
  value-equality for a thing that has identity. (The hazard is the *welding*, not
  mutability itself: a state-only mutable struct, §2, keeps only the benign part.)
- **A `function` is run-once.** A node wires itself up and then *keeps living* — its
  parameters mutate, its entities run on their own tasks (ARCHITECTURE §4) after setup
  returns. Binding parameters as arguments freezes them; they go stale under a running
  timer.

The primitive that fits is **dispatch**: Julia keeps behaviour in a type's method
table and entrains state into instances the methods dispatch over. Reactions become
methods on the live instance — every *read* sees current state, so no state read is
captured and frozen (the one residue dispatch can't carry — a parameter used as a
timer's *rate* — is handled by drift detection, §3.4). The struct is demoted to what
a struct is good at: holding state.

The further realisation (this revision): *which* reactions and entities a node has is
a **construction** concern — you build it from chunks, then it runs — even though each
reaction runs by dispatch at execution. You don't extend a node with new behaviour
while it runs; you build it, then it runs. Each chunk is a **mixin** — and a node is a
collection of them.

---

## 2. The mixin — the unit

`@mixin struct M … end` declares one cohesive chunk: its **own state** (and nothing
else — entities and parameters attach to the type, §3), plus the lifecycle hooks and
reactions dispatched on it. A mixin is the reusable, authorable, testable unit.
`@mixin` expands to a *mutable* struct (state-only), so reactions can update fields —
the §1 smell was entity handles + identity welded into a value, not mutability.

```julia
@mixin struct ImageCapture
    dev::Union{Device, Nothing} = nothing      # STATE ONLY
    pose::Pose = Pose()
    enabled::Bool = false
end
```

Because each mixin owns its state, the "stateful capability" problem from earlier
sketches is gone: a heartbeat's sequence counter lives in the heartbeat mixin, not
borrowed from a host.

### 2.1 Behaviour is dispatch over the live instance

A reaction is a method dispatched on the mixin type, handed the live instance. Each
invocation reads current state (`m.dev`, `parameters(m).fps`) at the moment it runs,
so the "parameter changed underneath me" hazard cannot arise for state or parameter
*reads*; the lone exception — a parameter used as a timer's rate, fixed at
materialisation — is detected and warned, not silently re-applied (§3.4).

### 2.2 Entities are authored onto the mixin

An entity's persisted state (the `Zenoh.Publisher`, the consumer task, the
liveliness token) is the framework's business — so entities do not live in the
struct. They are **authored onto the mixin type** and, at construction, materialised
onto the node-core's entity set (§4). The mixin is the unit you author services,
pub/sub, timers, and actions onto.

---

## 3. The port surface — HAS vs DOES

"Entities are behaviour" holds for half of them. The cut is whether a port has a
handler to author:

| | Ports | Surface | Why |
|---|---|---|---|
| **DOES** | subscription, service, action server, timer | an **authored function** (`@hears`/`@serves`/`@runs`/`@every`) with an `m::M` first arg | genuinely reactive — author the handler as one function, the `@ros_*` way |
| **HAS** | parameter, publisher, service/action client | a **declaration** (`@param`/`@publishes`/`@uses`) | mostly held output/handle the mixin drives from its reactions (a client also *awaits* its reply, §3.5) |

**One name, not three.** A *wire-named* port's identifier and its ROS wire name
default to each other — `image` ⇄ `"image"` — so you write it once and the redundant
`on "image"` disappears. For a HAS wire port (publisher/client) the identifier is the
field you wrote; for a DOES port it is the function name; the wire name is that
identifier unless overridden. (A parameter's identifier is its parameter-server name
and a timer's names only its local handle — neither has a wire name.) Two bailouts: a
**leading name-string overrides the wire name** (`@serves "~/set_mode" function arm(…)` —
handler `arm`, service name `~/set_mode`), and when you'd rather write the *name* than
invent a handler name, an anonymous `do`-reaction lets the overarching macro **parse
the wire name into the identifier** — its last sanitised segment (`@hears
"/camera/image_raw" do (m::Detector, msg::Image) … end` ⇒ handler `image_raw`;
`~/transition_event` ⇒ `transition_event`). A name collision or a non-identifier
segment forces an explicit name. (ROS calls these topic / service / action names; we
say "wire name" for the shared rule and reserve "topic" for pub/sub.)

### 3.1 HAS — declarations

```julia
@param     ImageCapture fps::Int64 = 30 ∈ 1..120     # §10 grammar; live, validated, ros2-param-driveable
@param     ImageCapture device::String = "/dev/video0"
@publishes ImageCapture image :: Image                 # output port — topic "image" (= the name); driven via `entities(m).image`
@uses      ImageCapture planner :: NavigateToPose on "/planner"   # action client; bailout: explicit absolute name
```

`@param` reuses §10's field grammar verbatim. A publisher declaration is a sibling
of a parameter — a typed thing the mixin *has*, not something it *does*; forcing it
into a bodyless "function" would be ceremony hiding nothing.

### 3.2 DOES — authored functions

Each mirrors its authored-entity macro, with one added first argument `m::M` that
attaches the function to the mixin and gives it the live instance. The body *is* the
handler.

```julia
@serves function arm(m::ImageCapture, enable::Bool)::@NamedTuple{success::Bool}   # service "arm" (= the name)
    m.enabled = enable                                   # `enabled` is a real ImageCapture field (§2)
    (success = enable,)
end
@hears function odom(m::ImageCapture, msg::Odometry)        # subscribe "odom" (= the name)
    m.pose = msg.pose.pose                                # Odometry.pose is a PoseWithCovariance; .pose is the Pose
end
@every :fps function tick(m::ImageCapture)            # rate bound to `fps` — drift detected + warned (§3.4)
    publish(entities(m).image, grab_frame(m.dev))
end
```

`@serves`/`@runs` inherit `@ros_service`/`@ros_action`'s two type-sources (author the
type inline from the signature, or serve an existing type); `@hears` takes the
message type from the `msg` annotation; `@runs` takes a `FeedbackSink` parameter
(ARCHITECTURE §9 Actions; `FeedbackSink` itself is the authored `@ros_action` layer,
`src/typesupport/authored.jl`). Each macro both **defines** the dispatched reaction
and **records a port** on `M` pointing at it; at construction the framework
materialises the port by wiring its ARCHITECTURE §6 data route to call the reaction
with the live `m`.

### 3.3 Lifecycle — plain methods

Lifecycle hooks carry no wire identity, so they are plain methods (defaulting to
no-ops), run under the ARCHITECTURE §14.2 three-way (return ⇒ SUCCESS, `failure` ⇒
revert, throw ⇒ ERROR → `on_error`):

```julia
configure(m::ImageCapture) = m.dev = open_camera(parameters(m).device)   # acquire; `device` is a @param
cleanup(m::ImageCapture)   = close_camera(m.dev)                         # release; `dev` is real state
on_error(m::ImageCapture)  = reset_device!(m.dev)
# activate/deactivate default to no-ops — gating (ARCHITECTURE §14.2) is automatic
```

These (and the DI hooks `requires`/`construct`, §4.2) *extend* framework generics, so a
component module must `import ROSNode: configure, activate, deactivate, cleanup,
on_error, requires, construct` (or qualify, `ROSNode.configure(m::M) = …`). A bare
definition under plain `using ROSNode` defines a *new* shadowing function the framework
never calls. (Reactions authored by `@hears`/`@serves`/`@every`/`@runs`, and `@param`/
`@provides`, are macro-emitted and need no import.)

### 3.4 Parameters are live; rate bindings are *detected*, not auto-applied

A reaction reads a parameter live — `parameters(m).fps` is the ARCHITECTURE §10
atomically-swapped snapshot, type-stable, always current. The one thing dispatch
cannot carry is wiring whose *parameter* is a parameter — a timer's rate, fixed when
the timer is materialised.

`@every :fps` does **not** silently re-time the timer when `fps` changes: a rate
shifting and re-phasing underneath you is exactly the kind of invisible behaviour
change that misleads. Instead it **declares the binding so the framework can detect
drift**. The timer keeps its rate; when `fps` changes and the timer has not been
rewired, the framework emits a **noisy, throttled warning** naming the timer, the
parameter, and the old→new value. You own the rewire:

```julia
@every :fps function tick(m::ImageCapture)
    publish(entities(m).image, grab_frame(m.dev))
end

@on_parameter ImageCapture :fps (m::ImageCapture, fps) -> retime!(entities(m).tick, fps)  # you rewire — nothing re-times on its own
```

Mechanically: `@every :fps` records, per timer, the bound parameter name and the Hz it
was last (re)timed at. `retime!` on a bound timer is **new API** (not the existing
`Timer` surface, ARCHITECTURE §7) — since `Timer.period` is `const`, it reopens the
backing `Base.Timer` rather than mutating in place (so it re-phases) and updates the
recorded basis to the value passed, which clears the warning. The drift check compares
that recorded *basis* (not the raw period, which is lossy under integer-Hz rounding)
to the live value after each param commit + `@on_parameter` pass. A literal rate
(`@every 30`) declares no binding and is never checked. It rides ARCHITECTURE §10's
on-change events and §12.2's detect-and-warn pattern (throttled, with a fix
suggestion). Nothing re-times without you.

### 3.5 Reaching ports & parameters — the reflective handle API

A mixin's framework surface is two **reflective, rich-typed views** of its node-core,
each keyed by the names the macros recorded — no loose symbols:

- `entities(m)` — the materialised ports. `entities(m).tick` *is* the live `Timer`,
  `entities(m).image` the `Publisher`; drive each through its own typed methods.
- `parameters(m)` — the live ARCHITECTURE §10 parameter snapshot. `parameters(m).fps`
  is the current, type-stable value (the mixin's own local name, regardless of any
  node-level prefix from §4.3).

```julia
publish(entities(m).image, frame)              # the Publisher handle
gh = send(entities(m).planner, goal)           # the action-client handle ⇒ a goal handle …
result = fetch(gh)                             #   … fetch the result (iterate feedback / cancel as usual)
retime!(entities(m).tick, parameters(m).fps)   # the Timer handle + the live rate (retime! is new API, §3.4)
```

Both are reflective lookups against the node-core, not struct fields, so the nucleus
stays state-only — `m.<field>` is only the mixin's own state. Before a port is
materialised the access errors clearly, and reactions run only after. Per mixin,
`entities(m)` / `parameters(m)` are the typed views described below;
`entities(node)` / `parameters(node)` are **heterogeneous aggregations** across all of
a node's mixins — iterable/filterable, but without the single-concrete-struct typing,
and member-namespaced (§4.4, e.g. `entities(node).image_capture.image`) rather than
mixin-local.

**Typed by the mixin type.** `parameters(m)` and `entities(m)` return
`Parameters{Foo}` / `Entities{Foo}`, parameterised on `Foo = typeof(m)` — and that
parameter *is* the efficiency. Dispatch on `Foo` selects a **concrete, per-mixin
struct** the macros generate at expansion: the §10 schema `P_Foo` (a
`@parameters`-style struct of the mixin's `@param`s) and a handle struct `E_Foo`
whose fields are the declared ports' concrete handle types — `tick::Timer`,
`image::Publisher{Image}`, …. So `parameters(m).fps` and `entities(m).tick` are
ordinary **typed field loads** — type-stable, inlinable, and autocompletable, not
dict lookups, with no `@generated` on the hot path (a field access on a concrete
struct constant-folds the literal name). `parameters(m)` captures one atomic §10
snapshot for a consistent read within a reaction; `entities(m)` holds the
materialised handles. It's the same **static-dispatch-over-a-type-parameter
technique** `ParameterServer{P}` and `PublisherHandle{T}` already use (ARCHITECTURE
§10 / §6) — here the parameter is the mixin type, which *selects* the derived
`P_Foo`/`E_Foo`. The dynamic-feeling surface, static dispatch underneath.

---

## 4. Composing mixins into a node — the DI protocol

A node is a **collection of mixins sharing one node-core** (identity, liveliness
token, parameter server, lifecycle state, entity set). It is **one node in the
graph** — an outside peer sees a single node whose entities are contributed by
different chunks. Mixins are an internal organisation, not sub-nodes.

```julia
@node Camera = ["capture" => ImageCapture, "heartbeat" => Heartbeat]    # syntax illustrative
```

### 4.1 Interfaces are generic functions; provision is a Holy trait

A mixin's dependency on a sibling is not "a subtype of `PoseSource`" — it is "I can
call `last_pose` on it." So an **interface is a set of generic functions**, and a
mixin **provides** one by declaring **Holy-trait evidence** — *not* by subtyping (no
single-supertype cap; a mixin provides any number) and *not* inferred from method
existence:

```julia
@interface PoseSource  last_pose(_)::Pose         # a NAME for a method-set; not an abstract type

@mixin struct ImageCapture
    dev::Union{Device, Nothing} = nothing
    pose::Pose = Pose()
    enabled::Bool = false
end
last_pose(m::ImageCapture) = m.pose               # satisfy the contract's methods
@provides ImageCapture PoseSource                 # Holy-trait evidence + an expansion-time check
# ⇒ provides(::Type{ImageCapture}) = (PoseSource,)   (a mixin may @provides any number)
```

**Why trait evidence, not `hasmethod`.** Resolving on method existence keys on
*incidental* presence — a broad fallback or an unrelated definition reads as
"provides," and the provider set can shift as later definitions advance the world.
Trait evidence is the *intentional* declaration Julia designed for capability
dispatch; it enumerates providers deterministically, and it asserts the *whole*
contract is met. Method existence has one job here: a one-time check, inside
`@provides`, that the declared claim is backed.

### 4.2 Requirements, construction, toposort

A mixin declares what it needs by interface, and its construction is a dispatched
method that receives the resolved providers injected:

```julia
@mixin struct Detector
    src::Any = nothing                            # slot for the injected provider
    model::Union{Model, Nothing} = nothing
end
requires(::Type{Detector}) = (PoseSource,)        # need-by-interface — drives the toposort
construct(::Type{Detector}, node, src) = Detector(src = src)   # dispatch on Detector; dep injected
@hears function image(m::Detector, img::Image)    # subscribe "image" (= the name)
    publish(entities(m).boxes, infer(m.model, img; roi = roi_from(last_pose(m.src))))   # `m.src` used via the interface
end
```

The framework, at construction:

1. for each mixin, read `requires`;
2. resolve each required interface to providers in the node, **excluding the requirer
   itself** — `{M ≠ requirer : I ∈ provides(M)}` — **exactly one** ⇒ a dependency
   edge; zero ⇒ unsatisfied error; more than one ⇒ ambiguous error — disambiguate by
   pinning the provider in `requires` as a pair, `requires(::Type{Detector}) =
   (PoseSource => ImageCapture,)` (a heterogeneous tuple of bare `Interface` or
   `Interface => Mixin`; the pair resolves to that mixin while still asserting `I ∈
   provides(M)`);
3. **toposort** the edges; a cycle is a construction error naming the offending edge;
4. `construct` each in order, injecting resolved siblings positionally in `requires`
   order — `construct(::Type{M}, node)` defaults to `M()` when a mixin has no
   dependencies;
5. the **same order governs lifecycle fan-out** — a dependency `configure`s before
   its dependent, and tears down after.

`construct` dispatches on the *mixin being built* (`::Type{Detector}`), not on the
dependency's type — there is no supertype to annotate `src` with, so the dep arrives
untyped and is used through its interface methods (`last_pose(m.src)`). The resolver
is the guarantee it satisfies the contract; duck-typed use is the Julian norm. The
`node` argument is the node-core handle (the back-ref attaches as `construct`
returns): inside `construct` you may read parameters through it, but an injected
provider must only be **stored**, not used — its state isn't configured until step 5
(see §10), so use it from `configure` onward, in dependency-first order.

### 4.3 Parameters: the namespace is a property of the construction path

Mixin `@param`s merge into the node's one §10 schema. Whether they're prefixed is
decided by **how you built the node**, not by how many mixins it ended up with — so
the names never shift underneath you. There are two construction paths:

- **A mixin used *as* a node** — promoted directly, `run(ImageCapture; …)` or
  `add!(c, ImageCapture; …)` — is the trivial "node that does exactly one thing" case
  for simple applications. The framework wraps the single mixin in a node-core behind
  the scenes and its params are **un-prefixed**: a clean `fps`.
- **A composed node** — the normal collection form `@node N = [ … ]` — is **always
  per-mixin namespaced** (by member name, §4.4), **even with a single member**, because
  you declared a composition. `@node` always takes the bracketed collection, so there is no
  bare-vs-bracket ambiguity to trip over.

```julia
run(ImageCapture; name = "cam")                          # mixin-as-node  ⇒ un-prefixed:  ros2 param get /cam fps
@node Rig = ["camera" => ImageCapture, "lidar" => Lidar] # composed       ⇒ prefixed:  /rig camera.fps , /rig lidar.fps
@node Box = ["camera" => ImageCapture]                   # composed, ONE  ⇒ STILL prefixed:  /box camera.fps
```

The within-a-node 1→2 rename is gone: a composed node is prefixed from one mixin
onward, so adding members never renames. The *only* rename is the deliberate refactor
from a mixin-as-node to a `@node` composition — a visible change of construction path,
exactly when you'd expect the parameter surface to change (it does change the external
`ros2 param` / launch-YAML name — see §10). A mixin can pin `prefix = …` to fix a
specific name across both paths.

**Override keys are mixin-local.** `overrides` (and `run`/`add!` parameter kwargs) are
keyed by the **mixin-local** name — exempt from the per-mixin prefix, matching
`parameters(m)` — so `overrides = (fps = 60,)` sets `ImageCapture`'s `fps` whether the
node is composed or not. When two mixins declare the same-named param, qualify it with
the prefixed string form, `var"image_capture.fps" = 60` (the separator is a dot, and
`image_capture` is the member name, §4.4).

### 4.4 Member names: instances, namespacing, and remap

A `@node` is a list of **members** — each `"name" => Mixin`, where the string `name`
is the member's **namespace** within the node (written out, never auto-derived from
the type, so a member's namespace is always explicit):

```julia
@node Perception = ["camera" => ImageCapture, "detector" => Detector, "beat" => Heartbeat]
@node Rig        = ["front" => Camera, "rear" => Camera]   # two Camera instances: front, rear
```

The member name is the mixin's one node-level namespace, applied uniformly to the two
**private** surfaces (this is §4.3's parameter prefix, generalised):

- **parameters** — `front.fps`, `rear.fps`, so two instances' params don't collide;
- **the node-level entity view** — `entities(node).front.image`, so two mixins' `image`
  handles don't collide.

Within a mixin, `parameters(m)` / `entities(m)` stay unprefixed and mixin-local — the
member namespace is transparent to the author. This dissolves both earlier open
questions: **one instance per type** (distinct members are distinct instances —
behaviour stays type-keyed via `construct`/`provides`/reactions; only state and handles
are per-member) and **node-level handle collisions** (member-namespaced). When an
interface has more than one provider, disambiguate by member —
`requires(::Type{Detector}) = (PoseSource => :front)` (a member name) or `=> ImageCapture`
(a type, when unique).

**Wire topics are the shared surface — remap, don't auto-prefix.** Unlike params and
handles, topics are *deliberately shared* between mixins (a `Detector` subscribing the
`image` an `ImageCapture` publishes is intra-node wiring), so auto-prefixing them by
member would silently break that. A member's topics instead resolve in the node
namespace as authored, and you rewire at assembly with a per-member **remap** (ROS
`-r`-style), which may point at another member's resolved name:

```julia
@node Rig = [front => Camera{image => "front/image"},     # isolate the two cameras' outputs
             rear  => Camera{image => "rear/image"},
             fuse  => Stitcher{left  => front.image,        # wire fuse's inputs to the
                               right => rear.image}]         #   cameras' (remapped) outputs
```

Assembly resolves every member's wire names and **errors on an unintended clobber** —
two members landing on one wire name unless one explicitly remaps onto it. An
accidental double-`image` is a hard error you fix by renaming; the deliberate
`fuse.left => front.image` wiring is allowed because you wrote it. (A genuinely
intended multi-publisher on one topic is expressed by remapping both onto it — making
the sharing explicit rather than accidental.)

---

## 5. The node at runtime

**Construction is framework-mediated.** You never write `Camera(fps = 30)` as a value
— that resurrects identity-as-value confusion. You write `run(K; …)` /
`add!(container, K; …)` / a `LoadNode` request (§7) — where `K` is either a `@node`
composition or a single `@mixin` promoted to a node (§4.3); the framework allocates
the node-core, applies `overrides`, runs the toposorted construction (§4.2), and
drives the lifecycle. Identity lives in the node-core.

**The node-core** is an ARCHITECTURE §6 `Node` (unmanaged) or §14.2 `LifecycleNode`
(managed), shared by all the node's mixins. A mixin instance stays a plain state
struct — `m.<field>` is only its own state, with no `getproperty` magic. Everything
framework-side is reached reflectively (`entities(m)` / `parameters(m)`, §3.5) or
through duck-typed helpers that carry `m` to the core (`context(m)`, `now(m)`,
`resolve_name(m, …)`), as today's accessors duck-type on `.context`/`.clocks`.

**Managed vs unmanaged — vocabulary always, surface opt-in.** The
configure/activate/deactivate/cleanup vocabulary is the model for every node; the
*wire control surface* (the five `lifecycle_msgs` services + `~/transition_event` +
dispatch gating, ARCHITECTURE §14.2) is opt-in:

- **Unmanaged (default).** `run(Camera; …)` autostarts (configure → activate at
  construction) and declares no control surface; a trivial node pays no gating branch.
- **Managed.** `run(Camera; managed = true)` (or a managing container) declares the
  control surface and lets an external orchestrator drive it; gating now applies and
  the mixins' `configure`/`cleanup` run at the real transitions. It defaults to
  starting `Unconfigured`, but may also `autostart` (configure+activate at
  construction, ARCHITECTURE §14.2) — `autostart` is orthogonal to the control surface.

The lifecycle methods you author are identical either way; only *when* they run and
*whether* the node is externally driveable differ. **Teardown** reuses ARCHITECTURE
§14: mixin `cleanup` runs **once** on the first teardown trigger (a transition for a
managed node, node-core close for an unmanaged one — guarded so a node already driven
to `Finalized` does not re-run it), then the node-core undeclares every materialised
port and the node token — the §14.1 "undeclare entities" step, specialised to a
mixin-composed node.

---

## 6. Capabilities = reusable mixins

Because a mixin is self-contained (state + entities + lifecycle), a mixin reused
across unrelated nodes *is* a capability — there is no separate mechanism:

```julia
@mixin struct Heartbeat
    seq::UInt32 = 0                               # its OWN state
end
@publishes Heartbeat beat :: std_msgs.msg.UInt32 on "~/heartbeat"
@every 1 function tick(m::Heartbeat)              # 1 Hz; `tick` is a distinct identifier from the `beat` port
    publish(entities(m).beat, std_msgs.msg.UInt32(data = (m.seq += 1)))
end

@node Camera = ["capture" => ImageCapture, "beat" => Heartbeat]   # Camera heartbeats — one line, no copy-paste
@node Lidar  = ["driver" => LidarDriver, "beat" => Heartbeat]
```

(The publisher port and the timer reaction must have **distinct** identifiers — both
land as fields of the one `E_Heartbeat` handle struct, §3.5 — hence `beat` for the
port and `tick` for the timer.)

This is not new: the framework **already** attaches `~/get_type_description` and the
`/rosout` logger to **every** node invisibly (ARCHITECTURE §13), and the lifecycle
control surface to **managed** nodes (§5; ARCHITECTURE §14.2). Today those are
hard-wired. The mixin model makes that same act *first-class and user-extensible*, so
the standard ROS interfaces every node re-implements (diagnostics, `tf` broadcast,
lifecycle status) become mixins you list. A capability that needs cross-mixin state
declares it like any mixin, via `requires`/`provides` (§4).

---

## 7. Composing nodes into a process — the second scale

Same shape, one scale up: a reusable unit, late-attached, the binding deferred. ROS
2's composition contract is exactly this — main-less code that runs **standalone in
its own process** *or* **composed into a shared process**, decided at deploy time.

```julia
run(Perception; name = "perception")              # standalone: its own Context/process

container("vision") do c                          # composed: both nodes share ONE Context
    add!(c, Perception; name = "perception")      # self-sufficient (ImageCapture ▸ Detector ▸ Heartbeat); publishes /perception/image
    add!(c, Recorder;   name = "recorder")        # a dep-free node that @hears "/perception/image" — same Context ⇒ a direct in-process path
end
```

`run` is the `rclcpp_components_register_node` → standalone-executable analog; a
fresh `Context` (own session, discovery, registry, threads) with one node. (Each node
must be **self-sufficient** — every mixin's `requires` satisfied within it, §4.2 — so
the consumer here is `Recorder`, a dep-free node, not the `Detector` mixin, whose
`PoseSource` dependency is satisfied *inside* `Perception`.) `container` opens **one**
Context and instantiates each added node on it. **Today** same-Context delivery rides
Zenoh's loopback (correct, just not short-circuited); once the §15.1 intra-process
wiring lands (deferred, D6), a `Recorder` subscription matched to a `Perception`
publisher takes the **direct in-process path** — no CDR serialise, no Zenoh hop,
sharing or copying per the subscriber's `view`. The session/discovery/registry/thread
consolidation below is real today; only the direct-path half awaits D6.

**Dynamic composition (`ros2 component` parity):** `@node`/`@mixin` register the kind
by name (the `rclcpp_components_register_nodes` analog, reusing the authored-macro
deferred-registration pattern); a container exposes a `~/_container/load_node` /
`unload_node` / `list_nodes` service, so `ros2 component load/unload/list` drives it,
and the `ComposableNodeContainer` / `LoadComposableNodes` launch actions call that same
`load_node` service to **push** composable nodes into a running container. A
runtime-loaded node is resolved against the running set the same way — one more
node-core on the Context.

**Why this is the resource story.** Standalone: *N* × (session + discovery + registry
+ threads). Composed: 1 × shared, plus a direct in-process path (zero-copy per the
subscriber's `view`, once D6 lands). Composition is precisely a resource-consolidation
decision deferred to deploy time, and the author never has to know which they got.
Type/QoS matching and lifecycle gating still apply across the short-circuit
(ARCHITECTURE §15.1); scope is same-Context (ARCHITECTURE §15.2).

---

## 8. What it reuses

| Surface | Built on |
|---|---|
| `@mixin` state + `@param` | ARCHITECTURE §10 `@parameters` grammar; the authored-macro deferred-registration pattern |
| node = mixins sharing a node-core | ARCHITECTURE §6 `Node` / §14.2 `LifecycleNode` |
| `configure`/`activate`/…/`on_error` | ARCHITECTURE §14.2 transition three-way + `on_*` hooks |
| ports materialised, gated until Active | ARCHITECTURE §6 entity declaration; §14.2 dispatch gating |
| `@serves`/`@hears`/`@runs` bodies | ARCHITECTURE §8 Service, §4 Subscription dispatch, §9 Action server + the authored `@ros_action` `FeedbackSink` |
| `@param` reads/sets, `ros2 param` | ARCHITECTURE §10 `ParameterServer{P}` on the node-core |
| `requires`/`provides`/`construct` | plain Julia dispatch + Holy traits — no new runtime |
| `run` (standalone) | ARCHITECTURE §5 `Context` + §6 `Node` + §14.1 `spin` |
| `container` / `add!` | one ARCHITECTURE §5 Context + §15.1 intra-process registry |
| `LoadNode` + `ros2 component` | a name-keyed kind registry + the ARCHITECTURE §13 introspection-service pattern |

The backticked `src/`-level symbols some rows imply (`make_entity`/`declare_*`,
`isactive`, `set_intra_process!`) are the implementations of the cited section's
prose, not names the prose itself uses.

---

## 9. Worked example

Domain helpers (`Device`, `open_camera`, `grab_frame`, `last_frame`, `estimate_pose`,
`load_model`, `infer`, `roi_from`, `record`, `Model`) are elided placeholders; the
example shows shape and rule-conformance, not loadability.

```julia
using ROSNode
@ros_import "sensor_msgs/msg/Image" "geometry_msgs/msg/Pose" "std_msgs/msg/UInt32" "vision_msgs/msg/Detection2DArray"

@interface PoseSource  last_pose(_)::Pose

@mixin struct ImageCapture
    dev::Union{Device, Nothing} = nothing
    pose::Pose = Pose()
    enabled::Bool = false
end
@param     ImageCapture fps::Int64 = 30 ∈ 1..120
@param     ImageCapture device::String = "/dev/video0"
@publishes ImageCapture image :: sensor_msgs.msg.Image
last_pose(m::ImageCapture) = m.pose
@provides  ImageCapture PoseSource
configure(m::ImageCapture) = m.dev = open_camera(parameters(m).device)   # `device` is a @param
cleanup(m::ImageCapture)   = close_camera(m.dev)
@every :fps function tick(m::ImageCapture)
    m.pose = estimate_pose(grab_frame(m.dev))
    publish(entities(m).image, last_frame(m.dev))
end
@on_parameter ImageCapture :fps (m::ImageCapture, fps) -> retime!(entities(m).tick, fps)   # own the rewire; else a drift warning (§3.4)

@mixin struct Detector
    src::Any = nothing
    model::Union{Model, Nothing} = nothing
end
@param     Detector weights::String = "yolov8.pt"
@publishes Detector boxes :: vision_msgs.msg.Detection2DArray
requires(::Type{Detector}) = (PoseSource,)
construct(::Type{Detector}, node, src) = Detector(src = src)
configure(m::Detector) = m.model = load_model(parameters(m).weights)   # `weights` is a @param
@hears function image(m::Detector, img::sensor_msgs.msg.Image)    # subscribe "image" (= the name)
    publish(entities(m).boxes, infer(m.model, img; roi = roi_from(last_pose(m.src))))
end

@mixin struct Heartbeat; seq::UInt32 = 0; end
@publishes Heartbeat beat :: std_msgs.msg.UInt32 on "~/heartbeat"
@every 1 function tick(m::Heartbeat)              # `tick` ≠ the `beat` port (one E_Foo, distinct fields)
    publish(entities(m).beat, std_msgs.msg.UInt32(data = (m.seq += 1)))
end

# A node assembled from chunks; Detector ▸ ImageCapture by PoseSource, Heartbeat free:
@node Perception = ["image_capture" => ImageCapture, "detector" => Detector, "heartbeat" => Heartbeat]

# Deploy-time choice — the SAME node either way:
container("vision") do c
    add!(c, Perception; name = "perception", overrides = (fps = 60,))   # mixin-local key (§4.3)
end
# …or `run(Perception; name = "perception")` in its own process.
```

---

## 10. Honest costs & open questions

- **Over-fragmentation.** If every trivial thing is a mixin, a simple node is a pile
  of them. Mitigation: a node can be a single mixin; mixins are for genuinely
  separable chunks, not ceremony.
- **Namespace tracks the construction path** (§4.3): within a path the names are
  stable, but the deliberate mixin-as-node → `@node` refactor **renames the parameter
  as seen externally** (`/cam fps` → `/box image_capture.fps`) — a breaking change for
  launch YAML / `ros2 param` / orchestrators keyed on the old name, mitigated by
  pinning `prefix`. Source-visible, but not non-breaking for out-of-process consumers.
- **Cross-mixin collisions are namespaced or caught** (§4.4): params and node-level
  handles are member-namespaced; wire topics resolve in the node namespace with an
  assembly-time **clobber error**, rewired by a per-member remap. Residual: a
  *deliberately* shared topic must be remapped onto its target explicitly — there is no
  implicit cross-mixin topic merge (by design — implicit merges were the hazard).
- **Multiple instances of a type** are distinct members with distinct names (§4.4), so
  the earlier one-instance-per-type limit is gone. Residual: behaviour stays
  **type-keyed** (all instances of a type share `construct`/reactions/`provides`), so
  per-instance *behavioural* variation — not just differing state or config — still
  needs a distinct mixin type.
- **Ambiguity** (§4.2): two mixins providing one interface is an error until
  disambiguated (`PoseSource => ImageCapture`). Deterministic, but the author resolves it.
- **`construct` injects untyped deps** (§4.2) — the price of dropping subtyping for
  unbounded `@provides`; deps are used duck-typed through their interface methods.
- **Two-phase init across `construct` and `configure`.** A dependency is injected
  (§4.2 step 4) while still constructed-but-unconfigured; its state is usable only from
  `configure` onward, in dependency-first order (step 5). Storing a provider in
  `construct` is fine; *using* it there sees uninitialised state.
- **Rate-binding is detect-and-warn, not auto-applied** (§3.4): a parameter-bound timer
  rate does not re-time on a param change — the author opts into rewiring via
  `@on_parameter`, or accepts a stale rate plus a recurring warning. A priced, defended
  decision: silent re-phasing is deliberately avoided.
- **Ports are reached reflectively, not by symbol** (§3.5): `entities(m).image` /
  `entities(m).tick` are rich typed handles, so the stringly-addressing footgun is
  gone — the only caveat is that the lookup resolves a port just after it's
  materialised, not before. The typed-field-load efficiency is a per-mixin guarantee;
  node-level `entities(node)`/`parameters(node)` are heterogeneous aggregations (§3.5).
- **Gone:** the state-only mixin removes entity fields, so the `#undef`-until-configure
  window does not exist; per-mixin state removes the stateful-capability wrinkle; and
  the one-interface-per-mixin cap is gone with subtyping (§4.1).

---

## 11. Phasing

Same endpoint, derisked order:

1. **Mixin + dispatch** — `@mixin` state, node-core + duck-typing, the HAS/DOES port
   surface, lifecycle methods, single-mixin `@node`, `run` (standalone). Supersedes
   the imperative node glue.
2. **DI** — `@interface`/`@provides`/`requires`/`construct`, resolution + toposort +
   lifecycle fan-out, multi-mixin `@node` with namespacing.
3. **Capabilities** — retrofit the framework's own type-description / `/rosout`
   (every node) and lifecycle control surface (managed nodes) attachments onto the
   mixin mechanism.
4. **Composition into a process** — `container` / `add!` over §15.1 (which itself must
   first be wired, D6), then the kind registry + `LoadNode` service + `ros2 component`
   interop.

The imperative `Node` / entity API (ARCHITECTURE §6) remains — the mixin layer is the
authored, reusable tier on top of it, as the authored macros sit on top of
`@ros_import`.

**Implementation status.** The core is implemented and smoke-verified in
`src/model/component/{component,run}.jl`: `@mixin`, `@param`, `@publishes`/`@uses`,
`@hears`, `@serves` (both existing-type and inline-authoring), `@runs` (inline-authoring
actions, with feedback + the `FeedbackSink`), `@every`, the lifecycle hooks,
`@interface`/`@provides`/`requires`/`construct` with toposorted DI + injection, the
reflective `parameters(m)` (typed `ParameterServer{P_Foo}` — type-stable + transactional
`ros2 param`) and `entities(m)` (type-stable — the ports' concrete NamedTuple type is
captured at first materialise), managed/unmanaged via `LifecycleNode` + dispatch gating
(§5), `@node`, `run`, and `container`/`add!` (§7; capabilities are reusable mixin
members, §6). The `@mixin`-vs-authored-types `__init__` collision is resolved (the
component `__init__` finalizes both).

Now also implemented (`src/model/component/composition.jl`,
`src/patterns/parameters/composite.jl`):

- **Node-level `ros2 param` namespacing across multi-member nodes** (§4.4). A
  `CompositeParameterServer` façade aggregates the members' per-mixin servers behind one
  member-prefixed `<member>.<field>` namespace, one `/parameter_events`, and one set of
  the six standard services (`wire_parameter_services!` is generic over an
  `AbstractParameterServer`). Prefixing tracks the **construction path** (§4.3):
  `composed = K isa NodeKind`, so a mixin promoted to a node is un-prefixed while a
  `@node` (even single-member) is prefixed. `parameters(node)` / `entities(node)` are the
  member-namespaced node-level views; `overrides` accept mixin-local **and** prefixed
  `var"member.field"` keys.
- **Dynamic `LoadNode`/`ros2 component`** (§7). `@node`/`@mixin` register the kind by name
  (the `rclcpp_components_register_nodes` analog); a `container` is itself a node hosting
  `~/_container/{load_node,unload_node,list_nodes}` over the vendored
  `composition_interfaces`, with a programmatic `load_node`/`unload_node`/`list_nodes`
  API. Each loaded node gets a container-unique id. (Empty-request services like
  `list_nodes` drove a supporting fix: ROSMessages now injects the rosidl synthetic
  `uint8 structure_needs_at_least_one_member` into fieldless messages, so an empty message
  has a correct RIHS01 + wire form — this also repairs the lifecycle empty-request
  services.)

Deferred, each a focused follow-up: a §D8 warm-up hook (first-`run` JIT latency); the
§4.4 **wire-topic remap + clobber-error** surface (the `@node N = [m => K{topic => "…"}]`
syntax — params/handles are namespaced today, but topics still resolve in the node
namespace without an assembly-time clobber check); a single node-level
`/parameter_events` for an atomic set spanning multiple members (today each member's
commit publishes its own event — correct per-member, but a multi-member atomic set emits
one event per touched member); managed-node teardown on a **Context drain** (cleanup now
fans out via a drain hook, but the managed `LifecycleNode`'s gate registration is not
deregistered on drain — only on an explicit `close`); and a precompiled-package
node-kind registry (kinds register at module-body eval, like `_register_mixin!`, so a
precompiled component *library* would need `__init__`-deferred registration).

---

## References

- ROS 2 composition — https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Composition.html
- ROS 2 managed nodes — https://design.ros2.org/articles/node_lifecycle.html
- Holy traits — the Julia capability-dispatch idiom (evidence by method, not subtyping)
- ROSNode architecture — `ARCHITECTURE.md` §5, §6, §10, §13, §14, §15.1
- Authored entities — `src/typesupport/authored.jl`; `examples/authored_*.jl`
