# Managed (lifecycle) nodes. A distinct `LifecycleNode` wraps a plain `Node`
# and adds the ROS2 managed-node contract: a state machine an external orchestrator
# drives and observes. A distinct type rather than a `Node` flag keeps a plain
# `Node` always-active with zero gating branch, and keeps the lifecycle surface
# (states, transitions, control services) off to the side, clear of the hot Node
# struct.
#
# The contract has three parts:
#   1. State machine — `Unconfigured`/`Inactive`/`Active`/`Finalized` as
#      discriminated types (the `ClockSource` precedent), driven by
#      `configure!`/`activate!`/`deactivate!`/`cleanup!`/`shutdown!`, each running a
#      user callback under the *action three-way* (return ⇒ SUCCESS land in target,
#      `failure` ⇒ revert to origin, throw ⇒ ERROR → `on_error`).
#   2. Control surface — the five `lifecycle_msgs` services + a `~/transition_event`
#      topic, always live and *never gated*, so an external manager drives and
#      asynchronously watches the node.
#   3. Gating — while not `Active`, every application entity on the node is gated
#      at dispatch (publishers drop, subs/timers don't fire, services error-reply).
#      Gating at dispatch (rather than at declaration) keeps inactive entities
#      visible in the graph; gating every entity (rclcpp gates only publishers)
#      leaves `on_activate`/`on_deactivate` usually empty.

using ROSZenoh: ROSZenoh, QosProfile, default_qos

export LifecycleNode, LifecycleState, Unconfigured, Inactive, Active, Finalized,
       state, isactive, inner_node,
       configure!, activate!, deactivate!, cleanup!, shutdown!

# ── state machine ──────────────────────────────────────────────────────────────
# Same singleton shape as `ClockSource`/`SettlementStatus`.

"""
    LifecycleState

Sealed abstract supertype of the four ROS 2 managed-node primary states:
[`Unconfigured`](@ref), [`Inactive`](@ref), [`Active`](@ref), and
[`Finalized`](@ref). Each is a zero-field singleton, so dispatching on
[`state`](@ref) turns an unhandled state into a `MethodError` at the call site.

Maps to the primary states of the ROS 2 [managed-node
lifecycle](https://design.ros2.org/articles/node_lifecycle.html)
(`lifecycle_msgs/msg/State` ids `PRIMARY_STATE_UNCONFIGURED`/`_INACTIVE`/`_ACTIVE`/
`_FINALIZED`). The four primary states are the only reified ones: a node occupies a
transition state (Configuring, Activating, …) exactly for the duration of its
callback, so the callback scope is the transition.
"""
abstract type LifecycleState end

"""
    Unconfigured() :: LifecycleState

The state immediately after [`LifecycleNode`](@ref) construction: no configuration
loaded, no resources acquired. The ROS 2 `PRIMARY_STATE_UNCONFIGURED`.

Reachable as the initial state, after [`cleanup!`](@ref) (`Inactive → Unconfigured`),
and as the recovery target when an error-processing callback succeeds. The valid
transitions out are [`configure!`](@ref) (to [`Inactive`](@ref)) and
[`shutdown!`](@ref) (to [`Finalized`](@ref)).
"""
struct Unconfigured <: LifecycleState end

"""
    Inactive() :: LifecycleState

Configured and idle: the node's entities exist and stay visible in the graph, and
the dispatch gate holds them shut — publishers drop, subscriptions and timers stay
quiet, application services error-reply. The ROS 2 `PRIMARY_STATE_INACTIVE`.

Reached by [`configure!`](@ref) (from [`Unconfigured`](@ref)) or
[`deactivate!`](@ref) (from [`Active`](@ref)). Valid transitions out:
[`activate!`](@ref), [`cleanup!`](@ref), and [`shutdown!`](@ref).
"""
struct Inactive     <: LifecycleState end

"""
    Active() :: LifecycleState

The live state: the gate is open and application entities dispatch normally —
publishers send, subscriptions and timers fire, services serve. The ROS 2
`PRIMARY_STATE_ACTIVE`, and the only state for which [`isactive`](@ref) is `true`.

Reached by [`activate!`](@ref) from [`Inactive`](@ref). Valid transitions out:
[`deactivate!`](@ref) and [`shutdown!`](@ref). On entry, [`activate!`](@ref)
re-runs each transient_local subscription's latched-history fetch so the node picks
up state published while it was inactive.
"""
struct Active       <: LifecycleState end

"""
    Finalized() :: LifecycleState

The terminal state, reached once [`shutdown!`](@ref) has run from any non-terminal
state. The ROS 2 `PRIMARY_STATE_FINALIZED`. No transitions lead out; the node is
spent and is typically closed.

Also the landing when error processing fails to recover: a callback that throws
runs `on_error`, and any `on_error` outcome other than success drops the node here.
A throwing `on_shutdown` lands here even when `on_error` recovers — shutdown is the
terminal path, so error processing cannot divert it. `close` runs
[`shutdown!`](@ref) first, which normally lands here.
"""
struct Finalized    <: LifecycleState end

Base.show(io::IO, ::Unconfigured) = print(io, "Unconfigured")
Base.show(io::IO, ::Inactive)     = print(io, "Inactive")
Base.show(io::IO, ::Active)       = print(io, "Active")
Base.show(io::IO, ::Finalized)    = print(io, "Finalized")

# The `lifecycle_msgs/msg/State` id for each primary state (wire-facing, used by the
# `get_state` reply + `transition_event` start/goal states). Kept here so the wire
# layer has the single source of truth.
_state_id(::Unconfigured) = 0x01
_state_id(::Inactive)     = 0x02
_state_id(::Active)       = 0x03
_state_id(::Finalized)    = 0x04

_state_label(::Unconfigured) = "unconfigured"
_state_label(::Inactive)     = "inactive"
_state_label(::Active)       = "active"
_state_label(::Finalized)    = "finalized"

# The `lifecycle_msgs/msg/Transition` id for each user-driven transition (the
# external `change_state` request carries one of these; the wire layer maps it onto
# the matching `_apply_transition!`). Reserved [0-9] public transitions.
const TRANSITION_CONFIGURE            = 0x01
const TRANSITION_CLEANUP              = 0x02
const TRANSITION_ACTIVATE             = 0x03
const TRANSITION_DEACTIVATE           = 0x04
const TRANSITION_UNCONFIGURED_SHUTDOWN = 0x05
const TRANSITION_INACTIVE_SHUTDOWN    = 0x06
const TRANSITION_ACTIVE_SHUTDOWN      = 0x07

# Internal error-processing outcome ids (`Transition.msg` 60/61): published on the
# error-recovery edge's `transition_event`, never requestable via `change_state`.
const TRANSITION_ON_ERROR_SUCCESS     = 0x3c
const TRANSITION_ON_ERROR_FAILURE     = 0x3d

# ── LifecycleNode ────────────────────────────────────────────────────────────────
# Wraps a plain `Node` (entities are created against the wrapped node, so they get
# the full id/token/route/graph lifecycle for free) and adds the managed-node
# state machine + callbacks + control surface. The current state is atomic — the
# gate predicate reads it on every dispatch from arbitrary handler tasks, and the
# control services drive transitions concurrently with `configure!`/… called
# in-process; a per-node `lock` serializes the transitions themselves so two racing
# `change_state` requests can't interleave callback execution.

"""
    LifecycleNode(ctx::Context, name::AbstractString; namespace=nothing, enclave=nothing,
                  autostart=false, on_configure=…, on_activate=…, on_deactivate=…,
                  on_cleanup=…, on_shutdown=…, on_error=…, msgs=nothing) -> LifecycleNode

A ROS 2 managed node: a distinct node type that wraps a plain [`Node`] and adds the
managed-node state machine an external orchestrator drives (`~/change_state`) and
observes (`~/get_state`, `~/transition_event`). Construction starts the node in
[`Unconfigured`](@ref). See the ROS 2 [managed-node
design](https://design.ros2.org/articles/node_lifecycle.html) for the state machine.

The six transition callbacks each take the `LifecycleNode` and run under the action
three-way settlement: returning normally is SUCCESS (land in the target state);
returning the [`failure`](@ref) token is FAILURE (a clean "can't right now" that
reverts to the origin state); throwing is ERROR (`on_error` runs — its SUCCESS
recovers to [`Unconfigured`](@ref), any other outcome — or any `on_shutdown` error —
drops the node to [`Finalized`](@ref)). Each callback defaults to a no-op, so a node
with no behavior still transitions cleanly. A thrown `ShutdownException` propagates
unchanged — it signals context shutdown, not a callback error.

While the node is not [`Active`](@ref), every application entity created on it is
gated at dispatch ([`isactive`](@ref)): publishers drop, subscriptions and timers
stay quiet, application services error-reply with "node inactive". The control
surface — the five `lifecycle_msgs` services plus the `~/transition_event` topic —
stays live in every state, since an external manager must reach it precisely while
the node is inactive. Create application entities against [`inner_node`](@ref)`(ln)`
so they register on the node the gate is keyed by.

`autostart=true` runs [`configure!`](@ref) then [`activate!`](@ref) on construction
for the simple single-owner case; a failed or erroring transition leaves the node in
whatever state the three-way reached. The `~/transition_event` topic is published
transient_local + reliable with depth 1, so a late-joining manager sees the latest
transition (matching rclcpp's latched event topic). `msgs` is accepted for
source-compatibility and ignored: the control surface always declares from the
vendored `lifecycle_msgs` types.

The per-node lock serializes transitions, so two racing `~/change_state` requests —
or a wire request racing an in-process [`configure!`](@ref) — cannot interleave
their callbacks. `close` (or the Context drain) runs
[`shutdown!`](@ref) before tearing down the node's entities.

```julia
node = LifecycleNode(ctx, "camera";
    on_configure = ln -> (open_device!(); nothing),
    on_cleanup   = ln -> (close_device!(); nothing))
img = Publisher(inner_node(node), "image", sensor_msgs.msg.Image)  # gated until Active
configure!(node)   # Unconfigured → Inactive, opens the device
activate!(node)    # Inactive → Active, image publishes start flowing
```
"""
mutable struct LifecycleNode
    const node::Node
    # Transition callbacks (`f(node)`); each returns normally / `failure` / throws.
    const on_configure::Function
    const on_activate::Function
    const on_deactivate::Function
    const on_cleanup::Function
    const on_shutdown::Function
    const on_error::Function
    # The current primary state — atomic so the gate predicate reads it lock-free
    # from any dispatch task; transitions latch it under `lock`.
    @atomic _state::LifecycleState
    # Serializes transitions: two concurrent `change_state` requests (or an
    # in-process `configure!` racing a wire request) must not interleave callbacks.
    const lock::ReentrantLock
    # The control surface — always-live entities exempt from the gate. The
    # `transition_event` publisher (`nothing` until `_wire_control_surface!` runs)
    # and the set of control `Entity` handles the gate must *not* gate (identity
    # membership).
    _event_pub::Any
    const _control::Base.IdSet{Entity}
    @atomic open::Bool
end

# Gate registry: maps a wrapped `Node` → its `LifecycleNode`, so the dispatch-side
# `isactive(::Node)` (which only ever has the plain `Node` in hand, via `Entity`)
# can find the state without a field on `Node`. A plain `Node` is simply absent →
# always active, the no-branch fast path. Keyed by object identity under a lock;
# entries are removed on `close`.
const _GATES = Base.IdDict{Node, Any}()
const _GATES_LOCK = ReentrantLock()

_register_gate!(ln::LifecycleNode) = @lock _GATES_LOCK (_GATES[ln.node] = ln)
_unregister_gate!(ln::LifecycleNode) =
    @lock _GATES_LOCK (haskey(_GATES, ln.node) && delete!(_GATES, ln.node))
_gate_for(node::Node) = @lock _GATES_LOCK get(_GATES, node, nothing)

function LifecycleNode(ctx::Context, name::AbstractString;
                       namespace::Union{AbstractString, Nothing}=nothing,
                       enclave::Union{AbstractString, Nothing}=nothing,
                       autostart::Bool=false,
                       on_configure::Function = _lc_noop,
                       on_activate::Function = _lc_noop,
                       on_deactivate::Function = _lc_noop,
                       on_cleanup::Function = _lc_noop,
                       on_shutdown::Function = _lc_noop,
                       on_error::Function = _lc_noop,
                       msgs = nothing)
    node = Node(ctx, name; namespace=namespace, enclave=enclave)
    ln = LifecycleNode(node, on_configure, on_activate, on_deactivate, on_cleanup,
                       on_shutdown, on_error, Unconfigured(), ReentrantLock(),
                       nothing, Base.IdSet{Entity}(), true)
    _register_gate!(ln)
    _wire_control_surface!(ln, msgs)

    # Autostart: bring the node up to Active in one shot (the simple single-owner
    # case). A failed/erroring transition leaves the node in whatever state the
    # three-way landed it; we don't force Active.
    if autostart
        try
            configure!(ln)
            state(ln) === Inactive() && activate!(ln)
        catch
            # A throw escaping configure!/activate! (ShutdownException, re-latch
            # snapshot) would leak the wired node: the constructor never returns, so
            # no owner exists to drive `close`. Tear down inline — node before gate,
            # since a gateless node reads as active (the unmanaged fast path).
            close(ln.node)
            _unregister_gate!(ln)
            rethrow()
        end
    end
    return ln
end

# Default callback — a managed node with no behavior still transitions cleanly.
_lc_noop(::LifecycleNode) = nothing

"""
    inner_node(ln::LifecycleNode) -> Node

The plain [`Node`] wrapped by a managed node. Create application entities
against it — `Publisher(inner_node(ln), "image", T)`,
`Timer(inner_node(ln), period) do … end`,
`Subscription(inner_node(ln), "cmd", T) do msg … end` — so they register on the node
the dispatch gate is keyed by and are silenced automatically while the managed node
is not [`Active`](@ref).

The pattern constructors are typed on `Node`; unwrap with `inner_node` at each
construction site. The returned node carries the context, fully-qualified name,
namespace, and clocks; naming and clock helpers reached through the `LifecycleNode`
forward to it.
"""
inner_node(ln::LifecycleNode) = ln.node

Base.isopen(ln::LifecycleNode) = (@atomic ln.open) && isopen(ln.node)

Base.show(io::IO, ln::LifecycleNode) =
    print(io, "LifecycleNode(", ln.node.fqn, ", ", state(ln),
          isopen(ln) ? "" : ", closed", ")")

# Forward the Node surface a manager reaches through the LifecycleNode directly
# (naming/clock/shutdown hooks). `context`/`resolve_name`/`clock`/`now`/`on_shutdown`
# are duck-typed on `.context`/`.fqn`/`.namespace`/`.clocks`, which `ln.node`
# carries, so most already work via the wrapped node; `context` gets an explicit
# forward for readability at call sites that hold the `LifecycleNode`.
context(ln::LifecycleNode) = context(ln.node)
resolve_name(ln::LifecycleNode, name::AbstractString; kwargs...) =
    resolve_name(ln.node, name; kwargs...)

# ── state queries + the gate predicate ──────────────────────────────────────────

"""
    state(ln::LifecycleNode) -> LifecycleState

The managed node's current primary lifecycle state — one of [`Unconfigured`](@ref),
[`Inactive`](@ref), [`Active`](@ref), or [`Finalized`](@ref), and the value an
external `~/get_state` query reports.

Read lock-free via an atomic load, so any dispatch task can consult it cheaply;
transitions latch it under the node lock. Compare or dispatch on the singleton to
branch: `state(ln) === Active()`.

Distinct from the same-named action method: an action goal's progress is
[`state`](@ref) on a [`GoalHandle`](@ref) (a `Symbol`), not a lifecycle state.
"""
state(ln::LifecycleNode) = @atomic ln._state

"""
    isactive(ln::LifecycleNode) -> Bool
    isactive(node::Node) -> Bool
    isactive(entity::Entity) -> Bool

The dispatch gate predicate: whether an entity fires right now. A
[`LifecycleNode`](@ref) is active only in the [`Active`](@ref) state. A plain
[`Node`] is active unless it is the wrapped node of some `LifecycleNode`, found
through an identity-keyed registry (one lock-guarded lookup; an unmanaged node is
absent and always active). An [`Entity`] follows its node's state, with one
exception: the control surface of a managed node (the `lifecycle_msgs` services and
the `~/transition_event` publisher) is always active, so an external manager can
drive and observe an inactive node.

This single predicate is threaded into each data-plane dispatch site — publish,
subscription delivery, timer tick, service handling — so while a managed node is
not Active its application entities are silenced automatically: publishers drop,
subscriptions and timers skip the handler, and services error-reply. Each call
recomputes from the node's current atomic state.
"""
isactive(ln::LifecycleNode) = state(ln) === Active()

# The `Entity.node` is always a plain `Node`, so this is the predicate the dispatch
# hooks reach through `isactive(e)`. A plain Node may still be a managed node's
# wrapped node — look it up once; absent ⇒ always active.
function isactive(node::Node)
    g = _gate_for(node)
    g === nothing ? true : isactive(g::LifecycleNode)
end

function isactive(e::Entity)
    g = _gate_for(e.node)
    g === nothing && return true                  # plain Node ⇒ always
    e in (g::LifecycleNode)._control && return true   # control surface exempt
    return isactive(g::LifecycleNode)
end

# Dispatch hooks ("gate everything, at dispatch"). The gate is a single
# `isactive(e::Entity)` guard threaded into each data-plane dispatch site. This
# file is included last and `isactive` is a normal late-bound method; the sites:
#
#   • pubsub.jl   `publish`        — after `isopen(e)`: drop while inactive
#   • dispatch.jl `_predispatch`   — before decode/novelty: skip delivery
#   • service.jl  `_serve_query`   — before the handler: error-reply "node inactive"
#                                    (infra services exempt via `_is_infra_service`)
#   • time.jl     `_timer_active`  — each tick, keyed on the timer's owning node
#
# A plain `Node` answers every `isactive` with one registry lookup, so the guard is
# cheap for the common (unmanaged) case.

# ── transitions: the action three-way, reused ───────────────────────────────────
# Each public transition validates the origin state, runs the user callback under
# the three-way, and on SUCCESS latches the target + publishes a `transition_event`.
# The three exits mirror action settlement exactly:
#   callback returns normally          ⇒ SUCCESS  → land in `target`
#   callback returns the `failure` tok ⇒ FAILURE  → revert to `origin` (clean no-op)
#   callback throws                    ⇒ ERROR    → `on_error`; its SUCCESS recovers
#                                                    to Unconfigured, else Finalized
#                                                    (shutdown: always Finalized)
# A non-applicable transition for the current state is an `ArgumentError` (the
# external `change_state` maps this to a `success=false` reply; in-process it
# surfaces to the caller).

"""
    TransitionResult

Alias for `Symbol`: the outcome of a lifecycle transition, returned by
[`configure!`](@ref), [`activate!`](@ref), [`deactivate!`](@ref),
[`cleanup!`](@ref), and [`shutdown!`](@ref). One of:

  - `:success` — the callback returned normally; the node landed in the target state.
  - `:failure` — the callback returned the [`failure`](@ref) token; the node stayed
    in its origin state (a clean decline, nothing published).
  - `:error` — the callback threw; `on_error` ran, recovering to [`Unconfigured`](@ref)
    on its success or dropping to [`Finalized`](@ref) otherwise. A [`shutdown!`](@ref)
    error drops to `Finalized` either way (shutdown is terminal).

The wire `~/change_state` reply reports `success = (result === :success)`.
"""
const TransitionResult = Symbol

"""
    configure!(ln::LifecycleNode) -> TransitionResult

Drive the `Unconfigured → Inactive` transition, running the `on_configure` callback.
The conventional place to create the node's entities (gated until
[`Active`](@ref)) and acquire resources such as devices or files. Matches the ROS 2
managed-node `configure` transition.

Returns `:success` when the callback lands the node in [`Inactive`](@ref),
`:failure` if the callback returns the [`failure`](@ref) token (the node stays
[`Unconfigured`](@ref)), or `:error` if it throws (`on_error` runs). Holds the
node's transition lock so it cannot interleave with a concurrent transition. Throws
`ArgumentError` if the node is closed, or if it is not currently `Unconfigured`.
"""
configure!(ln::LifecycleNode) =
    _drive!(ln, Unconfigured(), Inactive(), ln.on_configure,
            TRANSITION_CONFIGURE, "configure")

"""
    activate!(ln::LifecycleNode) -> TransitionResult

Drive the `Inactive → Active` transition, running the `on_activate` callback.
The callback is usually empty: gating is automatic, so activation is the
state flip that opens the dispatch gate. Matches the ROS 2 managed-node `activate`
transition.

Returns `:success` when the node lands in [`Active`](@ref), `:failure` if the
callback returns [`failure`](@ref) (the node stays [`Inactive`](@ref)), or `:error`
if it throws. Throws `ArgumentError` if the node is closed or not currently
`Inactive`.

On success, after the gate opens, each transient_local subscription on the node
re-runs its latched-history query (the ROS 2 transient_local durability replay), so
the node picks up the latest state it dropped while inactive. The replay is
novelty-gated — a sample already seen is skipped across deactivate/reactivate
cycles — and runs per-entity best-effort after the transition lock is released: a
re-latch failure is logged without unwinding the transition, and the replay can
overlap a subsequent transition.
"""
function activate!(ln::LifecycleNode)
    r = _drive!(ln, Inactive(), Active(), ln.on_activate,
                TRANSITION_ACTIVATE, "activate")
    # Now that the gate is open (state latched Active inside `_drive!`), re-run each
    # transient_local subscription's latched-history query so the node picks up state
    # it dropped while inactive. Novelty-gated, so an already-seen latched sample
    # isn't replayed across deactivate/reactivate cycles. A no-op for every other
    # entity. Best-effort: a re-latch failure must not unwind the transition.
    if r === :success
        for e in _node_entities_snapshot(ln.node)
            try
                _relatch!(e)
            catch err
                @error "activate: re-latch failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
            end
        end
    end
    return r
end

# A snapshot of the node's entities under its lock (the re-latch iterates outside the
# lock, since `_relatch!` reopens routes + joins consumer tasks).
_node_entities_snapshot(node::Node) = @lock node.lock copy(node.entities)

"""
    deactivate!(ln::LifecycleNode) -> TransitionResult

Drive the `Active → Inactive` transition, running the `on_deactivate` callback.
Usually empty, since gating is automatic; the state flip re-silences the
node's application entities. Matches the ROS 2 managed-node `deactivate` transition.

Returns `:success` when the node lands in [`Inactive`](@ref), `:failure` if the
callback returns [`failure`](@ref) (the node stays [`Active`](@ref)), or `:error` if
it throws. Throws `ArgumentError` if the node is closed or not currently `Active`.
"""
deactivate!(ln::LifecycleNode) =
    _drive!(ln, Active(), Inactive(), ln.on_deactivate,
            TRANSITION_DEACTIVATE, "deactivate")

"""
    cleanup!(ln::LifecycleNode) -> TransitionResult

Drive the `Inactive → Unconfigured` transition, running the `on_cleanup` callback.
The place to release what `on_configure` acquired — close entities, free
devices — returning the node to a fresh [`Unconfigured`](@ref) state it can be
reconfigured from. Matches the ROS 2 managed-node `cleanup` transition.

Returns `:success` when the node lands in [`Unconfigured`](@ref), `:failure` if the
callback returns [`failure`](@ref) (the node stays [`Inactive`](@ref)), or `:error`
if it throws. Throws `ArgumentError` if the node is closed or not currently
`Inactive`.
"""
cleanup!(ln::LifecycleNode) =
    _drive!(ln, Inactive(), Unconfigured(), ln.on_cleanup,
            TRANSITION_CLEANUP, "cleanup")

"""
    shutdown!(ln::LifecycleNode) -> TransitionResult

Drive the terminal `{Unconfigured, Inactive, Active} → Finalized` transition,
running the `on_shutdown` callback. Valid from any non-terminal state, and
the transition `close` / the Context drain reaches when closing a
managed node. Matches the ROS 2 managed-node `shutdown` transition; the
`~/transition_event` carries the origin-specific transition id
(`TRANSITION_UNCONFIGURED_SHUTDOWN` / `TRANSITION_INACTIVE_SHUTDOWN` /
`TRANSITION_ACTIVE_SHUTDOWN`).

Returns `:success` once the node lands in [`Finalized`](@ref); a node already
`Finalized` is a `:success` no-op (idempotent), even when closed. Returns `:error`
if `on_shutdown` throws: `on_error` runs (cleanup), but the node lands in
[`Finalized`](@ref) regardless of its outcome — shutdown is the terminal path, so
error recovery cannot return the node to service. Throws `ArgumentError` if a
non-`Finalized` node is closed.
"""
function shutdown!(ln::LifecycleNode)
    origin = state(ln)
    origin === Finalized() && return :success
    # `shutdown!` is valid from any non-terminal state, so it has no single origin
    # guard — `_drive!`'s origin check is bypassed by passing the live state.
    return _drive!(ln, origin, Finalized(), ln.on_shutdown,
                   _shutdown_transition_id(origin), "shutdown")
end

_shutdown_transition_id(::Unconfigured) = TRANSITION_UNCONFIGURED_SHUTDOWN
_shutdown_transition_id(::Inactive)     = TRANSITION_INACTIVE_SHUTDOWN
_shutdown_transition_id(::Active)       = TRANSITION_ACTIVE_SHUTDOWN
_shutdown_transition_id(::LifecycleState) = TRANSITION_INACTIVE_SHUTDOWN

# The transition core. Holds the per-node lock so callbacks don't interleave, then:
#   1. validates the live state matches `origin` (else ArgumentError — not applicable),
#   2. runs `callback(ln)` under the three-way,
#   3. on SUCCESS latches `target` + publishes the transition_event,
#   4. on FAILURE leaves `origin` (publishes no event — nothing changed),
#   5. on ERROR runs `on_error` and lands in Unconfigured (its SUCCESS) or Finalized;
#      a shutdown's error lands Finalized either way (terminal path).
# Returns the `TransitionResult`. Closed node ⇒ the only hard error.
function _drive!(ln::LifecycleNode, origin::LifecycleState, target::LifecycleState,
                 callback::Function, transition_id::UInt8, label::AbstractString)
    isopen(ln) || throw(ArgumentError("transition on a closed LifecycleNode"))
    @lock ln.lock begin
        live = state(ln)
        live === origin ||
            throw(ArgumentError("$(label): not valid from $(live) \
                                 (requires $(origin))"))

        outcome = _run_transition(ln, callback, label)
        if outcome === :success
            _land!(ln, origin, target, transition_id)
            return :success
        elseif outcome === :failure
            # Clean decline: revert to (stay in) the origin; nothing to publish.
            return :failure
        else  # :error → on_error decides recovery
            return _handle_error!(ln, origin, target, label)
        end
    end
end

# Run one transition callback under the action three-way. Returns `:success` /
# `:failure` / `:error`. The `failure` *token* (core.jl) returned from the callback
# is the clean-decline sentinel — same vocabulary as service/action settlement, so
# there's nothing new to learn. Any throw (incl. `Cancelled`, which
# has no goal meaning here) is an ERROR.
function _run_transition(ln::LifecycleNode, callback::Function, label::AbstractString)
    try
        result = callback(ln)
        return result === failure ? :failure : :success
    catch err
        err isa ShutdownException && rethrow()
        @error "lifecycle callback threw; entering error processing" transition=label node=ln.node.fqn exception=(err, catch_backtrace())
        return :error
    end
end

# SUCCESS landing: latch the new state, then publish the transition_event (the
# async observation channel). Publishing after the latch means a manager that
# reacts to the event always reads the already-updated state via get_state.
function _land!(ln::LifecycleNode, origin::LifecycleState, target::LifecycleState,
                transition_id::UInt8)
    @atomic ln._state = target
    _publish_transition_event(ln, transition_id, origin, target)
    nothing
end

# ERROR processing: run `on_error` (itself under the three-way). Its SUCCESS means
# the node cleaned up and recovers to `Unconfigured`; any other outcome (decline or
# a second throw) drops the node to `Finalized` — the safe terminal. `on_error`
# returning `failure` is treated as "could not recover" (→ Finalized), matching the
# rclcpp ERROR-processing contract. Shutdown (`target` Finalized) is the carve-out:
# it is the terminal path in the managed-node spec, so it lands `Finalized` even
# when `on_error` recovers (`on_error` is cleanup, not a way back to service).
function _handle_error!(ln::LifecycleNode, origin::LifecycleState, target::LifecycleState,
                        label::AbstractString)
    recovered = _run_transition(ln, ln.on_error, "$(label):on_error")
    if recovered === :success && target !== Finalized()
        _land!(ln, origin, Unconfigured(), TRANSITION_ON_ERROR_SUCCESS)
    else
        _land!(ln, origin, Finalized(), TRANSITION_ON_ERROR_FAILURE)
    end
    return :error
end

# ── control surface ──────────────────────────────────────────────────────────────
# The five `lifecycle_msgs` services + the `~/transition_event` topic, declared on
# the wrapped node so they ride the normal entity machinery (liveliness, graph,
# close-on-node-close), and registered in `ln._control` so the gate exempts them:
# an external manager must reach `change_state`/`get_state` while the node is
# inactive, which is the whole point. The handlers are thin marshals over the
# transition drivers + `state(ln)`.

# The vendored `lifecycle_msgs` types — always in-package. Aliased here so the wire
# layer reads cleanly.
const _LC_msg = Interfaces.lifecycle_msgs.msg
const _LC_srv = Interfaces.lifecycle_msgs.srv
const LCState                = _LC_msg.State
const LCTransition           = _LC_msg.Transition
const LCTransitionEvent      = _LC_msg.TransitionEvent
const LCTransitionDescription = _LC_msg.TransitionDescription
const ChangeState_Request    = _LC_srv.ChangeState_Request
const ChangeState_Response   = _LC_srv.ChangeState_Response
const GetState_Request       = _LC_srv.GetState_Request
const GetState_Response      = _LC_srv.GetState_Response
const GetAvailableStates_Request  = _LC_srv.GetAvailableStates_Request
const GetAvailableStates_Response = _LC_srv.GetAvailableStates_Response
const GetAvailableTransitions_Request  = _LC_srv.GetAvailableTransitions_Request
const GetAvailableTransitions_Response = _LC_srv.GetAvailableTransitions_Response
const GetTransitionGraph_Request  = _LC_srv.GetTransitionGraph_Request
const GetTransitionGraph_Response = _LC_srv.GetTransitionGraph_Response

# Register a control-surface handle's entity so the dispatch gate exempts it (the
# manager must reach it while the node is inactive). Returns the handle.
function _exempt!(ln::LifecycleNode, handle)
    push!(ln._control, entity(handle))
    return handle
end

# The wire `State` for one of our primary states (id + label, the single source of
# truth above). Used by `get_state` / `get_available_states` / transition events.
_wire_state(s::LifecycleState) = LCState(; id = _state_id(s), label = _state_label(s))

# The four primary states, in id order — the `get_available_states` reply and the
# row set the transition graph is built from.
_primary_states() = (Unconfigured(), Inactive(), Active(), Finalized())

# The static transition graph: every (transition_id → goal) edge keyed by origin
# state. `get_transition_graph` is the full set; `get_available_transitions` is the
# subset whose origin is the live state. Edges mirror `_apply_transition!`.
function _transitions_from(s::LifecycleState)
    s === Unconfigured() && return ((TRANSITION_CONFIGURE, Inactive()),
                                    (TRANSITION_UNCONFIGURED_SHUTDOWN, Finalized()))
    s === Inactive()     && return ((TRANSITION_ACTIVATE, Active()),
                                    (TRANSITION_CLEANUP, Unconfigured()),
                                    (TRANSITION_INACTIVE_SHUTDOWN, Finalized()))
    s === Active()       && return ((TRANSITION_DEACTIVATE, Inactive()),
                                    (TRANSITION_ACTIVE_SHUTDOWN, Finalized()))
    return ()   # Finalized is terminal
end

# Human-readable transition labels (the `lifecycle_msgs/Transition.label` field).
function _transition_label(id::UInt8)
    id == TRANSITION_CONFIGURE             && return "configure"
    id == TRANSITION_CLEANUP               && return "cleanup"
    id == TRANSITION_ACTIVATE              && return "activate"
    id == TRANSITION_DEACTIVATE            && return "deactivate"
    id == TRANSITION_UNCONFIGURED_SHUTDOWN && return "unconfigured_shutdown"
    id == TRANSITION_INACTIVE_SHUTDOWN     && return "inactive_shutdown"
    id == TRANSITION_ACTIVE_SHUTDOWN       && return "active_shutdown"
    id == TRANSITION_ON_ERROR_SUCCESS      && return "on_error_success"
    id == TRANSITION_ON_ERROR_FAILURE      && return "on_error_failure"
    return "transition_$(id)"
end

# A wire `TransitionDescription` (transition + start/goal states) for one edge.
_wire_transition_desc(origin::LifecycleState, id::UInt8, goal::LifecycleState) =
    LCTransitionDescription(;
        transition  = LCTransition(; id = id, label = _transition_label(id)),
        start_state = _wire_state(origin),
        goal_state  = _wire_state(goal))

# The `TransitionDescription`s available from a given origin state.
_available_descs(s::LifecycleState) =
    LCTransitionDescription[_wire_transition_desc(s, id, goal)
                            for (id, goal) in _transitions_from(s)]

# The full static graph (every edge, every origin).
_graph_descs() =
    LCTransitionDescription[_wire_transition_desc(s, id, goal)
                            for s in _primary_states()
                            for (id, goal) in _transitions_from(s)]

"""
    _wire_control_surface!(ln, msgs)

Declare the always-live control surface: the `~/transition_event` topic and
the five `lifecycle_msgs` services, each registered in `ln._control` so the
dispatch gate exempts it (an external manager reaches `change_state`/`get_state`
while the node is inactive — the whole point). The vendored `lifecycle_msgs`
types are always in-package, so this always wires; `msgs` is accepted for
source-compat but unused. Handlers are thin marshals over the transition drivers +
`state(ln)`:
  - `~/change_state`             → `_apply_transition!(req.transition.id)`, `success`
  - `~/get_state`                → the live primary `State`
  - `~/get_available_states`     → the four primary `State`s
  - `~/get_available_transitions`→ edges out of the live state
  - `~/get_transition_graph`     → the full static graph
"""
function _wire_control_surface!(ln::LifecycleNode, msgs)
    node = ln.node

    # change_state: drive the requested transition; reply success on landing. A
    # transition that isn't valid from the live state throws `ArgumentError` in
    # `_drive!` — for the wire contract that's a clean `success=false`, not a service
    # error (which would raise on the caller).
    _exempt!(ln, Service(node, "~/change_state", ChangeState_Request) do req
        result = try
            _apply_transition!(ln, req.transition.id)
        catch err
            err isa ArgumentError || rethrow()
            :failure
        end
        return ChangeState_Response(; success = (result === :success))
    end)

    _exempt!(ln, Service(node, "~/get_state", GetState_Request) do _req
        return GetState_Response(; current_state = _wire_state(state(ln)))
    end)

    _exempt!(ln, Service(node, "~/get_available_states", GetAvailableStates_Request) do _req
        return GetAvailableStates_Response(;
            available_states = LCState[_wire_state(s) for s in _primary_states()])
    end)

    _exempt!(ln, Service(node, "~/get_available_transitions",
                         GetAvailableTransitions_Request) do _req
        return GetAvailableTransitions_Response(;
            available_transitions = _available_descs(state(ln)))
    end)

    _exempt!(ln, Service(node, "~/get_transition_graph",
                         GetTransitionGraph_Request) do _req
        return GetTransitionGraph_Response(; available_transitions = _graph_descs())
    end)

    # transition_event: the async observation channel. Transient-local + reliable so
    # a late-joining manager sees the latest transition (rclcpp's latched event topic).
    pub = Publisher(node, "~/transition_event", LCTransitionEvent;
                    qos = QosProfile(durability = :transient_local,
                                     reliability = :reliable, depth = 1))
    _exempt!(ln, pub)
    ln._event_pub = pub
    return ln
end

# Map a `lifecycle_msgs/Transition` id (from a wire `change_state` request) onto the
# matching driver. The single dispatch point the wire `change_state` handler calls;
# kept here so the id→driver mapping lives with the drivers. An unknown/private id
# is a `:failure` (the request couldn't be applied), not a throw — the reply carries
# `success=false`.
function _apply_transition!(ln::LifecycleNode, transition_id::Integer)
    id = UInt8(transition_id)
    id == TRANSITION_CONFIGURE   && return configure!(ln)
    id == TRANSITION_CLEANUP     && return cleanup!(ln)
    id == TRANSITION_ACTIVATE    && return activate!(ln)
    id == TRANSITION_DEACTIVATE  && return deactivate!(ln)
    (id == TRANSITION_UNCONFIGURED_SHUTDOWN ||
     id == TRANSITION_INACTIVE_SHUTDOWN ||
     id == TRANSITION_ACTIVE_SHUTDOWN) && return shutdown!(ln)
    return :failure
end

# Publish a `TransitionEvent` on `~/transition_event` (the async observation
# channel). No-op before the surface is wired (`_event_pub === nothing`).
# `TransitionEvent.timestamp` is a bare `uint64` of nanoseconds (NOT a
# builtin_interfaces/Time), so stamp it with the node's ROS-clock ns directly; the
# start/goal `State`s come from the `_state_id`/`_state_label` of origin/target.
function _publish_transition_event(ln::LifecycleNode, transition_id::UInt8,
                                   origin::LifecycleState, target::LifecycleState)
    pub = ln._event_pub
    pub === nothing && return nothing
    event = LCTransitionEvent(;
        timestamp   = UInt64(nanoseconds(Dates.now(ln.node))),
        transition  = LCTransition(; id = transition_id,
                                   label = _transition_label(transition_id)),
        start_state = _wire_state(origin),
        goal_state  = _wire_state(target))
    publish(pub, event)
    return nothing
end

# ── teardown ─────────────────────────────────────────────────────────────────────

"""
    close(ln::LifecycleNode)

Finalize and tear down the managed node. Runs [`shutdown!`](@ref) (the
lifecycle drain, normally landing in [`Finalized`](@ref)) if not already terminal,
closes the wrapped [`Node`] — which undeclares every entity (control surface +
application) and the node token — then drops the gate registration. Idempotent.
"""
function Base.close(ln::LifecycleNode)
    isopen(ln) || return nothing
    # Run the shutdown transition while still open, so `shutdown!`'s own open-check
    # passes and `on_shutdown` fires + observers see the Finalized event before the
    # entities vanish. Guard it — a throwing transition must not abort teardown.
    # (`shutdown!` is idempotent: a concurrent close finds Finalized and no-ops, and
    # the `@atomicswap` below is the single-winner latch.)
    try
        state(ln) === Finalized() || shutdown!(ln)
    catch err
        @error "close(LifecycleNode): shutdown! failed" node=ln.node.fqn exception=(err, catch_backtrace())
    end
    (@atomicswap ln.open = false) || return nothing
    close(ln.node)            # reaps every entity (control + application) + token
    # Unregister last: a gateless node reads as active (the unmanaged fast path), so dropping
    # the entry earlier would let in-flight dispatch fire mid-teardown.
    _unregister_gate!(ln)
    nothing
end
