# §14.2 Managed (lifecycle) nodes. A distinct `LifecycleNode` wraps a plain `Node`
# and adds the ROS2 managed-node contract: a state machine an external orchestrator
# drives and observes. Two reasons for a distinct type over a `Node` flag: a plain
# `Node` stays always-active with *no* gating branch, and the lifecycle surface
# (states, transitions, control services) lives off to the side without bloating the
# hot Node struct.
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
#   3. Gating — while not `Active`, every *application* entity on the node is gated
#      at dispatch (publishers drop, subs/timers don't fire, services error-reply).
#      Gate-at-dispatch (not declaration) keeps inactive entities visible in the
#      graph; gate-*everything* (not just publishers, the rclcpp footgun) makes
#      `on_activate`/`on_deactivate` usually empty.

using ROSZenoh: ROSZenoh, QosProfile, default_qos

export LifecycleNode, LifecycleState, Unconfigured, Inactive, Active, Finalized,
       state, isactive, inner_node,
       configure!, activate!, deactivate!, cleanup!, shutdown!

# ── state machine (§14.2) ─────────────────────────────────────────────────────
# The four *primary* states as singletons of a sealed abstract type — same shape as
# `ClockSource`/`SettlementStatus`, so `state(node)` dispatches and an unknown state
# is a `MethodError`, not a silent wrong branch. The transition states
# (Configuring/Activating/…) are internal: we are "in" one only for the duration of
# a callback, so they need no reified value here — `_transition!` is the scope.

"Sealed tag for the four ROS2 lifecycle primary states (§14.2)."
abstract type LifecycleState end

"Immediately after construction; no configuration loaded (rclcpp `PRIMARY_STATE_UNCONFIGURED`)."
struct Unconfigured <: LifecycleState end
"Configured but idle — entities exist but are gated; not processing (`PRIMARY_STATE_INACTIVE`)."
struct Inactive     <: LifecycleState end
"The live state — application entities fire (`PRIMARY_STATE_ACTIVE`)."
struct Active       <: LifecycleState end
"Terminal — `shutdown!` ran, the node is finalized (`PRIMARY_STATE_FINALIZED`)."
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

# ── LifecycleNode (§14.2) ──────────────────────────────────────────────────────
# Wraps a plain `Node` (entities are created against the wrapped node, so they get
# the full §6 id/token/route/graph lifecycle for free) and adds the managed-node
# state machine + callbacks + control surface. The current state is atomic — the
# gate predicate reads it on every dispatch from arbitrary handler tasks, and the
# control services drive transitions concurrently with `configure!`/… called
# in-process; a per-node `lock` serializes the transitions themselves so two racing
# `change_state` requests can't interleave callback execution.

"""
    LifecycleNode(ctx, name; namespace=nothing, enclave=nothing, autostart=false,
                  on_configure=…, on_activate=…, on_deactivate=…, on_cleanup=…,
                  on_shutdown=…, on_error=…, msgs=nothing) -> LifecycleNode

A ROS2 *managed* node (§14.2): a distinct node type wrapping a plain [`Node`] with
a state machine an external orchestrator drives (`~/change_state`) and observes
(`~/get_state`, `~/transition_event`). Starts in [`Unconfigured`](@ref).

Transitions run user callbacks under the **action three-way** (§9): a callback
returns normally ⇒ SUCCESS (land in the target state); returns the
[`failure`](@ref) token ⇒ FAILURE (a clean "can't right now" — revert to the
origin state); throws ⇒ ERROR (`on_error` runs; its SUCCESS recovers to
`Unconfigured`, otherwise the node goes `Finalized`). Each callback takes the
`LifecycleNode` and defaults to a no-op.

While the node is not [`Active`](@ref), every *application* entity created on it is
gated **at dispatch** ([`isactive`](@ref)): publishers drop, subscriptions/timers
don't fire, services error-reply. The control surface is exempt (always live).

`autostart=true` runs `configure!` then `activate!` on construction (the simple
single-owner case). The wire control surface (the five `lifecycle_msgs` services +
the `~/transition_event` topic) is always declared from the vendored
`lifecycle_msgs` types; `msgs` is accepted for source-compat but no longer gates
generation.

Entities and the node die with `close` / the Context drain (§14), which runs
`shutdown!` → `Finalized` first.
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
        configure!(ln)
        state(ln) === Inactive() && activate!(ln)
    end
    return ln
end

# Default callback — a managed node with no behavior still transitions cleanly.
_lc_noop(::LifecycleNode) = nothing

"""
    inner_node(ln::LifecycleNode) -> Node

The wrapped plain [`Node`] (§14.2). Application entities are created against it —
`Publisher(inner_node(ln), "image", T)`, `Timer(inner_node(ln), …) do … end` — so
they register on the node the gate is keyed by and are silenced automatically while
the managed node is not [`Active`](@ref). (The pattern constructors are typed on
`Node`; unwrap here rather than passing the `LifecycleNode`.)
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

# ── state queries + the gate predicate (§14.2) ─────────────────────────────────

"""
    state(ln::LifecycleNode) -> LifecycleState

The node's current primary lifecycle state ([`Unconfigured`](@ref)/[`Inactive`](@ref)/
[`Active`](@ref)/[`Finalized`](@ref)). Read lock-free (atomic); the value an
external `get_state` query reports.
"""
state(ln::LifecycleNode) = @atomic ln._state

"""
    isactive(node) -> Bool
    isactive(ln::LifecycleNode) -> Bool

The dispatch gate predicate (§14.2). A [`LifecycleNode`] is active only in the
[`Active`](@ref) state. A plain [`Node`] is active unless it is the wrapped node of
some `LifecycleNode` (the gate registry) — an unmanaged node is *always* active,
the no-branch fast path. Hooked into the data-plane dispatch paths so that while a
managed node is not Active, its application entities are silenced automatically:
publishers drop, subscriptions/timers don't fire, services error-reply.
"""
isactive(ln::LifecycleNode) = state(ln) === Active()

# The `Entity.node` is always a plain `Node`, so this is the predicate the dispatch
# hooks reach through `isactive(e)`. A plain Node may still *be* a managed node's
# wrapped node — look it up once; absent ⇒ always active.
function isactive(node::Node)
    g = _gate_for(node)
    g === nothing ? true : isactive(g::LifecycleNode)
end

"""
    isactive(entity::Entity) -> Bool

Whether `entity` should dispatch right now (§14.2). The *control surface* of a
LifecycleNode is exempt (always live, so an external manager can drive/observe an
inactive node); every other entity follows its node's [`isactive`](@ref). This is
the single predicate the dispatch hooks (publish / subscription / timer / service)
consult.
"""
function isactive(e::Entity)
    g = _gate_for(e.node)
    g === nothing && return true                  # plain Node ⇒ always
    e in (g::LifecycleNode)._control && return true   # control surface exempt
    return isactive(g::LifecycleNode)
end

# Dispatch hooks (§14.2 "gate everything, at dispatch"). The gate is a single
# `isactive(e::Entity)` guard threaded into each data-plane dispatch site. Because
# this file is included last and `isactive` is a normal late-bound method, the four
# sites can each add a one-line check (they already short-circuit on `isopen(e)`,
# which is the natural place for the gate to ride alongside):
#
#   • pubsub.jl `publish`       — after `isopen(e) || return nothing`:
#         isactive(e) || return nothing                       # Publisher ⇒ drop
#   • node.jl  `_run_handler`   — at entry (before decode):
#         isactive(e) || return nothing                       # Subscription ⇒ no fire
#   • service.jl `_serve_query` — before running the handler:
#         isactive(e) || (reply_err(query, "node inactive"); finalize(query.q); return)
#   • time.jl  Timer `_start!`  — the timer's entity isn't in scope; the gate rides
#         on the user `f` closure instead (the Timer is created on the node, so the
#         body's `publish`/state access is already gated transitively). A direct
#         timer gate would need the owning node on the `Timer` — deferred.
#
# A plain `Node` makes every `isactive` call return `true` with one IdDict lookup,
# so the guard is free for the common (unmanaged) case.

# ── transitions: the action three-way, reused (§14.2/§9) ───────────────────────
# Each public transition validates the origin state, runs the user callback under
# the three-way, and on SUCCESS latches the target + publishes a `transition_event`.
# The three exits mirror action settlement exactly:
#   callback returns normally          ⇒ SUCCESS  → land in `target`
#   callback returns the `failure` tok ⇒ FAILURE  → revert to `origin` (clean no-op)
#   callback throws                    ⇒ ERROR    → `on_error`; its SUCCESS recovers
#                                                    to Unconfigured, else Finalized
# A non-applicable transition for the current state is an `ArgumentError` (the
# external `change_state` maps this to a `success=false` reply; in-process it
# surfaces to the caller).

"""
    TransitionResult

The outcome of a lifecycle transition: `:success` (landed in the target state),
`:failure` (callback declined — reverted to origin), or `:error` (callback threw —
`on_error` ran, recovered to `Unconfigured` or fell to `Finalized`). Returned by
[`configure!`](@ref)/[`activate!`](@ref)/… for in-process callers; the wire
`change_state` reply reports `success = (result === :success)`.
"""
const TransitionResult = Symbol   # one of :success / :failure / :error

"""
    configure!(ln) -> TransitionResult

`Unconfigured → Inactive`, running `on_configure` (§14.2). The conventional place
to create the node's (gated-until-Active) entities and acquire resources.
"""
configure!(ln::LifecycleNode) =
    _drive!(ln, Unconfigured(), Inactive(), ln.on_configure,
            TRANSITION_CONFIGURE, "configure")

"""
    activate!(ln) -> TransitionResult

`Inactive → Active`, running `on_activate` (§14.2). Usually empty — gating is
automatic, so activation is just the state flip that un-silences the node's
entities.
"""
function activate!(ln::LifecycleNode)
    r = _drive!(ln, Inactive(), Active(), ln.on_activate,
                TRANSITION_ACTIVATE, "activate")
    # D4: now that the gate is open (state latched Active inside `_drive!`), re-run
    # each transient_local subscription's latched-history query so the node picks up
    # state it dropped while inactive. Novelty-gated, so an already-seen latched
    # sample isn't replayed across deactivate/reactivate cycles. A no-op for every
    # other entity. Best-effort: a re-latch failure must not unwind the transition.
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
    deactivate!(ln) -> TransitionResult

`Active → Inactive`, running `on_deactivate` (§14.2). Usually empty (gating is
automatic); the state flip re-silences the node.
"""
deactivate!(ln::LifecycleNode) =
    _drive!(ln, Active(), Inactive(), ln.on_deactivate,
            TRANSITION_DEACTIVATE, "deactivate")

"""
    cleanup!(ln) -> TransitionResult

`Inactive → Unconfigured`, running `on_cleanup` (§14.2). Releases what
`on_configure` acquired (close entities, free devices).
"""
cleanup!(ln::LifecycleNode) =
    _drive!(ln, Inactive(), Unconfigured(), ln.on_cleanup,
            TRANSITION_CLEANUP, "cleanup")

"""
    shutdown!(ln) -> TransitionResult

`{Unconfigured,Inactive,Active} → Finalized`, running `on_shutdown` (§14.2). The
terminal transition; valid from any non-terminal state (this is the lifecycle drain
`close(ctx)` reaches, §14). Idempotent — already `Finalized` is a `:success` no-op.
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
#   5. on ERROR runs `on_error` and lands in Unconfigured (its SUCCESS) or Finalized.
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
            return _handle_error!(ln, origin, label)
        end
    end
end

# Run one transition callback under the action three-way. Returns `:success` /
# `:failure` / `:error`. The `failure` *token* (core.jl) returned from the callback
# is the clean-decline sentinel — same vocabulary as service/action settlement, so
# there's nothing new to learn (DESIGN §14.2). Any throw (incl. `Cancelled`, which
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
# rclcpp ERROR-processing contract.
function _handle_error!(ln::LifecycleNode, origin::LifecycleState, label::AbstractString)
    recovered = _run_transition(ln, ln.on_error, "$(label):on_error")
    target = recovered === :success ? Unconfigured() : Finalized()
    _land!(ln, origin, target, TRANSITION_ACTIVATE)  # event id is best-effort here
    return :error
end

# ── control surface (§14.2) ────────────────────────────────────────────────────
# The five `lifecycle_msgs` services + the `~/transition_event` topic, declared on
# the wrapped node so they ride the normal §6/§8 machinery (liveliness, graph,
# close-on-node-close) — but registered in `ln._control` so the gate exempts them:
# an external manager must reach `change_state`/`get_state` *while the node is
# inactive*, which is the whole point. The handlers are thin marshals over the
# already-written transition drivers + `state(ln)`.

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

Declare the always-live control surface (§14.2): the `~/transition_event` topic and
the five `lifecycle_msgs` services, each registered in `ln._control` so the
dispatch gate exempts it (an external manager reaches `change_state`/`get_state`
*while the node is inactive* — the whole point). The vendored `lifecycle_msgs`
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
# channel, §14.2). No-op before the surface is wired (`_event_pub === nothing`).
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

# ── teardown (§14) ─────────────────────────────────────────────────────────────

"""
    close(ln::LifecycleNode)

Finalize and tear down the managed node (§14.2/§14). Runs `shutdown!` → `Finalized`
(the lifecycle drain) if not already terminal, drops the gate registration, then
closes the wrapped [`Node`] — which undeclares every entity (control surface +
application) and the node token. Idempotent.
"""
function Base.close(ln::LifecycleNode)
    isopen(ln) || return nothing
    # Run the shutdown transition *before* latching closed, so `shutdown!`'s own
    # open-check passes and `on_shutdown` fires + observers see the Finalized event
    # before the entities vanish. Guard it — a throwing transition must not abort
    # teardown. (`shutdown!` is idempotent: a concurrent close finds Finalized and
    # no-ops, and the `@atomicswap` below is the single-winner latch.)
    try
        state(ln) === Finalized() || shutdown!(ln)
    catch err
        @error "close(LifecycleNode): shutdown! failed" node=ln.node.fqn exception=(err, catch_backtrace())
    end
    (@atomicswap ln.open = false) || return nothing
    _unregister_gate!(ln)
    close(ln.node)            # reaps every entity (control + application) + token
    nothing
end
