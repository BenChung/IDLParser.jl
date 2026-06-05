# ── SingleFlight orchestrator (§9, optional helper) ─────────────────────────────
# An orchestrator over the low-level API (NOT an `ActionServer` feature, §9): one
# active goal + a bounded queue, the common one-actuator pattern. Built on
# `on_accepted = g -> submit!(sched, g)` + `execute!(sched) do goal … end`. Pause
# reuses `checkpoint` (the orchestrator flips a flag the body's `checkpoint`
# blocks on). Shipped as an optional helper, deliberately minimal — richer
# scheduling is the next rung (§15.2 deferred).

"""
    SingleFlight(; queue=4)

An optional action orchestrator (§9): runs one accepted goal at a time with a
bounded `queue` of pending goals. Wire it via `on_accepted = g -> submit!(sched, g)`
on an [`ActionServer`](@ref) (which should `defer` in `on_goal`), then drive
execution with `execute!(sched) do goal … end`. Not a server feature — *a* policy,
not *the* server.

This is the minimal shape; `pause`/`resume`/`queued_goals` are sketched as the
orchestrator's own surface (DESIGN §orchestration).
"""
mutable struct SingleFlight
    const queue::Channel{Any}            # pending GoalHandles, bounded
    @atomic paused::Bool
    const lock::ReentrantLock
    @atomic _active::Any                 # the running GoalHandle, or nothing
end

SingleFlight(; queue::Integer = 4) =
    SingleFlight(Channel{Any}(max(1, Int(queue))), false, ReentrantLock(), nothing)

"""
    submit!(sched::SingleFlight, goal)

Enqueue an accepted (deferred) goal for the orchestrator to run (§9). The
`on_accepted` wiring (`g -> submit!(sched, g)`). Blocks if the queue is full
(backpressure); the goal stays `ACCEPTED` until [`execute!`](@ref) picks it up.
"""
function submit!(sched::SingleFlight, goal::GoalHandle)
    put!(sched.queue, goal)
    nothing
end

"""
    execute!(body, sched::SingleFlight)
    execute!(sched::SingleFlight) do goal … end

Run the orchestrator loop: pull one goal at a time off the queue and `execute` it
with `body(goal)` (the same `Cancelled` + settlement wrapper, §9), waiting for
each to finish before the next (single-flight). Honors [`pause`](@ref) at the
dispatch boundary — while paused the loop holds the next goal in the queue rather
than starting it. Spawned on its own task; returns the task.

(DESIGN sketches a richer pause that also blocks a *running* goal's `checkpoint`;
that needs the orchestrator's flag threaded through the `GoalHandle`, a later
rung — this minimal helper gates between goals.)
"""
function execute!(body::Function, sched::SingleFlight)
    Threads.@spawn begin
        for goal in sched.queue
            # Pause gate: hold the next goal while paused (cooperative, polled).
            while (@atomic sched.paused)
                Base.sleep(0.02)
            end
            @atomic sched._active = goal
            try
                execute(goal::GoalHandle, body)
                # Single-flight: wait for the goal's task before the next.
                t = goal._task
                t === nothing || wait(t)
            catch err
                @error "SingleFlight.execute! goal failed" exception=(err, catch_backtrace())
            finally
                @atomic sched._active = nothing
            end
        end
    end
end
execute!(sched::SingleFlight, body::Function) = execute!(body, sched)

# `pause`/`resume` are the orchestrator's own surface (not exported — `pause`
# shadows the unexported `Base.pause` CPU hint otherwise; reach via `ROSNode.pause`).
"Pause the orchestrator: queued goals wait, and a running goal's `checkpoint` blocks (§9)."
function pause(sched::SingleFlight; reason::AbstractString = "")
    @atomic sched.paused = true
    isempty(reason) || @info "SingleFlight paused" reason
    nothing
end

"Resume a paused orchestrator (§9)."
resume(sched::SingleFlight) = (@atomic sched.paused = false; nothing)

"The orchestrator's currently-running goal, or `nothing` (introspection, §9)."
active_goal(sched::SingleFlight) = @atomic sched._active

# ── enum-instance call-methods: the §6 constructor spelling for ActionServer ────
# `ActionServer` is a plain type here (not an `EndpointKind` instance — an action
# spans five endpoints). Constructors route to `_make_action_server`: the
# do-block form takes the body first (the `Timer(f, …)` precedent, §6), the
# low-level form takes only kwargs.

# Low-level: ActionServer(node, name, A; on_goal, on_cancel, on_accepted, …).
ActionServer(node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; kwargs...)

# High-level do-block: ActionServer(node, name, A; concurrency, …) do goal … end.
ActionServer(body::Function, node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; body=body, kwargs...)
