# ── SingleFlight orchestrator ───────────────────────────────────────────────────
# An orchestrator over the low-level action API: one active goal plus a bounded
# queue, the common one-actuator pattern. Wire it with
# `on_accepted = g -> submit!(sched, g)` and `execute!(sched) do goal … end`.
# Pause works through `checkpoint`: the orchestrator flips a flag the body's
# `checkpoint` blocks on.

"""
    SingleFlight(; queue=4)

Action orchestrator that runs one accepted goal at a time with a bounded `queue`
of pending goals. Wire it via `on_accepted = g -> submit!(sched, g)` on an
[`ActionServer`](@ref) (which should `defer` in `on_goal`), then drive execution
with `execute!(sched) do goal … end`.

`SingleFlight` is a scheduling policy layered over the low-level API, separate
from the server itself; `pause`, `resume`, and `active_goal` are its own surface.

See the ROS 2 actions concept: https://docs.ros.org/en/rolling/Concepts/Basic/About-Actions.html
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

Enqueue an accepted (deferred) goal for the orchestrator to run; this is the
target of the `on_accepted = g -> submit!(sched, g)` wiring. Blocks when the
queue is full (backpressure); the goal stays `ACCEPTED` until [`execute!`](@ref)
picks it up.
"""
function submit!(sched::SingleFlight, goal::GoalHandle)
    put!(sched.queue, goal)
    nothing
end

"""
    execute!(body, sched::SingleFlight)
    execute!(sched::SingleFlight) do goal … end

Run the orchestrator loop: pull one goal at a time off the queue and `execute` it
with `body(goal)` (the same `Cancelled` + settlement wrapper), waiting for each to
finish before the next (single-flight). [`pause`](@ref) gates at the dispatch
boundary: while paused the loop holds the next goal in the queue. Spawned on its
own task; returns the task.
"""
function execute!(body::Function, sched::SingleFlight)
    Threads.@spawn begin
        for goal in sched.queue
            # Cooperative pause gate: hold the next goal while paused.
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

# `pause`/`resume` are unexported to avoid shadowing `Base.pause` (the CPU hint);
# reach them via `ROSNode.pause` / `ROSNode.resume`.
"Pause the orchestrator so queued goals wait at the dispatch boundary."
function pause(sched::SingleFlight; reason::AbstractString = "")
    @atomic sched.paused = true
    isempty(reason) || @info "SingleFlight paused" reason
    nothing
end

"Resume a paused orchestrator."
resume(sched::SingleFlight) = (@atomic sched.paused = false; nothing)

"The orchestrator's currently-running goal, or `nothing`."
active_goal(sched::SingleFlight) = @atomic sched._active

# ── ActionServer constructors ───────────────────────────────────────────────────
# `ActionServer` is a plain type because an action spans five endpoints rather
# than one `EndpointKind`. Both forms route to `_make_action_server`; the do-block
# form takes the body first, following the `Timer(f, …)` precedent.

ActionServer(node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; kwargs...)

ActionServer(body::Function, node::Node, name::AbstractString, ::Type{A}; kwargs...) where {A} =
    _make_action_server(node, name, A; body=body, kwargs...)
