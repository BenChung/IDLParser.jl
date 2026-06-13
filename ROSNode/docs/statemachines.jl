# Generates the interactive state-machine definitions the docs render with XState.
#
# Each machine is emitted in XState's `createMachine` config shape (paste-able into
# Stately's visualizer) plus a sibling `meta` block the renderer uses for labels and
# styling. The lifecycle machine's primary states and edges are read out of ROSNode's
# own reified definitions (`_primary_states`, `_transitions_from`), and the goal
# machine's state set out of the `GoalState` enum — so adding a state or edge in the
# runtime surfaces here rather than drifting. The three-way / error-recovery overlays
# (the SUCCESS/FAILURE/ERROR settlement pattern) are uniform and encoded once, below.

module StateMachines

using ROSNode
const R = ROSNode

# ── ordered JSON (no dependency; preserves insertion order for readable output) ──
# Objects are `Vector{Pair{String,Any}}` so key order is stable across builds.
obj(ps::Pair...) = collect(Pair{String,Any}, ps)

_esc(s) = replace(string(s), '\\' => "\\\\", '"' => "\\\"", '\n' => "\\n")
jval(io, s::AbstractString) = print(io, '"', _esc(s), '"')
jval(io, s::Symbol)         = jval(io, string(s))
jval(io, b::Bool)           = print(io, b ? "true" : "false")
jval(io, n::Integer)        = print(io, n)
jval(io, ::Nothing)         = print(io, "null")
function jval(io, ps::Vector{<:Pair})
    print(io, '{')
    for (i, p) in enumerate(ps)
        i > 1 && print(io, ',')
        jval(io, p.first); print(io, ':'); jval(io, p.second)
    end
    print(io, '}')
end
function jval(io, xs::AbstractVector)
    print(io, '[')
    for (i, x) in enumerate(xs)
        i > 1 && print(io, ',')
        jval(io, x)
    end
    print(io, ']')
end
json(x) = (io = IOBuffer(); jval(io, x); String(take!(io)))

# ── 1. managed-node lifecycle (primary states + edges read from the runtime) ─────

# Transient state running each non-shutdown transition's callback. The label→name
# map is 1:1 with the origin state here (each callback has a single valid origin),
# so a transient's FAILURE edge reverts unambiguously to that origin.
const _TRANSIENT = Dict("configure" => "Configuring", "activate" => "Activating",
                        "deactivate" => "Deactivating", "cleanup" => "CleaningUp")

_lstate_desc(::R.Unconfigured) = "No configuration loaded, no resources held. The initial state, and where a successful cleanup or error-recovery returns the node."
_lstate_desc(::R.Inactive)     = "Configured but idle. Entities exist and stay visible in the graph; the dispatch gate holds them shut — publishers drop, subscriptions and timers stay quiet, services error-reply."
_lstate_desc(::R.Active)       = "Live. The gate is open: publishers send, subscriptions and timers fire, services serve. The only state for which isactive is true."
_lstate_desc(::R.Finalized)    = "Terminal. Reached by shutdown from any non-terminal state, or when error processing fails to recover. No edges lead out."

function lifecycle()
    states   = Pair{String,Any}[]
    smeta    = Pair{String,Any}[]
    seen_tr  = String[]                       # transients already defined

    for s in R._primary_states()
        sid = string(s)
        on  = Pair{String,Any}[]
        for (tid, goal) in R._transitions_from(s)
            label = R._transition_label(tid)
            if goal === R.Finalized()         # a shutdown edge → the shared Finalizing transient
                push!(on, label => "Finalizing")
            else
                tname = _TRANSIENT[label]
                push!(on, label => tname)
                if !(tname in seen_tr)
                    push!(seen_tr, tname)
                    push!(states, tname => obj("on" => obj(
                        "success" => string(goal), "failure" => sid,
                        "error"   => "ErrorProcessing")))
                    push!(smeta, tname => obj("kind" => "transient",
                        "desc" => "Running on_$(label). The callback's outcome picks the edge: " *
                                  "returns ⇒ success, returns the failure token ⇒ failure (revert), throws ⇒ error."))
                end
            end
        end
        push!(states, sid => (isempty(on) ? obj("type" => "final") : obj("on" => obj(on...))))
        push!(smeta,  sid => obj("kind" => (isempty(on) ? "terminal" : "primary"),
                                 "desc" => _lstate_desc(s)))
    end

    # Shared shutdown transient: success or error both land Finalized — shutdown is
    # the terminal path, so error processing cannot divert it.
    push!(states, "Finalizing" => obj("on" => obj("success" => "Finalized", "error" => "Finalized")))
    push!(smeta,  "Finalizing" => obj("kind" => "transient",
        "desc" => "Running on_shutdown. Success and error both land Finalized — shutdown is terminal, " *
                  "so on_error here is cleanup, not a way back to service."))
    # Error processing for a non-shutdown transition.
    push!(states, "ErrorProcessing" => obj("on" => obj(
        "recovered" => "Unconfigured", "not_recovered" => "Finalized")))
    push!(smeta,  "ErrorProcessing" => obj("kind" => "error",
        "desc" => "on_error runs (itself under the three-way). Its success recovers the node to Unconfigured; " *
                  "a decline or a second throw drops it to Finalized."))

    return obj(
        "title"   => "Managed-node lifecycle",
        "summary" => "A LifecycleNode an external orchestrator drives (~/change_state) and observes " *
                     "(~/get_state, ~/transition_event). Each transition runs its callback under the " *
                     "settlement three-way; click a transition, then choose the callback's outcome.",
        "machine" => obj("id" => "lifecycle", "initial" => "Unconfigured", "states" => obj(states...)),
        "meta"    => obj("states" => obj(smeta...)))
end

# ── 2. action goal lifecycle (state set read from the GoalState enum) ────────────

# action_msgs/GoalStatus codes that label a live or terminal goal. `:unknown` is the
# zero sentinel and never a reached state; the local-only `:rejected` is added below.
const _GOAL_DISPLAY = Dict(:accepted => "Accepted", :executing => "Executing",
    :canceling => "Canceling", :succeeded => "Succeeded", :canceled => "Canceled",
    :aborted => "Aborted")

function goal()
    # Assert the enum still matches what the edges below assume (surfaces a drift if
    # a GoalState value is added/renamed in support.jl).
    reached = sort(String[string(R._state_symbol(s)) for s in instances(R.GoalState)
                          if R._state_symbol(s) !== :unknown])
    expected = sort(collect(keys(_GOAL_DISPLAY)) .|> string)
    reached == expected || error("goal(): GoalState enum drifted from the documented machine: " *
                                 "got $(reached), expected $(expected)")

    states = obj(
        "Requested" => obj("on" => obj("accept" => "Accepted", "defer" => "Accepted", "reject" => "Rejected")),
        "Accepted"  => obj("on" => obj("execute" => "Executing", "cancel" => "Canceling")),
        "Executing" => obj("on" => obj("succeed" => "Succeeded", "abort" => "Aborted", "cancel" => "Canceling")),
        "Canceling" => obj("on" => obj("canceled" => "Canceled", "succeed" => "Succeeded", "abort" => "Aborted")),
        "Succeeded" => obj("type" => "final"),
        "Canceled"  => obj("type" => "final"),
        "Aborted"   => obj("type" => "final"),
        "Rejected"  => obj("type" => "final"))

    smeta = obj(
        "Requested" => obj("kind" => "primary",  "desc" => "A send_goal request awaiting the server's on_goal decision."),
        "Accepted"  => obj("kind" => "primary",  "desc" => "Accepted; not yet executing. accept runs on_accepted at once; defer leaves it here for the owner to execute later."),
        "Executing" => obj("kind" => "primary",  "desc" => "The handler is running. Publishes feedback; checkpoints observe cancellation."),
        "Canceling" => obj("kind" => "transient","desc" => "An accepted cancel moved it here. feedback!/checkpoint throw Cancelled at the next yield point, unwinding to Canceled."),
        "Succeeded" => obj("kind" => "terminal", "desc" => "Ran to completion. Result cached for get_result replay."),
        "Canceled"  => obj("kind" => "terminal", "desc" => "Settled canceled — explicitly, or by the fail-safe mapping a thrown Cancelled."),
        "Aborted"   => obj("kind" => "error",    "desc" => "Settled aborted — explicitly, or the fail-safe outcome of any other throw / an empty return."),
        "Rejected"  => obj("kind" => "terminal", "desc" => "The server declined (on_goal returned reject). A local handle state; never registered server-side."))

    return obj(
        "title"   => "Action goal lifecycle",
        "summary" => "The server-side goal machine (action_msgs/GoalStatus). The fail-safe settlement " *
                     "fills the result cell exactly once: a thrown Cancelled ⇒ Canceled, any other throw " *
                     "or an empty return ⇒ Aborted (see the settlement three-way).",
        "machine" => obj("id" => "goal", "initial" => "Requested", "states" => states),
        "meta"    => obj("states" => smeta))
end

# ── 3. the settlement three-way (the shared SUCCESS/FAILURE/ERROR pattern) ───────

function threeway()
    states = obj(
        "Running" => obj("on" => obj("returns" => "Success", "declines" => "Failure", "throws" => "Error")),
        "Success" => obj("type" => "final"),
        "Failure" => obj("type" => "final"),
        "Error"   => obj("type" => "final"))
    smeta = obj(
        "Running" => obj("kind" => "primary", "desc" => "The user callback runs."),
        "Success" => obj("kind" => "terminal","desc" => "Returned normally. Lifecycle: land the target state. Service: reply-ok. Action: SUCCEEDED."),
        "Failure" => obj("kind" => "transient","desc" => "Returned the failure/failed token — a clean decline. Lifecycle: revert to origin. Service: query error reply (call raises)."),
        "Error"   => obj("kind" => "error",   "desc" => "Threw. Lifecycle: on_error runs (recover or Finalized). Service: query error reply. Action: ABORTED (or CANCELED for a thrown Cancelled)."))
    return obj(
        "title"   => "Settlement three-way",
        "summary" => "One pattern reused by lifecycle transitions, service handlers, and action goals: " *
                     "every callback exit is exactly one of returns / declines / throws.",
        "machine" => obj("id" => "threeway", "initial" => "Running", "states" => states),
        "meta"    => obj("states" => smeta))
end

# ── 4. error-recovery zoom (the non-obvious bit, called out on its own) ──────────

function recovery()
    states = obj(
        "CallbackThrew" => obj("on" => obj("on_error" => "OnError")),
        "OnError"       => obj("on" => obj(
            "recovers"          => "Recovered",
            "declines_or_throws"=> "Finalized",
            "was_shutdown"      => "Finalized")),
        "Recovered"     => obj("type" => "final"),
        "Finalized"     => obj("type" => "final"))
    smeta = obj(
        "CallbackThrew" => obj("kind" => "error",    "desc" => "A transition callback threw (anything but ShutdownException, which propagates untouched)."),
        "OnError"       => obj("kind" => "transient","desc" => "on_error runs under the three-way to release what the callback acquired."),
        "Recovered"     => obj("kind" => "primary",  "desc" => "on_error returned cleanly on a non-shutdown transition ⇒ the node recovers to Unconfigured, ready to reconfigure."),
        "Finalized"     => obj("kind" => "terminal", "desc" => "on_error declined or threw — OR the failing transition was a shutdown. Shutdown error is always terminal: recovery cannot return a node to service from the terminal path."))
    return obj(
        "title"   => "Error recovery",
        "summary" => "What a thrown transition callback does. The non-obvious rule: a shutdown that errors " *
                     "lands Finalized regardless of on_error, because shutdown is the terminal path.",
        "machine" => obj("id" => "recovery", "initial" => "CallbackThrew", "states" => states),
        "meta"    => obj("states" => smeta))
end

# A static Mermaid `stateDiagram-v2` for the same machine — the at-a-glance
# schematic the docs render alongside the interactive stepper. Built from the same
# states/edges/kinds, so it can't drift. classDef colors mirror the widget's kinds.
function mermaid_spec(machine, meta)
    initial = first(p.second for p in machine if p.first == "initial")
    states  = first(p.second for p in machine if p.first == "states")
    smeta   = first(p.second for p in meta if p.first == "states")
    io = IOBuffer()
    println(io, "stateDiagram-v2")
    println(io, "    [*] --> ", initial)
    for (name, cfg) in states
        on  = nothing
        fin = false
        for p in cfg
            p.first == "on"   && (on = p.second)
            p.first == "type" && p.second == "final" && (fin = true)
        end
        on === nothing || for (ev, tgt) in on
            println(io, "    ", name, " --> ", tgt, " : ", ev)
        end
        fin && println(io, "    ", name, " --> [*]")
    end
    println(io, "    classDef primary fill:#2f6feb18,stroke:#2f6feb,color:#2f6feb;")
    println(io, "    classDef transient fill:#b8860b18,stroke:#b8860b,color:#9a6f08;")
    println(io, "    classDef error fill:#d1242f18,stroke:#d1242f,color:#d1242f;")
    println(io, "    classDef terminal fill:#6e778118,stroke:#6e7781,color:#6e7781;")
    for (name, m) in smeta
        kind = first(p.second for p in m if p.first == "kind")
        println(io, "    class ", name, " ", kind)
    end
    return String(take!(io))
end

function all_machines()
    ms = obj("lifecycle" => lifecycle(), "goal" => goal(),
             "threeway" => threeway(), "recovery" => recovery())
    for p in ms                                  # inject the source-derived Mermaid schematic
        machine = first(q.second for q in p.second if q.first == "machine")
        meta    = first(q.second for q in p.second if q.first == "meta")
        push!(p.second, "mermaid" => mermaid_spec(machine, meta))
    end
    return ms
end

"Write the machines as a JS asset that sets `window.ROSNODE_MACHINES`."
function write_js(path::AbstractString)
    open(path, "w") do io
        println(io, "// AUTO-GENERATED by docs/statemachines.jl — do not edit by hand.")
        println(io, "// Machines are read from ROSNode's reified definitions at doc-build time.")
        print(io, "window.ROSNODE_MACHINES = ")
        jval(io, all_machines())
        println(io, ";")
    end
    return path
end

end # module

# Script entry: print a readable dump and write the asset next to this file's docs.
if abspath(PROGRAM_FILE) == @__FILE__
    using ROSNode
    for (id, m) in StateMachines.all_machines()
        println("# ", id, " — ", first(p.second for p in m if p.first == "title"))
        println(StateMachines.json(first(p.second for p in m if p.first == "machine")))
        println()
    end
    out = joinpath(@__DIR__, "src", "assets", "rosnode-machines.js")
    StateMachines.write_js(out)
    println("wrote ", out)
end
