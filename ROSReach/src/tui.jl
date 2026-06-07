# A live terminal view over the graph, with an interactive directed-reach command
# line. Redraws when discovery changes (debounced) or on a timer, and accepts
# `reach <target>` (passive) / `reach! <target>` (active probe) to answer "why
# can't I reach X" without leaving the view.
#
# Discipline (R7): the `on_change` listener fires on the index consumer task, so it
# must NOT do the blocking transport calls (router_zids/scout). It only flips a
# `dirty` flag; a separate refresher task does the actual draw.

using Dates: Dates

"""
    watch(x; transport=true, scout=false, interval=2.0, commands=true, clear=false)

Open a live view of the ROS graph and keep it refreshed. `x` is a [`ReachIndex`](@ref)
(used as-is) or a `Session`/`.session` holder (a fresh index is opened and closed
with the view). Redraws on every discovery change (debounced) and every `interval`
seconds; `transport=true` adds the transport summary, `scout=true` adds a
scouting (reachability) section, `clear=true` does a full-screen redraw.

With `commands=true` (default) it reads a command line:
  - `reach <target>`  — passive diagnosis of a locator / z_id / node name
  - `reach! <target>` — escalate to an active probe (TCP + throwaway Zenoh session)
  - `q`               — quit

With `commands=false` it just refreshes until Ctrl-C.
"""
function watch(x; transport::Bool=true, scout::Bool=false, interval::Real=2.0,
               commands::Bool=true, clear::Bool=false)
    owns = !(x isa ReachIndex)
    idx = x isa ReachIndex ? x : open_index(x)
    stop = Ref(false)
    dirty = Threads.Atomic{Bool}(true)

    function draw()
        clear && print("\e[2J\e[H")
        println("─"^64)
        println("ROSReach @ ", Dates.format(Dates.now(), "HH:MM:SS"), "    ",
                commands ? "commands: reach <t> | reach! <t> | q" : "Ctrl-C to quit")
        println("─"^64)
        graph_report(stdout, idx, x; transport=transport)
        if scout
            println("\nReachable (scouting, in theory):")
            try
                ps = scout_peers(; timeout_ms=800)
                isempty(ps) ? println("  (none — off-segment, gossip-only, or multicast disabled)") :
                    foreach(p -> println("  ", p), ps)
            catch err
                println("  (scout failed: ", sprint(showerror, err), ")")
            end
        end
        flush(stdout)
    end

    # Mark dirty on change — cheap and safe on the consumer task.
    on_change(idx) do _
        Threads.atomic_xchg!(dirty, true)
    end

    refresher = Threads.@spawn begin
        last = 0.0
        while !stop[]
            sleep(0.25)
            if !stop[] && (Threads.atomic_xchg!(dirty, false) || time() - last >= interval)
                try; draw(); catch err; @debug "watch draw failed" exception=err; end
                last = time()
            end
        end
    end

    try
        if commands
            while !stop[]
                line = try
                    readline(stdin)
                catch err
                    err isa InterruptException ? rethrow() : break
                end
                if isempty(line)
                    eof(stdin) && break          # piped/closed stdin → exit cleanly
                    Threads.atomic_xchg!(dirty, true)
                    continue
                end
                cmd = strip(line)
                if cmd in ("q", "quit", "exit")
                    break
                elseif startswith(cmd, "reach! ")
                    _show_reach(diagnose(idx, x, strip(cmd[length("reach! ")+1:end]); active=true))
                elseif startswith(cmd, "reach ")
                    _show_reach(diagnose(idx, x, strip(cmd[length("reach ")+1:end]); active=false))
                else
                    println("commands: reach <target> | reach! <target> | q")
                end
            end
        else
            while !stop[]
                sleep(interval)
            end
        end
    catch err
        err isa InterruptException || rethrow()
    finally
        stop[] = true
        owns && close(idx)
    end
    nothing
end

function _show_reach(r::ReachReport)
    println()
    show(stdout, MIME"text/plain"(), r)
    println()
    flush(stdout)
end
