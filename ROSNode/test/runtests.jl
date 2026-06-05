using ROSNode
using Test
using Zenohd_jll

# Live tests (those that open real Zenoh sessions) run against a PRIVATE zenoh
# router on a random per-run port — the Zenoh.jl pattern. Sessions connect ONLY to
# this endpoint (multicast off), so the live tests are reliable and can't collide
# with a stray/shared router or get starved by multicast-discovery contention.
# Router logs go to a FILE, never the test stdout: a hung test leaving zenohd
# holding stdout would block any output capture indefinitely.
const ROS_TEST_PORT = rand(20000:39999)
const ROS_TEST_EP   = "tcp/localhost:$ROS_TEST_PORT"
const _ROUTER_LOG   = joinpath(tempdir(), "zenohd-rosnode-$ROS_TEST_PORT.log")
@info "ROSNode test router" ROS_TEST_EP log=_ROUTER_LOG
const _ROUTER = run(pipeline(`$(Zenohd_jll.zenohd()) -l $ROS_TEST_EP`,
                             stdout = _ROUTER_LOG, stderr = _ROUTER_LOG), wait = false)
sleep(0.6)   # let the router bind before any session connects

# Tear the router down without leaving a `<defunct>` zenohd behind. Two things bite:
# (1) libzenohc IGNORES SIGTERM, so a plain `kill` (SIGTERM) doesn't stop it — it
#     survives the test process and gets orphaned; we must SIGKILL.
# (2) a killed child must be `wait`-ed to reap it, else it lingers as a zombie until
#     the parent exits.
# So: SIGKILL then reap. Registered as an `atexit` hook too, so *every* exit path
# (error before the `finally`, a `Pkg.test` that bails, an `exit()`) still reaps it —
# `atexit` runs on normal exit and on SIGTERM-driven shutdown (not on SIGKILL, which
# nothing can intercept; an outer `timeout -k` force-kill then orphans it to init,
# which reaps it anyway). Idempotent.
function _kill_router!()
    try
        process_running(_ROUTER) || return nothing
        kill(_ROUTER, Base.SIGKILL)
        wait(_ROUTER)              # reap so no <defunct> zenohd is left
    catch
    end
    return nothing
end
atexit(_kill_router!)

# Run a test file with a flushed, timestamped progress banner on both sides, so a
# hung run shows exactly which section is live (the live tests can wedge on a
# blocking native call). `@info` is forwarded by `Pkg.test`; we `flush` so the line
# escapes any block-buffering before the suite blocks — without it a wedged testset
# leaves an empty log and no clue where it stuck.
const _T0 = time()
function _section(name::AbstractString, path::AbstractString)
    @info "▶ $name  (+$(round(time() - _T0; digits=1))s)"
    flush(stdout); flush(stderr)
    include(path)
    @info "✓ $name  (+$(round(time() - _T0; digits=1))s)"
    flush(stdout); flush(stderr)
end

try
    @testset "ROSNode" begin
        # Pure-logic unit tests (no Zenoh session) — the reliable core.
        _section("time", "time.jl")                       # §7  RTime/Duration arithmetic + cross-clock guard
        _section("settlement", "settlement.jl")           # §8/§9 write-once cell + fail-safe settlement
        _section("qos", "qos.jl")                         # §12 QoS RxO compatibility
        _section("name_resolution", "name_resolution.jl") # §5  name resolution (absolute/relative/private/remap)
        _section("parameters", "parameters.jl")           # §10 parameter transactions + dynamic dict
        _section("lifecycle", "lifecycle.jl")             # §14.2 lifecycle state machine + gating predicate
        _section("roszenoh_roundtrip", "roszenoh_roundtrip.jl") # keyexpr/attachment round-trips through ROSZenoh
        _section("dynamic_types", "dynamic_types.jl")     # §11/D5 dynamic type support (conversions, cache, macros)
        _section("resolution_d10b", "resolution_d10b.jl") # D10B per-module resolution tables + cross-materialize
        # Live D5 round-trips against the private router (§11/§13/D5 S2/S3/S5/S7).
        _section("dynamic_live", "dynamic_live.jl")
        # §4 concurrency: persistent worker-pool consumer (Parallel view/owned + Inf).
        _section("parallel_dispatch", "parallel_dispatch.jl")
        # Live D4: transient_local latched delivery + durability-aware lifecycle re-latch.
        _section("transient_local", "transient_local.jl")
        # §8 services + §9 actions — live request/reply + full goal lifecycle.
        _section("services", "services.jl")
        _section("actions", "actions.jl")
        # D7: unified logging — pure level/format/maxlog + live /rosout dispatcher wrap.
        _section("rosout", "rosout.jl")
        # D8: precompilation / warm-up — detector/@effectful/policy/_default_msg + live
        # :precompile delivery-transparency and :execute effect-suppression / null-route.
        _section("warmup", "warmup.jl")
        # D9: dynamic-discovery × precompilation — per-node interaction manifest
        # (persistence/union/gating) + live record-on-dispatch and Tier-1 startup replay.
        _section("dynamic_warm", "dynamic_warm.jl")
        # §14.1 Context drain/shutdown root — hook ordering, bounded slow hook,
        # idempotent request_shutdown, reverse undeclare, wait/spin unblock.
        _section("context_shutdown", "context_shutdown.jl")
    end
finally
    _kill_router!()
end
