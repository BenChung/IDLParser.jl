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

try
    @testset "ROSNode" begin
        # Pure-logic unit tests (no Zenoh session) — the reliable core.
        include("time.jl")                # §7  RTime/Duration arithmetic + cross-clock guard
        include("settlement.jl")          # §8/§9 write-once cell + fail-safe settlement
        include("qos.jl")                 # §12 QoS RxO compatibility
        include("name_resolution.jl")     # §5  name resolution (absolute/relative/private/remap)
        include("parameters.jl")          # §10 parameter transactions + dynamic dict
        include("lifecycle.jl")           # §14.2 lifecycle state machine + gating predicate
        include("roszenoh_roundtrip.jl")  # keyexpr/attachment round-trips through ROSZenoh
        include("dynamic_types.jl")       # §11/D5 dynamic type support (conversions, cache, macros)
        # Live D5 round-trips against the private router (§11/§13/D5 S2/S3/S5/S7).
        include("dynamic_live.jl")
    end
finally
    try; kill(_ROUTER); catch; end
end
