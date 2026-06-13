# Payoff checks for the warm-up / module-end-bake work (PLAN-WARMUP-REGISTRATION S1a/S3/S6):
# prove the machinery actually puts the handler path in the method cache, not just that it runs.
#   1. S1a — the function-barrier'd component callback dispatches the reaction STATICALLY
#      (`code_typed`, pure-unit, with an Any-capture negative control proving the detector bites).
#   2. S3  — a `ServiceClient` built under `:precompile,sync` has its `encode`/`call` path cached.
#   3. Startup — after a real component node bring-up, its reaction handler `(M, msgtype)` is
#      compiled (the warm reaches the user handler).
# The deterministic BAKE payoff — handlers compiled at LOAD of a precompiled package, race-free —
# is asserted in `component_precompile.jl`; this file covers the live runtime tiers.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ServiceClient, ActionClient, ActionServer, call, mixin_spec, _sub_cb
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_wcctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# File-local helper names (`_wc_*`): test files `include` into a shared `Main`, so a generic name
# would clobber another file's (e.g. `component_parametric_di.jl` also defines `_ci`/`_ssa_any`).

# Is `f(types)` already inferred/compiled in the method cache, WITHOUT forcing it? A precompiled
# or executed MethodInstance carries a `CodeInstance` in `.cache`; an untouched one does not.
function _wc_cached(@nospecialize(f), @nospecialize(types))
    sig = Base.signature_type(f, types)
    for m in methods(f, types), mi in Base.specializations(m)
        mi isa Core.MethodInstance || continue
        mi.specTypes <: sig && return isdefined(mi, :cache)
    end
    return false
end

# A value statement with an `Any` ssatype ⇒ uninferred/dynamic (terminators carry a placeholder Any).
_wc_ssa_any(ci) = any(i -> ci.ssavaluetypes[i] === Any &&
                           !(ci.code[i] isa Union{Core.ReturnNode, Core.GotoNode, Core.GotoIfNot}),
                      eachindex(ci.code))
_wc_ci(f, ts) = first(only(Base.code_typed(f, ts)))

# BYO service + action types, the same fixtures services.jl/actions.jl use (`from=` resolves
# relative to this file's dir).
module _WarmSrv
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/srv/DoThing"
end
const DoThing_Request = _WarmSrv.robot_msgs.srv.DoThing_Request

module _WarmAct
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/action/Process"
end

# Distinct mixins per testset so one testset's inference can't pre-satisfy another's cache check.
module _WarmTypes
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    @mixin struct Sink; got::Int = 0; end           # S1a subject
    @hears function absorb(m::Sink, msg::_T); m.got += 1; nothing; end
    @mixin struct Spin; got::Int = 0; end           # startup subject (untouched until run)
    @hears function gulp(m::Spin, msg::_T); m.got += 1; nothing; end
end
using ._WarmTypes: Sink, Spin

@testset "warm/bake actually compiles the path" begin

    @testset "S1a — component callback dispatches the reaction statically" begin
        m = Sink()
        p = only(port for port in mixin_spec(Sink).ports if port.kind === :subscription)
        T = _WarmTypes._T
        # production callback via the `where {F}` barrier → reaction lifted to a concrete type
        # parameter, captured concretely → the `reaction(m, msg)` call is statically dispatched
        @test !_wc_ssa_any(_wc_ci(_sub_cb(p.reaction, m), (T,)))
        # faithful pre-barrier control: read the reaction straight off `PortSpec.reaction::Any`
        # (what the barrier replaces) → the call is dynamic. Proves the detector actually bites.
        prebarrier = let pp = p, mm = m; msg -> pp.reaction(mm, msg); end
        @test _wc_ssa_any(_wc_ci(prebarrier, (T,)))
    end

    @testset "S3 — client warm anchors encode/call (Zenoh session)" begin
        _wcctx() do ctx
            node = Node(ctx, "warm_client")
            client = ServiceClient(node, "/warm_svc", DoThing_Request;
                                   warmup = :precompile, warmup_sync = true)
            @test _wc_cached(ROSNode.encode, (DoThing_Request,))      # _warm_client: precompile(encode,(Req,))
            @test _wc_cached(call, (typeof(client), DoThing_Request)) # _warm_client: precompile(call,(client,Req))
            close(client)
        end
    end

    @testset "startup — component reaction path compiled after bring-up (Zenoh session)" begin
        _wcctx() do ctx
            n = run(Spin; ctx = ctx, name = "warmspin", block = false)
            m = only(values(n.members))
            M = typeof(m)
            p = only(p for p in mixin_spec(M).ports if p.kind === :subscription)
            # The whole decode→dispatch chain is anchored, not just the handler: the consumer
            # invokes `_dispatch_decoded(e, h, T, cb, Owned())` where `cb` is the materialised
            # `_sub_cb` closure. Keyed on that unique closure type, so it can't be pre-baked.
            # The default warm is async on another thread, so poll for both in one condition —
            # the two anchors land at different instants within `_anchor_reactions!`.
            H = only(Base.return_types(_sub_cb, (typeof(p.reaction), M)))
            @test timedwait(5.0) do
                _wc_cached(p.reaction, (M, p.msgtype)) &&
                _wc_cached(ROSNode._dispatch_decoded,
                           (ROSNode.Entity, ROSNode.SampleHolder, Type{p.msgtype}, H, ROSNode.Owned))
            end === :ok
        end
    end

    @testset "action client/server warm anchor the wrapper codecs (Zenoh session)" begin
        A   = _WarmAct.robot_msgs.action.Process
        wrap(s) = ROSNode._action_wrapper(A, s)
        _wcctx() do ctx
            # #7 — client warm anchors the request wrappers it encodes
            client = ActionClient(Node(ctx, "warm_aclient"), "/warm_act", A;
                                  warmup = :precompile, warmup_sync = true)
            @test _wc_cached(ROSNode.encode, (wrap("_SendGoal_Request"),))
            @test _wc_cached(ROSNode.encode, (wrap("_GetResult_Request"),))

            # #6 — server warm anchors the wrappers its three services + feedback move (the gap
            # that read green behind a swallowing catch): responses encoded, requests decoded.
            srv = ActionServer(Node(ctx, "warm_aserver"), "/warm_act_s", A;
                               warmup = :precompile, warmup_sync = true) do g; nothing; end
            @test _wc_cached(ROSNode.encode,       (wrap("_SendGoal_Response"),))
            @test _wc_cached(ROSNode.encode,       (wrap("_GetResult_Response"),))
            @test _wc_cached(ROSNode.encode,       (wrap("_FeedbackMessage"),))
            @test _wc_cached(ROSNode.decode_owned, (Memory{UInt8}, Type{wrap("_SendGoal_Request")}))
            close(srv); close(client)
        end
    end
end
