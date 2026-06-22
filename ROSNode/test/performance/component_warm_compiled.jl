# Payoff checks for the warm-up / precompile-anchor work (PLAN-NODE-PLAN.md precompile_schema):
# prove the machinery actually puts the handler path in the method cache, not just that it runs.
#   1. S1a — the function-barrier'd component callback dispatches the reaction STATICALLY
#      (`code_typed`, pure-unit, with an Any-capture negative control proving the detector bites).
#   2. S3  — a `ServiceClient` built under `:precompile,sync` has its `encode`/`call` path cached.
#   3. Startup — after a real functor node bring-up, the `precompile_schema` reaction anchor
#      compiles the user handler `(CN, Mc, msgtype)` (the warm reaches the user handler).
# The deterministic BAKE payoff — handlers compiled at LOAD of a precompiled package, race-free —
# is asserted in `component_precompile.jl`; this file covers the live runtime tiers.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ServiceClient, ActionClient, ActionServer, call,
               member_schema, node, hears, _sub_cb, _base, _cb_type, _anchor_functor_sub!
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

# Distinct functor members per testset so one testset's inference can't pre-satisfy another's cache
# check. Node-first handlers; `member_schema(M) = component(M, hears(:p, T, h))` carries the sub.
module _WarmTypes
    using ROSNode
    using ROSNode: component, hears
    import ROSNode: member_schema          # EXTENDED below (must import, not `using`, to add methods)
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    mutable struct Sink{Name} <: ROSNode.Component{Name}; got::Int; end    # S1a subject
    Sink{Name}() where {Name} = Sink{Name}(0)
    absorb(node, m::Sink, msg::_T) = (m.got += 1; nothing)
    member_schema(::Type{Sink}) = component(Sink, hears(:in, _T, absorb))

    mutable struct Spin{Name} <: ROSNode.Component{Name}; got::Int; end    # startup subject
    Spin{Name}() where {Name} = Spin{Name}(0)
    gulp(node, m::Spin, msg::_T) = (m.got += 1; nothing)
    member_schema(::Type{Spin}) = component(Spin, hears(:in, _T, gulp))
end
using ._WarmTypes: Sink, Spin

@testset "warm/bake actually compiles the path" begin

    @testset "S1a — component callback dispatches the reaction statically" begin
        m = Sink{:s}()
        d = only(p for p in member_schema(Sink).ports if p isa ROSNode.Sub)
        T = _WarmTypes._T
        # production callback via the `where {F}` barrier → the handler (carried CONCRETELY on the
        # `Sub{Name,T,F}` descriptor as `.handler`) is lifted to a concrete type parameter and
        # captured concretely → the `reaction(node, m, msg)` call is statically dispatched
        # (node-first; `nothing` stands in for the node — the handler ignores it).
        @test !_wc_ssa_any(_wc_ci(_sub_cb(d.handler, nothing, m), (T,)))
        # faithful pre-barrier control: erase the handler's type through an `Any` slot (what
        # `_sub_cb`'s `where {F}` barrier recovers — the old `PortSpec.reaction::Any` field) → the
        # call is dynamic. Proves the detector actually bites.
        anyh = Any[d.handler]
        prebarrier = let h = anyh, mm = m; msg -> h[1](nothing, mm, msg); end
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

    @testset "startup — functor reaction path compiled by precompile_schema (Zenoh session)" begin
        _wcctx() do ctx
            schema = node("spin" => Spin)
            n = run(schema; ctx = ctx, name = "warmspin", block = false)
            m = only(values(n.members))
            # bring-up completes without error; `member_schema` keys on the base (the member type is
            # now `Spin{:spin}`, an instantiation — `_base` recovers the wrapper).
            @test member_schema(_base(typeof(m))) !== nothing

            # The functor anchor IS node-first (`_anchor_functor_sub!`, functor.jl), so the deferred
            # mixin cache-survival assert is now LIVE: derive the concrete (CN, Mc, T) + the `_sub_cb`
            # closure `H`, run the per-(T,H) anchor, and confirm the user handler + the decode→dispatch
            # leaves survive in the method cache.
            CN = typeof(n); Mc = typeof(m)
            d  = only(p for p in member_schema(_base(Mc)).ports if p isa ROSNode.Sub)
            T  = typeof(d).parameters[2]      # Sub{Name, T, F} → the message type
            reaction = d.handler
            _anchor_functor_sub!(CN, Mc, T, reaction)
            @test _wc_cached(reaction, (CN, Mc, T))                   # the user handler, node-first
            H = _cb_type(_sub_cb, reaction, CN, Mc)
            @test H !== nothing                                       # the materialised callback is one concrete type
            @test _wc_cached(ROSNode.decode_owned, (Memory{UInt8}, Type{T}))
            @test _wc_cached(ROSNode._dispatch_decoded, (ROSNode.Entity, ROSNode.SampleHolder, Type{T}, H, ROSNode.Owned))
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
