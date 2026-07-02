# Functor node API — integration test suite (start of migration; precompile + full @mixin-suite
# migration deferred). Run standalone:
#   julia --project=ROSNode ROSNode/test/model/component/functor.jl

using ROSNode
using Test
import ROSNode: member_schema, configure          # generics we EXTEND (must import to add methods)

const RNX = ROSNode
const _TimeT = ROSNode.Interfaces.builtin_interfaces.msg.Time
isconc(f, args) = (rts = Base.return_types(f, args); length(rts) == 1 && isconcretetype(only(rts)))

# ── fixtures ─────────────────────────────────────────────────────────────────────────────────────
@interface BatterySource  battery(_)::Float64

mutable struct DummyState{Name} <: Component{Name} end   # zero fields → implicit DummyState{Name}() ctor
member_schema(::Type{DummyState}) = component(DummyState)

mutable struct Sensor2{Name} <: Component{Name}; level::Float64; end
Sensor2{Name}() where {Name} = Sensor2{Name}(100.0)
battery(s::Sensor2) = s.level
member_schema(::Type{Sensor2}) = component(Sensor2; provides = (BatterySource,))

mutable struct Guard2{Name, B} <: Component{Name}; src::B; end
make_guard2(node, ::Val{Name}, src) where {Name} = Guard2{Name, typeof(src)}(src)
member_schema(::Type{Guard2}) = component(Guard2; requires = (BatterySource,), ctor = make_guard2)

mutable struct Rec2{Name, O} <: Component{Name}; odom::O; end
make_rec2(node, ::Val{Name}, s::Sensor2) where {Name} = Rec2{Name, typeof(s)}(s)
member_schema(::Type{Rec2}) = component(Rec2; requires = (Sensor2,), ctor = make_rec2)

# the same DI declared through the trait methods instead of the component(…) kwargs (the surface the
# `provides`/`requires`/`construct` docstrings demonstrate); `component(M)` with no kwargs reads them.
mutable struct TSensor{Name} <: Component{Name}; level::Float64; end
TSensor{Name}() where {Name} = TSensor{Name}(50.0)
battery(s::TSensor) = s.level
RNX.provides(::Type{TSensor}) = (BatterySource,)
member_schema(::Type{TSensor}) = component(TSensor)
mutable struct TGuard{Name, B} <: Component{Name}; src::B; end
RNX.requires(::Type{TGuard}) = (BatterySource,)
RNX.construct(::Type{TGuard}, node, ::Val{Name}, src) where {Name} = TGuard{Name, typeof(src)}(src)
member_schema(::Type{TGuard}) = component(TGuard)

@parameters struct ProbeParams
    rate::Int64  = 10 ∈ 1..50
    mode::Symbol = :auto ∈ (:auto, :manual)
end
mutable struct Probe{Name} <: Component{Name}; n::Int; end
Probe{Name}() where {Name} = Probe{Name}(0)
member_schema(::Type{Probe}) = component(Probe, ProbeParams, publishes(:out, _TimeT))

mutable struct Pinger{Name} <: Component{Name}; sent::Int; end
Pinger{Name}() where {Name} = Pinger{Name}(0)
ping_tick(node, m::Pinger) = (m.sent += 1; nothing)        # publish-in-timer under block=false is a runtime flake
member_schema(::Type{Pinger}) = component(Pinger, publishes(:ping, _TimeT), every(:tick, 50, ping_tick))

mutable struct Ponger{Name} <: Component{Name}; got::Int; end
Ponger{Name}() where {Name} = Ponger{Name}(0)
ping_hear(node, m::Ponger, msg) = (m.got += 1; nothing)
member_schema(::Type{Ponger}) = component(Ponger, hears(:ping, _TimeT, ping_hear))

# ── tests ─────────────────────────────────────────────────────────────────────────────────────────
@testset "functor: schema + 32-cutoff" begin
    @test publishes(:out, Int) isa RNX.Pub{:out, Int}
    @test isconcretetype(RNX.ports_nt_type(Tuple{RNX.Pub{:a, Int}, RNX.Sub{:b, Float64, typeof(identity)}}))
    # ports-per-member axis: concrete past 32
    @test isconcretetype(RNX.ports_nt_type(Tuple{ntuple(i -> RNX.Pub{Symbol("p", i), Int}, 40)...}))
    # members-per-node axis: node_ports_carrier + build_members concrete past 32
    ns40 = node([("m$i" => DummyState) for i in 1:40]...)
    @test isconc(RNX.node_ports_carrier, (typeof(ns40),))
    @test isconc(RNX.build_members, (typeof(ns40), Node))
end

@testset "functor: DI" begin
    ns = node("sensor" => Sensor2, "guard" => Guard2)
    @test ns.order == [:sensor, :guard]
    mbrs = RNX.build_members(ns, nothing)
    @test mbrs.guard isa Guard2{:guard, Sensor2{:sensor}}
    @test mbrs.guard.src === mbrs.sensor
    ns3 = node("sensor" => Sensor2, "guard" => Guard2, "rec" => Rec2)
    @test ns3.order[1] == :sensor
    @test RNX.build_members(ns3, nothing).rec isa Rec2{:rec, Sensor2{:sensor}}
    @test_throws ErrorException node("guard" => Guard2)                              # unsatisfied
    @test_throws ErrorException node("a" => Sensor2, "b" => Sensor2, "g" => Guard2)  # ambiguous
    # DI also resolves from the trait methods when the component(…) kwargs are omitted
    @test RNX.provides_of(typeof(member_schema(TSensor))) == (BatterySource,)
    @test RNX.requires_of(typeof(member_schema(TGuard)))  == (BatterySource,)
    tmb = RNX.build_members(node("s" => TSensor, "g" => TGuard), nothing)
    @test tmb.g isa TGuard{:g, TSensor{:s}} && tmb.g.src === tmb.s    # construct(::Type{TGuard}, …) injected the sibling
end

@testset "functor: params (§9 coercion)" begin
    @test fieldnames(ProbeParams) == (:rate, :mode)
    p = ProbeParams(; rate = 20, mode = "manual")
    @test p.mode === :manual && p.rate == 20      # String -> Symbol via the @parameters ctor
end

@testset "functor: live run + priming + duplicate members" begin
    RNX.set_intra_process!(true)
    cn = run(node("pinger" => Pinger, "ponger" => Ponger); name = "talk", block = false)
    try
        pub = entities(cn, cn.members.pinger).ping
        @test pub isa RNX.PublisherHandle{_TimeT}
        @test parameters(cn, cn.members.pinger) isa RNX.EmptyParams
        @test cn.members.pinger isa Pinger{:pinger}
        # a-priori priming put both member endpoints in the local graph
        g = RNX.local_graph(cn.node)
        @test count(e -> e.topic == "/ping", g) == 2                    # pub + sub
        # the materialised Tmr fires
        sleep(0.3); @test cn.members.pinger.sent > 0
        # functor Pub → functor Sub intra-process (manual publish from the main task)
        for _ in 1:5; publish(pub, ROSNode._default_msg(_TimeT)); end
        for _ in 1:100; cn.members.ponger.got >= 5 && break; sleep(0.01); end
        @test cn.members.ponger.got == 5                                # all 5 delivered (not just >0)
    finally
        close(cn)
    end
    @test !isopen(cn.node)

    # duplicate members of one mixin type stay distinct (Component{Name}); remap their shared output
    # so they don't clobber, and confirm entities resolve each member's own handle
    cn2 = run(node("front" => remap(Pinger, :ping => "front/ping"),
                   "rear"  => remap(Pinger, :ping => "rear/ping")); name = "dup", block = false)
    try
        @test cn2.members.front isa Pinger{:front}
        @test cn2.members.rear  isa Pinger{:rear}
        @test entities(cn2, cn2.members.front).ping !== entities(cn2, cn2.members.rear).ping
        @test entities(cn2, cn2.members.front).ping.entity.endpoint.topic == "/front/ping"
        @test entities(cn2, cn2.members.rear).ping.entity.endpoint.topic  == "/rear/ping"
    finally
        close(cn2)
    end
end

# ── review-driven coverage: the three blockers + DI-message/cycle/teardown ─────────────────────────
const GP_Req  = ROSNode.Interfaces.rcl_interfaces.srv.GetParameters_Request
const GP_Resp = ROSNode.Interfaces.rcl_interfaces.srv.GetParameters_Response
gp_handler(node, m, req) = ROSNode._default_msg(GP_Resp)
mutable struct SrvHost{Name} <: Component{Name} end
member_schema(::Type{SrvHost}) = component(SrvHost, serves(:gp, GP_Req, gp_handler))

mutable struct Refuser{Name} <: Component{Name} end
configure(node, m::Refuser) = ROSNode.failure          # cooperative abort token
member_schema(::Type{Refuser}) = component(Refuser, publishes(:p, _TimeT))

mutable struct Thrower{Name} <: Component{Name} end
configure(node, m::Thrower) = error("boom from configure")
member_schema(::Type{Thrower}) = component(Thrower, publishes(:p, _TimeT))

mutable struct BadReq{Name} <: Component{Name} end
badreq_ctor(node, ::Val{N}, dep) where {N} = BadReq{N}()               # valid DI ctor (so the no-ctor guard passes)…
member_schema(::Type{BadReq}) = component(BadReq; requires = (Int,), ctor = badreq_ctor)  # …but Int has no provider → fails at node()

@interface PoseSrc  poseof(_)

mutable struct CycA{Name, B} <: Component{Name}; b::B; end
mutable struct CycB{Name, A} <: Component{Name}; a::A; end
make_cyca(node, ::Val{N}, b) where {N} = CycA{N, typeof(b)}(b)
make_cycb(node, ::Val{N}, a) where {N} = CycB{N, typeof(a)}(a)
member_schema(::Type{CycA}) = component(CycA; requires = (CycB,), ctor = make_cyca)
member_schema(::Type{CycB}) = component(CycB; requires = (CycA,), ctor = make_cycb)

@testset "functor: review fixes" begin
    # blocker 1 — serves/Srv carrier provably equals the runtime ServiceHandle
    sdesc = serves(:gp, GP_Req, gp_handler)
    @test sdesc isa RNX.Srv{:gp, GP_Req}
    cn = run(node("h" => SrvHost); name = "srvhost", block = false)
    try
        h = entities(cn, cn.members.h).gp
        @test h isa RNX.ServiceHandle
        @test typeof(h) === RNX.handle_type(typeof(sdesc))      # declared cell type == built handle type
    finally
        close(cn)
    end

    # blocker 2 — a configure failure token aborts bring-up (node torn down, error raised)
    @test_throws ErrorException run(node("r" => Refuser); name = "ref", block = false)

    # blocker 3 — a Pair pin is rejected with an ErrorException at schema-build (not a raw TypeError at run)
    @test_throws ErrorException component(DummyState; requires = (PoseSrc => :x,))

    # minor — a non-component requires errors clearly at node()
    @test_throws ErrorException node("x" => BadReq)

    # dependency cycle detected at node()
    @test_throws ErrorException node("a" => CycA, "b" => CycB)

    # teardown: throwing configure on an owned ctx rethrows + cleans up; close is idempotent
    @test_throws ErrorException run(node("t" => Thrower); name = "thr", block = false)
    cn2 = run(node("p" => Pinger); name = "ci", block = false)
    close(cn2); close(cn2)
    @test !isopen(cn2.node)
end

@testset "functor: runtime warmup parity" begin
    # (a) unmanaged run(::NodeSchema): warmup/warmup_sync now set the node-core WarmupPolicy
    #     (previously dropped → always NoWarmup). warmup_sync=true warms inline so the fan-out
    #     is exercised synchronously during configure (no race on reading the result).
    s = node("h" => SrvHost, "pinger" => Pinger, "ponger" => Ponger; name = "warmnode", register = false)
    cn = run(s; warmup = :precompile, warmup_sync = true, block = false)
    try
        @test cn.node.warmup.mode isa RNX.Precompile
        @test cn.node.warmup.sync === true
        # the node still brought up: ports materialised, members warmed without throwing
        @test entities(cn, cn.members.h).gp isa RNX.ServiceHandle
        @test entities(cn, cn.members.pinger).ping isa RNX.PublisherHandle{_TimeT}
    finally
        close(cn)
    end
    @test !isopen(cn.node)

    # default stays :off (no behavior change when the kwarg is unset)
    cn_off = run(node("p" => Pinger; name = "warmoff", register = false); block = false)
    try
        @test cn_off.node.warmup.mode isa RNX.NoWarmup
    finally
        close(cn_off)
    end

    # (b) managed run(::NodeSchema): warmup reaches the inner LifecycleNode's Node
    cn_m = run(node("p" => Pinger; name = "warmmgd", register = false);
               managed = true, warmup = :execute, warmup_sync = true, block = false)
    try
        @test RNX.inner_node(cn_m).warmup.mode isa RNX.Execute
        @test RNX.inner_node(cn_m).warmup.sync === true
    finally
        close(cn_m)
    end

    # (c) run(::Type{M}) functor-promote branch threads warmup into run(::NodeSchema)
    #     (previously the branch silently dropped it before calling _run_functor_promote).
    cn_p = run(Pinger; name = "warmpromote", warmup = :precompile, warmup_sync = true, block = false)
    try
        @test cn_p.node.warmup.mode isa RNX.Precompile
        @test cn_p.node.warmup.sync === true
    finally
        close(cn_p)
    end
end

@testset "functor: remaps + clobber" begin
    # remap to explicit wires + a CROSS-MEMBER remap (a subscriber wired to another member's topic)
    cn = run(node("front" => remap(Pinger, :ping => "front/ping"),
                  "rear"  => remap(Pinger, :ping => "rear/ping"),
                  "log"   => remap(Ponger, :ping => (:front, :ping))); name = "rig", block = false)
    try
        @test entities(cn, cn.members.front).ping.entity.endpoint.topic == "/front/ping"
        @test entities(cn, cn.members.rear).ping.entity.endpoint.topic  == "/rear/ping"
        @test entities(cn, cn.members.log).ping.entity.endpoint.topic   == "/front/ping"   # cross-member ref resolved
    finally
        close(cn)
    end
    # two unremapped same-topic publishers = unintended clobber → error at run()
    @test_throws ErrorException run(node("a" => Pinger, "b" => Pinger); name = "clob", block = false)
    # a remapped publisher onto a name must not mask a LATER un-remapped clobbering pair on that same name
    # (the remapped occupant is excused, but a non-remapped collider takes over the slot)
    @test_throws ErrorException run(node("a" => remap(Pinger, :ping => "ping"),
                                         "b" => Pinger, "c" => Pinger); name = "mask", block = false)
    # remapping a port the member does not declare → error at node()
    @test_throws ErrorException node("x" => remap(Pinger, :nope => "w"))
end

@testset "functor: managed lifecycle" begin
    RNX.set_intra_process!(true)
    cn = run(node("pinger" => Pinger, "ponger" => Ponger); name = "mgd", managed = true, block = false)
    ln = cn.lifecycle
    try
        @test ln !== nothing
        @test RNX.state(ln) === Unconfigured()                  # managed starts Unconfigured (autostart=!managed)
        @test getfield(cn.ports, :pinger).v === nothing         # ports not materialised before configure
        @test RNX.configure!(ln) === :success
        @test RNX.state(ln) === Inactive()
        @test getfield(cn.ports, :pinger).v !== nothing         # materialised AT the configure transition
        @test entities(cn, cn.members.pinger).ping isa RNX.PublisherHandle
        @test RNX.activate!(ln) === :success
        @test RNX.state(ln) === Active()
        pub = entities(cn, cn.members.pinger).ping              # Active → the dispatch gate is open
        for _ in 1:3; publish(pub, ROSNode._default_msg(_TimeT)); end
        for _ in 1:100; cn.members.ponger.got >= 3 && break; sleep(0.01); end
        @test cn.members.ponger.got == 3
        @test RNX.deactivate!(ln) === :success
        @test RNX.state(ln) === Inactive()
        # gate CLOSED: publishing while Inactive must be dropped (got must not advance)
        before = cn.members.ponger.got
        for _ in 1:5; publish(pub, ROSNode._default_msg(_TimeT)); end
        sleep(0.2); @test cn.members.ponger.got == before
        @test entities(cn, cn.members.pinger).ping isa RNX.PublisherHandle   # ports stay materialised
    finally
        close(cn)
    end
    @test !isopen(cn.node)
end

# ── action server (pre-authored @ros_action type + node-first exec via runs) ───────────────────────
module FAct
    using ROSNode
    @ros_package "functor_test"
    @ros_action function Counter(target::Int32,
            fb::FeedbackSink{@NamedTuple{current::Int32}})::@NamedTuple{total::Int32}
        (total = target,)        # body unused by the functor — a node-first exec is passed via runs()
    end
end
const Counter_Result   = FAct.functor_test.action.Counter_Result
const Counter_Feedback = FAct.functor_test.action.Counter_Feedback

mutable struct CountHost{Name} <: Component{Name} end
function count_exec(node, m::CountHost, goal)
    for i in 1:goal.request.target
        RNX.feedback!(goal, Counter_Feedback(; current = Int32(i)))
    end
    RNX.succeed(goal, Counter_Result(; total = goal.request.target))
end
member_schema(::Type{CountHost}) = component(CountHost, runs(:count, FAct.Counter, count_exec))

@testset "functor: action server" begin
    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("svc" => CountHost); ctx = ctx, name = "acthost", block = false)
    cli = RNX.ActionClient(Node(ctx, "cli"), "/count", FAct.Counter)
    try
        # the Act port primed + materialised its 5 wire endpoints (send_goal/cancel/get_result + feedback + status)
        g = RNX.local_graph(cn.node)
        @test count(e -> startswith(e.topic, "/count"), g) == 5
        @test entities(cn, cn.members.svc).count isa RNX.ActionServer
        # full goal round-trip: the node-first exec runs, feedback streams, result settles
        @test RNX.wait_for_action_server(cli; timeout = 5)
        gh = RNX.send(cli; target = Int32(3))
        @test fetch(gh).total == 3
    finally
        close(cli); close(cn); close(ctx)
    end
end

# ── authored service through functor serves (world-age regression: response_type must resolve in
#    serves() in the NORMAL world, not in the @generated carrier's generator world — an AUTHORED
#    service's _Response binding postdates ROSNode's world. The prior single-marker Srv threw "is the
#    service fully generated?"; only an IMPORTED request type (the SrvHost blocker above) masked it.)
module FSrv
    using ROSNode
    @ros_package "functor_srv_test"
    @ros_service function Adder(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b,)        # body unused by the functor; serves() passes the node-first handler
    end
end
const Adder_Req  = FSrv.functor_srv_test.srv.Adder_Request
const Adder_Resp = FSrv.functor_srv_test.srv.Adder_Response
mutable struct SumHost{Name} <: Component{Name} end
add_handler(node, m::SumHost, req) = Adder_Resp(; sum = req.a + req.b)
member_schema(::Type{SumHost}) = component(SumHost, serves(:add, Adder_Req, add_handler; on = "~/add"))

@testset "functor: authored service (world-age)" begin
    # carrier-type derivation runs handle_type(Srv) over an AUTHORED type — threw pre-fix
    @test RNX.handle_type(typeof(serves(:add, Adder_Req, add_handler))) === RNX.ServiceHandle{Adder_Req, Adder_Resp}
    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("h" => SumHost); ctx = ctx, name = "adder", block = false)
    cli = ServiceClient(Node(ctx, "cli"), "/adder/add", Adder_Req)
    try
        @test entities(cn, cn.members.h).add isa RNX.ServiceHandle{Adder_Req, Adder_Resp}
        @test wait_for_service(cli; timeout = 5)
        @test call(cli, Adder_Req(a = Int64(2), b = Int64(40))).sum == 42
    finally
        close(cli); close(cn); close(ctx)
    end
end

# ── @service / @action inline authoring (level-3 sugar: type + impl + descriptor in one place) ─────
module FInline
    using ROSNode
    import ROSNode: member_schema, configure
    @ros_package "functor_inline_test"

    mutable struct Calc{Name} <: Component{Name}; bias::Int64; end
    Calc{Name}() where {Name} = Calc{Name}(0)
    configure(node, m::Calc) = (m.bias = 100; nothing)        # member state set in a lifecycle hook
    @service "~/add" function add(node, m::Calc, a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b + m.bias,)                               # reads member state; NamedTuple auto-wrapped
    end
    member_schema(::Type{Calc}) = component(Calc, add)

    mutable struct Counter{Name} <: Component{Name} end
    @action "~/countup" function countup(node, m::Counter, target::Int32,
                          fb::FeedbackSink{@NamedTuple{current::Int32}})::@NamedTuple{total::Int32}
        for i in 1:target; fb((current = Int32(i),)); end     # feedback streams
        (total = target,)                                     # normal return → SUCCEEDED with Result
    end
    member_schema(::Type{Counter}) = component(Counter, countup)
end

@testset "functor: @service inline authoring" begin
    AddReq  = FInline.functor_inline_test.srv.add_Request
    AddResp = FInline.functor_inline_test.srv.add_Response
    # `add` stays a first-class CALLABLE method (not a descriptor) …
    @test FInline.add isa Function
    @test FInline.add(nothing, FInline.Calc{:c}(5), Int64(1), Int64(2)) == (sum = 8,)   # 1+2+bias(5)
    # … carrying the @ros_service marker methods + the component_service meta-method (the trait)
    @test RNX.request_type(typeof(FInline.add))  === AddReq
    @test RNX.response_type(typeof(FInline.add)) === AddResp
    @test RNX.component_service(typeof(FInline.add)) isa RNX.ComponentService
    # the descriptor is DERIVED on use; default name+wire from the trait
    @test serves(FInline.add) isa RNX.Srv{:add, AddReq, AddResp}
    # reuse the SAME handler under another name (level-2): wraps the same method
    rb = serves(:total, FInline.add; on = "~/total")
    @test rb isa RNX.Srv{:total, AddReq, AddResp}
    @test rb.handler.f === FInline.add

    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("c" => FInline.Calc); ctx = ctx, name = "calc", block = false)   # component(Calc, add) auto-converts
    cli = ServiceClient(Node(ctx, "cli"), "/calc/add", AddReq)
    try
        @test entities(cn, cn.members.c).add isa RNX.ServiceHandle{AddReq, AddResp}
        @test wait_for_service(cli; timeout = 5)
        @test call(cli, AddReq(a = Int64(2), b = Int64(3))).sum == 105   # 2 + 3 + bias(100, set in configure)
    finally
        close(cli); close(cn); close(ctx)
    end
end

@testset "functor: @action inline authoring" begin
    # `countup` stays the named marker (ActionTypeSupport on it) + the component_action meta-method
    @test FInline.countup isa Function
    @test RNX.component_action(typeof(FInline.countup)) isa RNX.ComponentAction
    @test runs(FInline.countup) isa RNX.Act{:countup}
    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("k" => FInline.Counter); ctx = ctx, name = "ctr", block = false)
    cli = RNX.ActionClient(Node(ctx, "cli2"), "/ctr/countup", FInline.countup)   # client uses the named marker
    try
        g = RNX.local_graph(cn.node)
        @test count(e -> startswith(e.topic, "/ctr/countup"), g) == 5     # 5 action endpoints primed
        @test entities(cn, cn.members.k).countup isa RNX.ActionServer
        @test RNX.wait_for_action_server(cli; timeout = 5)
        gh = RNX.send(cli; target = Int32(4))
        @test fetch(gh).total == 4                                        # auto-settle SUCCEEDED with Result
    finally
        close(cli); close(cn); close(ctx)
    end
end

mutable struct UseClient{Name} <: Component{Name} end
member_schema(::Type{UseClient}) = component(UseClient,
    uses(:adder,   FInline.add),        # service client via a Function (@service) marker
    uses(:counter, FInline.countup))    # action client via a Function (@action) marker

@testset "functor: uses() client handle is concrete" begin
    AddReq  = FInline.functor_inline_test.srv.add_Request
    AddResp = FInline.functor_inline_test.srv.add_Response
    sup     = RNX.ActionTypeSupport(typeof(FInline.countup))
    ActT    = RNX.ActionClient{typeof(FInline.countup), RNX.goal_type(sup), RNX.result_type(sup), RNX.feedback_type(sup)}
    SrvT    = RNX.ServiceClient{AddReq, AddResp}

    # the handle type baked at uses() == what ServiceClient/ActionClient RETURN
    @test RNX.handle_type(typeof(uses(:adder,   FInline.add)))     === SrvT
    @test RNX.handle_type(typeof(uses(:counter, FInline.countup))) === ActT

    # the per-member ports NamedTuple is concrete; client fields are the concrete client types
    P = RNX.ports_nt_type(Tuple{typeof(uses(:adder, FInline.add)),
                                typeof(uses(:counter, FInline.countup))})
    @test isconcretetype(P)
    @test fieldtype(P, :adder)   === SrvT
    @test fieldtype(P, :counter) === ActT

    # live: entities(node, m) is type-stable, the cells hold the concrete clients, and the
    # client-field access infers to ONE concrete type (call/send dispatch statically — the fixed bug)
    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("u" => UseClient); ctx = ctx, name = "caller", block = false)
    try
        m = cn.members.u
        @test isconc(entities, (typeof(cn), typeof(m)))                       # whole ports NT is one concrete type
        ents = entities(cn, m)
        @test ents.adder   isa SrvT
        @test ents.counter isa ActT
        @test Base.return_types((c, mm) -> entities(c, mm).adder, (typeof(cn), typeof(m))) == [SrvT]
    finally
        close(cn); close(ctx)
    end
end

# ── fieldless (trigger) @service / goal-less @action: the splat adapters must surface ZERO request/goal
#    fields (a synthetic empty struct is wire-only) and build an empty response/result via `Rs()`/`R()`.
module FEmpty
    using ROSNode
    import ROSNode: member_schema
    @ros_package "functor_empty_test"
    mutable struct Trig{Name} <: Component{Name}; hits::Int64; end
    Trig{Name}() where {Name} = Trig{Name}(0)
    @service "~/trig" function trig(node, m::Trig)::@NamedTuple{ok::Bool}      # empty request
        m.hits += 1; (ok = true,)
    end
    @service "~/sink" function sink(node, m::Trig, v::Int64)::@NamedTuple{}    # empty response
        m.hits = v; (;)
    end
    mutable struct Go{Name} <: Component{Name} end
    @action "~/go" function go(node, m::Go, fb::FeedbackSink{@NamedTuple{step::Int32}})::@NamedTuple{done::Bool}
        fb((step = Int32(1),)); (done = true,)               # goal-less action
    end
    member_schema(::Type{Trig}) = component(Trig, trig, sink)
    member_schema(::Type{Go})   = component(Go, go)
end

@testset "functor: fieldless @service / goal-less @action" begin
    TrigReq = FEmpty.functor_empty_test.srv.trig_Request
    SinkReq = FEmpty.functor_empty_test.srv.sink_Request
    ctx = RNX.Context(; localhost_only = true)
    cn  = run(node("t" => FEmpty.Trig, "g" => FEmpty.Go); ctx = ctx, name = "fe", block = false)
    tcli = ServiceClient(Node(ctx, "fec1"), "/fe/trig", TrigReq)
    scli = ServiceClient(Node(ctx, "fec2"), "/fe/sink", SinkReq)
    acli = RNX.ActionClient(Node(ctx, "fec3"), "/fe/go", FEmpty.go)
    try
        @test wait_for_service(tcli; timeout = 5)
        @test call(tcli, TrigReq()).ok === true              # empty request splats nothing
        @test wait_for_service(scli; timeout = 5)
        @test call(scli, SinkReq(v = Int64(9))) isa FEmpty.functor_empty_test.srv.sink_Response  # empty response → Rs()
        @test cn.members.t.hits == 9
        @test RNX.wait_for_action_server(acli; timeout = 5)
        @test fetch(RNX.send(acli)).done === true            # goal-less action → empty Goal, Result built
    finally
        close(tcli); close(scli); close(acli); close(cn); close(ctx)
    end
end

# ── managed failure token (the managed path translates failure to a :failure transition, NOT a throw)
mutable struct ActRefuser{Name} <: Component{Name} end
ROSNode.activate(node, m::ActRefuser) = ROSNode.failure          # cooperative abort at activate
member_schema(::Type{ActRefuser}) = component(ActRefuser, publishes(:p, _TimeT))

@testset "functor: managed failure token" begin
    # configure failure → :failure, stays Unconfigured, the materialised cell is rolled back
    cn = run(node("r" => Refuser); name = "mgdfail", managed = true, block = false)
    try
        @test RNX.configure!(cn.lifecycle) === :failure
        @test RNX.state(cn.lifecycle) === Unconfigured()
        @test getfield(cn.ports, :r).v === nothing
    finally
        close(cn)
    end
    # activate failure → :failure, stays Inactive (configure succeeded first)
    cn2 = run(node("a" => ActRefuser); name = "mgdfail2", managed = true, block = false)
    try
        @test RNX.configure!(cn2.lifecycle) === :success
        @test RNX.activate!(cn2.lifecycle) === :failure
        @test RNX.state(cn2.lifecycle) === Inactive()
    finally
        close(cn2)
    end
end

# ── deployment / composition: by-name registration + container load_node/add!/unload ──────────────
@testset "functor: registration" begin
    # registers by default when a kind name is given (the rclcpp_components_register_nodes analog)
    s = node("p" => Probe; name = "FunctorRegProbe")
    @test RNX.node_kind("FunctorRegProbe") === s
    @test "FunctorRegProbe" in RNX.node_kinds()
    @test RNX._kind_default_name(s) == "FunctorRegProbe"
    # resolve tolerates a namespaced/`::`-qualified spelling by its last segment (ros2 plugin names)
    @test RNX.resolve_node_kind("my_pkg", "my_pkg::FunctorRegProbe") === s
    # opt-out: build + name the schema, but do not register it
    s2 = node("p" => Probe; name = "FunctorRegNoReg", register = false)
    @test RNX.node_kind("FunctorRegNoReg") === nothing
    @test s2.name === :FunctorRegNoReg            # name still carried (default instance name), just unregistered
    # anonymous schema: no name, not registered, errors when a name is demanded
    sa = node("p" => Probe)
    @test sa.name === nothing
    @test RNX.node_kind("Probe") === nothing
    @test_throws ArgumentError RNX._kind_default_name(sa)
    # register=true without a name is rejected up front
    @test_throws ArgumentError node("p" => Probe; register = true)

    # run() defaults its instance name from the schema's kind name; log_level is accepted + applied
    cn = run(s; log_level = RNX.Logging.Warn, block = false)
    try
        @test cn.node.fqn == "/FunctorRegProbe"   # name defaulted from node(…; name="FunctorRegProbe")
    finally
        close(cn)
    end
    # anonymous schema must be given an explicit name at run
    @test_throws ArgumentError run(sa; block = false)
end

@testset "functor: container load_node / add! / unload" begin
    schema = node("p" => Probe; name = "FunctorDeployProbe")
    c = container("fdeploy"; block = false, localhost_only = true) do _ end
    try
        # load_node BY NAME — the headline parity gap: resolve the registered schema, build + autostart
        cn, uid = RNX.load_node(c, "", "FunctorDeployProbe"; name = "probe1")
        @test cn isa RNX.ComponentNode
        @test uid == 1
        @test entities(cn, cn.members.p).out isa RNX.PublisherHandle   # autostarted: ports materialised
        @test cn.node.fqn == "/probe1"
        # default instance name path: empty name → _kind_default_name(schema)
        cn2, _ = RNX.load_node(c, "", "FunctorDeployProbe")
        @test cn2.node.fqn == "/FunctorDeployProbe"
        # add! programmatic forwarding (untyped K → run(::NodeSchema); name forwarded)
        cn3 = add!(c, schema; name = "probe3")
        @test cn3 isa RNX.ComponentNode
        @test length(RNX.list_nodes(c)) == 3
        # unload by id closes that node only
        @test RNX.unload_node(c, uid)
        @test !isopen(cn.node)
        @test length(RNX.list_nodes(c)) == 2
        @test RNX.unload_node(c, uid) == false                          # idempotent: id already gone
        # unknown kind → ArgumentError
        @test_throws ArgumentError RNX.load_node(c, "", "NoSuchFunctorKind")
    finally
        close(c)
    end
end

# ── nits group: leak-on-throw, client-kind describe_wiring, NodeFirstAction A-on-type ──────────────
module FNits
    using ROSNode
    import ROSNode: member_schema, construct_port
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    # A member with TWO ports whose SECOND construct_port throws — exercise the mid-materialise unwind.
    mutable struct TwoPort{Name} <: Component{Name} end
    struct Boom{Name} <: ROSNode.PortDesc{Name} end                 # a custom descriptor whose construct_port throws
    ROSNode.descname(::Type{Boom{N}}) where {N} = N
    ROSNode.handle_type(::Type{Boom{N}}) where {N} = Any
    ROSNode.port_descs(::Boom, node, w) = ROSNode.EndpointDesc[]     # no a-priori endpoint (keep id window simple)
    construct_port(::Boom, core, cn, m, pv, wm) = error("boom from second port")
    member_schema(::Type{TwoPort}) = component(TwoPort, publishes(:ok, _T), Boom{:bad}())
end

# A uses() client member (top-level so it can see the top-level FAct.Counter action type), to exercise
# describe_wiring's client-kind rendering.
mutable struct Caller{Name} <: Component{Name} end
member_schema(::Type{Caller}) = component(Caller, uses(:ctr, FAct.Counter; on = "~/ctr"))

@testset "functor: nits group" begin
    # (1) leak-on-throw: the first port built + registered, the second threw — the configure unwind must
    #     close the orphan so no /np/ok endpoint survives in the context graph after the failed bring-up.
    ctx = RNX.Context(; localhost_only = true)
    @test_throws ErrorException run(node("np" => FNits.TwoPort); ctx = ctx, name = "np", block = false)
    @test isempty(filter(e -> e.topic == "/np/ok", collect(values(ctx.graph.local_endpoints))))
    close(ctx)

    # (5) describe_wiring renders a uses() action client port (acli) under the service channel
    cn = run(node("caller" => Caller); name = "nw", block = false)
    try
        io = IOBuffer(); RNX.describe_wiring(io, cn); s = String(take!(io))
        @test occursin("ctr", s)
        @test occursin("acli", s)                                   # action_client kind rendered, not the raw symbol
        @test !occursin("action_client", s)                         # the symbol name itself must not leak through
        @test occursin("/nw/ctr", s)                                # resolved under the service channel (private name)
    finally
        close(cn)
    end

    # (2) NodeFirstAction pins the action type on its type param and stays callable per the 3-arg shape
    h = runs(FInline.countup).handler
    @test h isa RNX.NodeFirstAction
    @test typeof(h).parameters[2] === RNX.action_type(RNX.ActionTypeSupport(typeof(FInline.countup)))
end

@testset "functor: describe_wiring (functor node)" begin
    cn = run(node("front" => remap(Pinger, :ping => "front/ping"),
                  "log"   => remap(Ponger, :ping => (:front, :ping))); name = "dw", block = false)
    try
        io = IOBuffer()
        RNX.describe_wiring(io, cn)                 # must NOT throw on a functor node (the fixed bug)
        s = String(take!(io))
        @test occursin("front", s) && occursin("ping", s)
        @test occursin("/front/ping", s)           # remap resolved + shown
    finally
        close(cn)
    end
end

# ── @component: the concise authoring tier that EMITS the value API in one expression ───────────────
module CMacro
    using ROSNode
    @ros_package "functor_cmacro_test"
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time

    @component mutable struct Sensor{Name} <: Component{Name}
        n::Int64 = 0
        last::Float64 = 0.0
        @param rate::Int64 = 5 ∈ 1..50
        @param "gain doc" gain::Float64 = 1.0
        @publishes out::_T on "~/telemetry"
        @hears function ingest(node, m::Sensor, msg::_T)
            m.n += 1
        end
        @every :rate function tick(node, m::Sensor)
            m.last += 1.0
        end
        @service "~/q" function q(node, m::Sensor, x::Int64)::@NamedTuple{ok::Bool}
            (ok = x > 0,)
        end
        configure(node, m::Sensor) = (m.n = 100; nothing)   # hook → extends ROSNode.configure (not a shadow)
    end

    @interface BatterySource batt(_)
    @component mutable struct Batt                         # bare head (macro injects {Name}) + @provides + zero ports
        level::Float64 = 88.0
        @provides BatterySource
    end
    batt(b::Batt) = b.level

    import ROSNode: member_schema                          # a raw DI consumer to wire against the @component'd provider
    mutable struct Guard{Name, B} <: Component{Name}; b::B; end
    mkguard(node, ::Val{N}, src) where {N} = Guard{N, typeof(src)}(src)
    member_schema(::Type{Guard}) = component(Guard; requires = (BatterySource,), ctor = mkguard)

    # DI consumers authored ENTIRELY with @component: `@requires field::Marker` adds the hidden param that
    # holds the injected sibling + the `construct` that places it — no hand-written parametric struct or ctor.
    @component mutable struct GuardC{Name} <: Component{Name}        # a concrete sibling dependency
        @requires src::Batt
        seen::Int64 = 0
    end
    readc(g::GuardC) = batt(g.src)                                   # type-stable: src is the concrete sibling

    @component mutable struct GuardX{Name} <: Component{Name}        # an @interface dependency, same sugar
        @requires src::BatterySource
        @param thresh::Float64 = 10.0
    end

    # a raw parametric holder with NO ctor — the default `construct` builds it from its own type
    mutable struct HolderC{Name, B} <: Component{Name}; b::B; end
    member_schema(::Type{HolderC}) = component(HolderC; requires = (BatterySource,))

    # a FIELD-LESS @component provider — its zero-arg ctor must use the implicit inner ctor, not a
    # self-recursive emitted one. Carries its own interface so it doesn't collide with Batt's BatterySource.
    @interface Pinger2 png(_)
    @component mutable struct Stateless{Name} <: Component{Name}
        @provides Pinger2
    end
    png(::Stateless) = 7

    # two dependencies in one @component — an @interface and a concrete component
    @component mutable struct TwoDep{Name} <: Component{Name}
        @requires p::Pinger2
        @requires sib::Batt
        k::Int64 = 0
    end
    twosum(t::TwoDep) = png(t.p) + Int(batt(t.sib))
end

@testset "functor: @component macro" begin
    ms = RNX.member_schema(CMacro.Sensor)
    @test ms isa RNX.MemberSchema
    @test RNX.port_names(ms) == [:out, :ingest, :tick, :q]      # ports in block order
    @test RNX.param_names(ms) == [:rate, :gain]                  # @param → @parameters struct
    @test fieldnames(RNX.paramtype(typeof(ms))) == (:rate, :gain)
    @test hasmethod(RNX.member_schema, Tuple{Type{CMacro.Sensor}})   # on the BARE base (#12 fixed by construction)
    @test CMacro.Sensor{:s}().n == 0                            # zero-arg ctor from inline defaults
    @test RNX.provides_of(typeof(RNX.member_schema(CMacro.Batt))) == (CMacro.BatterySource,)

    QReq = CMacro.functor_cmacro_test.srv.q_Request
    cn = run(CMacro.Sensor; name = "cmsensor", block = false)   # promote (un-prefixed flat params)
    cli = ServiceClient(Node(cn.node.context, "cmcli"), "/cmsensor/q", QReq)
    try
        e = entities(cn, cn.members.sensor)
        @test e.out isa RNX.PublisherHandle && e.ingest isa RNX.SubscriptionHandle
        @test e.tick isa RNX.Timer && e.q isa RNX.ServiceHandle
        @test parameters(cn, cn.members.sensor).rate == 5
        @test cn.members.sensor.n == 100                      # the configure hook ran (m::Sensor injected)
        @test wait_for_service(cli; timeout = 5)
        @test call(cli, QReq(x = Int64(3))).ok === true         # inline @service round-trips
    finally
        close(cli); close(cn); close(cn.node.context)
    end

    # @provides composes via DI against a raw consumer
    v = run(node("batt" => CMacro.Batt, "guard" => CMacro.Guard); name = "cmveh", block = false)
    try
        @test v.members.guard isa CMacro.Guard{:guard, CMacro.Batt{:batt}}
        @test CMacro.batt(v.members.guard.b) == 88.0
    finally
        close(v)
    end

    # @requires (concrete sibling): the macro adds the hidden param + construct; the field is the concrete
    # sibling type, so the read is type-stable — no hand-written `GuardC{Name,B}` or ctor.
    gc = run(node("batt" => CMacro.Batt, "g" => CMacro.GuardC); name = "cmgc", block = false)
    try
        @test gc.members.g isa CMacro.GuardC{:g, CMacro.Batt{:batt}}        # hidden param fixed to the sibling
        @test fieldtype(typeof(gc.members.g), :src) === CMacro.Batt{:batt}
        @test CMacro.batt(gc.members.g.src) == 88.0
        @test RNX.requires_of(typeof(RNX.member_schema(CMacro.GuardC))) == (CMacro.Batt,)
    finally
        close(gc)
    end
    @test isconc(CMacro.readc, (CMacro.GuardC{:g, CMacro.Batt{:batt}},))    # the injected read is type-stable

    # @requires (an @interface): the same sugar, requirement resolved by provision
    gx = run(node("batt" => CMacro.Batt, "g" => CMacro.GuardX); name = "cmgx", block = false)
    try
        @test gx.members.g isa CMacro.GuardX{:g, CMacro.Batt{:batt}}
        @test RNX.requires_of(typeof(RNX.member_schema(CMacro.GuardX))) == (CMacro.BatterySource,)
        @test parameters(gx, gx.members.g).thresh == 10.0                   # @param coexists with @requires
    finally
        close(gx)
    end

    # default `construct`: a raw parametric holder with NO ctor builds `HolderC{Name, typeof(dep)}(dep)`
    hn = RNX.build_members(node("batt" => CMacro.Batt, "h" => CMacro.HolderC), nothing)
    @test typeof(hn.h) === CMacro.HolderC{:h, CMacro.Batt{:batt}}
    @test fieldtype(typeof(hn.h), :b) === CMacro.Batt{:batt}

    # a field-less @component builds (the zero-arg ctor uses the implicit inner ctor, no self-recursion)
    @test CMacro.Stateless{:s}() isa CMacro.Stateless{:s}

    # two @requires in one @component — interface + concrete, both injected and type-stable
    td = RNX.build_members(node("s" => CMacro.Stateless, "b" => CMacro.Batt, "t" => CMacro.TwoDep), nothing)
    @test typeof(td.t) === CMacro.TwoDep{:t, CMacro.Stateless{:s}, CMacro.Batt{:b}}
    @test RNX.requires_of(typeof(RNX.member_schema(CMacro.TwoDep))) == (CMacro.Pinger2, CMacro.Batt)
    @test isconc(CMacro.twosum, (typeof(td.t),))

    # guards: a bare `@requires Marker` (no field) errors in @component (the field::Marker form is required);
    # a malformed `@uses` (missing the `name::T` decl) errors naming the directive.
    eb = try; @macroexpand(@component mutable struct GBad{Name} <: Component{Name}
            @requires BatterySource
        end); nothing; catch e; e; end
    @test eb !== nothing && occursin("@requires", sprint(showerror, eb)) && occursin("field::Marker", sprint(showerror, eb))
    eu = try; @macroexpand(@component mutable struct UBad{Name} <: Component{Name}
            @uses foo
        end); nothing; catch e; e; end
    @test eu !== nothing && occursin("@uses", sprint(showerror, eu))

    # @param rejects malformed forms at macro-expand, with a self-identifying error
    ep1 = try                                            # > 2 parts (a junk token between doc and field)
        @eval module _CMBadParam1
            using ROSNode
            @component mutable struct P1
                @param "doc" junk rate::Int64 = 5
            end
        end
        nothing
    catch e; e; end
    @test ep1 !== nothing && occursin("@param", sprint(showerror, ep1 isa LoadError ? ep1.error : ep1))
    ep2 = try                                            # a non-string-literal doc slot
        @eval module _CMBadParam2
            using ROSNode
            @component mutable struct P2
                @param ("a" * "b") rate::Int64 = 5
            end
        end
        nothing
    catch e; e; end
    @test ep2 !== nothing && occursin("@param", sprint(showerror, ep2 isa LoadError ? ep2.error : ep2))
end

# ── review-2 fixes: @service error-text parity, parametric-head guard, directive guards/provenance ──
module FxR2
    using ROSNode
    import ROSNode: member_schema
    @ros_package "functor_r2_test"
    mutable struct Svc{Name} <: Component{Name} end
    @service "~/boom" function boom(node, m::Svc, x::Int64)::@NamedTuple{ok::Bool}
        x < 0 && error("custom boom: x was $(x)")            # a throwing handler
        (ok = true,)
    end
    member_schema(::Type{Svc}) = component(Svc, boom)

    # a `where`-clause def in a @component block must not crash the macro (peel :where before the member check)
    @component mutable struct WC{Name} <: Component{Name}
        x::Int64 = 0
        bump(node, m::WC, y::T) where {T} = (m.x += 1; nothing)
    end
end

@testset "functor: review-2 fixes" begin
    # High — a throwing @service replies with the REAL error text (parity with the authored adapter),
    # not the generic "service handler aborted".
    Req = FxR2.functor_r2_test.srv.boom_Request
    cn  = run(FxR2.Svc; name = "r2svc", block = false)
    cli = ServiceClient(Node(cn.node.context, "r2c"), "/r2svc/boom", Req)
    try
        @test wait_for_service(cli; timeout = 5)
        @test call(cli, Req(x = Int64(1))).ok === true                # happy path unchanged
        err = try; call(cli, Req(x = Int64(-1)); timeout_ms = 5000); nothing; catch e; e; end
        @test err !== nothing && occursin("custom boom", sprint(showerror, err))
    finally
        close(cli); close(cn); close(cn.node.context)
    end

    # Medium — a parametric (DI) @component head is rejected at macro-expand with a clear @component error
    e1 = try; @macroexpand(@component mutable struct R2Bad{Name, B} <: Component{Name} end); nothing; catch e; e; end
    @test e1 !== nothing && occursin("type parameter", sprint(showerror, e1)) && occursin("@component", sprint(showerror, e1))

    # Low — error provenance: a malformed head names @component (threaded `who`), not @mixin
    e2 = try; @macroexpand(@component struct R2NoMut end); nothing; catch e; e; end
    @test e2 !== nothing && occursin("@component", sprint(showerror, e2))

    # Low — a no-arg directive gives a clear, directive-named error, not a raw BoundsError. The shared
    # engine names the DIRECTIVE (`@publishes`), since the same parser backs `@component` and `@schema`.
    e3 = try
        @macroexpand(@component mutable struct R2B3{Name} <: Component{Name}
            @publishes
        end)
        nothing
    catch e; e; end
    @test e3 !== nothing && occursin("@publishes", sprint(showerror, e3)) && !occursin("BoundsError", sprint(showerror, e3))

    # Low — `where`-clause reaction/helper accepted (peeled), member still builds
    @test FxR2.WC{:w}().x == 0
    @test RNX.member_schema(FxR2.WC) isa RNX.MemberSchema
end

# ── @schema: the member_schema EDSL (shared directive engine with @component, but emits only the
#    member_schema — the struct/handlers/hooks stay separate, preserving raw-API representation power) ──
module SchM
    using ROSNode
    import ROSNode: member_schema, configure
    @ros_package "functor_schema_test"
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time

    @interface BatterySource2 batt2(_)::Float64

    # provider via @schema (explicit Params type); struct + ctor + handler authored separately
    @parameters struct SenP; rate::Int64 = 5 ∈ 1..50; end
    mutable struct Sen{Name} <: Component{Name}; level::Float64; end
    Sen{Name}() where {Name} = Sen{Name}(88.0)
    batt2(s::Sen) = s.level
    pubtick(node, s::Sen) = (publish(entities(node, s).out, _T()); nothing)
    @schema Sen SenP begin
        @publishes out::_T on "~/telemetry"
        @every     pubtick at :rate
        @provides  BatterySource2
    end

    # DI consumer via @schema, HAPPY PATH: bare-marker @requires (evidence only) and NO @ctor — the
    # holder shape (free param past Name is the injected dep) is built by the default `construct`, which
    # calls the type itself: `Grd{Name, typeof(src)}(src)`. The struct + @service handler are hand-written.
    mutable struct Grd{Name, B} <: Component{Name}; src::B; end
    @parameters struct GrdP; minb::Float64 = 20.0; end
    @service "~/safe" function safe2(node, g::Grd, t::Float64)::@NamedTuple{ok::Bool}
        (ok = batt2(g.src) >= parameters(node, g).minb && t <= 100.0,)
    end
    @schema Grd GrdP begin
        @serves   safe2
        @requires BatterySource2
    end

    # DI consumer via @schema with an explicit @ctor — for a CUSTOM shape the default can't build (an
    # extra non-dep field). The ctor places `Name`, threads the dep, and sets the extra state.
    mutable struct GrdK{Name, B} <: Component{Name}; src::B; tag::Symbol; end
    mkgrdk(node, ::Val{N}, src) where {N} = GrdK{N, typeof(src)}(src, :custom)
    @schema GrdK begin
        @requires BatterySource2
        @ctor     mkgrdk
    end

    # inline @param (no explicit P), a reference @hears, a renamed+retopiced @hears, and a client @uses
    hear(node, m, msg::_T) = (m.n += 1; nothing)
    mutable struct Mix{Name} <: Component{Name}; n::Int64; end
    Mix{Name}() where {Name} = Mix{Name}(0)
    @schema Mix begin
        @param gain::Float64 = 1.0
        @hears hear::_T
        @hears alt => hear::_T on "/other"
        @uses  cli::functor_schema_test.srv.safe2_Request on "/svc"   # client port + trailing wire
    end

    # the SAME new directive forms now available in @component: reference handler, name+topic override,
    # at-rate timer, @serves/@runs by reference (pre-authored @service/@action handlers), and the legacy
    # leading-wire inline @hears (a backward-compat spelling).
    @service "~/qq" function qq(node, m, x::Int64)::@NamedTuple{ok::Bool}; (ok = x > 0,); end
    @action "~/cnt" function cnt(node, m, target::Int32,
                                 fb::FeedbackSink{@NamedTuple{cur::Int32}})::@NamedTuple{total::Int32}
        (total = target,)
    end
    whear(node, m, msg::_T) = (m.k += 1; nothing)
    wtick(node, m) = (m.k += 1; nothing)
    @component mutable struct Wkr{Name} <: Component{Name}
        k::Int64 = 0
        @param  hz::Int64 = 10
        @hears  whear::_T
        @hears  alt => whear::_T on "/alt"
        @hears  "/legacy" function lh(node, m::Wkr, msg::_T); m.k += 1; nothing; end   # legacy leading-wire inline
        @every  wtick at :hz
        @serves qq
        @runs   cnt
    end
end

@testset "functor: @schema + unified directive forms" begin
    # @schema emits a MemberSchema (not a NodeSchema) on the bare base, with ports in block order.
    sen = RNX.member_schema(SchM.Sen)
    @test sen isa RNX.MemberSchema
    @test RNX.port_names(sen) == [:out, :pubtick]
    @test RNX.param_names(sen) == [:rate]                              # explicit SenP referenced
    @test RNX.provides_of(typeof(sen)) == (SchM.BatterySource2,)

    grd = RNX.member_schema(SchM.Grd)
    @test RNX.port_names(grd) == [:safe2]                             # @serves by reference
    @test RNX.requires_of(typeof(grd)) == (SchM.BatterySource2,)     # @requires = evidence only

    mix = RNX.member_schema(SchM.Mix)
    @test RNX.port_names(mix) == [:hear, :alt, :cli]                  # reference, override, client
    @test RNX.param_names(mix) == [:gain]                             # inline @param → MixParams
    _mp(nm) = mix.ports[findfirst(p -> RNX.descname(typeof(p)) === nm, mix.ports)]
    @test _mp(:alt) isa RNX.Sub && _mp(:alt).wire == "/other"        # name + topic override resolved
    @test _mp(:cli) isa RNX.Use && _mp(:cli).wire == "/svc"          # @uses client port + trailing wire
    @test RNX.handle_type(typeof(_mp(:cli))) <: RNX.ServiceClient    # marker → concrete service-client handle

    # @schema DI composes. Grd (no @ctor) is built by the DEFAULT construct = the type constructor;
    # GrdK (@ctor) is built by its custom ctor (which the default can't — it sets the extra `tag` field).
    m = RNX.build_members(node("sen" => SchM.Sen, "grd" => SchM.Grd, "grdk" => SchM.GrdK), nothing)
    @test typeof(m.grd) === SchM.Grd{:grd, SchM.Sen{:sen}}              # default ctor: Grd{Name, typeof(src)}(src)
    @test SchM.batt2(m.grd.src) == 88.0
    @test typeof(m.grdk) === SchM.GrdK{:grdk, SchM.Sen{:sen}} && m.grdk.tag === :custom   # @ctor override ran

    # a @schema-authored single node runs end-to-end (promote, un-prefixed params).
    cn = run(SchM.Sen; name = "schsen", block = false)
    try
        e = entities(cn, cn.members.sen)
        @test e.out isa RNX.PublisherHandle && e.pubtick isa RNX.Timer
        @test parameters(cn, cn.members.sen).rate == 5
    finally
        close(cn); close(cn.node.context)
    end

    # the new directive forms in @component lower the same way: reference @hears, name+topic override,
    # legacy leading-wire inline @hears, at-rate @every, @serves + @runs by reference.
    wkr = RNX.member_schema(SchM.Wkr)
    @test RNX.port_names(wkr) == [:whear, :alt, :lh, :wtick, :qq, :cnt]
    @test RNX.param_names(wkr) == [:hz]
    _wp(nm) = wkr.ports[findfirst(p -> RNX.descname(typeof(p)) === nm, wkr.ports)]
    @test _wp(:alt) isa RNX.Sub && _wp(:alt).wire == "/alt"          # trailing-on override
    @test _wp(:lh)  isa RNX.Sub && _wp(:lh).wire  == "/legacy"       # legacy leading-wire inline @hears
    @test _wp(:wtick) isa RNX.Tmr                                    # @every … at :hz
    @test _wp(:qq)  isa RNX.Srv                                      # @serves by reference
    @test _wp(:cnt) isa RNX.Act                                      # @runs by reference (action server)

    # @param ⊕ explicit-Params is rejected at macro-expand, self-identifying.
    ep = try
        @eval module _SchBadP
            using ROSNode
            @parameters struct PP; a::Int = 1; end
            mutable struct C{Name} <: Component{Name}; end
            @schema C PP begin
                @param b::Int = 2
            end
        end
        nothing
    catch e; e; end
    @test ep !== nothing && occursin("explicit parameters struct", sprint(showerror, ep isa LoadError ? ep.error : ep))

    # @component's `field::Marker` @requires form is rejected in @schema (which doesn't author the struct).
    er = try
        @eval module _SchBadReq
            using ROSNode
            mutable struct C2{Name, B} <: Component{Name}; b::B; end
            @schema C2 begin
                @requires src::Int
            end
        end
        nothing
    catch e; e; end
    @test er !== nothing && occursin("field::Marker", sprint(showerror, er isa LoadError ? er.error : er))
end

# ── directive-engine guards: the side conditions + the two leading-wire / tuple-requires defects ──────
const _GT = ROSNode.Interfaces.builtin_interfaces.msg.Time
_expand_err(ex) = try; Core.eval(@__MODULE__, ex); nothing; catch e; e isa LoadError ? e.error : e; end

@testset "functor: directive-engine guards" begin
    # SC3 — a bare `@hears` reference has no message type and is rejected (a subscription carries no trait).
    e_bare = _expand_err(:(@macroexpand @component mutable struct _GBareHears{Name} <: Component{Name}
        @hears somehandler
    end))
    @test e_bare !== nothing && occursin("@hears", sprint(showerror, e_bare)) &&
          occursin("message type", sprint(showerror, e_bare))

    # SC2 — at most one wire: a leading "wire" string AND a trailing `on "wire"` together is an error.
    e_two = _expand_err(:(@macroexpand @component mutable struct _GTwoWire{Name} <: Component{Name}
        @hears "lead" function h(node, m, msg::_GT); nothing; end on "trail"
    end))
    @test e_two !== nothing && occursin("not both", sprint(showerror, e_two))

    # SC2 sibling — `on` followed by a non-string literal is rejected.
    e_onbad = _expand_err(:(@macroexpand @component mutable struct _GOnBad{Name} <: Component{Name}
        @publishes out::_GT on somewhere
    end))
    @test e_onbad !== nothing && occursin("wire string literal", sprint(showerror, e_onbad))

    # Bug B (fixed) — the decl directives take the wire ONLY trailing; a leading string is rejected with a
    # directive-named pointer (it used to be silently applied as the wire).
    e_lead = _expand_err(:(@macroexpand @component mutable struct _GLeadPub{Name} <: Component{Name}
        @publishes "leadwire" out::_GT
    end))
    @test e_lead !== nothing && occursin("@publishes", sprint(showerror, e_lead)) &&
          occursin("trailing", sprint(showerror, e_lead))

    # Bug A (fixed) — @schema's `@requires` validates EACH tuple entry, so a `field::Marker` smuggled inside
    # a tuple is rejected at macro-expand (it used to slip through to a cryptic late UndefVarError).
    e_tup = _expand_err(:(module _GTupReq
        using ROSNode
        @interface _GS gsens(_)
        mutable struct C{Name, B} <: Component{Name}; b::B; end
        gmk(node, ::Val{N}, s) where {N} = C{N, typeof(s)}(s)
        @schema C begin
            @requires (_GS, bad::Int)
            @ctor gmk
        end
    end))
    @test e_tup !== nothing && occursin("field::Marker", sprint(showerror, e_tup))
end

# ── port-typed handlers: the message/request type may ride the PORT (`name::T =>`), not the handler ────
module PT
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    pref(node, m, msg) = nothing                         # untyped msg — the type comes from the port
    @component mutable struct PCC{Name} <: Component{Name}
        n::Int64 = 0
        @hears odom::_T => function oin(node, m::PCC, msg); m.n += 1; nothing; end  # port type + inline, UNTYPED msg
        @hears track::_T => pref                                                     # port type + reference
        @hears "/lead" wired::_T => function win(node, m::PCC, msg); nothing; end    # leading wire + port-typed inline
        @hears plain => pref::_T                                                     # handler-typed reference (rename)
        @hears ::_T => function noname(node, m::PCC, msg); nothing; end              # type-only port, default name → :noname
        @hears ::_T => pref                                                          # type-only port, reference → :pref
    end
end

@testset "functor: port-typed handlers" begin
    s = RNX.member_schema(PT.PCC)
    @test RNX.port_names(s) == [:odom, :track, :wired, :plain, :noname, :pref]
    _p(nm) = s.ports[findfirst(p -> RNX.descname(typeof(p)) === nm, s.ports)]
    # the type rode the PORT (left of `=>`) and threaded to `Sub{Name, T, F}` — even though inline `oin`'s
    # msg arg is UNTYPED: the macro emitted the handler verbatim and never read its signature.
    @test _p(:odom)  isa RNX.Sub && typeof(_p(:odom)).parameters[2]  === PT._T
    @test _p(:track) isa RNX.Sub && typeof(_p(:track)).parameters[2] === PT._T   # port type + reference
    @test _p(:wired) isa RNX.Sub && _p(:wired).wire == "/lead"                   # leading wire honoured
    @test typeof(_p(:wired)).parameters[2] === PT._T
    @test typeof(_p(:plain)).parameters[2] === PT._T   # `name::T => h` ≡ `name => h::T` — same Sub
    # `::T =>` states the type only and keeps the default port name (the handler's).
    @test _p(:noname) isa RNX.Sub && typeof(_p(:noname)).parameters[2] === PT._T   # inline, default name :noname
    @test _p(:pref)   isa RNX.Sub && typeof(_p(:pref)).parameters[2]   === PT._T   # reference, default name :pref
    @test only(methods(PT.oin)).sig.parameters[4] === Any   # untyped msg left untouched

    # guards: the type stated twice, on a timer, or on an inline service, and a malformed port LHS.
    g(ex) = (e = _expand_err(ex); e === nothing ? "NO ERROR" : sprint(showerror, e))
    @test occursin("given twice", g(:(@macroexpand @component mutable struct _GBoth{Name} <: Component{Name}
        @hears a::_GT => h::_GT
    end)))
    @test occursin("timer carries no message type", g(:(@macroexpand @component mutable struct _GTmr{Name} <: Component{Name}
        @every a::_GT => tick at :hz
    end)))
    @test occursin("derives its type", g(:(@macroexpand @component mutable struct _GSrv{Name} <: Component{Name}
        @serves s::_GT => function srv(node, m, x::Int64)::@NamedTuple{ok::Bool}; (ok = true,); end
    end)))
    @test occursin("port override is", g(:(@macroexpand @component mutable struct _GBadL{Name} <: Component{Name}
        @hears (a + b) => h::_GT
    end)))
    # a leading wire + type-only port collide in the parser (`"w" ::T` → `("w")::T`) — targeted hint.
    @test occursin("leading wire binds to the type", g(:(@macroexpand @component mutable struct _GWT{Name} <: Component{Name}
        @hears "/w" ::_GT => function h(node, m, msg); nothing; end
    end)))
end

# ── review-3 fixes: qualified handler refs, @schema arg guards, default-construct guidance, e2e by-ref ──
module QH                                   # handlers in a SIBLING module — the external-handler case
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    odom(node, m, msg::_T) = nothing
    beat(node, m) = nothing
end
module QHUse
    using ROSNode
    import ..QH
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    mutable struct W{Name} <: Component{Name}; n::Int; end
    W{Name}() where {Name} = W{Name}(0)
    @schema W begin
        @hears QH.odom::_T              # qualified reference; port name = :odom (final path component)
        @every QH.beat at 5
        @hears watch => QH.odom::_T     # qualified ref + name override
    end
end
module NM                              # the member arg is typed BY THE USER (any name); the macro never rewrites it
    using ROSNode
    @component mutable struct Probe{Name} <: Component{Name}
        x::Int64 = 0
        @param rate::Int64 = 1
        @every :rate function step(node, self::Probe)     # typed, NOT named `m`
            self.x += 1
        end
        configure(node, comp::Probe) = (comp.x = 7; nothing)   # typed, named `comp`
    end
end

@testset "functor: review-3 fixes" begin
    # Qualified handler references (Mod.f) lower; the port name defaults to the final path component.
    w = RNX.member_schema(QHUse.W)
    @test RNX.port_names(w) == [:odom, :beat, :watch]
    _qp(nm) = w.ports[findfirst(p -> RNX.descname(typeof(p)) === nm, w.ports)]
    @test _qp(:odom) isa RNX.Sub && _qp(:beat) isa RNX.Tmr && _qp(:watch) isa RNX.Sub

    # The member argument is emitted VERBATIM (any name, by position) — the macro neither rewrites nor
    # judges it: a typed arg keeps its type, a bare arg stays an ordinary `::Any` catch-all (no error).
    @test only(methods(NM.step)).sig.parameters[3] === NM.Probe       # `step(node, self::Probe)` — as written
    @test which(ROSNode.configure, (Any, NM.Probe{:p})).sig.parameters[3] === NM.Probe   # `configure(node, comp::Probe)`
    @eval module _BareMember                                         # a bare member arg compiles — left ::Any
        using ROSNode
        @component mutable struct B{Name} <: Component{Name}
            x::Int64 = 0
            @param rate::Int64 = 1
            @every :rate function step(node, m); m.x += 1; end       # bare `m` — not rewritten, not rejected
        end
    end
    @test only(methods(_BareMember.step)).sig.parameters[3] === Any   # the macro left the signature untouched

    # @schema argument-validation guards each error clearly, source-located.
    _g(ex) = (e = _expand_err(ex); e === nothing ? "NO ERROR" : sprint(showerror, e))
    @test occursin("the first argument is the component state type", _g(:(@macroexpand @schema 5 begin end)))
    @test occursin("too many arguments", _g(:(@macroexpand @schema A B C begin end)))
    @test occursin("must be a `begin", _g(:(@macroexpand @schema A notablock)))
    @test occursin("parameters struct TYPE", _g(:(@macroexpand @schema A "oops" begin end)))   # non-type Params
    @test occursin("given more than once", _g(:(@eval module _DupCtor; using ROSNode
        mutable struct C{N, B} <: Component{N}; b::B; end
        mk(n, v, d) = C{:x, typeof(d)}(d)
        @schema C begin @requires Int; @ctor mk; @ctor mk; end; end)))
    @test occursin("block holds directives only", _g(:(@eval module _Stray; using ROSNode
        mutable struct C{N} <: Component{N}; end
        @schema C begin; x = 1; end; end)))

    # Default `construct` gives shape guidance (not a raw MethodError) for an extra-field holder.
    eA = try
        @eval module _XField
            using ROSNode
            @interface Bs3 bb3(_)::Float64
            mutable struct Prov{N} <: Component{N}; v::Float64; end
            Prov{N}() where {N} = Prov{N}(1.0); bb3(p::Prov) = p.v
            ROSNode.member_schema(::Type{Prov}) = component(Prov; provides = (Bs3,))
            mutable struct Bad{N, B} <: Component{N}; src::B; tag::Symbol; end   # extra non-dep field, no ctor
            ROSNode.member_schema(::Type{Bad}) = component(Bad; requires = (Bs3,))
        end
        RNX.build_members(node("p" => _XField.Prov, "b" => _XField.Bad), nothing); nothing
    catch e; e; end
    @test eA !== nothing && occursin("other fields or a different parameter shape", sprint(showerror, eA))

    # @component nodefault error names the DI `construct`, not a (nonexistent) zero-arg ctor.
    eC = _expand_err(:(@eval module _NoDefDI; using ROSNode
        @interface Bs4 bb4(_)
        @component mutable struct C{Name} <: Component{Name}
            raw::Int64
            @requires src::Bs4
        end; end))
    @test eC !== nothing && occursin("construct(::Type{C}", sprint(showerror, eC))

    # End-to-end: a @component with @serves/@runs BY REFERENCE materialises a live service + action server,
    # and an at-rate reference timer fires.
    cn = run(SchM.Wkr; name = "wkr", block = false)
    try
        e = entities(cn, cn.members.wkr)
        @test e.qq isa RNX.ServiceHandle           # @serves qq (by reference) → live service handle
        @test e.cnt isa RNX.ActionServer            # @runs cnt (by reference) → live action server
        sleep(0.4)
        @test cn.members.wkr.k > 0                   # @every wtick at :hz — the reference timer fired
    finally
        close(cn); close(cn.node.context)
    end
end
