# Functor node API — parametric dependency injection. Pure-unit blocks need no session;
# the live block runs nodes on the suite's private router (`ROS_TEST_EP`) with `block=false`,
# driven in-process. Proves: DI resolution picks the composed instantiation while a no-`requires`
# standalone schema picks the null-provider one; the DI field read is type-stable (no `Any`-cascade);
# parameter typing is base-keyed + type-stable; and the inline `@service` member-arg guard rejects a
# curly (concrete-instantiation) member annotation.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ServiceClient, call, wait_for_service,
               parameters, entities
import ROSNode: member_schema, configure          # generics we EXTEND (must import to add methods)
using Test

const RNX = ROSNode
const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_cctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# The component types live in a submodule so the inline-authored service type generates at module
# load. `import ROSNode: member_schema` is load-bearing: a bare definition under plain `using` would
# define a fresh local trait ROSNode never dispatches on.
module _ParamDI
    using ROSNode
    import ROSNode: member_schema
    @ros_package "pdi"                     # names the inline-authored service type

    @interface BatterySource  battery(_)::Float64

    # monomorphic provider (also the F2 regression subject)
    @parameters struct SensorParams
        rate::Int64 = 5
    end
    mutable struct Sensor{Name} <: Component{Name}
        level::Float64
    end
    Sensor{Name}() where {Name} = Sensor{Name}(88.0)
    battery(s::Sensor) = s.level
    member_schema(::Type{Sensor}) = component(Sensor, SensorParams; provides = (BatterySource,))

    struct NullBattery end                 # null-object provider for the standalone schema
    battery(::NullBattery) = 0.0

    # parametric consumer: the DI slot is a type parameter, not Any (Name is the member path).
    mutable struct Guard{Name, B} <: Component{Name}
        battery_src::B
    end
    make_guard(node, ::Val{Name}, src) where {Name} = Guard{Name, typeof(src)}(src)        # injected ⇒ Guard{name,Sensor}
    make_guard_standalone(node, ::Val{Name}) where {Name} = Guard{Name, NullBattery}(NullBattery())
    @parameters struct GuardParams
        min_battery::Float64 = 20.0
    end
    @service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
    end
    # the composed (DI) schema: requires a BatterySource sibling
    member_schema(::Type{Guard}) = component(Guard, GuardParams, safe;
        requires = (BatterySource,), ctor = make_guard)
    # the standalone schema: no `requires`, a 0-dep ctor injecting the null provider. (The @mixin
    # API folded both into one mixin via two `construct` arities + a `run(Guard)` that skipped DI;
    # the functor API resolves DI eagerly at node(), so a no-sibling load needs its own no-requires
    # schema value.)
    standalone_guard() = component(Guard, GuardParams, safe; ctor = make_guard_standalone)

    # Any-field control: same shape as `safe`, untyped slot — the devirtualization detector below
    # must FAIL on this one (pins the detector itself).
    mutable struct AnyGuard
        battery_src::Any
    end
    function safe_any(g::AnyGuard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= 20.0 && target_altitude <= 100.0, battery = b)
    end

    # multi-parameter base extraction, incl. a bounded parameter
    mutable struct Fuse{Name, A, B <: Real} <: Component{Name}
        a::A
        b::B
    end
    make_fuse(node, ::Val{Name}) where {Name} = Fuse{Name, Symbol, Float64}(:sym, 2.5)
    member_schema(::Type{Fuse}) = component(Fuse; ctor = make_fuse)

    # parametric provider: where-form interface method + an explicit 0-dep ctor (composition feeds
    # only `requires` deps in, so this provider takes none).
    mutable struct PSensor{Name, T} <: Component{Name}
        v::T
    end
    battery(s::PSensor) = Float64(s.v)
    make_psensor(node, ::Val{Name}) where {Name} = PSensor{Name, Float64}(77.0)
    member_schema(::Type{PSensor}) = component(PSensor; provides = (BatterySource,), ctor = make_psensor)

    # direct-component DI: require a concrete sibling component (no interface). The dep field is
    # typed by the component base itself — and the ctor annotates it.
    mutable struct Watch{Name, S} <: Component{Name}
        sensor::S
    end
    make_watch(node, ::Val{Name}, s::Sensor) where {Name} = Watch{Name, typeof(s)}(s)
    member_schema(::Type{Watch}) = component(Watch; requires = (Sensor,), ctor = make_watch)

    # bad requirement: an entry that is neither an @interface nor a Component type (Int64 passes the
    # entry/ctor checks at component(), then fails resolution at node()). A valid ctor keeps the focus on
    # the node()-level "neither" error rather than the component()-level requires-without-ctor guard.
    mutable struct BadReq{Name} <: Component{Name} end
    badreq_ctor(node, ::Val{N}, dep) where {N} = BadReq{N}()
    member_schema(::Type{BadReq}) = component(BadReq; requires = (Int64,), ctor = badreq_ctor)
end
using ._ParamDI

const PVehicle  = node("sensor" => _ParamDI.Sensor,  "guard" => _ParamDI.Guard;  name = "PVehicle")
const PProvider = node("sensor" => _ParamDI.PSensor, "guard" => _ParamDI.Guard;  name = "PProvider")
const PWatch    = node("sensor" => _ParamDI.Sensor,  "watch" => _ParamDI.Watch;  name = "PWatch")   # direct-component dep

# DI-read probe: the headline inference subject, free of the (annotated) handler return and of
# `parameters` (whose accessor carries one deliberate Any-typed statement before its server assert,
# so a whole-handler Any scan would false-alarm).
_di_read(g) = _ParamDI.battery(g.battery_src)

_ci(f, ts)     = first(only(Base.code_typed(f, ts)))
# terminators (return/goto) carry a placeholder Any ssavaluetype — only value statements count
_ssa_any(ci)   = any(i -> ci.ssavaluetypes[i] === Any &&
                          !(ci.code[i] isa Union{Core.ReturnNode, Core.GotoNode, Core.GotoIfNot}),
                     eachindex(ci.code))
# a surviving dynamic call to the interface method. With a where-form method among the applicable
# set (this module's `battery(::PSensor{Y})`) the union-split fallback is a dynamic `:call` to
# `battery` — NOT a `Core.throw_methoderror` arm, which only appears for all-concrete method tables
# — so call-presence is the discriminating signal in this module shape.
_dyn_call(ci)  = any(stmt -> stmt isa Expr && stmt.head === :call &&
                             stmt.args[1] isa GlobalRef && stmt.args[1].name === :battery,
                     ci.code)

@testset "component parametric DI" begin

    @testset "base extraction, DI ctor, schema (pure-unit)" begin
        # the standalone (no-requires) schema's 0-dep ctor picks NullBattery
        m = _ParamDI.make_guard_standalone(nothing, Val(:g))
        @test m isa _ParamDI.Guard{:g, _ParamDI.NullBattery}
        @test RNX._basetype(typeof(m)) === _ParamDI.Guard
        # member_schema keys on the base, so it covers every instantiation
        @test RNX.member_schema(_ParamDI.Guard) isa RNX.MemberSchema
        @test RNX.statetype(typeof(RNX.member_schema(_ParamDI.Guard))) === _ParamDI.Guard
        # member_schema dispatches on the BASE, so it covers every instantiation — its param-schema
        # type is the element type of the node's per-member pservers carrier
        @test RNX.paramtype(typeof(RNX.member_schema(_ParamDI.Guard))) === _ParamDI.GuardParams

        # multi-parameter (bounded) base
        f = _ParamDI.make_fuse(nothing, Val(:f))
        @test f isa _ParamDI.Fuse{:f, Symbol, Float64}
        @test RNX._basetype(typeof(f)) === _ParamDI.Fuse

        # monomorphic default ctor untouched (DefaultCtor builds S{Name}())
        @test RNX.member_schema(_ParamDI.Sensor).ctor(nothing, Val(:s)) isa _ParamDI.Sensor
    end

    @testset "DI requirement resolution (pure-unit)" begin
        # an interface requirement resolves to the providing sibling
        edges = RNX.resolve_di_edges([:sensor, :guard],
            [(), (_ParamDI.BatterySource,)], [(_ParamDI.BatterySource,), ()],
            Any[_ParamDI.Sensor, _ParamDI.Guard])
        @test edges[:guard]  == [:sensor]
        @test edges[:sensor] == Symbol[]

        # a requirement naming a concrete sibling component resolves to that member (direct-component DI)
        dedges = RNX.resolve_di_edges([:sensor, :watch],
            [(), (_ParamDI.Sensor,)], [(), ()], Any[_ParamDI.Sensor, _ParamDI.Watch])
        @test dedges[:watch]  == [:sensor]
        @test dedges[:sensor] == Symbol[]

        # the injected dep is typed by the resolved sibling — no free parameter, no Any
        wn = RNX.build_members(PWatch, nothing)
        @test fieldtype(typeof(wn.watch), :sensor) === _ParamDI.Sensor{:sensor}

        # two members of the required component ⇒ ambiguous
        @test_throws "multiple" node("s1" => _ParamDI.Sensor, "s2" => _ParamDI.Sensor,
                                     "watch" => _ParamDI.Watch)
        # zero members of the required component ⇒ unsatisfied (self does not count)
        @test_throws "no sibling" node("watch" => _ParamDI.Watch)
        # a requirement that is neither an @interface nor a Component type ⇒ clear error
        @test_throws "neither" node("b" => _ParamDI.BadReq)
    end

    @testset "headline: the DI read is type-stable (pure-unit)" begin
        GS = _ParamDI.Guard{:g, _ParamDI.Sensor{:s}}
        @test fieldtype(GS, :battery_src) === _ParamDI.Sensor{:s}
        ci = _ci(_di_read, (GS,))
        @test !_ssa_any(ci) && !_dyn_call(ci)
        # negative controls, one per detector — un-ORed, so a silently dead detector
        # fails the suite instead of hiding behind the other
        ci_any = _ci(_ParamDI.safe_any, (_ParamDI.AnyGuard, Float64))
        @test _ssa_any(ci_any)
        @test _dyn_call(ci_any)
        @test _ssa_any(_ci(_di_read, (_ParamDI.AnyGuard,)))
    end

    @testset "parameter typing is base-keyed + type-stable (pure-unit)" begin
        # `member_schema` dispatches on the base, so its `paramtype` covers every instantiation —
        # that param schema type is the element type of the node's per-member `pservers` carrier.
        @test RNX.paramtype(typeof(RNX.member_schema(_ParamDI.Guard)))  === _ParamDI.GuardParams
        @test RNX.paramtype(typeof(RNX.member_schema(_ParamDI.Sensor))) === _ParamDI.SensorParams
        # `parameters(node, m)` reads the carrier and returns the param schema type concretely,
        # because `current(::ParameterServer{P}) :: P`. Here we pin that underlying type-stability.
        @test Base.return_types(RNX.current,
                  (RNX.ParameterServer{_ParamDI.SensorParams},)) == [_ParamDI.SensorParams]
    end

    @testset "guards: inline-handler curly member arg (pure-unit)" begin
        # the @service/@action member arg must cover the component BASE, not a curly instantiation —
        # driven through eval (not macroexpand) so the registration side-effect would be real under
        # a guard regression.
        err = try
            Core.eval(_ParamDI, quote
                ROSNode.@service function bads(node, m::Guard{Int}, x::Float64)::@NamedTuple{ok::Bool}
                    (ok = true,)
                end
            end)
            nothing
        catch e
            e
        end
        @test err !== nothing && occursin("cover the component base", sprint(showerror, err))
        err2 = try
            Core.eval(_ParamDI, quote
                ROSNode.@action function badr(node, m::Guard{Int}, n::Int64,
                          fb::ROSNode.FeedbackSink{@NamedTuple{p::Int64}})::@NamedTuple{r::Int64}
                    (r = n,)
                end
            end)
            nothing
        catch e
            e
        end
        @test err2 !== nothing && occursin("cover the component base", sprint(showerror, err2))

        # load-by-name registers the schema under its kind name (the @node `node_kind` analog)
        @test RNX.node_kind("PVehicle") === PVehicle
    end

    @testset "standalone + composed DI + parametric provider (Zenoh session)" begin
        _cctx() do ctx
            # standalone FIRST: the no-requires schema's 0-dep ctor injects NullBattery
            cn = run(node("guard" => _ParamDI.standalone_guard());
                     ctx = ctx, name = "pguard", block = false)
            @test cn.members[:guard] isa _ParamDI.Guard{:guard, _ParamDI.NullBattery}

            # composed: DI injects the monomorphic Sensor (member named :sensor)
            v = run(PVehicle; ctx = ctx, name = "pvehicle", block = false)
            g = v.members[:guard]
            @test g isa _ParamDI.Guard{:guard, _ParamDI.Sensor{:sensor}}

            # entities accessor across instantiations: `entities(node, m)` is type-stable —
            # the path is a constant from `m`'s type, the ports carrier is the node's typed field
            rts = Base.return_types(entities, (typeof(v), typeof(g)))
            @test length(rts) == 1 && isconcretetype(only(rts))

            # end-to-end: the reaction reads the provider through the interface
            ground = Node(ctx, "pground")
            SafeReq = _ParamDI.pdi.srv.safe_Request
            client = ServiceClient(ground, "/pvehicle/safe_to_fly", SafeReq)
            @test wait_for_service(client; timeout = 5)
            resp = call(client, SafeReq(target_altitude = 50.0); timeout_ms = 5000)
            @test resp.ok === true
            @test resp.battery == 88.0
            close(client)

            # parametric provider: the 0-dep ctor picks PSensor{Float64}; the where-form interface
            # method dispatches on it
            pv = run(PProvider; ctx = ctx, name = "pprov", block = false)
            pg = pv.members[:guard]
            @test pg isa _ParamDI.Guard{:guard, _ParamDI.PSensor{:sensor, Float64}}
            @test _ParamDI.battery(pg.battery_src) == 77.0

            # direct-component DI: Watch requires the concrete sibling Sensor (no interface); the
            # injected dep is the very Sensor member instance, stored in a typed field
            wn = run(PWatch; ctx = ctx, name = "pwatch", block = false)
            @test wn.members[:watch] isa _ParamDI.Watch
            @test wn.members[:watch].sensor === wn.members[:sensor]
            @test _ParamDI.battery(wn.members[:watch].sensor) == 88.0
        end
    end
end
