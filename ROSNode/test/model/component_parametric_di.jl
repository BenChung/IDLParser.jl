# Component layer — parametric mixins for type-stable dependency injection
# (PLAN-PARAMETRIC-MIXINS.md). Pure-unit blocks need no session; the live block runs
# nodes on the suite's private router (`ROS_TEST_EP`) with `block=false`, driven
# in-process. Proves: a parametric `@mixin` registers and loads by its base; the
# zero-dep `construct` picks the standalone instantiation while DI picks the composed
# one; the DI field read is type-stable (no `Any`-cascade); and the generated
# accessors cover every instantiation through base-keyed signatures.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ServiceClient, call, wait_for_service,
               parameters, entities, construct
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_cctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# The mixin types live in a submodule so kinds register and schemas generate at module
# load. `import ROSNode: requires, construct` is load-bearing: a bare definition under
# plain `using` would define fresh local functions ROSNode never calls.
module _ParamDI
    using ROSNode
    import ROSNode: requires, construct
    @ros_package "pdi"                     # names the inline-authored service type

    @interface BatterySource  battery(_)::Float64

    # monomorphic provider (also the F2 regression subject)
    @mixin struct Sensor
        level::Float64 = 88.0
    end
    @param Sensor rate::Int64 = 5
    battery(s::Sensor) = s.level
    @provides Sensor BatterySource

    struct NullBattery end                 # null-object provider for standalone loads
    battery(::NullBattery) = 0.0

    # parametric consumer: the DI slot is a type parameter, not Any
    @mixin struct Guard{B}
        battery_src::B
    end
    requires(::Type{Guard}) = (BatterySource,)
    construct(::Type{Guard}, node, src) = Guard(battery_src = src)            # injected ⇒ Guard{Sensor}
    construct(::Type{Guard}, node)      = Guard(battery_src = NullBattery())  # standalone ⇒ Guard{NullBattery}
    @param Guard min_battery::Float64 = 20.0
    @serves "~/safe_to_fly" function safe(g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= parameters(g).min_battery && target_altitude <= 100.0, battery = b)
    end

    # Any-field control: same shape as `safe`, untyped slot — the devirtualization
    # detector below must FAIL on this one (pins the detector itself).
    mutable struct AnyGuard
        battery_src::Any
    end
    function safe_any(g::AnyGuard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
        b = battery(g.battery_src)
        (ok = b >= 20.0 && target_altitude <= 100.0, battery = b)
    end

    # requires-only parametric mixin, NO zero-dep construct ⇒ the Phase-1.6 error
    @mixin struct NeedsDep{B}
        dep::B
    end
    requires(::Type{NeedsDep}) = (BatterySource,)
    construct(::Type{NeedsDep}, node, src) = NeedsDep(dep = src)

    # multi-parameter base extraction, incl. a bounded parameter
    @mixin struct Fuse{A, B <: Real}
        a::A
        b::B
    end
    construct(::Type{Fuse}, node) = Fuse(a = :sym, b = 2.5)

    # parametric provider: declared by its UnionAll, where-form interface method, and
    # an explicit zero-dep construct (composition feeds only `requires` deps in)
    @mixin struct PSensor{T}
        v::T
    end
    battery(s::PSensor{Y}) where {Y} = Float64(s.v)
    @provides PSensor BatterySource
    construct(::Type{PSensor}, node) = PSensor(v = 77.0)

    const GuardInt = Guard{Int}        # value-level instantiation for the backstop test
end
using ._ParamDI

@node PVehicle  = ["sensor" => _ParamDI.Sensor,  "guard" => _ParamDI.Guard]
@node PProvider = ["sensor" => _ParamDI.PSensor, "guard" => _ParamDI.Guard]

# DI-read probe: the headline inference subject, free of the (annotated) handler
# return and of `parameters` (whose accessor carries one deliberate Any-typed
# statement before its server assert, so a whole-handler Any scan would false-alarm).
_di_read(g) = _ParamDI.battery(g.battery_src)

_ci(f, ts)     = first(only(Base.code_typed(f, ts)))
# terminators (return/goto) carry a placeholder Any ssavaluetype — only value
# statements count
_ssa_any(ci)   = any(i -> ci.ssavaluetypes[i] === Any &&
                          !(ci.code[i] isa Union{Core.ReturnNode, Core.GotoNode, Core.GotoIfNot}),
                     eachindex(ci.code))
# a surviving dynamic call to the interface method. With a where-form method among
# the applicable set (this module's `battery(::PSensor{Y})`) the union-split fallback
# is a dynamic `:call` to `battery` — NOT a `Core.throw_methoderror` arm, which only
# appears for all-concrete method tables — so call-presence is the discriminating
# signal in this module shape.
_dyn_call(ci)  = any(stmt -> stmt isa Expr && stmt.head === :call &&
                             stmt.args[1] isa GlobalRef && stmt.args[1].name === :battery,
                     ci.code)

@testset "component parametric DI" begin

    @testset "base extraction, registry, construct (pure-unit)" begin
        m = construct(_ParamDI.Guard, nothing)
        @test m isa _ParamDI.Guard{_ParamDI.NullBattery}
        @test ROSNode._base(typeof(m)) === _ParamDI.Guard
        @test ROSNode.mixin_spec(ROSNode._base(typeof(m))) === ROSNode.mixin_spec(_ParamDI.Guard)
        @test ROSNode.ismixin(_ParamDI.Guard)
        @test ROSNode.pschema(_ParamDI.Guard) === ROSNode.pschema(_ParamDI.Guard{_ParamDI.Sensor})

        # multi-parameter (bounded) base
        f = construct(_ParamDI.Fuse, nothing)
        @test f isa _ParamDI.Fuse{Symbol, Float64}
        @test ROSNode._base(typeof(f)) === _ParamDI.Fuse

        # monomorphic fallback untouched
        @test construct(_ParamDI.Sensor, nothing) isa _ParamDI.Sensor
    end

    @testset "headline: the DI read is type-stable (pure-unit)" begin
        GS = _ParamDI.Guard{_ParamDI.Sensor}
        @test fieldtype(GS, :battery_src) === _ParamDI.Sensor
        ci = _ci(_di_read, (GS,))
        @test !_ssa_any(ci) && !_dyn_call(ci)
        # full handler: no surviving dynamic call to `battery`. (Base.return_types is
        # masked by the declared return annotation, and an Any-SSA scan is masked by
        # the F2 accessor's one deliberate Any-typed statement when inlined.)
        @test !_dyn_call(_ci(_ParamDI.safe, (GS, Float64)))
        # negative controls, one per detector — un-ORed, so a silently dead detector
        # fails the suite instead of hiding behind the other
        ci_any = _ci(_ParamDI.safe_any, (_ParamDI.AnyGuard, Float64))
        @test _ssa_any(ci_any)
        @test _dyn_call(ci_any)
        @test _ssa_any(_ci(_di_read, (_ParamDI.AnyGuard,)))
    end

    @testset "accessor (F2/edit-2) regression, both shapes (pure-unit)" begin
        @test Base.return_types(parameters, (_ParamDI.Sensor,)) ==
              [ROSNode.pschema(_ParamDI.Sensor)]
        @test Base.return_types(parameters, (_ParamDI.Guard{_ParamDI.Sensor},)) ==
              [ROSNode.pschema(_ParamDI.Guard)]
        # constructed-but-never-run keeps the friendly error (not a raw TypeError)
        @test_throws "not materialised" parameters(construct(_ParamDI.Guard, nothing))
    end

    @testset "guards: standalone-load / reaction curly / @node curly (pure-unit)" begin
        # Phase-1.6: a parametric mixin with no zero-dep construct gets the clear error
        @test_throws "free type parameter" construct(_ParamDI.NeedsDep, nothing)

        # edit 4: a concrete mixin annotation in a reaction is an expansion error —
        # driven through eval (not macroexpand) so the registration side-effect check
        # below is real: under a guard regression the eval would run `_add_port!`
        err = try
            Core.eval(@__MODULE__,
                :(ROSNode.@hears function bad(m::_ParamDI.Guard{Int}, x::Float64) end))
            nothing
        catch e
            e
        end
        @test err !== nothing
        @test occursin("cover the mixin base", sprint(showerror, err))
        err2 = try
            Core.eval(@__MODULE__,
                :(ROSNode.@serves function bads(m::_ParamDI.Guard{Int}, x::Float64)::@NamedTuple{ok::Bool} end))
            nothing
        catch e
            e
        end
        @test err2 !== nothing && occursin("cover the mixin base", sprint(showerror, err2))
        err3 = try
            Core.eval(@__MODULE__,
                :(ROSNode.@runs function badr(m::_ParamDI.Guard{Int}, n::Int64,
                                              fb::ROSNode.FeedbackSink{@NamedTuple{p::Int64}})::@NamedTuple{r::Int64} end))
            nothing
        catch e
            e
        end
        @test err3 !== nothing && occursin("cover the mixin base", sprint(showerror, err3))
        # a value-level spelling (type alias) bypasses the syntactic guards — the
        # runtime registry backstop in `_add_port!` catches it instead
        err4 = try
            Core.eval(_ParamDI, :(ROSNode.@hears function aliased(m::GuardInt, x::Float64) end))
            nothing
        catch e
            e
        end
        @test err4 !== nothing && occursin("attach to the mixin base", sprint(showerror, err4))
        # … and no concrete-instantiation shadow entry was registered by any of them
        @test !ROSNode.ismixin(_ParamDI.Guard{Int})
        # instance-keyed lookups stay base-keyed by design — `mixin_spec` dispatches on
        # the base only, so an instantiation hits no method; call sites normalize first
        @test_throws MethodError ROSNode.mixin_spec(_ParamDI.Guard{_ParamDI.Sensor})
        # a value-level instantiation at the run entry gets the friendly redirect
        @test_throws "name the base mixin" ROSNode._check_runnable(_ParamDI.Guard{Int}, "run")
        # load-by-name registers the base
        @test ROSNode.node_kind("Guard") === _ParamDI.Guard

        # edit 7: a curly member RHS that isn't the remap form must name the base …
        @test_throws "name the base mixin" ROSNode._parse_member_rhs(:g, :(Guard{Sensor}))
        # … while the §4.4 remap form keeps parsing
        mix, remaps = ROSNode._parse_member_rhs(:front, :(CamP{image => "front/image"}))
        @test mix === :CamP && length(remaps) == 1
    end

    @testset "standalone + composed DI + parametric provider (Zenoh session)" begin
        _cctx() do ctx
            # standalone FIRST: materialises the base-keyed entities accessor on the
            # NullBattery instantiation, so a concrete-keyed bug would strand the
            # composed Guard{Sensor} below on the untyped generic
            cn = run(_ParamDI.Guard; ctx = ctx, name = "pguard", block = false)
            @test cn.members[:guard] isa _ParamDI.Guard{_ParamDI.NullBattery}

            # composed: DI injects the monomorphic Sensor
            v = run(PVehicle; ctx = ctx, name = "pvehicle", block = false)
            g = v.members[:guard]
            @test g isa _ParamDI.Guard{_ParamDI.Sensor}

            # entities accessor across instantiations: one base-keyed method, concrete NT
            rts = Base.return_types(entities, (typeof(g),))
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

            # parametric provider, declared by its UnionAll: the zero-dep construct
            # picks PSensor{Float64}; the where-form interface method dispatches on it
            pv = run(PProvider; ctx = ctx, name = "pprov", block = false)
            pg = pv.members[:guard]
            @test pg isa _ParamDI.Guard{_ParamDI.PSensor{Float64}}
            @test _ParamDI.battery(pg.battery_src) == 77.0
        end
    end
end
