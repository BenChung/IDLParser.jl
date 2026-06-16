# `_handle_type` exactness guard. The `@precompile_nodes` bake generates `entities(m::M)::PortsNT`
# from the specs ALONE (via `_ports_nt_type`/`_handle_type`), without materialising. That static
# type MUST equal what the live materialise path captures (`typeof(entities(m))`), or the baked
# accessor asserts the wrong type and the materialise path is stranded on it (the reuse-marker skip
# in `_define_entities_accessor!` won't re-emit). This builds a mixin covering every statically-derivable port kind
# (publisher / subscription / service / timer), runs it on the suite's private router, and asserts
# the static derivation matches the materialised NamedTuple field-for-field.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, entities, mixin_spec, _ports_nt_type, _handle_type,
               PublisherHandle, SubscriptionHandle, ServiceHandle, Timer, ROS
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_sectx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# All four statically-derivable kinds on one mixin (no DI ⇒ runs standalone).
module _StaticEnt
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    @mixin struct Probe; end
    @param Probe rate::Int64 = 10
    @publishes Probe out :: _T
    @hears function ingest(m::Probe, msg::_T) end
    @serves "~/query" function query(m::Probe, x::Float64)::@NamedTuple{ok::Bool} end
    @every :rate function beat(m::Probe) end
end
using ._StaticEnt: Probe

@testset "entities static derivation matches live materialise" begin
    specs = mixin_spec(Probe).ports
    nt = _ports_nt_type(specs)
    @test nt !== nothing                                   # every kind here is derivable

    _sectx() do ctx
        n = run(Probe; ctx = ctx, name = "probe", block = false)
        m = only(values(n.members))
        live = typeof(entities(m))

        # The headline guard: static == live, exactly.
        @test live === nt

        # Spelled out per kind, so a divergence names the offending handle type.
        @test fieldnames(live) == (:out, :ingest, :query, :beat)   # spec order
        @test fieldtype(live, :out)    <: PublisherHandle
        @test fieldtype(live, :ingest) <: SubscriptionHandle
        @test fieldtype(live, :query)  <: ServiceHandle
        @test fieldtype(live, :beat)   === Timer{ROS}
        for p in specs
            @test fieldtype(live, p.name) === _handle_type(p)
        end
    end
end

# Drift guard for the `@precompile_nodes` CONSTRUCTION anchors (`_anchor_construction!`).
# Unlike the tier-2 scaffolding anchors, these are built per-mixin from the specs, so a
# renamed/re-aritied builder silently de-anchors (the bake quietly stops covering the
# materialise/param path, reading green). Pure: resolves signatures, opens no session.
@testset "construction-path precompile anchors resolve" begin
    R = ROSNode
    R._ensure_schema!(Probe)
    @test R._entities_accessor_from_specs!(Probe) !== nothing
    @test R._anchor_construction!(Probe) === nothing      # runs clean on a concrete mixin

    P = R.pschema(Probe)
    specs = mixin_spec(Probe).ports
    # the materialise frame + typed parameter-server build path
    @test precompile(R._materialize_ports!, (R.Node, Probe, Symbol, Vector{R.PortSpec}, P, Dict{Symbol, String}))
    @test precompile(R.ParameterServer, (R.Node, P))
    @test precompile(R._build_pserver, (R.Node, Type{P}, NamedTuple{(), Tuple{}}))
    @test precompile(R.wire_parameter_services!, (R.ParameterServer{P},))
    # per-kind endpoint builders, on the concrete reaction-closure types
    pub = only(p for p in specs if p.kind === :publisher)
    @test precompile(R._make_publisher, (R.Node, String, Type{pub.msgtype}))
    tmr = only(p for p in specs if p.kind === :timer)
    @test precompile(R._paused_timer, (R.Node, R.Duration, R._cb_type(R._timer_cb, tmr.reaction, Probe)))
    # the sub/service builders are reached with `warmup = :off`; the kwcall anchor bakes the
    # `#f#` body ONLY when the keyword name is one the target declares (else just the kwsorter).
    @test :warmup in reduce(union, (Base.kwarg_decl(m) for m in methods(R._make_subscription)); init = Symbol[])
    @test :warmup in reduce(union, (Base.kwarg_decl(m) for m in methods(R._make_service)); init = Symbol[])
end
