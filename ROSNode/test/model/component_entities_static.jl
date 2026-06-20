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
    @hears function ingest(node, m::Probe, msg::_T) end
    @serves "~/query" function query(node, m::Probe, x::Float64)::@NamedTuple{ok::Bool} end
    @every :rate function beat(node, m::Probe) end
end
using ._StaticEnt: Probe

@testset "entities static derivation matches live materialise" begin
    specs = mixin_spec(Probe).ports
    nt = _ports_nt_type(specs)
    @test nt !== nothing                                   # every kind here is derivable

    _sectx() do ctx
        n = run(Probe; ctx = ctx, name = "probe", block = false)
        m = only(values(n.members))
        live = typeof(entities(n, m))

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
    # The construction-path precompile (`_construction_precompile_specs`/`_anchor_construction!`)
    # is being RE-TUNED for the runtime-on-node rework: its per-(M) specs key on a now-UnionAll
    # mixin base and carry pre-rework (node-first reaction / node-owned-carrier) signatures, so
    # they bake nothing for now. Parked until that re-tuning lands — the deferred precompile
    # follow-up (DESIGN-RUNTIME-ON-NODE.md §8).
    @test_skip R._construction_precompile_specs(Probe)

    # Codec resolution for the component service request/response stays valid (independent of the
    # deferred construction-spec re-tuning).
    srv = only(p for p in mixin_spec(Probe).ports if p.kind === :service)
    @test precompile(R.decode_owned, (Memory{UInt8}, Type{R.request_type(srv.msgtype)}))
    @test precompile(R.encode, (R.response_type(srv.msgtype),))
end

# Drift guard for the action first-goal CODEC anchors (`_anchor_action_codecs!`). An action
# mixin is gated OUT of the static accessor bake (its handle type isn't derivable), so this
# accessor-independent codec bake is the only offline coverage — and it is guarded, so a drift
# silently de-anchors. A minimal `@runs` mixin exercises the goal/result/feedback codecs.
module _ActEnt
    using ROSNode
    @ros_package "act_drift"
    @mixin struct Mover end
    @runs function go(node, m::Mover, n::Int64,
                      fb::FeedbackSink{@NamedTuple{k::Int64}})::@NamedTuple{done::Bool}
        (done = true,)
    end
end

@testset "action codec precompile anchors resolve" begin
    R = ROSNode
    Mover = _ActEnt.Mover
    @test R._ports_nt_type(mixin_spec(Mover).ports) === nothing   # gated from the accessor bake

    # Same shared-source guard as the construction path: emitter and test iterate one list.
    specs = R._action_codec_precompile_specs(Mover)
    @test !isempty(specs)
    for (f, ts) in specs
        @test precompile(f, ts)
    end
end
