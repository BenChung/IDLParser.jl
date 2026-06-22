# `handle_type` exactness guard (functor API). The materialised-ports NamedTuple TYPE is derived from
# the descriptor TYPES ALONE — `node_ports_carrier` stamps each member's `PortCell{ports_nt_type(Ports)}`
# from the schema type, never from a materialised value. That static type MUST equal what the live
# materialise path captures (`typeof(entities(node, m))`), or the cell asserts the wrong type and the
# materialised handles are stranded on it. This builds a member covering every statically-typed port
# kind (publisher / subscription / service / timer), runs it on the suite's private router, and asserts
# the static derivation matches the materialised NamedTuple field-for-field.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, entities, ports_nt_type, handle_type,
               PublisherHandle, SubscriptionHandle, ServiceHandle, Timer, ROS,
               Pub, Sub, Srv, Act, Tmr
import ROSNode: member_schema          # EXTENDED below (must import, not `using`, to add methods)
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_sectx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
const _GP_Req = ROSNode.Interfaces.rcl_interfaces.srv.GetParameters_Request

# All four statically-typed kinds on one member (no DI ⇒ runs standalone).
mutable struct Probe{Name} <: Component{Name} end
_probe_ingest(node, m::Probe, msg::_T) = nothing
_probe_query(node, m::Probe, req) = ROSNode._default_msg(ROSNode.response_type(_GP_Req))
_probe_beat(node, m::Probe) = nothing
member_schema(::Type{Probe}) = component(Probe,
    publishes(:out, _T),
    hears(:ingest, _T, _probe_ingest),
    serves(:query, _GP_Req, _probe_query; on = "~/query"),
    every(:beat, 10, _probe_beat))

@testset "entities static derivation matches live materialise" begin
    ports = member_schema(Probe).ports
    nt = ports_nt_type(typeof(ports))
    # Unlike the @mixin `_ports_nt_type` (which returned `nothing` for non-derivable kinds), the functor
    # `ports_nt_type` is total — every kind here is concrete.
    @test isconcretetype(nt)

    _sectx() do ctx
        n = run(node("probe" => Probe); ctx = ctx, name = "probe", block = false)
        m = n.members.probe
        live = typeof(entities(n, m))

        # The headline guard: static == live, exactly.
        @test live === nt

        # Spelled out per kind, so a divergence names the offending handle type.
        @test fieldnames(live) == (:out, :ingest, :query, :beat)   # descriptor order
        @test fieldtype(live, :out)    <: PublisherHandle
        @test fieldtype(live, :ingest) <: SubscriptionHandle
        @test fieldtype(live, :query)  <: ServiceHandle
        @test fieldtype(live, :beat)   === Timer{ROS}
        for d in ports
            @test fieldtype(live, ROSNode.descname(typeof(d))) === handle_type(typeof(d))
        end
    end
end

# Drift guard for the functor construction/codec precompile bake (`precompile_schema`). The per-member,
# per-descriptor anchors (codec leaves via `_anchor_port`, the a-priori `port_descs`, the wire
# resolution, the handle-cell write) are reached through a runtime `getfield(members, nm)` the
# `precompile(run)` path can't specialize, so they only bake offline through `precompile_schema`. A
# renamed/re-aritied anchor silently de-bakes (reads green). Pure: resolves signatures, opens no session.
@testset "construction-path precompile anchors resolve" begin
    R = ROSNode
    schema = node("probe" => Probe)
    NS = typeof(schema)
    @test R.precompile_schema(NS) === nothing                       # bakes without erroring
    # Codec resolution for the component service request/response stays valid (the `Srv` descriptor
    # resolved request/response eagerly into its type params at `serves()`).
    sdesc = only(d for d in member_schema(Probe).ports if d isa Srv)
    Rq = typeof(sdesc).parameters[2]; Rs = typeof(sdesc).parameters[3]
    @test precompile(R.decode_owned, (Memory{UInt8}, Type{Rq}))
    @test precompile(R.encode, (Rs,))
end

# Drift guard for the action goal/result/feedback CODEC anchors. In the @mixin API an action port was
# GATED OUT of the static accessor bake (its handle type wasn't derivable, so `_ports_nt_type` returned
# `nothing`), and a separate `_action_codec_precompile_specs` was the only offline codec coverage. In the
# functor API `ports_nt_type` is total, and the action's handle is the CONCRETE `ActionServer{A,G,R,Fb}`
# (resolved eagerly into the `Act` `H` param at `runs()`), so the accessor is NOT gated and there is no
# separate per-action codec spec list — `precompile_schema` is the single bake.
# A minimal `@action` member exercises the goal/result/feedback codecs through it.
module _ActEnt
    using ROSNode
    import ROSNode: member_schema
    @ros_package "act_drift"
    mutable struct Mover{Name} <: Component{Name} end
    @action function go(node, m::Mover, n::Int64,
                        fb::FeedbackSink{@NamedTuple{k::Int64}})::@NamedTuple{done::Bool}
        (done = true,)
    end
    member_schema(::Type{Mover}) = component(Mover, go)
end

@testset "action codec precompile anchors resolve" begin
    R = ROSNode
    Mover = _ActEnt.Mover
    ports = member_schema(Mover).ports
    nt = R.ports_nt_type(typeof(ports))
    # The action server's handle type is resolved EAGERLY into the `Act` descriptor's `H` type param at
    # `runs()` (normal world), so — like pub/sub/service/client — the cell is the CONCRETE
    # `ActionServer{A,G,R,Fb}`, not the abstract `Any`. (Resolving it in `handle_type` from this
    # @generated carrier would hit the world-age trap; the eager `H` param avoids it.)
    @test nt <: NamedTuple
    @test fieldnames(nt) == (:go,)
    adesc = only(d for d in ports if d isa Act)
    HA = R.handle_type(typeof(adesc))
    @test HA <: R.ActionServer && isconcretetype(HA)
    @test fieldtype(nt, R.descname(typeof(adesc))) === HA

    # The single offline bake covers the action member's codecs (no separate spec list to drift).
    schema = node("svc" => Mover)
    @test R.precompile_schema(typeof(schema)) === nothing
end
