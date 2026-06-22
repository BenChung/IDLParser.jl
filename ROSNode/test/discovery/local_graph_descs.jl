# Drift guard for the a-priori local-graph enumeration (discovery/local_graph.jl) over the
# FUNCTOR node API. The schema's a-priori endpoint enumeration must reproduce EXACTLY the
# `is_local` endpoint set a node materialises — by (kind, topic, type, qos) — across every
# endpoint-bearing port kind plus the node's own shell / get_type_description / parameter
# services. The enumeration derives each endpoint's topic/type through the SAME `_*_desc`
# helpers `construct_port` ultimately reaches, so this asserts it cannot fall behind a port's
# wiring (the same shape as the `precompile_schema` drift guard).
#
# `node_endpoint_descs(cn::ComponentNode)` forks on the member kind (`hasmethod(mixin_spec, …)`):
# an @mixin member enumerates via `mixin_spec`, a functor member (a `member_schema`-backed state
# type) via `member_schema(base).ports`. So the public one-arg a-priori oracle is valid here and is
# what this guard asserts against the materialised set. The bare-`Node` form is substrate-level.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ActionServer, node_endpoint_descs, EndpointDesc
using Test
import ROSNode: member_schema

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_lgctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# A member covering every statically-derivable endpoint kind + a parameter, and a second,
# port-less member proving a zero-port member contributes nothing. `@service` inline authoring
# gives the `~/query` service its request/response type + a node-first handler in one place.
module _LGTypes
    using ROSNode
    import ROSNode: member_schema
    @ros_package "local_graph_test"
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time

    @parameters struct ProbeParams
        rate::Int64 = 10
    end
    mutable struct Probe{Name} <: Component{Name} end
    lg_ingest(node, m::Probe, msg::_T) = nothing
    lg_beat(node, m::Probe) = nothing
    @service "~/query" function lg_query(node, m::Probe, x::Float64)::@NamedTuple{ok::Bool}
        (ok = true,)
    end
    member_schema(::Type{Probe}) = component(Probe, ProbeParams,
        publishes(:out, _T),
        hears(:ingest, _T, lg_ingest),
        lg_query,                                   # @service descriptor auto-converts on use
        every(:beat, :rate, lg_beat))

    mutable struct Idle{Name} <: Component{Name} end
    member_schema(::Type{Idle}) = component(Idle)

    # A state type used ONLY via an inline `component(…)` value at the node() call site — it has NO
    # `member_schema` method, so introspection that re-derived `member_schema(base)` would MethodError.
    mutable struct InlineProbe{Name} <: Component{Name} end
end
const _Probe = _LGTypes.Probe
const _Idle  = _LGTypes.Idle

# BYO action type (robot_msgs/action/Process), as patterns/actions.jl declares it.
module _LGAct
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/action/Process"
end
const Process        = _LGAct.robot_msgs.action.Process
const Process_Result = _LGAct.robot_msgs.action.Process_Result

# A stable, value-based multiset key: `repr` of an EndpointDesc shows its field VALUES, so equal
# descs compare equal regardless of object identity.
_bag(ds) = sort(map(repr, ds))

# The node's materialised local endpoints as EndpointDescs (the comparison oracle): every
# is_local index entry — created by the node shell + each materialised port — minus the gid the
# a-priori enumeration cannot yet know. One node per Context, so is_local selects exactly this
# node's set.
_materialised(ctx) =
    [EndpointDesc(e.kind, e.topic, e.type, e.qos)
     for e in ROSNode.endpoints_snapshot(ctx) if e.is_local]

@testset "a-priori local graph reproduces the materialised set" begin
    @testset "composed node: shell + params + pub/sub/service/timer" begin
        _lgctx() do ctx
            schema = node("p" => _Probe, "o" => _Idle; name = "lg", register = false)
            cn = run(schema; ctx = ctx, name = "lg", block = false)
            got, truth = _bag(node_endpoint_descs(cn)), _bag(_materialised(ctx))
            got == truth || @info "drift" missing = setdiff(truth, got) extra = setdiff(got, truth)
            @test got == truth
            # B3b: every materialised entity landed on the id its a-priori local-graph entry used —
            # its liveliness key is in the local graph with a matching gid (so the declared token and
            # our own graph agree). A reserved-id mismatch would inject a SECOND entry (different
            # lv_key) → the bag compare above already fails; this pins the gid equality directly.
            le = ctx.graph.local_endpoints
            for e in cn.node.entities
                @test haskey(le, e.lv_key)
                @test le[e.lv_key].gid == e.gid
            end
            # no orphan, no double-count: one local entry per endpoint (member ports + shell).
            @test length(_materialised(ctx)) == length(node_endpoint_descs(cn))
            # B4: local_graph(cn) is the node-as-built's own is_local slice (one node per ctx ⇒
            # exactly the local set), and describe_graph renders it.
            @test _bag(local_graph(cn)) ==
                  _bag(EndpointInfo[e for e in ROSNode.endpoints_snapshot(ctx) if e.is_local])
            out = sprint(describe_graph, cn)
            @test occursin("local graph of /lg", out)
            @test occursin("~/query", out) || occursin("/lg/query", out)   # the service endpoint
        end
    end

    @testset "action server expands to its five endpoints" begin
        _lgctx() do ctx
            node = Node(ctx, "lga")
            ActionServer(node, "/lga/proc", Process) do g
                ROSNode.succeed(g, Process_Result(; done = Int32(1)))
            end
            mat  = _materialised(ctx)
            act  = filter(d -> occursin("/_action/", d.topic), mat)
            base = filter(d -> !occursin("/_action/", d.topic), mat)
            # the five-endpoint action expansion, derived a-priori from the type + name
            @test _bag(ROSNode._action_descs(node, "/lga/proc", Process)) == _bag(act)
            # the bare node enumeration is exactly the non-action remainder (shell + gtd)
            @test _bag(node_endpoint_descs(node)) == _bag(base)
        end
    end

    # ComponentNode source-of-truth: a member built from an inline `component(…)` VALUE has no
    # `member_schema` method, yet introspection reads the node's FROZEN per-member schema, so the public
    # a-priori oracle still works (and reproduces the materialised set) and describe_wiring renders it.
    @testset "inline component(…) member — frozen-schema introspection" begin
        _lgctx() do ctx
            inl    = component(_LGTypes.InlineProbe, publishes(:beat, _LGTypes._T))
            schema = node("ip" => inl; name = "lgi", register = false)
            cn     = run(schema; ctx = ctx, name = "lgi", block = false)
            @test _bag(node_endpoint_descs(cn)) == _bag(_materialised(ctx))   # no MethodError; matches reality
            out = sprint(describe_wiring, cn)
            @test occursin("beat", out)
        end
    end
end
