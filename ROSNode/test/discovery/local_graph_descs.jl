# Drift guard for the a-priori local-graph enumeration (discovery/local_graph.jl).
# `node_endpoint_descs` must reproduce EXACTLY the `is_local` endpoint set a node
# materialises — by (kind, topic, type, qos) — across every endpoint-bearing port
# kind plus the node's own shell / get_type_description / parameter services. The
# enumeration derives each endpoint's topic/type through the SAME helpers
# `make_entity` uses, so this asserts it cannot fall behind a pattern's wiring (the
# same shape as the `_construction_precompile_specs` drift guard).
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, ActionServer, node_endpoint_descs, EndpointDesc
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_lgctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

# A mixin covering every statically-derivable endpoint kind + a parameter; a second,
# port-less mixin proves a zero-port member contributes nothing.
module _LGTypes
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    @mixin struct Probe; end
    @param Probe rate::Int64 = 10
    @publishes Probe out :: _T
    @hears function ingest(node, m::Probe, msg::_T) end
    @serves "~/query" function query(node, m::Probe, x::Float64)::@NamedTuple{ok::Bool} end
    @every :rate function beat(node, m::Probe) end
    @mixin struct Idle; end
end
# Reference qualified (not `using`): the suite shares one `Main`, and another test file
# already imports a `Probe` there — an unqualified import would warn-conflict.
@node _LGComp = ["p" => _LGTypes.Probe, "o" => _LGTypes.Idle]

# BYO action type (robot_msgs/action/Process), as patterns/actions.jl declares it.
module _LGAct
    using ROSNode
    @ros_import from="../fixtures/byo" "robot_msgs/action/Process"
end
const Process        = _LGAct.robot_msgs.action.Process
const Process_Result = _LGAct.robot_msgs.action.Process_Result

# A stable, value-based multiset key: `repr` of an EndpointDesc shows its field
# VALUES, so equal descs compare equal regardless of object identity.
_bag(ds) = sort(map(repr, ds))

# The node's materialised local endpoints as EndpointDescs (the comparison oracle):
# every is_local index entry — created by the node shell + each `make_entity` — minus
# the gid the a-priori enumeration cannot yet know. One node per Context, so is_local
# selects exactly this node's set.
_materialised(ctx) =
    [EndpointDesc(e.kind, e.topic, e.type, e.qos)
     for e in ROSNode.endpoints_snapshot(ctx) if e.is_local]

@testset "a-priori local graph reproduces the materialised set" begin
    @testset "composed node: shell + params + pub/sub/service/timer" begin
        _lgctx() do ctx
            cn = run(_LGComp; ctx = ctx, name = "lg", block = false)
            got, truth = _bag(node_endpoint_descs(cn)), _bag(_materialised(ctx))
            got == truth || @info "drift" missing = setdiff(truth, got) extra = setdiff(got, truth)
            @test got == truth
            # B3b: every materialised entity landed on the id its a-priori local-graph entry
            # used — its liveliness key is in the local graph with a matching gid (so the
            # declared token and our own graph agree). A reserved-id mismatch would inject a
            # SECOND entry (different lv_key) → the bag compare above already fails; this pins
            # the gid equality directly.
            le = ctx.graph.local_endpoints
            for e in cn.node.entities
                @test haskey(le, e.lv_key)
                @test le[e.lv_key].gid == e.gid
            end
            # no orphan, no double-count: one local entry per endpoint (entities + shell).
            @test length(_materialised(ctx)) == length(node_endpoint_descs(cn))
            # B4: local_graph(cn) is the node-as-built's own is_local slice (one node per ctx
            # ⇒ exactly the local set), and describe_graph renders it.
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
end
