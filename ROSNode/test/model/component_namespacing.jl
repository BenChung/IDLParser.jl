# Component layer §4.4/§7 — node-level parameter namespacing across a multi-mixin
# `@node`, and dynamic `LoadNode`/`ros2 component` composition. LIVE: nodes run on a
# real Zenoh session (the suite's private router, `ROS_TEST_EP`) with `block=false`,
# driven in-process. Proves: a composed node exposes one member-prefixed (`<member>.
# <field>`) `ros2 param` surface; a single-member `@node` is still prefixed while a
# mixin promoted to a node is not (§4.3); and a container hosts working
# `~/_container/{load_node,unload_node,list_nodes}` services + the programmatic API.
#
# Always run Zenoh tests under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, Subscription, ServiceClient, call, wait_for_service,
               CompositeParameterServer, ParameterServer,
               parameters, entities, get_parameters, set_parameters_atomically,
               describe_parameters, parameter_names, node_kind, node_kinds,
               load_node, unload_node, list_nodes, container, add!
using Test

const _EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
            get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_cctx(f::Function) = Context(f; peers = [_EP], localhost_only = true)

const _RMSG = ROSNode.Interfaces.rcl_interfaces.msg
const _RSRV = ROSNode.Interfaces.rcl_interfaces.srv
const _CSRV = ROSNode.Interfaces.composition_interfaces.srv

# Two param-bearing mixins (a shared `fps` field, to exercise prefixed disambiguation)
# in a submodule, so their kinds register and schemas generate.
module _CompTypes
    using ROSNode
    @mixin struct Cam; x::Int = 0; end
    @param Cam fps::Int64 = 30 ∈ 1..120
    @param Cam gain::Float64 = 1.0

    @mixin struct Lid; y::Int = 0; end
    @param Lid fps::Int64 = 10
    @param Lid range::Float64 = 50.0
end
using ._CompTypes

@node Rig = ["camera" => _CompTypes.Cam, "lidar" => _CompTypes.Lid]
@node Box = ["camera" => _CompTypes.Cam]

# Port-bearing mixins for the §4.4 wire-topic remap + clobber tests (a vendored msg
# type fills the ports — content is irrelevant, only the wire names matter here).
module _CompPorts
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time
    @mixin struct CamP; end
    @publishes CamP image :: _T
    @mixin struct LidP; end
    @publishes LidP image :: _T
    @mixin struct Fuse; end
    @hears function left(m::Fuse, msg::_T) end
    @hears function right(m::Fuse, msg::_T) end
end
using ._CompPorts

@node TwoCam  = ["front" => _CompPorts.CamP{image => "front/image"},      # isolate the two outputs
                 "rear"  => _CompPorts.CamP{image => "rear/image"}]
@node Fused   = ["front" => _CompPorts.CamP{image => "front/image"},      # cross-member wiring:
                 "fuse"  => _CompPorts.Fuse{left => front.image}]         #   fuse.left ⇐ front.image
@node Collide = ["a" => _CompPorts.CamP, "b" => _CompPorts.LidP]          # both publish "image" ⇒ clobber
@node Shared  = ["a" => _CompPorts.CamP, "b" => _CompPorts.LidP{image => "a/image"}]  # remap ⇒ no clobber

@testset "component namespacing + composition (Zenoh session)" begin

    @testset "multi-member node-level parameter namespacing (§4.4)" begin
        _cctx() do ctx
            rig = run(Rig; ctx = ctx, name = "rig", block = false)
            ps = rig.node.parameters
            @test ps isa CompositeParameterServer
            @test sort(string.(parameter_names(ps))) ==
                  ["camera.fps", "camera.gain", "lidar.fps", "lidar.range"]
            # reads route to the right member, by prefix
            @test get_parameters(ps, ["camera.fps", "lidar.fps"]) == [30, 10]
            @test get_parameters(ps, ["nope.x", "camera.bogus"]) == [nothing, nothing]
            # describe re-keys onto the prefixed name and keeps the member's metadata
            d = describe_parameters(ps, ["camera.fps"])[1]
            @test d.name === Symbol("camera.fps")
            @test d.ptype === ROSNode.PARAMETER_INTEGER
            # a set routes to one member, leaving the like-named sibling field untouched
            ok, _ = set_parameters_atomically(ps, [("camera.fps", 60)])
            @test ok
            @test get_parameters(ps, ["camera.fps", "lidar.fps"]) == [60, 10]
            # a rejected (out-of-range) set commits nothing
            bad, _ = set_parameters_atomically(ps, [("camera.fps", 999)])
            @test !bad
            @test get_parameters(ps, ["camera.fps"]) == [60]
            # node-level member-namespaced view (§3.5/§4.4)
            @test parameters(rig).camera.fps == 60
            @test parameters(rig).lidar.range == 50.0
            # mixin-local `parameters(m)` stays unprefixed
            @test parameters(rig.members[:camera]).fps == 60
        end
    end

    @testset "prefixing tracks the construction path (§4.3)" begin
        _cctx() do ctx
            # composed @node, single member ⇒ STILL prefixed
            box = run(Box; ctx = ctx, name = "box", block = false)
            @test box.node.parameters isa CompositeParameterServer
            @test sort(string.(parameter_names(box.node.parameters))) ==
                  ["camera.fps", "camera.gain"]
            # mixin promoted to a node ⇒ un-prefixed, mixin-local names
            cam = run(_CompTypes.Cam; ctx = ctx, name = "cam", block = false)
            @test cam.node.parameters isa ParameterServer
            @test sort(string.(parameter_names(cam.node.parameters))) == ["fps", "gain"]
        end
    end

    @testset "overrides: mixin-local applies to all; prefixed targets one (§4.3)" begin
        _cctx() do ctx
            rig = run(Rig; ctx = ctx, name = "rig_ov", block = false,
                      overrides = (fps = 25, var"camera.fps" = 77))
            ps = rig.node.parameters
            @test get_parameters(ps, ["camera.fps"]) == [77]   # prefixed wins for camera
            @test get_parameters(ps, ["lidar.fps"]) == [25]    # mixin-local hits lidar
        end
    end

    @testset "prefixed ros2 param surface over the wire" begin
        _cctx() do ctx
            rig = run(Rig; ctx = ctx, name = "wrig", block = false)
            gc = ServiceClient(rig.node, "/wrig/get_parameters", _RSRV.GetParameters_Request)
            sc = ServiceClient(rig.node, "/wrig/set_parameters_atomically", _RSRV.SetParametersAtomically_Request)
            @test wait_for_service(gc; timeout = 5)
            @test wait_for_service(sc; timeout = 5)
            sleep(0.4)
            g = call(gc, _RSRV.GetParameters_Request(; names = ["camera.fps", "lidar.fps"]); timeout_ms = 5000)
            @test [ROSNode._from_param_value(v) for v in g.values] == [30, 10]
            p = _RMSG.Parameter(; name = "lidar.fps", value = ROSNode._to_param_value(15))
            s = call(sc, _RSRV.SetParametersAtomically_Request(; parameters = [p]); timeout_ms = 5000)
            @test s.result.successful
            g2 = call(gc, _RSRV.GetParameters_Request(; names = ["lidar.fps"]); timeout_ms = 5000)
            @test ROSNode._from_param_value(g2.values[1]) == 15
            close(gc); close(sc)
        end
    end

    @testset "kind registry (§7)" begin
        @test node_kind("Rig") === Rig
        @test node_kind("Cam") === _CompTypes.Cam
        @test node_kind("nonesuch") === nothing
        @test all(in(node_kinds()), ("Rig", "Box", "Cam", "Lid"))
        # tolerate a namespaced plugin spelling by last segment
        @test ROSNode.resolve_node_kind("", "my_pkg::Rig") === Rig
    end

    @testset "dynamic LoadNode — programmatic (§7)" begin
        _cctx() do ctx
            c = container("ctr"; block = false) do _ end
            @test ROSNode.inner_node(c) isa Node
            @test isempty(list_nodes(c))
            (cn1, uid1) = load_node(c, "", "Rig"; name = "lr")
            @test uid1 == 1
            @test endswith(String(cn1.node.fqn), "lr")
            (cn2, uid2) = load_node(c, "", "Cam"; name = "lc", parameters = (fps = 5,))
            @test uid2 == 2
            @test get_parameters(cn2.node.parameters, ["fps"]) == [5]
            l = list_nodes(c)
            @test length(l) == 2 && first.(l) == [UInt64(1), UInt64(2)]
            @test unload_node(c, uid1)
            @test length(list_nodes(c)) == 1
            @test unload_node(c, 999) == false
            @test_throws ArgumentError load_node(c, "", "DoesNotExist")
            close(c)
        end
    end

    @testset "dynamic LoadNode — over the wire (ros2 component, §7)" begin
        _cctx() do ctx
            c = container("wctr"; block = false) do _ end
            loadc = ServiceClient(c.node, "/wctr/_container/load_node", _CSRV.LoadNode_Request)
            listc = ServiceClient(c.node, "/wctr/_container/list_nodes", _CSRV.ListNodes_Request)
            unloadc = ServiceClient(c.node, "/wctr/_container/unload_node", _CSRV.UnloadNode_Request)
            for cl in (loadc, listc, unloadc)
                @test wait_for_service(cl; timeout = 5)
            end
            sleep(0.4)

            @test isempty(call(listc, _CSRV.ListNodes_Request(); timeout_ms = 5000).full_node_names)

            req = _CSRV.LoadNode_Request(; package_name = "", plugin_name = "Rig",
                node_name = "wired", node_namespace = "", log_level = UInt8(0),
                remap_rules = String[], parameters = _RMSG.Parameter[], extra_arguments = _RMSG.Parameter[])
            lr = call(loadc, req; timeout_ms = 8000)
            @test lr.success
            @test lr.unique_id == 1
            @test endswith(lr.full_node_name, "wired")

            lst = call(listc, _CSRV.ListNodes_Request(); timeout_ms = 5000)
            @test lst.unique_ids == [UInt64(1)]

            ur = call(unloadc, _CSRV.UnloadNode_Request(; unique_id = UInt64(1)); timeout_ms = 5000)
            @test ur.success
            @test isempty(call(listc, _CSRV.ListNodes_Request(); timeout_ms = 5000).full_node_names)

            # unknown component ⇒ a clean failure reply, not an error
            bad = _CSRV.LoadNode_Request(; package_name = "", plugin_name = "Nope",
                node_name = "", node_namespace = "", log_level = UInt8(0),
                remap_rules = String[], parameters = _RMSG.Parameter[], extra_arguments = _RMSG.Parameter[])
            @test !call(loadc, bad; timeout_ms = 5000).success

            close(loadc); close(listc); close(unloadc)
        end
    end

    @testset "wire-topic remap + clobber (§4.4)" begin
        # own-port string remap
        w, remapped = ROSNode._resolve_wires(TwoCam)
        @test w[:front][:image] == "front/image"
        @test w[:rear][:image]  == "rear/image"
        @test (:front, :image) in remapped
        # cross-member ref resolves to the target's (remapped) wire
        wf, _ = ROSNode._resolve_wires(Fused)
        @test wf[:fuse][:left]   == "front/image"
        @test wf[:front][:image] == "front/image"
        # a remap naming a port the mixin doesn't declare is an error
        @test_throws ErrorException ROSNode._resolve_wires(
            ROSNode.NodeKind(:Bad, [ROSNode.NodeMember(:a, _CompPorts.CamP, Pair{Symbol, Any}[:nope => "x"])]))

        _cctx() do ctx
            # two publishers on one resolved name, no remap ⇒ hard clobber error at assembly
            @test_throws ErrorException run(Collide; ctx = ctx, name = "collide", block = false)
            # remapping one side resolves the clobber
            @test run(Shared; ctx = ctx, name = "shared", block = false) isa ROSNode.ComponentNode
            # both remapped ⇒ assembles, and the resolved map the materialiser uses carries them
            tc = run(TwoCam; ctx = ctx, name = "twocam", block = false)
            @test tc.wires[:front][:image] == "front/image"
            @test tc.wires[:rear][:image]  == "rear/image"
        end
    end

    @testset "one /parameter_events per atomic multi-member set (§4.4)" begin
        _cctx() do ctx
            rig = run(Rig; ctx = ctx, name = "evrig", block = false)
            events = Channel{Any}(16)
            watcher = Node(ctx, "evwatch")
            Subscription(watcher, "/parameter_events", _RMSG.ParameterEvent) do ev
                ev.node == rig.node.fqn && put!(events, ev)
            end
            sleep(0.6)                                          # let the sub match
            ok, _ = ROSNode.set_parameters_atomically(rig.node.parameters,
                        [("camera.fps", 42), ("lidar.fps", 7)])
            @test ok
            sleep(0.6)                                          # let the event arrive
            evs = Any[]
            while isready(events); push!(evs, take!(events)); end
            @test length(evs) == 1                              # one combined event, not two
            @test sort([p.name for p in evs[1].changed_parameters]) == ["camera.fps", "lidar.fps"]
        end
    end
end
