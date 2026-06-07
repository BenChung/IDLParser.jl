# Authored Julia types → ROS (@ros_package / @ros_message). Registration/placement/RIHS
# are checked with no session; the round-trip publishes an authored message and receives
# it back over the per-run private router (`ROS_TEST_EP`, from runtests.jl). Standalone:
#   ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/typesupport/authored.jl
# Always under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: wait_for_service, wait_for_action_server
using Test
import ROSMessages: il_from_type, lower, type_description_from_struct, TypeDescription,
                    TypeDescriptionMsg, rihs01_hash

const _AUTH_EP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
                 get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")

# Authored interfaces — registered at include (script eval ⇒ the eval-flush drains).
module AuthoredMsgs
    using ROSNode
    @ros_package "authored_test"
    @ros_message struct Vec3;    x::Float64; y::Float64; z::Float64; end
    @ros_message struct Stamped; v::Vec3; label::String; n::Int32;   end   # nested authored ref
end

# Annotate form: an existing top-level struct marked authored.
module AuthoredAnnotate
    using ROSNode
    struct Twist; linear::Float64; angular::Float64; end
    @ros_package "annot_test"
    @ros_message Twist
end

# Authored service: args = request fields, @NamedTuple return = response fields.
module AuthoredSrv
    using ROSNode
    @ros_package "authored_test"
    @ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
        (sum = a + b,)
    end
end

# Authored action: goal args + a FeedbackSink, @NamedTuple return = result fields.
module AuthoredAct
    using ROSNode
    @ros_package "authored_test"
    @ros_action function Counter(target::Int32,
            fb::FeedbackSink{@NamedTuple{at::Int32}})::@NamedTuple{total::Int32}
        n = Int32(0)
        while n < target
            n += Int32(1)
            fb((at = n,))            # publish feedback + cancellation checkpoint
            sleep(0.02)
        end
        (total = n,)
    end
end

# Explicit-name form (#2): cross-package, and Julia name ≠ ROS name.
module AuthoredExplicit
    using ROSNode
    @ros_package "pkg_a"
    struct MyPoint; x::Float64; y::Float64; end
    @ros_message "geometry_msgs/msg/Point2" MyPoint                 # rename: MyPoint ↔ Point2
    @ros_message "other_pkg/msg/Foo" struct Foo; bar::Int32; end    # cross-package define
end

_struct_ast(il; package="") = ROSNode._scan_for_struct(lower(il; package=package))

# Local name (the suite `include`s many files into one scope — avoid clobbering a sibling's helper).
function _authored_recv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint=0.05) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

@testset "authored types — @ros_package / @ros_message" begin
    AM = AuthoredMsgs

    @testset "define form — placement, leaf-bind, RIHS (no session)" begin
        @test isdefined(AM, :authored_test) && isdefined(AM.authored_test, :msg)
        @test AM.Vec3 === AM.authored_test.msg.Vec3
        @test AM.Stamped === AM.authored_test.msg.Stamped
        @test fieldtype(AM.authored_test.msg.Stamped, :v) === AM.authored_test.msg.Vec3   # nested → same type

        tiv = ROSNode.type_info_of(AM.Vec3)
        @test tiv.name == "authored_test/msg/Vec3"
        exp_v = rihs01_hash(_struct_ast(il_from_type(AM.Vec3)), "authored_test/msg/Vec3"; package="authored_test")
        @test ROSNode.to_rihs_string(tiv.hash) == exp_v

        tis = ROSNode.type_info_of(AM.Stamped)
        @test tis.name == "authored_test/msg/Stamped"
        v_td  = type_description_from_struct(_struct_ast(il_from_type(AM.Vec3)),
                    "authored_test/msg/Vec3"; package="authored_test")
        s_ast = _struct_ast(il_from_type(AM.Stamped; name_of = _ -> "authored_test/msg/Vec3"); package="authored_test")
        exp_s = rihs01_hash(s_ast, "authored_test/msg/Stamped"; package="authored_test",
                            references=TypeDescription[v_td])
        @test ROSNode.to_rihs_string(tis.hash) == exp_s
    end

    @testset "annotate form" begin
        AA = AuthoredAnnotate
        @test isdefined(AA.annot_test.msg, :Twist) && AA.annot_test.msg.Twist === AA.Twist
        @test ROSNode.type_info_of(AA.Twist).name == "annot_test/msg/Twist"
    end

    @testset "explicit-name form (#2): cross-package + rename" begin
        AE = AuthoredExplicit
        # rename: the Julia struct keeps its name; the ROS identity is the explicit one.
        @test ROSNode.type_info_of(AE.MyPoint).name == "geometry_msgs/msg/Point2"
        @test AE.geometry_msgs.msg.Point2 === AE.MyPoint
        # cross-package define (not pkg_a from @ros_package).
        @test ROSNode.type_info_of(AE.other_pkg.msg.Foo).name == "other_pkg/msg/Foo"
        @test AE.Foo === AE.other_pkg.msg.Foo
    end

    @testset "action wrapper RIHS (#4: action-qualified nested ref)" begin
        act  = AuthoredAct.authored_test.action
        fmsg = act.Counter_FeedbackMessage
        # The five rosidl wrappers register with a real RIHS — `lower` carries the qualifier, so
        # the wrapper's nested section ref reflects to `…/action/Counter_Feedback` (a hardcoded
        # `msg` would mismatch a ROS2 peer's feedback-topic hash).
        @test ROSNode._entry_of(fmsg) !== nothing && ROSNode._entry_of(fmsg).td !== nothing
        @test ROSNode.type_info_of(fmsg).name == "authored_test/action/Counter_FeedbackMessage"
        # Independently recompute (UUID + Feedback closure) with explicitly action-qualified names:
        # confirms the registered hash carries the qualifier, not just that an entry exists.
        uuidT = ROSNode.Interfaces.unique_identifier_msgs.msg.UUID
        nm(S) = S === uuidT ? "unique_identifier_msgs/msg/UUID" :
                S === act.Counter_Feedback ? "authored_test/action/Counter_Feedback" :
                error("unexpected nested $S")
        astof(S) = ROSNode._scan_for_struct(lower(il_from_type(S; name_of=nm); package="x"))
        uuid_td = type_description_from_struct(astof(uuidT), "unique_identifier_msgs/msg/UUID"; package="unique_identifier_msgs")
        fb_td   = type_description_from_struct(astof(act.Counter_Feedback), "authored_test/action/Counter_Feedback"; package="authored_test")
        refs    = sort!(TypeDescription[uuid_td, fb_td]; by = t -> t.type_name)
        exp_fm  = rihs01_hash(astof(fmsg), "authored_test/action/Counter_FeedbackMessage";
                              package="authored_test", references=refs)
        @test ROSNode.to_rihs_string(ROSNode.type_info_of(fmsg).hash) == exp_fm
    end

    @testset "live pub/sub round-trip (authored type, real Zenoh)" begin
        Context(; peers=[_AUTH_EP], localhost_only=true) do ctx
            node = Node(ctx, "authored_node")
            got  = Channel{Any}(8)
            sub  = Subscription(node, "/authored/stamped", AM.Stamped) do msg; put!(got, msg); end
            pub  = Publisher(node, "/authored/stamped", AM.Stamped)
            sleep(0.4)
            publish(pub, AM.Stamped(AM.Vec3(1.0, 2.0, 3.0), "hi", Int32(7)))
            msg = _authored_recv(got, 5.0)
            @test msg !== nothing
            @test msg isa AM.Stamped
            @test msg.label == "hi"
            @test msg.n == Int32(7)
            @test (msg.v.x, msg.v.y, msg.v.z) == (1.0, 2.0, 3.0)   # nested struct round-trips
        end
    end

    @testset "live service round-trip (authored, real Zenoh)" begin
        Context(; peers=[_AUTH_EP], localhost_only=true) do ctx
            node   = Node(ctx, "authored_srv")
            srv    = Service(node, "/add_two_ints", AuthoredSrv.AddTwoInts)   # function = marker + handler
            client = ServiceClient(node, "/add_two_ints", AuthoredSrv.AddTwoInts)
            @test wait_for_service(client; timeout = 5)
            resp = call(client; a = Int64(2), b = Int64(40))                  # kwargs in, @NamedTuple out
            @test resp == (sum = Int64(42),)
            close(client); close(srv)
        end
    end

    @testset "live action round-trip (authored, real Zenoh)" begin
        Context(; peers=[_AUTH_EP], localhost_only=true) do ctx
            node   = Node(ctx, "authored_act")
            srv    = ActionServer(node, "/counter", AuthoredAct.Counter)   # function = marker + handler
            client = ActionClient(node, "/counter", AuthoredAct.Counter)
            @test wait_for_action_server(client; timeout = 5)
            gh  = send(client; target = Int32(3))                          # kwargs → Goal
            fbs = Int32[]
            for fb in feedback(gh); push!(fbs, fb.at); end                 # streamed feedback (#1: NamedTuple)
            r = fetch(gh)
            @test r isa NamedTuple && r.total == Int32(3)                  # #1: result is a @NamedTuple
            @test !isempty(fbs) && issorted(fbs) && all(x -> 1 <= x <= 3, fbs)
            close(client); close(srv)
        end
    end

    @testset "authored action cancellation (#3)" begin
        Context(; peers=[_AUTH_EP], localhost_only=true) do ctx
            snode  = Node(ctx, "authored_cancel_srv")
            cnode  = Node(ctx, "authored_cancel_cli")     # separate nodes (blocking get + consumer)
            srv    = ActionServer(snode, "/counter_c", AuthoredAct.Counter)
            client = ActionClient(cnode, "/counter_c", AuthoredAct.Counter)
            @test wait_for_action_server(client; timeout = 5)
            gh = send(client; target = Int32(1_000_000))                  # long-running goal
            @test timedwait(3.0; pollint=0.02) do
                ROSNode.state(gh) in (:accepted, :executing)
            end === :ok
            ROSNode.cancel(gh)                                            # fb(...) checkpoint throws Cancelled
            t = Threads.@spawn try; fetch(gh); catch; end                # drive get_result (must not hang)
            @test timedwait(8.0; pollint=0.05) do; istaskdone(t); end === :ok
            @test ROSNode.state(gh) === :canceled                        # terminal CANCELED, not CANCELING
            close(client); close(srv)
        end
    end
end
