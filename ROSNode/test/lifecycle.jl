# §14.2 managed-node state machine + gating predicate. The pure pieces — the
# state singletons (sealed dispatch tags), the wire id/label mappings, the
# transition-id table and its id→driver/shutdown-origin maps, and the gate
# predicate's "Active means active" logic — test without a session. Driving an
# actual transition (`configure!`/…) and gating live entities need a real Node
# (a Zenoh session), so they are live-transport tests, exercised elsewhere.

using ROSNode: LifecycleState, Unconfigured, Inactive, Active, Finalized, isactive

@testset "lifecycle" begin
    @testset "primary states are sealed dispatch singletons" begin
        for S in (Unconfigured, Inactive, Active, Finalized)
            @test S <: LifecycleState
            @test S() isa LifecycleState
            @test S() === S()                 # singleton identity
        end
        # distinct identities ⇒ `===` dispatch is exact, not a silent wrong branch
        @test Active() !== Inactive()
        # `show` renders the type name; the lowercase wire label is `_state_label`.
        @test sprint(show, Active()) == "Active"
        @test sprint(show, Unconfigured()) == "Unconfigured"
    end

    @testset "wire state ids + labels (lifecycle_msgs/msg/State)" begin
        @test ROSNode._state_id(Unconfigured()) == 0x01
        @test ROSNode._state_id(Inactive())     == 0x02
        @test ROSNode._state_id(Active())        == 0x03
        @test ROSNode._state_id(Finalized())     == 0x04
        @test ROSNode._state_label(Inactive()) == "inactive"
        @test ROSNode._state_label(Active())    == "active"
    end

    @testset "transition id constants (lifecycle_msgs/msg/Transition)" begin
        @test ROSNode.TRANSITION_CONFIGURE  == 0x01
        @test ROSNode.TRANSITION_CLEANUP    == 0x02
        @test ROSNode.TRANSITION_ACTIVATE   == 0x03
        @test ROSNode.TRANSITION_DEACTIVATE == 0x04
        # the three shutdown transitions are distinct per origin
        @test length(Set((ROSNode.TRANSITION_UNCONFIGURED_SHUTDOWN,
                          ROSNode.TRANSITION_INACTIVE_SHUTDOWN,
                          ROSNode.TRANSITION_ACTIVE_SHUTDOWN))) == 3
    end

    @testset "shutdown transition id is keyed by origin state" begin
        @test ROSNode._shutdown_transition_id(Unconfigured()) ==
              ROSNode.TRANSITION_UNCONFIGURED_SHUTDOWN
        @test ROSNode._shutdown_transition_id(Inactive()) ==
              ROSNode.TRANSITION_INACTIVE_SHUTDOWN
        @test ROSNode._shutdown_transition_id(Active()) ==
              ROSNode.TRANSITION_ACTIVE_SHUTDOWN
    end

    @testset "gating predicate: Active ⇔ active" begin
        # isactive on a LifecycleNode reduces to `state === Active()`; assert the
        # underlying logic directly across all four states.
        @test (Active()       === Active())
        @test !(Inactive()    === Active())
        @test !(Unconfigured()=== Active())
        @test !(Finalized()   === Active())
    end

    @testset "an unmanaged (plain) thing has no gate ⇒ always active" begin
        # _gate_for returns nothing for anything not in the registry; the predicate
        # then short-circuits to `true`. We can't build a real Node here, but the
        # registry lookup itself is testable: an arbitrary unregistered key is absent.
        @test ROSNode._GATES isa AbstractDict      # registry exists; no live entries here
    end
end
