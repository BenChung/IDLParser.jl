# §12.2 QoS RxO compatibility. Pure ROSZenoh rule re-exported through ROSNode:
# the *offered* (publisher) profile must rank ≥ the *requested* (subscription)
# one per policy. `qos_compatible(requested, offered)` returns the violations.

using ROSNode: qos_compatible, QosIncompatibility, QosProfile, default_qos
using ROSZenoh: Duration as ZDuration   # the unsigned QoS-policy Duration

@testset "qos RxO" begin
    @testset "identical profiles are compatible" begin
        @test isempty(qos_compatible(default_qos(), default_qos()))
    end

    @testset "reliability: best_effort offer < reliable request" begin
        requested = QosProfile(reliability=:reliable)
        offered   = QosProfile(reliability=:best_effort)
        issues = qos_compatible(requested, offered)
        @test length(issues) == 1
        @test issues[1].policy === :reliability
        @test issues[1].requested === :reliable
        @test issues[1].offered === :best_effort
        # reverse direction is fine: reliable offer satisfies best_effort request
        @test isempty(qos_compatible(offered, requested))
    end

    @testset "durability: volatile offer < transient_local request" begin
        requested = QosProfile(durability=:transient_local)
        offered   = QosProfile(durability=:volatile)
        issues = qos_compatible(requested, offered)
        @test length(issues) == 1
        @test issues[1].policy === :durability
        @test isempty(qos_compatible(offered, requested))
    end

    @testset "liveliness kind: automatic offer < manual_by_topic request" begin
        requested = QosProfile(liveliness=:manual_by_topic)
        offered   = QosProfile(liveliness=:automatic)
        issues = qos_compatible(requested, offered)
        @test length(issues) == 1
        @test issues[1].policy === :liveliness
    end

    @testset "deadline: a longer offered period than requested is incompatible" begin
        # offered deadline must be ≤ requested (offered period no longer).
        requested = QosProfile(deadline=ZDuration(1, 0))   # 1s
        offered   = QosProfile(deadline=ZDuration(2, 0))   # 2s — too slow
        issues = qos_compatible(requested, offered)
        @test length(issues) == 1
        @test issues[1].policy === :deadline
        # a faster (shorter) offered deadline satisfies the request
        @test isempty(qos_compatible(requested, QosProfile(deadline=ZDuration(1, 0))))
        # unset (∞) requested deadline accepts any offered
        @test isempty(qos_compatible(default_qos(), offered))
        # unset offered (∞) cannot satisfy a finite requested deadline
        @test !isempty(qos_compatible(requested, default_qos()))
    end

    @testset "liveliness_lease: longer offered lease than requested is incompatible" begin
        requested = QosProfile(liveliness_lease=ZDuration(1, 0))
        offered   = QosProfile(liveliness_lease=ZDuration(5, 0))
        issues = qos_compatible(requested, offered)
        @test length(issues) == 1
        @test issues[1].policy === :liveliness_lease
    end

    @testset "multiple simultaneous violations are all reported" begin
        requested = QosProfile(reliability=:reliable, durability=:transient_local)
        offered   = QosProfile(reliability=:best_effort, durability=:volatile)
        issues = qos_compatible(requested, offered)
        policies = Set(i.policy for i in issues)
        @test policies == Set((:reliability, :durability))
    end

    @testset "history/depth are local resource policies, not RxO" begin
        # differing history+depth alone must not produce a violation
        requested = QosProfile(history=:keep_last, depth=1)
        offered   = QosProfile(history=:keep_all, depth=1000)
        @test isempty(qos_compatible(requested, offered))
    end
end
