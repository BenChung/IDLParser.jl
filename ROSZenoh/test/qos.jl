@testset "QoS RxO compatibility" begin
    policies(issues) = sort([i.policy for i in issues])

    @testset "identical profiles are compatible" begin
        @test isempty(qos_compatible(default_qos(), default_qos()))
    end

    @testset "reliability — sub reliable, pub best_effort is incompatible" begin
        requested = QosProfile(reliability=:reliable)
        offered   = QosProfile(reliability=:best_effort)
        issues = qos_compatible(requested, offered)
        @test policies(issues) == [:reliability]
        @test issues[1].requested == :reliable
        @test issues[1].offered == :best_effort
        # Reverse direction (offered stronger than requested) is fine.
        @test isempty(qos_compatible(offered, requested))
    end

    @testset "durability — sub transient_local, pub volatile is incompatible" begin
        issues = qos_compatible(QosProfile(durability=:transient_local),
                                QosProfile(durability=:volatile))
        @test policies(issues) == [:durability]
        @test isempty(qos_compatible(QosProfile(durability=:volatile),
                                     QosProfile(durability=:transient_local)))
    end

    @testset "deadline — offered must be ≤ requested" begin
        # Requested finite, offered infinite (nothing) → offered too long.
        @test policies(qos_compatible(QosProfile(deadline=Duration(1, 0)),
                                      QosProfile(deadline=nothing))) == [:deadline]
        # Offered shorter than requested → compatible.
        @test isempty(qos_compatible(QosProfile(deadline=Duration(2, 0)),
                                     QosProfile(deadline=Duration(1, 0))))
        # Offered longer than requested → incompatible.
        @test policies(qos_compatible(QosProfile(deadline=Duration(1, 0)),
                                      QosProfile(deadline=Duration(2, 0)))) == [:deadline]
        # Requested infinite accepts any offered.
        @test isempty(qos_compatible(QosProfile(deadline=nothing),
                                     QosProfile(deadline=Duration(5, 0))))
    end

    @testset "liveliness — kind ordering and lease" begin
        # Requested manual_by_topic, offered automatic → kind too weak.
        @test policies(qos_compatible(QosProfile(liveliness=:manual_by_topic),
                                      QosProfile(liveliness=:automatic))) == [:liveliness]
        # Offered lease longer than requested → incompatible.
        @test policies(qos_compatible(QosProfile(liveliness_lease=Duration(1, 0)),
                                      QosProfile(liveliness_lease=Duration(2, 0)))) ==
              [:liveliness_lease]
        @test isempty(qos_compatible(QosProfile(liveliness_lease=Duration(2, 0)),
                                     QosProfile(liveliness_lease=Duration(1, 0))))
    end

    @testset "multiple violations are all reported" begin
        requested = QosProfile(reliability=:reliable, durability=:transient_local,
                               deadline=Duration(1, 0))
        offered   = QosProfile(reliability=:best_effort, durability=:volatile,
                               deadline=nothing)
        @test policies(qos_compatible(requested, offered)) ==
              [:deadline, :durability, :reliability]
    end
end
