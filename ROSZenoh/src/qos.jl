# Request-vs-Offered (RxO) QoS compatibility — the ROS2/DDS rule that a
# subscription *requests* a profile and a publisher *offers* one, and the offered
# must be **at least as strong as** the requested per policy. Formatter-agnostic:
# this is the ROS2-level rule, independent of how the profile is wire-encoded.
#
# ROSNode consumes this for `on_qos_incompatible` (and the throttled warnings).
# See ROSNode/DESIGN.md → "QoS-incompatibility detection".

"""
    QosIncompatibility(policy, requested, offered)

One RxO violation: `policy` is `:reliability`, `:durability`, `:deadline`,
`:liveliness`, or `:liveliness_lease`; `requested`/`offered` are the two sides'
values for that policy (for ROSNode to format the offered-vs-requested message).
"""
struct QosIncompatibility
    policy::Symbol
    requested::Any
    offered::Any
end

# Strength order (weak → strong). Offered must rank ≥ requested.
const _RELIABILITY_RANK = Dict(:best_effort => 0, :reliable => 1)
const _DURABILITY_RANK  = Dict(:volatile => 0, :transient_local => 1)
const _LIVELINESS_RANK  = Dict(:automatic => 0, :manual_by_topic => 1)

# Duration as a period in nanoseconds; `nothing` (∞/unset) is the largest period.
# For deadline and liveliness-lease, a *smaller* period is stronger, so the
# offered period must be ≤ the requested one.
_period_nanos(::Nothing) = typemax(UInt128)
_period_nanos(d::Duration) = UInt128(d.sec) * 1_000_000_000 + UInt128(d.nsec)

"""
    qos_compatible(requested::QosProfile, offered::QosProfile) -> Vector{QosIncompatibility}

Check a *requested* profile (the subscription side) against an *offered* one (the
publisher side) under the DDS RxO rule. Returns the list of violations — empty
means compatible. Direction matters: for our subscription ↔ remote publisher,
`requested` is ours; for our publisher ↔ remote subscription, `offered` is ours.

Covers reliability, durability, deadline, and liveliness (kind + lease). History
and depth are local resource policies, not part of RxO, so they're ignored.
"""
function qos_compatible(requested::QosProfile, offered::QosProfile)
    issues = QosIncompatibility[]

    if _RELIABILITY_RANK[requested.reliability] > _RELIABILITY_RANK[offered.reliability]
        push!(issues, QosIncompatibility(:reliability, requested.reliability, offered.reliability))
    end
    if _DURABILITY_RANK[requested.durability] > _DURABILITY_RANK[offered.durability]
        push!(issues, QosIncompatibility(:durability, requested.durability, offered.durability))
    end
    # Offered deadline must be ≤ requested (offered period no longer than requested).
    if _period_nanos(offered.deadline) > _period_nanos(requested.deadline)
        push!(issues, QosIncompatibility(:deadline, requested.deadline, offered.deadline))
    end
    if _LIVELINESS_RANK[requested.liveliness] > _LIVELINESS_RANK[offered.liveliness]
        push!(issues, QosIncompatibility(:liveliness, requested.liveliness, offered.liveliness))
    end
    if _period_nanos(offered.liveliness_lease) > _period_nanos(requested.liveliness_lease)
        push!(issues, QosIncompatibility(:liveliness_lease,
                                         requested.liveliness_lease, offered.liveliness_lease))
    end

    return issues
end
