# Revise-safety of the component declaration macros. Revise re-evaluates a definition's
# whole top-level expression when its body is edited; for `@hears`/`@every`/`@serves`/
# `@runs` that expression also carries the port registration, so the registration must be
# idempotent (replace-by-name) — a plain append would double the port on every reaction
# edit and clash two publishers/subscriptions onto one wire at assembly. `@param` (params),
# `@node` (`__node_kinds__`), and `@mixin` (`__mixin_bases__` + the spec store) share the
# same hazard. Pure-logic: replay the macro expansions exactly as Revise would
# (`Core.eval` of the same expression) and assert the spec/rosters stay duplicate-free.
#
# The captured `spec`/`kinds`/`bases` objects are the live stores, mutated in place by the
# replays — reading them sidesteps the testset's frozen world age (re-evaluated `@mixin`
# reuses, never replaces, the spec object).

using ROSNode
using ROSNode: mixin_spec
using Test

module _ReviseMix
    using ROSNode
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time

    @mixin struct RevWorker
        n::Int = 0
    end
    @param     RevWorker rate::Int64 = 10
    @publishes RevWorker out :: _T
    @hears     function ingest(node, m::RevWorker, msg::_T) end
    @every     :rate function beat(node, m::RevWorker) end

    @mixin struct RevHelper end
    @publishes RevHelper aux :: _T

    @node RevRig = ["w" => RevWorker, "h" => RevHelper]
end
using ._ReviseMix: RevWorker, RevHelper

@testset "component macros are Revise-idempotent" begin
    M = _ReviseMix
    spec  = mixin_spec(RevWorker)        # same object across re-evals (the spec is reused)
    kinds = M.__node_kinds__
    bases = M.__mixin_bases__
    pn() = [p.name for p in spec.ports]
    qn() = [p.name for p in spec.params]

    @test pn() == [:out, :ingest, :beat]
    @test qn() == [:rate]
    @test [e[1] for e in kinds] == ["RevRig"]
    nbases = length(bases)

    # Revise re-evaluates the WHOLE top-level expression of an edited definition — for a
    # reaction, the method def AND the macro's port registration. Replay each.
    Core.eval(M, :(@hears function ingest(node, m::RevWorker, msg::_T) m.n += 1 end))   # "edited body"
    Core.eval(M, :(@every :rate function beat(node, m::RevWorker) end))
    Core.eval(M, :(@publishes RevWorker out :: _T))
    Core.eval(M, :(@param RevWorker rate::Int64 = 10))
    Core.eval(M, :(@node RevRig = ["w" => RevWorker, "h" => RevHelper]))

    # Idempotent: no duplicates, order + content preserved.
    @test pn() == [:out, :ingest, :beat]
    @test qn() == [:rate]
    @test [e[1] for e in kinds] == ["RevRig"]
    @test length(bases) == nbases

    # Editing a struct body re-evaluates only the `@mixin` expression — the ports/params
    # from the separate statements it does NOT re-run must survive (spec reused, not reset).
    Core.eval(M, :(@mixin struct RevWorker; n::Int = 0; extra::Float64 = 1.5; end))
    @test pn() == [:out, :ingest, :beat]
    @test qn() == [:rate]
    @test length(bases) == nbases
end
