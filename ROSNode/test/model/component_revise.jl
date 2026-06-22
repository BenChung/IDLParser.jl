# Revise-safety of the functor component API — the SCHEMA-LEVEL contract.
#
# Editing a component's SCHEMA — its ports, params, reactions, wiring, or an inline @service/@action
# handler — is Revise-safe: re-evaluating the edited top-level expression yields a fresh, correct,
# duplicate-free result. This is STRUCTURAL, not bookkeeping:
#   • the schema is an immutable VALUE — `member_schema(::Type{S}) = component(S, …)` rebuilds a fresh
#     MemberSchema on every read, with no mutable per-mixin spec store to accumulate (the @mixin-era
#     port/param doubling hazard simply cannot occur);
#   • the node-kind registry is a Dict keyed by name — `node(…; name=)`/`register_node_kind!` REPLACE,
#     never append (no roster growth);
#   • authored @service/@action types intern idempotently — `absorb_static_types!` dedupes by type and
#     an identical struct re-eval reuses the type (Julia ≥1.12), so `request_type`/`response_type` are
#     stable across a handler edit;
#   • reaction handlers re-key on the UNCHANGED state type, so a body edit is picked up on re-derive.
#
# OUT OF SCOPE, by design: editing the component's STATE STRUCT itself. Struct redefinition mints a new
# type, orphaning `member_schema` + the reaction/hook methods (all keyed on the type) until re-run — a
# limitation Julia's recent struct-redefinition support shares ecosystem-wide. Re-run the whole
# component (or restart). The deferred `@component` macro (DESIGN-COMPONENT-MACRO.md) would re-couple
# them by bundling struct + schema into one expression.
#
# Method: replay each edit exactly as Revise would — `Core.eval` of the top-level expression.

using ROSNode
using ROSNode: member_schema, paramtype, _dname, node_kind, node_kinds, request_type, response_type
using Test

module _ReviseFunctor
    using ROSNode
    import ROSNode: member_schema
    @ros_package "revise_test"
    const _T = ROSNode.Interfaces.builtin_interfaces.msg.Time

    @parameters struct RevWorkerParams; rate::Int64 = 10; end
    mutable struct RevWorker{Name} <: Component{Name}; n::Int; end
    RevWorker{Name}() where {Name} = RevWorker{Name}(0)
    ingest(node, m::RevWorker, msg::_T) = nothing
    beat(node, m::RevWorker) = nothing
    member_schema(::Type{RevWorker}) = component(RevWorker, RevWorkerParams,
        publishes(:out, _T), hears(:ingest, _T, ingest), every(:beat, :rate, beat))

    mutable struct RevHelper{Name} <: Component{Name} end
    member_schema(::Type{RevHelper}) = component(RevHelper, publishes(:aux, _T))

    mutable struct RevSvc{Name} <: Component{Name} end
    @service "~/q" function q(node, m::RevSvc, x::Int64)::@NamedTuple{y::Int64}; (y = x + 1,); end
    member_schema(::Type{RevSvc}) = component(RevSvc, q)

    const RevRig = node("w" => RevWorker, "h" => RevHelper; name = "RevRig")
end
using ._ReviseFunctor: RevWorker, RevHelper, RevSvc

_revports(S)  = Symbol[_dname(d) for d in member_schema(S).ports]
_revparams(S) = collect(fieldnames(paramtype(typeof(member_schema(S)))))

@testset "functor schema-level Revise contract" begin
    M = _ReviseFunctor
    @test _revports(RevWorker) == [:out, :ingest, :beat]
    @test _revparams(RevWorker) == [:rate]
    @test node_kind("RevRig") === M.RevRig
    n0 = count(==("RevRig"), node_kinds()); @test n0 == 1

    @testset "re-eval member_schema + reaction bodies is idempotent (no doubling)" begin
        Core.eval(M, :(ingest(node, m::RevWorker, msg::_T) = (m.n += 1; nothing)))   # edited handler body
        Core.eval(M, :(beat(node, m::RevWorker) = nothing))
        Core.eval(M, :(member_schema(::Type{RevWorker}) = component(RevWorker, RevWorkerParams,
            publishes(:out, _T), hears(:ingest, _T, ingest), every(:beat, :rate, beat))))
        @test _revports(RevWorker) == [:out, :ingest, :beat]      # fresh immutable schema — nothing accumulates
        @test _revparams(RevWorker) == [:rate]
    end

    @testset "re-eval member_schema picks up a SCHEMA edit, then reverts cleanly" begin
        Core.eval(M, :(member_schema(::Type{RevHelper}) =
            component(RevHelper, publishes(:aux, _T), publishes(:aux2, _T))))   # added a publisher
        @test _revports(RevHelper) == [:aux, :aux2]               # edit visible on re-derive
        Core.eval(M, :(member_schema(::Type{RevHelper}) = component(RevHelper, publishes(:aux, _T))))
        @test _revports(RevHelper) == [:aux]                      # reverted — no leftover accumulation
    end

    @testset "re-eval an inline @service handler is idempotent (authored types stable)" begin
        rq, rs = request_type(typeof(M.q)), response_type(typeof(M.q))
        Core.eval(M, :(@service "~/q" function q(node, m::RevSvc, x::Int64)::@NamedTuple{y::Int64}
            (y = x + 2,)                                          # edited body
        end))
        @test request_type(typeof(M.q)) === rq                    # identical struct re-eval reuses the type
        @test response_type(typeof(M.q)) === rs                   #   (no re-mint ⇒ no registry doubling)
        @test member_schema(RevSvc).ports[1] isa ROSNode.Srv{:q}  # still derives a valid service port
    end

    @testset "re-eval node(…; name=) re-registers without duplicating" begin
        schema2 = node("w" => RevWorker, "h" => RevHelper; name = "RevRig")   # auto-registers, replace-by-name
        @test count(==("RevRig"), node_kinds()) == n0             # Dict registry: no roster growth
        @test node_kind("RevRig") === schema2                     # replaced, not appended
    end
end
