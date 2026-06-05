# D10B — deterministic per-module type resolution + intra-process cross-materialization.
# Pure-logic (no Zenoh session): the resolution-table assembly rule, the per-module home
# lookup, and the cross-alias `as`/`_ipc_*` materialization. The multi-package
# diamond/fork *baking* (real precompiled packages) is a separate integration fixture; the
# assembly rule itself is exercised here directly against `_fold_resolve!`.

using ROSNode
using Test

const RE = ROSNode.ResolveEntry

# Force two genuinely-distinct, same-RIHS Julia structs in one process by clearing the
# aliasing pool between imports (so the second mints its own instead of aliasing the
# first) — the in-process stand-in for two independently-precompiled packages. The
# `from=` root is a relative *literal* (resolved against this file → ROSNode/examples).
module _DA; using ROSNode; @ros_import from="../examples/interfaces" "sensor_demo/msg/Reading"; end
empty!(ROSNode._STATIC_TYPES.entries); empty!(ROSNode._STATIC_TYPES.seen)
module _DB; using ROSNode; @ros_import from="../examples/interfaces" "sensor_demo/msg/Reading"; end

@testset "D10B resolution + cross-materialize" begin
    @testset "fold/tie assembly rule (S1/S5)" begin
        struct _FA end; struct _FB end
        fold! = ROSNode._fold_resolve!
        # diamond: two sources agree → no tie
        d = Dict{Symbol,RE}(); fold!(d, Dict(:x=>RE(_FA,Main,false))); fold!(d, Dict(:x=>RE(_FA,Main,false)))
        @test d[:x].type === _FA && !d[:x].tied
        # fork: disagree → deterministic pick (min FQ string) + tied
        pick = string(_FA) <= string(_FB) ? _FA : _FB
        d2 = Dict{Symbol,RE}(); fold!(d2, Dict(:y=>RE(_FA,Main,false))); fold!(d2, Dict(:y=>RE(_FB,Main,false)))
        @test d2[:y].type === pick && d2[:y].tied
        # order independence: same pick regardless of fold order
        d3 = Dict{Symbol,RE}(); fold!(d3, Dict(:y=>RE(_FB,Main,false))); fold!(d3, Dict(:y=>RE(_FA,Main,false)))
        @test d3[:y].type === pick
        # tied is sticky (settled lower suppresses a fresh raw arm)
        d4 = Dict(:z=>RE(_FA,Main,true)); fold!(d4, Dict(:z=>RE(_FB,Main,false)))
        @test d4[:z].type === _FA && d4[:z].tied
        # adopt a settled tie over an unsettled disagreement
        d5 = Dict(:w=>RE(_FA,Main,false)); fold!(d5, Dict(:w=>RE(_FB,Main,true)))
        @test d5[:w].type === _FB && d5[:w].tied
    end

    @testset "3-way fork: global argmin, order-independent (S1)" begin
        # Three independently-minted structs for one wire type — the closure-reduction
        # picks the globally-smallest FQN, NOT whichever pair settled first under an
        # unordered `loaded_modules` walk. (A pairwise sticky fold would let a third,
        # smaller arm be dropped once the first two marked `tied`.)
        struct _GA end; struct _GB end; struct _GC end
        reduce! = ROSNode._reduce_candidates
        winner = argmin(string, (_GA, _GB, _GC))
        # every permutation of the three forks reduces to the same global winner, tied
        for perm in ([_GA,_GB,_GC], [_GC,_GB,_GA], [_GB,_GA,_GC], [_GC,_GA,_GB])
            cs = [RE(T, Main, false) for T in perm]
            r = reduce!(:g, cs)
            @test r.type === winner && r.tied
        end
        # a third differing fork is NOT dropped just because two earlier arms settled:
        # an already-`tied` non-winner arm still loses to the global-min raw arm.
        notwin = argmax(string, (_GA, _GB, _GC))
        r2 = reduce!(:g, [RE(notwin, Main, true), RE(winner, Main, false), RE(notwin, Main, true)])
        @test r2.type === winner && r2.tied
        # full agreement carries the settled flag forward (no spurious re-pick)
        @test reduce!(:g, [RE(_GA,Main,false), RE(_GA,Main,true)]).type === _GA
        @test reduce!(:g, [RE(_GA,Main,false), RE(_GA,Main,true)]).tied
        @test !reduce!(:g, [RE(_GA,Main,false), RE(_GA,Main,false)]).tied
    end

    @testset "per-module table + home lookup (S1/S2)" begin
        RA = _DA.sensor_demo.msg.Reading
        RB = _DB.sensor_demo.msg.Reading
        @test RA !== RB                                                   # distinct Julia structs
        @test ROSNode.type_info_of(RA).hash == ROSNode.type_info_of(RB).hash   # same wire type
        rsym = Symbol(ROSNode.to_rihs_string(ROSNode.type_info_of(RA).hash))
        # each module's baked table resolves the wire type to ITS OWN struct
        @test ROSNode.resolve_in_home(_DA, rsym) === RA
        @test ROSNode.resolve_in_home(_DB, rsym) === RB
        # a module that never imported it resolves nothing (→ content-canonical fallback)
        @test ROSNode.resolve_in_home(@__MODULE__, rsym) === nothing
    end

    @testset "cross-alias materialize (S4/S4c)" begin
        RA = _DA.sensor_demo.msg.Reading
        RB = _DB.sensor_demo.msg.Reading
        ev(x) = ROSNode._encode_to_vector(x)        # byte-equality: type-agnostic correctness
        a = ROSNode._default_msg(RA)
        # `as` boundary cast: RA value → RB, identical wire bytes
        b = ROSNode.as(a, RB)
        @test b isa RB && ev(b) == ev(a)
        @test ROSNode.as(a, RA) === a               # identity short-circuit
        # intra-process cross-materialize: owned + shared both land RB with identical bytes
        @test (o = ROSNode._ipc_own(RB, a);   o isa RB && ev(o) == ev(a))
        @test (s = ROSNode._ipc_share(RB, a); s isa RB && ev(s) == ev(a))
        # same-alias fast paths unchanged
        @test ROSNode._ipc_own(RA, a) isa RA
        @test ev(ROSNode._ipc_share(RA, a)) == ev(a)
    end
end
