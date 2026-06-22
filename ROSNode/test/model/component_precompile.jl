# Precompilation regression for the functor node layer (PLAN-NODE-PLAN.md; functor.jl).
#
# A functor `member_schema`/`node(…)` declared in a *precompiled package* must survive to runtime.
# `node(…; name=…)` skips its kind-registry mutation while `jl_generating_output` is set (a top-level
# mutation would be discarded with ROSNode's deserialized state before the package cache is written),
# so a precompiled package re-registers its `const` schemas at load from its own `__init__`. The
# member-port descriptors ride the schema's TYPE/value, and `precompile_node(schema)` bakes the
# first-`run` residue (carrier builders, lifecycle fan-out, spawned reaction/serve bodies, wire
# codecs) into the package image.
#
# This builds a throwaway environment that reuses the active resolved Manifest (so the fixture
# precompiles against THIS ROSNode without re-resolving the workspace's local deps), precompiles the
# fixture, then loads it in a fresh subprocess that asserts every declaration resolved. The
# subprocess's nonzero exit on any failed check is the signal.
using Test

# Assertions run in the fresh subprocess (written to the temp env below, not a tracked file): loading
# PrecompMixinFixture there deserializes its precompile image — its module body does NOT re-run — so
# these prove the `member_schema`/`node(…)`/authored declarations + the `precompile_node` bake
# survived precompilation. A top-level `@testset` throws on any failure, so a failed check exits the
# process nonzero — the signal the parent reads.
const _PRECOMP_CHECK = raw"""
using PrecompMixinFixture
using ROSNode
using Test
import ROSNode: member_schema

const F = PrecompMixinFixture
const B = PrecompMixinFixture.Byo
const RNX = ROSNode

# A MethodInstance for `f` over `ts` exists AND carries a compiled cache (rode the pkgimage).
cached(f, ts) = any(mi -> mi isa Core.MethodInstance && mi.specTypes <: Base.signature_type(f, ts) &&
                          isdefined(mi, :cache),
                    Iterators.flatten(Base.specializations(m) for m in methods(f, ts)))

@testset "precompiled functor declarations survive to runtime" begin
    sc = member_schema(F.Counter)
    @test sc isa RNX.MemberSchema
    @test fieldnames(RNX.paramtype(typeof(sc))) == (:fps, :label)         # the @parameters schema rode the image
    @test Set((RNX._dname(d), RNX._desc_kind(d)) for d in sc.ports) ==
          Set([(:tick, :timer), (:ingest, :subscription), (:out, :publisher), (:q, :service)])

    ingest = only(d for d in sc.ports if RNX._dname(d) == :ingest)
    @test typeof(ingest).parameters[2] === F.Ping                         # hears carries the authored type

    # `member_schema` is the functor analog of `ismixin` — a member state type has one, a non-member doesn't.
    @test member_schema(F.Other) isa RNX.MemberSchema
    @test !hasmethod(member_schema, Tuple{Type{Int}})

    # Node kinds are SCHEMA VALUES registered by `__init__` at load (the deferred-registration contract).
    @test RNX.node_kind("Rig")     === F.Rig
    @test RNX.node_kind("Counter") === F.CounterKind
    @test RNX.node_kind("Other")   === F.OtherKind

    @testset "precompile_node bake survives to runtime" begin
        # The bake produces no module-local marker `const`s (the functor schema IS the value carried
        # by the image); its proof is the baked MethodInstances surviving a load that does NOT re-run
        # the module body. `precompile_node(Rig)` baked `run`, the @generated carrier builders, the
        # lifecycle fan-out (on the derived concrete ComponentNode), and the spawned reaction/serve
        # bodies — a ROSNode-global would carry none of this here.
        NS = typeof(F.Rig)
        @test cached(Base.run, (NS,))
        @test cached(RNX.build_members, (NS, RNX.Node))
        @test cached(RNX.node_ports_carrier, (NS,))

        # The lifecycle fan-out is baked on the DERIVED concrete ComponentNode (the part `run`'s
        # Union-typed cnode local hides). Recover that CN the same way `precompile_schema` does.
        MembersT = only(Base.return_types(RNX.build_members, (NS, RNX.Node)))
        @test isconcretetype(MembersT)
        PortsT  = only(Base.return_types(RNX.node_ports_carrier, (NS,)))
        PservsT = only(Base.return_types(RNX.build_pservers, (NS, RNX.Node, NamedTuple{(), Tuple{}})))
        CN = RNX.ComponentNode{MembersT, PortsT, PservsT}
        @test cached(RNX.functor_members_configure!, (CN, NS))
        @test cached(RNX.functor_members_activate!,  (CN,))

        # The reaction/serve/codec leaves the spawned-task walker anchors. The Counter member carries
        # a timer (tick), a subscription (ingest::Ping), a publisher (out::Ping), and an @service (q):
        # the sub receive→dispatch chain + the service request/response codecs rode the image.
        @test cached(RNX.decode_owned, (Memory{UInt8}, Type{F.Ping}))     # hears Ping decode
        @test cached(RNX.encode, (F.Ping,))                               # publishes Ping encode

        # The accessor-gated `Mover` @action: its goal/result/feedback codecs ride the image too
        # (baked explicitly in the fixture — the functor walker defers Act anchoring).
        moverp   = only(d for d in member_schema(F.Mover).ports if d isa RNX.Act)
        msupport = RNX.ActionTypeSupport(typeof(moverp).parameters[2])
        @test cached(RNX.decode_owned, (Memory{UInt8}, Type{RNX.goal_type(msupport)}))
        @test cached(RNX.encode, (RNX.result_type(msupport),))
        @test cached(RNX.encode, (RNX.feedback_type(msupport),))
    end

    @testset "BYO __init__ keeps the schema registered" begin
        sb = member_schema(B.Solo)
        @test sb isa RNX.MemberSchema
        @test fieldnames(RNX.paramtype(typeof(sb))) == (:gain,)
        @test Set((RNX._dname(d), RNX._desc_kind(d)) for d in sb.ports) == Set([(:beat, :timer)])
        @test RNX.node_kind("Solo") === B.SoloKind
        # Bake survives under a BYO __init__ module too (separate module roster).
        @test cached(Base.run, (typeof(B.SoloKind),))
        @test cached(RNX.build_members, (typeof(B.SoloKind), RNX.Node))
    end

    @testset "deferred-registration helpers survive precompilation" begin
        A = PrecompMixinFixture.AutoByo
        # `@register_nodes RegMacroKind` rostered onto a module that already had a manual `__init__`;
        # that `__init__`'s `ros_init!` drained the roster at load.
        @test RNX.node_kind("RegMacro") === F.RegMacroKind
        # `register_node_kinds!(RegEagerKind)` called eagerly from the manual `__init__`.
        @test RNX.node_kind("RegEager") === F.RegEagerKind
        # `@register_nodes AutoKind` in a submodule with NO manual `__init__` — the macro installed
        # `__init__() = ros_init!(@__MODULE__)`, which Julia calls at load, so the kind resolves with
        # no hand-written hook (the auto-install branch).
        @test RNX.node_kind("Auto") === A.AutoKind
        # Each is loadable by name through the container resolve path.
        @test RNX.resolve_node_kind("precomp_fix", "precomp_fix::Auto") === A.AutoKind
        # Bakes rode the image for the macro/auto paths too (load did NOT re-run the bodies).
        @test cached(Base.run, (typeof(F.RegMacroKind),))
        @test cached(Base.run, (typeof(A.AutoKind),))
    end
end
"""

@testset "component precompilation survival" begin
    fixturedir = abspath(joinpath(@__DIR__, "..", "fixtures", "PrecompMixinFixture"))
    @test isfile(joinpath(fixturedir, "src", "PrecompMixinFixture.jl"))

    # The fixture's own UUID and its ROSNode dep UUID, read from its Project.toml by regex (no TOML
    # stdlib dep — it isn't on the test sandbox's path).
    fixtext  = read(joinpath(fixturedir, "Project.toml"), String)
    fix_uuid = match(r"(?m)^uuid *= *\"([^\"]+)\"", fixtext).captures[1]
    ros_uuid = match(r"(?m)^ROSNode *= *\"([^\"]+)\"", fixtext).captures[1]

    # The Manifest Julia actually resolves for the active project (the workspace root in this repo; a
    # sandbox manifest under `Pkg.test`). Its dir is the base for the relative `path =` entries of
    # workspace-member packages.
    manifest = Base.project_file_manifest_path(Base.active_project())
    @test manifest !== nothing && isfile(manifest)
    mroot = dirname(manifest)

    envdir = mktempdir()
    try
        # Reuse the resolved manifest, absolutizing relative member paths so they still point at the
        # real package sources from the temp env's location.
        man = read(manifest, String)
        man = replace(man, r"(?m)^path = \"(?!/)([^\"]+)\"$" =>
            s -> "path = \"" * joinpath(mroot, match(r"\"(.+)\"", s).captures[1]) * "\"")
        man *= """

        [[deps.PrecompMixinFixture]]
        deps = ["ROSNode"]
        path = "$fixturedir"
        uuid = "$fix_uuid"
        version = "0.0.1"
        """
        write(joinpath(envdir, "Manifest.toml"), man)
        write(joinpath(envdir, "Project.toml"),
              "[deps]\nPrecompMixinFixture = \"$fix_uuid\"\nROSNode = \"$ros_uuid\"\n")
        checkfile = joinpath(envdir, "precomp_check.jl")
        write(checkfile, _PRECOMP_CHECK)

        cmd = `$(Base.julia_cmd()) --project=$envdir --startup-file=no --color=no $checkfile`
        out = IOBuffer()
        # `Base.`-qualified: ROSNode exports `run` (node runner) and `success` (a settlement outcome),
        # which shadow the process builtins under `using ROSNode`.
        ok = Base.success(Base.run(pipeline(ignorestatus(cmd); stdout = out, stderr = out)))
        ok || @info "precompile-fixture subprocess output" log = String(take!(out))
        @test ok
    finally
        rm(envdir; recursive = true, force = true)
    end
end
