# Precompilation regression for the component layer (DESIGN-COMPONENTS.md §2–§7).
#
# A `@mixin`/`@node` declared in a *precompiled package* must survive to runtime. The
# spec store and loadable-kind registration once lived in ROSNode's process-global
# `_MIXINS`/`_NODE_KINDS`; a consuming package's top-level declarations mutated those
# during the package's own precompile, and that mutation was discarded with ROSNode's
# deserialized state before the package cache was written — so at runtime `mixin_spec`
# threw and `node_kind` returned nothing. The fix stores each mixin's spec in a `const`
# in the *defining* module (reached by `mixin_spec(::Type{M})` dispatch) and defers
# name registration to the module's load hook `ros_init!`.
#
# This builds a throwaway environment that reuses the active resolved Manifest (so the
# fixture precompiles against THIS ROSNode without re-resolving the workspace's local
# deps), precompiles the fixture, then loads it in a fresh subprocess that asserts every
# declaration resolved. The subprocess's nonzero exit on any failed check is the signal.
using Test

# Assertions run in the fresh subprocess (written to the temp env below, not a tracked
# file): loading PrecompMixinFixture there deserializes its precompile image — its module
# body does NOT re-run — so these prove the `@mixin`/`@param`/`@node`/authored
# declarations survived precompilation. A top-level `@testset` throws on any failure, so
# a failed check exits the process nonzero — the signal the parent reads.
const _PRECOMP_CHECK = raw"""
using PrecompMixinFixture
using ROSNode
using Test

const F = PrecompMixinFixture
const B = PrecompMixinFixture.Byo

@testset "precompiled @mixin declarations survive to runtime" begin
    sp = ROSNode.mixin_spec(F.Counter)
    @test [p.name for p in sp.params] == [:fps, :label]
    @test Set((p.name, p.kind) for p in sp.ports) == Set([(:tick, :timer), (:ingest, :subscription)])

    ingest = only(p for p in sp.ports if p.name == :ingest)
    @test ingest.msgtype === F.Ping                     # @hears carries the authored type

    @test ROSNode.ismixin(F.Counter)
    @test ROSNode.ismixin(F.Other)
    @test !ROSNode.ismixin(Int)

    @test ROSNode.node_kind("Counter") === F.Counter    # registered by ros_init! at load
    @test ROSNode.node_kind("Other")   === F.Other
    @test ROSNode.node_kind("Rig")     === F.Rig

    @test fieldnames(ROSNode._ensure_schema!(F.Counter)) == (:fps, :label)

    @testset "BYO __init__ keeps ROSNode initializing" begin
        spb = ROSNode.mixin_spec(B.Solo)
        @test [p.name for p in spb.params] == [:gain]
        @test Set((p.name, p.kind) for p in spb.ports) == Set([(:beat, :timer)])
        @test ROSNode.ismixin(B.Solo)
        @test ROSNode.node_kind("Solo") === B.Solo
        @test fieldnames(ROSNode._ensure_schema!(B.Solo)) == (:gain,)
    end
end
"""

@testset "component precompilation survival" begin
    fixturedir = abspath(joinpath(@__DIR__, "..", "fixtures", "PrecompMixinFixture"))
    @test isfile(joinpath(fixturedir, "src", "PrecompMixinFixture.jl"))

    # The fixture's own UUID and its ROSNode dep UUID, read from its Project.toml by
    # regex (no TOML stdlib dep — it isn't on the test sandbox's path).
    fixtext  = read(joinpath(fixturedir, "Project.toml"), String)
    fix_uuid = match(r"(?m)^uuid *= *\"([^\"]+)\"", fixtext).captures[1]
    ros_uuid = match(r"(?m)^ROSNode *= *\"([^\"]+)\"", fixtext).captures[1]

    # The Manifest Julia actually resolves for the active project (the workspace root in
    # this repo; a sandbox manifest under `Pkg.test`). Its dir is the base for the
    # relative `path =` entries of workspace-member packages.
    manifest = Base.project_file_manifest_path(Base.active_project())
    @test manifest !== nothing && isfile(manifest)
    mroot = dirname(manifest)

    envdir = mktempdir()
    try
        # Reuse the resolved manifest, absolutizing relative member paths so they still
        # point at the real package sources from the temp env's location.
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
        # `Base.`-qualified: ROSNode exports `run` (node runner) and `success` (a
        # settlement outcome), which shadow the process builtins under `using ROSNode`.
        ok = Base.success(Base.run(pipeline(ignorestatus(cmd); stdout = out, stderr = out)))
        ok || @info "precompile-fixture subprocess output" log = String(take!(out))
        @test ok
    finally
        rm(envdir; recursive = true, force = true)
    end
end
