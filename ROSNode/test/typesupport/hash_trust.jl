# D7 — type-revision trust gate (`resolve_type`'s ament re-validation).
#
# Session-free: `resolve_type` only reads the Context's `registry` + `weak_types`,
# never the wire, so a plain `Context(localhost_only=true)` (peer mode, no router)
# suffices and the test is fully deterministic. The gate compares a NAME-ONLY ament
# lookup's RIHS01 against `info.hash`: exact match binds, real-hash mismatch rejects
# (returns nothing so the wire path can fetch the peer revision), and the all-zero
# placeholder is accepted. `weak_types` only softens the mismatch diagnostic here
# (`@warn`→`@debug`); the wire-bind side of weak mode lives in introspection.jl's
# `resolve_or_discover` and is covered by the live discovery suite.
#
# Standalone (no router needed):
#   timeout -k 5 180 julia --project=. test/typesupport/hash_trust.jl

using ROSNode
using ROSNode: Context, registry, resolve_type, load_ament_type, lookup_type,
               register_type!, RegistryEntry, TypeRegistry,
               _is_placeholder_hash, _pinned_conflict, _warn_revision_mismatch
using ROSZenoh: TypeInfo, TypeHash, to_rihs_string
using Test
using Logging: Logging

# A throwaway ament prefix with one self-contained message (hashes exactly — no
# cross-package refs), the gate's name-only lookup target.
function _widget_prefix()
    root = mktempdir()
    md = joinpath(root, "share", "robot_msgs", "msg")
    mkpath(md)
    write(joinpath(md, "Widget.msg"), "string label\nint32 count\nfloat64[] samples\n")
    return root
end

# A nonzero (non-placeholder) digest distinct from any real hash — a peer's
# diverging revision of the same name.
_other_hash() = TypeHash(0x01, ntuple(i -> UInt8((i * 7) % 256), 32))

@testset "D7 type-revision trust" begin

    # ── helpers (pure, no Context) ──────────────────────────────────────────
    @testset "placeholder predicate + pinned-conflict scan" begin
        # `TypeHash()` is the RIHS01 placeholder (version 01, all-zero digest); any
        # real digest is not — the gate uses this to wave through uncomparable refs.
        @test _is_placeholder_hash(TypeHash())
        @test !_is_placeholder_hash(_other_hash())

        # `_pinned_conflict` finds a pinned (:static/:authored) entry under `name`
        # registered at a hash other than the one being resolved; nothing otherwise.
        reg = TypeRegistry()
        pinned_hash = _other_hash()
        info = TypeInfo("robot_msgs/msg/Widget", pinned_hash)
        # An :ament entry is NOT pinned, so it never counts as a conflict.
        register_type!(reg, info, RegistryEntry(info, nothing; provenance = :ament))
        @test _pinned_conflict(reg, "robot_msgs/msg/Widget", TypeHash()) === nothing
        # A :static entry under a different hash IS the conflict.
        register_type!(reg, info, RegistryEntry(info, nothing; provenance = :static))
        c = _pinned_conflict(reg, "robot_msgs/msg/Widget", TypeHash())
        @test c isa RegistryEntry && c.provenance === :static
        # Querying with the pinned hash itself excludes it (no self-conflict).
        @test _pinned_conflict(reg, "robot_msgs/msg/Widget", pinned_hash) === nothing
        # The diagnostic helper honors `weak`: loud @warn vs quiet @debug, same text.
        @test_logs (:warn,) _warn_revision_mismatch("p/msg/X", TypeHash(), _other_hash(); weak = false)
        @test_logs min_level = Logging.Debug (:debug,) _warn_revision_mismatch("p/msg/X", TypeHash(), _other_hash(); weak = true)
    end

    # ── the gate, driven through `resolve_type` against a real registry ──────
    root = _widget_prefix()
    withenv("AMENT_PREFIX_PATH" => root) do
        ctx = Context(; localhost_only = true)               # default: pinned trust
        try
            reg = registry(ctx)
            # The locally-parsed RIHS01 the ament Widget hashes to — the value a
            # matching peer would advertise.
            real = load_ament_type(reg, "robot_msgs/msg/Widget"; register = false).info.hash
            @test !_is_placeholder_hash(real)

            @testset "S1 matching hash binds" begin
                info = TypeInfo("robot_msgs/msg/Widget", real)
                entry = resolve_type(ctx, info; cache = false)
                @test entry isa RegistryEntry
                @test entry.info.hash == real
                @test entry.provenance === :ament
                # bound = now a registry hit under that exact (name, hash) key
                @test lookup_type(reg, info) === entry
            end

            @testset "S2 real-hash mismatch is rejected (default trust)" begin
                info = TypeInfo("robot_msgs/msg/Widget", _other_hash())
                warned = Ref(false)
                # Default (pinned) trust ⇒ a loud @warn on the divergence.
                entry = @test_logs (:warn,) match_mode = :any begin
                    resolve_type(ctx, info; cache = false, warned = warned)
                end
                @test entry === nothing                       # NOT bound
                @test warned[]                                # signalled to the wire site
                @test lookup_type(reg, info) === nothing      # nothing landed under the bad key
            end
        finally
            close(ctx)
        end
    end

    @testset "S3 weak mode softens the diagnostic (still not bound)" begin
        # Coverage boundary: at the resolve_type layer `weak_types` ONLY downgrades
        # the mismatch log (@warn→@debug). The gate STILL returns nothing on a
        # real-hash mismatch under weak mode — the actual wire-discovery + bind of the
        # peer's revision happens in introspection.jl `resolve_or_discover` (needs a
        # live remote advertiser) and is exercised by the discovery suite, not here.
        root = _widget_prefix()
        withenv("AMENT_PREFIX_PATH" => root) do
            ctx = Context(; localhost_only = true, weak_types = true)
            try
                @test ctx.weak_types
                info = TypeInfo("robot_msgs/msg/Widget", _other_hash())
                warned = Ref(false)
                # No :warn record under weak mode (the diagnostic dropped to @debug).
                entry = @test_logs min_level = Logging.Warn begin
                    resolve_type(ctx, info; cache = false, warned = warned)
                end
                @test entry === nothing                       # weak does NOT bind at this layer
                @test warned[]                                # mismatch still signalled
            finally
                close(ctx)
            end
        end
    end

    @testset "S4 placeholder hash is accepted, not rejected" begin
        # The predicate is verified above; here the gate's accept-placeholder
        # behavior. A genuine no-struct ament definition (the only source of the
        # placeholder from `load_ament_type`) is unreachable from real .msg/.srv/
        # .action text — every interface lowers to a struct — so the gate's
        # placeholder branch is exercised at the predicate + registry level: a
        # placeholder-keyed entry resolves rather than being turned away.
        @test _is_placeholder_hash(TypeHash())
        ctx = Context(; localhost_only = true)
        try
            reg = registry(ctx)
            info = TypeInfo("placeholder_msgs/msg/Ref", TypeHash())
            entry = RegistryEntry(info, nothing; provenance = :ament)
            register_type!(reg, info, entry)
            # A placeholder-hash type resolves (registry hit) — it is a valid,
            # accepted key, not a rejected one.
            @test resolve_type(ctx, info; ament = false, cache = false) === entry
        finally
            close(ctx)
        end
    end
end
