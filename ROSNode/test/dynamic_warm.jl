# §D9 — precompilation × dynamic discovery: the per-node interaction manifest and
# Tier-1 startup warm. Pure-logic tests for the manifest's persistence + union/dedup +
# opt-in gating (no session), plus live tests that (a) a dynamic dispatch *records* the
# interaction and (b) a fresh consumer *replays* it at startup, resolving + warming the
# recorded type before the first message.
#
# Standalone (outside the suite): start a router and point these at it, e.g.
#     zenohd -l tcp/localhost:7447 &
#     ROS_TEST_EP=tcp/localhost:7447 julia --project=… ROSNode/test/dynamic_warm.jl
# Always under a hard force-kill: `timeout -k 5 120 julia …`.

using ROSNode
using ROSNode: Context, Node, Subscription, Publisher, publish, WireKeyValue,
               DynamicSubscriptionHandle, registry, register_type_description!,
               lookup_type, realize!, resolve_name, _scan_for_struct,
               note_interaction!, load_manifest, Interaction
using ROSMessages: message_il, lower, type_description_from_struct,
                   TypeDescription, TypeDescriptionMsg, calculate_rihs01_hash
using ROSZenoh: TypeInfo, TypeHash, type_hash_from_rihs_string
using Test

const _DWEP = isdefined(Main, :ROS_TEST_EP) ? Main.ROS_TEST_EP :
              get(ENV, "ROS_TEST_EP", "tcp/localhost:7447")
_dwctx(f::Function) = Context(f; peers = [_DWEP], localhost_only = true)

function _dwrecv(ch::Channel, secs::Real)
    ref = Ref{Any}(nothing)
    timedwait(Float64(secs); pollint = 0.05) do
        isready(ch) ? (ref[] = take!(ch); true) : false
    end === :ok ? ref[] : nothing
end

# A throwaway hash for the manifest unit tests (any 32-byte digest round-trips through
# the RIHS01 string form the file uses as its key).
_mkhash(seed::Integer) = TypeHash(0x01, ntuple(i -> UInt8((seed + i) % 256), 32))

@testset "D9 manifest + Tier-1 warm" begin

    # ── manifest persistence: round-trip, union/dedup, opt-in gating (no session) ──
    @testset "manifest persistence (unit)" begin
        # Save/restore the env override so the test controls the opt-in deterministically.
        had_env = haskey(ENV, "ROS_TYPESUPPORT_CACHE")
        saved   = get(ENV, "ROS_TYPESUPPORT_CACHE", nothing)
        had_env && delete!(ENV, "ROS_TYPESUPPORT_CACHE")
        dir = mktempdir()
        try
            # Disabled by default ⇒ nothing recorded, nothing read.
            ROSNode.disable_project_cache!()
            empty!(ROSNode._MANIFESTS)
            @test note_interaction!("/ns/n", :subscription, _mkhash(1), "p/msg/A", "/t") == false
            @test isempty(load_manifest("/ns/n"))

            # Enable to the temp dir ⇒ records persist.
            ROSNode.enable_project_cache!(dir)
            empty!(ROSNode._MANIFESTS)
            hA, hB = _mkhash(1), _mkhash(2)
            @test note_interaction!("/ns/n", :subscription, hA, "p/msg/A", "/t") == true
            @test note_interaction!("/ns/n", :subscription, hB, "p/msg/B", "/t") == true
            @test note_interaction!("/ns/n", :subscription, hA, "p/msg/A", "/t") == false  # dedup (same run)
            # A different node's manifest is independent.
            @test note_interaction!("/ns/other", :subscription, hA, "p/msg/A", "/t") == true

            # Simulate a fresh process: drop the in-memory cache, re-read from disk.
            empty!(ROSNode._MANIFESTS)
            entries = load_manifest("/ns/n")
            @test length(entries) == 2
            @test Set((e.name, e.topic) for e in entries) == Set([("p/msg/A", "/t"), ("p/msg/B", "/t")])
            @test all(e -> e.role === :subscription, entries)
            # The hash survived the RIHS01 round-trip.
            @test any(e -> e.hash == hA && e.name == "p/msg/A", entries)

            # Union across runs: a record already on disk is not re-appended.
            @test note_interaction!("/ns/n", :subscription, hA, "p/msg/A", "/t") == false
            @test note_interaction!("/ns/n", :subscription, _mkhash(3), "p/msg/C", "/t") == true
            empty!(ROSNode._MANIFESTS)
            @test length(load_manifest("/ns/n")) == 3
        finally
            ROSNode.disable_project_cache!()
            empty!(ROSNode._MANIFESTS)
            had_env && (ENV["ROS_TYPESUPPORT_CACHE"] = saved)
            rm(dir; recursive = true, force = true)
        end
    end

    # ── live: a dynamic dispatch records the interaction it used ──────────────────
    @testset "dynamic dispatch records the interaction (live)" begin
        dir = mktempdir()
        try
            ROSNode.enable_project_cache!(dir)
            empty!(ROSNode._MANIFESTS)
            _dwctx() do ctx
                node = Node(ctx, "rec")                    # default warmup = :precompile
                got  = Channel{Any}(8)
                sub  = Subscription(node, "/kv") do msg; put!(got, msg); end
                @test sub isa DynamicSubscriptionHandle
                pub  = Publisher(node, "/kv", WireKeyValue)
                sleep(0.4)
                publish(pub, WireKeyValue(key = "hello", value = "d9"))
                @test _dwrecv(got, 5.0) !== nothing        # dispatched ⇒ first-sight branch ran

                # The first-sight warm branch ran synchronously before dispatch, so the
                # interaction is already on disk. Re-read from disk (fresh in-memory).
                empty!(ROSNode._MANIFESTS)
                entries = load_manifest(node.fqn)
                topic = resolve_name(node, "/kv")
                @test any(e -> e.role === :subscription && e.topic == topic &&
                               occursin("KeyValue", e.name), entries)
                close(sub); close(pub)
            end
        finally
            ROSNode.disable_project_cache!()
            empty!(ROSNode._MANIFESTS)
            rm(dir; recursive = true, force = true)
        end
    end

    # ── live: Tier-1 startup replay resolves + realizes a recorded (custom) type ──
    # A custom type (not a pre-registered well-known) is registered-but-unrealized;
    # a manifest entry for it is written as if a prior run used it on the topic; then a
    # *fresh* consumer (in-memory manifest cleared) is constructed with `warmup_sync` —
    # the Tier-1 replay must resolve + `realize!` the type at construction, before any
    # message. Observable: the registry entry goes from unrealized to realized.
    @testset "Tier-1 replay realizes recorded types at startup (live)" begin
        dir = mktempdir()
        try
            ROSNode.enable_project_cache!(dir)
            empty!(ROSNode._MANIFESTS)
            _dwctx() do ctx
                node = Node(ctx, "warmstart")
                # A custom type, registered without realizing (codegen deferred).
                il   = message_il("string id\nint64 stamp\n"; name = "Warm")
                ast  = _scan_for_struct(lower(il; package = "d9_demo"))
                td   = TypeDescriptionMsg(
                          type_description_from_struct(ast, "Warm"; package = "d9_demo", qualifier = "msg"),
                          TypeDescription[])
                h    = type_hash_from_rihs_string(calculate_rihs01_hash(td))
                info = TypeInfo("d9_demo/msg/Warm", h)
                register_type_description!(registry(ctx), td, info; cache = false)
                entry = lookup_type(registry(ctx), info)
                @test entry !== nothing
                @test entry.type === nothing               # not yet realized

                # Record the interaction as a prior run would, on the topic the new sub
                # will use (the resolved FQN — what the dispatch/replay filter compares).
                topic = resolve_name(node, "/wt")
                @test note_interaction!(node.fqn, :subscription, h, "d9_demo/msg/Warm", topic) == true

                # Fresh process: drop the in-memory manifest so the replay re-reads disk.
                empty!(ROSNode._MANIFESTS)

                # Construct the dynamic sub with sync warm-up: Tier-1 replay runs inline
                # at construction, resolving (wire=false) + realizing the recorded type.
                sub = Subscription(node, "/wt"; warmup = :precompile, warmup_sync = true) do msg; end
                @test sub isa DynamicSubscriptionHandle
                @test entry.type !== nothing               # Tier-1 realized it at startup
                close(sub)
            end
        finally
            ROSNode.disable_project_cache!()
            empty!(ROSNode._MANIFESTS)
            rm(dir; recursive = true, force = true)
        end
    end
end
