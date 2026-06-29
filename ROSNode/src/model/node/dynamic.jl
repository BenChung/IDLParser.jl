# Type-less subscription dispatch. The route is a wildcard data keyexpr matching
# every type on the topic; each sample carries its `(name, hash)`, resolved to a
# runtime-born type `T` reached through `invokelatest` for decode and handler. The
# typed form is faster — it drops the per-sample type resolution.
#
# Split into two tasks so codegen never deadlocks against the blocking recv:
#   • receiver (sticky): owns `z_recv`, copies bytes and enqueues, parked in a
#     GC-safe recv so foreign zenohc threads keep reaching safepoints;
#   • worker (plain `@spawn`): drains the buffer and runs resolve → `realize!`
#     codegen → dispatch. Its GC completes because the receiver keeps the FIFO
#     draining.
# Correct single-threaded: the FIFO recv is a `@threadcall` (the Julia task yields),
# so the worker runs even under `-t1`; `-t>=2` adds overlap. The buffer is
# QoS-depth-sized, so a slow first-sight realize backs up to that bound and the
# receiver blocks on `put!` — upstream backpressure, no drop.

using FunctionWrappers: FunctionWrapper

# Strip one leading and trailing slash so a parsed-keyexpr topic (no slashes) can
# be matched against a graph FQN topic.
function _strip_one_slash_local(s::AbstractString)
    s = startswith(s, "/") ? SubString(s, nextind(s, 1)) : SubString(s, 1)
    s = endswith(s, "/")   ? SubString(s, 1, prevind(s, lastindex(s))) : s
    return String(s)
end

# The wildcard data keyexpr for a dynamic subscription. Under rmw_zenoh the data
# key is `<domain>/<topic>/<type…>/<hash>`, so `**` matches the whole type+hash
# tail — every type published on the topic routes to us. Other formats (ros2dds)
# carry no type in the key, so the plain topic key already matches all data and the
# per-sample type is recovered from the graph instead.
function _wildcard_data_keyexpr(::RmwZenoh, ep::EndpointEntity)
    ep.node === nothing && throw(ArgumentError("dynamic subscription requires a node"))
    return string(ep.node.domain_id, "/", _strip_one_slash_local(ep.topic), "/**")
end
_wildcard_data_keyexpr(fmt::KeyExprFormat, ep::EndpointEntity) = topic_keyexpr(fmt, ep)

"""
    declare_subscription!(entity, handler; view=Owned(), concurrency=Serial())

Declare a type-less (dynamic) subscription route: a FIFO Zenoh subscriber on the
topic's wildcard data keyexpr, accepting samples of every type published on the
topic, plus a consumer that resolves each sample's type at runtime and dispatches
it through `invokelatest`. The typed method `declare_subscription!(entity, msgtype,
handler; …)` decodes a fixed `T` directly for the min-copy fast path.

Keyword arguments:
- `view::Union{Bool,ViewMode}`: how each sample reaches the handler — see
  [`ViewMode`](@ref). Every mode applies as in the typed form; resolution crosses
  the world-age boundary first, then `_dispatch_decoded` honors the mode unchanged:
    - [`Owned`](@ref)
    - [`Checked`](@ref)
    - [`Unchecked`](@ref)
- `concurrency::Concurrency`: how decode+handler are scheduled per sample — see
  [`Concurrency`](@ref) ([`Serial`](@ref) or [`Parallel`](@ref)). A single worker
  drains the buffer in arrival order across world-age re-entries, so [`Serial`](@ref)
  preserves total order even through first-sight `realize!` codegen.
- `warmup::WarmupPolicy`: the warm-up policy ([`WarmupMode`](@ref) plus a sync flag)
  that pre-JITs the encode/decode dispatch chain, defaulting to the node default. Here
  it also replays this node's interaction manifest to warm prior-run types ahead of the
  first message. The handler can branch on [`is_warming`](@ref) to skip side effects
  during a warm pass.

ROS 2 topic/subscription model:
https://docs.ros.org/en/rolling/Concepts/Basic/About-Topics.html
"""
function declare_subscription!(e::Entity, handler;
                               view::Union{Bool, ViewMode}=Owned(),
                               concurrency::Concurrency=Serial(),
                               warmup::WarmupPolicy=e.node.warmup)
    view = _view_mode(view)
    ctx = e.node.context
    ke = _wildcard_data_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    # A dynamic sub's keyexpr is a type-wildcard (`<domain>/<topic>/**`), so it can't use the
    # advanced-pubsub history query (which recovers latched state only over a concrete keyexpr).
    # transient_local latched capture therefore needs the TYPED subscription path (entity.jl);
    # a consumer that wants latched delivery resolves the type and uses `Subscription(node, topic, T)`.
    sub = Base.open(ctx.session, Keyexpr(ke); channel=:fifo, capacity=cap)
    e._route = sub
    e._consumer = _spawn_dynamic_consumer(e, handler, view, concurrency, warmup)
    return e
end

# Recycle pool of `SampleHolder`s for the receiver→worker handoff, avoiding a
# per-message allocation. Size = `cap` (buffered) + in-flight decodes + 1 (the one
# the receiver is filling). `Parallel(Inf)` returns `nothing` (unbounded in-flight
# decodes can't share a fixed pool), so that mode allocates a fresh holder per message.
function _make_holder_pool(concurrency::Concurrency, cap::Integer)
    concurrency isa Parallel && !isfinite(concurrency.n) && return nothing
    inflight = concurrency isa Parallel ? Int(concurrency.n) : 1
    sz = Int(cap) + inflight + 1
    pool = Channel{SampleHolder}(sz)
    for _ in 1:sz
        put!(pool, SampleHolder())
    end
    return pool
end

function _spawn_dynamic_consumer(e::Entity, handler, view::ViewMode, concurrency::Concurrency,
                                 warmup::WarmupPolicy)
    sched = _make_scheduler(concurrency)
    logged = Set{Tuple{String, TypeHash}}()      # log each discovered type once
    loglk = ReentrantLock()
    warmed = Set{DataType}()                      # warm each runtime type once at first sight
    warmlk = ReentrantLock()                      # guards `warmed`, shared with the manifest-replay task
    cap = _fifo_capacity(e.endpoint.qos)
    # Buffer between receiver and worker, QoS-depth-bounded so a slow first-sight
    # realize backs up to `cap` and the receiver then blocks on `put!` (upstream
    # backpressure, no drop). The receiver's `finally` closes it so the worker
    # drains-then-exits.
    # Holders carry an owned sample (refcount bump, no payload copy); recv! moves it
    # out of the ring slot, so a holder sitting in the buffer or captured by a
    # Parallel decode thunk never pins the ring.
    buf = Channel{SampleHolder}(cap)
    pool = _make_holder_pool(concurrency, cap)
    # Worker: a plain `@spawn` (not sticky) so its codegen GC completes while the
    # receiver stays parked in the GC-safe recv. Drains the buffer in order via the
    # world-age trampoline `_dynamic_worker`, which hoists `invokelatest` from
    # per-message to per-type.
    worker = Threads.@spawn try
        # Install the node logger once for the worker and the handler tasks the
        # scheduler spawns, so handler `@info` routes to /rosout without a
        # per-message `with_logger` scope.
        _with_node_logger(e.node) do
            _dynamic_worker(e, buf, handler, sched, view, pool, logged, loglk, warmed, warmlk, warmup)
        end
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "dynamic subscription worker task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end

    # Receiver. `recv!`s each sample into an owned `SampleHolder` and enqueues it,
    # with no payload copy and no keyexpr String — the holder carries the refcounted
    # payload and keyexpr, and the worker decodes and resolves by borrowing them.
    # Staying light keeps the ring drained while the worker codegens. `recv!` returns
    # nothing on close, ending the loop; `finally` closes the buffer so the worker
    # drains-then-exits.
    t = Task() do
        route = e._route::SubscriberHandler
        try
            while true
                h = pool === nothing ? SampleHolder() : take!(pool)
                if recv!(route, h) === nothing                # subscriber closed
                    pool === nothing || put!(pool, h)
                    break
                end
                if Zenoh.payload(h) === nothing               # e.g. a delete/tombstone
                    pool === nothing || put!(pool, h)         # recycle, don't enqueue
                    continue
                end
                _note_lost(e, h)                              # report dropped samples, only when a listener is registered
                # No `string(keyexpr(h))` here: the owned holder carries the keyexpr,
                # so the worker reads it by borrowing (hash + memcmp cache hit), and
                # only a cold first-sight miss materializes the String.
                try
                    put!(buf, h)
                catch err
                    # Buffer closed (teardown raced the route close) — stop cleanly.
                    err isa InvalidStateException && break
                    rethrow()
                end
            end
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @error "dynamic subscription receiver task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        finally
            close(buf)            # signal the worker to drain-and-exit
        end
    end
    t.sticky = true
    schedule(t)
    # Startup warm: replay this node's interaction manifest for this topic, warming
    # the codecs of types used on prior runs ahead of the first message to keep the
    # JIT off the hot path. Concurrent with the receiver/worker; the `warmlk`-guarded
    # `warmed` set makes whichever path warms a type first win, the other skip it.
    warmup.mode isa NoWarmup ||
        _spawn_manifest_warm(e, handler, warmed, warmlk, warmup)
    # `worker` needs no handle: the scheduler roots it while it runs, and the
    # receiver's `finally close(buf)` ends its drain loop on route close.
    return t
end

# Schedule the manifest replay. `sync` runs it inline at construction, guaranteeing
# the first message is warm; otherwise a plain `Threads.@spawn` so its potential
# `realize!` codegen GC completes while the node's other consumers keep safepoints
# reachable (same JIT-vs-Zenoh discipline as the dynamic worker). With `@ros_cache`
# baked, the resolve is a registry hit and no codegen runs.
function _spawn_manifest_warm(e::Entity, handler, warmed, warmlk, warmup::WarmupPolicy)
    if warmup.sync
        _replay_manifest_warm(e, handler, warmed, warmlk)
    else
        Threads.@spawn try
            _replay_manifest_warm(e, handler, warmed, warmlk)
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @warn "dynamic manifest warm-up failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    nothing
end

# Resolve and warm each manifest entry for this subscription's topic. `wire=false`
# keeps a node coming up from blocking on a remote `~/get_type_description`; a
# genuinely-new type instead pays one reactive wire fetch on its first message. A
# stale entry (cache evicted, version bump) resolves to `nothing` and is skipped.
# Crosses to the runtime-born type's warm anchor via `invokelatest`, as the worker does.
function _replay_manifest_warm(e::Entity, handler, warmed, warmlk)
    for it in load_manifest(e.node.fqn)
        it.role === :subscription || continue
        it.topic == e.endpoint.topic || continue
        T = _resolve_home(e.node, it.hash)                             # home table first
        T === nothing && (T = resolve_or_discover(e.node, it.name, it.hash; wire=false))
        T === nothing && continue
        isnew = @lock warmlk (T in warmed ? false : (push!(warmed, T); true))
        isnew || continue
        try
            Base.invokelatest(_warm_dynamic, T, handler)
        catch err
            err isa ShutdownException && rethrow()
            @warn "dynamic manifest warm-up: warming $(it.name) failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    nothing
end

# Per-type world-age crossing, hoisted out of the per-message path. A runtime-born
# type is in a newer world than the compiled dispatcher, so a direct call errors; the
# worker builds one `FunctionWrapper` per type (through the single `invokelatest`
# crossing, where the trampoline compiles against `T`'s codegen world) and thereafter
# the hot path calls the cached FW as a `ccall` through its cfunction ptr — not a
# world-age-gated Julia dispatch. A single worker drains `buf` in arrival order, so
# `Serial` total order holds.

# Crossing counter, bumped once per FW build (≈ distinct types, never per message);
# a test asserts it does not scale with message count.
const _DYNAMIC_REENTRIES = Threads.Atomic{Int}(0)

# A type-stable per-type dispatch callable `(SampleHolder) -> nothing` wrapping
# `_dispatch_decoded` for a fixed `T`; built via `invokelatest` so its trampoline
# compiles against the runtime-born `T`'s world, then called as a `ccall` on the hot path.
const _DynFW = FunctionWrapper{Nothing, Tuple{SampleHolder}}
_mk_fw(::Type{T}, e::Entity, handler, view::ViewMode) where {T} =
    _DynFW(h -> _dispatch_decoded(e, h, T, handler, view))

# Keyexpr cache key: a hash of the borrowed keyexpr bytes, so the hot path never
# allocates a `String`. FNV-1a over the raw bytes — allocation-free, no `unsafe_wrap`.
# Collisions are resolved by a `memcmp` against the stored bytes (`_ke_bytes_eq`), so
# the hash needn't be cryptographic, only well-spread.
@inline function _ke_fnv1a(ptr::Ptr{UInt8}, len::Int)
    h = 0xcbf29ce484222325 % UInt
    for i in 1:len
        h = (h ⊻ unsafe_load(ptr, i)) * (0x00000100000001b3 % UInt)
    end
    return h
end

# memcmp the borrowed keyexpr (ptr,len) against a stored byte copy — the collision
# check that makes the hash cache exact.
@inline function _ke_bytes_eq(ptr::Ptr{UInt8}, len::Int, b::Vector{UInt8})
    length(b) == len || return false
    return GC.@preserve b (ccall(:memcmp, Cint,
        (Ptr{UInt8}, Ptr{UInt8}, Csize_t), ptr, pointer(b), len) == 0)
end

# Resolve a wire hash against the Context's `home` module's baked `__ros_resolve__`
# table (per-module, deterministic). Returns `nothing` when there is no home or the
# home's dependency closure never imported this type, leaving the caller to fall
# back to `resolve_or_discover`.
function _resolve_home(node, hash)
    home = _ctx(node).home
    home === nothing && return nothing
    return resolve_in_home(home, Symbol(to_rihs_string(hash)))
end

function _dynamic_worker(e::Entity, buf, handler, sched, view::ViewMode, pool,
                         logged, loglk, warmed, warmlk, warmup::WarmupPolicy)
    # keyexpr-hash → (per-type FW, keyexpr bytes for memcmp verify). Worker-private (one
    # worker, no lock); the byte copy is the collision-safety net. With each type's FW
    # built once, this loop never re-enters a fresh world.
    cache = Dict{UInt, Tuple{_DynFW, Vector{UInt8}}}()
    for h in buf
        # Resolve the cached FW by borrowing the keyexpr bytes (no String): hash, look up,
        # `memcmp`-verify against the stored copy so a hash collision can't mis-dispatch.
        fw = Zenoh.keyexpr_view(h) do ptr, len
            entry = get(cache, _ke_fnv1a(ptr, len), nothing)
            (entry !== nothing && _ke_bytes_eq(ptr, len, entry[2])) ? entry[1] : nothing
        end
        if fw === nothing                                  # first sight of this keyexpr
            fw = _resolve_and_build!(e, h, cache, handler, view, logged, loglk, warmed, warmlk, warmup)
            if fw === nothing                              # unresolved type — recycle + skip
                pool === nothing || put!(pool, h)
                continue
            end
        end
        # Dispatch per the concurrency policy; the thunk owns the holder's lifetime. A
        # `Parallel` spawn captures the concrete FW + owned `h` and runs the whole
        # `_dispatch_decoded` synchronously inside the borrow — never decode-then-spawn.
        let fw = fw, h = h
            sched() do
                try
                    fw(h)                                  # ccall through the trampoline ptr — no dispatch
                finally
                    pool === nothing || put!(pool, h)
                end
            end
        end
    end
    nothing
end

# First sight of a keyexpr: materialize the String (cold, once per type), resolve the
# sample's type, `realize!` it if new, then build + cache its `FunctionWrapper` — the single
# per-type `invokelatest` world-age crossing (the trampoline compiles against `Tnew`'s codegen
# world, which also warms its `decode`+handler). Returns the FW, or `nothing` if the type
# can't be resolved (worker recycles the holder + skips). A resolve error is logged and
# swallowed (one bad sample must not kill the subscription); a `ShutdownException` propagates.
function _resolve_and_build!(e::Entity, h::SampleHolder,
                             cache::Dict{UInt, Tuple{_DynFW, Vector{UInt8}}},
                             handler, view::ViewMode, logged, loglk, warmed, warmlk,
                             warmup::WarmupPolicy)
    ke = string(Zenoh.keyexpr(h))
    try
        info = _sample_type_info(e, ke)
        info === nothing && return nothing
        Tnew = _resolve_home(e.node, info.hash)                    # home table first
        source = :home
        if Tnew === nothing
            Tnew = resolve_or_discover(e.node, info.name, info.hash)
            # Provenance for the graduation hint: `:static`/`:cache`/`:ament` mean we knew
            # the type locally; `:wire` means fetched + generated from the publisher's
            # ~/get_type_description.
            ent = Tnew === nothing ? nothing : lookup_type(registry(_ctx(e.node)), info)
            source = ent === nothing ? :wire : ent.provenance
        end
        if Tnew === nothing
            @warn "dynamic subscription: could not resolve type" topic=e.endpoint.topic type=info.name hash=to_rihs_string(info.hash) maxlog=1
            return nothing
        end
        # Build + cache the type-stable handler-of-bytes — the single per-type world-age
        # crossing, since `Tnew` is runtime-born and the trampoline must compile in the
        # post-`realize!` world. Key on the same FNV-1a the hot path uses; stash the bytes
        # for the memcmp collision check.
        Threads.atomic_add!(_DYNAMIC_REENTRIES, 1)
        fw = Base.invokelatest(_mk_fw, Tnew, e, handler, view)::_DynFW
        kebytes = Vector{UInt8}(codeunits(ke))
        cache[GC.@preserve kebytes _ke_fnv1a(pointer(kebytes), length(kebytes))] = (fw, kebytes)
        _log_discovered_once(e, info, source, logged, loglk)
        # Record for the next-run manifest warm, and mark `warmed` so the manifest-replay task
        # skips a type the worker already built (coordination only; the FW build warmed the
        # codec, the manifest task only `precompile`s).
        if !(warmup.mode isa NoWarmup)
            @lock warmlk (Tnew in warmed || push!(warmed, Tnew))
            note_interaction!(e.node.fqn, :subscription, info.hash, info.name, e.endpoint.topic)
        end
        return fw
    catch err
        err isa ShutdownException && rethrow()
        @error "dynamic subscription resolve failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        return nothing
    end
end

# The TypeInfo for a received sample: from the data keyexpr (rmw_zenoh embeds it),
# falling back to the graph (ros2dds / empty-type keys carry no type identity).
function _sample_type_info(e::Entity, ke::AbstractString)
    ctx = e.node.context
    parsed = try
        parse_topic_keyexpr(ctx.format, ke)
    catch err
        @debug "dynamic sub: unparseable data keyexpr" ke exception=err
        return nothing
    end
    parsed.type_info === nothing || return parsed.type_info
    return _graph_type_for_topic(ctx, parsed.topic)
end

# A publisher's advertised TypeInfo for `topic` (stripped form), from the discovery
# index — the type source when the data keyexpr carries none.
function _graph_type_for_topic(ctx, topic::AbstractString)
    for ep in endpoints_snapshot(ctx)
        ep.type === nothing && continue
        ep.kind === Publisher || continue
        _strip_one_slash_local(ep.topic) == topic || continue
        return ep.type
    end
    return nothing
end

# Log a discovered (name, hash) once per subscription: tell the user what to write
# to graduate to the static fast path.
function _log_discovered_once(e::Entity, info::TypeInfo, source::Symbol, logged, loglk)
    isnew = @lock loglk (info.name, info.hash) in logged ? false :
                        (push!(logged, (info.name, info.hash)); true)
    isnew || return nothing
    bare = split(info.name, '/')[end]
    graduate = "Subscription(node, \"$(e.endpoint.topic)\", $(bare))"
    rihs = to_rihs_string(info.hash)
    if source === :wire
        @info "dynamic subscription auto-generated a type over the wire — fetched its \
               definition from the publisher's ~/get_type_description (no local copy); \
               register it (e.g. `@ros_import`) to skip wire discovery, or use the static \
               form for the min-copy fast path" topic=e.endpoint.topic type=info.name hash=rihs graduate=graduate
    else
        @info "dynamic subscription resolved a known type — for the min-copy fast path, \
               use the static form" topic=e.endpoint.topic type=info.name hash=rihs source=source graduate=graduate
    end
    nothing
end

