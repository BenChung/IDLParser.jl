# ── dynamic (keyexpr-only) subscription dispatch (§11/D5 S5) ───────────────────
# A subscription created without a compile-time type. The route is a *wildcard*
# data keyexpr matching every type on the topic; the type rides in each sample's
# keyexpr. Per sample we recover `(name, hash)`, resolve_or_discover the real
# runtime type `T`, then cross the world-age boundary with `invokelatest` for both
# decode and the handler call (`T` is born at runtime, §11). Owned-only — the
# min-copy `view` path is the static fast path you graduate to.
#
# Two-stage pipeline (D8 — decouple reception from codegen). The first sight of a
# type runs `realize!` → `Core.eval` (heavy-allocating codegen that triggers GC).
# If that ran on the task owning the blocking `z_recv`, the FIFO would stop draining
# mid-codegen; a libzenohc foreign thread mid-handoff would never reach a Julia
# safepoint, and the GC the codegen needs could never complete — a hang (the JIT
# racing Zenoh). So we split:
#   • a *receiver* task (sticky, owns the recv) does nothing but copy bytes and
#     enqueue them — it stays parked in the recv and keeps the FIFO drained, so
#     foreign threads always reach safepoints;
#   • a *worker* task (a plain `@spawn`) drains the buffer and does resolve →
#     codegen → dispatch. Its GC can now complete (the receiver is parked in a
#     GC-safe recv and draining), breaking the deadlock at the source.
# This holds even single-threaded: Zenoh.jl's FIFO recv is a `@threadcall` (the
# blocking C recv runs on a libuv pool thread; the Julia task yields), so the worker
# is scheduled even with `-t1`. While the worker is mid-`Core.eval` (no yield points)
# the libuv thread keeps draining Zenoh's ring — Zenoh QoS buffers upstream — and
# Julia resumes the receiver when codegen yields. `-t>=2` just adds true parallelism
# (worker codegen overlapping reception); it isn't required for correctness.
# The buffer is sized to the QoS depth: a slow first-sight realize backs samples up
# to that bound, then the receiver blocks on `put!` (real backpressure upstream),
# never a silent drop and never a process-wide hang (the recoverable soft-lock).

# Strip exactly one leading/trailing slash (mirrors ROSZenoh's topic stripping) so
# we can match a parsed-keyexpr topic (no slashes) against a graph FQN topic.
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
    declare_subscription!(entity, handler; concurrency=Serial())

The no-`msgtype` method of [`declare_subscription!`](@ref): declare a *type-less*
(dynamic, §11/D5 S5) subscription route — a FIFO Zenoh subscriber on the wildcard
data keyexpr (so samples of any type on the topic arrive), plus a consumer that
per-sample resolves the type and dispatches via `invokelatest`. Contrast the typed
method `declare_subscription!(entity, msgtype, handler; …)` which decodes a fixed
`T` directly.
"""
function declare_subscription!(e::Entity, handler;
                               view::Union{Bool, ViewMode}=Owned(),
                               concurrency::Concurrency=Serial(),
                               warmup::WarmupPolicy=e.node.warmup)
    view = _view_mode(view)
    ctx = e.node.context
    ke = _wildcard_data_keyexpr(ctx.format, e.endpoint)
    cap = _fifo_capacity(e.endpoint.qos)
    sub = Base.open(ctx.session, Keyexpr(ke); channel=:fifo, capacity=cap)
    e._route = sub
    e._consumer = _spawn_dynamic_consumer(e, handler, view, concurrency, warmup)
    return e
end

# A recycle pool of `SampleHolder`s for the dynamic receiver→worker handoff, so we
# don't allocate one per message. Size = `cap` (buffered) + in-flight decodes + 1
# (the receiver filling one). `nothing` for `Parallel(Inf)`: unbounded concurrent
# decodes can't be bounded by a fixed pool, so that mode keeps a fresh holder per
# message. The finalizer on each holder stays as a shutdown-time backstop — pooled
# holders are reused, so it ~never fires per message.
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
    logged = Set{Tuple{String, TypeHash}}()      # S6: log each discovered type once
    loglk = ReentrantLock()
    warmed = Set{DataType}()                      # §D8: warm each runtime type once at first sight
    warmlk = ReentrantLock()                       # §D9: `warmed` is now shared with the Tier-1 replay task
    # The buffer between the (sticky) Zenoh receiver and the resolve/codegen/dispatch
    # worker. Bounded to the QoS depth so it mirrors the FIFO's own backpressure: a
    # slow first-sight realize fills it to `cap`, then the receiver blocks on `put!`
    # (upstream backpressure), never silently dropping. Closed by the receiver's
    # `finally` so the worker drains-then-exits on teardown.
    cap = _fifo_capacity(e.endpoint.qos)
    # Carry an *owned* sample (a refcount bump, no payload copy) from receiver to
    # worker, not a copied byte buffer — the worker decodes by borrowing it. The
    # owned holder may sit in the buffer / be captured by a Parallel decode thunk
    # without pinning the ring (recv! moved it out of the ring slot), so D1 holds.
    buf = Channel{SampleHolder}(cap)
    # Recycle the holders instead of allocating one per message: receiver takes a
    # free holder, the decode thunk returns it after use. Sized for everything that
    # can pin a holder at once — `cap` in the buffer + the in-flight decodes + the
    # one the receiver is filling. `nothing` for `Parallel(Inf)` (unbounded in-flight
    # can't be pooled → fall back to a fresh holder per message).
    pool = _make_holder_pool(concurrency, cap)
    # Stage 2 — worker. A plain `@spawn` (NOT sticky): its codegen GC completes while
    # the receiver sits parked in the (GC-safe, yielding `@threadcall`) recv (D8).
    # Drains the buffer in order (so `Serial()` order is preserved) via the
    # world-age trampoline (`_dynamic_worker`), which hoists the `invokelatest`
    # from per-message to per-type. Runs even under `-t1` — the recv yields, so the
    # scheduler reaches the worker; `-t>=2` adds true overlap.
    worker = Threads.@spawn try
        # D7: node logger installed once for the worker (and inherited by the handler
        # tasks the scheduler spawns), so a dynamic-sub handler's `@info` routes to
        # /rosout — without a per-message `with_logger` scope.
        _with_node_logger(e.node) do
            _dynamic_worker(e, buf, handler, sched, view, pool, logged, loglk, warmed, warmlk, warmup)
        end
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "dynamic subscription worker task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end

    # Stage 1 — receiver. `recv!`s each sample into a fresh owned `SampleHolder`
    # (moved out of the ring slot, so the ring isn't pinned — D1) and enqueues it.
    # No payload copy and no keyexpr String: the holder carries the refcounted
    # payload *and* keyexpr to the worker, which decodes + resolves the type by
    # borrowing them. Stays light, so it keeps the
    # ring drained while the worker codegens (D8). recv! returns nothing on close →
    # loop ends; `finally` closes the buffer so the worker drains-then-exits.
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
                _note_lost(e, h)                              # §12.3, only when a listener is registered
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
    # §D9 — Tier-1 startup warm: replay this node's interaction manifest for this
    # topic, warming the codecs of types it used on prior runs *before* the first
    # message arrives (shifting the JIT off the hot path). Runs concurrently with the
    # receiver/worker; the shared `warmlk`-guarded `warmed` set makes the two paths
    # idempotent (whichever warms a type first, the other skips it). Skipped when
    # warm-up is off.
    warmup.mode === :off ||
        _spawn_manifest_warm(e, handler, warmed, warmlk, warmup)
    # `worker` needs no explicit handle: the scheduler roots it while it runs, and the
    # receiver's `finally close(buf)` (on route close) ends its drain loop cleanly.
    return t
end

# §D9 — schedule the Tier-1 manifest replay. `sync` runs it inline at construction
# (hard real-time: first message guaranteed warm); otherwise a plain `Threads.@spawn`
# (NOT sticky) — in cache-only mode the replay's `resolve_or_discover` may run
# `realize!`→`Core.eval` codegen, whose GC must complete while the node's other
# consumers keep safepoints reachable (the JIT-races-Zenoh discipline the dynamic
# worker also follows, §D8). With `@ros_cache` baked, the resolve is a registry hit
# and no codegen runs.
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

# Resolve + warm each manifest entry for this subscription's topic. `wire=false`: a
# node coming up must not block on a remote `~/get_type_description` — a genuinely-new
# type still pays one reactive wire fetch on its first message, by design (§D9). A
# stale entry whose type no longer resolves (cache evicted, version bump) returns
# `nothing` and is skipped. Crosses to the runtime-born type's warm anchor via
# `invokelatest` (newer world age), exactly as the worker does.
function _replay_manifest_warm(e::Entity, handler, warmed, warmlk)
    for it in load_manifest(e.node.fqn)
        it.role === :subscription || continue
        it.topic == e.endpoint.topic || continue
        T = _resolve_home(e.node, it.hash)                             # D10B S2: home table first
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

# World-age trampoline for the dynamic worker. The per-message hot path must NOT
# cross the world-age boundary (you can't specialize through `invokelatest`), so
# instead of `invokelatest`-ing every message we hoist the crossing to once per
# *type*: `_dynamic_worker` runs a flat loop of `invokelatest` entries into
# `_drain_known`, which dispatches every already-seen type with a plain (cached)
# dynamic call — no `invokelatest` — and only returns when it meets a *new* type
# (whose `realize!` may have bumped the world). The outer loop then re-enters in
# the fresher world. Steady state = one long-lived `_drain_known` whose inner
# loop never returns ⇒ zero per-message `invokelatest`; the crossing count is
# ~one per distinct type (+ the once-per-type `_warm_dynamic`).
#
# Invariant: a key in `cache` was first seen in a prior incarnation that
# re-entered past its `realize!` world; worlds are monotonic, so it is ≤ this
# incarnation's world ⇒ `_run_typed_dynamic{T}` is directly callable. A single
# worker drains `buf` in order across re-entries, so `Serial` total order holds.
# `realize!` still runs here (off the receiver) so the FIFO keeps draining (D8).

# Incarnation counter — bumped once per `_drain_known` entry (i.e. once per
# re-entry), so ≈ distinct-types + 1, never per message. Observability + a test
# hook for "steady-state dispatch isn't re-`invokelatest`ing"; the increment is
# off the per-message path, so it costs nothing hot.
const _DYNAMIC_REENTRIES = Threads.Atomic{Int}(0)

# Keyexpr cache key: a hash of the borrowed keyexpr bytes, so the hot path never
# allocates a `String` (§3 alloc cleanup). FNV-1a over the raw bytes — allocation-
# free, no `unsafe_wrap`. Collisions are resolved by a `memcmp` against the stored
# bytes (`_ke_bytes_eq`), so the hash needn't be cryptographic, only well-spread.
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

# D10B S2: resolve a wire hash against the Context's `home` module's baked
# `__ros_resolve__` table (per-module, deterministic). `nothing` when there is no home,
# or the home's dependency closure never imported this type — the caller then falls to
# `resolve_or_discover` (content-canonical / cache / wire / realize-on-miss).
function _resolve_home(node, hash)
    home = _ctx(node).home
    home === nothing && return nothing
    return resolve_in_home(home, Symbol(to_rihs_string(hash)))
end

function _dynamic_worker(e::Entity, buf, handler, sched, view::ViewMode, pool,
                         logged, loglk, warmed, warmlk, warmup::WarmupPolicy)
    # hash(keyexpr bytes) → (resolved type, keyexpr bytes for memcmp verify);
    # worker-private (no lock). The byte copy is the collision-safety net.
    cache   = Dict{UInt, Tuple{Type, Vector{UInt8}}}()
    pending = nothing                     # just-realized item to dispatch first in the fresh world
    while true
        pending = Base.invokelatest(_drain_known, e, buf, handler, sched, view, pool, cache,
                                    logged, loglk, warmed, warmlk, warmup, pending)::Union{Symbol, Nothing, Tuple{SampleHolder, Type}}
        pending === :closed && break
    end
    nothing
end

function _drain_known(e::Entity, buf, handler, sched, view::ViewMode, pool,
                      cache::Dict{UInt, Tuple{Type, Vector{UInt8}}},
                      logged, loglk, warmed, warmlk, warmup::WarmupPolicy, pending)
    Threads.atomic_add!(_DYNAMIC_REENTRIES, 1)
    if pending !== nothing
        (h, T) = pending
        _run_typed_dynamic(T, e, h, handler, sched, view, pool) # direct: realized ≤ our world
    end
    for h in buf
        # Resolve the cached type by borrowing the keyexpr bytes (no String): hash
        # them, look up, and `memcmp`-verify against the stored copy so a hash
        # collision can't mis-dispatch. Returns the cached `T` or `nothing`.
        T = Zenoh.keyexpr_view(h) do ptr, len
            entry = get(cache, _ke_fnv1a(ptr, len), nothing)
            (entry !== nothing && _ke_bytes_eq(ptr, len, entry[2])) ? entry[1] : nothing
        end
        if T !== nothing
            _run_typed_dynamic(T, e, h, handler, sched, view, pool)  # HOT PATH — direct, no invokelatest
            continue
        end
        # First sight of this keyexpr by this worker: materialize the String (cold,
        # once per type) and resolve (the first sample of a new type pays discovery +
        # codegen here, off the receiver, D8 — HOL-blocking this sub only). Then hand
        # the item back so the outer loop re-enters in the post-`realize!` world, where
        # `T` is directly callable for every later message.
        ke = string(Zenoh.keyexpr(h))
        try
            info = _sample_type_info(e, ke)
            if info === nothing
                pool === nothing || put!(pool, h)              # not dispatched → recycle the holder
                continue
            end
            Tnew = _resolve_home(e.node, info.hash)                    # D10B S2: home table first
            source = :home                                            # a-priori: the Context's `home` module
            if Tnew === nothing
                Tnew = resolve_or_discover(e.node, info.name, info.hash)
                # Tag the resolution source for the hint: a resolved entry's provenance —
                # `:static`/`:cache`/`:ament` mean we already knew the type locally; `:wire`
                # means we fetched + generated it from the publisher's ~/get_type_description.
                ent = Tnew === nothing ? nothing : lookup_type(registry(_ctx(e.node)), info)
                source = ent === nothing ? :wire : ent.provenance
            end
            if Tnew === nothing
                @warn "dynamic subscription: could not resolve type" topic=e.endpoint.topic type=info.name hash=to_rihs_string(info.hash) maxlog=1
                pool === nothing || put!(pool, h)              # not dispatched → recycle the holder
                continue                                       # unresolved: skip, no cache, no re-entry
            end
            # Key on the same FNV-1a the hot path computes; stash the bytes for memcmp.
            kebytes = Vector{UInt8}(codeunits(ke))
            cache[GC.@preserve kebytes _ke_fnv1a(pointer(kebytes), length(kebytes))] = (Tnew, kebytes)
            _log_discovered_once(e, info, source, logged, loglk)
            # §D8/§D9: warm this runtime type's codec once (off the recv thread); `T`
            # is runtime-born so reach the warm anchor via `invokelatest`. `warmed` is
            # shared with the Tier-1 replay task, so the once-guard is under `warmlk`.
            if warmup.mode !== :off
                isnew = @lock warmlk (Tnew in warmed ? false : (push!(warmed, Tnew); true))
                if isnew
                    note_interaction!(e.node.fqn, :subscription, info.hash, info.name, e.endpoint.topic)
                    try
                        Base.invokelatest(_warm_dynamic, Tnew, handler)
                    catch err
                        @warn "dynamic warm-up failed (ignored)" topic=e.endpoint.topic exception=(err, catch_backtrace())
                    end
                end
            end
            return (h, Tnew)                # hand off; outer loop re-enters in the fresh world
        catch err
            err isa ShutdownException && rethrow()
            @error "dynamic subscription resolve failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
            pool === nothing || put!(pool, h)                  # not dispatched → recycle the holder
            continue
        end
    end
    return :closed
end

# The new-world worker (reached only via `invokelatest`, since `T` is runtime-born):
# dispatch the decode+handler per the concurrency policy. Because we are already in
# the latest world here, the `@spawn` inside `sched` (Parallel) captures it, so the
# spawned task runs `handler(decode(bytes, T))` natively — the whole hot loop body
# is compiled new-world code, no further `invokelatest`.
function _run_typed_dynamic(::Type{T}, e::Entity, h::SampleHolder,
                            handler, sched, view::ViewMode, pool) where {T}
    # Dispatch inside the sched thunk, which captures the owned holder `h` — so under
    # `Parallel` the spawned worker keeps the payload alive while the shared leaf
    # borrows + decodes it. The thunk returns `h` to the pool when done (after the
    # deferred decode under Parallel), recycling it for the next receive. The
    # decode→dispatch itself is the same `_dispatch_decoded` leaf the static consumer
    # runs — one implementation, no static/dynamic drift.
    sched() do
        try
            _dispatch_decoded(e, h, T, handler, view)
        finally
            pool === nothing || put!(pool, h)
        end
    end
    nothing
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

# Log a discovered (name, hash) once per subscription (§S6): tell the user what to
# write to graduate to the static fast path.
function _log_discovered_once(e::Entity, info::TypeInfo, source::Symbol, logged, loglk)
    isnew = @lock loglk (info.name, info.hash) in logged ? false :
                        (push!(logged, (info.name, info.hash)); true)
    isnew || return nothing
    bare = split(info.name, '/')[end]
    graduate = "Subscription(node, \"$(e.endpoint.topic)\", $(bare))"
    rihs = to_rihs_string(info.hash)
    if source === :wire
        # No local definition — we fetched the TypeDescription from the publisher and
        # generated the struct at runtime (distinct from resolving a type we already knew).
        @info "dynamic subscription auto-generated a type over the wire — fetched its \
               definition from the publisher's ~/get_type_description (no local copy); \
               register it (e.g. `@ros_import`) to skip wire discovery, or use the static \
               form for the min-copy fast path" topic=e.endpoint.topic type=info.name hash=rihs graduate=graduate
    else
        # Resolved a type we already knew (home/registry/cache/ament) — no wire trip.
        @info "dynamic subscription resolved a known type — for the min-copy fast path, \
               use the static form" topic=e.endpoint.topic type=info.name hash=rihs source=source graduate=graduate
    end
    nothing
end

