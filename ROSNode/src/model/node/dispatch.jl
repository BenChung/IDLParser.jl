# ── subscription dispatch runtime (§4) ────────────────────────────────────────
# The consumer task iterates the FIFO subscriber (blocking on the libuv thread until
# a sample arrives or the channel disconnects on close) and runs the handler per the
# concurrency policy. `Serial()` runs inline on the sticky consumer task — one at a
# time, order preserved, single OS thread; `Parallel(n)` spawns up to `n` handlers on
# OS threads. A handler throw is logged, never fatal: one bad message must not kill
# the subscription.

# Per-sample gating before delivery: the §14.2 lifecycle gate (drop while the node
# is inactive — no novelty side effect, so the delivered stream reflects only what
# the handler saw) and the D4 novelty gate (suppress re-latch replays). Returns
# whether to deliver. Volatile subs pass `gate === nothing` and skip the attachment
# decode entirely — the common fast path.
@inline function _predispatch(e::Entity, sample::AbstractSample, gate::Union{_NoveltyGate, Nothing})
    isactive(e) || return false               # §14.2 gate: drop while inactive (no record)
    gate === nothing && return true           # volatile fast path: no novelty work
    return _gate_deliver!(gate, _payload_hash(sample))   # D4: suppress re-latch replays
end

# `sub` is the concretely-typed route (plain `SubscriberHandler` or the advanced
# `AdvancedSubscriberHandler`); the consumer closes over it and iterates the typed
# local, so dispatch stays monomorphic regardless of which the QoS selected. (We do
# NOT read `e._route::Any` here — an abstract-typed iteration would box every sample;
# the bounded `S` recovers the type the erased field can't.)
# `T` is threaded as a type parameter (not a `::Type` value) so the per-message
# decode/dispatch specializes: `decode(sample, T)` is return-type-stable, the
# decoded message isn't boxed as `Any`, and the handler call is concrete. The
# call from `declare_subscription!` (where `msgtype::Type` is a value) resolves
# `T` with one dynamic dispatch per *subscription* — the per-*message* path
# inside the workers is monomorphic.
function _spawn_consumer(e::Entity, ::Type{T}, handler, view::ViewMode,
                         concurrency::Concurrency, sub::S,
                         gate::Union{_NoveltyGate, Nothing}) where {T, S<:Zenoh.AbstractSubscriberHandler}
    # Persistent worker pool — no per-message task spawn. Each worker owns a
    # reusable `SampleHolder` and runs its own recv!→dispatch loop, pulling
    # concurrently from `sub` (recv! is mutex-serialized). Returns one task handle
    # for `e._consumer` (the D4 re-latch `wait`s on it); workers exit when the
    # subscriber closes (recv! → nothing).
    if concurrency isa Serial
        # One sticky worker: ordered, pinned to the declaring thread (D3) so a
        # node's Serial handlers share one OS thread — no races, no user locks.
        w = Task(() -> _consume_loop(e, T, handler, view, sub, gate))
        w.sticky = true
        schedule(w)
        return w
    elseif concurrency isa Parallel && !isfinite(concurrency.n)
        return _spawn_unbounded_consumer(e, T, handler, view, sub, gate)
    end
    # Parallel(n): n non-sticky workers (run on OS threads, order not preserved),
    # wrapped by a sticky supervisor that joins them so `wait(e._consumer)`
    # transitively waits for every worker on close / re-latch.
    nworkers = Int(concurrency.n)
    sup = Task() do
        workers = map(1:nworkers) do _
            w = Task(() -> _consume_loop(e, T, handler, view, sub, gate))
            w.sticky = false
            schedule(w)
            w
        end
        for w in workers
            wait(w)
        end
    end
    sup.sticky = true
    schedule(sup)
    return sup
end

# The single decode→dispatch leaf, shared by the static consumer (`_consume_loop`
# and the `Parallel(Inf)` *view* path) and the dynamic worker (`_run_typed_dynamic`).
# Given a concrete `T`, a live `sample`/holder, the handler, and the `ViewMode`, it
# borrows the payload and runs the handler on the decoded message — ONE
# implementation, so the static and dynamic paths can't drift. Type-stable: `T` is
# concrete and `decode_view`/`decode_owned` infer concrete (the `CDR_LE` reader).
# A handler/decode throw is logged and swallowed so one bad message never kills the
# subscription; `ShutdownException` ends the dispatch quietly.
#
# D7: the node logger is installed once per consumer/worker task (`_consume_loop` /
# the `Parallel(Inf)` task / the dynamic worker) and inherited by every handler call
# (and any task they spawn), so a plain `@info` inside the handler routes to the
# node's /rosout — without the per-message `with_logger` scope (which allocates
# ~270 B/call, measured GC-off on 1.11/1.12, breaking the `Unchecked()` zero-alloc tier).
@inline function _dispatch_decoded(e::Entity, sample::AbstractSample, ::Type{T},
                                   handler, view::ViewMode) where {T}
    try
        _run(sample, T, handler, view)
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# Delivery strategies, dispatched on the (singleton) `ViewMode` so the choice is
# static per subscription. All borrow the payload (no pre-copy) and run the handler
# within the borrow scope (§3.2).
#   Owned     — materialize a self-contained `T` from the borrowed bytes (safe to
#               keep / forward / spawn).
#   Checked   — zero-copy `CDRView` + runtime escape guard: a `CDRView`/`CDRString`
#               used after the handler returns throws `BorrowError` instead of
#               reading freed memory. Allocates the guard — the validation tier.
#   Unchecked — bare isbits `PayloadView`: zero-copy, zero-alloc, no checks (an
#               escaping view is UB). The production tier; validate with `Checked`.
@inline _run(sample, ::Type{T}, handler, ::Owned) where {T} =
    with_payload_memory(sample) do mem
        handler(decode_owned(mem, T))
    end
@inline _run(sample, ::Type{T}, handler, ::Checked) where {T} =
    with_payload_memory_checked(sample) do mem
        handler(decode_view(mem, T))
    end
@inline _run(sample, ::Type{T}, handler, ::Unchecked) where {T} =
    with_payload_memory(sample) do mem
        handler(decode_view(mem, T))
    end

# Owned decode on the consumer task, returning the materialized message (or
# `nothing` if decode threw — logged and skipped). Used only by the `Parallel(Inf)`
# owned path, which must decode *inline* over its reused holder (before the next
# `recv!` overwrites it) and then spawn only the handler on the owned result.
# Borrow-based: materializes straight from the borrowed bytes, no payload pre-copy.
function _decode_on_consumer(e::Entity, sample::AbstractSample, ::Type{T}) where {T}
    try
        return with_payload_memory(sample) do mem
            decode_owned(mem, T)
        end
    catch err
        err isa ShutdownException && rethrow()
        @error "subscription decode failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        return nothing
    end
end

# Run the handler on an already-decoded owned message; a throw is logged, never
# fatal (one bad message must not kill the subscription). The `Parallel(Inf)` owned
# path spawns this after an inline `_decode_on_consumer`.
function _invoke_owned(e::Entity, msg, handler)
    try
        # D7: the node logger is the active logger here — set once on the spawning
        # consumer task and inherited by this spawned handler task — so a plain `@info`
        # inside the handler routes to the node's /rosout.
        handler(msg)
    catch err
        err isa ShutdownException && return
        @error "subscription handler threw" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# One worker's receive loop. Owns a `SampleHolder` and runs the full
# recv!→gate→decode→dispatch pipeline itself. N of these pull concurrently from
# the same subscriber — `recv!` is `uv_mutex`-serialized, so each gets a distinct
# sample, and the view path borrows the worker's *own* live holder (zero-copy,
# valid for the whole dispatch). The novelty gate is `ReentrantLock`-guarded, so
# concurrent delivery is safe. Decode runs here, on the worker (D1): with N
# workers a slow handler in one never stalls another's decode.
function _consume_loop(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    holder = SampleHolder()
    try
        # D7: install the node logger once for the whole task (not per message — that
        # `with_logger` scope allocates); handlers and any task they spawn inherit it,
        # so a plain `@info` inside the handler still routes to the node's /rosout.
        _with_node_logger(e.node) do
            while (sample = recv!(sub, holder)) !== nothing
                _note_lost(e, sample)             # §12.3, only when a listener is registered
                _predispatch(e, sample, gate) || continue
                # Inline (the holder is live for the whole dispatch) — both view and
                # owned go through the one shared leaf.
                _dispatch_decoded(e, sample, T, handler, view)
            end
        end
    catch err
        err isa ShutdownException && return
        isopen(e) &&
            @error "subscription consumer task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
    end
    nothing
end

# §12.3 message-lost feed: report attachment-sequence gaps to `on_message_lost`
# listeners. Gated on a per-entity flag so the common no-listener path is one atomic
# load (no attachment decode, no alloc); only a sub with a registered listener pays
# the per-sample `note_sequence!` decode.
@inline _note_lost(e::Entity, sample::AbstractSample) =
    (@atomic e._track_lost) && (note_sequence!(e, sample); nothing)

# `Parallel(Inf)`: unbounded concurrency can't be a fixed pool, so keep the
# spawn-per-message model (its per-message task-launch cost is the honest price
# of "no ceiling"). A single receiver pulls and spawns a handler per message;
# the view path receives into a *fresh* holder the spawned worker owns until GC
# (still zero-copy), the owned path decodes inline and spawns on the message.
function _spawn_unbounded_consumer(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    reused = SampleHolder()
    t = Task() do
        try
            # D7: node logger installed once for the receiver task; spawned handler
            # tasks inherit it (so handler `@info` routes to /rosout) without a
            # per-message `with_logger` scope.
            _with_node_logger(e.node) do
                while true
                    h = _is_view(view) ? SampleHolder() : reused
                    recv!(sub, h) === nothing && break
                    _note_lost(e, h)              # §12.3, only when a listener is registered
                    _predispatch(e, h, gate) || continue
                    if _is_view(view)
                        # Fresh holder per message → the spawned task owns it and can
                        # borrow it for the full dispatch via the shared leaf.
                        Threads.@spawn _dispatch_decoded(e, h, T, handler, view)
                    else
                        # Owned must decode INLINE over the reused holder (before the
                        # next recv! overwrites it); spawn only the handler on the owned
                        # result.
                        msg = _decode_on_consumer(e, h, T)
                        msg === nothing && continue
                        Threads.@spawn _invoke_owned(e, msg, handler)
                    end
                end
            end
        catch err
            err isa ShutdownException && return
            isopen(e) &&
                @error "subscription consumer task failed" topic=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    t.sticky = true
    schedule(t)
    return t
end

