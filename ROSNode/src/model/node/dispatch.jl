# ── subscription dispatch runtime ─────────────────────────────────────────────
# Each subscription owns its consumer task; Julia's scheduler runs it. The task
# iterates the FIFO subscriber and runs the handler per the concurrency policy.

# Per-sample lifecycle and novelty gating; returns whether to deliver. The lifecycle
# gate drops a sample on an inactive node before any novelty side effect, so the
# delivered stream reflects only what the handler saw. A weak-static sub then runs
# the per-sample type backstop, and a transient_local sub the novelty gate; volatile
# subs pass `gate === nothing` and skip the attachment decode.
@inline function _predispatch(e::Entity, sample::AbstractSample, gate::Union{_NoveltyGate, Nothing})
    isactive(e) || return false               # lifecycle gate: drop while inactive, record nothing
    # An off-type sample on a wildcard-matched sub is decode-unsafe; drop it before
    # decode, mismatch report deduped against the graph detector.
    if e._weak_static
        tm = check_sample_type(e, sample)
        tm === nothing || (report_type_mismatch!(e.node.context, tm); return false)
    end
    gate === nothing && return true           # volatile fast path: no novelty work
    return _gate_deliver!(gate, _payload_hash(sample))   # suppress re-latch replays
end

# Recover the route's concrete type via `sub::S` and the message type via the `T`
# parameter (not a `::Type` value), so iteration and per-message decode/dispatch
# stay monomorphic — `e._route::Any` and a `::Type` value would box every sample.
# `declare_subscription!` resolves both with one dynamic dispatch per subscription.
function _spawn_consumer(e::Entity, ::Type{T}, handler, view::ViewMode,
                         concurrency::Concurrency, sub::S,
                         gate::Union{_NoveltyGate, Nothing}) where {T, S<:Zenoh.AbstractSubscriberHandler}
    # Persistent worker pool, one task spawn per subscription. Each worker owns a
    # reusable `SampleHolder` and runs its own recv!→dispatch loop, pulling
    # concurrently from `sub` (recv! is mutex-serialized). Returns one task handle
    # for `e._consumer`, which the re-latch joins; workers exit when the subscriber
    # closes (recv! → nothing).
    if concurrency isa Serial
        # One sticky worker: ordered, pinned to the declaring thread so a node's
        # Serial handlers share one OS thread, freeing handlers from locking.
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

# The single decode→dispatch leaf, shared by the static consumer and the dynamic
# worker (wrapped per-type in the `FunctionWrapper` `_mk_fw` builds). Borrows the
# payload and runs the handler on the decoded message. Monomorphic: `T` is concrete
# and `decode_view`/`decode_owned` infer one concrete return type.
# A handler or decode throw is logged and swallowed so one bad message never kills
# the subscription; only `ShutdownException` propagates.
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

# Delivery strategies, dispatched on the singleton `ViewMode` so the choice is static
# per subscription. All borrow the payload directly and run the handler within the
# borrow scope.
#   Owned     — materialize a self-contained `T` from the borrowed bytes, safe to
#               keep, forward, or spawn.
#   Checked   — zero-copy `CDRView` plus a runtime escape guard that throws
#               `BorrowError` on a view used after the handler returns. Allocates the
#               guard — the validation tier.
#   Unchecked — bare isbits `PayloadView`: zero-copy and zero-alloc, requiring the
#               handler to confine the view to the borrow scope (an escaping view is
#               UB). The production tier; validate with `Checked`.
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

# Owned decode on the consumer task, returning the materialized message (`nothing`
# if decode threw — logged and skipped). The `Parallel(Inf)` owned path decodes
# inline over its reused holder while the holder is live, then spawns only the
# handler on the owned result.
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
# fatal. The `Parallel(Inf)` owned path spawns this after an inline
# `_decode_on_consumer`.
function _invoke_owned(e::Entity, msg, handler)
    try
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
# sample, and the view path borrows the worker's own live holder (zero-copy, valid
# for the whole dispatch). The novelty gate is `ReentrantLock`-guarded. Decode runs
# on the worker, so with N workers a slow handler in one keeps decoding in the others.
function _consume_loop(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    holder = SampleHolder()
    try
        # Node logger installed once per consumer task and inherited by every handler
        # call and task it spawns; a per-message `with_logger` scope would allocate and
        # break the `Unchecked` zero-alloc tier.
        _with_node_logger(e.node) do
            while (sample = recv!(sub, holder)) !== nothing
                _note_lost(e, sample)
                _predispatch(e, sample, gate) || continue
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

# Report attachment-sequence gaps to `on_message_lost` listeners. Gap detection is
# opt-in: the per-entity flag makes the common no-listener path one atomic load, and
# only a sub with a registered listener pays the per-sample `note_sequence!` decode.
@inline _note_lost(e::Entity, sample::AbstractSample) =
    (@atomic e._track_lost) && (note_sequence!(e, sample); nothing)

# `Parallel(Inf)`: a single receiver spawns a handler per message — a per-message
# task-launch cost for no concurrency ceiling. The view path receives into a fresh
# holder the spawned worker owns until GC; the owned path decodes inline and spawns
# on the message.
function _spawn_unbounded_consumer(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    reused = SampleHolder()
    t = Task() do
        try
            # Node logger installed once per receiver task (see `_consume_loop`);
            # spawned handler tasks inherit it.
            _with_node_logger(e.node) do
                while true
                    h = _is_view(view) ? SampleHolder() : reused
                    recv!(sub, h) === nothing && break
                    _note_lost(e, h)
                    _predispatch(e, h, gate) || continue
                    if _is_view(view)
                        # Fresh holder per message → the spawned task owns it and can
                        # borrow it for the full dispatch via the shared leaf.
                        Threads.@spawn _dispatch_decoded(e, h, T, handler, view)
                    else
                        # Owned decodes inline over the reused holder while it is
                        # still live, then spawns only the handler on the owned result.
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

