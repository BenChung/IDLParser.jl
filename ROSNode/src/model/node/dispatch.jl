# ── subscription dispatch runtime ─────────────────────────────────────────────
# The consumer task iterates the FIFO subscriber (blocking on the libuv thread until
# a sample arrives or the channel disconnects on close) and runs the handler per the
# concurrency policy. `Serial()` runs inline on the sticky consumer task — one at a
# time, order preserved, single OS thread; `Parallel(n)` spawns up to `n` handlers on
# OS threads. A handler throw is logged, never fatal: one bad message must not kill
# the subscription.

# Per-sample gating before delivery; returns whether to deliver. This is the
# skip-delivery point of the lifecycle gate owned by [`isactive`](@ref): an inactive
# node drops the sample with no novelty side effect, so the delivered stream reflects
# only what the handler saw. Then applies the novelty gate (suppress re-latch
# replays). Volatile subs pass `gate === nothing` and take the fast path that skips
# the attachment decode.
@inline function _predispatch(e::Entity, sample::AbstractSample, gate::Union{_NoveltyGate, Nothing})
    isactive(e) || return false               # lifecycle gate: drop while inactive (no novelty record)
    # Weak-static subs (wildcard-matched) run `check_sample_type`, the discovery
    # layer's per-sample type backstop: an off-type sample is decode-unsafe, so drop
    # it before decode. Mismatch reports are deduped/throttled
    # against the graph detector via `report_type_mismatch!`.
    if e._weak_static
        tm = check_sample_type(e, sample)
        tm === nothing || (report_type_mismatch!(e.node.context, tm); return false)
    end
    gate === nothing && return true           # volatile fast path: no novelty work
    return _gate_deliver!(gate, _payload_hash(sample))   # suppress re-latch replays
end

# `sub` is the concretely-typed route (plain `SubscriberHandler` or the advanced
# `AdvancedSubscriberHandler`); the consumer closes over the typed local and
# iterates it, keeping dispatch monomorphic whichever the QoS selected. Passing
# `sub::S` rather than the erased `e._route::Any` recovers the concrete type the
# field can't, so iteration avoids boxing every sample.
# `T` is a type parameter (not a `::Type` value) so the per-message decode/dispatch
# specializes: `decode(sample, T)` is return-type-stable, the decoded message stays
# concrete, and the handler call is concrete. `declare_subscription!` resolves `T`
# with one dynamic dispatch per subscription; the per-message path is monomorphic.
function _spawn_consumer(e::Entity, ::Type{T}, handler, view::ViewMode,
                         concurrency::Concurrency, sub::S,
                         gate::Union{_NoveltyGate, Nothing}) where {T, S<:Zenoh.AbstractSubscriberHandler}
    # Persistent worker pool, one task spawn per subscription rather than per
    # message. Each worker owns a reusable `SampleHolder` and runs its own
    # recv!→dispatch loop, pulling concurrently from `sub` (recv! is
    # mutex-serialized). Returns one task handle for `e._consumer`, which the
    # re-latch `wait`s on; workers exit when the subscriber closes (recv! → nothing).
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

# The single decode→dispatch leaf, shared by the static consumer (`_consume_loop`
# and the `Parallel(Inf)` view path) and the dynamic worker (wrapped per-type in the
# `FunctionWrapper` the worker builds — see `_mk_fw`, dynamic.jl).
# Given a concrete `T`, a live `sample`/holder, the handler, and the `ViewMode`, it
# borrows the payload and runs the handler on the decoded message. One
# implementation keeps the static and dynamic paths in sync. Type-stable: `T` is
# concrete and `decode_view`/`decode_owned` infer concrete (the `CDR_LE` reader).
# A handler/decode throw is logged and swallowed so one bad message never kills the
# subscription; `ShutdownException` ends the dispatch quietly.
#
# The node logger is installed once per consumer/worker task and inherited by every
# handler call (and any task they spawn), so a plain `@info` inside the handler
# routes to the node's /rosout. Installing once per task keeps the `Unchecked()`
# zero-alloc tier intact, which a per-message `with_logger` scope would break.
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
# static per subscription. All borrow the payload directly and run the handler
# within the borrow scope.
#   Owned     — materialize a self-contained `T` from the borrowed bytes (safe to
#               keep, forward, or spawn).
#   Checked   — zero-copy `CDRView` plus a runtime escape guard: a `CDRView`/`CDRString`
#               used after the handler returns throws `BorrowError` rather than
#               reading freed memory. Allocates the guard — the validation tier.
#   Unchecked — bare isbits `PayloadView`: zero-copy and zero-alloc, trusting the
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

# Owned decode on the consumer task, returning the materialized message (or
# `nothing` if decode threw — logged and skipped). Used only by the `Parallel(Inf)`
# owned path: it decodes inline over its reused holder while the holder is still
# live, then spawns only the handler on the owned result. Materializes straight
# from the borrowed bytes.
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
        # The node logger, inherited from the spawning consumer task, is active here,
        # so a plain `@info` inside the handler routes to the node's /rosout.
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
# for the whole dispatch). The novelty gate is `ReentrantLock`-guarded, so
# concurrent delivery is safe. Decode runs on the worker, so with N workers a slow
# handler in one keeps decoding in the others.
function _consume_loop(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    holder = SampleHolder()
    try
        # Install the node logger once for the whole task; handlers and any task they
        # spawn inherit it, so a plain `@info` inside the handler routes to the node's
        # /rosout. Installing once keeps the zero-alloc tier (a per-message
        # `with_logger` scope allocates).
        _with_node_logger(e.node) do
            while (sample = recv!(sub, holder)) !== nothing
                _note_lost(e, sample)             # only when a listener is registered
                _predispatch(e, sample, gate) || continue
                # Dispatch inline while the holder is live; both view and owned go
                # through the one shared leaf.
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

# Message-lost feed: report attachment-sequence gaps to `on_message_lost`
# listeners. A per-entity flag gates the work so the common no-listener path is one
# atomic load; only a sub with a registered listener pays the per-sample
# `note_sequence!` decode.
@inline _note_lost(e::Entity, sample::AbstractSample) =
    (@atomic e._track_lost) && (note_sequence!(e, sample); nothing)

# `Parallel(Inf)`: unbounded concurrency uses a spawn-per-message model, paying a
# per-message task-launch cost in exchange for no concurrency ceiling. A single
# receiver pulls and spawns a handler per message. The view path receives into a
# fresh holder the spawned worker owns until GC (zero-copy); the owned path decodes
# inline and spawns on the message.
function _spawn_unbounded_consumer(e::Entity, ::Type{T}, handler, view::ViewMode, sub::S, gate) where {T, S<:Zenoh.AbstractSubscriberHandler}
    reused = SampleHolder()
    t = Task() do
        try
            # Node logger installed once for the receiver task; spawned handler tasks
            # inherit it, so handler `@info` routes to /rosout at zero per-message cost.
            _with_node_logger(e.node) do
                while true
                    h = _is_view(view) ? SampleHolder() : reused
                    recv!(sub, h) === nothing && break
                    _note_lost(e, h)              # only when a listener is registered
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

