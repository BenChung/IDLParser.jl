# §D8 — precompilation / warm-up. The dispatch chain (foreign thread → Zenoh.jl
# trampoline → consumer task → decode(sample,T) → handler(::T) → user callees) is
# specialized on the message type, so the *first* publish/handler-on-`T` JITs the
# whole chain — a startup spike, or worse a mid-operation stall on the first
# message of a topic. We hold every concrete type at construction (`T`, the route
# type `R`, the stored handler closure), so we anchor warm-up at the innermost
# type-specialized frame here.
#
# Two depths (the `warmup` knob, a `WarmupPolicy` in core.jl):
#   • :precompile (default, safe) — `precompile(...)`-anchor the chain. Caches the
#     whole reachable inference tree (the expensive part) + codegens the named
#     frame and its inlined callees, side-effect-free, no message instance needed.
#     Residual: native codegen of deep *non-inlined* user callees defers to first
#     real message (you can't compile-to-depth code you never run).
#   • :execute (opt-in) — additionally run the handler once on a sample message,
#     under the `_WARMUP` scope so `is_warming()` is true: non-ROS effects skip via
#     `@effectful` (still compiled), outbound ROS ops (`publish`/`call`) null-route
#     just before the wire op. Reaches full native depth incl. the handler body.
#
# `sync=false` (default) warms on a background task (zero construction latency);
# `sync=true` blocks the constructor until warm (first message guaranteed compiled).

using PrecompileTools: @setup_workload, @compile_workload
# Fixed primitive arrays (`float[3]`) generate as `SVector`; detect them for the
# default-message builder. (StaticArrays is a ROSNode dep; typesupport.jl `using`s
# it module-wide, but that file is included after this one — name it explicitly.)
using StaticArrays: StaticArray

# ── the dispatcher ─────────────────────────────────────────────────────────────
# `warm` is a closure that does the precompile anchors (always) + the optional
# :execute run; `_warmup!` only decides off / inline (sync) / background (async).
# A background warm roots itself on the scheduler while it runs; it touches only
# already-constructed, type-stable state, so it races nothing.
function _warmup!(policy::WarmupPolicy, warm)
    policy.mode === :off && return nothing
    if policy.sync
        warm()
    else
        Threads.@spawn warm()
    end
    return nothing
end

# An entity's effective policy: an explicit `warmup`/`warmup_sync` kwarg overrides
# the node default (core.jl `WarmupPolicy`); `nothing` inherits it.
function _resolve_warmup(node::Node, mode::Union{Symbol, Nothing}, sync::Union{Bool, Nothing})
    mode === nothing && sync === nothing && return node.warmup
    WarmupPolicy(mode === nothing ? node.warmup.mode : mode,
                 sync === nothing ? node.warmup.sync : sync)
end

# Run a handler purely to warm it: under the `_WARMUP` scope (so `is_warming()` is
# true on this task and any child task it spawns) and swallowing throws — a warm-up
# failure must never be fatal, and a handler written for live data may not tolerate
# a synthesized sample. Logged at `warn` (not `error`): it's expected-ish. `thunk`
# is first so the `_run_warm(topic) do … end` call sites bind the do-block to it.
function _run_warm(thunk, topic)
    try
        Base.ScopedValues.with(thunk, _WARMUP => true)
    catch err
        err isa ShutdownException && return
        @warn "warmup :execute handler threw (ignored — code path still compiled)" topic=topic exception=(err, catch_backtrace())
    end
    nothing
end

# The sample message for an :execute warm-up: a user-supplied representative one
# (realism the zero-builder can't synthesize) or the default-built fallback.
_sample_msg(::Type{T}, sample::T) where {T} = sample
_sample_msg(::Type{T}, ::Nothing) where {T} = _default_msg(T)
_sample_msg(::Type{T}, sample) where {T} =
    throw(ArgumentError("warmup_sample must be a $(T) (got a $(typeof(sample)))"))

# ── default message construction ───────────────────────────────────────────────
# Generated message structs (both the `@cdr_fixed` and `@kwdef` forms) keep a
# positional inner constructor, so a value is built field-by-field from each
# field's *exact* type (`fieldtypes`). Used for the :execute fallback and the
# package `@compile_workload`. Recurses into nested message structs.

"""
    _default_msg(::Type{T}) -> T

A zero/empty instance of generated message type `T` for warm-up (§D8): every field
defaulted by its type — `0`/`false` for numbers, `""` for strings, a fixed array
(`SArray`) of its defaulted element, an empty `Vector` for sequences, recursively
for nested messages. Built positionally so it works regardless of whether the struct
has keyword defaults. `warmup_sample` overrides it when the caller supplies realism.
"""
_default_msg(::Type{T}) where {T} = T(map(_default_field, fieldtypes(T))...)

# A fixed array `T[N]` generates as `SArray{Tuple{N},T,1,N}` for *any* element
# `T` — string/struct included — so we default per-element and rebuild the SArray
# (`length(S)` is the flattened `prod(dims)` count, `eltype(S)` the element type).
# `zero` only covers numeric elements; on strings/structs it MethodErrors.
_default_field(::Type{S}) where {S} =
    S <: Number         ? zero(S) :
    S <: AbstractString ? convert(S, "") :
    S <: StaticArray    ? (eltype(S) <: Number ? zero(S) :
                           S(ntuple(_ -> _default_field(eltype(S)), length(S)))) :
    S <: AbstractVector ? S(undef, 0) :      # sequence (Vector{E})
    _default_msg(S)                           # nested message struct

# ── per-kind anchors ───────────────────────────────────────────────────────────

# Publisher: warm `encode(T)` and the monomorphic `publish` (which `put`s through
# the concrete route `R`). :execute publishes once — encode runs, the `put` itself
# null-routes (`_WARMUP[]` guard in `publish`).
function _warm_publisher(policy::WarmupPolicy, pub::PublisherHandle{T, R}, sample) where {T, R}
    precompile(encode, (T,))
    precompile(publish, (PublisherHandle{T, R}, T))
    if policy.mode === :execute
        # Sample build sits inside the warm try: a synthesizer failure (e.g. a
        # field type the default-builder can't handle) degrades to precompile-only
        # with a warning, never escaping to crash a sync constructor or vanish into
        # an unhandled background-task throw.
        _run_warm(pub.entity.endpoint.topic) do
            publish(pub, _sample_msg(T, sample))
        end
    end
    nothing
end

# Subscription: anchor decode + the innermost frame that calls `handler(msg)`
# (`_invoke_owned`/`_invoke_view`) — inference specializes decode and the handler
# call as one unit there. :execute encodes the sample to a plain buffer (no live
# Zenoh needed), decodes it (owned or view — the view aliases the buffer, which is
# alive across the call), and runs the handler to full native depth.
function _warm_subscription(policy::WarmupPolicy, e::Entity, ::Type{T}, handler::H,
                            view::ViewMode, sample) where {T, H}
    # The static and dynamic paths share `_dispatch_decoded` — warm it (the unit
    # where decode + the handler call specialize together) for the holder the
    # consumer actually borrows, in this subscription's view mode.
    precompile(_dispatch_decoded, (Entity, SampleHolder, Type{T}, H, typeof(view)))
    if _is_view(view)
        precompile(decode_view, (Memory{UInt8}, Type{T}))
    else
        precompile(decode_owned, (Memory{UInt8}, Type{T}))
        precompile(_decode_on_consumer, (Entity, SampleHolder, Type{T}))
        precompile(_invoke_owned, (Entity, T, H))
    end
    precompile(handler, (T,))
    if policy.mode === :execute
        # Sample build + encode inside the warm try (see `_warm_publisher`): a
        # builder failure degrades to precompile-only, never fatal.
        _run_warm(e.endpoint.topic) do
            buf = as_memory(encode(_sample_msg(T, sample)), UInt8)
            if _is_view(view)
                handler(decode_view(buf, T))
            else
                handler(decode_owned(buf, T))
            end
        end
    end
    nothing
end

# Dynamic (keyexpr-only) subscription: no compile-time `T` at construction — warmed
# at *first sight* of each runtime type (reached via `invokelatest` from
# `_run_typed_dynamic`, since `T` is runtime-born). Owned-only, precompile-only
# (running a discovered handler on a synthesized sample is the D9 manifest's job).
function _warm_dynamic(::Type{T}, handler::H) where {T, H}
    # The dynamic worker borrows a `Memory{UInt8}` (`with_payload_memory`) and
    # decodes via `decode_view`/`decode_owned` per the sub's `view` flag — warm both.
    precompile(decode_owned, (Memory{UInt8}, Type{T}))
    precompile(decode_view, (Memory{UInt8}, Type{T}))
    precompile(handler, (T,))
    nothing
end

# Service: anchor request decode, the handler on `Req`, and response encode.
# :execute runs the handler on a default request (its outbound `call`s null-route)
# and encodes the response it returns (warming the reply path to depth).
function _warm_service(policy::WarmupPolicy, e::Entity, ::Type{Req}, ::Type{Resp},
                       handler::H, sample) where {Req, Resp, H}
    # request decode runs over `as_memory(query payload)` via `decode_request`,
    # which branches to `decode_view`/`decode_owned` over `Memory{UInt8}`.
    precompile(decode_owned, (Memory{UInt8}, Type{Req}))
    precompile(decode_view, (Memory{UInt8}, Type{Req}))
    precompile(handler, (Req,))
    precompile(encode, (Resp,))
    if policy.mode === :execute
        # Sample build inside the warm try (see `_warm_publisher`): a builder
        # failure degrades to precompile-only, never fatal.
        _run_warm(e.endpoint.topic) do
            resp = handler(_sample_msg(Req, sample))
            resp isa Resp && encode(resp)
        end
    end
    nothing
end

# Action server: anchor goal decode, the per-goal execution callable on its
# `GoalHandle`, and result/feedback encode. Always precompile-only — a goal body is
# a long-running mission wrapped in cancellation/settlement machinery over a live
# `GoalHandle`; executing a fabricated one at warm-up is neither safe nor
# meaningful, so :execute is a no-op here (documented in the D8 status). `exec` is
# the do-block body (high-level) or the `on_accepted` callback (low-level).
function _warm_action(::Type{G}, ::Type{R}, ::Type{F}, exec::E,
                      ::Type{GH}) where {G, R, F, E, GH}
    precompile(decode_owned, (Memory{UInt8}, Type{G}))  # _decode_query → decode_owned
    precompile(exec, (GH,))
    precompile(encode, (R,))
    precompile(encode, (F,))
    nothing
end

# ── package warm-up (PrecompileTools) ──────────────────────────────────────────
# Exercise the codec round-trip over two representative *vendored* types — one
# all-fixed (`@cdr_fixed` path) and one with string fields (`@kwdef`/`read_view`
# path) — so encode/decode codegen is baked into ROSNode's pkgimage. Transport-free
# (no live Zenoh session) so it is precompile-safe; full through-the-transport
# dispatch warming stays per-entity at construction (a live session exists there).
@setup_workload begin
    @compile_workload begin
        for T in (Interfaces.builtin_interfaces.msg.Time,        # all-fixed
                  Interfaces.type_description_interfaces.msg.KeyValue)  # strings
            msg = _default_msg(T)
            z = encode(msg)
            decode_owned(as_memory(z, UInt8), T)
            decode_view(as_memory(z, UInt8), T)
        end
    end
end
