# Warm-up. The dispatch chain (foreign thread → Zenoh.jl trampoline → consumer task
# → decode(sample,T) → handler(::T) → user callees) is specialized on the message
# type, so the first publish/handler-on-`T` JITs the whole chain — a first-message
# stall. Construction holds every concrete type (`T`, the route type `R`, the handler
# closure), so the anchors below pin warm-up at the innermost type-specialized frame.
#
# Two depths (the `warmup` knob, a `WarmupPolicy`'s `WarmupMode`):
#   • Precompile() (default) — `precompile(...)`-anchor the chain: caches the reachable
#     inference tree and codegens the named frame and its inlined callees,
#     side-effect-free, needing no message instance. Native codegen of deep
#     non-inlined user callees still defers to the first real message.
#   • Execute() (opt-in) — additionally run the handler once on a sample message under
#     the `_WARMUP` scope (`is_warming()` true), reaching full native depth including
#     the handler body. `@effectful` skips non-ROS effects (still compiled) and
#     outbound ROS ops null-route just before the wire op.
#
# Warm-up never commits side-effects (the seq counter increment is skipped) and never
# fails fatally: a throw degrades to compile-only with a log, since a handler written
# for live data may reject a synthesized sample.
#
# `sync=false` (default) warms on a background task; `sync=true` blocks the
# constructor until the first message is guaranteed compiled.

using PrecompileTools: @setup_workload, @compile_workload
import PrecompileTools          # name needed for `@precompile_nodes`' GlobalRef to its `@compile_workload`
# Fixed primitive arrays (`float[3]`) generate as `SVector`; the default-message
# builder detects them. Named explicitly because the module-wide `using` lands in a
# file included after this one.
using StaticArrays: StaticArray

# ── the dispatcher ─────────────────────────────────────────────────────────────
# `_warmup!` only chooses off / inline (sync) / background (async). A background warm
# touches only already-constructed, type-stable state, so it races nothing.
function _warmup!(policy::WarmupPolicy, warm)
    policy.mode isa NoWarmup && return nothing
    if policy.sync
        warm()
    else
        Threads.@spawn warm()
    end
    return nothing
end

# An entity's effective policy: an explicit `warmup`/`warmup_sync` kwarg overrides the
# node default, `nothing` inherits it.
function _resolve_warmup(node::Node, mode::Union{Symbol, WarmupMode, Nothing}, sync::Union{Bool, Nothing})
    mode === nothing && sync === nothing && return node.warmup
    WarmupPolicy(mode === nothing ? node.warmup.mode : mode,
                 sync === nothing ? node.warmup.sync : sync)
end

# Run a thunk under the `_WARMUP` scope (`is_warming()` true on this task and any
# child it spawns), swallowing throws so a warm-up failure degrades to compile-only.
# Logged at `warn`: a synthesizer mismatch on live-data handlers is expected.
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
# Both generated struct forms (`@cdr_fixed`, `@kwdef`) keep a positional inner
# constructor, so a value builds field-by-field from `fieldtypes`, recursing into
# nested messages. Backs the :execute fallback and the package `@compile_workload`.

"""
    _default_msg(::Type{T}) -> T

Build a zero/empty instance of generated message type `T` for warm-up. Each field
gets its type's default:

- `Number` — `0`/`false`.
- `AbstractString` — `""`.
- `StaticArray` (a fixed array) — an `SArray` of defaulted elements.
- `AbstractVector` (a sequence) — an empty `Vector`.
- nested message — recurse, building it field-by-field.

The positional constructor handles both the `@cdr_fixed` and `@kwdef` struct forms.
`warmup_sample` overrides this when the caller supplies a representative message.

Generated from a ROS 2 interface (`.msg`) definition; see
https://docs.ros.org/en/rolling/Concepts/Basic/About-Interfaces.html
"""
_default_msg(::Type{T}) where {T} = T(map(_default_field, fieldtypes(T))...)

# A fixed array `T[N]` generates as `SArray{Tuple{N},T,1,N}` for any element `T`.
# `zero` covers numeric elements; string/struct elements are defaulted per-element
# and the SArray rebuilt, since `zero` MethodErrors on them.
_default_field(::Type{S}) where {S} =
    S <: Number         ? zero(S) :
    S <: AbstractString ? convert(S, "") :
    S <: StaticArray    ? (eltype(S) <: Number ? zero(S) :
                           S(ntuple(_ -> _default_field(eltype(S)), length(S)))) :
    S <: AbstractVector ? S(undef, 0) :      # sequence (Vector{E})
    _default_msg(S)                           # nested message struct

# ── per-kind anchors ───────────────────────────────────────────────────────────

# Publisher: warm `encode(T)` and the monomorphic `publish` over the concrete route
# `R`. :execute publishes once — encode and attach run, while the seq commit and the
# `put` itself null-route under the `_WARMUP` guard.
function _warm_publisher(policy::WarmupPolicy, pub::PublisherHandle{T, R}, sample) where {T, R}
    precompile(encode, (T,))
    precompile(publish, (PublisherHandle{T, R}, T))
    if policy.mode isa Execute
        _run_warm(pub.entity.endpoint.topic) do
            publish(pub, _sample_msg(T, sample))
        end
    end
    nothing
end

# Subscription: anchor decode and the innermost frame calling `handler(msg)`, where
# inference specializes decode and the handler call as one unit. :execute encodes the
# sample to a plain buffer, decodes it (the view aliases that buffer, alive across the
# call), and runs the handler to full native depth.
function _warm_subscription(policy::WarmupPolicy, e::Entity, ::Type{T}, handler::H,
                            view::ViewMode, sample) where {T, H}
    # Static and dynamic paths share `_dispatch_decoded`; warm it for the holder the
    # consumer borrows, in this subscription's view mode.
    precompile(_dispatch_decoded, (Entity, SampleHolder, Type{T}, H, typeof(view)))
    if _is_view(view)
        precompile(decode_view, (Memory{UInt8}, Type{T}))
    else
        precompile(decode_owned, (Memory{UInt8}, Type{T}))
        precompile(_decode_on_consumer, (Entity, SampleHolder, Type{T}))
        precompile(_invoke_owned, (Entity, T, H))
    end
    precompile(handler, (T,))
    if policy.mode isa Execute
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

# Dynamic (keyexpr-only) subscription: no compile-time `T` at construction. This is
# the manifest-replay anchor, run via `invokelatest` at startup to pre-warm prior-run
# types ahead of the first message. Precompile-only.
function _warm_dynamic(::Type{T}, handler::H) where {T, H}
    # The dynamic worker decodes via `decode_view`/`decode_owned` per the sub's `view`
    # flag, so warm both.
    precompile(decode_owned, (Memory{UInt8}, Type{T}))
    precompile(decode_view, (Memory{UInt8}, Type{T}))
    precompile(handler, (T,))
    nothing
end

# Service: anchor request decode, the handler on `Req`, and response encode. :execute
# runs the handler on a default request (its outbound `call`s null-route) and encodes
# the response, warming the reply path to depth.
function _warm_service(policy::WarmupPolicy, e::Entity, ::Type{Req}, ::Type{Resp},
                       handler::H, sample) where {Req, Resp, H}
    # `_decode_and_serve` branches to `decode_owned` (over a copied `Memory`) or
    # `decode_view` (over the borrowed `PayloadView`); warm both.
    precompile(decode_owned, (Memory{UInt8}, Type{Req}))
    precompile(decode_view, (Zenoh.PayloadView, Type{Req}))
    precompile(handler, (Req,))
    precompile(encode, (Resp,))
    if policy.mode isa Execute
        _run_warm(e.endpoint.topic) do
            resp = handler(_sample_msg(Req, sample))
            resp isa Resp && encode(resp)
        end
    end
    nothing
end

# Action server: anchor goal decode, the per-goal execution callable on its
# `GoalHandle`, and result/feedback encode. Always precompile-only — a goal body is a
# long-running mission over a live `GoalHandle`, so fabricating one to execute at
# warm-up is neither safe nor meaningful and :execute is a no-op here. `exec` is the
# do-block body (high-level) or the `on_accepted` callback (low-level).
function _warm_action(::Type{G}, ::Type{R}, ::Type{F}, exec::E,
                      ::Type{GoalHandle{A, G, R, F}}) where {A, G, R, F, E}
    GH = GoalHandle{A, G, R, F}
    precompile(decode_owned, (Memory{UInt8}, Type{G}))  # _decode_query → decode_owned
    precompile(exec, (GH,))
    precompile(encode, (R,))
    precompile(encode, (F,))
    # The three service wrappers the server puts on the wire (the inner G/R/F nest
    # inside these): each decodes its `_Request` and encodes its `_Response`, feedback
    # rides as `<A>_FeedbackMessage`, CancelGoal is the shared action_msgs type.
    # Guarded so a hand-rolled action lacking a generated wrapper degrades to the
    # inner-only warm rather than breaking construction.
    try
        precompile(decode_owned, (Memory{UInt8}, Type{_action_wrapper(A, "_SendGoal_Request")}))
        precompile(encode, (_action_wrapper(A, "_SendGoal_Response"),))
        precompile(decode_owned, (Memory{UInt8}, Type{_action_wrapper(A, "_GetResult_Request")}))
        precompile(encode, (_action_wrapper(A, "_GetResult_Response"),))
        precompile(encode, (_action_wrapper(A, "_FeedbackMessage"),))
        precompile(decode_owned, (Memory{UInt8}, Type{_CancelGoal_Request}))
        precompile(encode, (_CancelGoal_Response,))
    catch
    end
    nothing
end

# ── package warm-up (PrecompileTools) ──────────────────────────────────────────
# Bakes the codec round-trip into ROSNode's pkgimage over two representative vendored
# types — one all-fixed (`@cdr_fixed` path), one with string fields (`@kwdef`/
# `read_view` path). Transport-free, so it is precompile-safe; through-the-transport
# dispatch warming stays per-entity at construction where a live session exists.
@setup_workload begin
    @compile_workload begin
        for T in (Interfaces.builtin_interfaces.msg.Time,        # all-fixed
                  Interfaces.type_description_interfaces.msg.KeyValue)  # strings
            msg = _default_msg(T)
            z = encode(msg)
            decode_owned(as_memory(z, UInt8), T)
            decode_view(as_memory(z, UInt8), T)
        end
        # The six standard parameter services + /parameter_events ride every node that
        # calls `wire_parameter_services!`, and their request/response codecs are over
        # fixed `rcl_interfaces` types — so bake that shared codec layer once here rather
        # than re-JITing it at the first node with parameters. The per-mixin `ParameterServer{P_M}`
        # service *construction* is schema-specific and stays `@precompile_nodes`' job.
        let RCL = Interfaces.rcl_interfaces.srv
            for (ReqT, RespT) in ((RCL.DescribeParameters_Request,        RCL.DescribeParameters_Response),
                                  (RCL.GetParameterTypes_Request,         RCL.GetParameterTypes_Response),
                                  (RCL.GetParameters_Request,             RCL.GetParameters_Response),
                                  (RCL.ListParameters_Request,            RCL.ListParameters_Response),
                                  (RCL.SetParameters_Request,             RCL.SetParameters_Response),
                                  (RCL.SetParametersAtomically_Request,   RCL.SetParametersAtomically_Response))
                precompile(decode_owned, (Memory{UInt8}, Type{ReqT}))
                precompile(decode_view,  (Memory{UInt8}, Type{ReqT}))
                precompile(encode,       (RespT,))
            end
            precompile(encode, (Interfaces.rcl_interfaces.msg.ParameterEvent,))
        end
        # The composed-`@node` façade wiring is non-parametric — `CompositeParameterServer`
        # holds its members in a runtime field, not a type parameter — so its construction +
        # six-service wiring + `/parameter_events` aggregation is one specialisation shared by
        # every composed node. Bake it here rather than re-JITing the whole wiring at the first
        # composed node's `run` (the per-mixin `ParameterServer{P}` form stays per-schema).
        precompile(CompositeParameterServer, (Node, Vector{Pair{Symbol, ParameterServer}}))
        precompile(wire_parameter_services!, (CompositeParameterServer,))
        precompile(_wire_composite_events!,  (CompositeParameterServer,))
        # The action server `encode`s a `GoalStatusArray` onto `~/_action/status` on the first
        # goal accept and each transition. Fixed `action_msgs` type, so bake the status-snapshot
        # encode (transitively its nested `GoalStatus`/`GoalInfo`) once here instead of per node.
        precompile(encode, (Interfaces.action_msgs.msg.GoalStatusArray,))
        # Re-bake the PEG grammar combinators with Zenoh loaded. ROSMessages bakes them
        # in its Zenoh-free pkgimage, but Zenoh's `pointer(::GuardedPayloadView)`
        # specialization (GuardedPayloadView <: DenseVector) invalidates them at load,
        # so the first `Context()` re-JITs them (~4s) — the root cause of its cost.
        # Running `_wellknown_entries()` here (Zenoh present) pins valid copies in
        # ROSNode's image; it parses the same grammar the broader `_canonical_entries()`
        # uses, which can't run until staticgen.jl is included.
        _wellknown_entries()
    end
end
