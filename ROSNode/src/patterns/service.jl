# Services: one callback and a write-once result cell. Under rmw_zenoh a
# service is a Zenoh queryable — the request rides the query payload, the response
# the reply. The fail-safe settlement core (settlement.jl) guarantees every handler
# exit fills the cell exactly once: `return resp` → reply-ok; an explicit
# `respond!(req, failed, …)`, a throw, or a non-responding return → a Zenoh query
# error reply, so the client's blocking `call` raises rather than returning a
# plausible zeroed response. The cell settles within the handler's extent: a
# still-empty cell is force-aborted when the handler returns, then the owned
# `Query` is finalized. `detach!(req)` opts a handler out of that at-return
# settlement, transferring the owned query + cell to a spawned task (the
# action-server pattern, ported here); a per-service `detach_timeout` sweeper +
# `close`-drain bound a forgotten reply so the client still can't hang.
#
# Client side: `call(client, req)` is a `Zenoh.get` whose handler we drain on the
# *calling* task (it yields), raising on an error reply; `async=true` returns a
# fetchable that resolves the same way off a spawned task.

using Zenoh: Zenoh, Keyexpr, Query, Queryable, Querier, reply, reply_err, is_ok, sample,
             error_payload, payload, matching_status, MatchingListener,
             CancellationToken
import Zenoh   # `Zenoh.cancel` is qualified — ROSNode's own `cancel` (action goals) would clash
using ROSZenoh: ROSZenoh

export Service, ServiceClient, ServiceHandle, call, request_type, response_type,
       service_matched

# ── service-type reflection ──────────────────────────────────────────────────

"""
    request_type(SrvType) -> Type

The request message type of a service `SrvType`. A generated service `Foo` lives
as two sibling structs `Foo_Request` and `Foo_Response` under `<pkg>.srv` (there
is no umbrella `Foo` type), so the service is named by its request struct: the
default treats `SrvType` as that request struct (its name ends in `_Request`) and
returns it unchanged.

The [`TypeRegistry`](@ref) specializes this for service markers: `@ros_service` emits a
`request_type` method mapping the marker to its generated request type, so a
registered service value resolves through the same call.

Throws `ArgumentError` if `SrvType` is not a recognizable `*_Request` struct.

Paired with [`response_type`](@ref); both feed `Service` and
[`ServiceClient`](@ref) construction.
"""
function request_type(::Type{Req}) where {Req}
    endswith(string(nameof(Req)), "_Request") ||
        throw(ArgumentError("$(Req) is not a service request type (expected a \
                             generated `*_Request` struct); pass the service's \
                             Request type, e.g. `pkg.srv.SetBool_Request`"))
    return Req
end

"""
    response_type(SrvType) -> Type

The response message type paired with [`request_type`](@ref)`(SrvType)`. The
default resolves [`request_type`](@ref) first, strips the `_Request` suffix from
its name, and looks up the `_Response` sibling struct in the same module.

The [`TypeRegistry`](@ref) specializes this for service markers alongside
[`request_type`](@ref), so a registered service value resolves directly.

Throws `ArgumentError` if the request type is unrecognizable, or if the
`_Response` sibling is missing from the request's module (a partially generated
service).

The response type is what a `Service` handler returns and what
[`call`](@ref) decodes from the reply.
"""
function response_type(::Type{Req}) where {Req}
    req = request_type(Req)
    base = _strip_suffix(string(nameof(req)), "_Request")
    mod = parentmodule(req)
    resp_name = Symbol(base, "_Response")
    isdefined(mod, resp_name) ||
        throw(ArgumentError("no response type $(resp_name) alongside $(req) in \
                             $(mod); is the service fully generated?"))
    return getfield(mod, resp_name)::Type
end

_strip_suffix(s::AbstractString, suffix::AbstractString) =
    endswith(s, suffix) ? s[1:end-length(suffix)] : s

# ── ServiceHandle (the server) ───────────────────────────────────────────────
# An `Entity` (node.jl) carrying a channel-form Zenoh `Queryable` plus its
# consumer task. Draining with `take!` (not the callback form) lets the consumer
# own the `Query` lifetime: it holds the owned `Query` across the handler and
# `finalize`s it only after the reply is sent, which is the final-ack the client's
# `get` waits on. The callback form drops the `Query` the instant the callback
# returns, racing the reply against the undeclare.

"""
    ServiceHandle{Req, Resp}

A running [service](https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html)
server for request type `Req` / response type `Resp`. Under rmw_zenoh a
service is a Zenoh queryable: the request rides the query payload and the response
rides the reply. The handle wraps an [`Entity`](@ref) and owns the queryable route
plus the consumer task that drains it.

Construct one with the do-block spelling `Service(node, name, SrvType) do req … end`.
`isopen` reports whether it is still serving; `close`
undeclares the queryable, joins the consumer task within a bounded window (a
wedged native call is detached and teardown proceeds), and reaps the entity. It
also dies with its node when the node closes.
"""
mutable struct ServiceHandle{Req, Resp}
    const entity::Entity
end

# The per-query consumer wiring stashed in `entity.wire` so it shares the
# entity's lifecycle. `close(queryable)` disconnects the channel, ending the
# consumer task's drain loop; the task is waited on so a closing service can't
# leave a reply half-sent.
#
# `detached` is this service's registry of cells handed off by `detach!`: a
# detached cell's owned `Query` is *not* finalized at handler return (ownership
# transferred), so it lives here keyed to its force-abort deadline (`time()`
# seconds, `Inf` ⇒ bounded only by drain). The `sweeper` Timer force-aborts +
# finalizes cells past deadline; it is armed lazily on first detach and stopped
# when the table empties (no perpetual idle timer). Per-service ownership: the
# table dies with the service in `close`, draining outstanding cells.
mutable struct _ServiceWire
    const queryable::Queryable
    consumer::Union{Task, Nothing}
    @atomic seq::Int64           # per-service reply sequence_number (rmw_zenoh attachment)
    const detach_timeout::Float64
    const detached::Dict{ResultCell, Float64}   # cell → force-abort deadline (time() s)
    const detach_lock::ReentrantLock
    sweeper::Union{Base.Timer, Nothing}
end

_ServiceWire(queryable::Queryable, consumer, seq::Int64, detach_timeout::Real) =
    _ServiceWire(queryable, consumer, seq, Float64(detach_timeout),
                 Dict{ResultCell, Float64}(), ReentrantLock(), nothing)

# How often the sweeper scans for expired detached cells (seconds).
const _DETACH_SWEEP_INTERVAL = 1.0

function Base.close(w::_ServiceWire)
    # Closing the queryable disconnects its channel → the `take!` loop sees
    # CHANNEL_DISCONNECTED and ends. Then join the consumer so teardown is ordered
    # (no reply racing the undeclare) — but *bounded*: if the consumer doesn't end
    # promptly (e.g. wedged in a native call), we move on rather than hang `close`
    # forever. The stuck task is detached and dies with the process.
    try
        close(w.queryable)
    finally
        t = w.consumer
        w.consumer = nothing
        t === nothing ||
            (try; timedwait(() -> istaskdone(t), 3.0; pollint=0.02); catch; end)
    end
    _drain_detached!(w)
    nothing
end

# Drain outstanding detached cells at service teardown: force-abort + finalize
# every cell still registered (mirrors the action server's `_await_goals` bound,
# but services are single-reply so there is no body to await — a forgotten
# respond! is aborted outright). Stops the sweeper.
function _drain_detached!(w::_ServiceWire)
    cells = @lock w.detach_lock begin
        w.sweeper === nothing || (try; close(w.sweeper); catch; end)
        w.sweeper = nothing
        cs = collect(keys(w.detached))
        empty!(w.detached)
        cs
    end
    for cell in cells
        _abort_detached!(cell)
    end
    nothing
end

# Force-abort a detached cell and finalize its owned query (the final-ack
# `_serve_query` skipped on the detach path). Both are infallible — a settled
# cell's `force_abort!` is a no-op, finalize is idempotent.
function _abort_detached!(cell::ResultCell)
    try; force_abort!(cell); catch; end
    q = cell.handle
    q isa Query && (try; finalize(q.q); catch; end)
    nothing
end

# Register a detached cell on its owning service, keyed to its force-abort
# deadline, and arm the sweeper if it isn't already running.
function _register_detached!(w::_ServiceWire, cell::ResultCell)
    deadline = w.detach_timeout > 0 && isfinite(w.detach_timeout) ?
               time() + w.detach_timeout : Inf
    @lock w.detach_lock begin
        w.detached[cell] = deadline
        if w.sweeper === nothing && isfinite(deadline)
            w.sweeper = Base.Timer(_ -> _sweep_detached!(w),
                                   _DETACH_SWEEP_INTERVAL; interval=_DETACH_SWEEP_INTERVAL)
        end
    end
    nothing
end

# Drop a detached cell from the registry (settled or aborted); stop the sweeper
# once empty. Also finalizes the owned query — the final-ack `_serve_query`
# skipped on the detach path.
function _finalize_detached!(w::_ServiceWire, cell::ResultCell)
    @lock w.detach_lock begin
        delete!(w.detached, cell)
        if isempty(w.detached) && w.sweeper !== nothing
            try; close(w.sweeper); catch; end
            w.sweeper = nothing
        end
    end
    q = cell.handle
    q isa Query && (try; finalize(q.q); catch; end)
    nothing
end

# Sweeper tick: reap settled detached cells (finalizing their query) and
# force-abort + finalize any past its deadline with an @error (a forgotten
# respond!). Stops itself when the table empties.
function _sweep_detached!(w::_ServiceWire)
    now = time()
    settled = ResultCell[]
    expired = ResultCell[]
    @lock w.detach_lock begin
        for (cell, deadline) in w.detached
            if isfilled(cell)
                push!(settled, cell)
            elseif now >= deadline
                push!(expired, cell)
            end
        end
        for cell in Iterators.flatten((settled, expired))
            delete!(w.detached, cell)
        end
        if isempty(w.detached) && w.sweeper !== nothing
            try; close(w.sweeper); catch; end
            w.sweeper = nothing
        end
    end
    for cell in settled
        q = cell.handle
        q isa Query && (try; finalize(q.q); catch; end)
    end
    for cell in expired
        @error "detached service request exceeded detach_timeout; force-aborting" detach_timeout=w.detach_timeout
        _abort_detached!(cell)
    end
    nothing
end

"""
    Service(node, name, SrvType; qos=default_qos(), view=false,
            concurrency=Serial(), detach_timeout=60.0, warmup=nothing,
            warmup_sync=nothing, warmup_sample=nothing) do req … end -> ServiceHandle

Declare a service `name` of service type `SrvType` on `node` and start serving it,
returning a [`ServiceHandle`](@ref). `Service` is the `EndpointKind` enum
value given a call-method; the do-block desugars function-first, so the handler
becomes the first argument.

The handler runs once per request, settling the request's write-once `ResultCell`
through [`respond!`](@ref) (which owns the exactly-once handler-exit contract).
This domain's status mapping:

  - return the response message ⇒ reply-ok;
  - `respond!(req, failed, msg)` ⇒ a Zenoh query *error* reply, raising
    [`ServiceError`](@ref) in the client's [`call`](@ref);
  - a throw ⇒ a synthesized error reply (logged with a backtrace);
  - a return that is not a valid response ⇒ a synthesized error reply.

The cell settles within the handler's extent by default: force-aborted if still
empty when the handler returns, then the owned `Query` is finalized to send the
final-ack. A handler that calls [`detach!`](@ref)`(req)` instead transfers
settlement to another task — the at-return abort is suppressed and the owned
`Query` lives past handler return until that task settles via the returned handle
(the action-server pattern, ported to services). `detach_timeout` (seconds,
default `60.0`; `0`/`Inf` ⇒ bounded only by drain) caps how long a detached
request may stay unsettled: a sweeper force-aborts + finalizes it past the
deadline with an `@error`, so a forgotten `respond!` cannot hang a client. `close`
drains outstanding detached requests the same way.

The request is fully owned by default (storable, forwardable, spawnable past the
handler). `view=true` decodes it as a `CDRView` aliasing a private owned
copy of the payload — variable-length fields stay views over that copy, and the
view remains valid for as long as it is reachable, since the copy is owned
(zero-copy over the borrowed wire buffer itself is a pending refinement).

`concurrency` (a [`Concurrency`](@ref): [`Serial`](@ref)`()`, the default, or
[`Parallel`](@ref)`(n)`) schedules the handler bodies: `Serial` serves on the
node's one sticky cooperative thread, `Parallel(n)` on up to `n` OS threads. When
a finite pool is full, further queries buffer in the queryable FIFO until a slot
frees. `qos` maps onto the ROS 2 QoS policies for the route.

`SrvType` is the generated `*_Request` struct (the response type is its
`*_Response` sibling); the [`TypeRegistry`](@ref) can specialize [`request_type`](@ref) /
[`response_type`](@ref) for a registered service marker. `warmup`, `warmup_sync`,
and `warmup_sample` select the warm-up policy (precompile/execute/off, sync/async)
that pre-JITs the request-decode → handler → response-encode dispatch chain,
defaulting to the node's policy.

```julia
srv = Service(node, "add_two_ints", AddTwoInts_Request) do req
    AddTwoInts_Response(; sum = req.a + req.b)
end
```
"""
function _make_service(handler, node::Node, name::AbstractString, ::Type{Srv};
                       qos::QosProfile=default_qos(), view::Bool=false,
                       concurrency::Concurrency=Serial(),
                       detach_timeout::Real=60.0,
                       warmup::Union{Symbol, Nothing}=nothing,
                       warmup_sync::Union{Bool, Nothing}=nothing,
                       warmup_sample=nothing) where {Srv}
    Req  = request_type(Srv)
    Resp = response_type(Srv)
    sname = resolve_name(node, name; kind=:service)
    # The service entity carries the SERVICE-level type identity (`pkg::srv::dds_::
    # Base_` + the service RIHS01) in its keyexpr/liveliness, matching rmw_zenoh — the
    # request type's own info would never match a peer. Falls back to the request
    # info when the service type can't be synthesized (unregistered types).
    sti = something(service_type_info_of(Req, Resp), type_info_of(Req))
    ent = make_entity(node, Service, sname, sti; qos=qos)

    ctx = ent.node.context
    tk = topic_keyexpr(ctx.format, ent.endpoint)
    qable = Queryable(ctx.session, Keyexpr(tk); channel=:fifo, complete=true)
    wire = _ServiceWire(qable, nothing, Int64(0), detach_timeout)
    ent.wire = wire
    wire.consumer = _spawn_service_consumer(ent, Req, Resp, handler, view, concurrency)

    # Warm-up: precompile request-decode → handler → response-encode (and, under
    # :execute, serve a default request once, its outbound `call`s null-routed).
    pol = _resolve_warmup(node, warmup, warmup_sync)
    _warmup!(pol, () -> _warm_service(pol, ent, Req, Resp, handler, warmup_sample))

    return ServiceHandle{Req, Resp}(ent)
end

# Drain the queryable's FIFO channel. Each query is decoded and dispatched per
# the concurrency policy; a closed queryable ends the loop cleanly (the channel
# disconnects → `take!` throws the disconnect, caught below). `take!` (not
# `iterate`) is used so we own the `Query` lifetime: we finalize it only after
# the reply is sent, which is the final-ack the client's `get` waits on.
# A per-service scheduler: `Serial()` serves inline on the (sticky) consumer
# task — one request at a time on one OS thread; `Parallel(n)` serves on spawned
# threads bounded to `n` in flight by a semaphore; `Parallel(Inf)` is unbounded.
# (A "busy error-reply on saturation" for services is a pending refinement;
# today a full pool blocks the consumer, buffering queries in the queryable FIFO.)
_service_scheduler(::Serial, e, ::Type{Req}, ::Type{Resp}, h, v) where {Req, Resp} =
    query -> _serve_query(e, query, Req, Resp, h, v)
function _service_scheduler(c::Parallel, e, ::Type{Req}, ::Type{Resp}, h, v) where {Req, Resp}
    if isfinite(c.n)
        sem = Base.Semaphore(Int(c.n))
        return function (query)
            Base.acquire(sem)
            Threads.@spawn try
                _serve_query(e, query, Req, Resp, h, v)
            finally
                Base.release(sem)
            end
            nothing
        end
    else
        return query -> (Threads.@spawn _serve_query(e, query, Req, Resp, h, v); nothing)
    end
end

function _spawn_service_consumer(e::Entity, ::Type{Req}, ::Type{Resp}, handler,
                                 view::Bool, concurrency::Concurrency) where {Req, Resp}
    qable = (e.wire::_ServiceWire).queryable
    serve = _service_scheduler(concurrency, e, Req, Resp, handler, view)
    # Sticky consumer: Serial handlers run on the node's one cooperative thread.
    t = Task() do
        try
            while true
                serve(take!(qable))
            end
        catch err
            # A closed queryable disconnects its channel; iteration over a dead
            # channel is the normal teardown path, not an error.
            err isa ShutdownException && return
            isopen(e) &&
                @error "service consumer task failed" service=e.endpoint.topic exception=(err, catch_backtrace())
        end
    end
    t.sticky = true
    schedule(t)
    return t
end

# The result cell of the service request currently being handled, bound by
# `_serve_query` for the dynamic extent of the handler so the documented service
# spelling `respond!(req, status, payload)` can reach it — a handler holds the
# decoded request, not the cell.
const _ACTIVE_SERVICE_CELL = Base.ScopedValues.ScopedValue{Any}(nothing)
# The owning service's wire, bound alongside the cell so `detach!` can register
# the handed-off cell on the detach registry (the cell alone can't reach it).
const _ACTIVE_SERVICE_WIRE = Base.ScopedValues.ScopedValue{Any}(nothing)

"""
    respond!(req, status::SettlementStatus, payload) -> Bool

The service-handler spelling of the settlement verb [`respond!`](@ref) (an action
goal settles via `respond!(goal, …)`). This domain's two statuses:

  - `respond!(req, failed, msg)` (`failed` aliases [`failure`](@ref)) — a Zenoh
    query *error* reply carrying `msg`, so the client's [`call`](@ref) raises
    [`ServiceError`](@ref);
  - `respond!(req, success, resp)` — replies `resp`.

`req` is the handler's argument; the result cell is taken from the handler's
dynamic scope (a `ScopedValue` bound for the handler's extent).

Returns `true`. Throws `ArgumentError` when called outside a service handler, or
on an already-settled request (the double-settle error owned by [`respond!`](@ref)).

```julia
Service(node, "validate", Validate_Request) do req
    req.value < 0 && return respond!(req, failed, "value must be non-negative")
    Validate_Response(; ok = true)
end
```
"""
function respond!(@nospecialize(req), status::SettlementStatus, payload)
    cell = _ACTIVE_SERVICE_CELL[]
    cell === nothing && throw(ArgumentError(
        "respond!(req, status, payload) called outside a Service handler \
         (an action goal settles via respond!(goal, …))"))
    return respond!(cell::ResultCell, status, payload)
end

"""
    ServiceRequestHandle

A first-class handle to a detached service request, returned by [`detach!`](@ref).
It carries the request's result cell (and its owning service) so a spawned task
can settle the request via [`respond!`](@ref)`(handle, status, payload)` *without*
the handler's `ScopedValue` — which is out of scope on the spawned task. It is the
service analog of an action `GoalHandle`. Single-reply: the handle settles exactly
once (services have no feedback stream).
"""
struct ServiceRequestHandle
    cell::ResultCell
    wire::_ServiceWire
end

"""
    detach!(req) -> ServiceRequestHandle

Transfer settlement of the current service request off the handler frame. By
default a service handler must settle its request by the time it returns (a
still-empty cell is force-aborted); `detach!` opts out: the at-return abort is
suppressed and the owned `Query` is held past handler return, so another task can
settle it later. Returns a [`ServiceRequestHandle`](@ref) to pass to that task,
which settles via [`respond!`](@ref)`(handle, status, payload)`. Mirrors the
action server's detached goal settlement.

The detached request is bounded by the service's `detach_timeout` (and by drain):
if it is not settled by the deadline a sweeper force-aborts it with an `@error`, so
a forgotten `respond!` cannot hang the client. One-shot — `detach!` on an
already-detached request returns the existing handle.

`req` is the handler's argument; the cell and owning service are taken from the
handler's dynamic scope. Throws `ArgumentError` when called outside a service
handler.

```julia
Service(node, "plan", Plan_Request; detach_timeout = 30.0) do req
    h = detach!(req)
    Threads.@spawn respond!(h, success, solve(req))
    nothing
end
```
"""
function detach!(@nospecialize(req))
    cell = _ACTIVE_SERVICE_CELL[]
    wire = _ACTIVE_SERVICE_WIRE[]
    (cell === nothing || wire === nothing) && throw(ArgumentError(
        "detach!(req) called outside a Service handler"))
    c = cell::ResultCell
    w = wire::_ServiceWire
    # One-shot: a second detach! just hands back the handle (already registered).
    (@atomicswap c.detached = true) || _register_detached!(w, c)
    return ServiceRequestHandle(c, w)
end

"""
    respond!(handle::ServiceRequestHandle, status::SettlementStatus, payload) -> Bool

Settle a detached service request from a spawned task (see [`detach!`](@ref)).
`status`/`payload` carry the same meaning as the in-handler `respond!`:
`success` + a `Resp` replies-ok, `failed` + a message string error-replies. Settles
the cell, then finalizes the owned `Query` (the final-ack) and drops the request
from the detach registry. Returns `true`.
"""
function respond!(handle::ServiceRequestHandle, status::SettlementStatus, payload)
    ok = respond!(handle.cell, status, payload)
    _finalize_detached!(handle.wire, handle.cell)
    return ok
end

# rcl-level infrastructure services (`~/get_type_description`, the parameter services)
# sit BELOW the managed-node abstraction and must serve in every lifecycle state — a
# manager resolves types / reads-and-sets parameters on an inactive node. Only
# *application* services follow the `isactive` dispatch gate; matched by the resolved
# topic basename so it holds however/whenever the service was wired.
const _INFRA_SERVICE_NAMES = ("get_type_description", "describe_parameters",
    "get_parameter_types", "get_parameters", "list_parameters",
    "set_parameters", "set_parameters_atomically")
_is_infra_service(topic::AbstractString) =
    any(n -> endswith(topic, "/" * n), _INFRA_SERVICE_NAMES)

# Serve one query: decode the request, build the result cell whose `deliver`
# closure turns the settled outcome into a reply (ok / error) stamped with the
# attachment, then run the handler under the fail-safe wrapper. The owned `Query`
# is held by the closure (via the cell's `handle`) and finalized once — after the
# reply — to send the final-ack; finalizing in a `finally` guarantees it even if
# delivery itself threw.
function _serve_query(e::Entity, query::Query, ::Type{Req}, ::Type{Resp},
                      handler, view::Bool) where {Req, Resp}
    cell = nothing                          # hoisted so the `finally` can read its detach state
    try
        req = _decode_request(query, Req, view)
        cell = ResultCell{Query, Resp}(query, _service_deliver(e, query, Resp))
        # Dispatch gate (`isactive`): an inactive managed node's application services
        # don't run their handler — this site replies an error rather than a fabricated
        # response. The control surface is exempt, so lifecycle services still serve.
        # The owned query's final-ack rides the `finally` below.
        isactive(e) || _is_infra_service(e.endpoint.topic) ||
            (reply_err(query, "node inactive"); return)
        # Run the handler under the node logger so a plain `@info` inside it
        # routes to the node's /rosout (wherever `settle_handler!` runs the thunk).
        # Bind the cell + owning wire in scope so `respond!`/`detach!` reach them.
        settle_handler!(cell,
                        () -> Base.ScopedValues.with(_ACTIVE_SERVICE_CELL => cell,
                                                     _ACTIVE_SERVICE_WIRE => e.wire) do
                            _with_node_logger(() -> handler(req), e.node)
                        end;
                        success_status = success,
                        default_result = () -> _zero_response(Resp),
                        log_id = e.endpoint.topic)
    catch err
        # A decode failure (malformed request) can't go through the cell — there
        # is no handler to run — so reply an error directly. Other escapes here
        # are framework bugs; surface them and still send an error so the caller
        # doesn't hang.
        err isa ShutdownException && return
        @error "service request handling failed before settlement" service=e.endpoint.topic exception=(err, catch_backtrace())
        try
            reply_err(query, _failure_text(err))
        catch
        end
    finally
        # The final-ack: drop the owned query exactly once, after any reply. A
        # detached, still-empty cell is the exception — ownership of the query
        # transferred to the detach registry, which finalizes it on settle/sweep —
        # so we must NOT finalize it here. The attached path, and a detached cell
        # already settled at return (its `respond!(handle, …)` finalized it), still
        # finalize here (idempotent).
        if !(cell isa ResultCell && (@atomic cell.detached) && !isfilled(cell))
            try
                finalize(query.q)
            catch
            end
        end
    end
    nothing
end

# Decode the request from the query payload — owned by default, a `CDRView` over
# an owned copy under `view=true` (see `decode_request`). A query with no payload
# is a protocol error (a ROS request always carries a CDR body, even for an
# empty Request).
function _decode_request(query::Query, ::Type{Req}, view::Bool) where {Req}
    p = payload(query)
    p === nothing && throw(ArgumentError("service request carried no payload"))
    return decode_request(p, Req, view)
end

# Decode from a `ZBytes`. Copy the payload into owned `Memory` first (the
# query/payload buffer is borrowed and must not be aliased past the handler),
# mirroring the subscription owned path (serialization.jl `decode(::Sample, …)`).
# `view=true` then returns a `CDRView` aliasing that *owned* copy — safe to use
# and escape for as long as the view is reachable, since the copy outlives the
# query.
# TODO(view): zero-copy directly over the borrowed query payload.
function decode_request(p, ::Type{Req}, view::Bool) where {Req}
    mem = Zenoh.as_memory(p, UInt8)
    # Branch on the runtime `view` Bool so each arm calls a single-return-type
    # decode (no `Any` box from a `view`-keyword `decode`).
    return view ? decode_view(mem, Req) : decode_owned(mem, Req)
end

# The cell's `deliver`: map a settled (status, payload) onto a Zenoh reply on the
# captured query, stamping the rmw_zenoh attachment with this entity's gid. Runs
# once, under the cell lock, holding the first terminal write (settlement.jl).
# The `query` is closed over here (it's the cell's handle, but `deliver` is
# handed only `(status, payload)`), so success replies on the right query.
#
#   success + a `Resp` payload ⇒ reply-ok with the encoded Response
#   failure (a message string) ⇒ reply_err (client `call` raises)
#   aborted / force_abort! with `nothing` payload ⇒ a bare error reply (no ctor) —
#     the fail-safe outcome for a non-defaultable result or delivery failure.
#
# Delivery does *not* finalize the query — `_serve_query`'s `finally` does, once,
# after the handler returns and the cell has settled, separating the reply (here)
# from the final-ack (there).
function _service_deliver(e::Entity, query::Query, ::Type{Resp}) where {Resp}
    function (status::SettlementStatus, payload)
        if status === success && payload isa Resp
            bytes = encode(payload)
            seq = (@atomic (e.wire::_ServiceWire).seq += 1)
            ts = nanoseconds(Dates.now(e.node, System()))   # reply-time wall ns
            reply(query, bytes; attachment=encode_attachment(seq, ts, gid(e)))
        else
            # Non-success (explicit `failed`, a thrown handler, or a synthesized
            # abort) ⇒ a query error reply; the client's `call` raises. An
            # explicit failure carries the user's message string; an abort
            # carries a default/`nothing` Resp, for which we send a generic note.
            reply_err(query, _err_message(status, payload))
        end
        nothing
    end
end

# The error-reply message for a non-success settlement. An explicit `failure`
# carries the user's error string/exception as the payload; an `aborted` outcome
# carries a synthesized default Resp (or `nothing` from force_abort!) which has no
# useful message. So the status picks the strategy — surface the failure payload,
# or emit a generic note for an abort — and the payload formatting is one helper.
_err_message(::Failure, payload) = _failure_text(payload)
_err_message(::SettlementStatus, _) = "service handler aborted"

_failure_text(msg::AbstractString) = String(msg)
_failure_text(e::Exception)        = sprint(showerror, e)
_failure_text(other)               = string(other)

# A zero/default Response for the synthesized error/cancel reply. Most generated
# responses are `@kwdef`/`@cdr1_compat` with an all-defaults constructor; fall
# back to `nothing` (→ force_abort!'s bare-error path) if the type can't be
# default-constructed, so a non-defaultable Response still settles fail-safe.
function _zero_response(::Type{Resp}) where {Resp}
    try
        return Resp()
    catch
        return nothing
    end
end

Base.isopen(s::ServiceHandle) = isopen(s.entity)
Base.close(s::ServiceHandle) = close(s.entity)
Base.show(io::IO, s::ServiceHandle{Req}) where {Req} =
    print(io, "Service(", s.entity.endpoint.topic, ", ", _strip_suffix(string(nameof(Req)), "_Request"),
          isopen(s) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle)."
entity(s::ServiceHandle) = s.entity

# ── ServiceClient + call ──────────────────────────────────────────────────────
# The client is an `Entity` of kind `Client` (it announces a matching liveliness
# token so servers discover it); the request itself is a one-shot `Zenoh.get` on
# the service keyexpr, not a long-lived route. `call` blocks the *calling* task
# on the reply (it yields while draining the GetHandler's channel); `async=true`
# runs the same drain on a spawned task and returns that task.

# A client's routing-match wire: a `Querier` on a request keyexpr plus a lazily
# declared `MatchingListener` that wakes graph waiters on a match transition, so a
# `_wait_on_graph` over `_wire_matched` re-checks promptly. The shared building
# block behind both [`wait_for_service`](@ref) (ServiceClient, where this Querier
# is *also* the `call` transport — stashed in `entity.wire`) and
# [`wait_for_action_server`](@ref) (ActionClient, where it's a send_goal matching
# probe only). `lock` guards the lazy listener declare.
mutable struct _ClientWire
    const querier::Querier
    listener::Union{MatchingListener, Nothing}
    const lock::ReentrantLock
end

# Ground-truth routing-match predicate for a wire (`matching_status` is a live poll).
_wire_matched(w::_ClientWire) = matching_status(w.querier)

# Attach the match listener once (idempotent). It only *notifies* `ctx.graph.changed`
# on a transition — `_wire_matched` stays the ground truth a waiter re-checks — so
# there's no stale-state race. Shared by the service + action awaits.
function _ensure_wire_listener!(w::_ClientWire, ctx)
    @lock w.lock begin
        w.listener === nothing || return nothing
        w.listener = MatchingListener(w.querier) do _matched
            @lock ctx.graph.changed notify(ctx.graph.changed)
        end
    end
    nothing
end

# Close the listener first (it GC-pins the querier internally), then the querier.
# Bounded/tolerant so a closing client can't throw out of `close(node)`.
function Base.close(w::_ClientWire)
    @lock w.lock begin
        w.listener === nothing || (try; close(w.listener); catch; end)
        w.listener = nothing
    end
    try; close(w.querier); catch; end
    nothing
end

"""
    ServiceClient{Req, Resp}
    ServiceClient(node, name, SrvType; qos=default_qos()) -> ServiceClient

A [service](https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html)
client for request type `Req` / response type `Resp`. Constructing one
materializes a `Client` [`Entity`](@ref) (a liveliness token so servers discover
it, plus graph tracking) and a long-lived Zenoh `Querier` on the service keyexpr.
That querier is the transport every [`call`](@ref) queries through, and the handle
behind [`service_matched`](@ref) and [`wait_for_service`](@ref).

The querier targets `:all_complete` — every queryable declared complete — like
rmw_zenoh clients, so redundant servers on one service name see the same fan-out
a native client produces; the first reply still settles a call. The client keyexpr
carries the SERVICE-level type identity (service name plus service RIHS), matching
the server's queryable under rmw_zenoh. `SrvType` is the generated `*_Request`
struct; the response is its `*_Response` sibling. `qos` maps onto the ROS 2 QoS
policies.

`isopen` reports whether the client is live; `close` reaps the querier, its match
listener, and the entity, and the client dies with its node.

```julia
cli = ServiceClient(node, "add_two_ints", AddTwoInts_Request)
wait_for_service(cli)
resp = call(cli, AddTwoInts_Request(; a = 2, b = 3))
```
"""
mutable struct ServiceClient{Req, Resp}
    const entity::Entity
    @atomic seq::Int64           # per-client request sequence (rmw_zenoh attachment)
end

function ServiceClient(node::Node, name::AbstractString, ::Type{Srv};
                       qos::QosProfile=default_qos(),
                       warmup::Union{Symbol, Nothing}=nothing,
                       warmup_sync::Union{Bool, Nothing}=nothing) where {Srv}
    Req  = request_type(Srv)
    Resp = response_type(Srv)
    sname = resolve_name(node, name; kind=:service)
    # Service-level keyexpr type, matching the server's queryable (see `_make_service`).
    sti = something(service_type_info_of(Req, Resp), type_info_of(Req))
    ent = make_entity(node, Client, sname, sti; qos=qos)
    # The call transport: a Querier on the service keyexpr. `:all_complete` fans to
    # every complete server, like rmw_zenoh clients; the first reply settles the call.
    # Stashed in `entity.wire` (reaped on close).
    ctx = ent.node.context
    tk = topic_keyexpr(ctx.format, ent.endpoint)
    querier = Querier(ctx.session, Keyexpr(tk); target=:all_complete)
    ent.wire = _ClientWire(querier, nothing, ReentrantLock())
    client = ServiceClient{Req, Resp}(ent, 0)
    # Warm the call codec like the server endpoints (G2). Inherits `node.warmup`, so a
    # `ParameterClient`'s lazily-built internal `ServiceClient`s warm here too.
    _warmup!(_resolve_warmup(node, warmup, warmup_sync), () -> _warm_client(client))
    return client
end

# Anchor the request-reply codec + call path for a client (G2): `encode(Req)` and the
# `call(client, req)` body (which decodes the `Resp`). Precompile-only — never runs the
# network `call`, just compiles it.
function _warm_client(c::ServiceClient{Req, Resp}) where {Req, Resp}
    precompile(encode, (Req,))
    precompile(call, (typeof(c), Req))
    nothing
end

Base.isopen(c::ServiceClient) = isopen(c.entity)
Base.close(c::ServiceClient) = close(c.entity)
Base.show(io::IO, c::ServiceClient{Req}) where {Req} =
    print(io, "ServiceClient(", c.entity.endpoint.topic, ", ",
          _strip_suffix(string(nameof(Req)), "_Request"),
          isopen(c) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle)."
entity(c::ServiceClient) = c.entity

"""
    service_matched(client::ServiceClient) -> Bool

Whether the client's `Querier` is routing-matched to at least one service server
right now — i.e. a [`call`](@ref) would actually reach a server. Reads the
live Zenoh matching status of the querier; returns `false` on a closed client
(its wire has been reaped).

This is the routing-plane signal behind the client form of
[`wait_for_service`](@ref). It is stronger than [`service_is_ready`](@ref): for a
same-process server, liveliness is authoritative the instant the service is
declared (before Zenoh routing settles), whereas matching tracks actual
reachability. Prefer this whenever you hold the client.
"""
service_matched(client::ServiceClient) =
    (w = client.entity.wire; w isa _ClientWire && _wire_matched(w))

# Arm the match listener so `wait_for_service(client)` parks on `ctx.graph.changed`
# and re-checks `service_matched`. The Querier already exists (it's the `call`
# transport, declared at construction), so this only attaches the listener; no-op
# on a closed client (wire reaped).
function _ensure_match_listener!(client::ServiceClient)
    w = client.entity.wire
    w isa _ClientWire || return nothing
    _ensure_wire_listener!(w, client.entity.node.context)
end

"""
    ServiceError(msg)

Thrown by [`call`](@ref) when a service invocation fails: the server replied with
a Zenoh query *error* (ROSNode signals a handler failure over Zenoh's query
`reply_err`), no matching server replied, or the call timed out. A failed
invocation always surfaces as this exception — a real reply is the only way `call`
returns a response. `msg` carries the server's error string when there is one,
otherwise a description of the missing or timed-out reply.
"""
struct ServiceError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ServiceError) = print(io, "ServiceError: ", e.msg)

"""
    call(client::ServiceClient, req; async=false, timeout_ms=60_000) -> Resp | Task

Invoke the [service](https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html):
serialize `req`, query the client's `Querier` on the service keyexpr with the
per-request rmw_zenoh attachment (sequence number, request-time wall clock,
client gid), and resolve the single reply.

Synchronously (`async=false`, the default) this blocks the calling task — it
yields while the reply is in flight — and returns the decoded `Resp`. A failed
invocation raises [`ServiceError`](@ref): an error reply (the wire form of a
handler failure), a missing reply (no matching server), or a timeout.

With `async=true` the same query+drain runs on a spawned task, returned as a raw
`Task`; `fetch` it for the `Resp`. A failed async call surfaces through `fetch`
as a `TaskFailedException` wrapping the [`ServiceError`](@ref) — unwrap it (e.g.
via `current_exceptions(task)`) to reach the original; the synchronous path does
that unwrapping itself.

`timeout_ms` bounds the wait by arming a cancellation timer on the in-flight get;
it defaults to `60_000` (60 s), so every call is bounded unless the caller opts
out. Passing `0` is truly unbounded — no timer, no backstop — and the call waits
until the reply arrives or the Context drains. The synchronous path additionally
applies a hard backstop — `timeout_ms/1000 + 2` seconds — and when it elapses
cancels the in-flight get and raises [`ServiceError`](@ref), so a bounded caller
can never hang even on a `Zenoh.get` that fails to honor its own timeout. The
backstop is synchronous-only: an async call is bounded only by the cancellation
timer and the transport's own query timeout.

Throws `ArgumentError` if `client` is closed or `req` is not the client's request
type.

```julia
cli = ServiceClient(node, "add_two_ints", AddTwoInts_Request)
wait_for_service(cli)
resp = call(cli, AddTwoInts_Request(; a = 2, b = 3); timeout_ms = 1000)
@info "sum" resp.sum
```
"""
function call(client::ServiceClient{Req, Resp}, req::Req;
              async::Bool=false, timeout_ms::Integer=60_000) where {Req, Resp}
    isopen(client) ||
        throw(ArgumentError("call on a closed ServiceClient"))
    e = client.entity

    bytes = encode(req)
    # The :execute warm-up null-routes the wire op: `encode` above still compiles and
    # runs, but there is no server during warm-up, so hand back a default `Resp` (the
    # handler's continuation still compiles/runs) instead of issuing the query.
    if _WARMUP[]
        return async ? Threads.@spawn(_default_msg(Resp)) : _default_msg(Resp)
    end
    querier = (e.wire::_ClientWire).querier
    seq = (@atomic client.seq += 1)
    ts = nanoseconds(Dates.now(e.node, System()))      # request-time wall ns
    attach = encode_attachment(seq, ts, gid(e))

    # A querier get carries no per-call timeout in libzenoh, so bound it with a
    # `CancellationToken`: cancel fires → the in-flight get ends → the drain
    # completes cleanly (no abandoned task). A deadline timer arms it when
    # `timeout_ms > 0`, and the sync backstop also cancels on its own deadline.
    # `timeout_ms == 0` is truly unbounded: no timer, no backstop — the token
    # lives only for drain/transport teardown.
    tok = CancellationToken()
    run = function ()
        # `Base.Timer` — ROSNode's own `Timer` (ROS timer, time.jl) shadows it here.
        timer = timeout_ms > 0 ?
            Base.Timer(_ -> (try; Zenoh.cancel(tok); catch; end), timeout_ms / 1000) :
            nothing
        try
            gh = Base.get(querier; payload=bytes, attachment=attach, cancellation=tok)
            return _resolve_reply(gh, Resp, e.endpoint.topic)
        finally
            # Stop the timer so it can't fire post-return; the token drops via its
            # finalizer (closing it here could race a firing `cancel`).
            timer === nothing || close(timer)
        end
    end

    async && return Threads.@spawn run()

    # Synchronous, but **hard-bounded** so the caller can never hang forever (the
    # fail-safe contract extends to the client). We run the reply-drain on a task and wait at
    # most a deadline past `timeout_ms`; if the underlying `Zenoh.get` doesn't honor
    # its own timeout (e.g. wedged, or its I/O thread starved under transport/
    # multicast-discovery contention), we raise `ServiceError` rather than block
    # indefinitely, cancelling the token so the abandoned get/drain terminates
    # instead of leaking a blocked task per timed-out call.
    fut = Threads.@spawn run()
    # Explicit `timeout_ms == 0` is truly unbounded: wait forever for the reply.
    deadline = timeout_ms == 0 ? Inf : timeout_ms / 1000 + 2.0
    if timedwait(() -> istaskdone(fut), deadline; pollint=0.01) === :ok
        # `fetch` on a failed task throws `TaskFailedException`; unwrap it so the
        # caller sees the `ServiceError` (error reply / no reply) the service contract
        # promises, not the task wrapper.
        try
            return fetch(fut)
        catch err
            err isa TaskFailedException &&
                throw(current_exceptions(err.task)[end].exception)
            rethrow()
        end
    end
    try; Zenoh.cancel(tok); catch; end
    throw(ServiceError("service $(e.endpoint.topic) did not reply within \
                        $(round(deadline; digits=1))s (timeout_ms=$(timeout_ms), default 60000); \
                        no server, slow handler, or transport contention. \
                        Raise timeout_ms, or pass 0 to wait unbounded."))
end

# A non-`Req` payload is the common "wrong request type for this service"
# mistake; name it rather than letting a MethodError surface.
call(client::ServiceClient{Req}, req; kwargs...) where {Req} =
    throw(ArgumentError("call: expected a $(Req), got a $(typeof(req))"))

# Drain the GetHandler for the (single) reply. The first reply settles the call:
# ok → decode the Response (owned — it outlives the handler), error → raise
# ServiceError. No reply at all (timeout / no matching server) is also an error,
# since a blocked caller must not return a fabricated response.
function _resolve_reply(gh, ::Type{Resp}, service::AbstractString) where {Resp}
    for r in gh
        if is_ok(r)
            return decode_owned(sample(r), Resp)
        else
            msg = try
                String(error_payload(r))
            catch
                "service replied with an error"
            end
            throw(ServiceError(msg))
        end
    end
    throw(ServiceError("no reply from service $(service) (timeout or no server)"))
end

# ── enum-instance call-method: the do-block spelling ──────────────────────────
# `Service`/`Subscription` are `EndpointKind` *values* (core.jl re-export); the
# do-block `Service(node, name, T) do req … end` desugars function-first, so both
# route through one handler-form call-method dispatched on the kind. This is the
# single owner of that method (pubsub.jl deliberately leaves it to us — defining it
# in both files is an identical-signature overwrite, which precompile forbids): it
# fans Subscription → pubsub's `_make_subscription` and Service → `_make_service`.
# The `Client` kind has no do-block spelling — clients use `ServiceClient(…)`.
"""
    Subscription(node::Node, topic, ::Type{T};
                 qos=default_qos(), view=Owned(), concurrency=Serial(),
                 match=:exact, force_relatch=false,
                 warmup=nothing, warmup_sync=nothing, warmup_sample=nothing) do msg … end
        -> SubscriptionHandle{T}

Declare a subscription for ROS 2 message type `T` on `topic` and start its dispatch
runtime, returning a `SubscriptionHandle{T}`. The `do`-block handler runs once per
received message.

`Subscription` is the `EndpointKind` enum value (re-exported from ROSZenoh); this
call-method gives it the constructor spelling. The `do`-block desugars to a
function-first call, so the handler leads the argument list. Construction resolves the
name, builds the endpoint, declares the liveliness token and graph entry, then
opens a FIFO-channel Zenoh subscriber sized to the QoS history depth and spawns the
consumer task. The handle is tracked on `node` (reaped by `close(node)`); `close(sub)`
tears down the route and consumer.

`view` (a [`ViewMode`](@ref)) selects how each message reaches the handler:

  - `Owned()` (default) — a fully-owned decoded message, free to store, forward, or
    spawn beyond the handler.
  - `Checked()` — a zero-copy `CDRView` aliasing the payload, valid for the handler's
    duration; `collect`/`decode_owned` to keep it. A guard throws `BorrowError`
    if the view (or a `CDRString` from it) is read after the handler returns.
  - `Unchecked()` — the same zero-copy view at the zero-allocation tier with the guard
    removed. Validate under `Checked()`, then switch; an escaping `Unchecked` view is
    undefined behaviour.

`view=true`/`view=false` are shorthand for `Checked()`/`Owned()`.

`concurrency` is `Serial()` (default — one handler at a time on a single sticky task,
preserving message order with no handler-side locks; the rclcpp single-threaded-executor
model) or `Parallel(n)`, which runs up to `n` handlers across OS threads (order not
preserved). A handler throw is logged, never fatal.

`match=:exact` (default) subscribes to `T`'s exact type keyexpr, so the wire delivers
only matching-type samples. `match=:weak` declares `T` in the graph but wildcard-matches
the topic, routing an off-type sample to a per-sample backstop: a wire type or version
other than `T` fires `on_type_mismatch` and the sample is dropped.

`force_relatch=true` is the transient-local (durability) escape hatch: re-deliver
the latched history on every managed-node Active transition regardless of novelty, for
idempotent handlers that rebuild state from latched inputs. The default deduplicates
re-latch replays by payload novelty.

`qos` carries the ROS 2 QoS profile; `durability=:transient_local` opens an Advanced
subscriber that replays the publisher's latched history on join. `warmup`/`warmup_sync`/
`warmup_sample` drive precompilation of the decode + handler-dispatch chain, so it is
already compiled when the first real message arrives.

While a managed node is outside the [`Active`](@ref) lifecycle state, the handler does
not fire — this is the delivery point of the [`isactive`](@ref) dispatch gate.

```julia
sub = Subscription(node, "/chatter", std_msgs.msg.String) do msg
    @info "heard" msg.data
end
```
"""
(k::EndpointKind)(handler, node::Node, name::AbstractString, ::Type{T}; kwargs...) where {T} =
    k === Subscription ? _make_subscription(handler, node, name, T; kwargs...) :
    k === Service      ? _make_service(handler, node, name, T; kwargs...) :
        throw(ArgumentError("$(k) does not take a handler in this position \
                             (Subscription/Service do; Publisher/Client do not)"))
