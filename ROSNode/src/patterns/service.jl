# §8 services: one callback and a write-once result cell. Under rmw_zenoh a
# service is a Zenoh queryable — the request rides the query payload, the response
# the reply. The fail-safe settlement core (settlement.jl) guarantees every handler
# exit fills the cell exactly once: `return resp` → reply-ok; an explicit
# `respond!(req, failed, …)`, a throw, or a non-responding return → a Zenoh query
# error reply, so the client's blocking `call` raises rather than returning a
# plausible zeroed response. The cell settles within the handler's extent: a
# still-empty cell is force-aborted when the handler returns, then the owned
# `Query` is finalized. (Detached settlement — holding the query past handler
# return until another task fills the cell — is an action-server feature via
# `wait_settled`; the service path has no equivalent.)
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

# ── service-type reflection (§8/§11) ─────────────────────────────────────────

"""
    request_type(SrvType) -> Type

The request message type of a service `SrvType`. A generated service `Foo` lives
as two sibling structs `Foo_Request` and `Foo_Response` under `<pkg>.srv` (there
is no umbrella `Foo` type), so the service is named by its request struct: the
default treats `SrvType` as that request struct (its name ends in `_Request`) and
returns it unchanged.

The §11 type registry specializes this for service markers: `@ros_service` emits a
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

The §11 type registry specializes this for service markers alongside
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

# ── ServiceHandle (the server, §8) ───────────────────────────────────────────
# An `Entity` (node.jl) carrying a channel-form Zenoh `Queryable` plus its
# consumer task. Draining with `take!` (not the callback form) lets the consumer
# own the `Query` lifetime: it holds the owned `Query` across the handler and
# `finalize`s it only after the reply is sent, which is the final-ack the client's
# `get` waits on. The callback form drops the `Query` the instant the callback
# returns, racing the reply against the undeclare.

"""
    ServiceHandle{Req, Resp}

A running [service](https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html)
server for request type `Req` / response type `Resp` (§8). Under rmw_zenoh a
service is a Zenoh queryable: the request rides the query payload and the response
rides the reply. The handle wraps the generic [`Entity`](@ref) (id, liveliness
token, graph lifecycle) and owns the queryable route plus the consumer task that
drains it.

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
mutable struct _ServiceWire
    const queryable::Queryable
    consumer::Union{Task, Nothing}
    @atomic seq::Int64           # per-service reply sequence_number (attachment, §3.4)
end

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
    nothing
end

"""
    Service(node, name, SrvType; qos=default_qos(), view=false,
            concurrency=Serial(), warmup=nothing, warmup_sync=nothing,
            warmup_sample=nothing) do req … end -> ServiceHandle

Declare a service `name` of service type `SrvType` on `node` and start serving it
(§8), returning a [`ServiceHandle`](@ref). `Service` is the `EndpointKind` enum
value given a call-method; the do-block desugars function-first, so the handler
becomes the first argument.

The handler runs once per request. Every handler exit settles the request's
write-once cell exactly once (fail-safe settlement, settlement.jl), so the
framework always delivers exactly one reply. Returning the response message
replies-ok; `respond!(req, failed, msg)` fails the request as a Zenoh query
*error* reply, which raises [`ServiceError`](@ref) in the client's [`call`](@ref);
a throw or a return-without-responding becomes a synthesized error reply (logged
with a backtrace). The cell settles within the handler's extent: it is
force-aborted if still empty when the handler returns, then the owned `Query` is
finalized to send the final-ack. (Detached settlement — handing the request to
another task to settle after the handler returns — is an action-server feature;
in a service handler a late settle hits the already-aborted cell and the client
has the synthesized error reply.)

The request is fully owned by default (storable, forwardable, spawnable past the
handler, §3.1). `view=true` decodes it as a `CDRView` aliasing a private owned
copy of the payload — variable-length fields stay views over that copy, and the
view remains valid for as long as it is reachable, since the copy is owned
(zero-copy over the borrowed wire buffer itself is a pending §3.2 refinement).

`concurrency` is [`Serial`](@ref)`()` (the default: one request at a time on a
single sticky cooperative thread, matching rclcpp's single-threaded executor) or
[`Parallel`](@ref)`(n)` (up to `n` handlers on OS threads, rclcpp's multi-threaded
executor); when a finite pool is full, further queries buffer in the queryable
FIFO until a slot frees. `qos` maps onto the ROS 2 QoS policies for the route.

`SrvType` is the generated `*_Request` struct (the response type is its
`*_Response` sibling); the §11 registry can specialize [`request_type`](@ref) /
[`response_type`](@ref) for a registered service marker. `warmup`, `warmup_sync`,
and `warmup_sample` select the §D8 precompilation policy for the
decode→handler→encode chain, defaulting to the node's policy.

```julia
srv = Service(node, "add_two_ints", AddTwoInts_Request) do req
    AddTwoInts_Response(; sum = req.a + req.b)
end
```
"""
function _make_service(handler, node::Node, name::AbstractString, ::Type{Srv};
                       qos::QosProfile=default_qos(), view::Bool=false,
                       concurrency::Concurrency=Serial(),
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
    wire = _ServiceWire(qable, nothing, 0)
    ent.wire = wire
    wire.consumer = _spawn_service_consumer(ent, Req, Resp, handler, view, concurrency)

    # §D8: precompile request-decode → handler → response-encode (and, under
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
# (D2's "busy error-reply on saturation" for services is a pending refinement;
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
    # Sticky consumer: Serial handlers run on the node's one cooperative thread (D3).
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

"""
    respond!(req, status::SettlementStatus, payload) -> Bool

Settle the in-flight service request from inside its handler. `respond!(req,
failed, msg)` (`failed` aliases [`failure`](@ref)) fails the request — a Zenoh
query *error* reply carrying `msg`, so the client's [`call`](@ref) raises
[`ServiceError`](@ref); `respond!(req, success, resp)` replies `resp`. `req` is
the handler's argument: the result cell is taken from the handler's dynamic scope
(a `ScopedValue` bound for the handler's extent).

Returns `true` (the explicit `respond!` is the authoritative settle and always
wins the cell when it returns). Throws `ArgumentError` when called outside a
service handler, or on a request that is already settled — a second explicit
`respond!` is the double-settle error (settlement.jl); the return-value and
fail-safe paths instead yield silently to an earlier `respond!`.

This is the service-handler spelling of the settlement verb [`respond!`](@ref);
an action goal settles via `respond!(goal, …)`.

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

# rcl-level infrastructure services (`~/get_type_description`, the parameter services)
# sit BELOW the managed-node abstraction and must serve in every lifecycle state — a
# manager resolves types / reads-and-sets parameters on an inactive node. Only
# *application* services follow the §14.2 gate; matched by the resolved topic basename
# so it holds however/whenever the service was wired.
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
    try
        req = _decode_request(query, Req, view)
        cell = ResultCell{Query, Resp}(query, _service_deliver(e, query, Resp))
        # §14.2 gate: an inactive managed node's application services don't run their
        # handler — the caller gets an error reply, not a fabricated response. The
        # control surface is exempt (isactive), so lifecycle services still serve.
        # The owned query's final-ack rides the `finally` below.
        isactive(e) || _is_infra_service(e.endpoint.topic) ||
            (reply_err(query, "node inactive"); return)
        # D7: run the handler under the node logger so a plain `@info` inside it
        # routes to the node's /rosout (wherever `settle_handler!` runs the thunk).
        settle_handler!(cell,
                        () -> Base.ScopedValues.with(_ACTIVE_SERVICE_CELL => cell) do
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
        # The final-ack: drop the owned query exactly once, after any reply.
        try
            finalize(query.q)
        catch
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
# TODO(view §3.2): zero-copy directly over the borrowed query payload.
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
#     the §8 fail-safe outcome for a non-defaultable result or delivery failure.
#
# Delivery does *not* finalize the query — `_serve_query`'s `finally` does, once,
# after the handler returns and the cell has settled, separating the reply (here)
# from the final-ack (there).
function _service_deliver(e::Entity, query::Query, ::Type{Resp}) where {Resp}
    function (status::SettlementStatus, payload)
        if status === success && payload isa Resp
            bytes = encode(payload)
            seq = (@atomic (e.wire::_ServiceWire).seq += 1)
            ts = nanoseconds(Dates.now(e.node, System()))   # reply-time wall ns (§3.4)
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

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle, §6)."
entity(s::ServiceHandle) = s.entity

# ── ServiceClient + call (§8) ─────────────────────────────────────────────────
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
client for request type `Req` / response type `Resp` (§8). Constructing one
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
    @atomic seq::Int64           # per-client request sequence (attachment, §3.4)
end

function ServiceClient(node::Node, name::AbstractString, ::Type{Srv};
                       qos::QosProfile=default_qos()) where {Srv}
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
    return ServiceClient{Req, Resp}(ent, 0)
end

Base.isopen(c::ServiceClient) = isopen(c.entity)
Base.close(c::ServiceClient) = close(c.entity)
Base.show(io::IO, c::ServiceClient{Req}) where {Req} =
    print(io, "ServiceClient(", c.entity.endpoint.topic, ", ",
          _strip_suffix(string(nameof(Req)), "_Request"),
          isopen(c) ? "" : ", closed", ")")

"The underlying generic [`Entity`](@ref) (id/token/route/graph lifecycle, §6)."
entity(c::ServiceClient) = c.entity

"""
    service_matched(client::ServiceClient) -> Bool

Whether the client's `Querier` is routing-matched to at least one service server
right now — i.e. a [`call`](@ref) would actually reach a server (§12). Reads the
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
`reply_err`, §8), no matching server replied, or the call timed out. A failed
invocation always surfaces as this exception — a real reply is the only way `call`
returns a response. `msg` carries the server's error string when there is one,
otherwise a description of the missing or timed-out reply.
"""
struct ServiceError <: Exception
    msg::String
end
Base.showerror(io::IO, e::ServiceError) = print(io, "ServiceError: ", e.msg)

"""
    call(client::ServiceClient, req; async=false, timeout_ms=0) -> Resp | Task

Invoke the [service](https://docs.ros.org/en/rolling/Concepts/Basic/About-Services.html):
serialize `req`, query the client's `Querier` on the service keyexpr with the
per-request rmw_zenoh attachment (sequence number, request-time wall clock,
client gid, §3.4), and resolve the single reply (§8).

Synchronously (`async=false`, the default) this blocks the calling task — it
yields while the reply is in flight — and returns the decoded `Resp`. A failed
invocation raises [`ServiceError`](@ref): an error reply (the wire form of a
handler failure), a missing reply (no matching server), or a timeout.

With `async=true` the same query+drain runs on a spawned task, returned as a raw
`Task`; `fetch` it for the `Resp`. A failed async call surfaces through `fetch`
as a `TaskFailedException` wrapping the [`ServiceError`](@ref) — unwrap it (e.g.
via `current_exceptions(task)`) to reach the original; the synchronous path does
that unwrapping itself.

`timeout_ms` bounds the wait by arming a cancellation timer on the in-flight get
(`0` arms no timer). The synchronous path additionally applies a hard
backstop — `timeout_ms/1000 + 2` seconds when set, 60 seconds when
`timeout_ms == 0` — and when it elapses cancels the in-flight get and raises
[`ServiceError`](@ref), covering even a `Zenoh.get` that fails to honor its own
timeout. The backstop is
synchronous-only: an async call is bounded only by the cancellation timer and the
transport's own query timeout.

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
              async::Bool=false, timeout_ms::Integer=0) where {Req, Resp}
    isopen(client) ||
        throw(ArgumentError("call on a closed ServiceClient"))
    e = client.entity

    bytes = encode(req)
    # §D8 :execute warm-up null-routes the wire op: `encode` above still compiles and
    # runs, but there is no server during warm-up, so hand back a default `Resp` (the
    # handler's continuation still compiles/runs) instead of issuing the query.
    if _WARMUP[]
        return async ? Threads.@spawn(_default_msg(Resp)) : _default_msg(Resp)
    end
    querier = (e.wire::_ClientWire).querier
    seq = (@atomic client.seq += 1)
    ts = nanoseconds(Dates.now(e.node, System()))      # request-time wall ns (§3.4)
    attach = encode_attachment(seq, ts, gid(e))

    # A querier get carries no per-call timeout in libzenoh, so bound it with a
    # `CancellationToken`: cancel fires → the in-flight get ends → the drain
    # completes cleanly (no abandoned task). A deadline timer arms it when
    # `timeout_ms > 0`; the sync backstop cancels it on its own deadline (so the
    # token exists even at `timeout_ms == 0`); an unbounded async call leans on
    # the transport's query timeout.
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

    # Synchronous, but **hard-bounded** so the caller can never hang forever (§8
    # fail-safe extends to the client). We run the reply-drain on a task and wait at
    # most a deadline past `timeout_ms`; if the underlying `Zenoh.get` doesn't honor
    # its own timeout (e.g. wedged, or its I/O thread starved under transport/
    # multicast-discovery contention), we raise `ServiceError` rather than block
    # indefinitely, cancelling the token so the abandoned get/drain terminates
    # instead of leaking a blocked task per timed-out call.
    fut = Threads.@spawn run()
    deadline = timeout_ms > 0 ? timeout_ms / 1000 + 2.0 : 60.0
    if timedwait(() -> istaskdone(fut), deadline; pollint=0.01) === :ok
        # `fetch` on a failed task throws `TaskFailedException`; unwrap it so the
        # caller sees the `ServiceError` (error reply / no reply) the §8 contract
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
    throw(ServiceError("no reply from service $(e.endpoint.topic) within \
                        $(round(deadline; digits=1))s (timeout, no server, or transport contention)"))
end

# A non-`Req` payload is the common "wrong request type for this service"
# mistake; name it rather than letting a MethodError surface.
call(client::ServiceClient{Req}, req; kwargs...) where {Req} =
    throw(ArgumentError("call: expected a $(Req), got a $(typeof(req))"))

# Drain the GetHandler for the (single) reply. The first reply settles the call:
# ok → decode the Response (owned — it outlives the handler), error → raise
# ServiceError. No reply at all (timeout / no matching server) is also an error,
# since a blocked caller must not return a fabricated response (§8).
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

# ── enum-instance call-method: the §6/§8 do-block spelling ────────────────────
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
runtime (§4), returning a `SubscriptionHandle{T}`. The `do`-block handler runs once per
received message.

`Subscription` is the `EndpointKind` enum value (re-exported from ROSZenoh); this
call-method gives it the constructor spelling. The `do`-block desugars to a
function-first call, so the handler leads the argument list. Construction resolves the
name, builds the endpoint, declares the liveliness token and graph entry (§12), then
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

`concurrency` is `Serial()` (default, D3 — one handler at a time on a single sticky task,
preserving message order with no handler-side locks; the rclcpp single-threaded-executor
model) or `Parallel(n)`, which runs up to `n` handlers across OS threads (order not
preserved). A handler throw is logged, never fatal.

`match=:exact` (default) subscribes to `T`'s exact type keyexpr, so the wire delivers
only matching-type samples. `match=:weak` declares `T` in the graph but wildcard-matches
the topic, routing an off-type sample to a per-sample backstop: a wire type or version
other than `T` fires `on_type_mismatch` and the sample is dropped.

`force_relatch=true` is the transient-local (durability) escape hatch (D4): re-deliver
the latched history on every managed-node Active transition regardless of novelty, for
idempotent handlers that rebuild state from latched inputs. The default deduplicates
re-latch replays by payload novelty.

`qos` carries the ROS 2 QoS profile; `durability=:transient_local` opens an Advanced
subscriber that replays the publisher's latched history on join. `warmup`/`warmup_sync`/
`warmup_sample` drive §D8 precompilation of the decode + handler-dispatch chain, so it is
already compiled when the first real message arrives.

While a managed node is outside the `Active` lifecycle state, the handler does not fire
(the §14.2 dispatch gate).

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
