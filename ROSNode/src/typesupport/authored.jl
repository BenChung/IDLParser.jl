# Authored Julia types — the reverse pipeline (Julia type → ROS interface). A user
# struct/handler is the source of truth; reflection (ROSMessages.il_from_type) yields the
# IL, and everything downstream reuses the @ros_import generate→register→resolve path.
#
# Registration is deferred to module-load (`absorb_static_types!`, the authored drain
# arm in static_types.jl): a struct's IL — and thus its RIHS — isn't known until its
# nested types are registered, which happens in source order at load. The macros emit
# only (a) the structs into `<pkg>.<qual>` and (b) a `(Type, name)` push + flush hooks.

# ── @ros_package ─────────────────────────────────────────────────────────────────
"""
    @ros_package "pkg"

Set the module-local default ROS 2 package name for the authored macros (`@ros_message`,
`@ros_service`, `@ros_action`). Expands to `const __ros_package__ = "pkg"` in the calling
module, which those macros read so a package name is written once per module.

Takes a string literal; any non-string argument is a macro-expansion error. The interface
name then comes from the Julia struct/handler name and the qualifier (`msg`/`srv`/`action`)
from which macro is used, giving the full ROS identity `pkg/<qual>/Name`. To override the
package for one interface (a cross-package type, or a Julia name that differs from the ROS
name), use a macro's explicit `"pkg/<qual>/Name"` form, which needs no `@ros_package`.

```julia
module MyInterfaces
    using ROSNode
    @ros_package "my_robot_msgs"
    @ros_message struct Pose; x::Float64; y::Float64; theta::Float64; end  # → my_robot_msgs/msg/Pose
end
```
"""
macro ros_package(pkg)
    pkg isa AbstractString || error("@ros_package takes a string literal, e.g. `@ros_package \"my_pkg\"`")
    return esc(:(const __ros_package__ = $(String(pkg))))
end

function _caller_package(caller::Module)
    isdefined(caller, :__ros_package__) || error(
        "no `@ros_package` set in $(caller) — add `@ros_package \"my_pkg\"` (or use the explicit \
         `@ros_message \"pkg/msg/Name\" …` form)")
    return String(getfield(caller, :__ros_package__))
end

# ── cross-macro merge: fold a freshly-built `module <pkg>` into the live one ──────
# Each authored macro is its own top-level statement; a second top-level `module <pkg>`
# would rebind/wipe the first. So `<pkg>` is created fresh on first sighting (a real
# top-level `module`); after that, new members fold in through the live module's own
# `eval` with a BARE `Expr(:toplevel,…)` (a new `<qual>` submodule likewise via
# `<pkg>.eval(:(module <qual> end))`).
# Define structs into `<pkg>.<qual>`, resolving each field's type IN THE CALLER (so the refs the
# user wrote — sibling/imported messages, vendored `UUID`/`Time` — resolve where they're visible)
# and splicing the resolved `Type` into the struct. `specs :: [(name::Symbol, [(field::Symbol,
# type_expr)…])]`. A generated cross-ref (a wrapper → its section) is written as the qualified
# `<pkg>.<qual>.<Name>` path, caller-resolvable once the earlier struct in `specs` has been eval'd.
function _emit_structs(caller::Module, pkg::AbstractString, qual::Symbol, specs)
    pkgsym = Symbol(pkg)
    out = Any[]
    if !(isdefined(caller, pkgsym) && getfield(caller, pkgsym) isa Module)
        push!(out, Expr(:module, true, pkgsym, Expr(:block, Expr(:module, true, qual, Expr(:block)))))
    elseif !(isdefined(getfield(caller, pkgsym), qual) && getfield(getfield(caller, pkgsym), qual) isa Module)
        push!(out, :( $(pkgsym).eval(Expr(:module, true, $(QuoteNode(qual)), Expr(:block))) ))
    end
    qualpath = Expr(:., pkgsym, QuoteNode(qual))               # <pkg>.<qual>
    for (name, fields) in specs
        decls = Any[:(Expr(:(::), $(QuoteNode(fn)), $(ft))) for (fn, ft) in fields]   # $(ft) → Type in caller
        # `@kwdef` so the action/service runtime can build wrappers/sections by keyword
        # (e.g. `SendGoal_Request(; goal_id=…, goal=…)`, client.jl); the positional ctor the
        # generic CDR reader and `struct_from_nt` use is retained.
        push!(out, :( $(qualpath).eval(Expr(:macrocall, GlobalRef(Base, Symbol("@kwdef")),
            LineNumberNode(0), Expr(:struct, false, $(QuoteNode(name)), Expr(:block, $(decls...))))) ))
    end
    return out
end

# Field `(name, type_expr)` pairs of a plain `struct Name; f::T; … end` (define form).
function _struct_fields(s::Expr)
    out = Tuple{Symbol,Any}[]
    for d in s.args[3].args
        d isa LineNumberNode && continue
        (d isa Expr && d.head === :(::)) ||
            error("@ros_message: v1 supports plain `field::Type` lines only, got `$(d)`")
        push!(out, (d.args[1]::Symbol, d.args[2]))
    end
    return out
end

# ── emitting the authored registration (deferred reflection at load) ──────────────
# Mirrors `_static_register_stmts` but targets `__ros_authored_types__` of `(Type, fqn)`
# pairs (no JSON — the IL/RIHS is reflected at drain). Also seeds `__ros_resolve__`; the
# actual `_merge_resolve!` runs at drain once the RIHS is known.
const _AUTHORED_GLOBAL = :__ros_authored_types__

function _authored_register_stmts(pairs)
    g = _AUTHORED_GLOBAL
    pushpairs = [:(($(te), $(fqn))) for (te, fqn) in pairs]
    return Any[
        :(if !$(Expr(:isdefined, g)); global $(g) = Tuple{Type, String}[]; end),
        :(push!($(g), $(pushpairs...))),
        :(if !$(Expr(:isdefined, _RESOLVE_GLOBAL))
              global $(_RESOLVE_GLOBAL) = Dict{Symbol, ROSNode.ResolveEntry}()
          end),
        :(if ccall(:jl_generating_output, Cint, ()) == 0
              ROSNode.absorb_static_types!(@__MODULE__)
          end),
        :(if !$(Expr(:isdefined, :__init__))
              function __init__()
                  ROSNode.absorb_static_types!(@__MODULE__)
              end
          end),
    ]
end

# ── @ros_message ─────────────────────────────────────────────────────────────────
"""
    @ros_message struct Name … end          # define a new struct and register it
    @ros_message Name                        # mark an existing top-level struct
    @ros_message "pkg/msg/Name" struct Name … end   # explicit package (cross-package define)
    @ros_message "pkg/msg/Name" JuliaName    # explicit, when the Julia name ≠ the ROS name

Author a Julia struct as the ROS 2 message `pkg/msg/Name`, the reverse of `@ros_import`:
the struct is the source of truth and its RIHS01 type hash is computed by reflecting its
fields. `pkg` comes from `@ros_package`, or from an explicit `"pkg/msg/Name"` first
argument (which also serves cross-package definitions and renames). An explicit name's
qualifier must be `msg`, and an explicit define form's struct name must equal the ROS
name — a differently-named Julia struct binds through the annotate form.

The struct lives at `pkg.msg.Name` inside the calling module, where service and action
sections can reach it via `import ..msg: Name`. The define form also binds the bare `Name`
in the caller as a `const` alias; the annotate form adds the `pkg.msg.Name` alias around
the user's existing top-level struct, which stays usable under its original name. Field
types map per the reflection table: the integer/float/Bool primitives and `String` map to
their ROS scalars (`UInt8` → `uint8`), `Vector{T}` is an unbounded sequence, an
`SVector`/`SArray`/`NTuple{N,T}` is a fixed array `[N]`, and a concrete nested struct
becomes a reference whose own ROS name must already be registered (`@ros_message` or
`@ros_import` it first) or be a vendored type with a `ros_type_name`. `Char`, `Dict`,
abstract, and parametric types have no ROS image and are rejected.

Registration happens at module load: the macro pushes `(Type, fqn)` onto a module global
and drains it with `absorb_static_types!` — right after the declaration in a script/REPL
module, or from a generated `__init__` in a precompiled package. The drain reflects each
type to its IL, computes the real RIHS01, and folds the hash into the module's resolve
table so a dynamic subscriber maps the wire RIHS back to this struct. Declare nested
messages before the structs that reference them — the drain runs in source order and a
struct's hash needs its nested types already registered.

```julia
@ros_package "my_robot_msgs"
@ros_message struct Vec3; x::Float64; y::Float64; z::Float64; end
@ros_message struct Stamped; v::Vec3; label::String; n::Int32; end  # nested ref to Vec3
```
"""
macro ros_message(args...)
    if length(args) == 1
        arg = args[1]
        pkg = _caller_package(__module__)
        if arg isa Expr && arg.head === :struct
            return esc(_emit_message_define(__module__, pkg, _struct_name(arg), arg))
        elseif arg isa Symbol
            return esc(_emit_message_annotate(__module__, pkg, arg))
        end
    elseif length(args) == 2
        fqn, arg = args
        fqn isa AbstractString ||
            error("@ros_message: explicit form is `@ros_message \"pkg/msg/Name\" <struct|Name>`")
        pkg, qual, rname = split_ros_name(String(fqn))
        qual == "msg" || error("@ros_message: explicit name must be `pkg/msg/Name`, got \"$(fqn)\"")
        rsym = Symbol(rname)
        if arg isa Expr && arg.head === :struct
            _struct_name(arg) === rsym || error("@ros_message: in the explicit define form the struct " *
                "name ($(_struct_name(arg))) must equal the ROS name ($(rname)); use the annotate form " *
                "`@ros_message \"$(fqn)\" $(_struct_name(arg))` to bind a differently-named struct")
            return esc(_emit_message_define(__module__, pkg, rsym, arg))
        elseif arg isa Symbol
            return esc(_emit_message_annotate(__module__, pkg, rsym, arg))   # rosname, julia-name
        end
    end
    error("@ros_message: expected `struct Name … end`, `Name`, or `\"pkg/msg/Name\" <struct|Name>`")
end

function _struct_name(s::Expr)
    n = s.args[2]
    n isa Symbol && return n
    (n isa Expr && n.head === :(<:) && n.args[1] isa Symbol) && return n.args[1]
    (n isa Expr && n.head === :curly) && error("@ros_message: parametric structs are unsupported in v1")
    error("@ros_message: could not read the struct name")
end

function _emit_message_define(caller::Module, pkg::AbstractString, name::Symbol, structexpr::Expr)
    pkgsym = Symbol(pkg)
    block  = Expr(:toplevel)
    append!(block.args, _emit_structs(caller, pkg, :msg, [(name, _struct_fields(structexpr))]))
    target = Expr(:., Expr(:., pkgsym, QuoteNode(:msg)), QuoteNode(name))   # <pkg>.msg.Name
    push!(block.args, Expr(:const, Expr(:(=), name, target)))
    append!(block.args, _authored_register_stmts([(target, string(pkg, "/msg/", name))]))
    return block
end

# Annotate: alias the caller's existing top-level struct `jsym` into `<pkg>.msg` under the ROS
# name `rosname` (Type spliced at eval, where `jsym` exists), then register. `jsym` may differ
# from `rosname` (the explicit `"pkg/msg/Name" T` form). The struct stays usable at the caller too.
function _emit_message_annotate(caller::Module, pkg::AbstractString, rosname::Symbol, jsym::Symbol=rosname)
    pkgsym = Symbol(pkg)
    name   = rosname
    block  = Expr(:toplevel)
    has_pkg = isdefined(caller, pkgsym) && getfield(caller, pkgsym) isa Module
    has_msg = has_pkg && isdefined(getfield(caller, pkgsym), :msg) &&
              getfield(getfield(caller, pkgsym), :msg) isa Module
    aliasconst = :(Expr(:const, Expr(:(=), $(QuoteNode(rosname)), $(jsym))))   # const <rosname> = <live jsym>
    if has_msg
        push!(block.args, :( $(pkgsym).msg.eval($aliasconst) ))
    elseif has_pkg
        push!(block.args, :( $(pkgsym).eval(Expr(:module, true, :msg, Expr(:block, $aliasconst))) ))
    else
        push!(block.args, Expr(:module, true, pkgsym, Expr(:block, Expr(:module, true, :msg, Expr(:block)))))
        push!(block.args, :( $(pkgsym).msg.eval($aliasconst) ))
    end
    target = Expr(:., Expr(:., pkgsym, QuoteNode(:msg)), QuoteNode(name))
    append!(block.args, _authored_register_stmts([(target, string(pkg, "/msg/", name))]))
    return block
end

# ── deferred reflection intern (called from absorb_static_types!' authored arm) ───
# Reflect `T` → IL → TypeDescription (closure from already-registered nested entries) →
# RIHS01 → RegistryEntry(:authored). Nested names resolve through the registry (`_entry_of`),
# which is populated nested-first by the source-order drain — a hard error if absent.
function _intern_authored_entry!(@nospecialize(T), fqn::AbstractString)
    @lock _STATIC_ENTRY_LOCK begin
        haskey(_STATIC_ENTRY_CACHE, T) && return _STATIC_ENTRY_CACHE[T]
        pkg, _, _ = split_ros_name(fqn)
        il   = ROSMessages.il_from_type(T; name_of=_authored_name_of)
        ast  = _scan_for_struct(lower(il; package=pkg))
        ast === nothing && return nothing
        main = type_description_from_struct(ast, fqn; package=pkg, qualifier="msg")
        pool = Dict{String, TypeDescription}()
        for S in _nested_struct_types(T)
            _dep_into_pool!(pool, S, fqn)
        end
        tdmsg = TypeDescriptionMsg(main, _collect_td_closure(main, pool))
        hash  = type_hash_from_rihs_string(calculate_rihs01_hash(tdmsg))
        hash === nothing && return nothing
        e = RegistryEntry(TypeInfo(fqn, hash), lift(tdmsg); td=tdmsg, provenance=:authored)
        e.mod  = parentmodule(T)
        e.type = T
        _record_type_entry!(e)
        _STATIC_ENTRY_CACHE[T] = e
        return e
    end
end

# A nested type's ROS name: the registry first (authored/imported), else `ros_type_name` (a vendored
# type the action wrappers reference — UUID/Time — resolved from its module nesting). A bare
# (non-qualified) result means an unregistered user type: a hard error, not a silent wrong RRef.
function _authored_name_of(@nospecialize(S))
    e = _entry_of(S)
    e === nothing || return e.info.name
    n = try; ros_type_name(S); catch; ""; end
    occursin('/', n) || error("authored: nested type $(S) has no registered ROS name — " *
                              "@ros_message or @ros_import it first")
    return n
end

# Add nested type `S`'s `TypeDescription` (and its own closure) to the RIHS `pool`. An authored/
# imported type comes from the registry (`_entry_of`); a vendored protocol type the wrappers
# reference (UUID/Time — not registry-resident) is reflected on the fly. Recurses (UUID/Time are leaves).
function _dep_into_pool!(pool, @nospecialize(S), ctx::AbstractString)
    e = _entry_of(S)
    if e !== nothing && e.td !== nothing
        pool[e.td.type_description.type_name] = e.td.type_description
        for r in e.td.referenced_type_descriptions; pool[r.type_name] = r; end
        return
    end
    vname = _authored_name_of(S)
    haskey(pool, vname) && return
    pkg  = split_ros_name(vname)[1]
    vast = _scan_for_struct(lower(ROSMessages.il_from_type(S; name_of=_authored_name_of); package=pkg))
    vast === nothing && error("authored: could not reflect dependency $(S) (referenced by $(ctx))")
    pool[vname] = type_description_from_struct(vast, vname; package=pkg, qualifier="msg")
    for S2 in _nested_struct_types(S); _dep_into_pool!(pool, S2, ctx); end
end

# Nested *message* struct types referenced by `T`'s fields (through array/tuple element
# types), for assembling the RIHS closure. Primitives/String/array-of-primitive contribute none.
function _nested_struct_types(@nospecialize(T))
    out = Any[]
    for ft in fieldtypes(T)
        S = _elem_struct(ft)
        S === nothing || push!(out, S)
    end
    return out
end

function _elem_struct(@nospecialize(ft))
    E = ft <: AbstractArray ? eltype(ft) :
        (ft <: Tuple && isconcretetype(ft) && fieldcount(ft) >= 1) ? fieldtype(ft, 1) : ft
    (isconcretetype(E) && isstructtype(E) && !(E <: Number) && E !== String &&
     !(E <: AbstractArray) && !(E <: Tuple)) ? E : nothing
end

# ── @ros_service ─────────────────────────────────────────────────────────────────
"""
    @ros_service function Name(field::T, …)::@NamedTuple{out::U, …} … end

Author a ROS 2 service `pkg/srv/Name` from a Julia function: the argument list is the
request fields and the `@NamedTuple` return annotation is the response fields (the return
annotation is required). `pkg` comes from `@ros_package`. The macro generates the
`Name_Request` and `Name_Response` section structs under `pkg.srv` (field types map per
the `@ros_message` reflection table) and registers their RIHS01 hashes at module load.

The named function itself becomes the runtime identity. `request_type`/`response_type`
are specialized on `typeof(Name)`, so:

  - `Service(node, ke, Name)` serves it — the request's fields are splatted positionally
    into the handler and the returned `@NamedTuple` is rebuilt into a `Name_Response` (a
    value that is already a `Name_Response` is forwarded as-is).
  - `ServiceClient(node, ke, Name)` plus `call(client; field=…)` mirrors it, taking
    request fields by keyword and returning the response as a `@NamedTuple`.

Return the response named tuple for a reply-ok. To fail, `throw`: the exception is caught
and turned into a `respond!(req, failed, message)`, which this package maps to a Zenoh
query error reply under rmw_zenoh (§8). The client raises a `ServiceError` carrying that
message, so a blocked call never returns a plausible zeroed response.

```julia
@ros_package "example_interfaces"
@ros_service function AddTwoInts(a::Int64, b::Int64)::@NamedTuple{sum::Int64}
    (sum = a + b,)
end
# server: Service(node, "add_two_ints", AddTwoInts)
# client: call(ServiceClient(node, "add_two_ints", AddTwoInts); a=2, b=3)  # → (sum = 5,)
```
"""
macro ros_service(f)
    pkg = _caller_package(__module__)
    (fname, args, rettype) = _parse_handler_sig(f)
    rettype === nothing &&
        error("@ros_service: the handler needs a `::@NamedTuple{…}` return annotation (the response fields)")
    return esc(_emit_service(__module__, pkg, fname, args, _nt_fields(rettype), f))
end

function _emit_service(caller::Module, pkg::AbstractString, fname::Symbol, args, resp_fields, f)
    pkgsym   = Symbol(pkg)
    reqname  = Symbol(fname, "_Request")
    respname = Symbol(fname, "_Response")
    block = Expr(:toplevel)
    push!(block.args, f)                                                  # handler = marker + body
    append!(block.args, _emit_structs(caller, pkg, :srv, [(reqname, args), (respname, resp_fields)]))
    reqpath  = Expr(:., Expr(:., pkgsym, QuoteNode(:srv)), QuoteNode(reqname))
    resppath = Expr(:., Expr(:., pkgsym, QuoteNode(:srv)), QuoteNode(respname))
    push!(block.args, :(ROSNode.request_type(::Type{typeof($fname)})  = $reqpath))
    push!(block.args, :(ROSNode.response_type(::Type{typeof($fname)}) = $resppath))
    append!(block.args, _authored_register_stmts([
        (reqpath,  string(pkg, "/srv/", reqname)),
        (resppath, string(pkg, "/srv/", respname))]))
    return block
end

# Parse `function Name(a::T, …)::Ret … end` → (Name, [(arg, Texpr)…], Ret-expr | nothing).
function _parse_handler_sig(f::Expr)
    (f.head === :function || f.head === :(=)) ||
        error("@ros_service/@ros_action expects a `function …` definition")
    sig = f.args[1]; rettype = nothing
    if sig isa Expr && sig.head === :(::)
        rettype = sig.args[2]; sig = sig.args[1]
    end
    (sig isa Expr && sig.head === :call && sig.args[1] isa Symbol) ||
        error("@ros_service/@ros_action: could not parse the handler signature")
    args = Tuple{Symbol, Any}[]
    for a in sig.args[2:end]
        (a isa Expr && a.head === :(::)) ||
            error("@ros_service/@ros_action: handler arg `$(a)` needs a type annotation")
        push!(args, (a.args[1]::Symbol, a.args[2]))
    end
    return (sig.args[1]::Symbol, args, rettype)
end

# Field (name, type-expr) pairs from a `@NamedTuple{a::T, …}` expression.
function _nt_fields(ntexpr)
    (ntexpr isa Expr && ntexpr.head === :macrocall && ntexpr.args[1] === Symbol("@NamedTuple")) ||
        error("@ros_service/@ros_action: the return/feedback must be a `@NamedTuple{…}`, got $(ntexpr)")
    braces = ntexpr.args[end]
    (braces isa Expr && braces.head === :braces) || error("malformed @NamedTuple")
    out = Tuple{Symbol, Any}[]
    for fld in braces.args
        (fld isa Expr && fld.head === :(::)) || error("@NamedTuple field `$(fld)` needs a type")
        push!(out, (fld.args[1]::Symbol, fld.args[2]))
    end
    return out
end

# ── service runtime: adapter + Function-marker dispatch + client (defined once) ───
# Splat the decoded request's fields into the handler; build the response struct from its
# returned @NamedTuple (by field name). A throw maps to a message-preserving `failed` reply
# so its text reaches the client's `ServiceError`.
function _authored_service_adapter(f::F) where {F<:Function}
    Req = request_type(F); Resp = response_type(F)
    return function (req)
        out = try
            f((getfield(req, n) for n in fieldnames(Req))...)
        catch e
            respond!(req, failed, sprint(showerror, e)); return nothing
        end
        out === nothing && return nothing
        return out isa Resp ? out : ROSMessages.struct_from_nt(Resp, out)
    end
end

# `Service(node, name, f::Function)` — the function is marker + handler (3-arg: distinct from
# the 4-arg do-block method and the `Publisher`/`Subscription` `::Type` 3-arg method).
(k::EndpointKind)(node::Node, name::AbstractString, f::F; kwargs...) where {F<:Function} =
    k === Service ?
        _make_service(_authored_service_adapter(f), node, name, F; kwargs...) :
        throw(ArgumentError("$(k)(node, name, ::Function) is only valid for Service"))

ServiceClient(node::Node, name::AbstractString, f::F; kwargs...) where {F<:Function} =
    ServiceClient(node, name, typeof(f); kwargs...)

# kwargs in, @NamedTuple out — mirrors the handler. (Struct-passing `call(client, req)` stays.)
function call(client::ServiceClient{Req, Resp}; async::Bool=false, timeout_ms::Integer=0, fields...) where {Req, Resp}
    out = call(client, ROSMessages.struct_from_nt(Req, NamedTuple(fields)); async=async, timeout_ms=timeout_ms)
    return async ? out : ROSMessages.nt_from_struct(out)
end

# ── @ros_action ──────────────────────────────────────────────────────────────────
"""
    @ros_action function Name(goalfield::T, …, fb::FeedbackSink{@NamedTuple{ff::V, …}})::@NamedTuple{rf::W, …} … end

Author a ROS 2 action `pkg/action/Name` from a Julia function. The arguments other than the
`FeedbackSink` parameter are the Goal fields, the sink's `@NamedTuple` parameter gives the
Feedback fields, and the `@NamedTuple` return annotation gives the Result fields. `pkg`
comes from `@ros_package`; the return annotation and exactly one `fb::FeedbackSink{…}`
parameter are required.

The macro generates eight structs under `pkg.action` — the `Name_Goal`/`_Result`/`_Feedback`
sections (field types map per the `@ros_message` reflection table), then the five rosidl
protocol wrappers `Name_SendGoal_Request`/`_SendGoal_Response`/`_GetResult_Request`/
`_GetResult_Response`/`_FeedbackMessage` — and registers all eight RIHS01 hashes
nested-first at module load. The wrappers carry the vendored
`unique_identifier_msgs/UUID` and `builtin_interfaces/Time` types, so their hashes match a
ROS 2 peer's.

The named function is the runtime identity: `ActionTypeSupport`, `_action_wrapper`, and
the feedback-parameter index are specialized on `typeof(Name)`. `ActionServer(node, ke,
Name)` serves it — the Goal's fields are splatted positionally into the handler and a
concrete `FeedbackSink{Name_Feedback}` is injected at the recorded parameter position; the
returned `@NamedTuple` is rebuilt into a `Name_Result`. `ActionClient(node, ke, Name)`
plus `send(client; field=…)` dispatches a goal by keyword. Inside the handler, `fb((ff=…,))`
publishes one feedback message and acts as a cooperative cancellation checkpoint (it
throws `Cancelled` if the goal is `CANCELING`); `goal_handle(fb)` recovers the server-side
`GoalHandle`. A normal return settles the goal `SUCCEEDED`, a propagating `Cancelled`
settles it `CANCELED`, and any other thrown exception settles it `ABORTED` (the goal-state
machine of ROS 2 actions).

```julia
@ros_package "my_robot_msgs"
@ros_action function Counter(target::Int32,
        fb::FeedbackSink{@NamedTuple{at::Int32}})::@NamedTuple{total::Int32}
    n = Int32(0)
    while n < target
        n += Int32(1)
        fb((at = n,))            # publish feedback + cancellation checkpoint
        sleep(0.02)
    end
    (total = n,)
end
```
"""
macro ros_action(f)
    pkg = _caller_package(__module__)
    (fname, args, rettype) = _parse_handler_sig(f)
    rettype === nothing &&
        error("@ros_action: the handler needs a `::@NamedTuple{…}` return annotation (the result fields)")
    return esc(_emit_action(__module__, pkg, fname, args, rettype, f))
end

_is_feedbacksink(t) = t === :FeedbackSink ||
                      (t isa Expr && t.head === :curly && t.args[1] === :FeedbackSink)
function _feedbacksink_nt(t)
    (t isa Expr && t.head === :curly && t.args[1] === :FeedbackSink && length(t.args) == 2) ||
        error("@ros_action: the feedback sink must be `FeedbackSink{@NamedTuple{…}}`, got $(t)")
    return t.args[2]
end

# Rebuild the handler with the `FeedbackSink` parameter stripped to an untyped `fb` (the adapter
# injects a concrete `FeedbackSink{<Name>_Feedback}` there); keep the Goal-field types + return.
function _rebuild_handler(f::Expr, fname::Symbol, args, fb_pos::Int, rettype)
    callargs = Any[i == fb_pos ? args[i][1] : Expr(:(::), args[i][1], args[i][2]) for i in eachindex(args)]
    return Expr(:function, Expr(:(::), Expr(:call, fname, callargs...), rettype), f.args[2])
end

function _emit_action(caller::Module, pkg::AbstractString, fname::Symbol, args, rettype, f)
    pkgsym = Symbol(pkg)
    fb_pos = findfirst(a -> _is_feedbacksink(a[2]), args)
    fb_pos === nothing &&
        error("@ros_action: the handler needs a `fb::FeedbackSink{@NamedTuple{…}}` parameter")
    feedback_fields = _nt_fields(_feedbacksink_nt(args[fb_pos][2]))
    goal_args     = Tuple{Symbol,Any}[args[i] for i in eachindex(args) if i != fb_pos]
    result_fields = _nt_fields(rettype)
    goalsym, resultsym, fbsym = Symbol(fname, "_Goal"), Symbol(fname, "_Result"), Symbol(fname, "_Feedback")
    actpath(sym) = Expr(:., Expr(:., pkgsym, QuoteNode(:action)), QuoteNode(sym))   # <pkg>.action.<sym>
    actmod = Expr(:., pkgsym, QuoteNode(:action))
    # Protocol-internal types resolve in the caller to the always-present vendored copy.
    uuidexpr = :(ROSNode.Interfaces.unique_identifier_msgs.msg.UUID)
    timeexpr = :(ROSNode.Interfaces.builtin_interfaces.msg.Time)
    # Sections (user field types) precede the five wrappers (fixed rosidl shapes); a wrapper's ref to
    # a section is the qualified `<pkg>.action.<Name>` path, caller-resolvable once the section evals.
    specs = Any[
        (goalsym,   goal_args),
        (resultsym, result_fields),
        (fbsym,     feedback_fields),
        (Symbol(fname, "_SendGoal_Request"),  [(:goal_id, uuidexpr), (:goal, actpath(goalsym))]),
        (Symbol(fname, "_SendGoal_Response"), [(:accepted, :Bool), (:stamp, timeexpr)]),
        (Symbol(fname, "_GetResult_Request"),  [(:goal_id, uuidexpr)]),
        (Symbol(fname, "_GetResult_Response"), [(:status, :Int8), (:result, actpath(resultsym))]),
        (Symbol(fname, "_FeedbackMessage"),    [(:goal_id, uuidexpr), (:feedback, actpath(fbsym))]),
    ]
    block = Expr(:toplevel)
    push!(block.args, _rebuild_handler(f, fname, args, fb_pos, rettype))     # handler = marker + body
    append!(block.args, _emit_structs(caller, pkg, :action, specs))
    # Wire the action runtime to `typeof(fname)` (the reflective default keys on `nameof(A)`,
    # which a function type lacks): specialize the support + wrapper + fb-index resolvers.
    push!(block.args, :(ROSNode.ActionTypeSupport(::Type{typeof($fname)}) =
        ROSNode.ActionTypeSupport{typeof($fname), $(actpath(goalsym)), $(actpath(resultsym)), $(actpath(fbsym))}()))
    push!(block.args, :(ROSNode._action_wrapper(::Type{typeof($fname)}, suffix::AbstractString) =
        getfield($actmod, Symbol($(string(fname)), suffix))))
    push!(block.args, :(ROSNode._feedback_param_index(::Type{typeof($fname)}) = $fb_pos))
    # Register all eight types nested-first: the three sections, then the five rosidl wrappers.
    # The qualifier-carrying `lower` makes a wrapper's section ref reflect to `pkg/action/<Section>`,
    # so each wrapper's RIHS matches a ROS2 peer's (the section TDs are interned by the time a
    # source-order drain reaches the wrappers).
    regpairs = Tuple{Any,String}[
        (actpath(goalsym),   string(pkg, "/action/", goalsym)),
        (actpath(resultsym), string(pkg, "/action/", resultsym)),
        (actpath(fbsym),     string(pkg, "/action/", fbsym))]
    for suf in ("_SendGoal_Request", "_SendGoal_Response", "_GetResult_Request", "_GetResult_Response", "_FeedbackMessage")
        wsym = Symbol(fname, suf)
        push!(regpairs, (actpath(wsym), string(pkg, "/action/", wsym)))
    end
    append!(block.args, _authored_register_stmts(regpairs))
    return block
end

# ── action runtime: FeedbackSink + adapter + Function-marker dispatch (defined once) ───
# Named FeedbackSink because `Feedback` is a SettlementStatus in core.jl.
"""
    FeedbackSink{F}

The feedback handle passed to an authored action handler (`@ros_action`). `F` is the
action's generated `Name_Feedback` struct; the adapter constructs the sink around the live
`GoalHandle` and injects it at the handler's `fb::FeedbackSink` parameter position.

Calling it publishes one feedback message on the action's feedback topic and is a
cooperative cancellation checkpoint, throwing `Cancelled` first if the goal is `CANCELING`
(so a feedback loop is also a cancellation-observation loop). Two call forms:

  - `fb(nt::NamedTuple)` builds an `F` from the named tuple by field name, then publishes.
  - `fb(msg::F)` publishes an already-built `Name_Feedback` struct.

A publish after the goal has settled is dropped. [`goal_handle`](@ref) recovers the
underlying `GoalHandle` for the lower-level verbs (`succeed`, `abort`, `state`, `respond!`).
"""
struct FeedbackSink{F}
    gh::GoalHandle
end
(fb::FeedbackSink{F})(nt::NamedTuple) where {F} = (feedback!(fb.gh, ROSMessages.struct_from_nt(F, nt)); nothing)
(fb::FeedbackSink{F})(msg::F)         where {F} = (feedback!(fb.gh, msg); nothing)

"""
    goal_handle(fb::FeedbackSink) -> GoalHandle

Recover the server-side `GoalHandle` that a `FeedbackSink` wraps, from inside an authored
action handler (`@ros_action`). Use it to reach the lower-level goal verbs beyond the
sink's surface: `state(goal)`, `iscancelled(goal)`, `checkpoint(goal)`, and explicit
settlement via `succeed`/`abort`/`respond!`.
"""
goal_handle(fb::FeedbackSink) = fb.gh

_feedback_param_index(::Type) = error("authored action: handler not registered (no _feedback_param_index)")
_splice(xs::AbstractVector, x, pos::Integer) = (ys = collect(Any, xs); insert!(ys, pos, x); ys)

# Splat the decoded Goal's fields into the handler, injecting a `FeedbackSink{<Feedback>}` at the
# recorded position; build the Result struct from the returned @NamedTuple. A throw → ABORTED, a
# `Cancelled` (from `fb`) → CANCELED (settle_handler!), so the adapter just returns the result.
function _authored_action_adapter(f::FN, ::Type{AT}) where {FN<:Function, AT}
    support = ActionTypeSupport(AT)
    G = goal_type(support); R = result_type(support); FB = feedback_type(support)
    fb_pos = _feedback_param_index(AT)
    return function (g::GoalHandle)
        sink    = FeedbackSink{FB}(g)
        gfields = Any[getfield(g.request, n) for n in fieldnames(G)]
        out     = f(_splice(gfields, sink, fb_pos)...)
        return out isa R ? out : ROSMessages.struct_from_nt(R, out)
    end
end

# `ActionServer(node, name, f::Function)` — the function is marker + handler (3-arg: distinct
# from the do-block `(body, node, name, ::Type)` and the `(node, name, ::Type)` forms).
ActionServer(node::Node, name::AbstractString, f::FN; kwargs...) where {FN<:Function} =
    _make_action_server(node, name, FN; body = _authored_action_adapter(f, FN), kwargs...)

ActionClient(node::Node, name::AbstractString, f::FN; kwargs...) where {FN<:Function} =
    ActionClient(node, name, typeof(f); kwargs...)

# kwargs in → builds the Goal struct (the struct-passing `send(client, goal)` stays).
send(client::ActionClient{A, G, R, F}; fields...) where {A, G, R, F} =
    send(client, ROSMessages.struct_from_nt(G, NamedTuple(fields)))

# @NamedTuple out for authored actions — mirrors the handler. Keyed on the function marker, so
# `@ros_import` actions (tag-struct marker, not <:Function) keep returning their structs. Field
# access is identical either way; this is the symmetric surface. (`invoke` reaches the generic
# struct-returning methods, then converts.)
function feedback(g::ClientGoal{A, G, R, F}) where {A<:Function, G, R, F}
    src = invoke(feedback, Tuple{ClientGoal}, g)         # the generic Channel{F}
    out = Channel{NamedTuple}(32)
    errormonitor(Threads.@spawn begin
        try
            for fb in src; put!(out, ROSMessages.nt_from_struct(fb)); end
        finally
            close(out)
        end
    end)
    return out
end

Base.fetch(g::ClientGoal{A, G, R, F}) where {A<:Function, G, R, F} =
    ROSMessages.nt_from_struct(invoke(Base.fetch, Tuple{ClientGoal}, g))

export @ros_package, @ros_message, @ros_service, @ros_action, FeedbackSink, goal_handle
