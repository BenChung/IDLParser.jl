# Inline service/action AUTHORING for the functor API — the DRY top of the authoring hierarchy.
#
#   1. @ros_service / @ros_action            — author the wire TYPE (reusable on its own)
#   2. serves(name, ReqType, handler; on)    — bind ANY handler to ANY type under ANY name; plus
#      runs(name, Action, exec; on)            serves(name, ::Srv; on) / runs(name, ::Act; on) rebind
#                                               an existing descriptor under another name/wire
#   3. @service / @action                    — author the TYPE + the handler IMPL in ONE place
#
# @service/@action DO NOT reimplement type authoring: they reuse `@ros_service`/`@ros_action`'s codegen
# (`_emit_service` / the Goal/Result/Feedback + wrapper specs) on the request/goal fields and the
# `@NamedTuple` return, so the handler becomes a normal `@ros_service`-style MARKER carrying
# `request_type`/`response_type` (and `ActionTypeSupport`). The ONLY thing added is a `component_*`
# meta-method (a Holy trait on the marker type) recording "this is a NODE-FIRST component handler"
# plus its default port name + wire. The handler stays a first-class named method; `serves`/`runs`/
# `component` derive the descriptor by dispatching on the trait. Because the marker keeps its name,
# a client can use it directly (e.g. `ActionClient(node, ke, countup)`), and the authored
# `*_Request`/`*_Response` types stay reachable for the drop-down tiers.

export @service, @action

# ── component-handler traits (the meta-methods @service/@action attach to the marker type) ──────────
# Their EXISTENCE marks a node-first component handler; they carry the default port name + wire.
struct ComponentService; name::Symbol; wire::Union{String, Nothing}; end
struct ComponentAction;  name::Symbol; wire::Union{String, Nothing}; fb_pos::Int; end
component_service(@nospecialize(::Type)) = nothing
component_action(@nospecialize(::Type))  = nothing

# ── node-first adapters: bridge the user handler to the (node, m, ·) callback the substrate runs ────
# Service: splat the request fields after (node, m); wrap the @NamedTuple return into the Response.
# Rq/Rs ride the TYPE (resolved at serves() in the normal world), so the @generated body reads only
# type params — it never calls response_type from a generator world (the world-age trap).
struct NodeFirstService{F, Rq, Rs}; f::F; end
# `_surface_fields`/`_from_nt` (authored.jl) treat a synthetic-only (fieldless) section as empty, so a
# trigger service (empty request / empty response) splats no args and builds via `Rs()` — matching
# `_authored_service_adapter`. Raw `fieldnames` would splat the synthetic `structure_needs_at_least_one_member`.
@generated function (a::NodeFirstService{F, Rq, Rs})(node, m, req) where {F, Rq, Rs}
    splat = [:( getfield(req, $(QuoteNode(fn))) ) for fn in _surface_fields(Rq)]
    # Mirror `_authored_service_adapter`: a handler THROW replies `failed` carrying the real error
    # text (via the task-local active-service cell `respond!` reads), so the client gets a useful
    # message instead of the generic "service handler aborted" the settlement fallback would produce.
    return :( try
                  let out = a.f(node, m, $(splat...))
                      out isa $Rs ? out : $(_from_nt)($Rs, out)
                  end
              catch e
                  respond!(req, failed, sprint(showerror, e))
                  nothing
              end )
end
# Action: the per-goal exec — splat goal fields, inject a FeedbackSink, SUCCEEDED-on-return / CANCELED on a
# thrown Cancelled / ABORTED on any other throw (the body runs under `settle_handler!`). The action type
# `A` rides the TYPE (resolved ONCE at `_runs_trait` in the normal world), so each goal reads only a type
# param — no per-goal `ActionTypeSupport(typeof(a.f))` reflection.
struct NodeFirstAction{F, A}; f::F; fb_pos::Int; end
(a::NodeFirstAction{F, A})(node, m, goal) where {F, A} =
    _component_action_adapter(a.f, ActionTypeSupport(A), a.fb_pos, node, m)(goal)

# ── derive a descriptor from a trait-bearing handler (serves/runs over an @service/@action marker) ──
function serves(f::Function; on = nothing)
    t = component_service(typeof(f))
    t === nothing && throw(ArgumentError("serves($(f)): not an @service handler — for a raw handler use `serves(:name, ReqType, handler; on)`"))
    return _serves_trait(f, t, on)
end
serves(name::Symbol, f::Function; on = nothing) = _serves_trait(f, _need_service(f), on; name = name)
function _serves_trait(f, t::ComponentService, on; name::Symbol = t.name)
    Rq = request_type(typeof(f)); Rs = response_type(typeof(f))      # resolved here, normal world
    h  = NodeFirstService{typeof(f), Rq, Rs}(f)
    return Srv{name, Rq, Rs, typeof(h)}(h, on === nothing ? t.wire : _norm_wire(on))
end
_need_service(f) = (t = component_service(typeof(f));
    t === nothing && throw(ArgumentError("serves(name, f): $(f) is not an @service handler")); t)

function runs(f::Function; on = nothing)
    t = component_action(typeof(f))
    t === nothing && throw(ArgumentError("runs($(f)): not an @action handler — for a raw exec use `runs(:name, Action, exec; on)`"))
    return _runs_trait(f, t, on)
end
runs(name::Symbol, f::Function; on = nothing) = _runs_trait(f, _need_action(f), on; name = name)
function _runs_trait(f, t::ComponentAction, on; name::Symbol = t.name)
    A = action_type(ActionTypeSupport(typeof(f)))            # resolved here, normal world (mirrors serves())
    h = NodeFirstAction{typeof(f), A}(f, t.fb_pos)
    # `construct_port` builds the server from the Act's first type param (the marker `typeof(f)`), so the
    # handle type `H` is resolved over `typeof(f)` too — eagerly here, never in the @generated carrier.
    return Act{name, typeof(f), typeof(h), _action_server_type(typeof(f))}(h, on === nothing ? t.wire : _norm_wire(on))
end
_need_action(f) = (t = component_action(typeof(f));
    t === nothing && throw(ArgumentError("runs(name, f): $(f) is not an @action handler")); t)

# component() accepts a trait-bearing handler directly: a bare @service/@action method drops in.
_to_port(p::PortDesc) = p
function _to_port(@nospecialize(f))
    component_service(typeof(f)) !== nothing && return serves(f)
    component_action(typeof(f))  !== nothing && return runs(f)
    throw(ArgumentError("component: $(f) is not a port descriptor or an @service/@action handler"))
end

# ── the macros: @ros_service/@ros_action codegen + one component_* meta-method ──────────────────────
_authoring_pkg(mod::Module) = isdefined(mod, :__ros_package__) ?
    String(getfield(mod, :__ros_package__)) : _snake(String(nameof(mod)))

# A node-first handler's member arg must cover the component BASE (`m::M`), not a curly instantiation.
function _check_member_arg(macroname, M)
    M isa Expr && M.head === :curly && error(string(
        "$(macroname): the member arg must cover the component base — got `m::$(M)`",
        M.args[1] === :Union ? "" : "; annotate `m::$(M.args[1])`"))
    return nothing
end

# Request/goal FIELD args must be concretely typed: `allow_untyped` excuses the leading `node` arg, but an
# untyped (=> `::Any`) request/goal field has no ROS wire mapping and would only error far downstream at
# RIHS reflection (module-init drain). Reject it here, at the offending source.
function _check_field_types(macroname, args)
    for (nm, ty) in args
        ty === :Any && error("$(macroname): the field `$(nm)` needs a concrete type annotation " *
            "(`$(nm)::T`) — an untyped field becomes `::Any`, which has no ROS wire mapping")
    end
    return nothing
end

"""
    @service        function f(node, m::M, field::T, …)::@NamedTuple{out::U, …} … end
    @service "wire" function f(node, m::M, field::T, …)::@NamedTuple{out::U, …} … end

Author a component **service port** in one place. The arguments after `node, m` are the request
fields and the `::@NamedTuple{…}` return is the response. The macro reuses `@ros_service`'s codegen to
generate `f_Request`/`f_Response` (package from `@ros_package`, else the snake-cased module name),
register their RIHS01, and specialize `request_type`/`response_type` on `typeof(f)` — so `f` is a
normal `@ros_service` marker. It then adds ONE meta-method, `component_service(typeof(f))`, marking `f`
a node-first component handler with a default port name (the function name) and wire (a leading string,
else `~/f`). The handler **stays a normal method** (callable, testable); `serves`/`component` derive the
`Srv` descriptor from the trait, splatting the request fields and wrapping the
`@NamedTuple` return into `f_Response`.

Drop down the hierarchy for more control: `serves(:other, f; on="…")` reuses this handler under another
name; `serves(:name, f_Request, other; on="…")` reuses the authored TYPE with a different handler.

```julia
@service "~/safe_to_fly" function safe(node, g::Guard, target_altitude::Float64)::@NamedTuple{ok::Bool, battery::Float64}
    b = battery(g.battery_src)
    (ok = b >= parameters(node, g).min_battery && target_altitude <= 100.0, battery = b)
end
member_schema(::Type{Guard}) = component(Guard, GuardParams, safe; requires = (BatterySource,), ctor = make_guard)
```
"""
macro service(a, b = nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@service expects a `function f(node, m::M, fields…)::@NamedTuple{…} … end` definition")
    fname, allargs, rettype = _parse_handler_sig(f; allow_untyped = true)
    length(allargs) >= 2 || error("@service: the handler is node-first — write `(node, m::M, fields…)`")
    _check_member_arg("@service", allargs[2][2])
    _check_field_types("@service", allargs[3:end])
    rettype === nothing && error("@service: needs a `::@NamedTuple{…}` return (the response fields); " *
        "to wire a PRE-authored service type, use `serves(:name, ReqType, handler; on)`")
    pkg = _authoring_pkg(__module__)
    block = _emit_service(__module__, pkg, fname, allargs[3:end], _nt_fields(rettype), f)   # @ros_service codegen
    push!(block.args, :(ROSNode.component_service(::Type{typeof($fname)}) =
        ROSNode.ComponentService($(QuoteNode(fname)), $wire)))                              # the extra meta-method
    return esc(block)
end

"""
    @action        function f(node, m::M, goal::T, …, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…} … end
    @action "wire" function f(node, m::M, goal::T, …, fb::FeedbackSink{@NamedTuple{…}})::@NamedTuple{…} … end

Author a component **action port** in one place — the action analog of [`@service`](@ref). After
`node, m`, the arguments other than the [`FeedbackSink`](@ref) are the Goal fields, the sink's
`@NamedTuple` gives the Feedback fields, and the `::@NamedTuple{…}` return is the Result. The macro
reuses `@ros_action`'s codegen (Goal/Result/Feedback + the five protocol wrappers, registration, and
`ActionTypeSupport(typeof(f))`), so `f` is a normal action marker, then adds the `component_action`
meta-method. The handler stays a named method; `runs`/`component` derive the `Act`
descriptor, running `f` per accepted goal with a live `FeedbackSink`. The body's exit maps to the goal's
terminal status, exactly as [`@ros_action`](@ref) / `runs`:

- a normal return settles **SUCCEEDED** with the converted Result;
- a thrown [`Cancelled`](@ref) (raised at a [`checkpoint`](@ref)/[`feedback!`](@ref) after a cancel
  request) settles **CANCELED**;
- any other throw (or a body that exits without responding) settles **ABORTED**.

Because `f` keeps its name (and the authored `f_Goal`/`f_Result`/`f_Feedback` + protocol-wrapper types
stay reachable), a client uses it directly: `ActionClient(node, ke, f)`.

```julia
@action function countup(node, m::Counter, target::Int32,
                         fb::FeedbackSink{@NamedTuple{current::Int32}})::@NamedTuple{total::Int32}
    for i in 1:target; fb((current = Int32(i),)); end
    (total = target,)
end
member_schema(::Type{Counter}) = component(Counter, countup)
```
"""
macro action(a, b = nothing)
    wire, f = b === nothing ? (nothing, a) : (String(a), b)
    (f isa Expr && (f.head === :function || f.head === :(=))) ||
        error("@action expects a `function f(node, m::M, goalfields…, fb::FeedbackSink{…})::@NamedTuple{…} … end`")
    fname, allargs, rettype = _parse_handler_sig(f; allow_untyped = true)
    rettype === nothing && error("@action: the handler needs a `::@NamedTuple{…}` result return")
    length(allargs) >= 3 || error("@action: the handler is node-first — write `(node, m::M, goalfields…, fb::FeedbackSink{…})`")
    _check_member_arg("@action", allargs[2][2])
    pkg = _authoring_pkg(__module__)
    return esc(_emit_functor_action(__module__, pkg, fname, allargs, rettype, wire, f))
end

# @ros_action's type codegen but NODE-FIRST: author from the goal/fb args, keep `fname` as the marker
# (ActionTypeSupport on typeof(fname)), and add the component_action meta-method (carrying fb_pos).
function _emit_functor_action(caller::Module, pkg::AbstractString, fname::Symbol, allargs, rettype, wire, f)
    pkgsym = Symbol(pkg)
    goalfb = allargs[3:end]
    fb_rel = findfirst(a -> _is_feedbacksink(a[2]), goalfb)
    fb_rel === nothing && error("@action: the handler needs a `fb::FeedbackSink{@NamedTuple{…}}` parameter")
    fb_pos_full = fb_rel + 2                                  # position in [node, m, goalfields…, fb]
    feedback_fields = _nt_fields(_feedbacksink_nt(goalfb[fb_rel][2]))
    goal_args = Tuple{Symbol, Any}[goalfb[i] for i in eachindex(goalfb) if i != fb_rel]
    _check_field_types("@action", goal_args)     # untyped goal fields have no wire mapping (see @service)
    result_fields = _nt_fields(rettype)
    goalsym, resultsym, fbsym = Symbol(fname, "_Goal"), Symbol(fname, "_Result"), Symbol(fname, "_Feedback")
    actpath(sym) = Expr(:., Expr(:., pkgsym, QuoteNode(:action)), QuoteNode(sym))
    actmod = Expr(:., pkgsym, QuoteNode(:action))
    uuidexpr = :(ROSNode.Interfaces.unique_identifier_msgs.msg.UUID)
    timeexpr = :(ROSNode.Interfaces.builtin_interfaces.msg.Time)
    specs = Any[
        (goalsym, goal_args), (resultsym, result_fields), (fbsym, feedback_fields),
        (Symbol(fname, "_SendGoal_Request"),  [(:goal_id, uuidexpr), (:goal, actpath(goalsym))]),
        (Symbol(fname, "_SendGoal_Response"), [(:accepted, :Bool), (:stamp, timeexpr)]),
        (Symbol(fname, "_GetResult_Request"),  [(:goal_id, uuidexpr)]),
        (Symbol(fname, "_GetResult_Response"), [(:status, :Int8), (:result, actpath(resultsym))]),
        (Symbol(fname, "_FeedbackMessage"),    [(:goal_id, uuidexpr), (:feedback, actpath(fbsym))]),
    ]
    block = Expr(:toplevel)
    push!(block.args, _rebuild_handler(f, fname, allargs, fb_pos_full, rettype))     # keep `fname` as marker+method
    append!(block.args, _emit_structs(caller, pkg, :action, specs))
    push!(block.args, :(ROSNode.ActionTypeSupport(::Type{typeof($fname)}) =
        ROSNode.ActionTypeSupport{typeof($fname), $(actpath(goalsym)), $(actpath(resultsym)), $(actpath(fbsym))}()))
    push!(block.args, :(ROSNode._action_wrapper(::Type{typeof($fname)}, suffix::AbstractString) =
        getfield($actmod, Symbol($(string(fname)), suffix))))
    regpairs = Tuple{Any, String}[(actpath(goalsym), string(pkg, "/action/", goalsym)),
        (actpath(resultsym), string(pkg, "/action/", resultsym)),
        (actpath(fbsym), string(pkg, "/action/", fbsym))]
    for suf in ("_SendGoal_Request", "_SendGoal_Response", "_GetResult_Request", "_GetResult_Response", "_FeedbackMessage")
        push!(regpairs, (actpath(Symbol(fname, suf)), string(pkg, "/action/", Symbol(fname, suf))))
    end
    append!(block.args, _authored_register_stmts(regpairs))
    push!(block.args, :(ROSNode.component_action(::Type{typeof($fname)}) =
        ROSNode.ComponentAction($(QuoteNode(fname)), $wire, $fb_pos_full)))             # the extra meta-method
    return block
end
