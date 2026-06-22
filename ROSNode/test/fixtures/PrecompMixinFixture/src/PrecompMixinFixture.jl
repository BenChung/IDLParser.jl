module PrecompMixinFixture

using ROSNode
import ROSNode: member_schema          # the trait we EXTEND (must import to add methods)

# Authored type — exercises the unified `ros_init!` covering the authored-types drain.
@ros_package "precomp_fix"
@ros_message struct Ping
    seq::Int64
end

# A component with params + a timer (every) + a subscription (hears) port. The functor analog of
# the old `_MIXINS` trap: the SCHEMA is a `const` value built here, and `node(…; name=…)` defers
# the registry mutation while `jl_generating_output` is set — so a precompiled package re-registers
# its const schemas from `__init__` (see below). A publisher + service port too, so `precompile_node`
# exercises the publisher send-path codec and the service request/response codec anchors.
mutable struct Counter{Name} <: Component{Name}
    n::Int
end
Counter{Name}() where {Name} = Counter{Name}(0)           # no-DI default ctor (DefaultCtor calls S{Name}())

@parameters struct CounterParams
    fps::Int64    = 30 ∈ 1..120     # "tick rate"
    label::String = "hi"
end

tick(node, m::Counter)            = (m.n += 1; nothing)    # fires at `fps` Hz
ingest(node, m::Counter, msg::Ping) = (m.n = Int(msg.seq); nothing)
# `@service` authors the wire TYPE + the handler + the descriptor in one place (the @serves analog).
@service "~/q" function q(node, m::Counter, x::Int64)::@NamedTuple{ok::Bool}
    (ok = x > 0,)
end
member_schema(::Type{Counter}) = component(Counter, CounterParams,
    publishes(:out, Ping),
    every(:tick, :fps, tick),
    hears(:ingest, Ping, ingest),
    q)

mutable struct Other{Name} <: Component{Name}
    x::Float64
end
Other{Name}() where {Name} = Other{Name}(1.0)
member_schema(::Type{Other}) = component(Other)

# An action member, authored inline with `@action` (the @runs analog). The functor `precompile_node`
# walker DEFERS Act anchoring (`_anchor_functor_member!`'s Act branch is a follow-up), so its
# goal/result/feedback codecs are baked explicitly below — proving the action-codec bake survival.
mutable struct Mover{Name} <: Component{Name} end
@action function move(node, m::Mover, n::Int64,
                      fb::FeedbackSink{@NamedTuple{k::Int64}})::@NamedTuple{done::Bool}
    (done = true,)
end
member_schema(::Type{Mover}) = component(Mover, move)

# A composed node kind — a `const` schema VALUE. `name="Rig"` registers the kind, but the
# registry mutation is deferred while generating precompile output, so `__init__` (below) does it.
const Rig = node("a" => Counter, "b" => Other; name = "Rig")

# Standalone kinds, also registered at load via `__init__`, so the subprocess can resolve them.
const CounterKind = node("c" => Counter; name = "Counter")
const OtherKind   = node("o" => Other;   name = "Other")

# Two MORE deferred-registration paths, exercised so their precompile survival is tested:
#   • `@register_nodes` — rosters the kind onto `__node_kinds__`; here the module already defines
#     `__init__` (below), so the macro does NOT install one — it relies on this module's manual
#     `ros_init!(@__MODULE__)` call to drain the roster at load.
#   • `register_node_kinds!(...)` — the eager helper, called from `__init__` directly (below).
const RegMacroKind = node("rm" => Counter; name = "RegMacro")
const RegEagerKind = node("re" => Counter; name = "RegEager")

# Re-register the const schemas at LOAD (the deferred-registration contract for a precompiled
# package — `node(…; name=…)` skips the registry mutation under `jl_generating_output`).
function __init__()
    ROSNode.ros_init!(@__MODULE__)                 # drains __node_kinds__ (incl. the @register_nodes RegMacroKind)
    register_node_kind!("Rig", Rig)
    register_node_kind!("Counter", CounterKind)
    register_node_kind!("Other", OtherKind)
    register_node_kinds!(RegEagerKind)             # the eager helper, called from a BYO-style __init__
end

# `@register_nodes` placed AFTER the manual `__init__` so the macro sees `__init__` already defined and
# skips installing one — the manual `__init__`'s `ros_init!` drains the roster at load (deterministic).
@register_nodes RegMacroKind
precompile_node(RegMacroKind)      # bake the macro-registered kind's first-run path (asserted in the subprocess)

# Bake the node's first-`run` path (carrier builders, lifecycle fan-out, reaction/serve bodies, and
# the wire codecs) into THIS package's precompile image. The baked MethodInstances are asserted to
# survive in the fresh-subprocess check (a load that does NOT re-run this body).
precompile_node(Rig)

# Explicitly bake the Mover action's goal/result/feedback codecs (precompile_node defers Act
# anchoring); the fresh-subprocess check probes that these MIs ride the image.
let support = ROSNode.ActionTypeSupport(typeof(move))
    precompile(ROSNode.decode_owned, (Memory{UInt8}, Type{ROSNode.goal_type(support)}))
    precompile(ROSNode.encode, (ROSNode.result_type(support),))
    precompile(ROSNode.encode, (ROSNode.feedback_type(support),))
end

# BYO `__init__`: a separate module keeps its OWN load hook, calling `ros_init!` itself; the const
# schema's deferred registration is done here too. Proves the documented BYO contract survives
# precompilation.
module Byo
    using ROSNode
    import ROSNode: member_schema

    mutable struct Solo{Name} <: Component{Name}
        v::Int
    end
    Solo{Name}() where {Name} = Solo{Name}(7)
    @parameters struct SoloParams
        gain::Float64 = 2.0
    end
    beat(node, m::Solo) = (m.v += 1; nothing)
    member_schema(::Type{Solo}) = component(Solo, SoloParams, every(:beat, 5, beat))

    const SoloKind = node("s" => Solo; name = "Solo")

    function __init__()
        ROSNode.ros_init!(@__MODULE__)
        register_node_kind!("Solo", SoloKind)
    end

    precompile_node(SoloKind)      # bake under a BYO __init__ too (separate module roster)
end

# Auto-`__init__` path: a submodule that uses ONLY `@register_nodes` and defines NO `__init__`, so
# the macro INSTALLS `__init__() = ros_init!(@__MODULE__)` itself. Julia calls a submodule's `__init__`
# at load, so `AutoKind` must register by name in the subprocess without any hand-written hook here —
# guarding the macro's auto-install branch (`!isdefined(modu, :__init__)`), previously untested.
module AutoByo
    using ROSNode
    import ROSNode: member_schema

    mutable struct Auto{Name} <: Component{Name}
        v::Int
    end
    Auto{Name}() where {Name} = Auto{Name}(0)
    tap(node, m::Auto) = (m.v += 1; nothing)
    member_schema(::Type{Auto}) = component(Auto, every(:tap, 7, tap))

    const AutoKind = node("a" => Auto; name = "Auto")
    @register_nodes AutoKind        # rosters AND installs __init__() = ros_init!(@__MODULE__) (no manual __init__)

    precompile_node(AutoKind)
end

end # module
