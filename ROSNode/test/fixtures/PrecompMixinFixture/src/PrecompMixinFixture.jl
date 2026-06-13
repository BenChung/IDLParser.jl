module PrecompMixinFixture

using ROSNode

# Authored type — exercises the unified `ros_init!` covering the authored-types drain.
@ros_package "precomp_fix"
@ros_message struct Ping
    seq::Int64
end

# A mixin with params + a timer (@every) + a subscription (@hears) port. Pre-fix, the
# spec pushes ran during THIS package's precompile and mutated ROSNode's `_MIXINS`,
# which the package cache could not carry — so at runtime `mixin_spec(Counter)` threw.
@mixin struct Counter
    n::Int = 0
end
@param Counter "tick rate" fps::Int64 = 30 ∈ 1..120
@param Counter label::String = "hi"
@every :fps function tick(m::Counter)
    m.n += 1
end
@hears function ingest(m::Counter, msg::Ping)
    m.n = Int(msg.seq)
end

@mixin struct Other
    x::Float64 = 1.0
end

# A composed node kind — its name must resolve via `node_kind` at runtime (registered
# by `ros_init!` for the precompiled case).
@node Rig = ["a" => Counter, "b" => Other]

# BYO `__init__`: defined ABOVE the macros, so they step aside; the user keeps ROSNode
# initializing by calling `ros_init!` themselves. Proves the documented BYO contract
# survives precompilation.
module Byo
    using ROSNode
    function __init__()
        ROSNode.ros_init!(@__MODULE__)
    end
    @mixin struct Solo
        v::Int = 7
    end
    @param Solo gain::Float64 = 2.0
    @every 5 function beat(m::Solo)
        m.v += 1
    end
end

end # module
