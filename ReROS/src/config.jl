# Recorder configuration (DESIGN.md §6): sinks, timelines, the mapper manifest, and the
# ordered `record`/`exclude` rules (first-match) that gate data + connectivity recording,
# plus a static node/topic denylist for administrative noise.

# ── Selectors (DESIGN.md §6.1) — match an endpoint by topic/type/namespace/kind/node ──
abstract type Selector end
struct TopicSel <: Selector; glob::Glob; end
struct TypeSel  <: Selector; glob::Glob; end
struct NsSel    <: Selector; prefix::String; end
struct KindSel  <: Selector; kind::Symbol; end          # :pub | :sub | :service | :client
struct NodeSel  <: Selector; glob::Glob; end
struct AndSel   <: Selector; a::Selector; b::Selector; end
struct OrSel    <: Selector; a::Selector; b::Selector; end
struct NotSel   <: Selector; a::Selector; end

topic(p::AbstractString)     = TopicSel(parse_glob(p))
type(p::AbstractString)      = TypeSel(parse_glob(p))
namespace(p::AbstractString) = NsSel(String(p))
kind(k::Symbol)              = KindSel(k)
node(p::AbstractString)      = NodeSel(parse_glob(p))
Base.:&(a::Selector, b::Selector) = AndSel(a, b)
Base.:|(a::Selector, b::Selector) = OrSel(a, b)
Base.:!(a::Selector)              = NotSel(a)

const _KIND_SYM = Dict(:pub => ROSReach.Publisher, :sub => ROSReach.Subscription,
                       :service => ROSReach.Service, :client => ROSReach.Client)

sel_match(s::TopicSel, e) = glob_match(s.glob, e.topic)
sel_match(s::TypeSel, e)  = e.type !== nothing && glob_match(s.glob, e.type.name)
sel_match(s::NsSel, e)    = startswith(e.namespace, s.prefix)
sel_match(s::KindSel, e)  = haskey(_KIND_SYM, s.kind) && e.kind == _KIND_SYM[s.kind]
sel_match(s::NodeSel, e)  = glob_match(s.glob, e.node_name)
sel_match(s::AndSel, e)   = sel_match(s.a, e) && sel_match(s.b, e)
sel_match(s::OrSel, e)    = sel_match(s.a, e) || sel_match(s.b, e)
sel_match(s::NotSel, e)   = !sel_match(s.a, e)

# Selector match against a bare GraphNode: only node/namespace selectors apply (a node
# has no topic/type/kind), so those never match. Used for connectivity filtering of nodes.
sel_match_node(s::NodeSel, n) = glob_match(s.glob, n.name)
sel_match_node(s::NsSel, n)   = startswith(n.namespace, s.prefix)
sel_match_node(s::AndSel, n)  = sel_match_node(s.a, n) && sel_match_node(s.b, n)
sel_match_node(s::OrSel, n)   = sel_match_node(s.a, n) || sel_match_node(s.b, n)
sel_match_node(s::NotSel, n)  = !sel_match_node(s.a, n)
sel_match_node(::Selector, n) = false

"""A `record` rule: when its selector matches, set whether to log `data`/`connectivity`."""
struct Rule
    sel::Selector
    data::Bool
    connectivity::Bool
    mapper::Any                 # :auto | :structural | a Function | "pkg/msg/Name"
end
rule(sel::Selector; data::Bool=true, connectivity::Bool=true, mapper=:auto) =
    Rule(sel, data, connectivity, mapper)
exclude(sel::Selector) = Rule(sel, false, false, :auto)

struct RecorderConfig
    app_id::String
    recording_id::Union{String,Nothing}
    sinks::Vector{SinkSpec}
    timelines::Timelines
    mappers::MapperManifest          # effective manifest (already merged over BUILTIN_MAPPERS)
    opts::MapOpts
    capacity::Int
    drop_when_full::Bool
    record::Vector{Rule}             # ordered; first match wins (DESIGN.md §6)
    deny_node::Vector{Regex}         # node-name denylist (connectivity)
    deny_topic::Vector{Regex}        # topic/service-name denylist
    sub_depth::Int                   # cap on derived data-subscription history depth (§3.3)
end

function RecorderConfig(app_id::AbstractString;
                        recording_id::Union{AbstractString,Nothing}=nothing,
                        sinks=SinkSpec[Save("recording.rrd")],
                        timelines::Timelines=Timelines(),
                        mappers::MapperManifest=MapperManifest(),
                        opts::MapOpts=MapOpts(),
                        capacity::Integer=4096,
                        drop_when_full::Bool=true,
                        record=Rule[],
                        deny_node=[r"^_", r"ros2cli"],
                        deny_topic=[r"/parameter_events$", r"/rosout$"],
                        sub_depth::Integer=10)
    return RecorderConfig(String(app_id), recording_id, collect(SinkSpec, sinks), timelines,
                          merge(BUILTIN_MAPPERS, mappers), opts, Int(capacity), drop_when_full,
                          collect(Rule, record), collect(Regex, deny_node), collect(Regex, deny_topic),
                          Int(sub_depth))
end

_denied_node(cfg::RecorderConfig, name::AbstractString) = any(re -> occursin(re, name), cfg.deny_node)
_denied_topic(cfg::RecorderConfig, topic::AbstractString) = any(re -> occursin(re, topic), cfg.deny_topic)

"""First matching `record` rule for endpoint `e`, or `nothing` (DESIGN.md §6, first-match)."""
rule_for(cfg::RecorderConfig, e) = (i = findfirst(r -> sel_match(r.sel, e), cfg.record);
                                    i === nothing ? nothing : cfg.record[i])
"""First `record` rule matching GraphNode `n` by node/namespace, or `nothing`."""
rule_for_node(cfg::RecorderConfig, n) = (i = findfirst(r -> sel_match_node(r.sel, n), cfg.record);
                                         i === nothing ? nothing : cfg.record[i])
"""Does the config select endpoint `e`'s topic for *data* (payload) recording?"""
function wants_data(cfg::RecorderConfig, e)
    r = rule_for(cfg, e)
    return r !== nothing && r.data
end
"""Does a matching rule turn *connectivity* off for endpoint `e` (e.g. `exclude(...)`)?"""
conn_denied(cfg::RecorderConfig, e) = (r = rule_for(cfg, e); r !== nothing && !r.connectivity)
"""Does a matching rule turn *connectivity* off for node `n`?"""
conn_denied_node(cfg::RecorderConfig, n) = (r = rule_for_node(cfg, n); r !== nothing && !r.connectivity)
