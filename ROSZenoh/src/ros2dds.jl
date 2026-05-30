# Ros2DDS formatter — matches `zenoh-plugin-ros2dds`.
#
# Topic KE:    <topic-strip-slashes>
# LV KE:       @/<zid>/@ros2_lv/<kind>/<topic-mangled>/<type-mangled-or-"unknown">[/<qos>]
#
# Differences from RmwZenoh — the same questions, answered differently:
#
# Both formats encode roughly the same information (entity kind, topic,
# type, QoS) but route it to two different transports — rmw_zenoh is a
# pure Zenoh-native RMW, while ros2dds is a *bridge* sitting between Zenoh
# and a DDS network. The DDS side already has its own discovery and QoS
# machinery, so the bridge can drop redundancies; conversely, the bridge
# has to surface a DDS-only concept (keyed vs keyless topics) that
# Zenoh-native code never sees.
#
# - Node identity. rmw_zenoh embeds the full node tuple (domain, zid,
#   node_id, name, namespace) into every endpoint token because Zenoh-side
#   peers have no other way to learn it. ros2dds writes no node identity —
#   DDS participant discovery already advertises the same data on the DDS
#   side, so duplicating it would be wasted bytes. parse_liveliness here
#   therefore returns an `EndpointEntity` with `node === nothing` unless the
#   caller supplies one (the `node` kwarg below — useful when the higher
#   level already knows which node owns this endpoint).
#
# - QoS on services/clients. Both formats *could* attach QoS to a service
#   or client token. rmw_zenoh does (uniformity); ros2dds doesn't, because
#   in DDS every ROS service uses the same fixed QoS (reliable + volatile +
#   small history) set by the rmw service contract, so encoding it
#   per-endpoint adds no signal for matching.
#
# - QoS encoding shape. Both serialise reliability/durability/history+depth
#   over a `:`-separated string, but choose opposite compactness trade-offs:
#     * rmw_zenoh elides defaults (default Reliable → "", default Volatile →
#       "") and has six fields (the trailing three — deadline / lifespan /
#       liveliness — are fixed sentinels but reserved). It also has no
#       `keyless` slot, because Zenoh-native pub/sub has no concept of a
#       keyed topic; the trait surfaces `keyless` only to keep the signature
#       uniform, and rmw_zenoh just drops it on the floor.
#     * ros2dds emits every field explicitly (`0`/`1`) because it
#       round-trips into a DDS QoS object where every field is set, and
#       prepends a `keyless` flag because DDS distinguishes keyed topics
#       (primary-key field, per-instance lifecycle) from keyless ones —
#       a distinction the DDS side needs for matching even though ROS2's
#       user API hides it.
#
# - Topic key expression. rmw_zenoh writes `<domain>/<topic>/<type>/<hash>`
#   because subscribers need all four to match — Zenoh-native has no
#   parallel type/hash discovery. ros2dds writes the bare topic name; the
#   DDS side already publishes type + hash through participant discovery,
#   so the Zenoh-side keyexpr just needs to route data.
#
# - Node liveliness. rmw_zenoh has a `NN`-kind token shaped like an
#   endpoint token sans topic/type/qos, used by peers to discover nodes.
#   ros2dds has no equivalent — the bridge sees DDS participants and
#   endpoints, not nodes — so we don't produce a default keyexpr here. The
#   `token` kwarg on `liveliness_keyexpr(::Ros2DDS, ::NodeEntity, ...)`
#   lets higher-level code that has its own scheme pass one through.

# ---- QoS ------------------------------------------------------------------
#
# Format: <keyless>:<reliability>:<durability>:<history_kind>,<depth>
# - keyless: "K" if not keyless, "" if keyless
# - reliability: 0 BestEffort, 1 Reliable
# - durability: 0 Volatile, 1 TransientLocal
# - history: 0,<depth> KeepLast; 1,0 KeepAll

function encode_qos(::Ros2DDS, qos::QosProfile; keyless::Bool=false)
    keyless_str = keyless ? "" : "K"
    rel = qos.reliability == :best_effort ? "0" :
          qos.reliability == :reliable    ? "1" :
          error("invalid reliability $(qos.reliability)")
    dur = qos.durability == :volatile         ? "0" :
          qos.durability == :transient_local  ? "1" :
          error("invalid durability $(qos.durability)")
    hist = qos.history == :keep_last ? "0,$(qos.depth)" :
           qos.history == :keep_all  ? "1,0" :
           error("invalid history $(qos.history)")
    return string(keyless_str, ':', rel, ':', dur, ':', hist)
end

function decode_qos(::Ros2DDS, s::AbstractString)
    parts = split(String(s), ':')
    length(parts) >= 4 ||
        throw(ArgumentError("ros2dds QoS has at least 4 fields, got $(length(parts))"))

    keyless = parts[1] == ""

    reliability = parts[2] == ""  ? :reliable :        # default-on-empty matches Rust
                  parts[2] == "0" ? :best_effort :
                  parts[2] == "1" ? :reliable :
                  :reliable

    durability = parts[3] == ""  ? :volatile :
                 parts[3] == "0" ? :volatile :
                 parts[3] == "1" ? :transient_local :
                 :volatile

    history, depth = if parts[4] == ""
        :keep_last, 10
    else
        hp = split(parts[4], ',')
        if length(hp) >= 2
            d = tryparse(Int, String(hp[2]))
            d === nothing && (d = 10)
            if hp[1] == "0" || hp[1] == ""
                :keep_last, d
            elseif hp[1] == "1"
                :keep_all, 0
            else
                :keep_last, d
            end
        else
            :keep_last, 10
        end
    end

    return (keyless, QosProfile(reliability=reliability, durability=durability,
                                history=history, depth=depth))
end

# ---- Topic KE -------------------------------------------------------------

function topic_keyexpr(::Ros2DDS, e::EndpointEntity)
    return _strip_one_slash(e.topic)
end

# The ros2dds data key is a bare topic name — it carries no domain or type, so
# there's nothing to recover beyond the topic itself.
function parse_topic_keyexpr(::Ros2DDS, ke::AbstractString)
    return (; domain_id=nothing, topic=_strip_one_slash(ke), type_info=nothing)
end

# ---- Liveliness KE --------------------------------------------------------

function liveliness_keyexpr(f::Ros2DDS, e::EndpointEntity)
    # ros2dds needs a zid; the endpoint's node carries one when present, and
    # in practice ros2dds uses the session's zid. If no node, we can't write
    # a meaningful KE.
    e.node === nothing &&
        throw(ArgumentError("ros2dds liveliness requires a node (for the zenoh id)"))
    zid = e.node.z_id

    topic = mangle(f, _strip_one_slash(e.topic))
    type_name = e.type_info === nothing ? "unknown" : mangle(f, e.type_info.name)

    qos_str = if e.kind == Publisher || e.kind == Subscription
        "/" * encode_qos(f, e.qos)
    else
        ""
    end

    return string("@/", zid, '/', admin_space(f), '/',
                  kind_code(e.kind), '/',
                  topic, '/', type_name,
                  qos_str)
end

"""
    liveliness_keyexpr(::Ros2DDS, n::NodeEntity; token=nothing) -> String

ros2dds has no native node-liveliness scheme (DDS discovery is endpoint-
centric — see the file header). Callers whose higher-level code maintains
its own node-liveliness convention can pass the precomputed token via the
`token` kwarg and this method returns it unchanged, keeping the dispatch
shape uniform with `RmwZenoh`. Without a token, this raises — silently
producing something meaningless would be worse than the explicit failure.
"""
function liveliness_keyexpr(::Ros2DDS, ::NodeEntity;
                            token::Union{Nothing, AbstractString}=nothing)
    token === nothing && throw(ArgumentError(
        "ros2dds has no native node-liveliness scheme; supply `token=<keyexpr>` " *
        "to pass a higher-level-computed token through this dispatch"))
    return String(token)
end

# ---- Parsing --------------------------------------------------------------

"""
    parse_liveliness(::Ros2DDS, ke; node=nothing) -> EndpointEntity

ros2dds tokens carry no node identity, so the returned EndpointEntity has
`node === nothing` by default. If the caller already knows which node owns
this endpoint (e.g. from a higher-level discovery layer), they can pass it
in via `node=` and it will be attached to the result verbatim — the zid in
the token is not cross-checked against `node.z_id`.
"""
function parse_liveliness(f::Ros2DDS, ke::AbstractString;
                          node::Union{NodeEntity, Nothing}=nothing)
    parts = split(String(ke), '/')
    length(parts) >= 6 ||
        throw(ArgumentError("ros2dds liveliness has at least 6 components, got $(length(parts))"))

    parts[1] == "@" ||
        throw(ArgumentError("ros2dds liveliness must start with '@', got $(repr(parts[1]))"))

    # parts[2] is the zid; we don't store it on the parsed entity because
    # ros2dds tokens are node-less. The caller can pass `node=` if they
    # have a node to attach (its z_id is then authoritative).
    _zid = parts[2]

    parts[3] == admin_space(f) ||
        throw(ArgumentError("expected admin space $(admin_space(f)), got $(repr(parts[3]))"))

    kind_str = String(parts[4])
    kind = if kind_str in ("AS", "AC")
        # Action server/client maps to Service in the Rust reference.
        Service
    else
        k = parse_kind_code(kind_str)
        k === nothing && throw(ArgumentError("unknown entity kind $(repr(kind_str))"))
        k
    end

    topic_demangled = demangle(f, parts[5])
    topic = isempty(topic_demangled) ? "/" :
            startswith(topic_demangled, '/') ? topic_demangled :
            string('/', topic_demangled)

    type_name = demangle(f, parts[6])
    type_info = if isempty(type_name) || type_name == "unknown"
        nothing
    else
        TypeInfo(type_name, TypeHash())
    end

    qos = if length(parts) >= 7
        _, q = decode_qos(f, String(parts[7]))
        q
    else
        default_qos()
    end

    return EndpointEntity(0, node, kind, topic, type_info, qos)
end
