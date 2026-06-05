# RmwZenoh formatter — matches `rmw_zenoh_cpp`.
#
# Topic KE:        <domain>/<topic-strip-slashes>/<type-or-empty>/<hash-or-empty>
# Endpoint LV KE:  @ros2_lv/<domain>/<zid>/<nid>/<eid>/<kind>/%/<ns>/<name>/<topic>/<type>/<hash>/<qos>
# Node     LV KE:  @ros2_lv/<domain>/<zid>/<nid>/<nid>/NN/%/<ns>/<name>
#
# The enclave slot is always `%` on write (rmw_zenoh doesn't transmit
# enclave). Empty/`/` namespace also becomes `%` (because mangling `/` itself
# produces `%`). All slashes inside components are mangled in liveliness
# tokens but **preserved** in topic keys.

# ---- QoS ------------------------------------------------------------------
#
# Six colon-separated fields, each component omitted (empty) when it matches the
# ROS2 default — exactly mirroring rmw_zenoh_cpp's `qos_to_keyexpr`:
#
#   <reliability>:<durability>:<hist>,<depth>:<dl_s>,<dl_ns>:<ls_s>,<ls_ns>:<liv>,<lease_s>,<lease_ns>
#
# - reliability: "" (Reliable, default) / "2" (BestEffort). [RMW: 1/2]
# - durability:  "" (Volatile, default) / "1" (TransientLocal). [RMW: 1/2]
# - history:     hist "" (KeepLast, default) / "1" (KeepLast) / "2" (KeepAll);
#                depth always emitted (",<depth>") — matches hiroz, which keeps
#                the depth even at the default 10 rather than omitting it.
# - deadline / lifespan: "<sec>,<nsec>", each component empty when 0 (∞ default).
# - liveliness:  "<kind>,<lease_sec>,<lease_nsec>"; kind "" (Automatic, default)
#                / "3" (ManualByTopic) [RMW: 1/3]; lease components empty when 0.
#
# The all-default profile therefore encodes the trailing fields as `,:,:,,`,
# identical to the previous placeholder form.

# A Duration component renders as its integer, or "" when 0 (the ∞/default).
_dur_sec_nsec(::Nothing) = ("", "")
_dur_sec_nsec(d::Duration) = (d.sec == 0 ? "" : string(d.sec),
                              d.nsec == 0 ? "" : string(d.nsec))

function encode_qos(::RmwZenoh, qos::QosProfile; keyless::Bool=false)
    rel = qos.reliability == :reliable     ? "" :
          qos.reliability == :best_effort  ? "2" :
          error("invalid reliability $(qos.reliability)")
    # Only "Reliable" matches default; explicit non-default reliable still
    # ends up "" here — the Rust does the same (it compares against the
    # Default impl).
    dur = qos.durability  == :volatile         ? "" :
          qos.durability  == :transient_local  ? "1" :
          error("invalid durability $(qos.durability)")
    hist = if qos.history == :keep_all
        "2,"
    elseif qos.depth == 10
        # Default depth — emit just the depth without the kind prefix.
        ",$(qos.depth)"
    else
        "1,$(qos.depth)"
    end

    dl_s, dl_ns = _dur_sec_nsec(qos.deadline)
    ls_s, ls_ns = _dur_sec_nsec(qos.lifespan)
    liv = qos.liveliness == :automatic       ? "" :
          qos.liveliness == :manual_by_topic ? "3" :
          error("invalid liveliness $(qos.liveliness)")
    lease_s, lease_ns = _dur_sec_nsec(qos.liveliness_lease)

    return string(rel, ':', dur, ':', hist, ':',
                  dl_s, ',', dl_ns, ':',
                  ls_s, ',', ls_ns, ':',
                  liv, ',', lease_s, ',', lease_ns)
end

# Parse a "<sec>,<nsec>" duration field into `Union{Nothing, Duration}`. Empty
# components default to 0; an all-default (0,0) field is the ∞/unset `nothing`.
function _parse_duration(comps, what)
    sec  = isempty(comps[1]) ? 0 : tryparse(UInt64, String(comps[1]))
    nsec = length(comps) >= 2 && !isempty(comps[2]) ? tryparse(UInt64, String(comps[2])) : 0
    (sec === nothing || nsec === nothing) &&
        throw(ArgumentError("invalid QoS $what $(repr(join(comps, ',')))"))
    (sec == 0 && nsec == 0) ? nothing : Duration(sec, nsec)
end

function decode_qos(::RmwZenoh, s::AbstractString)
    parts = split(String(s), ':')
    length(parts) >= 3 || throw(ArgumentError("rmw_zenoh QoS has at least 3 fields, got $(length(parts))"))

    reliability = parts[1] == ""  ? :reliable :
                  parts[1] == "1" ? :reliable :
                  parts[1] == "2" ? :best_effort :
                  throw(ArgumentError("invalid reliability code $(repr(parts[1]))"))

    durability = parts[2] == ""  ? :volatile :
                 parts[2] == "1" ? :transient_local :
                 parts[2] == "2" ? :volatile :
                 throw(ArgumentError("invalid durability code $(repr(parts[2]))"))

    history_parts = split(parts[3], ',')
    length(history_parts) >= 2 || throw(ArgumentError("invalid QoS history $(repr(parts[3]))"))
    history, depth = if history_parts[1] == "" || history_parts[1] == "1"
        # Empty depth → default (10), tolerating rmw_zenoh's omit-on-default form.
        d = isempty(history_parts[2]) ? 10 : tryparse(Int, String(history_parts[2]))
        d === nothing && throw(ArgumentError("invalid QoS depth $(repr(history_parts[2]))"))
        (:keep_last, d)
    elseif history_parts[1] == "2"
        (:keep_all, 0)
    else
        throw(ArgumentError("invalid QoS history kind $(repr(history_parts[1]))"))
    end

    # Trailing fields are tolerated when omitted (older peers, default-only tokens).
    deadline = _parse_duration(length(parts) >= 4 ? split(parts[4], ',') : [""], "deadline")
    lifespan = _parse_duration(length(parts) >= 5 ? split(parts[5], ',') : [""], "lifespan")

    liveliness = :automatic
    liveliness_lease = nothing
    if length(parts) >= 6
        liv_parts = split(parts[6], ',')
        liveliness = liv_parts[1] == ""  ? :automatic :
                     liv_parts[1] == "1" ? :automatic :
                     liv_parts[1] == "3" ? :manual_by_topic :
                     throw(ArgumentError("invalid liveliness kind $(repr(liv_parts[1]))"))
        liveliness_lease = _parse_duration(length(liv_parts) >= 3 ? liv_parts[2:3] :
                                           length(liv_parts) >= 2 ? liv_parts[2:2] : [""],
                                           "liveliness lease")
    end

    return (false, QosProfile(reliability=reliability, durability=durability,
                              history=history, depth=depth,
                              deadline=deadline, lifespan=lifespan,
                              liveliness=liveliness, liveliness_lease=liveliness_lease))
end

# ---- Topic KE -------------------------------------------------------------

# rmw_zenoh carries the message type in the keyexpr (and the liveliness token) as the
# DDS/IDL type name `pkg::sub::dds_::Type_` — a SINGLE keyexpr component using `::`
# separators — NOT the ROS form `pkg/sub/Type`. Convert at the wire boundary so our
# pub/sub keyexprs and liveliness match a real `rmw_zenoh` peer (`std_msgs/msg/String`
# <-> `std_msgs::msg::dds_::String_`; see hiroz `ke.rs` / `discovery.rs`).
function _ros_to_dds_type(name::AbstractString)
    parts = split(String(name), '/')
    length(parts) < 2 && return String(name)            # non-standard; pass through
    string(join(parts[1:end-1], "::"), "::dds_::", parts[end], "_")
end
function _dds_to_ros_type(name::AbstractString)
    occursin("::dds_::", name) || return String(name)   # already ROS form / non-standard
    pre, post = split(String(name), "::dds_::"; limit=2) # ("std_msgs::msg", "String_")
    typ = endswith(post, '_') ? chop(post) : post
    string(replace(pre, "::" => "/"), '/', typ)
end

function topic_keyexpr(f::RmwZenoh, e::EndpointEntity)
    e.node === nothing &&
        throw(ArgumentError("rmw_zenoh topic key requires a node"))
    domain_id = e.node.domain_id
    topic = _strip_one_slash(e.topic)
    type_part = if e.type_info === nothing
        string(EMPTY_TOPIC_TYPE, '/', EMPTY_TOPIC_HASH)
    else
        # The type is the DDS name (`pkg::sub::dds_::Type_`) — a single component.
        string(_ros_to_dds_type(e.type_info.name), '/', to_rihs_string(e.type_info.hash))
    end
    return string(domain_id, '/', topic, '/', type_part)
end

# Inverse of the above. The topic preserves internal slashes, but the DDS type name
# (`pkg::sub::dds_::Type_`) is a SINGLE component (it uses `::`, not `/`), so we anchor
# on the fixed ends: domain is the first component, the RIHS hash is the last, the type
# is second-to-last, and the (slash-preserving) topic is everything between. The
# empty-type placeholder uses two components (`EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH`)
# instead, so it's detected first.
function parse_topic_keyexpr(f::RmwZenoh, ke::AbstractString)
    parts = split(String(ke), '/')

    domain_id = tryparse(Int, String(parts[1]))
    domain_id === nothing && throw(ArgumentError("bad domain_id $(repr(parts[1]))"))

    # Empty type: <domain>/<topic...>/EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH
    if length(parts) >= 4 && parts[end] == EMPTY_TOPIC_HASH && parts[end-1] == EMPTY_TOPIC_TYPE
        topic = join(parts[2:end-2], '/')
        return (; domain_id, topic, type_info=nothing)
    end

    # Real type: <domain>/<topic...>/<dds_type_name>/<RIHS hash>.
    # Need at least domain + 1 topic segment + the type + the hash.
    length(parts) >= 4 ||
        throw(ArgumentError("rmw_zenoh topic key needs >= 4 components for a typed topic, got $(length(parts))"))

    hash = type_hash_from_rihs_string(parts[end])
    hash === nothing && throw(ArgumentError("bad RIHS hash $(repr(String(parts[end])))"))

    type_name = _dds_to_ros_type(String(parts[end-1]))
    topic = join(parts[2:end-2], '/')

    return (; domain_id, topic, type_info=TypeInfo(type_name, hash))
end

# ---- Liveliness KE --------------------------------------------------------

function liveliness_keyexpr(f::RmwZenoh, e::EndpointEntity)
    e.node === nothing &&
        throw(ArgumentError("rmw_zenoh liveliness requires a node"))
    n = e.node

    # Empty namespace → "%"; otherwise mangle. Note "/" itself mangles to
    # "%" so a root-only namespace lands in the same place.
    ns = isempty(n.namespace) ? EMPTY_PLACEHOLDER : mangle(f, n.namespace)
    name = mangle(f, n.name)
    topic = mangle(f, _strip_trailing_slash(e.topic))

    type_part = if e.type_info === nothing
        string(EMPTY_TOPIC_TYPE, '/', EMPTY_TOPIC_HASH)
    else
        # DDS type name (`pkg::sub::dds_::Type_`) — a single token, no '/', as in the
        # data keyexpr; mangling is therefore a no-op on it.
        string(_ros_to_dds_type(e.type_info.name), '/', to_rihs_string(e.type_info.hash))
    end

    qos_str = encode_qos(f, e.qos)

    return string(admin_space(f), '/',
                  n.domain_id, '/', n.z_id, '/', n.id, '/', e.id, '/',
                  kind_code(e.kind), '/',
                  EMPTY_PLACEHOLDER, '/',  # enclave (never transmitted)
                  ns, '/', name, '/',
                  topic, '/',
                  type_part, '/',
                  qos_str)
end

function liveliness_keyexpr(f::RmwZenoh, n::NodeEntity)
    ns = isempty(n.namespace) ? EMPTY_PLACEHOLDER : mangle(f, n.namespace)
    name = mangle(f, n.name)
    return string(admin_space(f), '/',
                  n.domain_id, '/', n.z_id, '/', n.id, '/', n.id, '/',
                  "NN", '/',
                  EMPTY_PLACEHOLDER, '/',
                  ns, '/', name)
end

# ---- Parsing --------------------------------------------------------------

function parse_liveliness(f::RmwZenoh, ke::AbstractString)
    parts = split(String(ke), '/')
    length(parts) >= 9 || throw(ArgumentError("rmw_zenoh liveliness has at least 9 components, got $(length(parts))"))

    parts[1] == admin_space(f) ||
        throw(ArgumentError("expected admin space $(admin_space(f)), got $(repr(parts[1]))"))

    domain_id = tryparse(Int, String(parts[2]))
    domain_id === nothing && throw(ArgumentError("bad domain_id $(repr(parts[2]))"))

    z_id = ZenohId(String(parts[3]))

    node_id = tryparse(Int, String(parts[4]))
    node_id === nothing && throw(ArgumentError("bad node_id $(repr(parts[4]))"))

    entity_id = tryparse(Int, String(parts[5]))
    entity_id === nothing && throw(ArgumentError("bad entity_id $(repr(parts[5]))"))

    kind_str = String(parts[6])

    enclave = parts[7] == EMPTY_PLACEHOLDER ? "" : demangle(f, parts[7])
    namespace = parts[8] == EMPTY_PLACEHOLDER ? "" : demangle(f, parts[8])
    node_name = demangle(f, parts[9])

    node = NodeEntity(domain_id, z_id, node_id, node_name, namespace, enclave)

    if kind_str == "NN"
        return node
    end

    kind = parse_kind_code(kind_str)
    kind === nothing && throw(ArgumentError("unknown entity kind $(repr(kind_str))"))

    length(parts) >= 13 || throw(ArgumentError("rmw_zenoh endpoint liveliness has 13 components, got $(length(parts))"))

    topic = demangle(f, parts[10])
    topic_type = String(parts[11])
    topic_hash = String(parts[12])
    qos_str = String(parts[13])

    type_info = if topic_type == EMPTY_TOPIC_TYPE || topic_hash == EMPTY_TOPIC_HASH
        nothing
    else
        h = type_hash_from_rihs_string(topic_hash)
        h === nothing && (h = TypeHash(0x00, ntuple(_ -> 0x00, 32)))
        TypeInfo(_dds_to_ros_type(topic_type), h)
    end

    _, qos = decode_qos(f, qos_str)

    return EndpointEntity(entity_id, node, kind, topic, type_info, qos)
end
