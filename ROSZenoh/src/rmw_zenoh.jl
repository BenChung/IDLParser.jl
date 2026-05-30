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
# Format: <reliability>:<durability>:<history>:,:,:,,
# - reliability/durability emit empty string when at default (Reliable /
#   Volatile), otherwise "1" or "2".
# - history is ",<depth>" when default (KeepLast(10)), "1,<depth>" when
#   non-default keep-last, "2," for KeepAll.
# - deadline/lifespan/liveliness are always ",", ",", ",,".

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
    return string(rel, ':', dur, ':', hist, ":,:,:,,")
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
    if history_parts[1] == "" || history_parts[1] == "1"
        depth = tryparse(Int, String(history_parts[2]))
        depth === nothing && throw(ArgumentError("invalid QoS depth $(repr(history_parts[2]))"))
        return (false, QosProfile(reliability=reliability, durability=durability,
                                  history=:keep_last, depth=depth))
    elseif history_parts[1] == "2"
        return (false, QosProfile(reliability=reliability, durability=durability,
                                  history=:keep_all, depth=0))
    else
        throw(ArgumentError("invalid QoS history kind $(repr(history_parts[1]))"))
    end
end

# ---- Topic KE -------------------------------------------------------------

function topic_keyexpr(f::RmwZenoh, e::EndpointEntity)
    e.node === nothing &&
        throw(ArgumentError("rmw_zenoh topic key requires a node"))
    domain_id = e.node.domain_id
    topic = _strip_one_slash(e.topic)
    type_part = if e.type_info === nothing
        string(EMPTY_TOPIC_TYPE, '/', EMPTY_TOPIC_HASH)
    else
        # Note: type *name* is demangled (so stored-mangled names like
        # `std_msgs%msg%String` are rendered as `std_msgs/msg/String`).
        # The hash string is also demangled — practically a no-op since
        # `RIHS01_<hex>` never contains `%`, but it matches the reference.
        type_name = demangle(f, e.type_info.name)
        type_hash = demangle(f, to_rihs_string(e.type_info.hash))
        string(type_name, '/', type_hash)
    end
    return string(domain_id, '/', topic, '/', type_part)
end

# Inverse of the above. The topic and type name both preserve internal
# slashes, so they can't be split positionally on their own — instead we anchor
# on the fixed ends: domain is the first component, the RIHS hash is the last,
# and a ROS2 type name is always exactly three components (`<pkg>/<qualifier>/
# <Type>`). The empty-type placeholder uses two components
# (`EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH`) instead, so it's detected first.
function parse_topic_keyexpr(f::RmwZenoh, ke::AbstractString)
    parts = split(String(ke), '/')

    domain_id = tryparse(Int, String(parts[1]))
    domain_id === nothing && throw(ArgumentError("bad domain_id $(repr(parts[1]))"))

    # Empty type: <domain>/<topic...>/EMPTY_TOPIC_TYPE/EMPTY_TOPIC_HASH
    if length(parts) >= 4 && parts[end] == EMPTY_TOPIC_HASH && parts[end-1] == EMPTY_TOPIC_TYPE
        topic = join(parts[2:end-2], '/')
        return (; domain_id, topic, type_info=nothing)
    end

    # Real type: <domain>/<topic...>/<pkg>/<qualifier>/<Type>/<RIHS hash>.
    # Need at least domain + 1 topic segment + 3 type segments + hash.
    length(parts) >= 6 ||
        throw(ArgumentError("rmw_zenoh topic key needs >= 6 components for a typed topic, got $(length(parts))"))

    hash = type_hash_from_rihs_string(parts[end])
    hash === nothing && throw(ArgumentError("bad RIHS hash $(repr(String(parts[end])))"))

    type_name = join(parts[end-3:end-1], '/')
    topic = join(parts[2:end-4], '/')

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
        string(mangle(f, e.type_info.name), '/', to_rihs_string(e.type_info.hash))
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
        TypeInfo(demangle(f, topic_type), h)
    end

    _, qos = decode_qos(f, qos_str)

    return EndpointEntity(entity_id, node, kind, topic, type_info, qos)
end
