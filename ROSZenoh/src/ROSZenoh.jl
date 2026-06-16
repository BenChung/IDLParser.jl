"""
    ROSZenoh

Maps ROS 2 entities and topics to Zenoh key expressions and QoS. Two RMW
formatters share this model: `RmwZenoh` targets `rmw_zenoh_cpp` (the native
ROS 2 Zenoh RMW) and matches the `hiroz-protocol` Rust reference byte-for-byte,
so it drives the default code paths; `Ros2DDS` bridges to the
`zenoh-plugin-ros2dds` DDS plugin for cross-vendor interop.

See https://docs.ros.org/en/rolling/Concepts/Intermediate/About-Different-Middleware-Vendors.html
"""
module ROSZenoh

import Zenoh
using SHA: sha256

include("entities.jl")
include("keyexpr.jl")
include("qos.jl")
include("rmw_zenoh.jl")
include("ros2dds.jl")
include("bridge.jl")
include("attachment.jl")

export ZenohId, TypeHash, TypeInfo, QosProfile, Duration,
       NodeEntity, EndpointEntity, EndpointKind,
       PublisherKind, SubscriptionKind, ServiceKind, ClientKind,
       Publisher, Subscription, Service, Client,
       KeyExprFormat, RmwZenoh, Ros2DDS,
       topic_keyexpr, parse_topic_keyexpr, liveliness_keyexpr, parse_liveliness,
       encode_qos, decode_qos, mangle, demangle,
       to_rihs_string, type_hash_from_rihs_string, default_qos,
       type_info_from_struct,
       entity_gid, encode_attachment, decode_attachment,
       qos_compatible, QosIncompatibility

end # module ROSZenoh
