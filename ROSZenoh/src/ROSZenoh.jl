"""
    ROSZenoh

ROS2-to-Zenoh key expression and entity modelling. The primary target is
`rmw_zenoh_cpp` (the native ROS2 Zenoh RMW) via the `RmwZenoh` formatter —
this is the path that should match the `hiroz-protocol` Rust reference
byte-for-byte and stay so. The `Ros2DDS` formatter is a secondary path for
interop with the older `zenoh-plugin-ros2dds` DDS bridge; it exists for
compatibility, but the primary code paths and defaults assume RmwZenoh.
"""
module ROSZenoh

import Zenoh
using SHA: sha256

include("entities.jl")
include("keyexpr.jl")
include("rmw_zenoh.jl")
include("ros2dds.jl")
include("bridge.jl")
include("attachment.jl")

export ZenohId, TypeHash, TypeInfo, QosProfile,
       NodeEntity, EndpointEntity, EndpointKind,
       Publisher, Subscription, Service, Client,
       KeyExprFormat, RmwZenoh, Ros2DDS,
       topic_keyexpr, parse_topic_keyexpr, liveliness_keyexpr, parse_liveliness,
       encode_qos, decode_qos, mangle, demangle,
       to_rihs_string, type_hash_from_rihs_string, default_qos,
       type_info_from_struct,
       entity_gid, encode_attachment, decode_attachment

end # module ROSZenoh
