# The shipped v1 manifest (DESIGN-MAPPING.md §5). A `const` value — no global mutation,
# precompile-clean. Users `merge` their own manifest over this.

const BUILTIN_MAPPERS = MapperManifest(
    # std_msgs
    MapperEntry(NameMatch("std_msgs/msg/String"), map_std_string!; desc = "TextLog"),
    MapperEntry(NameMatch("std_msgs/msg/Bool"),   map_std_scalar!; desc = "Scalars(data)"),
    MapperEntry(NameMatch("std_msgs/msg/Char"),   map_std_scalar!; desc = "Scalars(data)"),
    MapperEntry(NameMatch("std_msgs/msg/Byte"),   map_std_scalar!; desc = "Scalars(data)"),
    MapperEntry(NameMatch("std_msgs/msg/Int*"),   map_std_scalar!; desc = "Scalars(data)"),
    MapperEntry(NameMatch("std_msgs/msg/UInt*"),  map_std_scalar!; desc = "Scalars(data)"),
    MapperEntry(NameMatch("std_msgs/msg/Float*"), map_std_scalar!; desc = "Scalars(data)"),
    # geometry_msgs
    MapperEntry(NameMatch("geometry_msgs/msg/Pose"),        map_pose!;       desc = "Transform3D"),
    MapperEntry(NameMatch("geometry_msgs/msg/PoseStamped"), map_pose_stamped!; desc = "Transform3D"),
    MapperEntry(NameMatch("geometry_msgs/msg/Transform"),   map_transform!;  desc = "Transform3D"),
    MapperEntry(NameMatch("geometry_msgs/msg/Point"),       map_point!;      desc = "Points3D"),
    MapperEntry(NameMatch("geometry_msgs/msg/Point32"),     map_point!;      desc = "Points3D"),
    MapperEntry(NameMatch("geometry_msgs/msg/Vector3"),     map_vector3!;    desc = "Scalars x/y/z"),
    MapperEntry(NameMatch("geometry_msgs/msg/Quaternion"),  map_quaternion!; desc = "Scalars x/y/z/w"),
    MapperEntry(NameMatch("geometry_msgs/msg/Twist"),       map_twist!;      desc = "Scalars linear/*,angular/*"),
)
