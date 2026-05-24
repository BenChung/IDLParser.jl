# ROS 2 standard interface files

This directory holds `.msg` / `.srv` / `.action` files vendored from the
upstream ROS 2 standard interface packages. They power `test/ros2_standard.jl`,
which exercises the full ROS2 → IDL → CDR pipeline against the real-world
type catalog.

The contents are **not** tracked in git — only this NOTICE and the vendor
script are. To populate (or refresh) the tree, run:

    test/files/vendor_ros2.sh

The script shallow-clones each repo into `/tmp/ros2_vendor/` and copies the
interface definition files out. Re-running is idempotent.

Sources (all Apache-2.0 licensed, default branch):

- https://github.com/ros2/common_interfaces — std_msgs, geometry_msgs, sensor_msgs, nav_msgs, diagnostic_msgs, shape_msgs, stereo_msgs, trajectory_msgs, visualization_msgs, std_srvs
- https://github.com/ros2/rcl_interfaces — action_msgs, builtin_interfaces, composition_interfaces, lifecycle_msgs, rcl_interfaces, rosgraph_msgs, service_msgs, statistics_msgs, test_msgs, type_description_interfaces
- https://github.com/ros2/unique_identifier_msgs — unique_identifier_msgs

When this NOTICE was last updated, the vendor produced 192 interface files
(163 .msg, 28 .srv, 1 .action) across 21 packages.
