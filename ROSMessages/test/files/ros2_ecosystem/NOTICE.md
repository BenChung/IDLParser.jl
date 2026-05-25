# ROS 2 ecosystem interface files

This directory holds `.msg` / `.srv` / `.action` files vendored from
community ROS 2 packages built on top of the standard interfaces. They
exercise the parser, code generator, and CDR codec against the breadth
of types real-world robotics stacks publish, and power
`test/ros2_ecosystem.jl`.

The contents are **not** tracked in git — only this NOTICE and the vendor
script are. To populate (or refresh) the tree, run:

    test/files/vendor_ros2.sh

The script shallow-clones each repo into `/tmp/ros2_vendor/` and copies the
interface definition files out. Re-running is idempotent.

Direct sources (each under its own permissive license — Apache-2.0, BSD,
or similar):

- https://github.com/ros-navigation/navigation2 — nav2_msgs, dwb_msgs, nav_2d_msgs
- https://github.com/moveit/moveit_msgs — moveit_msgs
- https://github.com/ros-controls/control_msgs — control_msgs
- https://github.com/PX4/px4_msgs — px4_msgs
- https://github.com/SteveMacenski/slam_toolbox — slam_toolbox

Transitive dependencies (pulled in because the above reference them):

- https://github.com/OctoMap/octomap_msgs — octomap_msgs (referenced by moveit_msgs)
- https://github.com/wg-perception/object_recognition_msgs — object_recognition_msgs (referenced by moveit_msgs)
- https://github.com/ros-geographic-info/geographic_info — geographic_msgs (referenced by nav2_msgs)
- https://github.com/ros-geographic-info/unique_identifier — uuid_msgs (referenced by geographic_msgs)

When this NOTICE was last updated, the vendor produced 493 interface files
(391 .msg, 69 .srv, 33 .action) across 11 packages.
