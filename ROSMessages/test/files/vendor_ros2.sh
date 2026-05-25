#!/usr/bin/env bash
# Vendor ROS2 .msg / .srv / .action files used by the integration tests.
#
# Output layout (already gitignored — only NOTICE.md is tracked):
#   test/files/ros2_standard/<pkg>/{msg,srv,action}/<File>.<ext>
#   test/files/ros2_ecosystem/<pkg>/{msg,srv,action}/<File>.<ext>
#
# Usage:
#   ./test/files/vendor_ros2.sh                  # vendor into the default tree
#   ./test/files/vendor_ros2.sh /alt/output/dir  # vendor under an alternate root
#
# Each upstream repo is cloned shallowly into a scratch directory and only
# its interface definition files are copied out. The clone scratch is left
# in /tmp so re-running picks up local clones if present — delete /tmp/ros2_vendor
# to force a fresh fetch.

set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
out_root=${1:-"$repo_root/test/files"}
scratch=${SCRATCH_DIR:-/tmp/ros2_vendor}

mkdir -p "$scratch"

# Args: scratch-subdir-name, github-org/repo
clone_if_absent() {
    local name=$1 repo=$2
    if [[ ! -d "$scratch/$name" ]]; then
        git clone --depth=1 --quiet "https://github.com/$repo.git" "$scratch/$name"
        echo "  cloned $repo"
    else
        echo "  using cached clone of $repo"
    fi
}

# Copy every .msg/.srv/.action from the given repo subdir into
# $out_root/$dest_root/<pkg>/<kind>/<file>. The package name is inferred
# from the directory that contains the kind dir (msg/srv/action).
vendor_from() {
    local src=$1 dest_root=$2
    find "$src" \( -name '*.msg' -o -name '*.srv' -o -name '*.action' \) -print0 |
        while IFS= read -r -d '' f; do
            local fname=${f##*/}
            local parent=${f%/*}
            local subdir=${parent##*/}
            local grandparent=${parent%/*}
            local pkg=${grandparent##*/}
            local dest="$out_root/$dest_root/$pkg/$subdir/$fname"
            mkdir -p "${dest%/*}"
            cp "$f" "$dest"
        done
}

echo "→ standard interfaces"
clone_if_absent common_interfaces      ros2/common_interfaces
clone_if_absent rcl_interfaces         ros2/rcl_interfaces
clone_if_absent unique_identifier_msgs ros2/unique_identifier_msgs

rm -rf "$out_root/ros2_standard"/*/
vendor_from "$scratch/common_interfaces"      ros2_standard
vendor_from "$scratch/rcl_interfaces"         ros2_standard
vendor_from "$scratch/unique_identifier_msgs" ros2_standard

echo "→ ecosystem interfaces"
clone_if_absent navigation2                ros-navigation/navigation2
clone_if_absent moveit_msgs                moveit/moveit_msgs
clone_if_absent control_msgs               ros-controls/control_msgs
clone_if_absent px4_msgs                   PX4/px4_msgs
clone_if_absent slam_toolbox               SteveMacenski/slam_toolbox
# Transitive deps surfaced by the above (object_recognition by moveit,
# geographic + uuid by nav2).
clone_if_absent octomap_msgs               OctoMap/octomap_msgs
clone_if_absent object_recognition_msgs    wg-perception/object_recognition_msgs
clone_if_absent geographic_info            ros-geographic-info/geographic_info
clone_if_absent unique_identifier          ros-geographic-info/unique_identifier

rm -rf "$out_root/ros2_ecosystem"/*/
for r in navigation2 moveit_msgs control_msgs px4_msgs slam_toolbox \
         octomap_msgs object_recognition_msgs geographic_info unique_identifier; do
    vendor_from "$scratch/$r" ros2_ecosystem
done

msg=$(find "$out_root/ros2_standard" "$out_root/ros2_ecosystem" -name '*.msg' | wc -l)
srv=$(find "$out_root/ros2_standard" "$out_root/ros2_ecosystem" -name '*.srv' | wc -l)
act=$(find "$out_root/ros2_standard" "$out_root/ros2_ecosystem" -name '*.action' | wc -l)
echo "✓ vendored: $msg msg, $srv srv, $act action (total $((msg + srv + act)))"
