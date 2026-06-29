# Built-in geometry_msgs mappers (DESIGN-MAPPING.md §5.1). Duck-typed (field access),
# so they work on dynamic or authored types alike. Quaternions are scalar-last (x,y,z,w),
# matching RotationQuat; points/translations are raw NTuples (a component struct fails the
# binding's fixed-size-list `_fits`, cdata.jl:495).

"""geometry_msgs/msg/Pose → Transform3D archetype value (reusable builder)."""
pose_transform(p) = Rerun.Archetypes.Transform3D(;
    translation = [(Float32(p.position.x), Float32(p.position.y), Float32(p.position.z))],
    quaternion  = [(Float32(p.orientation.x), Float32(p.orientation.y),
                    Float32(p.orientation.z), Float32(p.orientation.w))])

"""geometry_msgs/msg/Transform → Transform3D archetype value."""
transform_transform(t) = Rerun.Archetypes.Transform3D(;
    translation = [(Float32(t.translation.x), Float32(t.translation.y), Float32(t.translation.z))],
    quaternion  = [(Float32(t.rotation.x), Float32(t.rotation.y),
                    Float32(t.rotation.z), Float32(t.rotation.w))])

"""geometry_msgs/msg/Point (or Point32) → a single Position3D NTuple."""
point_position(p) = (Float32(p.x), Float32(p.y), Float32(p.z))

map_pose!(sink, m, ctx)          = (emit!(sink, ctx, pose_transform(m)); nothing)
map_transform!(sink, m, ctx)     = (emit!(sink, ctx, transform_transform(m)); nothing)
map_pose_stamped!(sink, m, ctx)  = (emit!(sink, ctx, pose_transform(m.pose)); nothing)
map_point!(sink, m, ctx)         = (emit!(sink, ctx, Rerun.Archetypes.Points3D([point_position(m)])); nothing)

function map_vector3!(sink, m, ctx)
    emit!(sink, ctx, "x", _scalars(m.x)); emit!(sink, ctx, "y", _scalars(m.y)); emit!(sink, ctx, "z", _scalars(m.z))
    return nothing
end

function map_quaternion!(sink, m, ctx)
    emit!(sink, ctx, "x", _scalars(m.x)); emit!(sink, ctx, "y", _scalars(m.y))
    emit!(sink, ctx, "z", _scalars(m.z)); emit!(sink, ctx, "w", _scalars(m.w))
    return nothing
end

function map_twist!(sink, m, ctx)
    emit!(sink, ctx, "linear/x",  _scalars(m.linear.x));  emit!(sink, ctx, "linear/y",  _scalars(m.linear.y))
    emit!(sink, ctx, "linear/z",  _scalars(m.linear.z));  emit!(sink, ctx, "angular/x", _scalars(m.angular.x))
    emit!(sink, ctx, "angular/y", _scalars(m.angular.y)); emit!(sink, ctx, "angular/z", _scalars(m.angular.z))
    return nothing
end
