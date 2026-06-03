# A sensor reading, published by some other node on the graph. A BYO interface
# (no ROS2 install needed) — referenced from warming.jl via `@ros_import from=`.
string sensor_id
float64 value
float64[] history
