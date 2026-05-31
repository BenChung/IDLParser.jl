# Primary state definitions as depicted in
# http://design.ros2.org/articles/node_lifecycle.html

# These are the primary states. State changes can only be requested when the
# node is in one of these states.

# Indicates state has not yet been set.
uint8 PRIMARY_STATE_UNKNOWN = 0

# This is the life cycle state the node is in immediately after being
# instantiated.
uint8 PRIMARY_STATE_UNCONFIGURED = 1

# This state represents a node that is not currently performing any processing.
uint8 PRIMARY_STATE_INACTIVE = 2

# This is the main state of the node's life cycle. While in this state, the node
# performs any processing, responds to service requests, reads and processes
# data, produces output, etc.
uint8 PRIMARY_STATE_ACTIVE = 3

# The finalized state is the state in which the node ends immediately before
# being destroyed.
uint8 PRIMARY_STATE_FINALIZED = 4

# These are the transition states. The node will be in one of these states when
# a transition is in progress.

# The node's onConfigure callback is being executed.
uint8 TRANSITION_STATE_CONFIGURING = 10

# The node's onCleanup callback is being executed.
uint8 TRANSITION_STATE_CLEANINGUP = 11

# The node's onShutdown callback is being executed.
uint8 TRANSITION_STATE_SHUTTINGDOWN = 12

# The node's onActivate callback is being executed.
uint8 TRANSITION_STATE_ACTIVATING = 13

# The node's onDeactivate callback is being executed.
uint8 TRANSITION_STATE_DEACTIVATING = 14

# The node's onError callback is being executed.
uint8 TRANSITION_STATE_ERRORPROCESSING = 15

# The state id value from the above definitions.
uint8 id

# A text label of the state.
string label
