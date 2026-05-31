# The following are the transitions which the user can initiate. They are the
# only transitions which can be requested over the lifecycle service interface.

# This transition will instantiate the node, but will not run any code beyond the
# constructor.
uint8 TRANSITION_CREATE = 0

# The node's onConfigure callback will be called to allow the node to load its
# configuration and conduct any required setup.
uint8 TRANSITION_CONFIGURE = 1

# The node's onCleanup callback will be called to clear all state and return the
# node to a functionally equivalent state as when first created.
uint8 TRANSITION_CLEANUP = 2

# The node's onActivate callback will be called to do any final preparations to
# start executing.
uint8 TRANSITION_ACTIVATE = 3

# The node's onDeactivate callback will be called to do any cleanup to start
# executing, and reverse the onActivate changes.
uint8 TRANSITION_DEACTIVATE = 4

# This shutdown transition will simply cause the deallocation of the node.
uint8 TRANSITION_UNCONFIGURED_SHUTDOWN = 5

# This shutdown transition will simply cause the deallocation of the node.
uint8 TRANSITION_INACTIVE_SHUTDOWN = 6

# This shutdown transition will simply cause the deallocation of the node.
uint8 TRANSITION_ACTIVE_SHUTDOWN = 7

# This transition will simply cause the deallocation of the node.
uint8 TRANSITION_DESTROY = 8

# The rest of these transitions are internal to the state machine implementation.
# They are the result of callbacks, and may not be triggered directly.

uint8 TRANSITION_ON_CONFIGURE_SUCCESS = 10
uint8 TRANSITION_ON_CONFIGURE_FAILURE = 11
uint8 TRANSITION_ON_CONFIGURE_ERROR = 12

uint8 TRANSITION_ON_CLEANUP_SUCCESS = 20
uint8 TRANSITION_ON_CLEANUP_FAILURE = 21
uint8 TRANSITION_ON_CLEANUP_ERROR = 22

uint8 TRANSITION_ON_ACTIVATE_SUCCESS = 30
uint8 TRANSITION_ON_ACTIVATE_FAILURE = 31
uint8 TRANSITION_ON_ACTIVATE_ERROR = 32

uint8 TRANSITION_ON_DEACTIVATE_SUCCESS = 40
uint8 TRANSITION_ON_DEACTIVATE_FAILURE = 41
uint8 TRANSITION_ON_DEACTIVATE_ERROR = 42

uint8 TRANSITION_ON_SHUTDOWN_SUCCESS = 50
uint8 TRANSITION_ON_SHUTDOWN_FAILURE = 51
uint8 TRANSITION_ON_SHUTDOWN_ERROR = 52

uint8 TRANSITION_ON_ERROR_SUCCESS = 60
uint8 TRANSITION_ON_ERROR_FAILURE = 61
uint8 TRANSITION_ON_ERROR_ERROR = 62

uint8 TRANSITION_CALLBACK_SUCCESS = 97
uint8 TRANSITION_CALLBACK_FAILURE = 98
uint8 TRANSITION_CALLBACK_ERROR = 99

# The transition id value from the above definitions.
uint8 id

# A text label of the transition.
string label
