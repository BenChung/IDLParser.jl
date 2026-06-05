# Constants for the type of event this message represents.
uint8 REQUEST_SENT = 0
uint8 REQUEST_RECEIVED = 1
uint8 RESPONSE_SENT = 2
uint8 RESPONSE_RECEIVED = 3

# The type of event this message represents.
uint8 event_type

# Timestamp for when the event occurred.
builtin_interfaces/Time stamp

# Unique identifier of the client that sent the service request.
# For the introspection feature, this is the GID of the client's request publisher.
char[16] client_gid

# Sequence number of the event, for ordering events from the same client.
int64 sequence_number
