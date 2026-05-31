# This is the message to communicate a parameter. It is an open struct with an
# enum to select which value is in use.

# The full name of the parameter.
string name

# The parameter's value, which can be one of several types as defined in the
# ParameterValue message.
ParameterValue value
