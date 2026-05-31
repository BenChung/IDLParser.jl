# This is the message to communicate a parameter's descriptor.

# The name of the parameter.
string name

# Enum values are defined in `ParameterType.msg`.
uint8 type

# Description of the parameter, visible from introspection tools.
string description

# Plain English description of additional constraints which cannot be expressed
# with the available constraints, e.g. "only prime numbers".
#
# By convention, this should only be used to clarify already-expressed
# constraints, not replace them.
string additional_constraints

# If 'true' then the value cannot change after it has been initialized.
bool read_only false

# If true, the parameter is allowed to change type.
bool dynamic_typing false

# FloatingPointRange consists of a from_value, a to_value, and a step.
# FloatingPointRange should be used for all parameters which require a floating
# point value, and may only be used once in a descriptor.
FloatingPointRange[<=1] floating_point_range

# IntegerRange consists of a from_value, a to_value, and a step.
# IntegerRange should be used for all parameters which require an integer value,
# and may only be used once in a descriptor.
IntegerRange[<=1] integer_range
