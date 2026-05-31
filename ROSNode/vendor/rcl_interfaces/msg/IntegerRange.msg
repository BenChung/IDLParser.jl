# Represents bounds and a step value for an integer typed parameter.

# Begin of the range that defines the valid values and its meaning.
int64 from_value

# End of the range that defines the valid values and its meaning.
int64 to_value

# Set to a positive value to bound the step of the range, leave at 0 for
# continuous values.
uint64 step
