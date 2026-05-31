# Used to determine which of the next *_value fields are set.
# ParameterType.PARAMETER_NOT_SET indicates that the parameter was not set
# (if gotten) or is uninitialized.
uint8 type

# Boolean value, can be either true or false.
bool bool_value

# Integer value ranging from -9223372036854775808 to 9223372036854775807.
int64 integer_value

# A double precision floating point value following IEEE 754.
float64 double_value

# A textual value with no practical length limit.
string string_value

# An array of bytes, used for non-textual information.
byte[] byte_array_value

# An array of boolean values.
bool[] bool_array_value

# An array of 64-bit integer values.
int64[] integer_array_value

# An array of 64-bit floating point values.
float64[] double_array_value

# An array of string values.
string[] string_array_value
