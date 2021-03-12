# Simple types
int64      number
string     text
string<=8  bounded_string
float64    real

# Structured types
builtin_interfaces/Time  time

# Array types
float32[]  dynamic_array
int32[8]   static_array
int8[<=8]  bounded_array

builtin_interfaces/Time[2]  fix_msg_arr
builtin_interfaces/Time[]   dyn_msg_arr

# Matrix types
std_msgs/UInt64MultiArray  matrix

