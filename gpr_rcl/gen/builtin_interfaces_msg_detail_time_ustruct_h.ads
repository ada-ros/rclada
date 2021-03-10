pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with stddef_h;

package builtin_interfaces_msg_detail_time_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from builtin_interfaces:msg/Time.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  -- Struct defined in msg/Time in the package builtin_interfaces.
   type builtin_interfaces_u_msg_u_Time is record
      sec : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:23
      nanosec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:24
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:21

  -- Struct for a sequence of builtin_interfaces__msg__Time.
   type builtin_interfaces_u_msg_u_Time_u_Sequence is record
      data : access builtin_interfaces_u_msg_u_Time;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:30
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:32
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:34
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/builtin_interfaces/msg/detail/time__struct.h:28

  --/ The number of valid items in data
  --/ The number of allocated items in data
end builtin_interfaces_msg_detail_time_ustruct_h;
