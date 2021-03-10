pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with stddef_h;

package unique_identifier_msgs_msg_detail_uuid_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from unique_identifier_msgs:msg/UUID.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  -- Struct defined in msg/UUID in the package unique_identifier_msgs.
   type unique_identifier_msgs_u_msg_u_UUID_uuid_array is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type unique_identifier_msgs_u_msg_u_UUID is record
      uuid : aliased unique_identifier_msgs_u_msg_u_UUID_uuid_array;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:23
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:21

  -- Struct for a sequence of unique_identifier_msgs__msg__UUID.
   type unique_identifier_msgs_u_msg_u_UUID_u_Sequence is record
      data : access unique_identifier_msgs_u_msg_u_UUID;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:29
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:31
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/unique_identifier_msgs/msg/detail/uuid__struct.h:27

  --/ The number of valid items in data
  --/ The number of allocated items in data
end unique_identifier_msgs_msg_detail_uuid_ustruct_h;
