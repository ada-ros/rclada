pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with stddef_h;

package unique_identifier_msgs_unique_identifier_msgs_msg_detail_uuid_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from unique_identifier_msgs:msg/UUID.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "unique_identifier_msgs/msg/uuid.h"
  -- Constants defined in the message
  --/ Struct defined in msg/UUID in the package unique_identifier_msgs.
  --*
  --  * A universally unique identifier (UUID).
  --  *
  --  *  http://en.wikipedia.org/wiki/Universally_unique_identifier
  --  *  http://tools.ietf.org/html/rfc4122.html
  --  

   type anon_array1122 is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type unique_identifier_msgs_u_msg_u_UUID is record
      uuid : aliased anon_array1122;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:31
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:29

  -- Struct for a sequence of unique_identifier_msgs__msg__UUID.
   type unique_identifier_msgs_u_msg_u_UUID_u_Sequence is record
      data : access unique_identifier_msgs_u_msg_u_UUID;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:37
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:39
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/unique_identifier_msgs/unique_identifier_msgs/msg/detail/uuid__struct.h:35

  --/ The number of valid items in data
  --/ The number of allocated items in data
end unique_identifier_msgs_unique_identifier_msgs_msg_detail_uuid_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
