pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rosidl_runtime_c_rosidl_runtime_c_string_h;
with stddef_h;

package type_description_interfaces_type_description_interfaces_msg_detail_key_value_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:msg/KeyValue.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/msg/key_value.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'key'
  -- Member 'value'
  --/ Struct defined in msg/KeyValue in the package type_description_interfaces.
  --*
  --  * Represents an arbitrary key-value pair for application-specific information.
  --  

   type type_description_interfaces_u_msg_u_KeyValue is record
      key : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:33
      value : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:34
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:31

  -- Struct for a sequence of type_description_interfaces__msg__KeyValue.
   type type_description_interfaces_u_msg_u_KeyValue_u_Sequence is record
      data : access type_description_interfaces_u_msg_u_KeyValue;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:40
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:42
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:44
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/key_value__struct.h:38

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_msg_detail_key_value_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
