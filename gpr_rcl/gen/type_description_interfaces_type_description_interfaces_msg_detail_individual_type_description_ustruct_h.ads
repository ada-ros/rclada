pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rosidl_runtime_c_rosidl_runtime_c_string_h;
with type_description_interfaces_type_description_interfaces_msg_detail_field_ustruct_h;
with stddef_h;

package type_description_interfaces_type_description_interfaces_msg_detail_individual_type_description_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:msg/IndividualTypeDescription.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/msg/individual_type_description.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'type_name'
  -- Member 'fields'
  -- constants for array fields with an upper bound
  -- type_name
  --/ Struct defined in msg/IndividualTypeDescription in the package type_description_interfaces.
  --*
  --  * Represents a single type, without the types it references, if any.
  --  

  --/ Name of the type.
  --/ This is limited to 255 characters.
  --/ TODO(wjwwood): this 255 character limit was chosen due to this being the limit
  --/   for DDSI-RTPS based middlewares, which is the most commonly used right now.
  --/   We lack a ROS 2 specific limit in our design documents, but we should update
  --/   this and/or link to the design doc when that is available.
   type type_description_interfaces_u_msg_u_IndividualTypeDescription is record
      type_name : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:47
      fields : aliased type_description_interfaces_type_description_interfaces_msg_detail_field_ustruct_h.type_description_interfaces_u_msg_u_Field_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:49
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:39

  --/ Fields of the type.
  -- Struct for a sequence of type_description_interfaces__msg__IndividualTypeDescription.
   type type_description_interfaces_u_msg_u_IndividualTypeDescription_u_Sequence is record
      data : access type_description_interfaces_u_msg_u_IndividualTypeDescription;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:55
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:57
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:59
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/individual_type_description__struct.h:53

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_msg_detail_individual_type_description_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
