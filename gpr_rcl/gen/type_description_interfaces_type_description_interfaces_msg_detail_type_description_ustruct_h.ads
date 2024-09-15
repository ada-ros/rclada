pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with type_description_interfaces_type_description_interfaces_msg_detail_individual_type_description_ustruct_h;
with stddef_h;

package type_description_interfaces_type_description_interfaces_msg_detail_type_description_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:msg/TypeDescription.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/msg/type_description.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'type_description'
  -- Member 'referenced_type_descriptions'
  --/ Struct defined in msg/TypeDescription in the package type_description_interfaces.
  --*
  --  * Represents a complete type description, including the type itself as well as the types it references.
  --  

  --/ Description of the type.
   type type_description_interfaces_u_msg_u_TypeDescription is record
      type_description : aliased type_description_interfaces_type_description_interfaces_msg_detail_individual_type_description_ustruct_h.type_description_interfaces_u_msg_u_IndividualTypeDescription;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:34
      referenced_type_descriptions : aliased type_description_interfaces_type_description_interfaces_msg_detail_individual_type_description_ustruct_h.type_description_interfaces_u_msg_u_IndividualTypeDescription_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:36
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:31

  --/ Descriptions of all referenced types, recursively.
  -- Struct for a sequence of type_description_interfaces__msg__TypeDescription.
   type type_description_interfaces_u_msg_u_TypeDescription_u_Sequence is record
      data : access type_description_interfaces_u_msg_u_TypeDescription;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:42
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:44
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:46
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_description__struct.h:40

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_msg_detail_type_description_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
