pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rosidl_runtime_c_rosidl_runtime_c_string_h;
with type_description_interfaces_type_description_interfaces_msg_detail_field_type_ustruct_h;
with stddef_h;

package type_description_interfaces_type_description_interfaces_msg_detail_field_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:msg/Field.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/msg/field.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'name'
  -- Member 'default_value'
  -- Member 'type'
  --/ Struct defined in msg/Field in the package type_description_interfaces.
  --*
  --  * Represents a single field in a type.
  --  

  --/ Name of the field.
   type type_description_interfaces_u_msg_u_Field is record
      name : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:36
      c_type : aliased type_description_interfaces_type_description_interfaces_msg_detail_field_type_ustruct_h.type_description_interfaces_u_msg_u_FieldType;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:38
      default_value : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:33

  --/ Type of the field, including details about the type like length, nested name, etc.
  --/ Literal default value of the field as a string, as it appeared in the original
  --/ message description file, whether that be .msg/.srv/.action or .idl.
  -- Struct for a sequence of type_description_interfaces__msg__Field.
   type type_description_interfaces_u_msg_u_Field_u_Sequence is record
      data : access type_description_interfaces_u_msg_u_Field;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:47
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:49
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:51
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/field__struct.h:45

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_msg_detail_field_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
