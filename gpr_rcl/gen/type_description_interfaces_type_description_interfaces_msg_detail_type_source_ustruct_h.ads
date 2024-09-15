pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rosidl_runtime_c_rosidl_runtime_c_string_h;
with stddef_h;

package type_description_interfaces_type_description_interfaces_msg_detail_type_source_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:msg/TypeSource.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/msg/type_source.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'type_name'
  -- Member 'encoding'
  -- Member 'raw_file_contents'
  --/ Struct defined in msg/TypeSource in the package type_description_interfaces.
  --*
  --  * Represents the original source of a ROS 2 interface definition.
  --  

  --/ ROS interface type name, in PACKAGE/NAMESPACE/TYPENAME format.
   type type_description_interfaces_u_msg_u_TypeSource is record
      type_name : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:35
      encoding : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:42
      raw_file_contents : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:45
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:32

  --/ The type of the original source file, typically matching the file extension.
  --/ Well-known encodings: "idl", "msg", "srv", "action", "dynamic", "implicit".
  --/ "dynamic" specifies a type created programmatically by a user, thus having no source.
  --/ "implicit" specifies a type created automatically as a subtype of a
  --/ complex type (service or action) - such as the request message for a service.
  --/ Implicit types will have no contents, the full source will be available on the parent srv/action.
  --/ Dumped contents of the interface definition source file.
  --/ If `encoding` is "dynamic" or "implicit", this field will be empty.
  -- Struct for a sequence of type_description_interfaces__msg__TypeSource.
   type type_description_interfaces_u_msg_u_TypeSource_u_Sequence is record
      data : access type_description_interfaces_u_msg_u_TypeSource;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:51
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:53
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:55
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/msg/detail/type_source__struct.h:49

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_msg_detail_type_source_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
