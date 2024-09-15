pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rosidl_runtime_c_rosidl_runtime_c_string_h;
with Interfaces.C.Extensions;
with stddef_h;
with type_description_interfaces_type_description_interfaces_msg_detail_type_description_ustruct_h;
with type_description_interfaces_type_description_interfaces_msg_detail_type_source_ustruct_h;
with type_description_interfaces_type_description_interfaces_msg_detail_key_value_ustruct_h;
with service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h;

package type_description_interfaces_type_description_interfaces_srv_detail_get_type_description_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from type_description_interfaces:srv/GetTypeDescription.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "type_description_interfaces/srv/get_type_description.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'type_name'
  -- Member 'type_hash'
  --/ Struct defined in srv/GetTypeDescription in the package type_description_interfaces.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Request is record
      type_name : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:31
      type_hash : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:33
      include_type_sources : aliased Extensions.bool;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:35
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:29

  --/ REP-2011 RIHS hash string.
  --/ Whether to return the original idl/msg/etc. source file(s) in the response.
  -- Struct for a sequence of type_description_interfaces__srv__GetTypeDescription_Request.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Request_u_Sequence is record
      data : access type_description_interfaces_u_srv_u_GetTypeDescription_Request;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:41
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:43
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:45
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:39

  --/ The number of valid items in data
  --/ The number of allocated items in data
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'failure_reason'
  -- already included above
  -- #include "rosidl_runtime_c/string.h"
  -- Member 'type_description'
  -- Member 'type_sources'
  -- Member 'extra_information'
  --/ Struct defined in srv/GetTypeDescription in the package type_description_interfaces.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Response is record
      successful : aliased Extensions.bool;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:64
      failure_reason : aliased rosidl_runtime_c_rosidl_runtime_c_string_h.rosidl_runtime_c_u_String;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:67
      type_description : aliased type_description_interfaces_type_description_interfaces_msg_detail_type_description_ustruct_h.type_description_interfaces_u_msg_u_TypeDescription;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:69
      type_sources : aliased type_description_interfaces_type_description_interfaces_msg_detail_type_source_ustruct_h.type_description_interfaces_u_msg_u_TypeSource_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:76
      extra_information : aliased type_description_interfaces_type_description_interfaces_msg_detail_key_value_ustruct_h.type_description_interfaces_u_msg_u_KeyValue_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:78
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:62

  --/ If `successful` is false, contains a reason for failure.
  --/ If `successful` is true, this is left empty.
  --/ The parsed type description which can be used programmatically.
  --/ A list containing the interface definition source text of the requested type,
  --/ plus all types it recursively depends on.
  --/ Each source text is a copy of the original contents of the
  --/ .msg, .srv, .action, .idl, or other file if it exists, including comments and whitespace.
  --/ Sources can be matched with IndividualTypeDescriptions by their `type_name`.
  --/ The `encoding` field of each entry informs how to interpret its contents.
  --/ Key-value pairs of extra information.
  -- Struct for a sequence of type_description_interfaces__srv__GetTypeDescription_Response.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Response_u_Sequence is record
      data : access type_description_interfaces_u_srv_u_GetTypeDescription_Response;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:84
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:86
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:88
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:82

  --/ The number of valid items in data
  --/ The number of allocated items in data
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'info'
  -- constants for array fields with an upper bound
  -- request
  -- response
  --/ Struct defined in srv/GetTypeDescription in the package type_description_interfaces.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Event is record
      info : aliased service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h.service_msgs_u_msg_u_ServiceEventInfo;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:112
      request : aliased type_description_interfaces_u_srv_u_GetTypeDescription_Request_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:113
      response : aliased type_description_interfaces_u_srv_u_GetTypeDescription_Response_u_Sequence;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:114
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:110

  -- Struct for a sequence of type_description_interfaces__srv__GetTypeDescription_Event.
   type type_description_interfaces_u_srv_u_GetTypeDescription_Event_u_Sequence is record
      data : access type_description_interfaces_u_srv_u_GetTypeDescription_Event;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:120
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:122
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:124
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/type_description_interfaces/type_description_interfaces/srv/detail/get_type_description__struct.h:118

  --/ The number of valid items in data
  --/ The number of allocated items in data
end type_description_interfaces_type_description_interfaces_srv_detail_get_type_description_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
