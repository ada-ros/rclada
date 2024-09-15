pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with builtin_interfaces_builtin_interfaces_msg_detail_time_ustruct_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with stddef_h;

package service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from service_msgs:msg/ServiceEventInfo.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "service_msgs/msg/service_event_info.h"
  -- Constants defined in the message
  --/ Constant 'REQUEST_SENT'.
  --/ Constant 'REQUEST_RECEIVED'.
  --/ Constant 'RESPONSE_SENT'.
  --/ Constant 'RESPONSE_RECEIVED'.
  -- Include directives for member types
  -- Member 'stamp'
  --/ Struct defined in msg/ServiceEventInfo in the package service_msgs.
  --/ The type of event this message represents
   type anon_array1138 is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type service_msgs_u_msg_u_ServiceEventInfo is record
      event_type : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:54
      stamp : aliased builtin_interfaces_builtin_interfaces_msg_detail_time_ustruct_h.builtin_interfaces_u_msg_u_Time;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:56
      client_gid : aliased anon_array1138;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:62
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:65
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:51

  --/ Timestamp for when the event occurred (sent or received time)
  --/ Unique identifier for the client that sent the service request
  --/ Note, this is only unique for the current session.
  --/ The size here has to match the size of rmw_dds_common/msg/Gid,
  --/ but unfortunately we cannot use that message directly due to a
  --/ circular dependency.
  --/ Sequence number for the request
  --/ Combined with the client ID, this creates a unique ID for the service transaction
  -- Struct for a sequence of service_msgs__msg__ServiceEventInfo.
   type service_msgs_u_msg_u_ServiceEventInfo_u_Sequence is record
      data : access service_msgs_u_msg_u_ServiceEventInfo;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:71
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:73
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:75
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/service_msgs/service_msgs/msg/detail/service_event_info__struct.h:69

  --/ The number of valid items in data
  --/ The number of allocated items in data
end service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
