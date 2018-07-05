pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Extensions;
with rcutils_allocator_h;

package rmw_types_h is

   RMW_RET_OK : constant := 0;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:33
   RMW_RET_ERROR : constant := 1;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:34
   RMW_RET_TIMEOUT : constant := 2;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:35

   RMW_RET_BAD_ALLOC : constant := 10;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:38

   RMW_RET_INVALID_ARGUMENT : constant := 11;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:40

   RMW_GID_STORAGE_SIZE : constant := 24;  --  /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:44

  -- Copyright 2014-2017 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  -- map rcutils specific log levels to rmw speicfic type
   subtype rmw_ret_t is int;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:32

  --/ Failed to allocate memory return code.
  --/ Invalid argument return code.
  -- 24 bytes is the most memory needed to represent the GID by any current
  -- implementation. It may need to be increased in the future.
   type rmw_node_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:48
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:49
      name : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:50
      namespace_u : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:51
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:46

   type rmw_publisher_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:56
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:57
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:58
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_publisher_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:54

   type rmw_subscription_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:63
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:64
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:65
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscription_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:61

   type rmw_service_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:70
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:71
      service_name : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:72
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_service_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:68

   type rmw_client_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:77
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:78
      service_name : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:79
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_client_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:75

   type rmw_guard_condition_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:84
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:85
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_condition_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:82

  --/ Array of subsciber handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific subscriptions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of subscriptions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of subscribers represented by the array.
   type rmw_subscriptions_t is record
      subscriber_count : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:98
      subscribers : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:100
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscriptions_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:95

  --/ Pointer to an array of void * pointers of subscriptions.
  --/ Array of service handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific services.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of services represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of services represented by the array.
   type rmw_services_t is record
      service_count : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:113
      services : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:115
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_services_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:110

  --/ Pointer to an array of void * pointers of services.
  --/ Array of client handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific clients.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of clients represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of clients represented by the array.
   type rmw_clients_t is record
      client_count : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:128
      clients : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:130
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_clients_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:125

  --/ Pointer to an array of void * pointers of clients.
  --/ Array of guard condition handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific guard conditions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of guard conditions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of guard conditions represented by the array.
   type rmw_guard_conditions_t is record
      guard_condition_count : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:143
      guard_conditions : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_conditions_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:140

  --/ Pointer to an array of void * pointers of guard conditions.
   type rmw_wait_set_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:150
      guard_conditions : access rmw_guard_conditions_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:151
      data : System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:152
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_wait_set_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:148

   type rmw_request_id_t_writer_guid_array is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
   type rmw_request_id_t is record
      writer_guid : aliased rmw_request_id_t_writer_guid_array;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:157
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:158
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_request_id_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:155

   type rmw_time_t is record
      sec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:163
      nsec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:164
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_time_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:161

   type rmw_security_enforcement_policy_t is 
     (RMW_SECURITY_ENFORCEMENT_PERMISSIVE,
      RMW_SECURITY_ENFORCEMENT_ENFORCE);
   pragma Convention (C, rmw_security_enforcement_policy_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:167

   type rmw_node_security_options_t is record
      enforce_security : aliased rmw_security_enforcement_policy_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:175
      security_root_path : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:176
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_security_options_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:173

   type rmw_qos_reliability_policy_t is 
     (RMW_QOS_POLICY_RELIABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_RELIABILITY_RELIABLE,
      RMW_QOS_POLICY_RELIABILITY_BEST_EFFORT);
   pragma Convention (C, rmw_qos_reliability_policy_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:179

   type rmw_qos_history_policy_t is 
     (RMW_QOS_POLICY_HISTORY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_HISTORY_KEEP_LAST,
      RMW_QOS_POLICY_HISTORY_KEEP_ALL);
   pragma Convention (C, rmw_qos_history_policy_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:186

   type rmw_qos_durability_policy_t is 
     (RMW_QOS_POLICY_DURABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_DURABILITY_TRANSIENT_LOCAL,
      RMW_QOS_POLICY_DURABILITY_VOLATILE);
   pragma Convention (C, rmw_qos_durability_policy_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:193

   type rmw_qos_profile_t is record
      history : aliased rmw_qos_history_policy_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:202
      depth : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:203
      reliability : aliased rmw_qos_reliability_policy_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:204
      durability : aliased rmw_qos_durability_policy_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:205
      avoid_ros_namespace_conventions : aliased Extensions.bool;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:216
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_qos_profile_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:200

  --/ If true, any ROS specific namespacing conventions will be circumvented.
  --*
  --   * In the case of DDS and topics, for example, this means the typical
  --   * ROS specific prefix of `rt` would not be applied as described here:
  --   *
  --   *   http://design.ros2.org/articles/topic_and_service_names.html#ros-specific-namespace-prefix
  --   *
  --   * This might be useful when trying to directly connect a native DDS topic
  --   * with a ROS 2 topic.
  --    

   type rmw_gid_t_data_array is array (0 .. 23) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rmw_gid_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:221
      data : aliased rmw_gid_t_data_array;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:222
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_gid_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:219

  -- serialized message data
   type rmw_serialized_message_t is record
      buffer : Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:228
      buffer_length : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:229
      buffer_capacity : aliased stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:230
      allocator : aliased rcutils_allocator_h.rcutils_allocator_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:231
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_serialized_message_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:225

  -- const rmw_time_t received_timestamp;
   type rmw_message_info_t is record
      publisher_gid : aliased rmw_gid_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:237
      from_intra_process : aliased Extensions.bool;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:238
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_message_info_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:234

  -- Type mapping of rcutil log severity types to
  -- rmw specific types.
   subtype RWM_PUBLIC_TYPE is unsigned;
   RMW_LOG_SEVERITY_DEBUG : constant RWM_PUBLIC_TYPE := 10;
   RMW_LOG_SEVERITY_INFO : constant RWM_PUBLIC_TYPE := 20;
   RMW_LOG_SEVERITY_WARN : constant RWM_PUBLIC_TYPE := 30;
   RMW_LOG_SEVERITY_ERROR : constant RWM_PUBLIC_TYPE := 40;
   RMW_LOG_SEVERITY_FATAL : constant RWM_PUBLIC_TYPE := 50;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:245

   subtype rmw_log_severity_t is RWM_PUBLIC_TYPE;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/types.h:252

end rmw_types_h;
