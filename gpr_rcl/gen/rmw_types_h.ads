pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
limited with rmw_init_h;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Extensions;

package rmw_types_h is

   RMW_GID_STORAGE_SIZE : constant := 24;  --  /opt/ros/crystal/include/rmw/types.h:37

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
  -- 24 bytes is the most memory needed to represent the GID by any current
  -- implementation. It may need to be increased in the future.
   type rmw_node_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:41
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:42
      name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:43
      namespace_u : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:44
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/crystal/include/rmw/types.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_t);  -- /opt/ros/crystal/include/rmw/types.h:39

   type rmw_publisher_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:50
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:51
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:52
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_publisher_t);  -- /opt/ros/crystal/include/rmw/types.h:48

   type rmw_subscription_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:57
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:58
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:59
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscription_t);  -- /opt/ros/crystal/include/rmw/types.h:55

   type rmw_service_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:64
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:65
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:66
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_service_t);  -- /opt/ros/crystal/include/rmw/types.h:62

   type rmw_client_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:71
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:72
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_client_t);  -- /opt/ros/crystal/include/rmw/types.h:69

   type rmw_guard_condition_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:78
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:79
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/crystal/include/rmw/types.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_condition_t);  -- /opt/ros/crystal/include/rmw/types.h:76

  --/ Array of subscriber handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific subscriptions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of subscriptions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of subscribers represented by the array.
   type rmw_subscriptions_t is record
      subscriber_count : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rmw/types.h:93
      subscribers : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:95
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscriptions_t);  -- /opt/ros/crystal/include/rmw/types.h:90

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
      service_count : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rmw/types.h:108
      services : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:110
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_services_t);  -- /opt/ros/crystal/include/rmw/types.h:105

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
      client_count : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rmw/types.h:123
      clients : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:125
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_clients_t);  -- /opt/ros/crystal/include/rmw/types.h:120

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
      guard_condition_count : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rmw/types.h:138
      guard_conditions : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:140
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_conditions_t);  -- /opt/ros/crystal/include/rmw/types.h:135

  --/ Pointer to an array of void * pointers of guard conditions.
   type rmw_wait_set_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:145
      guard_conditions : access rmw_guard_conditions_t;  -- /opt/ros/crystal/include/rmw/types.h:146
      data : System.Address;  -- /opt/ros/crystal/include/rmw/types.h:147
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_wait_set_t);  -- /opt/ros/crystal/include/rmw/types.h:143

   type rmw_request_id_t_writer_guid_array is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
   type rmw_request_id_t is record
      writer_guid : aliased rmw_request_id_t_writer_guid_array;  -- /opt/ros/crystal/include/rmw/types.h:152
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /opt/ros/crystal/include/rmw/types.h:153
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_request_id_t);  -- /opt/ros/crystal/include/rmw/types.h:150

   type rmw_time_t is record
      sec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/crystal/include/rmw/types.h:158
      nsec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/crystal/include/rmw/types.h:159
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_time_t);  -- /opt/ros/crystal/include/rmw/types.h:156

   type rmw_security_enforcement_policy_t is 
     (RMW_SECURITY_ENFORCEMENT_PERMISSIVE,
      RMW_SECURITY_ENFORCEMENT_ENFORCE);
   pragma Convention (C, rmw_security_enforcement_policy_t);  -- /opt/ros/crystal/include/rmw/types.h:162

   type rmw_node_security_options_t is record
      enforce_security : aliased rmw_security_enforcement_policy_t;  -- /opt/ros/crystal/include/rmw/types.h:170
      security_root_path : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:171
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_security_options_t);  -- /opt/ros/crystal/include/rmw/types.h:168

   type rmw_qos_reliability_policy_t is 
     (RMW_QOS_POLICY_RELIABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_RELIABILITY_RELIABLE,
      RMW_QOS_POLICY_RELIABILITY_BEST_EFFORT);
   pragma Convention (C, rmw_qos_reliability_policy_t);  -- /opt/ros/crystal/include/rmw/types.h:174

   type rmw_qos_history_policy_t is 
     (RMW_QOS_POLICY_HISTORY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_HISTORY_KEEP_LAST,
      RMW_QOS_POLICY_HISTORY_KEEP_ALL);
   pragma Convention (C, rmw_qos_history_policy_t);  -- /opt/ros/crystal/include/rmw/types.h:181

   type rmw_qos_durability_policy_t is 
     (RMW_QOS_POLICY_DURABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_DURABILITY_TRANSIENT_LOCAL,
      RMW_QOS_POLICY_DURABILITY_VOLATILE);
   pragma Convention (C, rmw_qos_durability_policy_t);  -- /opt/ros/crystal/include/rmw/types.h:188

   type rmw_qos_profile_t is record
      history : aliased rmw_qos_history_policy_t;  -- /opt/ros/crystal/include/rmw/types.h:197
      depth : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rmw/types.h:198
      reliability : aliased rmw_qos_reliability_policy_t;  -- /opt/ros/crystal/include/rmw/types.h:199
      durability : aliased rmw_qos_durability_policy_t;  -- /opt/ros/crystal/include/rmw/types.h:200
      avoid_ros_namespace_conventions : aliased Extensions.bool;  -- /opt/ros/crystal/include/rmw/types.h:211
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_qos_profile_t);  -- /opt/ros/crystal/include/rmw/types.h:195

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
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/types.h:216
      data : aliased rmw_gid_t_data_array;  -- /opt/ros/crystal/include/rmw/types.h:217
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_gid_t);  -- /opt/ros/crystal/include/rmw/types.h:214

  -- const rmw_time_t received_timestamp;
   type rmw_message_info_t is record
      publisher_gid : aliased rmw_gid_t;  -- /opt/ros/crystal/include/rmw/types.h:223
      from_intra_process : aliased Extensions.bool;  -- /opt/ros/crystal/include/rmw/types.h:224
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_message_info_t);  -- /opt/ros/crystal/include/rmw/types.h:220

  -- Type mapping of rcutils log severity types to
  -- rmw specific types.
   subtype RWM_PUBLIC_TYPE is unsigned;
   RMW_LOG_SEVERITY_DEBUG : constant RWM_PUBLIC_TYPE := 10;
   RMW_LOG_SEVERITY_INFO : constant RWM_PUBLIC_TYPE := 20;
   RMW_LOG_SEVERITY_WARN : constant RWM_PUBLIC_TYPE := 30;
   RMW_LOG_SEVERITY_ERROR : constant RWM_PUBLIC_TYPE := 40;
   RMW_LOG_SEVERITY_FATAL : constant RWM_PUBLIC_TYPE := 50;  -- /opt/ros/crystal/include/rmw/types.h:231

   subtype rmw_log_severity_t is RWM_PUBLIC_TYPE;  -- /opt/ros/crystal/include/rmw/types.h:238

end rmw_types_h;
