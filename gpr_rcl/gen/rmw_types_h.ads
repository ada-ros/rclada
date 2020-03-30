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

   RMW_GID_STORAGE_SIZE : constant := 24;  --  /opt/ros/dashing/include/rmw/types.h:37
   --  unsupported macro: RMW_QOS_DEADLINE_DEFAULT {0, 0}
   --  unsupported macro: RMW_QOS_LIFESPAN_DEFAULT {0, 0}
   --  unsupported macro: RMW_QOS_LIVELINESS_LEASE_DURATION_DEFAULT {0, 0}

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
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:41
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:42
      name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:43
      namespace_u : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:44
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/dashing/include/rmw/types.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_t);  -- /opt/ros/dashing/include/rmw/types.h:39

   type rmw_publisher_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:50
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:51
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:52
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_publisher_t);  -- /opt/ros/dashing/include/rmw/types.h:48

   type rmw_subscription_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:57
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:58
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:59
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscription_t);  -- /opt/ros/dashing/include/rmw/types.h:55

   type rmw_service_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:64
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:65
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:66
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_service_t);  -- /opt/ros/dashing/include/rmw/types.h:62

   type rmw_client_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:71
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:72
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:73
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_client_t);  -- /opt/ros/dashing/include/rmw/types.h:69

   type rmw_guard_condition_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:78
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:79
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/dashing/include/rmw/types.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_condition_t);  -- /opt/ros/dashing/include/rmw/types.h:76

   type rmw_publisher_allocation_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:85
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:86
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_publisher_allocation_t);  -- /opt/ros/dashing/include/rmw/types.h:83

   type rmw_subscription_allocation_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:91
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:92
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscription_allocation_t);  -- /opt/ros/dashing/include/rmw/types.h:89

  --/ Array of subscriber handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific subscriptions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of subscriptions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of subscribers represented by the array.
   type rmw_subscriptions_t is record
      subscriber_count : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:105
      subscribers : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:107
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_subscriptions_t);  -- /opt/ros/dashing/include/rmw/types.h:102

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
      service_count : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:120
      services : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:122
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_services_t);  -- /opt/ros/dashing/include/rmw/types.h:117

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
      client_count : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:135
      clients : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:137
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_clients_t);  -- /opt/ros/dashing/include/rmw/types.h:132

  --/ Pointer to an array of void * pointers of clients.
  --/ The number of events represented by the array.
   type rmw_events_t is record
      event_count : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:143
      events : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_events_t);  -- /opt/ros/dashing/include/rmw/types.h:140

  --/ Pointer to an array of void * pointers of events.
  --/ Array of guard condition handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific guard conditions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of guard conditions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of guard conditions represented by the array.
   type rmw_guard_conditions_t is record
      guard_condition_count : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:158
      guard_conditions : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:160
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_guard_conditions_t);  -- /opt/ros/dashing/include/rmw/types.h:155

  --/ Pointer to an array of void * pointers of guard conditions.
   type rmw_wait_set_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:165
      guard_conditions : access rmw_guard_conditions_t;  -- /opt/ros/dashing/include/rmw/types.h:166
      data : System.Address;  -- /opt/ros/dashing/include/rmw/types.h:167
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_wait_set_t);  -- /opt/ros/dashing/include/rmw/types.h:163

   type rmw_request_id_t_writer_guid_array is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
   type rmw_request_id_t is record
      writer_guid : aliased rmw_request_id_t_writer_guid_array;  -- /opt/ros/dashing/include/rmw/types.h:172
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /opt/ros/dashing/include/rmw/types.h:173
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_request_id_t);  -- /opt/ros/dashing/include/rmw/types.h:170

   type rmw_time_t is record
      sec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/dashing/include/rmw/types.h:178
      nsec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/dashing/include/rmw/types.h:179
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_time_t);  -- /opt/ros/dashing/include/rmw/types.h:176

   type rmw_security_enforcement_policy_t is 
     (RMW_SECURITY_ENFORCEMENT_PERMISSIVE,
      RMW_SECURITY_ENFORCEMENT_ENFORCE);
   pragma Convention (C, rmw_security_enforcement_policy_t);  -- /opt/ros/dashing/include/rmw/types.h:182

   type rmw_node_security_options_t is record
      enforce_security : aliased rmw_security_enforcement_policy_t;  -- /opt/ros/dashing/include/rmw/types.h:190
      security_root_path : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:191
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_node_security_options_t);  -- /opt/ros/dashing/include/rmw/types.h:188

   type rmw_qos_reliability_policy_t is 
     (RMW_QOS_POLICY_RELIABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_RELIABILITY_RELIABLE,
      RMW_QOS_POLICY_RELIABILITY_BEST_EFFORT,
      RMW_QOS_POLICY_RELIABILITY_UNKNOWN);
   pragma Convention (C, rmw_qos_reliability_policy_t);  -- /opt/ros/dashing/include/rmw/types.h:194

   type rmw_qos_history_policy_t is 
     (RMW_QOS_POLICY_HISTORY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_HISTORY_KEEP_LAST,
      RMW_QOS_POLICY_HISTORY_KEEP_ALL,
      RMW_QOS_POLICY_HISTORY_UNKNOWN);
   pragma Convention (C, rmw_qos_history_policy_t);  -- /opt/ros/dashing/include/rmw/types.h:202

   type rmw_qos_durability_policy_t is 
     (RMW_QOS_POLICY_DURABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_DURABILITY_TRANSIENT_LOCAL,
      RMW_QOS_POLICY_DURABILITY_VOLATILE,
      RMW_QOS_POLICY_DURABILITY_UNKNOWN);
   pragma Convention (C, rmw_qos_durability_policy_t);  -- /opt/ros/dashing/include/rmw/types.h:210

   type rmw_qos_liveliness_policy_t is 
     (RMW_QOS_POLICY_LIVELINESS_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_LIVELINESS_AUTOMATIC,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC,
      RMW_QOS_POLICY_LIVELINESS_UNKNOWN);
   pragma Convention (C, rmw_qos_liveliness_policy_t);  -- /opt/ros/dashing/include/rmw/types.h:218

  --/ ROS MiddleWare quality of service profile.
   type rmw_qos_profile_t is record
      history : aliased rmw_qos_history_policy_t;  -- /opt/ros/dashing/include/rmw/types.h:234
      depth : aliased stddef_h.size_t;  -- /opt/ros/dashing/include/rmw/types.h:236
      reliability : aliased rmw_qos_reliability_policy_t;  -- /opt/ros/dashing/include/rmw/types.h:238
      durability : aliased rmw_qos_durability_policy_t;  -- /opt/ros/dashing/include/rmw/types.h:240
      deadline : aliased rmw_time_t;  -- /opt/ros/dashing/include/rmw/types.h:242
      lifespan : aliased rmw_time_t;  -- /opt/ros/dashing/include/rmw/types.h:244
      liveliness : aliased rmw_qos_liveliness_policy_t;  -- /opt/ros/dashing/include/rmw/types.h:246
      liveliness_lease_duration : aliased rmw_time_t;  -- /opt/ros/dashing/include/rmw/types.h:248
      avoid_ros_namespace_conventions : aliased Extensions.bool;  -- /opt/ros/dashing/include/rmw/types.h:260
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_qos_profile_t);  -- /opt/ros/dashing/include/rmw/types.h:232

  --/ Size of the message queue.
  --/ Reliabiilty QoS policy setting
  --/ Durability QoS policy setting
  --/ The period at which messages are expected to be sent/received
  --/ The age at which messages are considered expired and no longer valid
  --/ Liveliness QoS policy setting
  --/ The time within which the RMW node or publisher must show that it is alive
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
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rmw/types.h:265
      data : aliased rmw_gid_t_data_array;  -- /opt/ros/dashing/include/rmw/types.h:266
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_gid_t);  -- /opt/ros/dashing/include/rmw/types.h:263

  -- const rmw_time_t received_timestamp;
   type rmw_message_info_t is record
      publisher_gid : aliased rmw_gid_t;  -- /opt/ros/dashing/include/rmw/types.h:272
      from_intra_process : aliased Extensions.bool;  -- /opt/ros/dashing/include/rmw/types.h:273
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_message_info_t);  -- /opt/ros/dashing/include/rmw/types.h:269

  --/ Type mapping of rcutils log severity types to rmw specific types.
   subtype rmw_log_severity_t is unsigned;
   RMW_LOG_SEVERITY_DEBUG : constant rmw_log_severity_t := 10;
   RMW_LOG_SEVERITY_INFO : constant rmw_log_severity_t := 20;
   RMW_LOG_SEVERITY_WARN : constant rmw_log_severity_t := 30;
   RMW_LOG_SEVERITY_ERROR : constant rmw_log_severity_t := 40;
   RMW_LOG_SEVERITY_FATAL : constant rmw_log_severity_t := 50;  -- /opt/ros/dashing/include/rmw/types.h:286

  --/ QoS Liveliness Changed information provided by a subscription.
  --*
  --   * The total number of currently active Publishers which publish to the topic associated with
  --   * the Subscription.
  --   * This count increases when a newly matched Publisher asserts its liveliness for the first time
  --   * or when a Publisher previously considered to be not alive reasserts its liveliness.
  --   * The count decreases when a Publisher considered alive fails to assert its liveliness and
  --   * becomes not alive, whether because it was deleted normally or for some other reason.
  --    

   type rmw_liveliness_changed_status_t is record
      alive_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:299
      not_alive_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:308
      alive_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:310
      not_alive_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:312
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_liveliness_changed_status_t);  -- /opt/ros/dashing/include/rmw/types.h:289

  --*
  --   * The total count of current Publishers which publish to the topic associated with the
  --   * Subscription that are no longer asserting their liveliness.
  --   * This count increases when a Publisher considered alive fails to assert its liveliness and
  --   * becomes not alive for some reason other than the normal deletion of that Publisher.
  --   * It decreases when a previously not alive Publisher either reasserts its liveliness or is
  --   * deleted normally.
  --    

  --/ The change in the alive_count since the status was last read.
  --/ The change in the not_alive_count since the status was last read.
  --/ QoS Requested Deadline Missed information provided by a subscription.
  --*
  --   * Lifetime cumulative number of missed deadlines detected for any instance read by the
  --   * subscription.
  --   * Missed deadlines accumulate; that is, each deadline period the total_count will be incremented
  --   * by one for each instance for which data was not received.
  --    

   type rmw_requested_deadline_missed_status_t is record
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:324
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:326
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_requested_deadline_missed_status_t);  -- /opt/ros/dashing/include/rmw/types.h:316

  --/ The incremental number of deadlines detected since the status was read.
  --/ QoS Liveliness Lost information provided by a publisher.
  --*
  --   * Lifetime cumulative number of times that a previously-alive Publisher became not alive due to
  --   * a failure to actively signal its liveliness within its offered liveliness period.
  --   * This count does not change when an already not alive Publisher simply remains not alive for
  --   * another liveliness period.
  --    

   type rmw_liveliness_lost_status_t is record
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:338
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:340
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_liveliness_lost_status_t);  -- /opt/ros/dashing/include/rmw/types.h:330

  --/ The change in total_count since the last time the status was last read.
  --/ QoS Deadline Missed information provided by a publisher.
  --*
  --   * Lifetime cumulative number of offered deadline periods elapsed during which a Publisher failed
  --   * to provide data.
  --   * Missed deadlines accumulate; that is, each deadline period the total_count will be incremented
  --   * by one.
  --    

   type rmw_offered_deadline_missed_status_t is record
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:352
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/dashing/include/rmw/types.h:354
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_offered_deadline_missed_status_t);  -- /opt/ros/dashing/include/rmw/types.h:344

  --/ The change in total_count since the last time the status was last read.
end rmw_types_h;
