pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
limited with rmw_init_h;
with Interfaces.C.Extensions;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with rcutils_time_h;

package rmw_types_h is

   RMW_GID_STORAGE_SIZE : constant := 24;  --  /opt/ros/foxy/include/rmw/types.h:39

   RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE_DEPRECATED_MSG : aliased constant String := "RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE is deprecated. " & "Use RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC if manually asserted liveliness is needed." & ASCII.NUL;  --  /opt/ros/foxy/include/rmw/types.h:387
   --  arg-macro: procedure RMW_DECLARE_DEPRECATED (name, msg)
   --    name __attribute__((deprecated(msg)))
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
  --/ Structure which encapsulates an rmw node
  --/ Name of the rmw implementation
   type rmw_node_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:45
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:48
      name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:51
      namespace_u : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:54
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/foxy/include/rmw/types.h:57
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:42

  --/ Type erased pointer to this node's data
  --/ A concise name of this rmw node for identification
  --/ The namespace of this rmw node
  --/ Context information about node's init specific information
  --/ Endpoint enumeration type
   type rmw_endpoint_type_t is 
     (RMW_ENDPOINT_INVALID,
      RMW_ENDPOINT_PUBLISHER,
      RMW_ENDPOINT_SUBSCRIPTION)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/types.h:61

  --/ Endpoint type has not yet been set
  --/ Creates and publishes messages to the ROS topic
  --/ Listens for and receives messages from a topic
  --/ Options that can be used to configure the creation of a publisher in rmw.
  --/ Used to pass rmw implementation specific resources during publisher creation.
  --*
  --   * This field is type erased (rather than forward declared) because it will
  --   * usually be a non-owned reference to an language specific object, e.g.
  --   * C++ it may be a polymorphic class that only the rmw implementation can use.
  --   *
  --   * The resource pointed to here needs to outlive this options structure, and
  --   * any rmw_publisher objects that are created using it, as they copy this
  --   * structure and may use this payload throughout their lifetime.
  --    

   type rmw_publisher_options_t is record
      rmw_specific_publisher_payload : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:86
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:74

  --/ Structure which encapsulates an rmw publisher
  --/ Name of the rmw implementation
   type rmw_publisher_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:93
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:96
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:99
      options : aliased rmw_publisher_options_t;  -- /opt/ros/foxy/include/rmw/types.h:110
      can_loan_messages : aliased Extensions.bool;  -- /opt/ros/foxy/include/rmw/types.h:113
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:90

  --/ Type erased pointer to this publisher's data
  --/ The name of the ROS topic this publisher publishes to
  --/ Publisher options.
  --*
  --   * The options structure passed to rmw_create_publisher() should be
  --   * assigned to this field by the rmw implementation.
  --   * The fields should not be modified after creation, but
  --   * the contents of the options structure may or may not be const, i.e.
  --   * shallow const-ness.
  --   * This field is not marked const to avoid any const casting during setup.
  --    

  --/ Indicate whether this publisher supports loaning messages
  --/ Options that can be used to configure the creation of a subscription in rmw.
  --/ Used to pass rmw implementation specific resources during subscription creation.
  --*
  --   * All the same details and restrictions of this field in
  --   * rmw_publisher_options_t apply to this struct as well.
  --   *
  --   * \sa rmw_publisher_options_t.rmw_specific_publisher_payload
  --    

   type rmw_subscription_options_t is record
      rmw_specific_subscription_payload : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:126
      ignore_local_publications : aliased Extensions.bool;  -- /opt/ros/foxy/include/rmw/types.h:141
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:117

  --/ If true then the middleware should not deliver data from local publishers.
  --*
  --   * This setting is most often used when data should only be received from
  --   * remote nodes, especially to avoid "double delivery" when both intra- and
  --   * inter- process communication is taking place.
  --   *
  --   * \TODO(wjwwood): nail this down when participant mapping is sorted out.
  --   *   See: https://github.com/ros2/design/pull/250
  --   *
  --   * The definition of local is somewhat vague at the moment.
  --   * Right now it means local to the node, and that definition works best, but
  --   * may become more complicated when/if participants map to a context instead.
  --    

  --/ Name of the rmw implementation
   type rmw_subscription_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:147
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:150
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:153
      options : aliased rmw_subscription_options_t;  -- /opt/ros/foxy/include/rmw/types.h:164
      can_loan_messages : aliased Extensions.bool;  -- /opt/ros/foxy/include/rmw/types.h:167
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:144

  --/ Type erased pointer to this subscription
  --/ Name of the ros topic this subscription listens to
  --/ Subscription options.
  --*
  --   * The options structure passed to rmw_create_subscription() should be
  --   * assigned to this field by the rmw implementation.
  --   * The fields should not be modified after creation, but
  --   * the contents of the options structure may or may not be const, i.e.
  --   * shallow const-ness.
  --   * This field is not marked const to avoid any const casting during setup.
  --    

  --/ Indicates whether this subscription can loan messages
  --/ A handle to an rmw service
  --/ The name of the rmw implementation
   type rmw_service_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:174
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:177
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:180
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:171

  --/ Type erased pointer to this service
  --/ The name of this service as exposed to the ros graph
  --/ A handle to an rmw service client
  --/ The name of the rmw implementation
   type rmw_client_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:187
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:190
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:193
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:184

  --/ Type erased pointer to this service client
  --/ The name of this service as exposed to the ros graph
  --/ Handle for an rmw guard condition
  --/ The name of the rmw implementation
   type rmw_guard_condition_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:200
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:203
      context : access rmw_init_h.rmw_context_t;  -- /opt/ros/foxy/include/rmw/types.h:206
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:197

  --/ Type erased pointer to this guard condition
  --/ rmw context associated with this guard condition
  --/ Allocation of memory for an rmw publisher
  --/ The name of the rmw implementation
   type rmw_publisher_allocation_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:213
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:216
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:210

  --/ Type erased pointer to this allocation
  --/ Allocation of memory for an rmw subscription
  --/ The name of the rmw implementation
   type rmw_subscription_allocation_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:223
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:226
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:220

  --/ Type erased pointer to this allocation
  --/ Array of subscriber handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific subscriptions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of subscriptions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of subscribers represented by the array.
   type rmw_subscriptions_t is record
      subscriber_count : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:239
      subscribers : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:241
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:236

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
      service_count : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:254
      services : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:256
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:251

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
      client_count : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:269
      clients : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:271
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:266

  --/ Pointer to an array of void * pointers of clients.
  --/ The number of events represented by the array.
   type rmw_events_t is record
      event_count : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:277
      events : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:279
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:274

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
      guard_condition_count : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:292
      guard_conditions : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:294
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:289

  --/ Pointer to an array of void * pointers of guard conditions.
  --/ Container for guard conditions to be waited on
  --/ The name of the rmw implementation
   type rmw_wait_set_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:301
      guard_conditions : access rmw_guard_conditions_t;  -- /opt/ros/foxy/include/rmw/types.h:304
      data : System.Address;  -- /opt/ros/foxy/include/rmw/types.h:307
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:298

  --/ The guard condition to be waited on
  --/ Type erased pointer to this wait set's data
  --/ An rmw service request identifier
  --/ The guid of the writer associated with this request
   type rmw_request_id_t_writer_guid_array is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
   type rmw_request_id_t is record
      writer_guid : aliased rmw_request_id_t_writer_guid_array;  -- /opt/ros/foxy/include/rmw/types.h:314
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /opt/ros/foxy/include/rmw/types.h:317
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:311

  --/ Sequence number of this service
  --/ Struct representing a time point for rmw
  --/ Seconds since the epoch
   type rmw_time_t is record
      sec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/foxy/include/rmw/types.h:324
      nsec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/foxy/include/rmw/types.h:327
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:321

  --/ Nanoseconds component of this time point
   subtype rmw_time_point_value_t is rcutils_time_h.rcutils_time_point_value_t;  -- /opt/ros/foxy/include/rmw/types.h:330

  --/ Meta-data for a service-related take.
   type rmw_service_info_t is record
      source_timestamp : aliased rmw_time_point_value_t;  -- /opt/ros/foxy/include/rmw/types.h:335
      received_timestamp : aliased rmw_time_point_value_t;  -- /opt/ros/foxy/include/rmw/types.h:336
      request_id : aliased rmw_request_id_t;  -- /opt/ros/foxy/include/rmw/types.h:337
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:333

   type rmw_qos_reliability_policy_t is 
     (RMW_QOS_POLICY_RELIABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_RELIABILITY_RELIABLE,
      RMW_QOS_POLICY_RELIABILITY_BEST_EFFORT,
      RMW_QOS_POLICY_RELIABILITY_UNKNOWN)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/types.h:340

  --/ Implementation specific default
  --/ Guarantee that samples are delivered, may retry multiple times.
  --/ Attempt to deliver samples, but some may be lost if the network is not robust
  --/ Reliability policy has not yet been set
  --/ QoS history enumerations describing how samples endure
   type rmw_qos_history_policy_t is 
     (RMW_QOS_POLICY_HISTORY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_HISTORY_KEEP_LAST,
      RMW_QOS_POLICY_HISTORY_KEEP_ALL,
      RMW_QOS_POLICY_HISTORY_UNKNOWN)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/types.h:356

  --/ Implementation default for history policy
  --/ Only store up to a maximum number of samples, dropping oldest once max is exceeded
  --/ Store all samples, subject to resource limits
  --/ History policy has not yet been set
  --/ QoS durability enumerations describing how samples persist
   type rmw_qos_durability_policy_t is 
     (RMW_QOS_POLICY_DURABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_DURABILITY_TRANSIENT_LOCAL,
      RMW_QOS_POLICY_DURABILITY_VOLATILE,
      RMW_QOS_POLICY_DURABILITY_UNKNOWN)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/types.h:372

  --/ Impplementation specific default
  --/ The rmw publisher is responsible for persisting samples for “late-joining” subscribers
  --/ Samples are not persistent
  --/ Durability policy has not yet been set
  --/ QoS liveliness enumerations that describe a publisher's reporting policy for its alive status.
  --/ For a subscriber, these are its requirements for its topic's publishers.
  -- Suppress syntax errors, as cppcheck does not seem to handle enumerator attributes.
  -- cppcheck-suppress syntaxError
   type rmw_qos_liveliness_policy_t is 
     (RMW_QOS_POLICY_LIVELINESS_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_LIVELINESS_AUTOMATIC,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC,
      RMW_QOS_POLICY_LIVELINESS_UNKNOWN)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/types.h:401

  --/ Implementation specific default
  --/ The signal that establishes a Topic is alive comes from the ROS rmw layer.
  --/ Explicitly asserting node liveliness is required in this case.
  --/ This option is deprecated, use RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC if your application
  --/ requires to assert liveliness manually.
  --/ The signal that establishes a Topic is alive is at the Topic level. Only publishing a message
  --/ on the Topic or an explicit signal from the application to assert liveliness on the Topic
  --/ will mark the Topic as being alive.
  -- Using `3` for backwards compatibility.
  --/ Liveliness policy has not yet been set
  --/ QoS Deadline default, 0s indicates deadline policies are not tracked or enforced
  --/ QoS Lifespan default, 0s indicate lifespan policies are not tracked or enforced
  --/ QoS Liveliness lease duration default, 0s indicate lease durations are not tracked or enforced
  --/ ROS MiddleWare quality of service profile.
   type rmw_qos_profile_t is record
      history : aliased rmw_qos_history_policy_t;  -- /opt/ros/foxy/include/rmw/types.h:438
      depth : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/types.h:440
      reliability : aliased rmw_qos_reliability_policy_t;  -- /opt/ros/foxy/include/rmw/types.h:442
      durability : aliased rmw_qos_durability_policy_t;  -- /opt/ros/foxy/include/rmw/types.h:444
      deadline : aliased rmw_time_t;  -- /opt/ros/foxy/include/rmw/types.h:446
      lifespan : aliased rmw_time_t;  -- /opt/ros/foxy/include/rmw/types.h:448
      liveliness : aliased rmw_qos_liveliness_policy_t;  -- /opt/ros/foxy/include/rmw/types.h:450
      liveliness_lease_duration : aliased rmw_time_t;  -- /opt/ros/foxy/include/rmw/types.h:452
      avoid_ros_namespace_conventions : aliased Extensions.bool;  -- /opt/ros/foxy/include/rmw/types.h:464
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:436

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

  --/ ROS graph ID of the topic
  --/ Name of the rmw implementation
   type rmw_gid_t_data_array is array (0 .. 23) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rmw_gid_t is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/types.h:471
      data : aliased rmw_gid_t_data_array;  -- /opt/ros/foxy/include/rmw/types.h:474
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:468

  --/ Bype data Gid value
  --/ Information describing an rmw message
   type rmw_message_info_t is record
      source_timestamp : aliased rmw_time_point_value_t;  -- /opt/ros/foxy/include/rmw/types.h:480
      received_timestamp : aliased rmw_time_point_value_t;  -- /opt/ros/foxy/include/rmw/types.h:481
      publisher_gid : aliased rmw_gid_t;  -- /opt/ros/foxy/include/rmw/types.h:482
      from_intra_process : aliased Extensions.bool;  -- /opt/ros/foxy/include/rmw/types.h:485
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:478

  --/ Whether this message is from intra_process communication or not
  --/ Get zero initialized mesage info.
   function rmw_get_zero_initialized_message_info return rmw_message_info_t  -- /opt/ros/foxy/include/rmw/types.h:492
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_message_info";

  --/ Default size of the rmw queue when history is set to RMW_QOS_POLICY_HISTORY_KEEP_LAST,
  --/ 0 indicates it is currently not set
  --/ Type mapping of rcutils log severity types to rmw specific types.
  --/ Debug log severity, for pedantic messaging
  --/ Informational log severity, for reporting expected but not overwhelming information
  --/ Warning log severity, for reporting recoverable issues
  --/ Error log severity, for reporting uncoverable issues
  --/ Fatal log severity, for reporting issue causing imminent shutdown
   subtype rmw_log_severity_t is unsigned;
   RMW_LOG_SEVERITY_DEBUG : constant unsigned := 10;
   RMW_LOG_SEVERITY_INFO : constant unsigned := 20;
   RMW_LOG_SEVERITY_WARN : constant unsigned := 30;
   RMW_LOG_SEVERITY_ERROR : constant unsigned := 40;
   RMW_LOG_SEVERITY_FATAL : constant unsigned := 50;  -- /opt/ros/foxy/include/rmw/types.h:515

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
      alive_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:528
      not_alive_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:537
      alive_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:539
      not_alive_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:541
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:518

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
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:553
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:555
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:545

  --/ The incremental number of deadlines detected since the status was read.
  --/ QoS Liveliness Lost information provided by a publisher.
  --*
  --   * Lifetime cumulative number of times that a previously-alive Publisher became not alive due to
  --   * a failure to actively signal its liveliness within its offered liveliness period.
  --   * This count does not change when an already not alive Publisher simply remains not alive for
  --   * another liveliness period.
  --    

   type rmw_liveliness_lost_status_t is record
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:567
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:569
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:559

  --/ The change in total_count since the last time the status was last read.
  --/ QoS Deadline Missed information provided by a publisher.
  --*
  --   * Lifetime cumulative number of offered deadline periods elapsed during which a Publisher failed
  --   * to provide data.
  --   * Missed deadlines accumulate; that is, each deadline period the total_count will be incremented
  --   * by one.
  --    

   type rmw_offered_deadline_missed_status_t is record
      total_count : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:581
      total_count_change : aliased x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/types.h:583
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/types.h:573

  --/ The change in total_count since the last time the status was last read.
end rmw_types_h;
