pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
limited with rmw_rmw_init_h;
with Interfaces.C.Extensions;
limited with rmw_rmw_subscription_content_filter_options_h;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with rmw_rmw_time_h;

package rmw_rmw_types_h is

   RMW_GID_STORAGE_SIZE : constant := 16;  --  /opt/ros/jazzy/include/rmw/rmw/types.h:44

   RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE_DEPRECATED_MSG : aliased constant String := "RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE is deprecated. " & "Use RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC if manually asserted liveliness is needed." & ASCII.NUL;  --  /opt/ros/jazzy/include/rmw/rmw/types.h:458
   --  arg-macro: procedure RMW_DECLARE_DEPRECATED (name, msg)
   --    name __attribute__((deprecated(msg)))
   --  unsupported macro: RMW_QOS_DEADLINE_DEFAULT RMW_DURATION_UNSPECIFIED
   --  unsupported macro: RMW_QOS_DEADLINE_BEST_AVAILABLE {9223372036LL, 854775806LL}
   --  unsupported macro: RMW_QOS_LIFESPAN_DEFAULT RMW_DURATION_UNSPECIFIED
   --  unsupported macro: RMW_QOS_LIVELINESS_LEASE_DURATION_DEFAULT RMW_DURATION_UNSPECIFIED
   --  unsupported macro: RMW_QOS_LIVELINESS_LEASE_DURATION_BEST_AVAILABLE {9223372036LL, 854775806LL}
   --  unsupported macro: RMW_MESSAGE_INFO_SEQUENCE_NUMBER_UNSUPPORTED UINT64_MAX

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
  -- We define the GID as 16 bytes (128 bits).  This should be enough to ensure
  -- uniqueness amongst all entities in the system.  It is up to the individual
  -- RMW implementations to fill that in, either from the underlying middleware
  -- or from the RMW layer itself.
  --/ Structure which encapsulates an rmw node
  --/ Name of the rmw implementation
   type rmw_node_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:50
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:53
      name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:56
      namespace_u : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:59
      context : access rmw_rmw_init_h.rmw_context_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:62
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:47

  --/ Type erased pointer to this node's data
  --/ A concise name of this rmw node for identification
  --/ The namespace of this rmw node
  --/ Context information about node's init specific information
   subtype rmw_node_t is rmw_node_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:63

  --/ Endpoint enumeration type
   type rmw_endpoint_type_e is 
     (RMW_ENDPOINT_INVALID,
      RMW_ENDPOINT_PUBLISHER,
      RMW_ENDPOINT_SUBSCRIPTION)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:66

  --/ Endpoint type has not yet been set
  --/ Creates and publishes messages to the ROS topic
  --/ Listens for and receives messages from a topic
   subtype rmw_endpoint_type_t is rmw_endpoint_type_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:76

  --/ Unique network flow endpoints requirement enumeration
   type rmw_unique_network_flow_endpoints_requirement_e is 
     (RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_NOT_REQUIRED,
      RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_STRICTLY_REQUIRED,
      RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_OPTIONALLY_REQUIRED,
      RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_SYSTEM_DEFAULT)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:79

  --/ Unique network flow endpoints not required
  --/ Unique network flow endpoins strictly required.
  --/ Error if not provided by RMW implementation.
  --/ Unique network flow endpoints optionally required.
  --/ No error if not provided RMW implementation.
  --/ Unique network flow endpoints requirement decided by system.
   subtype rmw_unique_network_flow_endpoints_requirement_t is rmw_unique_network_flow_endpoints_requirement_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:94

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

   type rmw_publisher_options_s is record
      rmw_specific_publisher_payload : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:109
      require_unique_network_flow_endpoints : aliased rmw_unique_network_flow_endpoints_requirement_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:118
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:97

  --/ Require middleware to generate unique network flow endpoints.
  --*
  --   * Unique network flow endpoints are required to differentiate the QoS provided by
  --   * networks for flows between publishers and subscribers in communicating
  --   * nodes.
  --   * Default value is RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_NOT_REQUIRED.
  --    

   subtype rmw_publisher_options_t is rmw_publisher_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:119

  --/ Structure which encapsulates an rmw publisher
  --/ Name of the rmw implementation
   type rmw_publisher_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:125
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:128
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:131
      options : aliased rmw_publisher_options_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:142
      can_loan_messages : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:145
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:122

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
   subtype rmw_publisher_t is rmw_publisher_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:146

  --/ Options that can be used to configure the creation of a subscription in rmw.
  --/ Used to pass rmw implementation specific resources during subscription creation.
  --*
  --   * All the same details and restrictions of this field in
  --   * rmw_publisher_options_t apply to this struct as well.
  --   *
  --   * \sa rmw_publisher_options_t.rmw_specific_publisher_payload
  --    

   type rmw_subscription_options_s is record
      rmw_specific_subscription_payload : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:158
      ignore_local_publications : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:168
      require_unique_network_flow_endpoints : aliased rmw_unique_network_flow_endpoints_requirement_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:177
      content_filter_options : access rmw_rmw_subscription_content_filter_options_h.rmw_subscription_content_filter_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:180
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:149

  --/ If true then the middleware should not deliver data from local publishers.
  --*
  --   * This setting is most often used when data should only be received from
  --   * remote nodes, especially to avoid "double delivery" when both intra- and
  --   * inter- process communication is taking place.
  --   *
  --   * The definition of local means that in the same context.
  --    

  --/ Require middleware to generate unique network flow endpoints.
  --*
  --   * Unique network flow endpoints are required to differentiate the QoS provided by
  --   * networks for flows between publishers and subscribers in communicating
  --   * nodes.
  --   * Default value is RMW_UNIQUE_NETWORK_FLOW_ENDPOINTS_NOT_REQUIRED.
  --    

  --/ Used to create a content filter options during subscription creation.
   subtype rmw_subscription_options_t is rmw_subscription_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:181

  --/ Name of the rmw implementation
   type rmw_subscription_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:186
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:189
      topic_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:192
      options : aliased rmw_subscription_options_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:203
      can_loan_messages : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:206
      is_cft_enabled : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:209
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:183

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
  --/ Indicates whether content filtered topic of this subscription is enabled
   subtype rmw_subscription_t is rmw_subscription_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:210

  --/ A handle to an rmw service
  --/ The name of the rmw implementation
   type rmw_service_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:216
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:219
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:222
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:213

  --/ Type erased pointer to this service
  --/ The name of this service as exposed to the ros graph
   subtype rmw_service_t is rmw_service_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:223

  --/ A handle to an rmw service client
  --/ The name of the rmw implementation
   type rmw_client_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:229
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:232
      service_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:235
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:226

  --/ Type erased pointer to this service client
  --/ The name of this service as exposed to the ros graph
   subtype rmw_client_t is rmw_client_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:236

  --/ Handle for an rmw guard condition
  --/ The name of the rmw implementation
   type rmw_guard_condition_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:242
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:245
      context : access rmw_rmw_init_h.rmw_context_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:248
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:239

  --/ Type erased pointer to this guard condition
  --/ rmw context associated with this guard condition
   subtype rmw_guard_condition_t is rmw_guard_condition_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:249

  --/ Allocation of memory for an rmw publisher
  --/ The name of the rmw implementation
   type rmw_publisher_allocation_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:255
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:258
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:252

  --/ Type erased pointer to this allocation
   subtype rmw_publisher_allocation_t is rmw_publisher_allocation_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:259

  --/ Allocation of memory for an rmw subscription
  --/ The name of the rmw implementation
   type rmw_subscription_allocation_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:265
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:268
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:262

  --/ Type erased pointer to this allocation
   subtype rmw_subscription_allocation_t is rmw_subscription_allocation_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:269

  --/ Array of subscriber handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific subscriptions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of subscriptions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of subscribers represented by the array.
   type rmw_subscriptions_s is record
      subscriber_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:281
      subscribers : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:283
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:278

  --/ Pointer to an array of void * pointers of subscriptions.
   subtype rmw_subscriptions_t is rmw_subscriptions_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:284

  --/ Array of service handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific services.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of services represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of services represented by the array.
   type rmw_services_s is record
      service_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:296
      services : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:298
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:293

  --/ Pointer to an array of void * pointers of services.
   subtype rmw_services_t is rmw_services_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:299

  --/ Array of client handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific clients.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of clients represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of clients represented by the array.
   type rmw_clients_s is record
      client_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:311
      clients : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:313
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:308

  --/ Pointer to an array of void * pointers of clients.
   subtype rmw_clients_t is rmw_clients_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:314

  --/ The number of events represented by the array.
   type rmw_events_s is record
      event_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:319
      events : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:321
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:316

  --/ Pointer to an array of void * pointers of events.
   subtype rmw_events_t is rmw_events_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:322

  --/ Array of guard condition handles.
  --*
  -- * An array of void * pointers representing type-erased middleware-specific guard conditions.
  -- * The number of non-null entries may be smaller than the allocated size of the array.
  -- * The number of guard conditions represented may be smaller than the allocated size of the array.
  -- * The creator of this struct is responsible for allocating and deallocating the array.
  --  

  --/ The number of guard conditions represented by the array.
   type rmw_guard_conditions_s is record
      guard_condition_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:334
      guard_conditions : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:336
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:331

  --/ Pointer to an array of void * pointers of guard conditions.
   subtype rmw_guard_conditions_t is rmw_guard_conditions_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:337

  --/ Container for guard conditions to be waited on
  --/ The name of the rmw implementation
   type rmw_wait_set_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:343
      guard_conditions : access rmw_guard_conditions_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:346
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:349
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:340

  --/ The guard condition to be waited on
  --/ Type erased pointer to this wait set's data
   subtype rmw_wait_set_t is rmw_wait_set_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:350

  --/ An rmw service request identifier
  --/ The guid of the writer associated with this request
   type anon_array2848 is array (0 .. 15) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rmw_request_id_s is record
      writer_guid : aliased anon_array2848;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:356
      sequence_number : aliased x86_64_linux_gnu_bits_stdint_intn_h.int64_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:359
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:353

  --/ Sequence number of this service
   subtype rmw_request_id_t is rmw_request_id_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:360

  --/ Meta-data for a service-related take.
   type rmw_service_info_s is record
      source_timestamp : aliased rmw_rmw_time_h.rmw_time_point_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:365
      received_timestamp : aliased rmw_rmw_time_h.rmw_time_point_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:366
      request_id : aliased rmw_request_id_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:367
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:363

   subtype rmw_service_info_t is rmw_service_info_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:368

   type rmw_qos_reliability_policy_e is 
     (RMW_QOS_POLICY_RELIABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_RELIABILITY_RELIABLE,
      RMW_QOS_POLICY_RELIABILITY_BEST_EFFORT,
      RMW_QOS_POLICY_RELIABILITY_UNKNOWN,
      RMW_QOS_POLICY_RELIABILITY_BEST_AVAILABLE)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:370

  --/ Implementation specific default
  --/ Guarantee that samples are delivered, may retry multiple times.
  --/ Attempt to deliver samples, but some may be lost if the network is not robust
  --/ Reliability policy has not yet been set
  --/ Will match the majority of endpoints and use a reliable policy if possible
  --*
  --   * A policy will be chosen at the time of creating a subscription or publisher.
  --   * A reliable policy will by chosen if it matches with all discovered endpoints,
  --   * otherwise a best effort policy will be chosen.
  --   *
  --   * The QoS policy reported by functions like `rmw_subscription_get_actual_qos` or
  --   * `rmw_publisher_get_actual_qos` may be best available, reliable, or best effort.
  --   *
  --   * Services and clients are not supported and default to the reliability value in
  --   * `rmw_qos_profile_services_default`.
  --   *
  --   * The middleware is not expected to update the policy after creating a subscription or
  --   * publisher, even if the chosen policy is incompatible with newly discovered endpoints.
  --   * Therefore, this policy should be used with care since non-deterministic behavior
  --   * can occur due to races with discovery.
  --    

   subtype rmw_qos_reliability_policy_t is rmw_qos_reliability_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:402

  --/ QoS history enumerations describing how samples endure
   type rmw_qos_history_policy_e is 
     (RMW_QOS_POLICY_HISTORY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_HISTORY_KEEP_LAST,
      RMW_QOS_POLICY_HISTORY_KEEP_ALL,
      RMW_QOS_POLICY_HISTORY_UNKNOWN)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:405

  --/ Implementation default for history policy
  --/ Only store up to a maximum number of samples, dropping oldest once max is exceeded
  --/ Store all samples, subject to resource limits
  --/ History policy has not yet been set
   subtype rmw_qos_history_policy_t is rmw_qos_history_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:418

  --/ QoS durability enumerations describing how samples persist
   type rmw_qos_durability_policy_e is 
     (RMW_QOS_POLICY_DURABILITY_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_DURABILITY_TRANSIENT_LOCAL,
      RMW_QOS_POLICY_DURABILITY_VOLATILE,
      RMW_QOS_POLICY_DURABILITY_UNKNOWN,
      RMW_QOS_POLICY_DURABILITY_BEST_AVAILABLE)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:421

  --/ Impplementation specific default
  --/ The rmw publisher is responsible for persisting samples for “late-joining” subscribers
  --/ Samples are not persistent
  --/ Durability policy has not yet been set
  --/ Will match the majority of endpoints and use a transient local policy if possible
  --*
  --   * A policy will be chosen at the time of creating a subscription or publisher.
  --   * A transient local policy will by chosen if it matches with all discovered endpoints,
  --   * otherwise a volatile policy will be chosen.
  --   *
  --   * In the case that a volatile policy is chosen for a subscription, any messages sent before
  --   * the subscription was created by transient local publishers will not be received.
  --   *
  --   * The QoS policy reported by functions like `rmw_subscription_get_actual_qos` or
  --   * `rmw_publisher_get_actual_qos` may be best available, transient local, or volatile.
  --   *
  --   * Services and clients are not supported and default to the durability value in
  --   * `rmw_qos_profile_services_default`.
  --   *
  --   * The middleware is not expected to update the policy after creating a subscription or
  --   * publisher, even if the chosen policy is incompatible with newly discovered endpoints.
  --   * Therefore, this policy should be used with care since non-deterministic behavior
  --   * can occur due to races with discovery.
  --    

   subtype rmw_qos_durability_policy_t is rmw_qos_durability_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:456

  --/ QoS liveliness enumerations that describe a publisher's reporting policy for its alive status.
  --/ For a subscriber, these are its requirements for its topic's publishers.
  -- Suppress syntax errors, as cppcheck does not seem to handle enumerator attributes.
  -- cppcheck-suppress syntaxError
   type rmw_qos_liveliness_policy_e is 
     (RMW_QOS_POLICY_LIVELINESS_SYSTEM_DEFAULT,
      RMW_QOS_POLICY_LIVELINESS_AUTOMATIC,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE,
      RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_TOPIC,
      RMW_QOS_POLICY_LIVELINESS_UNKNOWN,
      RMW_QOS_POLICY_LIVELINESS_BEST_AVAILABLE)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:472

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
  --/ Will match the majority of endpoints and use a manual by topic policy if possible
  --*
  --   * A policy will be chosen at the time of creating a subscription or publisher.
  --   * A manual by topic policy will by chosen if it matches with all discovered endpoints,
  --   * otherwise an automatic policy will be chosen.
  --   *
  --   * The QoS policy reported by functions like `rmw_subscription_get_actual_qos` or
  --   * `rmw_publisher_get_actual_qos` may be best available, automatic, or manual by topic.
  --   *
  --   * Services and clients are not supported and default to the liveliness value in
  --   * `rmw_qos_profile_services_default`.
  --   *
  --   * The middleware is not expected to update the policy after creating a subscription or
  --   * publisher, even if the chosen policy is incompatible with newly discovered endpoints.
  --   * Therefore, this policy should be used with care since non-deterministic behavior
  --   * can occur due to races with discovery.
  --    

   subtype rmw_qos_liveliness_policy_t is rmw_qos_liveliness_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:514

  --/ QoS Deadline default.
  --/ Will match the majority of endpoints while maintaining as strict a policy as possible
  --*
  -- * Value is RMW_DURATION_INFINITE - 1.
  -- *
  -- * A policy will be chosen at the time of creating a subscription or publisher.
  -- * For a subscription, the deadline will be the maximum value of all discovered publisher
  -- * deadlines.
  -- * For a publisher, the deadline will be the minimum value of all discovered subscription
  -- * deadlines.
  -- *
  -- * The QoS policy reported by functions like `rmw_subscription_get_actual_qos` or
  -- * `rmw_publisher_get_actual_qos` may be best available or the actual deadline value.
  -- *
  -- * Services and clients are not supported and default to the deadline value in
  -- * `rmw_qos_profile_services_default`.
  -- *
  -- * The middleware is not expected to update the policy after creating a subscription or
  -- * publisher, even if the chosen policy is incompatible with newly discovered endpoints.
  -- * Therefore, this policy should be used with care since non-deterministic behavior
  -- * can occur due to races with discovery.
  --  

  --/ QoS Lifespan default.
  --/ QoS Liveliness lease duration default.
  --/ Will match the majority of endpoints while maintaining as strict a policy as possible
  --*
  -- * Value is RMW_DURATION_INFINITE - 1.
  -- *
  -- * A policy will be chosen at the time of creating a subscription or publisher.
  -- * For a subscription, the lease duration will be the maximum value of all discovered publisher
  -- * lease durations.
  -- * For a publisher, the lease duration will be the minimum value of all discovered subscription
  -- * lease durations.
  -- *
  -- * The QoS policy reported by functions like `rmw_subscription_get_actual_qos` or
  -- * `rmw_publisher_get_actual_qos` may be best available or the actual lease duration value.
  -- *
  -- * Services and clients are not supported and default to the lease duration value in
  -- * `rmw_qos_profile_services_default`.
  -- *
  -- * The middleware is not expected to update the policy after creating a subscription or
  -- * publisher, even if the chosen policy is incompatible with newly discovered endpoints.
  -- * Therefore, this policy should be used with care since non-deterministic behavior
  -- * can occur due to races with discovery.
  --  

  --/ ROS MiddleWare quality of service profile.
   type rmw_qos_profile_s is record
      history : aliased rmw_qos_history_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:572
      depth : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:574
      reliability : aliased rmw_qos_reliability_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:576
      durability : aliased rmw_qos_durability_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:578
      deadline : aliased rmw_rmw_time_h.rmw_time_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:585
      lifespan : aliased rmw_rmw_time_h.rmw_time_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:592
      liveliness : aliased rmw_qos_liveliness_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:594
      liveliness_lease_duration : aliased rmw_rmw_time_h.rmw_time_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:601
      avoid_ros_namespace_conventions : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:613
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:570

  --/ Size of the message queue.
  --/ Reliabiilty QoS policy setting
  --/ Durability QoS policy setting
  --/ The period at which messages are expected to be sent/received
  --*
  --    * RMW_DURATION_UNSPECIFIED will use the RMW implementation's default value,
  --    *   which may or may not be infinite.
  --    * RMW_DURATION_INFINITE explicitly states that messages never miss a deadline expectation.
  --     

  --/ The age at which messages are considered expired and no longer valid
  --*
  --    * RMW_DURATION_UNSPEFICIED will use the RMW implementation's default value,
  --    *   which may or may not be infinite.
  --    * RMW_DURATION_INFINITE explicitly states that messages do not expire.
  --     

  --/ Liveliness QoS policy setting
  --/ The time within which the RMW node or publisher must show that it is alive
  --*
  --    * RMW_DURATION_UNSPEFICIED will use the RMW implementation's default value,
  --    *   which may or may not be infinite.
  --    * RMW_DURATION_INFINITE explicitly states that liveliness is not enforced.
  --     

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

   subtype rmw_qos_profile_t is rmw_qos_profile_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:614

  --/ Globally unique identifier for a ROS graph entity
  --*
  -- * This is expected to be globally unique within a ROS domain.
  -- * The identifier should be the same when reported both locally (where the entity was created)
  -- * and on remote hosts or processes.
  --  

  --/ Name of the rmw implementation
   type rmw_gid_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:625
      data : aliased anon_array2848;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:628
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:622

  --/ Byte data GID value
   subtype rmw_gid_t is rmw_gid_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:629

  --/ Information describing an rmw message
  --/ Time when the message was published by the publisher.
  --*
  --   * The exact point at which the timestamp is taken is not specified, but
  --   * it should be taken consistently at the same point in the
  --   * publishing process each time.
  --    

   type rmw_message_info_s is record
      source_timestamp : aliased rmw_rmw_time_h.rmw_time_point_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:642
      received_timestamp : aliased rmw_rmw_time_h.rmw_time_point_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:649
      publication_sequence_number : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:671
      reception_sequence_number : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:689
      publisher_gid : aliased rmw_gid_t;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:714
      from_intra_process : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:717
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:634

  --/ Time when the message was received by the subscription.
  --*
  --   * The exact point at which the timestamp is taken is not specified, but
  --   * it should be taken consistently at the same point in the
  --   * process of receiving a message each time.
  --    

  --/ Sequence number of the received message set by the publisher.
  --*
  --   * This sequence number is set by the publisher and therefore uniquely identifies
  --   * a message when combined with the publisher GID.
  --   * For long running applications, the sequence number might wrap around at some point.
  --   *
  --   * If the rmw implementation doesn't support sequence numbers, its value will be
  --   * RMW_MESSAGE_INFO_SEQUENCE_NUMBER_UNSUPPORTED.
  --   *
  --   * Requirements:
  --   *
  --   * If `psn1` and `psn2` are the publication sequence numbers obtained by
  --   * calls to `rmw_take*()`, where `psn1` was obtained in a call that happened before `psn2` and both
  --   * sequence numbers are from the same publisher (i.e. also same publisher gid), then:
  --   *
  --   * - psn2 > psn1 (except in the case of a wrap around)
  --   * - `psn2 - psn1 - 1` is the number of messages the publisher sent in the middle of both
  --   *   received messages.
  --   *   Those might have already been taken by other `rmw_take*()` calls that happened in between or lost.
  --   *   `psn2 - psn1 - 1 = 0` if and only if the messages were sent by the publisher consecutively.
  --    

  --/ Sequence number of the received message set by the subscription.
  --*
  --   * This sequence number is set by the subscription regardless of which
  --   * publisher sent the message.
  --   * For long running applications, the sequence number might wrap around at some point.
  --   *
  --   * If the rmw implementation doesn't support sequence numbers, its value will be
  --   * RMW_MESSAGE_INFO_SEQUENCE_NUMBER_UNSUPPORTED.
  --   *
  --   * Requirements:
  --   *
  --   * If `rsn1` and `rsn2` are the reception sequence numbers obtained by
  --   * calls to `rmw_take*()`, where `rsn1` was obtained in a call that happened before `rsn2`, then:
  --   *
  --   * - rsn2 > rsn1 (except in the case of a wrap around)
  --   * - `rsn2 = rsn1 + 1` if and only if both `rmw_take*()` calls happened consecutively.
  --    

  --/ Global unique identifier of the publisher that sent the message.
  --*
  --   * The identifier uniquely identifies the publisher for the local context, but
  --   * it will not necessarily be the same identifier given in other contexts or processes
  --   * for the same publisher.
  --   * Therefore the identifier will uniquely identify the publisher within your application
  --   * but may disagree about the identifier for that publisher when compared to another
  --   * application.
  --   * Even with this limitation, when combined with the publisher sequence number it can
  --   * uniquely identify a message within your local context.
  --   * Publisher GIDs generated by the rmw implementation could collide at some point, in which
  --   * case it is not possible to distinguish which publisher sent the message.
  --   * The details of how GIDs are generated are rmw implementation dependent.
  --   *
  --   * It is possible the the rmw implementation needs to reuse a publisher GID,
  --   * due to running out of unique identifiers or some other constraint, in which case
  --   * the rmw implementation may document what happens in that case, but that
  --   * behavior is not defined here.
  --   * However, this should be avoided, if at all possible, by the rmw implementation,
  --   * and should be unlikely to happen in practice.
  --   *
  --   * \todo In the future we want this to uniquely identify the publisher globally across
  --   *   contexts, processes, and machines.
  --    

  --/ Whether this message is from intra_process communication or not
   subtype rmw_message_info_t is rmw_message_info_s;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:718

  --/ Get zero initialized mesage info.
   function rmw_get_zero_initialized_message_info return rmw_message_info_t  -- /opt/ros/jazzy/include/rmw/rmw/types.h:724
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
   rmw_log_severity_t_RMW_LOG_SEVERITY_DEBUG : constant rmw_log_severity_t := 10;
   rmw_log_severity_t_RMW_LOG_SEVERITY_INFO : constant rmw_log_severity_t := 20;
   rmw_log_severity_t_RMW_LOG_SEVERITY_WARN : constant rmw_log_severity_t := 30;
   rmw_log_severity_t_RMW_LOG_SEVERITY_ERROR : constant rmw_log_severity_t := 40;
   rmw_log_severity_t_RMW_LOG_SEVERITY_FATAL : constant rmw_log_severity_t := 50;  -- /opt/ros/jazzy/include/rmw/rmw/types.h:747

end rmw_rmw_types_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
