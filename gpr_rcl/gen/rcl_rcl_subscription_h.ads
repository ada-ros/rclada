pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rmw_rmw_types_h;
with rcl_rcl_allocator_h;
with Interfaces.C.Extensions;
with rmw_rmw_subscription_content_filter_options_h;
limited with rcl_rcl_node_h;
limited with rosidl_runtime_c_rosidl_runtime_c_message_type_support_struct_h;
with Interfaces.C.Strings;
with rcl_rcl_types_h;
with stddef_h;
with System;
limited with rmw_rmw_message_sequence_h;
limited with rcutils_rcutils_types_uint8_array_h;
limited with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_data_h;
with rmw_rmw_ret_types_h;
with rcl_rcl_event_callback_h;

package rcl_rcl_subscription_h is

  -- Copyright 2015 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ @file
  --/ Internal rcl implementation struct.
   type rcl_subscription_impl_s is null record;   -- incomplete struct

   subtype rcl_subscription_impl_t is rcl_subscription_impl_s;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:36

  --/ Structure which encapsulates a ROS Subscription.
  --/ Pointer to the subscription implementation
   type rcl_subscription_s is record
      impl : access rcl_subscription_impl_t;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:42
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:39

   subtype rcl_subscription_t is rcl_subscription_s;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:43

  --/ Options available for a rcl subscription.
  --/ Middleware quality of service settings for the subscription.
   type rcl_subscription_options_s is record
      qos : aliased rmw_rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:49
      allocator : aliased rcl_rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:52
      rmw_subscription_options : aliased rmw_rmw_types_h.rmw_subscription_options_t;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:54
      disable_loaned_message : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:56
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:46

  --/ Custom allocator for the subscription, used for incidental allocations.
  --* For default behavior (malloc/free), see: rcl_get_default_allocator()  
  --/ rmw specific subscription options, e.g. the rmw implementation specific payload.
  --/ Disable flag to LoanedMessage, initialized via environmental variable.
   subtype rcl_subscription_options_t is rcl_subscription_options_s;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:57

   type rcl_subscription_content_filter_options_s is record
      rmw_subscription_content_filter_options : aliased rmw_rmw_subscription_content_filter_options_h.rmw_subscription_content_filter_options_t;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:61
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:59

   subtype rcl_subscription_content_filter_options_t is rcl_subscription_content_filter_options_s;  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:62

  --/ Return a rcl_subscription_t struct with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_subscription_t before passing to
  -- * rcl_subscription_init().
  --  

   function rcl_get_zero_initialized_subscription return rcl_subscription_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_subscription";

  --/ Initialize a ROS subscription.
  --*
  -- * After calling this function on a rcl_subscription_t, it can be used to take
  -- * messages of the given type to the given topic using rcl_take().
  -- *
  -- * The given rcl_node_t must be valid and the resulting rcl_subscription_t is
  -- * only valid as long as the given rcl_node_t remains valid.
  -- *
  -- * The rosidl_message_type_support_t is obtained on a per .msg type basis.
  -- * When the user defines a ROS message, code is generated which provides the
  -- * required rosidl_message_type_support_t object.
  -- * This object can be obtained using a language appropriate mechanism.
  -- * \todo TODO(wjwwood) write these instructions once and link to it instead
  -- * For C a macro can be used (for example `std_msgs/String`):
  -- *
  -- * ```c
  -- * #include <rosidl_runtime_c/message_type_support_struct.h>
  -- * #include <std_msgs/msg/string.h>
  -- * const rosidl_message_type_support_t * string_ts =
  -- *   ROSIDL_GET_MSG_TYPE_SUPPORT(std_msgs, msg, String);
  -- * ```
  -- *
  -- * For C++ a template function is used:
  -- *
  -- * ```cpp
  -- * #include <rosidl_typesupport_cpp/message_type_support.hpp>
  -- * #include <std_msgs/msgs/string.hpp>
  -- * using rosidl_typesupport_cpp::get_message_type_support_handle;
  -- * const rosidl_message_type_support_t * string_ts =
  -- *   get_message_type_support_handle<std_msgs::msg::String>();
  -- * ```
  -- *
  -- * The rosidl_message_type_support_t object contains message type specific
  -- * information used to subscribe to messages.
  -- *
  -- * The topic name must be a c string which follows the topic and service name
  -- * format rules for unexpanded names, also known as non-fully qualified names:
  -- *
  -- * \see rcl_expand_topic_name
  -- *
  -- * The options struct allows the user to set the quality of service settings as
  -- * well as a custom allocator which is used when (de)initializing the
  -- * subscription to allocate space for incidental things, e.g. the topic
  -- * name string.
  -- *
  -- * Expected usage (for C messages):
  -- *
  -- * ```c
  -- * #include <rcl/rcl.h>
  -- * #include <rosidl_runtime_c/message_type_support_struct.h>
  -- * #include <std_msgs/msg/string.h>
  -- *
  -- * rcl_node_t node = rcl_get_zero_initialized_node();
  -- * rcl_node_options_t node_ops = rcl_node_get_default_options();
  -- * rcl_ret_t ret = rcl_node_init(&node, "node_name", "/my_namespace", &node_ops);
  -- * // ... error handling
  -- * const rosidl_message_type_support_t * ts =
  -- *   ROSIDL_GET_MSG_TYPE_SUPPORT(std_msgs, msg, String);
  -- * rcl_subscription_t subscription = rcl_get_zero_initialized_subscription();
  -- * rcl_subscription_options_t subscription_ops = rcl_subscription_get_default_options();
  -- * ret = rcl_subscription_init(&subscription, &node, ts, "chatter", &subscription_ops);
  -- * // ... error handling, and when finished deinitialization
  -- * ret = rcl_subscription_fini(&subscription, &node);
  -- * // ... error handling for rcl_subscription_fini()
  -- * ret = rcl_node_fini(&node);
  -- * // ... error handling for rcl_node_fini()
  -- * ```
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[out] subscription preallocated subscription structure
  -- * \param[in] node valid rcl node handle
  -- * \param[in] type_support type support object for the topic's type
  -- * \param[in] topic_name the name of the topic
  -- * \param[in] options subscription options, including quality of service settings
  -- * \return #RCL_RET_OK if subscription was initialized successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_ALREADY_INIT if the subcription is already initialized, or
  -- * \return #RCL_RET_NODE_INVALID if the node is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_TOPIC_NAME_INVALID if the given topic name is invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_subscription_init
     (subscription : access rcl_subscription_t;
      node : access constant rcl_rcl_node_h.rcl_node_s;
      type_support : access constant rosidl_runtime_c_rosidl_runtime_c_message_type_support_struct_h.rosidl_message_type_support_t;
      topic_name : Interfaces.C.Strings.chars_ptr;
      options : access constant rcl_subscription_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:166
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_init";

  --/ Finalize a rcl_subscription_t.
  --*
  -- * After calling, the node will no longer be subscribed on this topic
  -- * (assuming this is the only subscription on this topic in this node).
  -- *
  -- * After calling, calls to rcl_wait and rcl_take will fail when using this
  -- * subscription.
  -- * Additioanlly rcl_wait will be interrupted if currently blocking.
  -- * However, the given node handle is still valid.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] subscription handle to the subscription to be deinitialized
  -- * \param[in] node a valid (not finalized) handle to the node used to create the subscription
  -- * \return #RCL_RET_OK if subscription was deinitialized successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_NODE_INVALID if the node is invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_subscription_fini (subscription : access rcl_subscription_t; node : access rcl_rcl_node_h.rcl_node_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:203
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_fini";

  --/ Return the default subscription options in a rcl_subscription_options_t.
  --*
  -- * The defaults are:
  -- *
  -- * - qos = rmw_qos_profile_default
  -- * - allocator = rcl_get_default_allocator()
  -- * - rmw_subscription_options = rmw_get_default_subscription_options();
  -- * - disable_loaned_message = true, false only if ROS_DISABLE_LOANED_MESSAGES=0
  -- *
  -- * \return A structure containing the default options for a subscription.
  --  

   function rcl_subscription_get_default_options return rcl_subscription_options_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_default_options";

  --/ Reclaim resources held inside rcl_subscription_options_t structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | No
  -- *
  -- * \param[in] option The structure which its resources have to be deallocated.
  -- * \return `RCL_RET_OK` if the memory was successfully freed, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if option is NULL, or
  -- * \return `RCL_RET_BAD_ALLOC` if deallocating memory fails.
  --  

   function rcl_subscription_options_fini (option : access rcl_subscription_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_options_fini";

  --/ Set the content filter options for the given subscription options.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | No
  -- *
  -- * \param[in] filter_expression The filter expression is similar to the WHERE part of an SQL clause.
  -- * \param[in] expression_parameters_argc The maximum of expression parameters argc is 100.
  -- * \param[in] expression_parameter_argv The expression parameters argv are the tokens placeholder
  -- * ‘parameters’ (i.e., "%n" tokens begin from 0) in the filter_expression.
  -- *
  -- * It can be NULL if there is no "%n" tokens placeholder in filter_expression.
  -- * \param[out] options The subscription options to be set.
  -- * \return `RCL_RET_OK` if set options successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if arguments invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory fails.
  --  

   function rcl_subscription_options_set_content_filter_options
     (filter_expression : Interfaces.C.Strings.chars_ptr;
      expression_parameters_argc : stddef_h.size_t;
      expression_parameter_argv : System.Address;
      options : access rcl_subscription_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:265
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_options_set_content_filter_options";

  --/ Return the zero initialized subscription content filter options.
   function rcl_get_zero_initialized_subscription_content_filter_options return rcl_subscription_content_filter_options_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_subscription_content_filter_options";

  --/ Initialize the content filter options for the given subscription options.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | No
  -- *
  -- * \param[in] subscription the handle to the subscription.
  -- * \param[in] filter_expression The filter expression is similar to the WHERE part of an SQL clause,
  -- * use empty ("") can reset (or clear) the content filter setting of a subscription.
  -- * \param[in] expression_parameters_argc The maximum of expression parameters argc is 100.
  -- * \param[in] expression_parameter_argv The expression parameters argv are the tokens placeholder
  -- * ‘parameters’ (i.e., "%n" tokens begin from 0) in the filter_expression.
  -- *
  -- * It can be NULL if there is no "%n" tokens placeholder in filter_expression.
  -- * \param[out] options The subscription options to be set.
  -- * \return `RCL_RET_OK` if set options successfully, or
  -- * \return `RCL_RET_SUBSCRIPTION_INVALID` if subscription is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if arguments invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory fails.
  --  

   function rcl_subscription_content_filter_options_init
     (subscription : access constant rcl_subscription_t;
      filter_expression : Interfaces.C.Strings.chars_ptr;
      expression_parameters_argc : stddef_h.size_t;
      expression_parameter_argv : System.Address;
      options : access rcl_subscription_content_filter_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:304
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_content_filter_options_init";

  --/ Set the content filter options for the given subscription options.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | No
  -- *
  -- * \param[in] subscription the handle to the subscription.
  -- * \param[in] filter_expression The filter expression is similar to the WHERE part of an SQL clause,
  -- * use empty ("") can reset (or clear) the content filter setting of a subscription.
  -- * \param[in] expression_parameters_argc The maximum of expression parameters argc is 100.
  -- * \param[in] expression_parameter_argv The expression parameters argv are the tokens placeholder
  -- * ‘parameters’ (i.e., "%n" tokens begin from 0) in the filter_expression.
  -- *
  -- * It can be NULL if there is no "%n" tokens placeholder in filter_expression.
  -- * \param[out] options The subscription options to be set.
  -- * \return `RCL_RET_OK` if set options successfully, or
  -- * \return `RCL_RET_SUBSCRIPTION_INVALID` if subscription is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if arguments invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory fails.
  --  

   function rcl_subscription_content_filter_options_set
     (subscription : access constant rcl_subscription_t;
      filter_expression : Interfaces.C.Strings.chars_ptr;
      expression_parameters_argc : stddef_h.size_t;
      expression_parameter_argv : System.Address;
      options : access rcl_subscription_content_filter_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:338
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_content_filter_options_set";

  --/ Reclaim rcl_subscription_content_filter_options_t structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | No
  -- *
  -- * \param[in] subscription the handle to the subscription.
  -- * \param[in] options The structure which its resources have to be deallocated.
  -- * \return `RCL_RET_OK` if the memory was successfully freed, or
  -- * \return `RCL_RET_SUBSCRIPTION_INVALID` if subscription is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if option is NULL, or
  -- *  if its allocator is invalid and the structure contains initialized memory.
  --  

   function rcl_subscription_content_filter_options_fini (subscription : access constant rcl_subscription_t; options : access rcl_subscription_content_filter_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:365
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_content_filter_options_fini";

  --/ Check if the content filtered topic feature is enabled in the subscription.
  --*
  -- * Depending on the middleware and whether cft is enabled in the subscription.
  -- *
  -- * \return `true` if the content filtered topic of `subscription` is enabled, otherwise `false`
  --  

   function rcl_subscription_is_cft_enabled (subscription : access constant rcl_subscription_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:378
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_is_cft_enabled";

  --/ Set the filter expression and expression parameters for the subscription.
  --*
  -- * This function will set a filter expression and an array of expression parameters
  -- * for the given subscription.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Maybe [1]
  -- * Lock-Free          | Maybe [1]
  -- *
  -- * \param[in] subscription The subscription to set content filter options.
  -- * \param[in] options The rcl content filter options.
  -- * \return `RCL_RET_OK` if the query was successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `subscription` is NULL, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `options` is NULL, or
  -- * \return `RCL_RET_UNSUPPORTED` if the implementation does not support content filter topic, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_subscription_set_content_filter (subscription : access constant rcl_subscription_t; options : access constant rcl_subscription_content_filter_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:404
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_set_content_filter";

  --/ Retrieve the filter expression of the subscription.
  --*
  -- * This function will return an filter expression by the given subscription.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Maybe [1]
  -- * Lock-Free          | Maybe [1]
  -- *
  -- * \param[in] subscription The subscription object to inspect.
  -- * \param[out] options The rcl content filter options.
  -- *   It is up to the caller to finalize this options later on, using
  -- *   rcl_subscription_content_filter_options_fini().
  -- * \return `RCL_RET_OK` if the query was successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `subscription` is NULL, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `options` is NULL, or
  -- * \return `RCL_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \return `RCL_RET_UNSUPPORTED` if the implementation does not support content filter topic, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_subscription_get_content_filter (subscription : access constant rcl_subscription_t; options : access rcl_subscription_content_filter_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:435
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_content_filter";

  --/ Take a ROS message from a topic using a rcl subscription.
  --*
  -- * It is the job of the caller to ensure that the type of the ros_message
  -- * argument and the type associated with the subscription, via the type
  -- * support, match.
  -- * Passing a different type to rcl_take produces undefined behavior and cannot
  -- * be checked by this function and therefore no deliberate error will occur.
  -- *
  -- * TODO(wjwwood) blocking of take?
  -- * TODO(wjwwood) pre-, during-, and post-conditions for message ownership?
  -- * TODO(wjwwood) is rcl_take thread-safe?
  -- * TODO(wjwwood) Should there be an rcl_message_info_t?
  -- *
  -- * The ros_message pointer should point to an already allocated ROS message
  -- * struct of the correct type, into which the taken ROS message will be copied
  -- * if one is available.
  -- * If taken is false after calling, then the ROS message will be unmodified.
  -- *
  -- * The taken boolean may be false even if a wait set reports that the
  -- * subscription was ready to be taken from in some cases, e.g. when the
  -- * state of the subscription changes it may cause the wait set to wake up
  -- * but subsequent takes to fail to take anything.
  -- *
  -- * If allocation is required when taking the message, e.g. if space needs to
  -- * be allocated for a dynamically sized array in the target message, then the
  -- * allocator given in the subscription options is used.
  -- *
  -- * The rmw_message_info struct contains meta information about this particular
  -- * message instance, like what the GUID of the publisher which published it
  -- * originally or whether or not the message received from within the same
  -- * process.
  -- * The message_info argument should be an already allocated rmw_message_info_t
  -- * structure.
  -- * Passing `NULL` for message_info will result in the argument being ignored.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Maybe [1]
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] only if required when filling the message, avoided for fixed sizes</i>
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take
  -- * \param[inout] ros_message type-erased ptr to a allocated ROS message
  -- * \param[out] message_info rmw struct which contains meta-data for the message
  -- * \param[in] allocation structure pointer used for memory preallocation (may be NULL)
  -- * \return #RCL_RET_OK if the message was taken, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_SUBSCRIPTION_TAKE_FAILED if take failed but no error
  -- *         occurred in the middleware, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_take
     (subscription : access constant rcl_subscription_t;
      ros_message : System.Address;
      message_info : access rmw_rmw_types_h.rmw_message_info_s;
      allocation : access rmw_rmw_types_h.rmw_subscription_allocation_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:499
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take";

  --/ Take a sequence of messages from a topic using a rcl subscription.
  --*
  -- * In contrast to rcl_take(), this function can take multiple messages at
  -- * the same time.
  -- * It is the job of the caller to ensure that the type of the message_sequence
  -- * argument and the type associated with the subscription, via the type
  -- * support, match.
  -- *
  -- * The message_sequence pointer should point to an already allocated sequence
  -- * of ROS messages of the correct type, into which the taken ROS messages will
  -- * be copied if messages are available.
  -- * The message_sequence `size` member will be set to the number of messages
  -- * correctly taken.
  -- *
  -- * The rmw_message_info_sequence struct contains meta information about the
  -- * corresponding message instance index.
  -- * The message_info_sequence argument should be an already allocated
  -- * rmw_message_info_sequence_t structure.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Maybe [1]
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] only if storage in the serialized_message is insufficient</i>
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take.
  -- * \param[in] count number of messages to attempt to take.
  -- * \param[inout] message_sequence pointer to a (pre-allocated) message sequence.
  -- * \param[inout] message_info_sequence pointer to a (pre-allocated) message info sequence.
  -- * \param[in] allocation structure pointer used for memory preallocation (may be NULL)
  -- * \return #RCL_RET_OK if one or more messages was taken, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_SUBSCRIPTION_TAKE_FAILED if take failed but no error
  -- *         occurred in the middleware, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_take_sequence
     (subscription : access constant rcl_subscription_t;
      count : stddef_h.size_t;
      message_sequence : access rmw_rmw_message_sequence_h.rmw_message_sequence_s;
      message_info_sequence : access rmw_rmw_message_sequence_h.rmw_message_info_sequence_s;
      allocation : access rmw_rmw_types_h.rmw_subscription_allocation_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:550
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take_sequence";

  --/ Take a serialized raw message from a topic using a rcl subscription.
  --*
  -- * In contrast to rcl_take(), this function stores the taken message in
  -- * its raw binary representation.
  -- * It is the job of the caller to ensure that the type associate with the subscription
  -- * matches, and can optionally be deserialized into its ROS message via, the correct
  -- * type support.
  -- * If the `serialized_message` parameter contains enough preallocated memory, the incoming
  -- * message can be taken without any additional memory allocation.
  -- * If not, the function will dynamically allocate enough memory for the message.
  -- * Passing a different type to rcl_take produces undefined behavior and cannot
  -- * be checked by this function and therefore no deliberate error will occur.
  -- *
  -- * Apart from the differences above, this function behaves like rcl_take().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Maybe [1]
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] only if storage in the serialized_message is insufficient</i>
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take
  -- * \param[inout] serialized_message pointer to a (pre-allocated) serialized message.
  -- * \param[out] message_info rmw struct which contains meta-data for the message
  -- * \param[in] allocation structure pointer used for memory preallocation (may be NULL)
  -- * \return #RCL_RET_OK if the message was published, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_SUBSCRIPTION_TAKE_FAILED if take failed but no error
  -- *         occurred in the middleware, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_take_serialized_message
     (subscription : access constant rcl_subscription_t;
      serialized_message : access rcutils_rcutils_types_uint8_array_h.rcutils_uint8_array_s;
      message_info : access rmw_rmw_types_h.rmw_message_info_s;
      allocation : access rmw_rmw_types_h.rmw_subscription_allocation_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:597
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take_serialized_message";

  --/ Take a dynamic type message from a topic using a rcl subscription.
  --*
  -- * In contrast to rcl_take(), this function takes a dynamic type message with dynamic data taken
  -- * directly from the middleware.
  -- * It is the job of the caller to ensure that the type associated with the subscription
  -- * matches, and that the subscription uses the dynamic type rosidl_message_type_support_t.
  -- *
  -- * Apart from the differences above, this function behaves like rcl_take().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take
  -- * \param[inout] dynamic_message pointer to a (pre-allocated) dynamic type message.
  -- * \param[out] message_info rmw struct which contains meta-data for the message
  -- * \param[in] allocation structure pointer used for memory preallocation (may be NULL)
  -- * \return #RCL_RET_OK if the message was taken, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_SUBSCRIPTION_TAKE_FAILED if take failed but no error
  -- *         occurred in the middleware, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_take_dynamic_message
     (subscription : access constant rcl_subscription_t;
      dynamic_message : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_data_h.rosidl_dynamic_typesupport_dynamic_data_s;
      message_info : access rmw_rmw_types_h.rmw_message_info_s;
      allocation : access rmw_rmw_types_h.rmw_subscription_allocation_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:635
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take_dynamic_message";

  --/ Take a loaned message from a topic using a rcl subscription.
  --*
  -- * Depending on the middleware, incoming messages can be loaned to the user's callback
  -- * without further copying.
  -- * The implicit contract here is that the middleware owns the memory allocated for this message.
  -- * The user must not destroy the message, but rather has to return it with a call to
  -- * \sa rcl_return_loaned_message to the middleware.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take
  -- * \param[inout] loaned_message a pointer to the loaned messages.
  -- * \param[out] message_info rmw struct which contains meta-data for the message.
  -- * \param[in] allocation structure pointer used for memory preallocation (may be NULL)
  -- * \return #RCL_RET_OK if the loaned message sequence was taken, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_SUBSCRIPTION_TAKE_FAILED if take failed but no error
  -- *         occurred in the middleware, or
  -- * \return #RCL_RET_UNSUPPORTED if the middleware does not support that feature, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_take_loaned_message
     (subscription : access constant rcl_subscription_t;
      loaned_message : System.Address;
      message_info : access rmw_rmw_types_h.rmw_message_info_s;
      allocation : access rmw_rmw_types_h.rmw_subscription_allocation_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:673
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take_loaned_message";

  --/ Return a loaned message from a topic using a rcl subscription.
  --*
  -- * If a loaned message was previously obtained from the middleware with a call to
  -- * \sa rcl_take_loaned_message, this message has to be returned to indicate to the middleware
  -- * that the user no longer needs that memory.
  -- * The user must not delete the message.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription the handle to the subscription from which to take
  -- * \param[in] loaned_message a pointer to the loaned messages.
  -- * \return #RCL_RET_OK if the message was published, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_UNSUPPORTED if the middleware does not support that feature, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_return_loaned_message_from_subscription (subscription : access constant rcl_subscription_t; loaned_message : System.Address) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:705
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_return_loaned_message_from_subscription";

  --/ Get the topic name for the subscription.
  --*
  -- * This function returns the subscription's internal topic name string.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - subscription is `NULL`
  -- *   - subscription is invalid (never called init, called fini, or invalid)
  -- *
  -- * The returned string is only valid as long as the subscription is valid.
  -- * The value of the string may change if the topic name changes, and therefore
  -- * copying the string is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription the pointer to the subscription
  -- * \return name string if successful, otherwise `NULL`
  --  

   function rcl_subscription_get_topic_name (subscription : access constant rcl_subscription_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:734
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_topic_name";

  --/ Return the rcl subscription options.
  --*
  -- * This function returns the subscription's internal options struct.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - subscription is `NULL`
  -- *   - subscription is invalid (never called init, called fini, or invalid)
  -- *
  -- * The returned struct is only valid as long as the subscription is valid.
  -- * The values in the struct may change if the subscription's options change,
  -- * and therefore copying the struct is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription pointer to the subscription
  -- * \return options struct if successful, otherwise `NULL`
  --  

   function rcl_subscription_get_options (subscription : access constant rcl_subscription_t) return access constant rcl_subscription_options_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:761
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_options";

  --/ Return the rmw subscription handle.
  --*
  -- * The handle returned is a pointer to the internally held rmw handle.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - subscription is `NULL`
  -- *   - subscription is invalid (never called init, called fini, or invalid)
  -- *
  -- * The returned handle is made invalid if the subscription is finalized or if
  -- * rcl_shutdown() is called.
  -- * The returned handle is not guaranteed to be valid for the life time of the
  -- * subscription as it may be finalized and recreated itself.
  -- * Therefore it is recommended to get the handle from the subscription using
  -- * this function each time it is needed and avoid use of the handle
  -- * concurrently with functions that might change it.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription pointer to the rcl subscription
  -- * \return rmw subscription handle if successful, otherwise `NULL`
  --  

   function rcl_subscription_get_rmw_handle (subscription : access constant rcl_subscription_t) return access rmw_rmw_types_h.rmw_subscription_s  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:792
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_rmw_handle";

  --/ Check that the subscription is valid.
  --*
  -- * The bool returned is `false` if `subscription` is invalid.
  -- * The bool returned is `true` otherwise.
  -- * In the case where `false` is to be returned, an error message is set.
  -- * This function cannot fail.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription pointer to the rcl subscription
  -- * \return `true` if `subscription` is valid, otherwise `false`
  --  

   function rcl_subscription_is_valid (subscription : access constant rcl_subscription_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:814
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_is_valid";

  --/ Get the number of publishers matched to a subscription.
  --*
  -- * Used to get the internal count of publishers matched to a subscription.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Maybe [1]
  -- * Lock-Free          | Maybe [1]
  -- * <i>[1] only if the underlying rmw doesn't make use of this feature </i>
  -- *
  -- * \param[in] subscription pointer to the rcl subscription
  -- * \param[out] publisher_count number of matched publishers
  -- * \return #RCL_RET_OK if the count was retrieved, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_SUBSCRIPTION_INVALID if the subscription is invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_subscription_get_publisher_count (subscription : access constant rcl_subscription_t; publisher_count : access stddef_h.size_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:839
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_publisher_count";

  --/ Get the actual qos settings of the subscription.
  --*
  -- * Used to get the actual qos settings of the subscription.
  -- * The actual configuration applied when using RMW_*_SYSTEM_DEFAULT
  -- * can only be resolved after the creation of the subscription, and it
  -- * depends on the underlying rmw implementation.
  -- * If the underlying setting in use can't be represented in ROS terms,
  -- * it will be set to RMW_*_UNKNOWN.
  -- * The returned struct is only valid as long as the rcl_subscription_t is valid.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] subscription pointer to the rcl subscription
  -- * \return qos struct if successful, otherwise `NULL`
  --  

   function rcl_subscription_get_actual_qos (subscription : access constant rcl_subscription_t) return access constant rmw_rmw_types_h.rmw_qos_profile_s  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:867
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_get_actual_qos";

  --/ Check if subscription instance can loan messages.
  --*
  -- * Depending on the middleware and the message type, this will return true if the middleware
  -- * can allocate a ROS message instance.
  -- *
  -- * \param[in] subscription The subscription instance to check for the ability to loan messages
  -- * \return `true` if the subscription instance can loan messages, `false` otherwise.
  --  

   function rcl_subscription_can_loan_messages (subscription : access constant rcl_subscription_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:879
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_can_loan_messages";

  --/ Set the on new message callback function for the subscription.
  --*
  -- * This API sets the callback function to be called whenever the
  -- * subscription is notified about a new message.
  -- *
  -- * \sa rmw_subscription_set_on_new_message_callback for details about this
  -- * function.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Maybe [1]
  -- * Lock-Free          | Maybe [1]
  -- * <i>[1] rmw implementation defined</i>
  -- *
  -- * \param[in] subscription The subscription on which to set the callback
  -- * \param[in] callback The callback to be called when new messages arrive, may be NULL
  -- * \param[in] user_data Given to the callback when called later, may be NULL
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `subscription` is NULL, or
  -- * \return `RCL_RET_UNSUPPORTED` if the API is not implemented in the dds implementation
  --  

   function rcl_subscription_set_on_new_message_callback
     (subscription : access constant rcl_subscription_t;
      callback : rcl_rcl_event_callback_h.rcl_event_callback_t;
      user_data : System.Address) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/subscription.h:908
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_set_on_new_message_callback";

end rcl_rcl_subscription_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
