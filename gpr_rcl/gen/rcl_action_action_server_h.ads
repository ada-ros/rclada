pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;
with rcl_allocator_h;
with rcl_time_h;
limited with rcl_node_h;
limited with rosidl_runtime_c_action_type_support_struct_h;
with Interfaces.C.Strings;
with rcl_types_h;
with System;
limited with rcl_action_types_h;
limited with rcl_action_goal_handle_h;
with stddef_h;
with Interfaces.C.Extensions;

package rcl_action_action_server_h is

  -- Copyright 2018 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Internal rcl_action implementation struct.
   type rcl_action_server_impl_t is null record;   -- incomplete struct

  --/ Structure which encapsulates a ROS Action Server.
  --/ Pointer to the action server implementation
   type rcl_action_server_t is record
      impl : access rcl_action_server_impl_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl_action/action_server.h:36

  --/ Options available for a rcl_action_server_t.
  --/ Middleware quality of service settings for the action server.
  --/ Goal service quality of service
   type rcl_action_server_options_t is record
      goal_service_qos : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:47
      cancel_service_qos : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:49
      result_service_qos : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:51
      feedback_topic_qos : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:53
      status_topic_qos : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:55
      allocator : aliased rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:58
      result_timeout : aliased rcl_time_h.rcl_duration_t;  -- /opt/ros/foxy/include/rcl_action/action_server.h:60
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl_action/action_server.h:43

  --/ Cancel service quality of service
  --/ Result service quality of service
  --/ Feedback topic quality of service
  --/ Status topic quality of service
  --/ Custom allocator for the action server, used for incidental allocations.
  --* For default behavior (malloc/free), see: rcl_get_default_allocator()  
  --/ Goal handles that have results longer than this time are deallocated.
  --/ Return a rcl_action_server_t struct with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_action_server_t before passing to
  -- * rcl_action_server_init().
  --  

   function rcl_action_get_zero_initialized_server return rcl_action_server_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_server";

  --/ Initialize an action server.
  --*
  -- * After calling this function on a rcl_action_server_t, it can be used to take
  -- * goals of the given type for the given action name using rcl_action_take_goal_request()
  -- * and take cancel requests with rcl_action_take_cancel_request().
  -- * It can also send a result for a request using rcl_action_send_result() or
  -- * rcl_action_send_cancel_response().
  -- *
  -- * After accepting a goal with rcl_action_take_goal_request(), the action server can
  -- * be used to send feedback with rcl_action_publish_feedback() and send status
  -- * messages with rcl_action_publish_status().
  -- *
  -- * The given rcl_node_t must be valid and the resulting rcl_action_server_t is
  -- * only valid as long as the given rcl_node_t remains valid.
  -- *
  -- * The give rcl_clock_t must be valid and the resulting rcl_ction_server_t is
  -- * only valid as long ast he given rcl_clock_t remains valid.
  -- *
  -- * The rosidl_action_type_support_t is obtained on a per .action type basis.
  -- * When the user defines a ROS action, code is generated which provides the
  -- * required rosidl_action_type_support_t object.
  -- * This object can be obtained using a language appropriate mechanism.
  -- * \todo TODO(jacobperron) write these instructions once and link to it instead
  -- *
  -- * For C, a macro can be used (for example `example_interfaces/Fibonacci`):
  -- *
  -- * ```c
  -- * #include <rosidl_runtime_c/action_type_support_struct.h>
  -- * #include <example_interfaces/action/fibonacci.h>
  -- * const rosidl_action_type_support_t * ts =
  -- *   ROSIDL_GET_ACTION_TYPE_SUPPORT(example_interfaces, Fibonacci);
  -- * ```
  -- *
  -- * For C++, a template function is used:
  -- *
  -- * ```cpp
  -- * #include <rosidl_runtime_cpp/action_type_support.hpp>
  -- * #include <example_interfaces/action/fibonacci.h>
  -- * using rosidl_typesupport_cpp::get_action_type_support_handle;
  -- * const rosidl_action_type_support_t * ts =
  -- *   get_action_type_support_handle<example_interfaces::action::Fibonacci>();
  -- * ```
  -- *
  -- * The rosidl_action_type_support_t object contains action type specific
  -- * information used to send or take goals, results, and feedback.
  -- *
  -- * The topic name must be a c string that follows the topic and service name
  -- * format rules for unexpanded names, also known as non-fully qualified names:
  -- *
  -- * \see rcl_expand_topic_name
  -- *
  -- * The options struct allows the user to set the quality of service settings as
  -- * well as a custom allocator that is used when initializing/finalizing the
  -- * client to allocate space for incidentals, e.g. the action server name string.
  -- *
  -- * Expected usage (for C action servers):
  -- *
  -- * ```c
  -- * #include <rcl/rcl.h>
  -- * #include <rcl_action/rcl_action.h>
  -- * #include <rosidl_runtime_c/action_type_support_struct.h>
  -- * #include <example_interfaces/action/fibonacci.h>
  -- *
  -- * rcl_node_t node = rcl_get_zero_initialized_node();
  -- * rcl_node_options_t node_ops = rcl_node_get_default_options();
  -- * rcl_ret_t ret = rcl_node_init(&node, "node_name", "/my_namespace", &node_ops);
  -- * // ... error handling
  -- * const rosidl_action_type_support_t * ts =
  -- *   ROSIDL_GET_ACTION_TYPE_SUPPORT(example_interfaces, Fibonacci);
  -- * rcl_action_server_t action_server = rcl_action_get_zero_initialized_server();
  -- * rcl_action_server_options_t action_server_ops = rcl_action_server_get_default_options();
  -- * ret = rcl_action_server_init(&action_server, &node, ts, "fibonacci", &action_server_ops);
  -- * // ... error handling, and on shutdown do finalization:
  -- * ret = rcl_action_server_fini(&action_server, &node);
  -- * // ... error handling for rcl_action_server_fini()
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
  -- * \param[out] action_server handle to a preallocated, zero-initialized action server structure
  -- *   to be initialized.
  -- * \param[in] node valid node handle
  -- * \param[in] clock valid clock handle
  -- * \param[in] type_support type support object for the action's type
  -- * \param[in] action_name the name of the action
  -- * \param[in] options action_server options, including quality of service settings
  -- * \return `RCL_RET_OK` if action_server was initialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_NODE_INVALID` if the node is invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ACTION_NAME_INVALID` if the given action name is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_server_init
     (action_server : access rcl_action_server_t;
      node : access rcl_node_h.rcl_node_t;
      clock : access rcl_time_h.rcl_clock_t;
      type_support : access constant rosidl_runtime_c_action_type_support_struct_h.rosidl_action_type_support_t;
      action_name : Interfaces.C.Strings.chars_ptr;
      options : access constant rcl_action_server_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_init";

  --/ Finalize an action server.
  --*
  -- * After calling, the node will no longer listen to services and topics for this action server.
  -- * (assuming this is the only action server of this type in this node).
  -- *
  -- * After calling, calls to rcl_wait(), rcl_action_take_goal_request(),
  -- * rcl_action_take_cancel_request(), rcl_action_publish_feedback(),
  -- * rcl_action_publish_status(), rcl_action_send_result(), and
  -- * rcl_action_send_cancel_response() will fail when using this action server.
  -- * Additionally, rcl_wait() will be interrupted if currently blocking.
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
  -- * \param[inout] action_server handle to the action_server to be deinitialized
  -- * \param[in] node handle to the node used to create the action server
  -- * \return `RCL_RET_OK` if the action server was deinitialized successfully, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server pointer is null, or
  -- * \return `RCL_RET_NODE_INVALID` if the node is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_server_fini (action_server : access rcl_action_server_t; node : access rcl_node_h.rcl_node_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:215
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_fini";

  --/ Return the default action server options in a rcl_action_server_options_t.
  --*
  -- * The defaults are:
  -- *
  -- * - goal_service_qos = rmw_qos_profile_services_default;
  -- * - cancel_service_qos = rmw_qos_profile_services_default;
  -- * - result_service_qos = rmw_qos_profile_services_default;
  -- * - feedback_topic_qos = rmw_qos_profile_default;
  -- * - status_topic_qos = rcl_action_qos_profile_status_default;
  -- * - allocator = rcl_get_default_allocator();
  -- * - result_timeout = RCUTILS_S_TO_NS(15 * 60);  // 15 minutes
  --  

   function rcl_action_server_get_default_options return rcl_action_server_options_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:232
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_get_default_options";

  --/ Take a pending ROS goal using an action server.
  --*
  -- * \todo TODO(jacobperron) blocking of take?
  -- *
  -- * \todo TODO(jacobperron) pre-, during-, and post-conditions for message ownership?
  -- *
  -- * \todo TODO(jacobperron) is this thread-safe?
  -- *
  -- * The caller is responsible for ensuring that the type of `ros_goal_request`
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * `ros_goal_request` should point to a preallocated, zero-initialized,
  -- * ROS goal message.
  -- * If a goal request is taken successfully, it will be copied into `ros_goal_request`.
  -- *
  -- * If allocation is required when taking the request, e.g. if space needs to
  -- * be allocated for a dynamically sized array in the target message, then the
  -- * allocator given in the action server options is used.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Maybe [1]
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] only if required when filling the request, avoided for fixed sizes</i>
  -- *
  -- * \param[in] action_server handle to the action server that will take the request
  -- * \param[out] request_header pointer to the goal request header
  -- * \param[out] ros_goal_request a preallocated, zero-initialized, ROS goal request message
  -- *   where the request is copied
  -- * \return `RCL_RET_OK` if the request was taken, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ACTION_SERVER_TAKE_FAILED` if take failed but no error occurred
  -- *         in the middleware, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_take_goal_request
     (action_server : access constant rcl_action_server_t;
      request_header : access rmw_types_h.rmw_request_id_t;
      ros_goal_request : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_take_goal_request";

  --/ Send a response for a goal request to an action client using an action server.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * The caller is responsible for ensuring that the type of `ros_goal_response`
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * If the caller intends to send an 'accepted' response, before calling this function
  -- * the caller should use rcl_action_accept_new_goal() to get a rcl_action_goal_handle_t
  -- * for future interaction with the goal (e.g. publishing feedback and canceling the goal).
  -- *
  -- * This function is thread safe so long as access to both the action server and the
  -- * `ros_goal_response` are synchronized.
  -- * That means that calling rcl_action_send_goal_response() from multiple threads is
  -- * allowed, but calling rcl_action_send_goal_response() at the same time as non-thread safe
  -- * action server functions is not, e.g. calling rcl_action_send_goal_response() and
  -- * rcl_action_server_fini() concurrently is not allowed.
  -- * Before calling rcl_action_send_goal_response() the `ros_goal_request` can change and
  -- * after calling rcl_action_send_goal_response() the `ros_goal_request` can change, but it
  -- * cannot be changed during the rcl_action_send_goal_response() call.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes [1]
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] for unique pairs of action servers and responses, see above for more</i>
  -- *
  -- * \param[in] action_server handle to the action server that will make the goal response
  -- * \param[in] response_header pointer to the goal response header
  -- * \param[in] ros_goal_response a ROS goal response message to send
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_send_goal_response
     (action_server : access constant rcl_action_server_t;
      response_header : access rmw_types_h.rmw_request_id_t;
      ros_goal_response : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_send_goal_response";

  --/ Accept a new goal using an action server.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * Creates and returns a new goal handle.
  -- * The action server starts tracking it internally.
  -- * If a failure occurs, `NULL` is returned and an error message is set.
  -- * Possible reasons for failure:
  -- *   - action server is invalid
  -- *   - goal info is invalid
  -- *   - goal ID is already being tracked by the action server
  -- *   - memory allocation failure
  -- *
  -- * This function should be called after receiving a new goal request with
  -- * rcl_action_take_goal_request() and before sending a response with
  -- * rcl_action_send_goal_response().
  -- *
  -- * After calling this function, the action server will start tracking the goal.
  -- * The pointer to the goal handle becomes invalid after `rcl_action_server_fini()` is called.
  -- * The caller becomes responsible for finalizing the goal handle later.
  -- *
  -- * Example usage:
  -- *
  -- * ```c
  -- * #include <rcl/rcl_action.h>
  -- *
  -- * // ... init an action server
  -- * // Take a goal request (client library type)
  -- * rcl_ret_t ret = rcl_action_take_goal_request(&action_server, &goal_request);
  -- * // ... error handling
  -- * // If the goal is accepted, then tell the action server
  -- * // First, create a goal info message
  -- * rcl_action_goal_info_t goal_info = rcl_action_get_zero_initialized_goal_info();
  -- * // ... populate goal_info.uuid (unique_identifier_msgs/UUID)
  -- * // ... populate goal_info.stamp (builtin_interfaces/Time)
  -- * rcl_action_goal_handle_t * goal_handle = rcl_action_accept_new_goal(&action_server, &goal_info);
  -- * // ... error_handling
  -- * // ... Populate goal response (client library type)
  -- * ret = rcl_action_send_goal_response(&action_server, &goal_response);
  -- * // ... error handling, and sometime before shutdown finalize goal info message
  -- * ret = rcl_action_goal_info_fini(&goal_info, &action_server);
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
  -- * \param[in] action_server handle to the action server that is accepting the goal
  -- * \param[in] goal_info a message containing info about the goal being accepted
  -- * \return a pointer to a new goal handle representing the accepted goal, or
  -- * \return `NULL` if a failure occured.
  --  

   function rcl_action_accept_new_goal (action_server : access rcl_action_server_t; goal_info : access constant rcl_action_types_h.rcl_action_goal_info_t) return access rcl_action_goal_handle_h.rcl_action_goal_handle_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_accept_new_goal";

  --/ Publish a ROS feedback message for an active goal using an action server.
  --*
  -- * The caller is responsible for ensuring that the type of `ros_feedback`
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * This function acts like a ROS publisher and is potentially a blocking call.
  -- * \see rcl_publish()
  -- *
  -- * This function is thread safe so long as access to both the action server and
  -- * `ros_feedback` is synchronized.
  -- * That means that calling rcl_action_publish_feedback() from multiple threads
  -- * is allowed, but calling rcl_action_publish_feedback() at the same time as
  -- * non-thread safe action server functions is not, e.g. calling
  -- * rcl_action_publish_feedback() and rcl_action_server_fini() concurrently is not
  -- * allowed.
  -- *
  -- * Before calling rcl_action_publish_feedback() the `ros_feedback` message ca
  -- * change and after calling rcl_action_publish_feedback() the `ros_feedback` message
  -- * can change, but it cannot be changed during the publish call.
  -- * The same `ros_feedback` can be passed to multiple calls of
  -- * rcl_action_publish_feedback() simultaneously, even if the action servers differ.
  -- * `ros_feedback` is unmodified by rcl_action_publish_feedback().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes [1]
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] for unique pairs of action servers and feedback, see above for more</i>
  -- *
  -- * \param[in] action_server handle to the action server that will publish the feedback
  -- * \param[in] ros_feedback a ROS message containing the goal feedback
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs. *
  --  

   function rcl_action_publish_feedback (action_server : access constant rcl_action_server_t; ros_feedback : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:439
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_publish_feedback";

  --/ Get a status array message for accepted goals associated with an action server.
  --*
  -- * The provided `status_message` should be zero-initialized with
  -- * rcl_action_get_zero_initialized_goal_status_array() before calling this function.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will publish the status message
  -- * \param[out] status_message an action_msgs/StatusArray ROS message
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_get_goal_status_array (action_server : access constant rcl_action_server_t; status_message : access rcl_action_types_h.rcl_action_goal_status_array_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:466
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_goal_status_array";

  --/ Publish a status array message for accepted goals associated with an action server.
  --*
  -- * This function acts like a ROS publisher and is potentially a blocking call.
  -- * \see rcl_publish()
  -- *
  -- * A status array message associated with the action server can be created with
  -- * rcl_action_get_goal_status_array().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will publish the status message
  -- * \param[in] status_message an action_msgs/StatusArray ROS message to publish
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_publish_status (action_server : access constant rcl_action_server_t; status_message : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:496
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_publish_status";

  --/ Take a pending result request using an action server.
  --*
  -- * \todo TODO(jacobperron) blocking of take?
  -- *
  -- * \todo TODO(jacobperron) pre-, during-, and post-conditions for message ownership?
  -- *
  -- * \todo TODO(jacobperron) is this thread-safe?
  -- *
  -- * The caller is responsible for ensuring that the type of `ros_result_request`
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will take the result request
  -- * \param[out] request_header pointer to the result request header
  -- * \param[out] ros_result_request a preallocated ROS result request message where the
  -- *   request is copied.
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_TAKE_FAILED` if take failed but no error occurred
  -- *         in the middleware, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_take_result_request
     (action_server : access constant rcl_action_server_t;
      request_header : access rmw_types_h.rmw_request_id_t;
      ros_result_request : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:535
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_take_result_request";

  --/ Send a result response using an action server.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * The caller is responsible for ensuring that the type of `ros_result_response`
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * Before calling this function, the caller should use rcl_action_update_goal_state()
  -- * to update the goals state to the appropriate terminal state.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will send the result response
  -- * \param[in] response_header pointer to the result response header
  -- * \param[in] ros_result_response a ROS result response message to send
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_send_result_response
     (action_server : access constant rcl_action_server_t;
      response_header : access rmw_types_h.rmw_request_id_t;
      ros_result_response : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:571
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_send_result_response";

  --/ Expires goals associated with an action server.
  --*
  -- * A goal is 'expired' if it has been in a terminal state (has a result) for longer
  -- * than some duration.
  -- * The timeout duration is set as part of the action server options.
  -- *
  -- * If a negative timeout value if provided, then goal results never expire (kept forever).
  -- * If a timeout of zero is set, then goal results are discarded immediately (ie. goal
  -- * results are discarded whenever this function is called).
  -- *
  -- * Expired goals are removed from the internal array of goal handles.
  -- * rcl_action_server_goal_exists() will return false for any goals that have expired.
  -- *
  -- * \attention If one or more goals are expired then a previously returned goal handle
  -- * array from rcl_action_server_get_goal_handles() becomes invalid.
  -- *
  -- * `expired_goals`, `expired_goals_capacity` and `num_expired` are optional arguments.
  -- * If set to (`NULL`, 0u, `NULL`) then they are not used.
  -- * To use them allocate an array with size equal to the maximum number of goals that you want to
  -- * expire.
  -- * Pass the number of goals the array can hold in as `expired_goals_capacity`.
  -- * This function will set `num_expired` to the number of goals that were expired.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Maybe[1]
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- * <i>[1] if one or more goals expires, then the internal goal handle array may be
  -- * resized or deallocated</i>
  -- *
  -- * \param[in] action_server handle to the action server from which expired goals
  -- *   will be cleared.
  -- * \param[inout] expired_goals the identifiers of goals that expired, or set to `NULL` if unused
  -- * \param[inout] expired_goals_capacity the allocated size of `expired_goals`, or 0 if unused
  -- * \param[out] num_expired the number of expired goals, or set to `NULL` if unused
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_expire_goals
     (action_server : access constant rcl_action_server_t;
      expired_goals : access rcl_action_types_h.rcl_action_goal_info_t;
      expired_goals_capacity : stddef_h.size_t;
      num_expired : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:623
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_expire_goals";

  --/ Notifies action server that a goal handle reached a terminal state.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server
  -- * \return `RCL_RET_OK` if everything is ok, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_notify_goal_done (action_server : access constant rcl_action_server_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:648
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_notify_goal_done";

  --/ Take a pending cancel request using an action server.
  --*
  -- * \todo TODO(jacobperron) blocking of take?
  -- *
  -- * \todo TODO(jacobperron) pre-, during-, and post-conditions for message ownership?
  -- *
  -- * \todo TODO(jacobperron) is this thread-safe?
  -- *
  -- * The caller is responsible for ensuring that the type of `ros_cancel_request`_
  -- * and the type associate with the client (via the type support) match.
  -- * Passing a different type produces undefined behavior and cannot be checked
  -- * by this function and therefore no deliberate error will occur.
  -- *
  -- * After receiving a successful cancel request, the appropriate goals can be
  -- * transitioned to the state CANCELING using rcl_action_process_cancel_request().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will take the cancel request
  -- * \param[out] request_header pointer to the cancel request header
  -- * \param[out] ros_cancel_request a preallocated ROS cancel request where the request
  -- *   message is copied
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_TAKE_FAILED` if take failed but no error occurred
  -- *         in the middleware, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_take_cancel_request
     (action_server : access constant rcl_action_server_t;
      request_header : access rmw_types_h.rmw_request_id_t;
      ros_cancel_request : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:689
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_take_cancel_request";

  --/ Process a cancel request using an action server.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * This function will compute a list of goals that a cancelation request is attempting to cancel.
  -- * It does not change the state of any goal.
  -- * The following cancel policy applies based on the goal ID and the timestamp
  -- * contained in the cancel request:
  -- *
  -- * - If the goal ID is zero and timestamp is zero, cancel all goals.
  -- * - If the goal ID is zero and timestamp is not zero, cancel all goals accepted
  -- *   at or before the timestamp.
  -- * - If the goal ID is not zero and timestamp is zero, cancel the goal with the
  -- *   given ID regardless of the time it was accepted.
  -- * - If the goal ID is not zero and timestamp is not zero, cancel the goal with the
  -- *   given ID and all goals accepted at or before the timestamp.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will process the cancel request
  -- * \param[in] cancel_request a C-typed ROS cancel request to process
  -- * \param[out] cancel_response a zero-initialized cancel response struct
  -- *   where the goal info of goals which should be cancelled are copied
  -- * \return `RCL_RET_OK` if the response was sent successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_process_cancel_request
     (action_server : access constant rcl_action_server_t;
      cancel_request : access constant rcl_action_types_h.rcl_action_cancel_request_t;
      cancel_response : access rcl_action_types_h.rcl_action_cancel_response_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:732
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_process_cancel_request";

  --/ Send a cancel response using an action server.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server that will send the cancel response
  -- * \param[in] response_header pointer to the cancel response header
  -- * \param[in] ros_cancel_response a ROS cancel response to send
  -- * \return `RCL_RET_OK` if the request was taken, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_send_cancel_response
     (action_server : access constant rcl_action_server_t;
      response_header : access rmw_types_h.rmw_request_id_t;
      ros_cancel_response : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:760
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_send_cancel_response";

  --/ Get the action name for an action server.
  --*
  -- * This function returns the action server's internal topic name string.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - action server is `NULL`
  -- *   - action server is invalid (e.g. never called init or called fini)
  -- *
  -- * The returned string is only valid as long as the action server is valid.
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
  -- * \param[in] action_server pointer to the action server
  -- * \return name string if successful, or
  -- * \return `NULL` otherwise.
  --  

   function rcl_action_server_get_action_name (action_server : access constant rcl_action_server_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl_action/action_server.h:791
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_get_action_name";

  --/ Return the rcl_action_server_options_t for an action server.
  --*
  -- * This function returns the action server's internal options struct.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - action server is `NULL`
  -- *   - action server is invalid (e.g. never called init or called fini)
  -- *
  -- * The returned struct is only valid as long as the action server is valid.
  -- * The values in the struct may change if the action server's options change,
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
  -- * \param[in] action_server handle to the action server
  -- * \return options struct if successful, or
  -- * \return `NULL` otherwise.
  --  

   function rcl_action_server_get_options (action_server : access constant rcl_action_server_t) return access constant rcl_action_server_options_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:819
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_get_options";

  --/ Get the goal handles for all goals an action server is tracking.
  --*
  -- * A pointer to the internally held array of pointers to goal handle structs is returned
  -- * along with the number of items in the array.
  -- *
  -- * The returned handle is made invalid if the action server is finalized, if
  -- * rcl_shutdown() is called, or if rcl_action_expire_goals() is called and one or more
  -- * goals are expired.
  -- * The returned handle is not guaranteed to be valid for the life time of the
  -- * action server as it may be finalized and recreated itself.
  -- * Therefore, it is recommended to get the handle from the action server using
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
  -- * \param[in] action_server handle to the action server
  -- * \param[out] goal_handles is set to the array of pointers to goal handles if successful.
  -- * \param[out] num_goals is set to the number of goals in the returned array if successful,
  -- *   not set otherwise.
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_server_get_goal_handles
     (action_server : access constant rcl_action_server_t;
      goal_handles : System.Address;
      num_goals : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/action_server.h:855
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_get_goal_handles";

  --/ Check if a goal is already being tracked by an action server.
  --*
  -- * Checks whether or not a goal is being tracked in the internal goal array.
  -- * The goal state has no effect on the return value.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server handle to the action server
  -- * \param[in] goal_info handle to a struct containing the goal ID to check for
  -- * \return `true` if `action_server` is currently tracking a goal with the provided goal ID, or
  -- * \return `false` otherwise.
  --  

   function rcl_action_server_goal_exists (action_server : access constant rcl_action_server_t; goal_info : access constant rcl_action_types_h.rcl_action_goal_info_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/action_server.h:881
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_goal_exists";

  --/ Check if an action server is valid.
  --*
  -- * In the case where `false` is returned (ie. the action server is invalid),
  -- * an error message is set.
  -- *
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
  -- * \param[in] action_server handle to the action server
  -- * \return `true` if `action_server` is valid, or
  -- * \return `false` otherwise.
  --  

   function rcl_action_server_is_valid (action_server : access constant rcl_action_server_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/action_server.h:907
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_is_valid";

  --/ Check if an action server is valid without erroring if the library is shutting down.
  --*
  -- * In the case where `false` is returned (ie. the action server is invalid),
  -- * an error message is set.
  -- *
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
  -- * \param[in] action_server handle to the action server
  -- * \return `true` if `action_server` is valid, or
  -- * \return `false` otherwise.
  --  

   function rcl_action_server_is_valid_except_context (action_server : access constant rcl_action_server_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/action_server.h:931
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_is_valid_except_context";

end rcl_action_action_server_h;
