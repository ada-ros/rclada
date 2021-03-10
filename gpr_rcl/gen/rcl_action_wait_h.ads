pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with rcl_wait_h;
limited with rcl_action_action_client_h;
with stddef_h;
with rcl_types_h;
limited with rcl_action_action_server_h;
with Interfaces.C.Extensions;

package rcl_action_wait_h is

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
  --/ Add a rcl_action_client_t to a wait set.
  --*
  -- * This function will add the underlying service clients and subscribers to the wait set.
  -- *
  -- * This function behaves similar to adding subscriptions to the wait set, but will add
  -- * five entities:
  -- *
  -- * - Three service clients
  -- * - Two subscriptions
  -- *
  -- * \see rcl_wait_set_add_subscription
  -- *
  -- * If this function fails for any reason, `client_index` and `subscription_index` are not set.
  -- * It is also possible the provided wait set is left in an inconsistent state (e.g. some
  -- * of the clients and subscriptions were added to the wait set, but not all).
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] wait_set struct where action client service client and subscription
  -- *   are to be stored
  -- * \param[in] action_client the action client to be added to the wait set
  -- * \param[out] client_index the starting index in the wait set's client container where
  -- *   the action clients underlying service clients were added. Optionally, set to `NULL`
  -- *   if ignored.
  -- * \param[out] subscription_index the starting index in the wait set's subscription container
  -- *   where the action clients underlying subscriptions were added. Optionally, set to `NULL`
  -- *   if ignored.
  -- * \return `RCL_RET_OK` if added successfully, or
  -- * \return `RCL_RET_WAIT_SET_INVALID` if the wait set is zero initialized, or
  -- * \return `RCL_RET_WAIT_SET_FULL` if the subscription set is full, or
  -- * \return `RCL_RET_ACTION_CLIENT_INVALID` if the action client is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_wait_set_add_action_client
     (wait_set : access rcl_wait_h.rcl_wait_set_t;
      action_client : access constant rcl_action_action_client_h.rcl_action_client_t;
      client_index : access stddef_h.size_t;
      subscription_index : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_wait_set_add_action_client";

  --/ Add a rcl_action_server_t to a wait set.
  --*
  -- * This function will add the underlying services to the wait set.
  -- *
  -- * This function behaves similar to adding services to the wait set, but will add
  -- * three services.
  -- *
  -- * \see rcl_wait_set_add_service
  -- *
  -- * * If this function fails for any reason, `service_index` is not set.
  -- * It is also possible the provided wait set is left in an inconsistent state (e.g. some
  -- * of the clients and subscribers were added to the wait set, but not all).
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] wait_set struct where action server services are to be stored
  -- * \param[in] action_server the action server to be added to the wait set
  -- * \param[out] service_index the starting index in the wait set's service container where
  -- *   the action servers underlying services were added. Optionally, set to `NULL`
  -- *   if ignored.
  -- * \return `RCL_RET_OK` if added successfully, or
  -- * \return `RCL_RET_WAIT_SET_INVALID` if the wait set is zero initialized, or
  -- * \return `RCL_RET_WAIT_SET_FULL` if the subscription set is full, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_wait_set_add_action_server
     (wait_set : access rcl_wait_h.rcl_wait_set_t;
      action_server : access constant rcl_action_action_server_h.rcl_action_server_t;
      service_index : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_wait_set_add_action_server";

  --/ Get the number of wait set entities associated with a rcl_action_client_t.
  --*
  -- * Returns the number of entities that are added to the wait set if
  -- * rcl_action_wait_set_add_action_client() is called.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_client an action client to query
  -- * \param[out] num_subscriptions the number of subscriptions added when the action client
  -- *   is added to the wait set
  -- * \param[out] num_guard_conditions the number of guard conditions added when the action client
  -- *   is added to the wait set
  -- * \param[out] num_timers the number of timers added when the action client
  -- *   is added to the wait set
  -- * \param[out] num_clients the number of clients added when the action client
  -- *   is added to the wait set
  -- * \param[out] num_services the number of services added when the action client
  -- *   is added to the wait set
  -- * \return `RCL_RET_OK` if call is successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_CLIENT_INVALID` if the action client is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_client_wait_set_get_num_entities
     (action_client : access constant rcl_action_action_client_h.rcl_action_client_t;
      num_subscriptions : access stddef_h.size_t;
      num_guard_conditions : access stddef_h.size_t;
      num_timers : access stddef_h.size_t;
      num_clients : access stddef_h.size_t;
      num_services : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:148
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_client_wait_set_get_num_entities";

  --/ Get the number of wait set entities associated with a rcl_action_server_t.
  --*
  -- * Returns the number of entities that are added to the wait set if
  -- * rcl_action_wait_set_add_action_server() is called.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] action_server an action server to query
  -- * \param[out] num_subscriptions the number of subscriptions added when the action server
  -- *   is added to the wait set
  -- * \param[out] num_guard_conditions the number of guard conditions added when the action server
  -- *   is added to the wait set
  -- * \param[out] num_timers the number of timers added when the action server
  -- *   is added to the wait set
  -- * \param[out] num_clients the number of clients added when the action server
  -- *   is added to the wait set
  -- * \param[out] num_services the number of services added when the action server
  -- *   is added to the wait set
  -- * \return `RCL_RET_OK` if call is successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_SERVER_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_server_wait_set_get_num_entities
     (action_server : access constant rcl_action_action_server_h.rcl_action_server_t;
      num_subscriptions : access stddef_h.size_t;
      num_guard_conditions : access stddef_h.size_t;
      num_timers : access stddef_h.size_t;
      num_clients : access stddef_h.size_t;
      num_services : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:188
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_wait_set_get_num_entities";

  --/ Get the wait set entities that are ready for a rcl_action_client_t.
  --*
  -- * The caller can use this function to determine the relevant action client functions
  -- * to call: rcl_action_take_feedback(), rcl_action_take_status(),
  -- * rcl_action_take_goal_response(), rcl_action_take_cancel_response(), or
  -- * rcl_action_take_result_response().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] wait_set struct where action server services are to be stored
  -- * \param[in] action_client an action client to query
  -- * \param[out] is_feedback_ready `true` if there is a feedback message ready to take,
  -- *   `false` otherwise
  -- * \param[out] is_status_ready `true` if there is a status message ready to take,
  -- *   `false` otherwise
  -- * \param[out] is_goal_response_ready `true` if there is a goal response message ready
  -- *   to take, `false` otherwise
  -- * \param[out] is_cancel_response_ready `true` if there is a cancel response message ready
  -- *   to take, `false` otherwise
  -- * \param[out] is_result_response_ready `true` if there is a result response message ready
  -- *   to take, `false` otherwise
  -- * \return `RCL_RET_OK` if call is successful, or
  -- * \return `RCL_RET_WAIT_SET_INVALID` if the wait set is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_CLIENT_INVALID` if the action client is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_client_wait_set_get_entities_ready
     (wait_set : access constant rcl_wait_h.rcl_wait_set_t;
      action_client : access constant rcl_action_action_client_h.rcl_action_client_t;
      is_feedback_ready : access Extensions.bool;
      is_status_ready : access Extensions.bool;
      is_goal_response_ready : access Extensions.bool;
      is_cancel_response_ready : access Extensions.bool;
      is_result_response_ready : access Extensions.bool) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:232
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_client_wait_set_get_entities_ready";

  --/ Get the wait set entities that are ready for a rcl_action_server_t.
  --*
  -- * The caller can use this function to determine the relevant action server functions
  -- * to call: rcl_action_take_goal_request(), rcl_action_take_cancel_request(), or
  -- * rcl_action_take_result_request().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] wait_set struct where action server services are to be stored
  -- * \param[in] action_server an action server to query
  -- * \param[out] is_goal_request_ready `true` if there is a goal request message ready
  -- *   to take, `false` otherwise
  -- * \param[out] is_cancel_request_ready `true` if there is a cancel request message ready
  -- *   to take, `false` otherwise
  -- * \param[out] is_result_request_ready `true` if there is a result request message ready
  -- *   to take, `false` otherwise
  -- * \param[out] is_goal_expired `true` if there is a goal that expired, `false` otherwise
  -- * \return `RCL_RET_OK` if call is successful, or
  -- * \return `RCL_RET_WAIT_SET_INVALID` if the wait set is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ACTION_CLIENT_INVALID` if the action server is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  -- 

   function rcl_action_server_wait_set_get_entities_ready
     (wait_set : access constant rcl_wait_h.rcl_wait_set_t;
      action_server : access constant rcl_action_action_server_h.rcl_action_server_t;
      is_goal_request_ready : access Extensions.bool;
      is_cancel_request_ready : access Extensions.bool;
      is_result_request_ready : access Extensions.bool;
      is_goal_expired : access Extensions.bool) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/wait.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_server_wait_set_get_entities_ready";

end rcl_action_wait_h;
