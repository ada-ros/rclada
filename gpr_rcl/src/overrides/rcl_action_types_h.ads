pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with action_msgs_msg_detail_goal_info_ustruct_h;
with action_msgs_msg_detail_goal_status_ustruct_h;
with action_msgs_msg_detail_goal_status_array_ustruct_h;
with rcl_allocator_h;
with action_msgs_srv_detail_cancel_goal_ustruct_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with Interfaces.C.Strings;
with stddef_h;
with rcl_types_h;

package rcl_action_types_h is

   RCL_RET_ACTION_NAME_INVALID : constant := 2000;  --  /opt/ros/foxy/include/rcl_action/types.h:38

   RCL_RET_ACTION_GOAL_ACCEPTED : constant := 2100;  --  /opt/ros/foxy/include/rcl_action/types.h:40

   RCL_RET_ACTION_GOAL_REJECTED : constant := 2101;  --  /opt/ros/foxy/include/rcl_action/types.h:42

   RCL_RET_ACTION_CLIENT_INVALID : constant := 2102;  --  /opt/ros/foxy/include/rcl_action/types.h:44

   RCL_RET_ACTION_CLIENT_TAKE_FAILED : constant := 2103;  --  /opt/ros/foxy/include/rcl_action/types.h:46

   RCL_RET_ACTION_SERVER_INVALID : constant := 2200;  --  /opt/ros/foxy/include/rcl_action/types.h:48

   RCL_RET_ACTION_SERVER_TAKE_FAILED : constant := 2201;  --  /opt/ros/foxy/include/rcl_action/types.h:50

   RCL_RET_ACTION_GOAL_HANDLE_INVALID : constant := 2300;  --  /opt/ros/foxy/include/rcl_action/types.h:52

   RCL_RET_ACTION_GOAL_EVENT_INVALID : constant := 2301;  --  /opt/ros/foxy/include/rcl_action/types.h:54

   UUID_SIZE : constant := 16;  --  /opt/ros/foxy/include/rcl_action/types.h:57
   --  arg-macro: function uuidcmp (uuid0, uuid1)
   --    return 0 = memcmp(uuid0, uuid1, UUID_SIZE);
   --  unsupported macro: zerouuid (uint8_t[UUID_SIZE]) {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
   --  arg-macro: procedure uuidcmpzero (uuid)
   --    uuidcmp(uuid, (zerouuid))
   --  unsupported macro: GOAL_STATE_UNKNOWN action_msgs__msg__GoalStatus__STATUS_UNKNOWN
   --  unsupported macro: GOAL_STATE_ACCEPTED action_msgs__msg__GoalStatus__STATUS_ACCEPTED
   --  unsupported macro: GOAL_STATE_EXECUTING action_msgs__msg__GoalStatus__STATUS_EXECUTING
   --  unsupported macro: GOAL_STATE_CANCELING action_msgs__msg__GoalStatus__STATUS_CANCELING
   --  unsupported macro: GOAL_STATE_SUCCEEDED action_msgs__msg__GoalStatus__STATUS_SUCCEEDED
   --  unsupported macro: GOAL_STATE_CANCELED action_msgs__msg__GoalStatus__STATUS_CANCELED
   --  unsupported macro: GOAL_STATE_ABORTED action_msgs__msg__GoalStatus__STATUS_ABORTED

   GOAL_STATE_NUM_STATES : constant := 7;  --  /opt/ros/foxy/include/rcl_action/types.h:98

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
  -- rcl action specific ret codes in 2XXX
  --/ Action name does not pass validation return code.
  --/ Action goal accepted return code.
  --/ Action goal rejected return code.
  --/ Action client is invalid return code.
  --/ Action client failed to take response return code.
  --/ Action server is invalid return code.
  --/ Action server failed to take request return code.
  --/ Action goal handle invalid return code.
  --/ Action invalid event return code.
  -- TODO(jacobperron): Move these to a common place for UUIDs
  -- Forward declare
   type rcl_action_server_t is null record;   -- incomplete struct

  -- Typedef generated messages for convenience
  -- JANO OVERRIDE: make subtype a type so it is visible elsewhere (gnat bug)
   type rcl_action_goal_info_t is new action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/foxy/include/rcl_action/types.h:66

   subtype rcl_action_goal_status_t is action_msgs_msg_detail_goal_status_ustruct_h.action_msgs_u_msg_u_GoalStatus;  -- /opt/ros/foxy/include/rcl_action/types.h:67

  --/ Struct with the action goal status array
  --/ Goal status array message
   type rcl_action_goal_status_array_t is record
      msg : aliased action_msgs_msg_detail_goal_status_array_ustruct_h.action_msgs_u_msg_u_GoalStatusArray;  -- /opt/ros/foxy/include/rcl_action/types.h:72
      allocator : aliased rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/foxy/include/rcl_action/types.h:74
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl_action/types.h:69

  --/ Allocator used to initialize this struct.
  -- JANO OVERRIDE: make subtype a type so it is visible elsewhere (gnat bug)
   type rcl_action_cancel_request_t is new action_msgs_srv_detail_cancel_goal_ustruct_h.action_msgs_u_srv_u_CancelGoal_Request;  -- /opt/ros/foxy/include/rcl_action/types.h:76

  --/ Struct with the action cancel response
  --/ Cancel goal response message
   type rcl_action_cancel_response_t is record
      msg : aliased action_msgs_srv_detail_cancel_goal_ustruct_h.action_msgs_u_srv_u_CancelGoal_Response;  -- /opt/ros/foxy/include/rcl_action/types.h:81
      allocator : aliased rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/foxy/include/rcl_action/types.h:83
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl_action/types.h:78

  --/ Allocator used to initialize this struct.
  --/ Goal states
  -- TODO(jacobperron): Let states be defined by action_msgs/msg/goal_status.h
  -- Ideally, we could use an enum type directly from the message when the feature
  -- is available. Issue: https://github.com/ros2/rosidl/issues/260
   subtype rcl_action_goal_state_t is x86_64_linux_gnu_bits_stdint_intn_h.int8_t;  -- /opt/ros/foxy/include/rcl_action/types.h:90

  --/ User friendly error messages for invalid trasntions
  -- Description variables in types.c should be changed if enum values change
   goal_state_descriptions : array (size_t) of Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl_action/types.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "goal_state_descriptions";

   goal_event_descriptions : array (size_t) of Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl_action/types.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "goal_event_descriptions";

  --/ Goal state transition events
   type rcl_action_goal_event_t is 
     (GOAL_EVENT_EXECUTE,
      GOAL_EVENT_CANCEL_GOAL,
      GOAL_EVENT_SUCCEED,
      GOAL_EVENT_ABORT,
      GOAL_EVENT_CANCELED,
      GOAL_EVENT_NUM_EVENTS)
   with Convention => C;  -- /opt/ros/foxy/include/rcl_action/types.h:106

  --/ Return a rcl_action_goal_info_t with members set to zero values.
   function rcl_action_get_zero_initialized_goal_info return rcl_action_goal_info_t  -- /opt/ros/foxy/include/rcl_action/types.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_goal_info";

  --/ Return a rcl_action_goal_status_array_t with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_action_goal_status_array_t before passing to
  -- * rcl_action_server_get_goal_status_array().
  --  

   function rcl_action_get_zero_initialized_goal_status_array return rcl_action_goal_status_array_t  -- /opt/ros/foxy/include/rcl_action/types.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_goal_status_array";

  --/ Return a rcl_action_cancel_request_t with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_action_cancel_request_t before passing to
  -- *
  -- * rcl_action_cancel_request_init().
  --  

   function rcl_action_get_zero_initialized_cancel_request return rcl_action_cancel_request_t  -- /opt/ros/foxy/include/rcl_action/types.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_cancel_request";

  --/ Return a rcl_action_cancel_response_t with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_action_cancel_response_t before passing to
  -- * rcl_action_cancel_response_init().
  --  

   function rcl_action_get_zero_initialized_cancel_response return rcl_action_cancel_response_t  -- /opt/ros/foxy/include/rcl_action/types.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_cancel_response";

  --/ Initialize a rcl_action_goal_status_array_t.
  --*
  -- * After calling this function on a rcl_action_goal_status_array_t, it can be populated
  -- * and used to get and send status array messages with an action server using
  -- * rcl_action_get_goal_status_array() and rcl_action_publish_status() respectively.
  -- *
  -- * Example usage:
  -- *
  -- * ```c
  -- * #include <rcl/rcl.h>
  -- * #include <rcl_action/rcl_action.h>
  -- *
  -- * rcl_action_goal_status_array_t goal_status_array =
  -- *   rcl_action_get_zero_initialized_goal_status_array();
  -- * size_t num_status = 42;
  -- * ret = rcl_action_goal_status_array_init(
  -- *   &goal_status_array,
  -- *   num_status,
  -- *   rcl_get_default_allocator());
  -- * // ... error handling, and when done with message, finalize
  -- * ret = rcl_action_goal_status_array_fini(&goal_status_array, rcl_get_default_allocator());
  -- * // ... error handling
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
  -- * \param[out] status_array a preallocated, zero-initialized, goal status array message
  -- *   to be initialized.
  -- * \param[in] num_status the number of status messages to allocate space for.
  -- *   Must be greater than zero
  -- * \param[in] allocator a valid allocator
  -- * \return `RCL_RET_OK` if cancel response was initialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ALREADY_INIT` if the status array has already been initialized, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_goal_status_array_init
     (status_array : access rcl_action_goal_status_array_t;
      num_status : stddef_h.size_t;
      allocator : rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/types.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_status_array_init";

  --/ Finalize a rcl_action_goal_status_array_t.
  --*
  -- * After calling, the goal status array message will no longer be valid.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] status_array the goal status array message to be deinitialized
  -- * \return `RCL_RET_OK` if the goal status array was deinitialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_goal_status_array_fini (status_array : access rcl_action_goal_status_array_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/types.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_status_array_fini";

  --/ Initialize a rcl_action_cancel_response_t.
  --*
  -- * After calling this function on a rcl_action_cancel_response_t, it can be populated
  -- * and used to process cancel requests with an action server using
  -- * rcl_action_process_cancel_request().
  -- *
  -- * Example usage:
  -- *
  -- * ```c
  -- * #include <rcl/rcl.h>
  -- * #include <rcl_action/rcl_action.h>
  -- *
  -- * rcl_action_cancel_response_t cancel_response =
  -- *   rcl_action_get_zero_initialized_cancel_response();
  -- * size_t num_goals_canceling = 10;
  -- * ret = rcl_action_cancel_response_init(
  -- *   &cancel_response,
  -- *   num_goals_canceling,
  -- *   rcl_get_default_allocator());
  -- * // ... error handling, and when done processing response, finalize
  -- * ret = rcl_action_cancel_response_fini(&cancel_response, rcl_get_default_allocator());
  -- * // ... error handling
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
  -- * \param[out] cancel_response a preallocated, zero-initialized, cancel response message
  -- *   to be initialized.
  -- * \param[in] num_goals_canceling the number of goals that are canceling to add to the response
  -- *   Must be greater than zero
  -- * \param[in] allocator a valid allocator
  -- * \return `RCL_RET_OK` if cancel response was initialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ALREADY_INIT` if the cancel response has already been initialized, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_cancel_response_init
     (cancel_response : access rcl_action_cancel_response_t;
      num_goals_canceling : stddef_h.size_t;
      allocator : rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/types.h:272
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_cancel_response_init";

  --/ Finalize a rcl_action_cancel_response_t.
  --*
  -- * After calling, the cancel response message will no longer be valid.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] cancel_response the cancel response message to be deinitialized
  -- * \return `RCL_RET_OK` if the cancel response was deinitialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_action_cancel_response_fini (cancel_response : access rcl_action_cancel_response_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/types.h:297
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_cancel_response_fini";

end rcl_action_types_h;
