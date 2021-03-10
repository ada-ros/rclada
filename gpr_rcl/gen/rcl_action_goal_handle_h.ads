pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcl_action_types_h;
with rcl_allocator_h;
with rcl_types_h;
with Interfaces.C.Extensions;

package rcl_action_goal_handle_h is

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
  --/ Internal rcl action goal implementation struct.
   type rcl_action_goal_handle_impl_t is null record;   -- incomplete struct

  --/ Goal handle for an action.
  --/ Pointer to the action goal handle implementation
   type rcl_action_goal_handle_t is record
      impl : access rcl_action_goal_handle_impl_t;  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:36
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:33

  --/ Return a rcl_action_goal_handle_t struct with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_action_goal_handle_t before passing to
  -- * rcl_action_goal_handle_init().
  --  

   function rcl_action_get_zero_initialized_goal_handle return rcl_action_goal_handle_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_get_zero_initialized_goal_handle";

  --/ Initialize a rcl_action_goal_handle_t.
  --*
  -- * After calling this function on a rcl_action_goal_handle_t, it can be used to update the
  -- * goals state with rcl_action_update_goal_state().
  -- * It can also be used to query the state of the goal with
  -- * rcl_action_goal_handle_get_message() and rcl_action_goal_handle_is_active().
  -- * Goal information can be accessed with rcl_action_goal_handle_get_message() and
  -- * rcl_action_goal_handle_get_info().
  -- *
  -- * Goal handles are typically initialized and finalized by action servers.
  -- * I.e. The allocator should be provided by the action server.
  -- * Goal handles are created with rcl_action_accept_new_goal() and destroyed with
  -- * rcl_action_clear_expired_goals() or rcl_action_server_fini().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[out] goal_handle preallocated, zero-initialized, goal handle structure
  -- *   to be initialized
  -- * \param[in] goal_info information about the goal to be copied to the goal handle
  -- * \param[in] allocator a valid allocator used to initialized the goal handle
  -- * \return `RCL_RET_OK` if goal_handle was initialized successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if the allocator is invalid, or
  -- * \return `RCL_RET_ACTION_GOAL_HANDLE_INVALID` if the goal handle is invalid, or
  -- * \return `RCL_RET_ALREADY_INIT` if the goal handle has already been initialized, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed
  --  

   function rcl_action_goal_handle_init
     (goal_handle : access rcl_action_goal_handle_t;
      goal_info : access constant rcl_action_types_h.rcl_action_goal_info_t;
      allocator : rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_init";

  --/ Finalize a rcl_action_goal_handle_t.
  --*
  -- * After calling, rcl_action_goal_handle_t will no longer be valid and
  -- * rcl_action_server_t will no longer track the goal associated with the goal handle.
  -- *
  -- * After calling, calls to rcl_action_publish_feedback(), rcl_action_publish_status(),
  -- * rcl_action_update_goal_state(), rcl_action_goal_handle_get_status(),
  -- * rcl_action_goal_handle_is_active(), rcl_action_goal_handle_get_message(), and
  -- * rcl_action_goal_handle_get_info() will fail when using this goal handle.
  -- *
  -- * However, the given action server is still valid.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] goal_handle struct to be deinitialized
  -- * \return `RCL_RET_OK` if the goal handle was deinitialized successfully, or
  -- * \return `RCL_RET_ACTION_GOAL_HANDLE_INVALID` if the goal handle is invalid, or
  --  

   function rcl_action_goal_handle_fini (goal_handle : access rcl_action_goal_handle_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:116
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_fini";

  --/ Update a goal state with a rcl_action_goal_handle_t and an event.
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
  -- * \param[inout] goal_handle struct containing goal state to transition
  -- * \param[in] goal_event the event used to transition the goal state
  -- * \return `RCL_RET_OK` if the goal state was updated successfully, or
  -- * \return `RCL_RET_ACTION_GOAL_EVENT_INVALID` if the goal event is invalid, or
  -- * \return `RCL_RET_ACTION_GOAL_HANDLE_INVALID` if the goal handle is invalid, or
  --  

   function rcl_action_update_goal_state (goal_handle : access rcl_action_goal_handle_t; goal_event : rcl_action_types_h.rcl_action_goal_event_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:139
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_update_goal_state";

  --/ Get the ID of a goal using a rcl_action_goal_handle_t.
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
  -- * \param[in] goal_handle struct containing the goal and meta
  -- * \param[out] goal_info a preallocated struct where the goal info is copied
  -- * \return `RCL_RET_OK` if the goal ID was accessed successfully, or
  -- * \return `RCL_RET_ACTION_GOAL_HANDLE_INVALID` if the goal handle is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if the goal_info argument is invalid
  --  

   function rcl_action_goal_handle_get_info (goal_handle : access constant rcl_action_goal_handle_t; goal_info : access rcl_action_types_h.rcl_action_goal_info_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:164
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_get_info";

  --/ Get the status of a goal.
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
  -- * \param[in] goal_handle struct containing the goal and metadata
  -- * \param[out] status a preallocated struct where the goal status is copied
  -- * \return `RCL_RET_OK` if the goal ID was accessed successfully, or
  -- * \return `RCL_RET_ACTION_GOAL_HANDLE_INVALID` if the goal handle is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if the status argument is invalid
  --  

   function rcl_action_goal_handle_get_status (goal_handle : access constant rcl_action_goal_handle_t; status : access rcl_action_types_h.rcl_action_goal_state_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_get_status";

  --/ Check if a goal is active using a rcl_action_goal_handle_t.
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
  -- * \param[in] goal_handle struct containing the goal and metadata
  -- * \return `true` if the goal is in one of the following states: ACCEPTED, EXECUTING, or CANCELING, or
  -- * \return `false` if the goal handle pointer is invalid, or
  -- * \return `false` otherwise
  -- 

   function rcl_action_goal_handle_is_active (goal_handle : access constant rcl_action_goal_handle_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_is_active";

  --/ Check if a goal can be transitioned to CANCELING in its current state.
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
  -- * \param[in] goal_handle struct containing the goal and metadata
  -- * \return `true` if the goal can be transitioned to CANCELING from its current state, or
  -- * \return `false` if the goal handle pointer is invalid, or
  -- * \return `false` otherwise
  -- 

   function rcl_action_goal_handle_is_cancelable (goal_handle : access constant rcl_action_goal_handle_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_is_cancelable";

  --/ Check if a rcl_action_goal_handle_t is valid.
  --*
  -- * This is a non-blocking call.
  -- *
  -- * A goal handle is invalid if:
  -- *   - the implementation is `NULL` (rcl_action_goal_handle_init() not called or failed)
  -- *   - rcl_shutdown() has been called since the goal handle has been initialized
  -- *   - the goal handle has been finalized with rcl_action_goal_handle_fini()
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] goal_handle struct to evaluate as valid or not
  -- * \return `true` if the goal handle is valid, or
  -- * \return `false` if the goal handle pointer is null, or
  -- * \return `false` otherwise
  --  

   function rcl_action_goal_handle_is_valid (goal_handle : access constant rcl_action_goal_handle_t) return Extensions.bool  -- /opt/ros/foxy/include/rcl_action/goal_handle.h:262
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_action_goal_handle_is_valid";

end rcl_action_goal_handle_h;
