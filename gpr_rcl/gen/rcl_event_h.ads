pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
limited with rcl_publisher_h;
with rcl_types_h;
limited with rcl_subscription_h;

package rcl_event_h is

  -- Copyright 2019 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   type rcl_publisher_event_type_t is 
     (RCL_PUBLISHER_OFFERED_DEADLINE_MISSED,
      RCL_PUBLISHER_LIVELINESS_LOST);
   pragma Convention (C, rcl_publisher_event_type_t);  -- /opt/ros/dashing/include/rcl/event.h:30

   type rcl_subscription_event_type_t is 
     (RCL_SUBSCRIPTION_REQUESTED_DEADLINE_MISSED,
      RCL_SUBSCRIPTION_LIVELINESS_CHANGED);
   pragma Convention (C, rcl_subscription_event_type_t);  -- /opt/ros/dashing/include/rcl/event.h:36

  --/ rmw struct.
   --  skipped empty struct rmw_event_t

  --/ Internal rcl implementation struct.
   --  skipped empty struct rcl_event_impl_t

  --/ Structure which encapsulates a ROS QoS event handle.
   type rcl_event_t is record
      impl : System.Address;  -- /opt/ros/dashing/include/rcl/event.h:51
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_event_t);  -- /opt/ros/dashing/include/rcl/event.h:49

  --/ Return a rcl_event_t struct with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_event_t before passing to
  -- * rcl_event_init().
  --  

   function rcl_get_zero_initialized_event return rcl_event_t;  -- /opt/ros/dashing/include/rcl/event.h:62
   pragma Import (C, rcl_get_zero_initialized_event, "rcl_get_zero_initialized_event");

  --/ Initialize an rcl_event_t with a publisher.
  --*
  -- * Fill the rcl_event_t with the publisher and desired event_type.
  -- *
  -- * \param[in,out] event pointer to fill
  -- * \param[in] publisher to get events from
  -- * \param[in] event_type to listen for
  -- * \return `RCL_RET_OK` if the rcl_event_t is filled, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_publisher_event_init
     (event : access rcl_event_t;
      publisher : access constant rcl_publisher_h.rcl_publisher_t;
      event_type : rcl_publisher_event_type_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/event.h:78
   pragma Import (C, rcl_publisher_event_init, "rcl_publisher_event_init");

  --/ Initialize an rcl_event_t with a subscription.
  --*
  -- * Fill the rcl_event_t with the subscription and desired event_type.
  -- *
  -- * \param[in,out] event pointer to fill
  -- * \param[in] subscription to get events from
  -- * \param[in] event_type to listen for
  -- * \return `RCL_RET_OK` if the rcl_event_t is filled, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_subscription_event_init
     (event : access rcl_event_t;
      subscription : access constant rcl_subscription_h.rcl_subscription_t;
      event_type : rcl_subscription_event_type_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/event.h:97
   pragma Import (C, rcl_subscription_event_init, "rcl_subscription_event_init");

  -- Take event using the event handle.
  --*
  -- * Take an event from the event handle.
  -- *
  -- * \param[in] event_handle event object to take from
  -- * \param[in, out] event_info event info object to write taken data into
  -- * \param[in, out] taken boolean flag indicating if an event was taken or not
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_BAD_ALLOC` if memory allocation failed, or
  -- * \return `RCL_RET_ERROR` if an unexpected error occurs.
  --  

   function rcl_take_event (event : access constant rcl_event_t; event_info : System.Address) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/event.h:116
   pragma Import (C, rcl_take_event, "rcl_take_event");

  -- Finalize an event.
  --*
  -- * Finalize an event.
  -- *
  -- * \param[in] event to finalize
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_EVENT_INVALID` if event is null, or
  -- * \return `RCL_RET_ERROR` if an unexpected error occurs.
  --  

   function rcl_event_fini (event : access rcl_event_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/event.h:132
   pragma Import (C, rcl_event_fini, "rcl_event_fini");

  --/ Return the rmw event handle.
  --*
  -- * The handle returned is a pointer to the internally held rmw handle.
  -- * This function can fail, and therefore return `NULL`, if the:
  -- *   - event is `NULL`
  -- *   - event is invalid (never called init, called fini, or invalid node)
  -- *
  -- * The returned handle is made invalid if the event is finalized or if
  -- * rcl_shutdown() is called.
  -- * The returned handle is not guaranteed to be valid for the life time of the
  -- * event as it may be finalized and recreated itself.
  -- * Therefore it is recommended to get the handle from the event using
  -- * this function each time it is needed and avoid use of the handle
  -- * concurrently with functions that might change it.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] event pointer to the rcl event
  -- * \return rmw event handle if successful, otherwise `NULL`
  --  

   function rcl_event_get_rmw_handle (event : access constant rcl_event_t) return System.Address;  -- /opt/ros/dashing/include/rcl/event.h:163
   pragma Import (C, rcl_event_get_rmw_handle, "rcl_event_get_rmw_handle");

end rcl_event_h;
