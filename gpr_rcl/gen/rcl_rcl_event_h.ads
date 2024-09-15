pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
limited with rcl_rcl_publisher_h;
with rcl_rcl_types_h;
limited with rcl_rcl_subscription_h;
with System;
limited with rmw_rmw_event_h;
with Interfaces.C.Extensions;
with rcl_rcl_event_callback_h;

package rcl_rcl_event_h is

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
  --/ @file
  --/ Enumeration of all of the publisher events that may fire.
   type rcl_publisher_event_type_e is 
     (RCL_PUBLISHER_OFFERED_DEADLINE_MISSED,
      RCL_PUBLISHER_LIVELINESS_LOST,
      RCL_PUBLISHER_OFFERED_INCOMPATIBLE_QOS,
      RCL_PUBLISHER_INCOMPATIBLE_TYPE,
      RCL_PUBLISHER_MATCHED)
   with Convention => C;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:36

   subtype rcl_publisher_event_type_t is rcl_publisher_event_type_e;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:43

  --/ Enumeration of all of the subscription events that may fire.
   type rcl_subscription_event_type_e is 
     (RCL_SUBSCRIPTION_REQUESTED_DEADLINE_MISSED,
      RCL_SUBSCRIPTION_LIVELINESS_CHANGED,
      RCL_SUBSCRIPTION_REQUESTED_INCOMPATIBLE_QOS,
      RCL_SUBSCRIPTION_MESSAGE_LOST,
      RCL_SUBSCRIPTION_INCOMPATIBLE_TYPE,
      RCL_SUBSCRIPTION_MATCHED)
   with Convention => C;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:46

   subtype rcl_subscription_event_type_t is rcl_subscription_event_type_e;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:54

  --/ Internal rcl implementation struct.
   type rcl_event_impl_s is null record;   -- incomplete struct

   subtype rcl_event_impl_t is rcl_event_impl_s;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:57

  --/ Structure which encapsulates a ROS QoS event handle.
  --/ Pointer to the event implementation
   type rcl_event_s is record
      impl : access rcl_event_impl_t;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:63
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:60

   subtype rcl_event_t is rcl_event_s;  -- /opt/ros/jazzy/include/rcl/rcl/event.h:64

  --/ Return a rcl_event_t struct with members set to `NULL`.
  --*
  -- * Should be called to get a null rcl_event_t before passing to
  -- * rcl_event_init().
  -- *
  -- * \return Zero initialized rcl_event_t.
  --  

   function rcl_get_zero_initialized_event return rcl_event_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_event";

  --/ Initialize an rcl_event_t with a publisher.
  --*
  -- * Fill the rcl_event_t with the publisher and desired event_type.
  -- *
  -- * \param[in,out] event pointer to fill
  -- * \param[in] publisher to get events from
  -- * \param[in] event_type to listen for
  -- * \return #RCL_RET_OK if the rcl_event_t is filled, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory fails, or
  -- * \return #RCL_RET_UNSUPPORTED if event_type is not supported, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_publisher_event_init
     (event : access rcl_event_t;
      publisher : access constant rcl_rcl_publisher_h.rcl_publisher_s;
      event_type : rcl_publisher_event_type_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:94
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_publisher_event_init";

  --/ Initialize an rcl_event_t with a subscription.
  --*
  -- * Fill the rcl_event_t with the subscription and desired event_type.
  -- *
  -- * \param[in,out] event pointer to fill
  -- * \param[in] subscription to get events from
  -- * \param[in] event_type to listen for
  -- * \return #RCL_RET_OK if the rcl_event_t is filled, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory fails, or
  -- * \return #RCL_RET_UNSUPPORTED if event_type is not supported, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_subscription_event_init
     (event : access rcl_event_t;
      subscription : access constant rcl_rcl_subscription_h.rcl_subscription_s;
      event_type : rcl_subscription_event_type_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_subscription_event_init";

  -- Take event using the event handle.
  --*
  -- * Take an event from the event handle.
  -- *
  -- * \param[in] event event object to take from
  -- * \param[in, out] event_info event info object to write taken data into
  -- * \return #RCL_RET_OK if successful, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if memory allocation failed, or
  -- * \return #RCL_RET_EVENT_TAKE_FAILED if the take event failed, or
  -- * \return #RCL_RET_ERROR if an unexpected error occurs.
  --  

   function rcl_take_event (event : access constant rcl_event_t; event_info : System.Address) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:135
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_take_event";

  -- Finalize an event.
  --*
  -- * Finalize an event.
  -- *
  -- * \param[in] event to finalize
  -- * \return #RCL_RET_OK if successful, or
  -- * \return #RCL_RET_EVENT_INVALID if event is null, or
  -- * \return #RCL_RET_ERROR if an unexpected error occurs.
  --  

   function rcl_event_fini (event : access rcl_event_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_event_fini";

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

   function rcl_event_get_rmw_handle (event : access constant rcl_event_t) return access rmw_rmw_event_h.rmw_event_s  -- /opt/ros/jazzy/include/rcl/rcl/event.h:182
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_event_get_rmw_handle";

  --/ Check that the event is valid.
  --*
  -- * The bool returned is `false` if `event` is invalid.
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
  -- * \param[in] event pointer to the rcl event
  -- * \return `true` if `event` is valid, otherwise `false`
  --  

   function rcl_event_is_valid (event : access constant rcl_event_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/event.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_event_is_valid";

  --/ Set the callback function for the event.
  --*
  -- * This API sets the callback function to be called whenever the
  -- * event is notified about a new instance of the event.
  -- *
  -- * \sa rmw_event_set_callback for more details about this function.
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
  -- * \param[in] event The event on which to set the callback
  -- * \param[in] callback The callback to be called when new events occur, may be NULL
  -- * \param[in] user_data Given to the callback when called later, may be NULL
  -- * \return `RCL_RET_OK` if callback was set to the listener, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if `event` is NULL, or
  -- * \return `RCL_RET_UNSUPPORTED` if the API is not implemented in the dds implementation
  --  

   function rcl_event_set_callback
     (event : access constant rcl_event_t;
      callback : rcl_rcl_event_callback_h.rcl_event_callback_t;
      user_data : System.Address) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/event.h:232
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_event_set_callback";

end rcl_rcl_event_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
