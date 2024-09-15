pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
limited with rmw_rmw_types_h;
with rmw_rmw_ret_types_h;
with Interfaces.C.Extensions;

package rmw_rmw_event_h is

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
  --/ Define publisher/subscription events
   type rmw_event_type_e is 
     (RMW_EVENT_LIVELINESS_CHANGED,
      RMW_EVENT_REQUESTED_DEADLINE_MISSED,
      RMW_EVENT_REQUESTED_QOS_INCOMPATIBLE,
      RMW_EVENT_MESSAGE_LOST,
      RMW_EVENT_SUBSCRIPTION_INCOMPATIBLE_TYPE,
      RMW_EVENT_SUBSCRIPTION_MATCHED,
      RMW_EVENT_LIVELINESS_LOST,
      RMW_EVENT_OFFERED_DEADLINE_MISSED,
      RMW_EVENT_OFFERED_QOS_INCOMPATIBLE,
      RMW_EVENT_PUBLISHER_INCOMPATIBLE_TYPE,
      RMW_EVENT_PUBLICATION_MATCHED,
      RMW_EVENT_INVALID)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:33

  -- subscription events
  -- publisher events
  -- sentinel value
   subtype rmw_event_type_t is rmw_event_type_e;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:52

  --/ Encapsulate the RMW event implementation, data, and type.
  --/ Implementation identifier, used to ensure two different implementations are not being mixed.
   type rmw_event_s is record
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:58
      data : System.Address;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:60
      event_type : aliased rmw_event_type_t;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:62
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:55

  --/ Data specific to this event type from either the publisher or subscriber.
  --/ The event type that occurred.
   subtype rmw_event_t is rmw_event_s;  -- /opt/ros/jazzy/include/rmw/rmw/event.h:63

  --/ Return a zero initialized event structure.
   function rmw_get_zero_initialized_event return rmw_event_t  -- /opt/ros/jazzy/include/rmw/rmw/event.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_event";

  --/ Initialize a rmw publisher event.
  --*
  -- * \param[inout] rmw_event to initialize
  -- * \param[in] publisher to initialize with
  -- * \param[inout] event_type for the event to initialize
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if invalid argument, or
  -- * \return `RMW_RET_UNSUPPORTED` if event_type is not supported, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs.
  --  

   function rmw_publisher_event_init
     (rmw_event : access rmw_event_t;
      publisher : access constant rmw_rmw_types_h.rmw_publisher_s;
      event_type : rmw_event_type_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/event.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_publisher_event_init";

  --/ Initialize a rmw subscription event.
  --*
  -- * \param[inout] rmw_event to initialize
  -- * \param[in] subscription to initialize with
  -- * \param[inout] event_type for the event to handle
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if invalid argument, or
  -- * \return `RMW_RET_UNSUPPORTED` if event_type is not supported, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs.
  --  

   function rmw_subscription_event_init
     (rmw_event : access rmw_event_t;
      subscription : access constant rmw_rmw_types_h.rmw_subscription_s;
      event_type : rmw_event_type_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/event.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_subscription_event_init";

  --/ Take an event from the event handle.
  --*
  -- * \param[in] event_handle event object to take from
  -- * \param[inout] event_info event info object to write taken data into
  -- * \param[out] taken boolean flag indicating if an event was taken or not
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_BAD_ALLOC` if memory allocation failed, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs.
  --  

   function rmw_take_event
     (event_handle : access constant rmw_event_t;
      event_info : System.Address;
      taken : access Extensions.bool) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/event.h:119
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_take_event";

  --/ Finalize an rmw_event_t.
  --*
  -- * \param[in] event to finalize
  --  

   function rmw_event_fini (event : access rmw_event_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/event.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_event_fini";

end rmw_rmw_event_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
