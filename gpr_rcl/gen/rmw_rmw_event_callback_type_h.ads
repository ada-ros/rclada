pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;

package rmw_rmw_event_callback_type_h is

  -- Copyright 2021 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Common event callback type signature.
  --*
  -- * Event callbacks of this type can be called in various scenarios, e.g.
  -- * data becomes available on a subscription, a QoS event has occurred, or
  -- * something similar.
  -- *
  -- * The user_data argument is given by the user when registering the callback,
  -- * and is given back to the callback each time so it can have associated,
  -- * user-defined state.
  -- *
  -- * The number_of_events argument indicates the number of events since the
  -- * callback was called.
  -- * This is most often 1, but can be > 1 when events occur before the callback
  -- * is registered.
  -- * It should never be 0.
  -- *
  -- * \sa rmw_subscription_set_on_new_message_callback()
  -- * \sa rmw_service_set_on_new_request_callback()
  -- * \sa rmw_client_set_on_new_response_callback()
  -- * \sa rmw_event_set_callback()
  --  

   type rmw_event_callback_t is access procedure (arg1 : System.Address; arg2 : stddef_h.size_t)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/event_callback_type.h:46

end rmw_rmw_event_callback_type_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
