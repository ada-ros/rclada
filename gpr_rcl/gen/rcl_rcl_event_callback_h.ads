pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rmw_rmw_event_callback_type_h;

package rcl_rcl_event_callback_h is

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
   subtype rcl_event_callback_t is rmw_rmw_event_callback_type_h.rmw_event_callback_t;  -- /opt/ros/jazzy/include/rcl/rcl/event_callback.h:25

end rcl_rcl_event_callback_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
