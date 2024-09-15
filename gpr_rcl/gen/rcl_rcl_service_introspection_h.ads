pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;

package rcl_rcl_service_introspection_h is

   RCL_SERVICE_INTROSPECTION_TOPIC_POSTFIX : aliased constant String := "/_service_event" & ASCII.NUL;  --  /opt/ros/jazzy/include/rcl/rcl/service_introspection.h:18

  -- Copyright 2022 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ The introspection state for a client or service.
   type rcl_service_introspection_state_e is 
     (RCL_SERVICE_INTROSPECTION_OFF,
      RCL_SERVICE_INTROSPECTION_METADATA,
      RCL_SERVICE_INTROSPECTION_CONTENTS)
   with Convention => C;  -- /opt/ros/jazzy/include/rcl/rcl/service_introspection.h:21

  --/ Introspection disabled
  --/ Introspect metadata only
  --/ Introspection metadata and contents
   subtype rcl_service_introspection_state_t is rcl_service_introspection_state_e;  -- /opt/ros/jazzy/include/rcl/rcl/service_introspection.h:29

end rcl_rcl_service_introspection_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
