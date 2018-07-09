pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;

package rmw_node_security_options_h is

  -- Copyright 2017 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   function rmw_get_zero_initialized_node_security_options return rmw_types_h.rmw_node_security_options_t;  -- /opt/ros/bouncy/include/rmw/node_security_options.h:27
   pragma Import (C, rmw_get_zero_initialized_node_security_options, "rmw_get_zero_initialized_node_security_options");

   function rmw_get_default_node_security_options return rmw_types_h.rmw_node_security_options_t;  -- /opt/ros/bouncy/include/rmw/node_security_options.h:31
   pragma Import (C, rmw_get_default_node_security_options, "rmw_get_default_node_security_options");

end rmw_node_security_options_h;
