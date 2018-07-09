pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with rcutils_types_string_array_h;
with rmw_types_h;

package rmw_sanity_checks_h is

  -- Copyright 2016-2017 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Check that a rmw_node_names_t struct is zero initialized.
   function rmw_check_zero_rmw_string_array (c_array : access rcutils_types_string_array_h.rcutils_string_array_t) return rmw_types_h.rmw_ret_t;  -- /opt/ros/bouncy/include/rmw/sanity_checks.h:33
   pragma Import (C, rmw_check_zero_rmw_string_array, "rmw_check_zero_rmw_string_array");

end rmw_sanity_checks_h;
