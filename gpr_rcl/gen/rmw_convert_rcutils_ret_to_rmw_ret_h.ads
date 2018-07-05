pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_types_rcutils_ret_h;
with rmw_types_h;

package rmw_convert_rcutils_ret_to_rmw_ret_h is

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
   function rmw_convert_rcutils_ret_to_rmw_ret (rcutils_ret : rcutils_types_rcutils_ret_h.rcutils_ret_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/convert_rcutils_ret_to_rmw_ret.h:31
   pragma Import (C, rmw_convert_rcutils_ret_to_rmw_ret, "rmw_convert_rcutils_ret_to_rmw_ret");

  -- extern "C"
end rmw_convert_rcutils_ret_to_rmw_ret_h;
