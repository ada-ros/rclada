pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with rmw_localhost_h;
with rcl_types_h;

package rcl_localhost_h is

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
   RCL_LOCALHOST_ENV_VAR : constant Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl/localhost.h:27
   with Import => True, 
        Convention => C, 
        External_Name => "RCL_LOCALHOST_ENV_VAR";

  --/ Determine if the user wants to communicate using loopback only.
  --*
  -- * Checks if localhost should be used for network communication based on environment.
  -- *
  -- * \param[out] localhost_only Must not be NULL.
  -- * \returns RCL_RET_INVALID_ARGUMENT if an argument is invalid, or
  -- * \returns RCL_RET_ERROR if an unexpected error happened, or
  -- * \returns RCL_RET_OK.
  --  

   function rcl_get_localhost_only (localhost_only : access rmw_localhost_h.rmw_localhost_only_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/localhost.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_localhost_only";

end rcl_localhost_h;
