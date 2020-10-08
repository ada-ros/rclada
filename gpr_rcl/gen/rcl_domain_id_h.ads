pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;
with rcl_types_h;

package rcl_domain_id_h is

   --  unsupported macro: RCL_DEFAULT_DOMAIN_ID RMW_DEFAULT_DOMAIN_ID
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
   RCL_DOMAIN_ID_ENV_VAR : constant Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl/domain_id.h:31
   with Import => True, 
        Convention => C, 
        External_Name => "RCL_DOMAIN_ID_ENV_VAR";

  --/ Determine the default domain ID, based on the environment.
  --*
  -- * \param[out] domain_id Must not be NULL.
  -- * \returns RCL_RET_INVALID_ARGUMENT if an argument is invalid, or,
  -- * \returns RCL_RET_ERROR in case of an unexpected error, or,
  -- * \returns RCL_RET_OK.
  --  

   function rcl_get_default_domain_id (domain_id : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/domain_id.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_default_domain_id";

end rcl_domain_id_h;
