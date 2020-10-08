pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with rcutils_allocator_h;
with rmw_ret_types_h;

package rmw_security_options_h is

  -- Copyright 2020 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   type rmw_security_enforcement_policy_t is 
     (RMW_SECURITY_ENFORCEMENT_PERMISSIVE,
      RMW_SECURITY_ENFORCEMENT_ENFORCE)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/security_options.h:30

   type rmw_security_options_t is record
      enforce_security : aliased rmw_security_enforcement_policy_t;  -- /opt/ros/foxy/include/rmw/security_options.h:38
      security_root_path : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/security_options.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/security_options.h:36

  --/ Get zero initialized security options.
   function rmw_get_zero_initialized_security_options return rmw_security_options_t  -- /opt/ros/foxy/include/rmw/security_options.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_security_options";

  --/ Get default initialized security options.
   function rmw_get_default_security_options return rmw_security_options_t  -- /opt/ros/foxy/include/rmw/security_options.h:50
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_default_security_options";

  --/ Copy the given security options.
  --*
  -- * \param[in] src security options to be copied.
  -- * \param[in] allocator allocator used when copying data to the new security options.
  -- * \param[out] dst security options to be set.
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_security_options_copy
     (src : access constant rmw_security_options_t;
      allocator : access constant rcutils_allocator_h.rcutils_allocator_t;
      dst : access rmw_security_options_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/security_options.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_security_options_copy";

  --/ Set the security root path for the given security options.
  --*
  -- * The provided `security_root_path` will be copied into allocated memory.
  -- *
  -- * \param[in] security_root_path path to be set.
  -- * \param[in] allocator allocator used to allocate the new path.
  -- * \param[in|out] security_options security options to be set.
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_security_options_set_root_path
     (security_root_path : Interfaces.C.Strings.chars_ptr;
      allocator : access constant rcutils_allocator_h.rcutils_allocator_t;
      security_options : access rmw_security_options_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/security_options.h:78
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_security_options_set_root_path";

  --/ Finalize the given security_options.
  --*
  -- * \param[in] security_options security options to be finalized.
  -- * \param[in] allocator allocator used to deallocate the root path.
  -- * \returns RMW_RET_ERROR, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_security_options_fini (security_options : access rmw_security_options_t; allocator : access constant rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/security_options.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_security_options_fini";

end rmw_security_options_h;
