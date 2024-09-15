pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with rcutils_rcutils_allocator_h;
with rmw_rmw_ret_types_h;

package rmw_rmw_security_options_h is

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
   type rmw_security_enforcement_policy_e is 
     (RMW_SECURITY_ENFORCEMENT_PERMISSIVE,
      RMW_SECURITY_ENFORCEMENT_ENFORCE)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:30

   subtype rmw_security_enforcement_policy_t is rmw_security_enforcement_policy_e;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:34

   type rmw_security_options_s is record
      enforce_security : aliased rmw_security_enforcement_policy_t;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:38
      security_root_path : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:36

   subtype rmw_security_options_t is rmw_security_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:40

  --/ Get zero initialized security options.
   function rmw_get_zero_initialized_security_options return rmw_security_options_t  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_security_options";

  --/ Get default initialized security options.
   function rmw_get_default_security_options return rmw_security_options_t  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:50
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
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dst : access rmw_security_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:62
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_security_options_copy";

  --/ Set the security root path for the given security options.
  --*
  -- * The provided `security_root_path` will be copied into allocated memory.
  -- *
  -- * \param[in] security_root_path path to be set.
  -- * \param[in] allocator allocator used to allocate the new path.
  -- * \param[inout] security_options security options to be set.
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_security_options_set_root_path
     (security_root_path : Interfaces.C.Strings.chars_ptr;
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      security_options : access rmw_security_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:79
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

   function rmw_security_options_fini (security_options : access rmw_security_options_t; allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/security_options.h:93
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_security_options_fini";

end rmw_rmw_security_options_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
