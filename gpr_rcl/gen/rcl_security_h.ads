pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with rcutils_allocator_h;
with rmw_security_options_h;
with rcl_types_h;
with Interfaces.C.Extensions;
limited with rcl_allocator_h;

package rcl_security_h is

   ROS_SECURITY_ENCLAVE_OVERRIDE : aliased constant String := "ROS_SECURITY_ENCLAVE_OVERRIDE" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/security.h:31

   ROS_SECURITY_KEYSTORE_VAR_NAME : aliased constant String := "ROS_SECURITY_KEYSTORE" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/security.h:35

   ROS_SECURITY_STRATEGY_VAR_NAME : aliased constant String := "ROS_SECURITY_STRATEGY" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/security.h:39

   ROS_SECURITY_ENABLE_VAR_NAME : aliased constant String := "ROS_SECURITY_ENABLE" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/security.h:43

  -- Copyright 2018-2020 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Initialize security options from values in the environment variables and given names.
  --*
  -- * Initialize the given security options based on the environment.
  -- * For more details:
  -- *  \sa rcl_security_enabled
  -- *  \sa rcl_get_enforcement_policy
  -- *  \sa rcl_get_secure_root
  -- *
  -- * \param[in] name name used to find the securiy root path.
  -- * \param[in] allocator used to do allocations.
  -- * \param[out] security_options security options that will be configured according to
  -- *  the environment.
  -- * \return `RCL_RET_OK` If the security options are returned properly, or
  -- * \returns RCL_RET_INVALID_ARGUMENT if an argument is not valid, or
  -- * \returns RCL_RET_ERROR if an unexpected error happened
  --  

   function rcl_get_security_options_from_environment
     (name : Interfaces.C.Strings.chars_ptr;
      allocator : access constant rcutils_allocator_h.rcutils_allocator_t;
      security_options : access rmw_security_options_h.rmw_security_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/security.h:64
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_security_options_from_environment";

  --/ Check if security has to be used, according to the environment.
  --*
  -- * If `ROS_SECURITY_ENABLE` environment variable is set to "true", `use_security` will be set to
  -- * true.
  -- *
  -- * \param[out] use_security Must not be NULL.
  -- * \returns RCL_RET_INVALID_ARGUMENT if an argument is not valid, or
  -- * \returns RCL_RET_ERROR if an unexpected error happened, or
  -- * \returns RCL_RET_OK.
  --  

   function rcl_security_enabled (use_security : access Extensions.bool) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/security.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_security_enabled";

  --/ Get security enforcement policy from the environment.
  --*
  -- * Sets `policy` based on the value of `ROS_SECURITY_STRATEGY` environment variable.
  -- * If `ROS_SECURITY_STRATEGY` is "Enforce", `policy` will be `RMW_SECURITY_ENFORCEMENT_ENFORCE`.
  -- * If not, `policy` will be `RMW_SECURITY_ENFORCEMENT_PERMISSIVE`.
  -- *
  -- * \param[out] policy Must not be NULL.
  -- * \returns RCL_RET_INVALID_ARGUMENT if an argument is not valid, or
  -- * \returns RCL_RET_ERROR if an unexpected error happened, or
  -- * \returns RCL_RET_OK.
  --  

   function rcl_get_enforcement_policy (policy : access rmw_security_options_h.rmw_security_enforcement_policy_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/security.h:96
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_enforcement_policy";

  --/ Return the secure root given a enclave name.
  --*
  -- * Return the security directory associated with the enclave name.
  -- *
  -- * The value of the environment variable `ROS_SECURITY_KEYSTORE` is used as a root.
  -- * The specific directory to be used, is found from that root using the `name` passed.
  -- * E.g. for a context named "/a/b/c" and root "/r", the secure root path will be
  -- * "/r/a/b/c", where the delimiter "/" is native for target file system (e.g. "\\" for _WIN32).
  -- *
  -- * However, this expansion can be overridden by setting the secure enclave override environment
  -- * (`ROS_SECURITY_ENCLAVE_OVERRIDE`) variable, allowing users to explicitly specify the exact enclave
  -- * `name` to be utilized.
  -- * Such an override is useful for applications where the enclave is non-deterministic
  -- * before runtime, or when testing and using additional tools that may not otherwise be easily
  -- * provisioned.
  -- *
  -- * \param[in] name validated name (a single token)
  -- * \param[in] allocator the allocator to use for allocation
  -- * \returns Machine specific (absolute) enclave directory path or NULL on failure.
  -- *  Returned pointer must be deallocated by the caller of this function
  --  

   function rcl_get_secure_root (name : Interfaces.C.Strings.chars_ptr; allocator : access constant rcl_allocator_h.rcl_allocator_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl/security.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_secure_root";

end rcl_security_h;
