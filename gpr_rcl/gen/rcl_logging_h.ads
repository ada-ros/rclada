pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with rcl_arguments_h;
limited with rcl_allocator_h;
with rcl_types_h;

package rcl_logging_h is

  -- Copyright 2018 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Configure the logging system.
  --*
  -- * This function should be called during the ROS initialization process.
  -- * It will add the enabled log output appenders to the root logger.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param global_args The global arguments for the system
  -- * \param allocator Used to allocate memory used by the logging system
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERR` if a general error occurs
  --  

   function rcl_logging_configure (global_args : access constant rcl_arguments_h.rcl_arguments_t; allocator : access constant rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/logging.h:52
   pragma Import (C, rcl_logging_configure, "rcl_logging_configure");

  --*
  -- * This function should be called to tear down the logging setup by the configure function.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `RCL_RET_OK` if successful.
  -- * \return `RCL_RET_ERR` if a general error occurs
  --  

   function rcl_logging_fini return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/logging.h:72
   pragma Import (C, rcl_logging_fini, "rcl_logging_fini");

end rcl_logging_h;
