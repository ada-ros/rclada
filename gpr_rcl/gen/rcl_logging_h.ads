pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_logging_h;
limited with rcl_arguments_h;
limited with rcl_allocator_h;
with rcl_types_h;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with rcutils_time_h;
with stdarg_h;

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
   subtype rcl_logging_output_handler_t is rcutils_logging_h.rcutils_logging_output_handler_t;  -- /opt/ros/foxy/include/rcl/logging.h:31

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
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERR` if a general error occurs
  --  

   function rcl_logging_configure (global_args : access constant rcl_arguments_h.rcl_arguments_t; allocator : access constant rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_configure";

  --/ Configure the logging system with the provided output handler.
  --*
  -- * Similar to rcl_logging_configure, but it uses the provided output handler.
  -- * \sa rcl_logging_configure
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
  -- * \param output_handler Output handler to be installed
  -- * \return `RCL_RET_OK` if successful, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERR` if a general error occurs
  --  

   function rcl_logging_configure_with_output_handler
     (global_args : access constant rcl_arguments_h.rcl_arguments_t;
      allocator : access constant rcl_allocator_h.rcl_allocator_t;
      output_handler : rcl_logging_output_handler_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_configure_with_output_handler";

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

   function rcl_logging_fini return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_fini";

  --/ See if logging rosout is enabled.
  --*
  -- * This function is meant to be used to check if logging rosout is enabled.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `TRUE` if logging rosout is enabled, or
  -- * \return `FALSE` if logging rosout is disabled.
  --  

   function rcl_logging_rosout_enabled return Extensions.bool  -- /opt/ros/foxy/include/rcl/logging.h:123
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_enabled";

  --/ Default output handler used by rcl.
  --*
  -- * This function can be wrapped in a language specific client library,
  -- * adding the necessary mutual exclusion protection there, and then use
  -- * `rcl_logging_configure_with_output_handler` instead of
  -- * `rcl_logging_configure`.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  --  

   procedure rcl_logging_multiple_output_handler
     (location : access constant rcutils_logging_h.rcutils_log_location_t;
      severity : int;
      name : Interfaces.C.Strings.chars_ptr;
      timestamp : rcutils_time_h.rcutils_time_point_value_t;
      format : Interfaces.C.Strings.chars_ptr;
      args : access stdarg_h.va_list)  -- /opt/ros/foxy/include/rcl/logging.h:142
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_multiple_output_handler";

end rcl_logging_h;
