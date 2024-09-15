pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcutils_rcutils_logging_h;
with Interfaces.C.Strings;
with stddef_h;
with rcl_rcl_allocator_h;
limited with rcutils_rcutils_allocator_h;
with rcl_rcl_types_h;

package rcl_rcl_log_level_h is

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
  --/ @file
  --/ typedef for RCUTILS_LOG_SEVERITY;
   subtype rcl_log_severity_t is rcutils_rcutils_logging_h.RCUTILS_LOG_SEVERITY;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:31

  --/ A logger item to specify a name and a log level.
  --/ Name for the logger.
   type rcl_logger_setting_s is record
      name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:37
      level : aliased rcl_log_severity_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:34

  --/ Minimum log level severity of the logger.
   subtype rcl_logger_setting_t is rcl_logger_setting_s;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:40

  --/ Hold default logger level and other logger setting.
  --/ Minimum default logger level severity.
   type rcl_log_levels_s is record
      default_logger_level : aliased rcl_log_severity_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:46
      logger_settings : access rcl_logger_setting_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:48
      num_logger_settings : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:50
      capacity_logger_settings : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:52
      allocator : aliased rcl_rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:54
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:43

  --/ Array of logger setting.
  --/ Number of logger settings.
  --/ Capacity of logger settings.
  --/ Allocator used to allocate objects in this struct.
   subtype rcl_log_levels_t is rcl_log_levels_s;  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:55

  --/ Return a rcl_log_levels_t struct with members initialized to zero value.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return a rcl_log_levels_t struct with members initialized to zero value.
  --  

   function rcl_get_zero_initialized_log_levels return rcl_log_levels_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_log_levels";

  --/ Initialize a log levels structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] log_levels The structure to be initialized.
  -- * \param[in] allocator Memory allocator to be used and assigned into log_levels.
  -- * \param[in] logger_count Number of logger settings to be allocated.
  -- *  This reserves memory for logger_settings, but doesn't initialize it.
  -- * \return #RCL_RET_OK if the structure was initialized successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels is NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels contains initialized memory, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if allocator is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed.
  --  

   function rcl_log_levels_init
     (log_levels : access rcl_log_levels_t;
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      logger_count : stddef_h.size_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:97
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_log_levels_init";

  --/ Copy one log levels structure into another.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] src The structure to be copied.
  -- *  Its allocator is used to copy memory into the new structure.
  -- * \param[out] dst A log levels structure to be copied into.
  -- * \return #RCL_RET_OK if the structure was copied successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if src is NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if src allocator is invalid, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if dst is NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if dst contains already allocated memory, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed.
  --  

   function rcl_log_levels_copy (src : access constant rcl_log_levels_t; dst : access rcl_log_levels_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:123
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_log_levels_copy";

  --/ Reclaim resources held inside rcl_log_levels_t structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] log_levels The structure which its resources have to be deallocated.
  -- * \return #RCL_RET_OK if the memory was successfully freed, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels is NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if the log_levels allocator is invalid and the structure contains initialized memory.
  --  

   function rcl_log_levels_fini (log_levels : access rcl_log_levels_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:142
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_log_levels_fini";

  --/ Shrink log levels structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] log_levels The structure to be shrunk.
  -- * \return #RCL_RET_OK if the memory was successfully shrunk, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels is NULL or if its allocator is invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if reallocating memory failed.
  --  

   function rcl_log_levels_shrink_to_size (log_levels : access rcl_log_levels_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:161
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_log_levels_shrink_to_size";

  --/ Add logger setting with a name and a level.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] log_levels The structure where to set the logger log level.
  -- * \param[in] logger_name Name for the logger, a copy of it will be stored in the structure.
  -- * \param[in] log_level Minimum log level severity to be set for logger_name.
  -- * \return #RCL_RET_OK if add logger setting successfully, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels is NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels was not initialized, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if log_levels allocator is invalid, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if logger_name is NULL, or
  -- * \return #RCL_RET_ERROR if the log_levels structure is already full.
  --  

   function rcl_log_levels_add_logger_setting
     (log_levels : access rcl_log_levels_t;
      logger_name : Interfaces.C.Strings.chars_ptr;
      log_level : rcl_log_severity_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/log_level.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_log_levels_add_logger_setting";

end rcl_rcl_log_level_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
