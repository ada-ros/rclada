pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with rcl_allocator_h;
with rcl_types_h;

package rcl_arguments_h is

   RCL_ROS_ARGS_FLAG : aliased constant String := "--ros-args" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:38
   RCL_ROS_ARGS_EXPLICIT_END_TOKEN : aliased constant String := "--" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:39
   RCL_PARAM_FLAG : aliased constant String := "--param" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:40
   RCL_SHORT_PARAM_FLAG : aliased constant String := "-p" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:41
   RCL_PARAM_FILE_FLAG : aliased constant String := "--params-file" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:42
   RCL_REMAP_FLAG : aliased constant String := "--remap" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:43
   RCL_SHORT_REMAP_FLAG : aliased constant String := "-r" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:44
   RCL_ENCLAVE_FLAG : aliased constant String := "--enclave" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:45
   RCL_SHORT_ENCLAVE_FLAG : aliased constant String := "-e" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:46
   RCL_LOG_LEVEL_FLAG : aliased constant String := "--log-level" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:47
   RCL_EXTERNAL_LOG_CONFIG_FLAG : aliased constant String := "--log-config-file" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:48

   RCL_LOG_STDOUT_FLAG_SUFFIX : aliased constant String := "stdout-logs" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:50
   RCL_LOG_ROSOUT_FLAG_SUFFIX : aliased constant String := "rosout-logs" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:51
   RCL_LOG_EXT_LIB_FLAG_SUFFIX : aliased constant String := "external-lib-logs" & ASCII.NUL;  --  /opt/ros/foxy/include/rcl/arguments.h:52

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
   type rcl_arguments_impl_t is null record;   -- incomplete struct

  --/ Hold output of parsing command line arguments.
  --/ Private implementation pointer.
   type rcl_arguments_t is record
      impl : access rcl_arguments_impl_t;  -- /opt/ros/foxy/include/rcl/arguments.h:35
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl/arguments.h:32

  -- To be prefixed with --enable- or --disable-
  --/ Return a rcl_arguments_t struct with members initialized to `NULL`.
   function rcl_get_zero_initialized_arguments return rcl_arguments_t  -- /opt/ros/foxy/include/rcl/arguments.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_arguments";

  --/ Parse command line arguments into a structure usable by code.
  --*
  -- * \sa rcl_get_zero_initialized_arguments()
  -- *
  -- * ROS arguments are expected to be scoped by a leading `--ros-args` flag and a trailing double
  -- * dash token `--` which may be elided if no non-ROS arguments follow after the last `--ros-args`.
  -- *
  -- * Remap rule parsing is supported via `-r/--remap` flags e.g. `--remap from:=to` or `-r from:=to`.
  -- * Successfully parsed remap rules are stored in the order they were given in `argv`.
  -- * If given arguments `{"__ns:=/foo", "__ns:=/bar"}` then the namespace used by nodes in this
  -- * process will be `/foo` and not `/bar`.
  -- *
  -- * \sa rcl_remap_topic_name()
  -- * \sa rcl_remap_service_name()
  -- * \sa rcl_remap_node_name()
  -- * \sa rcl_remap_node_namespace()
  -- *
  -- * Parameter override rule parsing is supported via `-p/--param` flags e.g. `--param name:=value`
  -- * or `-p name:=value`.
  -- *
  -- * The default log level will be parsed as `--log-level level`, where `level` is a name
  -- * representing one of the log levels in the `RCUTILS_LOG_SEVERITY` enum, e.g. `info`, `debug`,
  -- * `warn`, not case sensitive.
  -- * If multiple of these rules are found, the last one parsed will be used.
  -- *
  -- * If an argument does not appear to be a valid ROS argument e.g. a `-r/--remap` flag followed by
  -- * anything but a valid remap rule, parsing will fail immediately.
  -- *
  -- * If an argument does not appear to be a known ROS argument, then it is skipped and left unparsed.
  -- *
  -- * \sa rcl_arguments_get_count_unparsed_ros()
  -- * \sa rcl_arguments_get_unparsed_ros()
  -- *
  -- * All arguments found outside a `--ros-args ... --` scope are skipped and left unparsed.
  -- *
  -- * \sa rcl_arguments_get_count_unparsed()
  -- * \sa rcl_arguments_get_unparsed()
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] argc The number of arguments in argv.
  -- * \param[in] argv The values of the arguments.
  -- * \param[in] allocator A valid allocator.
  -- * \param[out] args_output A structure that will contain the result of parsing.
  -- *   Must be zero initialized before use.
  -- * \return `RCL_RET_OK` if the arguments were parsed successfully, or
  -- * \return `RCL_RET_INVALID_ROS_ARGS` if an invalid ROS argument is found, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_parse_arguments
     (argc : int;
      argv : System.Address;
      allocator : rcl_allocator_h.rcl_allocator_t;
      args_output : access rcl_arguments_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_parse_arguments";

  --/ Return the number of arguments that were not ROS specific arguments.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \return number of unparsed arguments, or
  -- * \return -1 if args is `NULL` or zero initialized.
  --  

   function rcl_arguments_get_count_unparsed (args : access constant rcl_arguments_t) return int  -- /opt/ros/foxy/include/rcl/arguments.h:143
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_count_unparsed";

  --/ Return a list of indices to non ROS specific arguments.
  --*
  -- * Non ROS specific arguments may have been provided i.e. arguments outside a '--ros-args' scope.
  -- * This function populates an array of indices to these arguments in the original argv array.
  -- * Since the first argument is always assumed to be a process name, the list will always contain
  -- * the index 0.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \param[in] allocator A valid allocator.
  -- * \param[out] output_unparsed_indices An allocated array of indices into the original argv array.
  -- *   This array must be deallocated by the caller using the given allocator.
  -- *   If there are no unparsed args then the output will be set to NULL.
  -- * \return `RCL_RET_OK` if everything goes correctly, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_get_unparsed
     (args : access constant rcl_arguments_t;
      allocator : rcl_allocator_h.rcl_allocator_t;
      output_unparsed_indices : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:174
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_unparsed";

  --/ Return the number of ROS specific arguments that were not successfully parsed.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \return number of unparsed ROS specific arguments, or
  -- * \return -1 if args is `NULL` or zero initialized.
  --  

   function rcl_arguments_get_count_unparsed_ros (args : access constant rcl_arguments_t) return int  -- /opt/ros/foxy/include/rcl/arguments.h:196
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_count_unparsed_ros";

  --/ Return a list of indices to unknown ROS specific arguments that were left unparsed.
  --*
  -- * Some ROS specific arguments may not have been recognized, or were not intended to be
  -- * parsed by rcl.
  -- * This function populates an array of indices to these arguments in the original argv array.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \param[in] allocator A valid allocator.
  -- * \param[out] output_unparsed_ros_indices An allocated array of indices into the original argv array.
  -- *   This array must be deallocated by the caller using the given allocator.
  -- *   If there are no unparsed ROS specific arguments then the output will be set to NULL.
  -- * \return `RCL_RET_OK` if everything goes correctly, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_get_unparsed_ros
     (args : access constant rcl_arguments_t;
      allocator : rcl_allocator_h.rcl_allocator_t;
      output_unparsed_ros_indices : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:226
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_unparsed_ros";

  --/ Return the number of parameter yaml files given in the arguments.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \return number of yaml files, or
  -- * \return -1 if args is `NULL` or zero initialized.
  --  

   function rcl_arguments_get_param_files_count (args : access constant rcl_arguments_t) return int  -- /opt/ros/foxy/include/rcl/arguments.h:248
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_param_files_count";

  --/ Return a list of yaml parameter file paths specified on the command line.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] arguments An arguments structure that has been parsed.
  -- * \param[in] allocator A valid allocator.
  -- * \param[out] parameter_files An allocated array of paramter file names.
  -- *   This array must be deallocated by the caller using the given allocator.
  -- *   The output is NULL if there were no paramter files.
  -- * \return `RCL_RET_OK` if everything goes correctly, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_get_param_files
     (arguments : access constant rcl_arguments_t;
      allocator : rcl_allocator_h.rcl_allocator_t;
      parameter_files : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:275
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_param_files";

  --/ Return all parameter overrides parsed from the command line.
  --*
  -- * Parameter overrides are parsed directly from command line arguments and
  -- * parameter files provided in the command line.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] arguments An arguments structure that has been parsed.
  -- * \param[out] parameter_overrides Parameter overrides as parsed from command line arguments.
  -- *   This structure must be finalized by the caller.
  -- *   The output is NULL if no parameter overrides were parsed.
  -- * \return `RCL_RET_OK` if everything goes correctly, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_get_param_overrides (arguments : access constant rcl_arguments_t; parameter_overrides : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_get_param_overrides";

  --/ Return a list of arguments with ROS-specific arguments removed.
  --*
  -- * Some arguments may not have been intended as ROS arguments.
  -- * This function populates an array of the aruments in a new argv array.
  -- * Since the first argument is always assumed to be a process name, the list
  -- * will always contain the first value from the argument vector.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] argv The argument vector
  -- * \param[in] args An arguments structure that has been parsed.
  -- * \param[in] allocator A valid allocator.
  -- * \param[out] nonros_argc The count of arguments that aren't ROS-specific
  -- * \param[out] nonros_argv An allocated array of arguments that aren't ROS-specific
  -- *   This array must be deallocated by the caller using the given allocator.
  -- *   If there are no non-ROS args, then the output will be set to NULL.
  -- * \return `RCL_RET_OK` if everything goes correctly, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_remove_ros_arguments
     (argv : System.Address;
      args : access constant rcl_arguments_t;
      allocator : rcl_allocator_h.rcl_allocator_t;
      nonros_argc : access int;
      nonros_argv : System.Address) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:339
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_remove_ros_arguments";

  --/ Copy one arguments structure into another.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args The structure to be copied.
  -- *  Its allocator is used to copy memory into the new structure.
  -- * \param[out] args_out A zero-initialized arguments structure to be copied into.
  -- * \return `RCL_RET_OK` if the structure was copied successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_copy (args : access constant rcl_arguments_t; args_out : access rcl_arguments_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_copy";

  --/ Reclaim resources held inside rcl_arguments_t structure.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] args The structure to be deallocated.
  -- * \return `RCL_RET_OK` if the memory was successfully freed, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_arguments_fini (args : access rcl_arguments_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/arguments.h:389
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_arguments_fini";

end rcl_arguments_h;
