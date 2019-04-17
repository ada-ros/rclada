pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_error_handling_h;

package rcl_error_handling_h is

   --  unsupported macro: rcl_initialize_error_handling_thread_local_storage rcutils_initialize_error_handling_thread_local_storage
   --  unsupported macro: rcl_set_error_state rcutils_set_error_state
   --  arg-macro: procedure RCL_CHECK_ARGUMENT_FOR_NULL (argument, error_return_type)
   --    RCUTILS_CHECK_ARGUMENT_FOR_NULL(argument, error_return_type)
   --  arg-macro: procedure RCL_CHECK_FOR_NULL_WITH_MSG (value, msg, error_statement)
   --    RCUTILS_CHECK_FOR_NULL_WITH_MSG(value, msg, error_statement)
   --  arg-macro: procedure RCL_SET_ERROR_MSG (msg)
   --    RCUTILS_SET_ERROR_MSG(msg)
   --  unsupported macro: RCL_SET_ERROR_MSG_WITH_FORMAT_STRING(fmt_str,...) RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING(fmt_str, __VA_ARGS__)
   --  unsupported macro: rcl_error_is_set rcutils_error_is_set
   --  unsupported macro: rcl_get_error_state rcutils_get_error_state
   --  unsupported macro: rcl_get_error_string rcutils_get_error_string
   --  unsupported macro: rcl_reset_error rcutils_reset_error
  -- Copyright 2015 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ The error handling in RCL is just an alias to the error handling in rcutils.
   subtype rcl_error_state_t is rcutils_error_handling_h.rcutils_error_state_t;  -- /opt/ros/crystal/include/rcl/error_handling.h:22

   subtype rcl_error_string_t is rcutils_error_handling_h.rcutils_error_string_t;  -- /opt/ros/crystal/include/rcl/error_handling.h:23

end rcl_error_handling_h;
