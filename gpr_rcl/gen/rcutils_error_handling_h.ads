pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;
with rcutils_allocator_h;
with rcutils_types_rcutils_ret_h;
with Interfaces.C.Extensions;

package rcutils_error_handling_h is

   --  arg-macro: procedure RCUTILS_SAFE_FWRITE_TO_STDERR (msg)
   --    fwrite(msg, sizeof(char), strlen(msg), stderr)
   --  unsupported macro: RCUTILS_CHECK_ARGUMENT_FOR_NULL(argument,error_return_type,allocator) RCUTILS_CHECK_FOR_NULL_WITH_MSG(argument, #argument " argument is null", return error_return_type, allocator)
   --  arg-macro: procedure RCUTILS_CHECK_FOR_NULL_WITH_MSG (value, msg, error_statement, allocator)
   --    if (NULL = value) { RCUTILS_SET_ERROR_MSG(msg, allocator); error_statement; }
   --  arg-macro: procedure RCUTILS_SET_ERROR_MSG (msg, allocator)
   --    rcutils_set_error_state(msg, __FILE__, __LINE__, allocator);
   --  unsupported macro: RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING(allocator,format_string,...) do { char * output_msg = NULL; output_msg = rcutils_format_string(allocator, format_string, __VA_ARGS__); if (output_msg) { RCUTILS_SET_ERROR_MSG(output_msg, allocator); allocator.deallocate(output_msg, allocator.state); } else { RCUTILS_SAFE_FWRITE_TO_STDERR("Failed to allocate memory for error message\n"); } } while (false)
  -- Copyright 2014 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  -- Note: migrated from rmw/error_handling.h in 2017-04
  --/ Struct which encapsulates the error state set by RCUTILS_SET_ERROR_MSG().
   type rcutils_error_state_t is record
      message : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:40
      file : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:41
      line_number : aliased stddef_h.size_t;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:42
      allocator : aliased rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, rcutils_error_state_t);  -- /opt/ros/bouncy/include/rcutils/error_handling.h:38

  -- TODO(dhood): use __STDC_LIB_EXT1__ if/when supported in other implementations.
  -- Limit the buffer size in the `fwrite` call to give an upper bound to buffer overrun in the case
  -- of non-null terminated `msg`.
  --/ Copy an error state into a destination error state.
  --*
  -- * The destination state must be empty, the memory will not be free'd.
  -- * The allocator from the source is used to allocate memory in the dst.
  -- *
  -- * The copied error_state should be finalized with rcutils_error_state_fini().
  -- *
  -- * \param[in] src the error state to copy from
  -- * \param[out] dst the error state to copy into
  -- * \returns RCUTILS_RET_OK if successful, or
  -- * \returns RCUTILS_RET_BAD_ALLOC if memory allocation fails, or
  -- * \returns RCUTILS_RET_ERROR if an unknown error occurs.
  --  

   function rcutils_error_state_copy (src : access constant rcutils_error_state_t; dst : access rcutils_error_state_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:71
   pragma Import (C, rcutils_error_state_copy, "rcutils_error_state_copy");

  --/ Finalizes a copied error state.
   procedure rcutils_error_state_fini (error_state : access rcutils_error_state_t);  -- /opt/ros/bouncy/include/rcutils/error_handling.h:76
   pragma Import (C, rcutils_error_state_fini, "rcutils_error_state_fini");

  --/ Set the error message, as well as the file and line on which it occurred.
  --*
  -- * This is not meant to be used directly, but instead via the
  -- * RCUTILS_SET_ERROR_MSG(msg) macro.
  -- *
  -- * The error_msg parameter is copied into the internal error storage and must
  -- * be null terminated.
  -- * The file parameter is not copied, but instead is assumed to be a global as
  -- * it should be set to the __FILE__ preprocessor literal when used with the
  -- * RCUTILS_SET_ERROR_MSG() macro.
  -- * It should also be null terminated.
  -- *
  -- * The allocator is kept within the error state so that it can be used to
  -- * deallocate it in the future.
  -- * Therefore the allocator state needs to exist until after the last time
  -- * rcutils_reset_error() is called.
  -- *
  -- * \param[in] error_msg The error message to set.
  -- * \param[in] file The path to the file in which the error occurred.
  -- * \param[in] line_number The line number on which the error occurred.
  -- * \param[in] allocator The allocator to be used when allocating space for the error state.
  --  

   procedure rcutils_set_error_state
     (error_string : Interfaces.C.Strings.chars_ptr;
      file : Interfaces.C.Strings.chars_ptr;
      line_number : stddef_h.size_t;
      allocator : rcutils_allocator_h.rcutils_allocator_t);  -- /opt/ros/bouncy/include/rcutils/error_handling.h:102
   pragma Import (C, rcutils_set_error_state, "rcutils_set_error_state");

  --/ Check an argument for a null value.
  --*
  -- * If the argument's value is `NULL`, set the error message saying so and
  -- * return the `error_return_type`.
  -- *
  -- * \param[in] argument The argument to test.
  -- * \param[in] error_return_type The type to return if the argument is `NULL`.
  -- * \param[in] allocator The allocator to use if an error message needs to be allocated.
  --  

  --/ Check a value for null, with an error message and error statement.
  --*
  -- * If `value` is `NULL`, the error statement will be evaluated after
  -- * setting the error message.
  -- *
  -- * \param[in] value The value to test.
  -- * \param[in] msg The error message if `value` is `NULL`.
  -- * \param[in] error_statement The statement to evaluate if `value` is `NULL`.
  -- * \param[in] allocator The allocator to use if an error message needs to be allocated.
  --  

  --/ Set the error message, as well as append the current file and line number.
  --*
  -- * If an error message was previously set, and rcutils_reset_error() was not called
  -- * afterwards, and this library was built with RCUTILS_REPORT_ERROR_HANDLING_ERRORS
  -- * turned on, then the previously set error message will be printed to stderr.
  -- * Error state storage is thread local and so all error related functions are
  -- * also thread local.
  -- *
  -- * \param[in] msg The error message to be set.
  -- * \param[in] allocator The allocator to be used when allocating space for the error state.
  --  

  --/ Set the error message using a format string and format arguments.
  --*
  -- * This function sets the error message using the given format string and
  -- * then frees the memory allocated during the string formatting.
  -- *
  -- * \param[in] allocator The allocator to be used when allocating space for the error state.
  -- * \param[in] format_string The string to be used as the format of the error message.
  -- * \param[in] ... Arguments for the format string.
  --  

  --/ Return `true` if the error is set, otherwise `false`.
   function rcutils_error_is_set return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:172
   pragma Import (C, rcutils_error_is_set, "rcutils_error_is_set");

  --/ Return an rcutils_error_state_t which was set with rcutils_set_error_state().
  --*
  -- * The returned pointer will be NULL if no error has been set in this thread.
  -- *
  -- * The returned pointer is valid until RCUTILS_SET_ERROR_MSG, rcutils_set_error_state,
  -- * or rcutils_reset_error are called in the same thread.
  -- *
  -- * \return A pointer to the current error state struct.
  --  

   function rcutils_get_error_state return access constant rcutils_error_state_t;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:185
   pragma Import (C, rcutils_get_error_state, "rcutils_get_error_state");

  --/ Return the error message followed by `, at <file>:<line>`, or `NULL`.
  --*
  -- * The returned pointer is valid until RCUTILS_SET_ERROR_MSG(),
  -- * rcutils_set_error_state(), or rcutils_reset_error() are called from the same thread.
  -- *
  -- * \return The current formatted error string, or NULL if not set.
  --  

   function rcutils_get_error_string return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:196
   pragma Import (C, rcutils_get_error_string, "rcutils_get_error_string");

  --/ Return the error message followed by `, at <file>:<line>` if set, else "error not set".
  --*
  -- * This function is guaranteed to return a valid c-string.
  -- *
  -- * The returned pointer is valid until RCUTILS_SET_ERROR_MSG,
  -- * rcutils_set_error_state, or rcutils_reset_error are called in the same thread.
  -- *
  -- * \return The current error string, with file and line number, or "error not set" if not set.
  --  

   function rcutils_get_error_string_safe return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:209
   pragma Import (C, rcutils_get_error_string_safe, "rcutils_get_error_string_safe");

  --/ Reset the error state by clearing any previously set error state.
   procedure rcutils_reset_error;  -- /opt/ros/bouncy/include/rcutils/error_handling.h:214
   pragma Import (C, rcutils_reset_error, "rcutils_reset_error");

end rcutils_error_handling_h;
