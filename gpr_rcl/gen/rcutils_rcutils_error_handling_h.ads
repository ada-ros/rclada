pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with rcutils_rcutils_allocator_h;
with rcutils_rcutils_types_rcutils_ret_h;
with Interfaces.C.Strings;
with stddef_h;
with Interfaces.C.Extensions;

package rcutils_rcutils_error_handling_h is

   --  arg-macro: procedure RCUTILS_SAFE_FWRITE_TO_STDERR (msg)
   --    do {fwrite(msg, sizeof(char), strlen(msg), stderr);} while (0)
   --  unsupported macro: RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING(format_string,...) do { char output_msg[RCUTILS_ERROR_MESSAGE_MAX_LENGTH]; int ret = rcutils_snprintf(output_msg, sizeof(output_msg), format_string, __VA_ARGS__); if (ret < 0) { RCUTILS_SAFE_FWRITE_TO_STDERR("Failed to call snprintf for error message formatting\n"); } else { RCUTILS_SAFE_FWRITE_TO_STDERR(output_msg); } } while (0)
   RCUTILS_ERROR_STATE_LINE_NUMBER_STR_MAX_LENGTH : constant := 20;  --  /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:78

   RCUTILS_ERROR_FORMATTING_CHARACTERS : constant := 6;  --  /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:81

   RCUTILS_ERROR_MESSAGE_MAX_LENGTH : constant := 1024;  --  /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:84

   RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH : constant := 768;  --  /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:91
   --  unsupported macro: RCUTILS_ERROR_STATE_FILE_MAX_LENGTH ( RCUTILS_ERROR_MESSAGE_MAX_LENGTH - RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH - RCUTILS_ERROR_STATE_LINE_NUMBER_STR_MAX_LENGTH - RCUTILS_ERROR_FORMATTING_CHARACTERS - 1)
   --  unsupported macro: RCUTILS_CHECK_ARGUMENT_FOR_NULL(argument,error_return_type) RCUTILS_CHECK_FOR_NULL_WITH_MSG( argument, #argument " argument is null", return error_return_type)
   --  arg-macro: procedure RCUTILS_CHECK_FOR_NULL_WITH_MSG (value, msg, error_statement)
   --    do { if (NULL = value) { RCUTILS_SET_ERROR_MSG(msg); error_statement; } } while (0)
   --  arg-macro: procedure RCUTILS_SET_ERROR_MSG (msg)
   --    do {rcutils_set_error_state(msg, __FILE__, __LINE__);} while (0)
   --  unsupported macro: RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING(format_string,...) do { char output_msg[RCUTILS_ERROR_MESSAGE_MAX_LENGTH]; int ret = rcutils_snprintf(output_msg, sizeof(output_msg), format_string, __VA_ARGS__); if (ret < 0) { RCUTILS_SAFE_FWRITE_TO_STDERR("Failed to call snprintf for error message formatting\n"); } else { RCUTILS_SET_ERROR_MSG(output_msg); } } while (0)
   --  arg-macro: procedure RCUTILS_CAN_SET_MSG_AND_RETURN_WITH_ERROR_OF (error_return_value)
   --    RCUTILS_CAN_FAIL_WITH( { RCUTILS_SET_ERROR_MSG("Injecting " RCUTILS_STRINGIFY(error_return_value)); return error_return_value; })
   --  arg-macro: procedure RCUTILS_SET_ERROR_MSG_AND_APPEND_PREV_ERROR (msg)
   --    do { rcutils_error_string_t error_string := rcutils_get_error_string(); rcutils_reset_error(); RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING( RCUTILS_EXPAND(msg ": %s"), error_string.str); } while (0)
   --  unsupported macro: RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING_AND_APPEND_PREV_ERROR(format_string,...) do { rcutils_error_string_t error_string = rcutils_get_error_string(); rcutils_reset_error(); RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING( RCUTILS_EXPAND(format_string ": %s"), __VA_ARGS__, error_string.str); } while (0)
   --  arg-macro: procedure RCUTILS_SAFE_FWRITE_TO_STDERR_AND_APPEND_PREV_ERROR (msg)
   --    do { rcutils_error_string_t error_string := rcutils_get_error_string(); rcutils_reset_error(); RCUTILS_SAFE_FWRITE_TO_STDERR(msg); RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING(": %s", error_string.str); } while (0)
   --  unsupported macro: RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING_AND_APPEND_PREV_ERROR(format_string,...) do { rcutils_error_string_t error_string = rcutils_get_error_string(); rcutils_reset_error(); RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING(format_string, __VA_ARGS__); RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING(": %s", error_string.str); } while (0)

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
  --/ \file
  --/ Write the given msg out to stderr, limiting the buffer size in the `fwrite`.
  --*
  -- * This ensures that there is an upper bound to a buffer overrun if `msg` is
  -- * non-null terminated.
  --  

  --/ Write the given msg out to stderr.
  --/ Set the error message to stderr using a format string and format arguments.
  --*
  -- * This function sets the error message to stderr using the given format string.
  -- * The resulting formatted string is silently truncated at
  -- * RCUTILS_ERROR_MESSAGE_MAX_LENGTH.
  -- *
  -- * \param[in] format_string The string to be used as the format of the error message.
  -- * \param[in] ... Arguments for the format string.
  --  

  --/ The maximum length a formatted number is allowed to have.
  --/ The maximum number of formatting characters allowed.
  --/ The maximum formatted string length.
  --/ The maximum length for user defined error message
  --*
  -- * Remember that "chained" errors will include previously specified file paths
  -- * e.g. "some error, at /path/to/a.c:42, at /path/to/b.c:42"
  --  

  --/ The calculated maximum length for the filename.
  --*
  -- * With RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH = 768, RCUTILS_ERROR_STATE_FILE_MAX_LENGTH == 229
  --  

  --/ Struct wrapping a fixed-size c string used for returning the formatted error string.
  --/ The fixed-size C string used for returning the formatted error string.
   subtype anon_array2331 is Interfaces.C.char_array (0 .. 1023);
   type rcutils_error_string_s is record
      str : aliased anon_array2331;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:108
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:105

   subtype rcutils_error_string_t is rcutils_error_string_s;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:109

  --/ Struct which encapsulates the error state set by RCUTILS_SET_ERROR_MSG().
  --/ User message storage, limited to RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH characters.
   subtype anon_array2336 is Interfaces.C.char_array (0 .. 767);
   subtype anon_array2338 is Interfaces.C.char_array (0 .. 228);
   type rcutils_error_state_s is record
      message : aliased anon_array2336;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:115
      file : aliased anon_array2338;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:118
      line_number : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:120
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:112

  --/ File name, limited to what's left from RCUTILS_ERROR_STATE_MAX_SIZE characters
  --/ after subtracting storage for others.
  --/ Line number of error.
   subtype rcutils_error_state_t is rcutils_error_state_s;  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:121

  -- make sure our math is right...
  -- null terminating character  
  --/ Forces initialization of thread-local storage if called in a newly created thread.
  --*
  -- * If this function is not called beforehand, then the first time the error
  -- * state is set or the first time the error message is retrieved, the default
  -- * allocator will be used to allocate thread-local storage.
  -- *
  -- * This function may or may not allocate memory.
  -- * The system's thread-local storage implementation may need to allocate
  -- * memory, since it usually has no way of knowing how much storage is needed
  -- * without knowing how many threads will be created.
  -- * Most implementations (e.g. C11, C++11, and pthread) do not have ways to
  -- * specify how this memory is allocated, but if the implementation allows, the
  -- * given allocator to this function will be used, but is otherwise unused.
  -- * This only occurs when creating and destroying threads, which can be avoided
  -- * in the "steady" state by reusing pools of threads.
  -- *
  -- * It is worth considering that repeated thread creation and destruction will
  -- * result in repeated memory allocations and could result in memory
  -- * fragmentation.
  -- * This is typically avoided anyways by using pools of threads.
  -- *
  -- * In case an error is indicated by the return code, no error message will have
  -- * been set.
  -- *
  -- * If called more than once in a thread, or after implicitly initialized by
  -- * setting the error state, it will still return `RCUTILS_RET_OK`, even
  -- * if the given allocator is invalid.
  -- * Essentially this function does nothing if thread-local storage has already
  -- * been called.
  -- * If already initialized, the given allocator is ignored, even if it does not
  -- * match the allocator used originally to initialize the thread-local storage.
  -- *
  -- * \param[in] allocator to be used to allocate and deallocate memory
  -- * \return #RCUTILS_RET_OK if successful, or
  -- * \return #RCUTILS_RET_INVALID_ARGUMENT if the allocator is invalid, or
  -- * \return #RCUTILS_RET_BAD_ALLOC if allocating memory fails, or
  -- * \return #RCUTILS_RET_ERROR if an unspecified error occurs.
  --  

   function rcutils_initialize_error_handling_thread_local_storage (allocator : rcutils_rcutils_allocator_h.rcutils_allocator_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:176
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_initialize_error_handling_thread_local_storage";

  --/ Set the error message, as well as the file and line on which it occurred.
  --*
  -- * This is not meant to be used directly, but instead via the
  -- * RCUTILS_SET_ERROR_MSG(msg) macro.
  -- *
  -- * The error_msg parameter is copied into the internal error storage and must
  -- * be null terminated.
  -- * The file parameter is copied into the internal error storage and must
  -- * be null terminated.
  -- *
  -- * \param[in] error_string The error message to set.
  -- * \param[in] file The path to the file in which the error occurred.
  -- * \param[in] line_number The line number on which the error occurred.
  --  

   procedure rcutils_set_error_state
     (error_string : Interfaces.C.Strings.chars_ptr;
      file : Interfaces.C.Strings.chars_ptr;
      line_number : stddef_h.size_t)  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:194
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_set_error_state";

  --/ Check an argument for a null value.
  --*
  -- * If the argument's value is `NULL`, set the error message saying so and
  -- * return the `error_return_type`.
  -- *
  -- * \param[in] argument The argument to test.
  -- * \param[in] error_return_type The type to return if the argument is `NULL`.
  --  

  --/ Check a value for null, with an error message and error statement.
  --*
  -- * If `value` is `NULL`, the error statement will be evaluated after
  -- * setting the error message.
  -- *
  -- * \param[in] value The value to test.
  -- * \param[in] msg The error message if `value` is `NULL`.
  -- * \param[in] error_statement The statement to evaluate if `value` is `NULL`.
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
  --  

  --/ Set the error message using a format string and format arguments.
  --*
  -- * This function sets the error message using the given format string.
  -- * The resulting formatted string is silently truncated at
  -- * RCUTILS_ERROR_MESSAGE_MAX_LENGTH.
  -- *
  -- * \param[in] format_string The string to be used as the format of the error message.
  -- * \param[in] ... Arguments for the format string.
  --  

  --/ Indicate that the function intends to set an error message and return an error value.
  --*
  -- * \def RCUTILS_CAN_SET_MSG_AND_RETURN_WITH_ERROR_OF
  -- * Indicating macro similar to RCUTILS_CAN_RETURN_WITH_ERROR_OF, that also sets an error
  -- * message.
  -- *
  -- * For now, this macro simply relies on `RCUTILS_CAN_FAIL_WITH` to set a generic error
  -- * message and return the given `error_return_value` if fault injection is enabled.
  -- *
  -- * \param error_return_value the value returned as a result of a given error.
  --  

  --/ Return `true` if the error is set, otherwise `false`.
   function rcutils_error_is_set return Extensions.bool  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_error_is_set";

  --/ Return an rcutils_error_state_t which was set with rcutils_set_error_state().
  --*
  -- * The returned pointer will be NULL if no error has been set in this thread.
  -- *
  -- * The returned pointer is valid until RCUTILS_SET_ERROR_MSG, rcutils_set_error_state,
  -- * or rcutils_reset_error are called in the same thread.
  -- *
  -- * \return A pointer to the current error state struct.
  --  

   function rcutils_get_error_state return access constant rcutils_error_state_t  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_get_error_state";

  --/ Return the error message followed by `, at <file>:<line>` if set, else "error not set".
  --*
  -- * This function is "safe" because it returns a copy of the current error
  -- * string or one containing the string "error not set" if no error was set.
  -- * This ensures that the copy is owned by the calling thread and is therefore
  -- * never invalidated by other error handling calls, and that the C string
  -- * inside is always valid and null terminated.
  -- *
  -- * \return The current error string, with file and line number, or "error not set" if not set.
  --  

   function rcutils_get_error_string return rcutils_error_string_t  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_get_error_string";

  --/ Reset the error state by clearing any previously set error state.
   procedure rcutils_reset_error  -- /opt/ros/jazzy/include/rcutils/rcutils/error_handling.h:315
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_reset_error";

  --/ Set the error message using RCUTILS_SET_ERROR_MSG and append the previous error.
  --*
  -- * If there is no previous error, has same behavior as RCUTILS_SET_ERROR_MSG.
  -- * \param[in] msg The error message to be set.
  --  

  --/ Set the error message with RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING and append the previous
  --/ error.
  --*
  -- * This function sets the error message using the given format string, and appends and resets the
  -- * latest error string.
  -- * The resulting formatted string is silently truncated at RCUTILS_ERROR_MESSAGE_MAX_LENGTH.
  -- *
  -- * If there is no previous error, has same behavior as RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING.
  -- *
  -- * \param[in] format_string The string to be used as the format of the error message.
  -- * \param[in] ... Arguments for the format string.
  --  

  --/ Write the given msg out to stderr, limiting the buffer size in the `fwrite`, appending the
  --/ previous error.
  --*
  -- * This will reset the previous error, if it exists.
  -- * If there is no previous error, has same behavior as RCUTILS_SAFE_FWRITE_TO_STDERR.
  --  

  --/ Set the error message to stderr using a format string and format arguments, appending the
  --/ previous error.
  --*
  -- * This function sets the error message to stderr using the given format string, appending and
  -- * resetting the previous error.
  -- * The resulting formatted string is silently truncated at RCUTILS_ERROR_MESSAGE_MAX_LENGTH.
  -- *
  -- * This will reset the previous error, if it exists.
  -- * If there is no previous error, has same behavior as
  -- * RCUTILS_SAFE_FWRITE_TO_STDERR_WITH_FORMAT_STRING.
  -- *
  -- * \param[in] format_string The string to be used as the format of the error message.
  -- * \param[in] ... Arguments for the format string.
  --  

end rcutils_rcutils_error_handling_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
