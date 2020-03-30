pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with rcutils_allocator_h;
with rcutils_types_rcutils_ret_h;
with Interfaces.C.Strings;
with stddef_h;
with Interfaces.C.Extensions;

package rcutils_error_handling_h is

   --  arg-macro: procedure RCUTILS_SAFE_FWRITE_TO_STDERR (msg)
   --    do {fwrite(msg, sizeof(char), strlen(msg), stderr);} while (0)
   RCUTILS_ERROR_STATE_LINE_NUMBER_STR_MAX_LENGTH : constant := 20;  --  /opt/ros/dashing/include/rcutils/error_handling.h:53
   RCUTILS_ERROR_FORMATTING_CHARACTERS : constant := 6;  --  /opt/ros/dashing/include/rcutils/error_handling.h:54

   RCUTILS_ERROR_MESSAGE_MAX_LENGTH : constant := 1024;  --  /opt/ros/dashing/include/rcutils/error_handling.h:57

   RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH : constant := 768;  --  /opt/ros/dashing/include/rcutils/error_handling.h:62
   --  unsupported macro: RCUTILS_ERROR_STATE_FILE_MAX_LENGTH ( RCUTILS_ERROR_MESSAGE_MAX_LENGTH - RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH - RCUTILS_ERROR_STATE_LINE_NUMBER_STR_MAX_LENGTH - RCUTILS_ERROR_FORMATTING_CHARACTERS - 1)
   --  unsupported macro: RCUTILS_CHECK_ARGUMENT_FOR_NULL(argument,error_return_type) RCUTILS_CHECK_FOR_NULL_WITH_MSG(argument, #argument " argument is null", return error_return_type)
   --  arg-macro: procedure RCUTILS_CHECK_FOR_NULL_WITH_MSG (value, msg, error_statement)
   --    do { if (NULL = value) { RCUTILS_SET_ERROR_MSG(msg); error_statement; } } while (0)
   --  arg-macro: procedure RCUTILS_SET_ERROR_MSG (msg)
   --    do {rcutils_set_error_state(msg, __FILE__, __LINE__);} while (0)
   --  unsupported macro: RCUTILS_SET_ERROR_MSG_WITH_FORMAT_STRING(format_string,...) do { char output_msg[RCUTILS_ERROR_MESSAGE_MAX_LENGTH]; int ret = rcutils_snprintf(output_msg, sizeof(output_msg), format_string, __VA_ARGS__); if (ret < 0) { RCUTILS_SAFE_FWRITE_TO_STDERR("Failed to call snprintf for error message formatting\n"); } else { RCUTILS_SET_ERROR_MSG(output_msg); } } while (0)

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
  -- Limit the buffer size in the `fwrite` call to give an upper bound to buffer overrun in the case
  -- of non-null terminated `msg`.
  -- fixed constraints
  -- max formatted string length
  -- adjustable max length for user defined error message
  -- remember "chained" errors will include previously specified file paths
  -- e.g. "some error, at /path/to/a.c:42, at /path/to/b.c:42"
  -- with RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH = 768, RCUTILS_ERROR_STATE_FILE_MAX_LENGTH == 229
  --/ Struct wrapping a fixed-size c string used for returning the formatted error string.
   subtype rcutils_error_string_t_str_array is Interfaces.C.char_array (0 .. 1023);
   type rcutils_error_string_t is record
      str : aliased rcutils_error_string_t_str_array;  -- /opt/ros/dashing/include/rcutils/error_handling.h:74
   end record;
   pragma Convention (C_Pass_By_Copy, rcutils_error_string_t);  -- /opt/ros/dashing/include/rcutils/error_handling.h:72

  --/ Struct which encapsulates the error state set by RCUTILS_SET_ERROR_MSG().
  --/ User message storage, limited to RCUTILS_ERROR_STATE_MESSAGE_MAX_LENGTH characters.
   subtype rcutils_error_state_t_message_array is Interfaces.C.char_array (0 .. 767);
   subtype rcutils_error_state_t_file_array is Interfaces.C.char_array (0 .. 228);
   type rcutils_error_state_t is record
      message : aliased rcutils_error_state_t_message_array;  -- /opt/ros/dashing/include/rcutils/error_handling.h:81
      file : aliased rcutils_error_state_t_file_array;  -- /opt/ros/dashing/include/rcutils/error_handling.h:84
      line_number : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/dashing/include/rcutils/error_handling.h:86
   end record;
   pragma Convention (C_Pass_By_Copy, rcutils_error_state_t);  -- /opt/ros/dashing/include/rcutils/error_handling.h:78

  --/ File name, limited to what's left from RCUTILS_ERROR_STATE_MAX_SIZE characters
  --/ after subtracting storage for others.
  --/ Line number of error.
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
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` if the allocator is invalid, or
  -- * \return `RCUTILS_RET_BAD_ALLOC` if allocating memory fails, or
  -- * \return `RCUTILS_RET_ERROR` if an unspecified error occurs.
  --  

   function rcutils_initialize_error_handling_thread_local_storage (allocator : rcutils_allocator_h.rcutils_allocator_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/dashing/include/rcutils/error_handling.h:141
   pragma Import (C, rcutils_initialize_error_handling_thread_local_storage, "rcutils_initialize_error_handling_thread_local_storage");

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
      line_number : stddef_h.size_t);  -- /opt/ros/dashing/include/rcutils/error_handling.h:159
   pragma Import (C, rcutils_set_error_state, "rcutils_set_error_state");

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

  --/ Return `true` if the error is set, otherwise `false`.
   function rcutils_error_is_set return Extensions.bool;  -- /opt/ros/dashing/include/rcutils/error_handling.h:227
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

   function rcutils_get_error_state return access constant rcutils_error_state_t;  -- /opt/ros/dashing/include/rcutils/error_handling.h:241
   pragma Import (C, rcutils_get_error_state, "rcutils_get_error_state");

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

   function rcutils_get_error_string return rcutils_error_string_t;  -- /opt/ros/dashing/include/rcutils/error_handling.h:256
   pragma Import (C, rcutils_get_error_string, "rcutils_get_error_string");

  --/ Reset the error state by clearing any previously set error state.
   procedure rcutils_reset_error;  -- /opt/ros/dashing/include/rcutils/error_handling.h:261
   pragma Import (C, rcutils_reset_error, "rcutils_reset_error");

end rcutils_error_handling_h;
