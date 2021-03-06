pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with rcutils_allocator_h;
with rcutils_types_rcutils_ret_h;
with Interfaces.C.Strings;
with stddef_h;
with rcutils_time_h;
with stdarg_h;
limited with rcutils_types_char_array_h;

package rcutils_logging_h is

   RCUTILS_LOGGING_SEPARATOR_CHAR : aliased constant Character := '.';  --  /opt/ros/foxy/include/rcutils/logging.h:34
   RCUTILS_LOGGING_SEPARATOR_STRING : aliased constant String := "." & ASCII.NUL;  --  /opt/ros/foxy/include/rcutils/logging.h:35
   --  unsupported macro: RCUTILS_DEFAULT_LOGGER_DEFAULT_LEVEL RCUTILS_LOG_SEVERITY_INFO
   --  unsupported macro: RCUTILS_LOGGING_AUTOINIT if (RCUTILS_UNLIKELY(!g_rcutils_logging_initialized)) { rcutils_ret_t ret = rcutils_logging_initialize(); if (ret != RCUTILS_RET_OK) { RCUTILS_SAFE_FWRITE_TO_STDERR( "[rcutils|" __FILE__ ":" RCUTILS_STRINGIFY(__LINE__) "] error initializing logging: "); RCUTILS_SAFE_FWRITE_TO_STDERR(rcutils_get_error_string().str); RCUTILS_SAFE_FWRITE_TO_STDERR("\n"); rcutils_reset_error(); } }

  -- Copyright 2017 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --*
  -- * \def RCUTILS_DEFAULT_LOGGER_DEFAULT_LEVEL
  -- * \brief The default severity level of the default logger.
  --  

  --/ The flag if the logging system has been initialized.
   g_rcutils_logging_initialized : aliased Extensions.bool  -- /opt/ros/foxy/include/rcutils/logging.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "g_rcutils_logging_initialized";

  --/ Initialize the logging system using the specified allocator.
  --*
  -- * Initialize the logging system only if it was not in an initialized state.
  -- *
  -- * If an invalid allocator is passed, the initialization will fail.
  -- * Otherwise, this function will still set the internal state to initialized
  -- * even if an error occurs, to avoid repeated failing initialization attempts
  -- * since this function is called automatically from logging macros.
  -- * To re-attempt initialization, call rcutils_logging_shutdown() before
  -- * re-calling this function.
  -- *
  -- * If multiple errors occur, the error code of the last error will be returned.
  -- *
  -- * The `RCUTILS_CONSOLE_OUTPUT_FORMAT` environment variable can be used to set
  -- * the output format of messages logged to the console.
  -- * Available tokens are:
  -- *   - `file_name`, the full file name of the caller including the path
  -- *   - `function_name`, the function name of the caller
  -- *   - `line_number`, the line number of the caller
  -- *   - `message`, the message string after it has been formatted
  -- *   - `name`, the full logger name
  -- *   - `severity`, the name of the severity level, e.g. `INFO`
  -- *   - `time`, the timestamp of log message in floating point seconds
  -- *   - `time_as_nanoseconds`, the timestamp of log message in integer nanoseconds
  -- *
  -- * The `RCUTILS_COLORIZED_OUTPUT` environment variable allows configuring if colours
  -- * are used or not. Available values are:
  -- *  - `1`: Force using colours.
  -- *  - `0`: Don't use colours.
  -- * If it is unset, colours are used depending if the target stream is a terminal or not.
  -- * See `isatty` documentation.
  -- *
  -- * The format string can use these tokens by referencing them in curly brackets,
  -- * e.g. `"[{severity}] [{name}]: {message} ({function_name}() at {file_name}:{line_number})"`.
  -- * Any number of tokens can be used.
  -- * The limit of the format string is 2048 characters.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] allocator rcutils_allocator_t to be used.
  -- * \return `RCUTILS_RET_OK` if successful.
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` if the allocator is invalid, in which
  -- *   case initialization will fail.
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` if an error occurs reading the output
  -- *   format from the `RCUTILS_CONSOLE_OUTPUT_FORMAT` environment variable, in
  -- *   which case the default format will be used.
  -- * \return `RCUTILS_RET_LOGGING_SEVERITY_MAP_INVALID` if the internal logger
  -- *   severity level map cannot be initialized, in which case logger severity
  -- *   levels will not be configurable.
  --  

   function rcutils_logging_initialize_with_allocator (allocator : rcutils_allocator_h.rcutils_allocator_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_initialize_with_allocator";

  --/ Initialize the logging system.
  --*
  -- * Call rcutils_logging_initialize_with_allocator() using the default allocator.
  -- * This function is called automatically when using the logging macros.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `RCUTILS_RET_OK` if successful.
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` if an error occurs reading the output
  -- *   format from the `RCUTILS_CONSOLE_OUTPUT_FORMAT` environment variable, in
  -- *   which case the default format will be used.
  -- * \return `RCUTILS_RET_LOGGING_SEVERITY_MAP_INVALID` if the internal logger
  -- *   severity level map cannot be initialized, in which case logger levels
  -- *   will not be configurable.
  --  

   function rcutils_logging_initialize return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_initialize";

  --/ Shutdown the logging system.
  --*
  -- * Free the resources allocated for the logging system.
  -- * This puts the system into a state equivalent to being uninitialized.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `RCUTILS_RET_OK` if successful.
  -- * \return `RCUTILS_RET_LOGGING_SEVERITY_MAP_INVALID` if the internal logger
  -- *   severity level map cannot be finalized.
  --  

   function rcutils_logging_shutdown return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_shutdown";

  --/ The structure identifying the caller location in the source code.
  --/ The name of the function containing the log call.
   type rcutils_log_location_t is record
      function_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rcutils/logging.h:157
      file_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rcutils/logging.h:159
      line_number : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rcutils/logging.h:161
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcutils/logging.h:154

  --/ The name of the source file containing the log call.
  --/ The line number containing the log call.
  --/ The severity levels of log messages / loggers.
   subtype RCUTILS_LOG_SEVERITY is unsigned;
   RCUTILS_LOG_SEVERITY_UNSET : constant unsigned := 0;
   RCUTILS_LOG_SEVERITY_DEBUG : constant unsigned := 10;
   RCUTILS_LOG_SEVERITY_INFO : constant unsigned := 20;
   RCUTILS_LOG_SEVERITY_WARN : constant unsigned := 30;
   RCUTILS_LOG_SEVERITY_ERROR : constant unsigned := 40;
   RCUTILS_LOG_SEVERITY_FATAL : constant unsigned := 50;  -- /opt/ros/foxy/include/rcutils/logging.h:165

  --/< The unset log level
  --/< The debug log level
  --/< The info log level
  --/< The warn log level
  --/< The error log level
  --/< The fatal log level
  --/ The names of severity levels.
   g_rcutils_log_severity_names : array (0 .. 50) of Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcutils/logging.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "g_rcutils_log_severity_names";

  --/ Get a severity value from its string representation (e.g. DEBUG).
  --*
  -- * String representation must match one of the values in
  -- * `g_rcutils_log_severity_names`, but is not case-sensitive.
  -- * Examples: UNSET, DEBUG, INFO, WARN, Error, fatal.
  -- *
  -- * \param[in] severity_string String representation of the severity, must be a
  -- *   null terminated c string
  -- * \param[in] allocator rcutils_allocator_t to be used
  -- * \param[in,out] severity The severity level as a represented by the
  -- *   `RCUTILS_LOG_SEVERITY` enum
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` on invalid arguments, or
  -- * \return `RCUTILS_RET_LOGGING_SEVERITY_STRING_INVALID` if unable to match
  -- *   string, or
  -- * \return `RCUTILS_RET_ERROR` if an unspecified error occured
  --  

   function rcutils_logging_severity_level_from_string
     (severity_string : Interfaces.C.Strings.chars_ptr;
      allocator : rcutils_allocator_h.rcutils_allocator_t;
      severity : access int) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_severity_level_from_string";

  --/ The function signature to log messages.
  --*
  -- * \param[in] location The location information about where the log came from
  -- * \param[in] severity The severity of the log message expressed as an integer
  -- * \param[in] name The name of the logger that this message came from
  -- * \param[in] timestamp The time at which the log message was generated
  -- * \param[in] format The list of arguments to insert into the formatted log message
  -- * \param[in] args The variable argument list
  --  

   type rcutils_logging_output_handler_t is access procedure
        (arg1 : access constant rcutils_log_location_t;
         arg2 : int;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : rcutils_time_h.rcutils_time_point_value_t;
         arg5 : Interfaces.C.Strings.chars_ptr;
         arg6 : access stdarg_h.va_list)
   with Convention => C;  -- /opt/ros/foxy/include/rcutils/logging.h:211

  -- location
  -- severity
  -- name
  -- timestamp
  -- format
  -- args
  --/ The function pointer of the current output handler.
   g_rcutils_logging_output_handler : rcutils_logging_output_handler_t  -- /opt/ros/foxy/include/rcutils/logging.h:222
   with Import => True, 
        Convention => C, 
        External_Name => "g_rcutils_logging_output_handler";

  --/ Get the current output handler.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return The function pointer of the current output handler.
  --  

   function rcutils_logging_get_output_handler return rcutils_logging_output_handler_t  -- /opt/ros/foxy/include/rcutils/logging.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_get_output_handler";

  --/ Set the current output handler.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] function The function pointer of the output handler to be used.
  --  

   procedure rcutils_logging_set_output_handler (c_function : rcutils_logging_output_handler_t)  -- /opt/ros/foxy/include/rcutils/logging.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_set_output_handler";

  --/ Formats a log message according to RCUTILS_CONSOLE_OUTPUT_FORMAT
  --*
  -- * A formatter that is meant to be used by an output handler to format a log message to the match
  -- * the format specified in RCUTILS_CONSOLE_OUTPUT_FORMAT by performing token replacement.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `RCUTILS_RET_OK` if successful.
  -- * \return `RCUTILS_RET_BAD_ALLOC` if memory allocation error occured
  -- * \param[in] location The location information about where the log came from
  -- * \param[in] severity The severity of the log message expressed as an integer
  -- * \param[in] name The name of the logger that this message came from
  -- * \param[in] timestamp The time at which the log message was generated
  -- * \param[in] msg The message being logged
  -- * \param[out] logging_output An output buffer for the formatted message
  --  

   function rcutils_logging_format_message
     (location : access constant rcutils_log_location_t;
      severity : int;
      name : Interfaces.C.Strings.chars_ptr;
      timestamp : rcutils_time_h.rcutils_time_point_value_t;
      msg : Interfaces.C.Strings.chars_ptr;
      logging_output : access rcutils_types_char_array_h.rcutils_char_array_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_format_message";

  --/ The default severity level for loggers.
  --*
  -- * This level is used for (1) nameless log calls and (2) named log
  -- * calls where the effective level of the logger name is unspecified.
  -- *
  -- * \see rcutils_logging_get_logger_effective_level()
  --  

   g_rcutils_logging_default_logger_level : aliased int  -- /opt/ros/foxy/include/rcutils/logging.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "g_rcutils_logging_default_logger_level";

  --/ Get the default level for loggers.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return The level.
  --  

   function rcutils_logging_get_default_logger_level return int  -- /opt/ros/foxy/include/rcutils/logging.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_get_default_logger_level";

  --/ Set the default severity level for loggers.
  --*
  -- * If the severity level requested is `RCUTILS_LOG_SEVERITY_UNSET`, the default
  -- * value for the default logger (`RCUTILS_DEFAULT_LOGGER_DEFAULT_LEVEL`)
  -- * will be restored instead.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] level The level to be used.
  --  

   procedure rcutils_logging_set_default_logger_level (level : int)  -- /opt/ros/foxy/include/rcutils/logging.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_set_default_logger_level";

  --/ Get the severity level for a logger.
  --*
  -- * This considers the severity level of the specifed logger only.
  -- * To get the effective level of a logger given the severity level of its
  -- * ancestors, see rcutils_logging_get_logger_effective_level().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] name The name of the logger, must be null terminated c string
  -- * \return The level of the logger if it has been set, or
  -- * \return `RCUTILS_LOG_SEVERITY_UNSET` if unset, or
  -- * \return `g_rcutils_logging_default_logger_level` for an empty name, or
  -- * \return -1 on invalid arguments, or
  -- * \return -1 if an error occurred
  --  

   function rcutils_logging_get_logger_level (name : Interfaces.C.Strings.chars_ptr) return int  -- /opt/ros/foxy/include/rcutils/logging.h:352
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_get_logger_level";

  --/ Get the level for a logger and its name length.
  --*
  -- * Identical to rcutils_logging_get_logger_level() but without
  -- * relying on the logger name to be a null terminated c string.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] name The name of the logger
  -- * \param[in] name_length Logger name length
  -- * \return The level of the logger if it has been set, or
  -- * \return `RCUTILS_LOG_SEVERITY_UNSET` if unset, or
  -- * \return `g_rcutils_logging_default_logger_level` for `name_length` of `0`, or
  -- * \return -1 on invalid arguments, or
  -- * \return -1 if an error occurred
  --  

   function rcutils_logging_get_logger_leveln (name : Interfaces.C.Strings.chars_ptr; name_length : stddef_h.size_t) return int  -- /opt/ros/foxy/include/rcutils/logging.h:377
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_get_logger_leveln";

  --/ Set the severity level for a logger.
  --*
  -- * If an empty string is specified as the name, the
  -- * `g_rcutils_logging_default_logger_level` will be set.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] name The name of the logger, must be null terminated c string.
  -- * \param[in] level The level to be used.
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` on invalid arguments, or
  -- * \return `RCUTILS_RET_LOGGING_SEVERITY_MAP_INVALID` if severity map invalid, or
  -- * \return `RCUTILS_RET_ERROR` if an unspecified error occured
  --  

   function rcutils_logging_set_logger_level (name : Interfaces.C.Strings.chars_ptr; level : int) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/logging.h:401
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_set_logger_level";

  --/ Determine if a logger is enabled for a severity level.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] name The name of the logger, must be null terminated c string or NULL.
  -- * \param[in] severity The severity level.
  -- *
  -- * \return true if the logger is enabled for the level; false otherwise.
  --  

   function rcutils_logging_logger_is_enabled_for (name : Interfaces.C.Strings.chars_ptr; severity : int) return Extensions.bool  -- /opt/ros/foxy/include/rcutils/logging.h:420
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_logger_is_enabled_for";

  --/ Determine the effective level for a logger.
  --*
  -- * The effective level is determined as the severity level of
  -- * the logger if it is set, otherwise it is the first specified severity
  -- * level of the logger's ancestors, starting with its closest ancestor.
  -- * The ancestor hierarchy is signified by logger names being separated by dots:
  -- * a logger named `x` is an ancestor of `x.y`, and both `x` and `x.y` are
  -- * ancestors of `x.y.z`, etc.
  -- * If the level has not been set for the logger nor any of its
  -- * ancestors, the default level is used.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, provided logging system is already initialized
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] name The name of the logger, must be null terminated c string.
  -- *
  -- * \return The level, or
  -- * \return -1 on invalid arguments, or
  -- * \return -1 if an error occurred.
  --  

   function rcutils_logging_get_logger_effective_level (name : Interfaces.C.Strings.chars_ptr) return int  -- /opt/ros/foxy/include/rcutils/logging.h:449
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_get_logger_effective_level";

  --/ Log a message.
  --*
  -- * The attributes of this function are also being influenced by the currently
  -- * set output handler.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No, for formatted outputs <= 1023 characters
  -- *                    | Yes, for formatted outputs >= 1024 characters
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] location The pointer to the location struct or NULL
  -- * \param[in] severity The severity level
  -- * \param[in] name The name of the logger, must be null terminated c string or NULL
  -- * \param[in] format The format string
  -- * \param[in] ... The variable arguments
  --  

   procedure rcutils_log
     (location : access constant rcutils_log_location_t;
      severity : int;
      name : Interfaces.C.Strings.chars_ptr;
      format : Interfaces.C.Strings.chars_ptr  -- , ...
      )  -- /opt/ros/foxy/include/rcutils/logging.h:472
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_log";

  --/ The default output handler outputs log messages to the standard streams.
  --*
  -- * The messages with a severity level `DEBUG` and `INFO` are written to `stdout`.
  -- * The messages with a severity level `WARN`, `ERROR`, and `FATAL` are written
  -- * to `stderr`.
  -- * The console output format of the logged message can be configured through
  -- * the `RCUTILS_CONSOLE_OUTPUT_FORMAT` environment variable: see
  -- * rcutils_logging_initialize_with_allocator() for details.
  -- * For configuring if using colours or not, `RCUTILS_COLORIZED_OUTPUT` can be used:
  -- * see rcutils_logging_initialize_with_allocator() for details.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes, if the underlying *printf functions are
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] location The pointer to the location struct or NULL
  -- * \param[in] severity The severity level
  -- * \param[in] name The name of the logger, must be null terminated c string
  -- * \param[in] timestamp The timestamp for when the log message was made
  -- * \param[in] format The format string
  -- * \param[in] args The `va_list` used by the logger
  --  

   procedure rcutils_logging_console_output_handler
     (location : access constant rcutils_log_location_t;
      severity : int;
      name : Interfaces.C.Strings.chars_ptr;
      timestamp : rcutils_time_h.rcutils_time_point_value_t;
      format : Interfaces.C.Strings.chars_ptr;
      args : access stdarg_h.va_list)  -- /opt/ros/foxy/include/rcutils/logging.h:507
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_logging_console_output_handler";

  --*
  -- * \def RCUTILS_LOGGING_AUTOINIT
  -- * \brief Initialize the rcl logging library.
  -- * Usually it is unnecessary to call the macro directly.
  -- * All logging macros ensure that this has been called once.
  --  

end rcutils_logging_h;
