pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;
with rmw_types_h;

package rmw_validate_namespace_h is

   RMW_NAMESPACE_VALID : constant := 0;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:27
   RMW_NAMESPACE_INVALID_IS_EMPTY_STRING : constant := 1;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:28
   RMW_NAMESPACE_INVALID_NOT_ABSOLUTE : constant := 2;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:29
   RMW_NAMESPACE_INVALID_ENDS_WITH_FORWARD_SLASH : constant := 3;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:30
   RMW_NAMESPACE_INVALID_CONTAINS_UNALLOWED_CHARACTERS : constant := 4;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:31
   RMW_NAMESPACE_INVALID_CONTAINS_REPEATED_FORWARD_SLASH : constant := 5;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:32
   RMW_NAMESPACE_INVALID_NAME_TOKEN_STARTS_WITH_NUMBER : constant := 6;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:33
   RMW_NAMESPACE_INVALID_TOO_LONG : constant := 7;  --  /opt/ros/bouncy/include/rmw/validate_namespace.h:34
   --  unsupported macro: RMW_NAMESPACE_MAX_LENGTH (RMW_TOPIC_MAX_NAME_LENGTH - 2U)

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
  -- An additional 2 characters are reserved for the shortest possible topic, e.g. '/X'.
  --/ Determine if a given namespace is valid.
  --* Validity of a namespace is based on rules for a topic defined here:
  -- *
  -- *   http://design.ros2.org/articles/topic_and_service_names.html
  -- *
  -- * Note that this function expects that there are no URL suffixes as described
  -- * in the above document which can be used for topics and services.
  -- *
  -- * If either the C string or validation_result pointer are null, then
  -- * `RMW_RET_INVALID_ARGUMENT` will be returned.
  -- * The namespace_ should be a valid, null-terminated C string.
  -- * The validation_result int pointer should point to valid memory so a result
  -- * can be stored in it as an output variable.
  -- * The invalid_index size_t pointer should either point NULL or to valid memory
  -- * so in the event of a validation error, the location in the input string can
  -- * be stored therein.
  -- * If NULL is passed in for invalid_index, it will be not be set.
  -- *
  -- * The invalid_index will not be assigned a value if the namespace is valid.
  -- *
  -- * The int which validation_result points to will have a one of a few possible
  -- * results values (defined with macros) stored into it:
  -- *
  -- * - RMW_NAMESPACE_VALID
  -- * - RMW_NAMESPACE_INVALID_IS_EMPTY_STRING
  -- * - RMW_NAMESPACE_INVALID_NOT_ABSOLUTE
  -- * - RMW_NAMESPACE_INVALID_ENDS_WITH_FORWARD_SLASH
  -- * - RMW_NAMESPACE_INVALID_CONTAINS_UNALLOWED_CHARACTERS
  -- * - RMW_NAMESPACE_INVALID_CONTAINS_REPEATED_FORWARD_SLASH
  -- * - RMW_NAMESPACE_INVALID_NAME_TOKEN_STARTS_WITH_NUMBER
  -- * - RMW_NAMESPACE_INVALID_TOO_LONG
  -- *
  -- * The result value can be converted to a description with the
  -- * rmw_namespace_validation_result_string() function.
  -- *
  -- * The ``RMW_NAMESPACE_INVALID_ENDS_WITH_FORWARD_SLASH`` validation result does
  -- * not apply to ``"/"``, which is a valid namespace.
  -- *
  -- * The ``RMW_NAMESPACE_INVALID_TOO_LONG`` is guaranteed to be checked last,
  -- * such that if you get that result, then you can assume all other checks
  -- * succeeded.
  -- * This is done so that the length limit can be treated as a warning rather
  -- * than an error if desired.
  -- *
  -- * \param[in] namespace_ namespace to be validated
  -- * \param[out] validation_result int in which the result of the check is stored
  -- * \param[out] invalid_index index of the input string where an error occurred
  -- * \returns `RMW_RET_OK` on successfully running the check, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` on invalid parameters, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_validate_namespace
     (namespace_u : Interfaces.C.Strings.chars_ptr;
      validation_result : access int;
      invalid_index : access stddef_h.size_t) return rmw_types_h.rmw_ret_t;  -- /opt/ros/bouncy/include/rmw/validate_namespace.h:93
   pragma Import (C, rmw_validate_namespace, "rmw_validate_namespace");

  --/ Deterimine if a given namespace is valid.
  --*
  -- * This is an overload with an extra parameter for the length of namespace_.
  -- * \param[in] namespace_length The number of characters in namespace_.
  -- *
  -- * \sa rmw_validate_namespace(const char *, int *, size_t *)
  --  

   function rmw_validate_namespace_with_size
     (namespace_u : Interfaces.C.Strings.chars_ptr;
      namespace_length : stddef_h.size_t;
      validation_result : access int;
      invalid_index : access stddef_h.size_t) return rmw_types_h.rmw_ret_t;  -- /opt/ros/bouncy/include/rmw/validate_namespace.h:108
   pragma Import (C, rmw_validate_namespace_with_size, "rmw_validate_namespace_with_size");

  --/ Return a validation result description, or NULL if unknown or RMW_NAMESPACE_VALID.
   function rmw_namespace_validation_result_string (validation_result : int) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rmw/validate_namespace.h:118
   pragma Import (C, rmw_namespace_validation_result_string, "rmw_namespace_validation_result_string");

end rmw_validate_namespace_h;
