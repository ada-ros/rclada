pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;
with rcl_types_h;

package rcl_validate_enclave_name_h is

   --  unsupported macro: RCL_ENCLAVE_NAME_VALID RMW_NAMESPACE_VALID
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_IS_EMPTY_STRING RMW_NAMESPACE_INVALID_IS_EMPTY_STRING
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_NOT_ABSOLUTE RMW_NAMESPACE_INVALID_NOT_ABSOLUTE
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_ENDS_WITH_FORWARD_SLASH RMW_NAMESPACE_INVALID_ENDS_WITH_FORWARD_SLASH
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_CONTAINS_UNALLOWED_CHARACTERS RMW_NAMESPACE_INVALID_CONTAINS_UNALLOWED_CHARACTERS
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_CONTAINS_REPEATED_FORWARD_SLASH RMW_NAMESPACE_INVALID_CONTAINS_REPEATED_FORWARD_SLASH
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_NAME_TOKEN_STARTS_WITH_NUMBER RMW_NAMESPACE_INVALID_NAME_TOKEN_STARTS_WITH_NUMBER
   --  unsupported macro: RCL_ENCLAVE_NAME_INVALID_TOO_LONG RMW_NAMESPACE_INVALID_TOO_LONG
   --  unsupported macro: RCL_ENCLAVE_NAME_MAX_LENGTH RMW_NODE_NAME_MAX_NAME_LENGTH
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
  --/ Determine if a given enclave name is valid.
  --*
  -- * The same rules as \ref rmw_validate_namespace are used.
  -- * The only difference is in the maximum allowed length, which can be up to 255 characters.
  -- *
  -- * \param[in] enclave enclave to be validated
  -- * \param[out] validation_result int in which the result of the check is stored
  -- * \param[out] invalid_index index of the input string where an error occurred
  -- * \returns `RMW_RET_OK` on successfully running the check, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` on invalid parameters, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rcl_validate_enclave_name
     (enclave : Interfaces.C.Strings.chars_ptr;
      validation_result : access int;
      invalid_index : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/validate_enclave_name.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_validate_enclave_name";

  --/ Deterimine if a given enclave name is valid.
  --*
  -- * This is an overload of \ref rcl_validate_enclave_name with an extra parameter
  -- * for the length of enclave.
  -- *
  -- * \param[in] enclave_length The number of characters in enclave.
  --  

   function rcl_validate_enclave_name_with_size
     (enclave : Interfaces.C.Strings.chars_ptr;
      enclave_length : stddef_h.size_t;
      validation_result : access int;
      invalid_index : access stddef_h.size_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/validate_enclave_name.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_validate_enclave_name_with_size";

  --/ Return a validation result description, or NULL if unknown or RCL_ENCLAVE_NAME_VALID.
   function rcl_enclave_name_validation_result_string (validation_result : int) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/foxy/include/rcl/validate_enclave_name.h:85
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_enclave_name_validation_result_string";

end rcl_validate_enclave_name_h;
