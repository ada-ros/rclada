pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with rcutils_allocator_h;
limited with rcutils_types_string_array_h;
with rcutils_types_rcutils_ret_h;

package rcutils_split_h is

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
  --/ Split a given string with the specified delimiter
  --*
  -- * \param[in] str string to split
  -- * \param[in] delimiter on where to split
  -- * \param[in] allocator for allocating new memory for the output array
  -- * \param[out] string_array with the split tokens
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` for invalid arguments, or
  -- * \return `RCUTILS_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \return `RCUTILS_RET_ERROR` if an unknown error occurs
  --  

   function rcutils_split
     (str : Interfaces.C.Strings.chars_ptr;
      delimiter : char;
      allocator : rcutils_allocator_h.rcutils_allocator_t;
      string_array : access rcutils_types_string_array_h.rcutils_string_array_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/split.h:40
   pragma Import (C, rcutils_split, "rcutils_split");

  --/ Split a given string on the last occurrence of the specified delimiter
  --*
  -- * \param[in] str string to split
  -- * \param[in] delimiter on where to split
  -- * \param[in] allocator for allocating new memory for the output array
  -- * \param[out] string_array with the split tokens
  -- * \returns array with split token, NULL in case of error
  --  

   function rcutils_split_last
     (str : Interfaces.C.Strings.chars_ptr;
      delimiter : char;
      allocator : rcutils_allocator_h.rcutils_allocator_t;
      string_array : access rcutils_types_string_array_h.rcutils_string_array_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/split.h:56
   pragma Import (C, rcutils_split_last, "rcutils_split_last");

end rcutils_split_h;
