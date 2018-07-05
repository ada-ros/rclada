pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with rcutils_allocator_h;
with stddef_h;

package rcutils_strdup_h is

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
  --/ Return a duplicated string with an allocator, or null if an error occurs.
  --*
  -- * This function is identical to rcutils_strndup() except the length of the
  -- * c string does not have to be given and therefore the c string must be
  -- * null terminated.
  -- *
  -- * \see rcutils_strndup()
  -- *
  -- * \param[in] str null terminated c string to be duplicated
  -- * \param[in] allocator the allocator to use for allocation
  -- * \returns duplicated string or null if there is an error
  --  

   function rcutils_strdup (str : Interfaces.C.Strings.chars_ptr; allocator : rcutils_allocator_h.rcutils_allocator_t) return Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/strdup.h:44
   pragma Import (C, rcutils_strdup, "rcutils_strdup");

  --/ Return a duplicated string with an allocator, or null if an error occurs.
  --*
  -- * This function can fail and return null if memory cannot be allocated or
  -- * if the input c string pointer is null.
  -- * In both cases no error message is set.
  -- * The returned string should be deallocated using the given allocator when
  -- * it is no longer needed.
  -- *
  -- * The string_length given does not include the null terminating character.
  -- * Therefore a string_length of 0 will still result in a duplicated string, but
  -- * the string will be an empty string of strlen 0, but it still must be
  -- * deallocated.
  -- * All returned strings are null terminated.
  -- *
  -- * \param[in] str null terminated c string to be duplicated
  -- * \param[in] string_length length of the string to duplicate
  -- * \param[in] allocator the allocator to use for allocation
  -- * \returns duplicated string or null if there is an error
  --  

   function rcutils_strndup
     (str : Interfaces.C.Strings.chars_ptr;
      string_length : stddef_h.size_t;
      allocator : rcutils_allocator_h.rcutils_allocator_t) return Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/strdup.h:68
   pragma Import (C, rcutils_strndup, "rcutils_strndup");

end rcutils_strdup_h;
