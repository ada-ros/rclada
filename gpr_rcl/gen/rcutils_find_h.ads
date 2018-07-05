pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;

package rcutils_find_h is

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
  --/ Return the first index of a character in a string.
  --*
  -- * Search in a string for the first occurence of a delimiter.
  -- *
  -- * \param[in] str null terminated c string to search
  -- * \param[in] delimiter the character to search for
  -- * \returns the index of the first occurence of the delimiter if found, or
  -- * \returns `SIZE_MAX` for invalid arguments or if the delimiter is not found
  --  

   function rcutils_find (str : Interfaces.C.Strings.chars_ptr; delimiter : char) return stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/find.h:37
   pragma Import (C, rcutils_find, "rcutils_find");

  --/ Return the first index of a character in a string of specified length.
  --*
  -- * Identical to rcutils_find_first() but without relying on the string to be a
  -- * null terminated c string.
  -- *
  -- * \param[in] str string to search
  -- * \param[in] delimiter the character to search for
  -- * \param[in] string_length length of the string to search
  -- * \returns the index of the first occurence of the delimiter if found, or
  -- * \returns `SIZE_MAX` for invalid arguments or if the delimiter is not found
  --  

   function rcutils_findn
     (str : Interfaces.C.Strings.chars_ptr;
      delimiter : char;
      string_length : stddef_h.size_t) return stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/find.h:52
   pragma Import (C, rcutils_findn, "rcutils_findn");

  --/ Return the last index of a character in a string.
  --*
  -- * Search in a string for the last occurence of a delimiter.
  -- *
  -- * \param[in] str null terminated c string to search
  -- * \param[in] delimiter the character to search for
  -- * \returns the index of the last occurence of the delimiter if found, or
  -- * \returns `SIZE_MAX` for invalid arguments or if the delimiter is not found
  --  

   function rcutils_find_last (str : Interfaces.C.Strings.chars_ptr; delimiter : char) return stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/find.h:65
   pragma Import (C, rcutils_find_last, "rcutils_find_last");

  --/ Return the last index of a character in a string of specifed length.
  --*
  -- * Identical to rcutils_find_last() but without relying on the string to be a
  -- * null terminated c string.
  -- *
  -- * \param[in] str string to search
  -- * \param[in] delimiter the character to search for
  -- * \param[in] string_length length of the string to search
  -- * \returns the index of the last occurence of the delimiter if found, or
  -- * \returns `SIZE_MAX` for invalid arguments or if the delimiter is not found
  --  

   function rcutils_find_lastn
     (str : Interfaces.C.Strings.chars_ptr;
      delimiter : char;
      string_length : stddef_h.size_t) return stddef_h.size_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/find.h:80
   pragma Import (C, rcutils_find_lastn, "rcutils_find_lastn");

end rcutils_find_h;
