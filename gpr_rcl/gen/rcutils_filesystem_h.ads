pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with stddef_h;
with Interfaces.C.Extensions;
with rcutils_allocator_h;

package rcutils_filesystem_h is

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
  --/ Return current working directory.
  --*
  -- * \param[in] buffer Allocated string to store current directory path to
  -- * \param[in] max_length maximum length to be stored in buffer
  -- * \return bool True if success
  -- *              False if buffer is NULL
  -- *              False on failure
  --  

   function rcutils_get_cwd (buffer : Interfaces.C.Strings.chars_ptr; max_length : stddef_h.size_t) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:41
   pragma Import (C, rcutils_get_cwd, "rcutils_get_cwd");

  --/ Check if the provided path points to a directory.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if directory
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_is_directory (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:52
   pragma Import (C, rcutils_is_directory, "rcutils_is_directory");

  --/ Check if the provided path points to a file.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if file
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_is_file (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:63
   pragma Import (C, rcutils_is_file, "rcutils_is_file");

  --/ Check if the provided path points to an existing file/folder.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if the path exists
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_exists (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:74
   pragma Import (C, rcutils_exists, "rcutils_exists");

  --/ Check if the provided path points to a file/folder readable by current user.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if the file is readable
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_is_readable (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:85
   pragma Import (C, rcutils_is_readable, "rcutils_is_readable");

  --/ Check if the provided path points to a file/folder writable by current user.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if the file is writable
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_is_writable (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:96
   pragma Import (C, rcutils_is_writable, "rcutils_is_writable");

  --/ Check if the provided path points to a file/folder both readable and writable by current user.
  --*
  -- * \param[in] abs_path Absolute path to check.
  -- * \return bool True if the file is redable and writable False otherwise
  -- *              False if abs_path is NULL
  -- *              False on failure
  --  

   function rcutils_is_readable_and_writable (abs_path : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:107
   pragma Import (C, rcutils_is_readable_and_writable, "rcutils_is_readable_and_writable");

  --/ Return newly allocated string with arguments separated by correct delimiter for the platform.
  --*
  -- * This function allocates memory and returns it to the caller.
  -- * It is up to the caller to release the memory once it is done with it by
  -- * calling `deallocate` on the same allocator passed here.
  -- *
  -- * \param[in] left_hand_path
  -- * \param[in] right_hand_path
  -- * \param[in] allocator
  -- * \return char * concatenated path on success
  -- *         NULL on invalid arguments
  -- *         NULL on failure
  --  

   function rcutils_join_path
     (left_hand_path : Interfaces.C.Strings.chars_ptr;
      right_hand_path : Interfaces.C.Strings.chars_ptr;
      allocator : rcutils_allocator_h.rcutils_allocator_t) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/bouncy/include/rcutils/filesystem.h:124
   pragma Import (C, rcutils_join_path, "rcutils_join_path");

end rcutils_filesystem_h;
