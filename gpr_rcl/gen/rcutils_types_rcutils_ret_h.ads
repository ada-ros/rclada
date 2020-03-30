pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package rcutils_types_rcutils_ret_h is

   RCUTILS_RET_OK : constant := 0;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:24
   RCUTILS_RET_WARN : constant := 1;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:25
   RCUTILS_RET_ERROR : constant := 2;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:26

   RCUTILS_RET_BAD_ALLOC : constant := 10;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:29

   RCUTILS_RET_INVALID_ARGUMENT : constant := 11;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:31

   RCUTILS_RET_NOT_ENOUGH_SPACE : constant := 12;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:33

   RCUTILS_RET_NOT_INITIALIZED : constant := 13;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:35

   RCUTILS_RET_NOT_FOUND : constant := 14;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:37

   RCUTILS_RET_STRING_MAP_ALREADY_INIT : constant := 30;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:40

   RCUTILS_RET_STRING_MAP_INVALID : constant := 31;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:42

   RCUTILS_RET_STRING_KEY_NOT_FOUND : constant := 32;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:44

   RCUTILS_RET_LOGGING_SEVERITY_MAP_INVALID : constant := 40;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:47

   RCUTILS_RET_LOGGING_SEVERITY_STRING_INVALID : constant := 41;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:49

   RCUTILS_RET_HASH_MAP_NO_MORE_ENTRIES : constant := 50;  --  /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:52

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
   subtype rcutils_ret_t is int;  -- /opt/ros/dashing/include/rcutils/types/rcutils_ret.h:23

  --/ Failed to allocate memory return code.
  --/ Invalid argument return code.
  --/ Not enough storage to do operation.
  --/ Resource is not initialized
  --/ Resource for request not found
  --/ Given string map was either already initialized or was not zero initialized.
  --/ Given string map is invalid, perhaps not initialized yet.
  --/ Given key not found in given string map.
  --/ Internal severity map for logger thresholds is invalid.
  --/ String representation of a severity is invalid.
  --/ There are no more entires beyond the last one in the map
end rcutils_types_rcutils_ret_h;
