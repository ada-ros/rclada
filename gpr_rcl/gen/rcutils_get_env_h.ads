pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;

package rcutils_get_env_h is

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
  --/ Retrieve the value of the given environment variable if it exists, or "".
  -- The c-string which is returned in the env_value output parameter is only
  -- * valid until the next time this function is called, because it is a direct
  -- * pointer to the static storage.
  -- * The variable env_value populated by this function should never have free()
  -- * called on it.
  -- * If the environment variable is not set, an empty string will be returned.
  -- *
  -- * In both cases, environment variable set or unset, NULL is returned unless
  -- * an exception has occurred, in which case the error string is returned.
  -- * For example:
  -- *
  -- * ```c
  -- * #include <stdio.h>
  -- * #include <rcutils/get_env.h>
  -- * const char * env_value;
  -- * const char * error_str;
  -- * error_str = rcutils_get_env("SOME_ENV_VAR", &env_value);
  -- * if (error_str != NULL) {
  -- *   fprintf(stderr, "Error getting env var: %s\n", error_str);
  -- * }
  -- * printf("Valued of 'SOME_ENV_VAR': %s\n", env_value);
  -- * ```
  -- *
  -- * Environment variables will be truncated at 2048 characters on Windows.
  -- *
  -- * This function is not thread-safe.
  -- *
  -- * \param[in] env_name the name of the environment variable
  -- * \param[out] env_value pointer to the value cstring, or "" if unset
  -- * \return NULL on success (success can be returning an empty string)
  -- *         error string on failure
  --  

   function rcutils_get_env (env_name : Interfaces.C.Strings.chars_ptr; env_value : System.Address) return Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/get_env.h:62
   pragma Import (C, rcutils_get_env, "rcutils_get_env");

end rcutils_get_env_h;
