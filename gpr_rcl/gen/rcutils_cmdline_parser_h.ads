pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package rcutils_cmdline_parser_h is

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
   function rcutils_cli_option_exist
     (c_begin : System.Address;
      c_end : System.Address;
      option : Interfaces.C.Strings.chars_ptr) return Extensions.bool;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/cmdline_parser.h:29
   pragma Import (C, rcutils_cli_option_exist, "rcutils_cli_option_exist");

   function rcutils_cli_get_option
     (c_begin : System.Address;
      c_end : System.Address;
      option : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /home/jano/local/ros2/ros2_bouncy/install/rcutils/include/rcutils/cmdline_parser.h:33
   pragma Import (C, rcutils_cli_get_option, "rcutils_cli_get_option");

end rcutils_cmdline_parser_h;
