pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_intn_h;

package rmw_ret_types_h is

   RMW_RET_OK : constant := 0;  --  /opt/ros/crystal/include/rmw/ret_types.h:26
   RMW_RET_ERROR : constant := 1;  --  /opt/ros/crystal/include/rmw/ret_types.h:27
   RMW_RET_TIMEOUT : constant := 2;  --  /opt/ros/crystal/include/rmw/ret_types.h:28

   RMW_RET_BAD_ALLOC : constant := 10;  --  /opt/ros/crystal/include/rmw/ret_types.h:31

   RMW_RET_INVALID_ARGUMENT : constant := 11;  --  /opt/ros/crystal/include/rmw/ret_types.h:33

   RMW_RET_INCORRECT_RMW_IMPLEMENTATION : constant := 12;  --  /opt/ros/crystal/include/rmw/ret_types.h:35

  -- Copyright 2014-2018 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   subtype rmw_ret_t is x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/crystal/include/rmw/ret_types.h:25

  --/ Failed to allocate memory return code.
  --/ Invalid argument return code.
  --/ Incorrect rmw implementation.
end rmw_ret_types_h;
