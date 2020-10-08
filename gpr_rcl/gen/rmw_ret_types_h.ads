pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_intn_h;

package rmw_ret_types_h is

   RMW_RET_OK : constant := 0;  --  /opt/ros/foxy/include/rmw/ret_types.h:28

   RMW_RET_ERROR : constant := 1;  --  /opt/ros/foxy/include/rmw/ret_types.h:30

   RMW_RET_TIMEOUT : constant := 2;  --  /opt/ros/foxy/include/rmw/ret_types.h:32

   RMW_RET_UNSUPPORTED : constant := 3;  --  /opt/ros/foxy/include/rmw/ret_types.h:34

   RMW_RET_BAD_ALLOC : constant := 10;  --  /opt/ros/foxy/include/rmw/ret_types.h:37

   RMW_RET_INVALID_ARGUMENT : constant := 11;  --  /opt/ros/foxy/include/rmw/ret_types.h:39

   RMW_RET_INCORRECT_RMW_IMPLEMENTATION : constant := 12;  --  /opt/ros/foxy/include/rmw/ret_types.h:41

   RMW_RET_NODE_NAME_NON_EXISTENT : constant := 203;  --  /opt/ros/foxy/include/rmw/ret_types.h:46

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
  --/ Return code for rmw functions
   subtype rmw_ret_t is x86_64_linux_gnu_bits_stdint_intn_h.int32_t;  -- /opt/ros/foxy/include/rmw/ret_types.h:26

  --/ The operation ran as expected
  --/ Generic error to indicate operation could not complete successfully
  --/ The operation was halted early because it exceeded its timeout critera
  --/ The operation or event handling is not supported.
  --/ Failed to allocate memory
  --/ Argument to function was invalid
  --/ Incorrect rmw implementation.
  -- rmw node specific ret codes in 2XX
  --/ Failed to find node name
  -- Using same return code than in rcl
end rmw_ret_types_h;
