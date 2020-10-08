pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_types_uint8_array_h;

package rmw_serialized_message_h is

   --  unsupported macro: rmw_get_zero_initialized_serialized_message rcutils_get_zero_initialized_uint8_array
   --  unsupported macro: rmw_serialized_message_init rcutils_uint8_array_init
   --  unsupported macro: rmw_serialized_message_fini rcutils_uint8_array_fini
   --  unsupported macro: rmw_serialized_message_resize rcutils_uint8_array_resize
  -- Copyright 2018 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  -- aliases for rcutils_uint8_array_t
  -- * For now this is a simple aliasing from a serialized message to a uint8 array.
  -- * However, in future developments this serialized message can become something
  -- * more complex and is therefore aliased.
  --  

   subtype rmw_serialized_message_t is rcutils_types_uint8_array_h.rcutils_uint8_array_t;  -- /opt/ros/foxy/include/rmw/serialized_message.h:31

end rmw_serialized_message_h;
