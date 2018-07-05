pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_allocator_h;

package rcl_allocator_h is

   --  unsupported macro: rcl_get_default_allocator rcutils_get_default_allocator
   --  unsupported macro: rcl_reallocf rcutils_reallocf
   --  arg-macro: procedure RCL_CHECK_ALLOCATOR (allocator, fail_statement)
   --    RCUTILS_CHECK_ALLOCATOR(allocator, fail_statement)
   --  arg-macro: procedure RCL_CHECK_ALLOCATOR_WITH_MSG (allocator, msg, fail_statement)
   --    RCUTILS_CHECK_ALLOCATOR_WITH_MSG(allocator, msg, fail_statement)
  -- Copyright 2015 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   subtype rcl_allocator_t is rcutils_allocator_h.rcutils_allocator_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rcl/include/rcl/allocator.h:25

end rcl_allocator_h;
