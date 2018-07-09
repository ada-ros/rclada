pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_error_handling_h;
with Interfaces.C.Strings;
with stddef_h;

package rmw_error_handling_h is

   --  unsupported macro: rmw_error_state_copy rcutils_error_state_copy
   --  unsupported macro: rmw_error_state_fini rcutils_error_state_fini
   --  arg-macro: procedure RMW_SET_ERROR_MSG (msg)
   --    rmw_set_error_state(msg, __FILE__, __LINE__);
   --  arg-macro: procedure RMW_SET_ERROR_MSG_ALLOC (msg, allocator)
   --    rcutils_set_error_state(msg, __FILE__, __LINE__, allocator);
   --  unsupported macro: rmw_error_is_set rcutils_error_is_set
   --  unsupported macro: rmw_get_error_state rcutils_get_error_state
   --  unsupported macro: rmw_get_error_string rcutils_get_error_string
   --  unsupported macro: rmw_get_error_string_safe rcutils_get_error_string_safe
   --  unsupported macro: rmw_reset_error rcutils_reset_error
  -- Copyright 2014 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   subtype rmw_error_state_t is rcutils_error_handling_h.rcutils_error_state_t;  -- /opt/ros/bouncy/include/rmw/error_handling.h:30

  -- TODO(wjwwood): replace this completely with rcutils_set_error_state()
  --                once the rmw APIs take an allocator that can be passed
  --                by the rmw implementations on to the error functions
  --/ Set the error state, implicitly uses rcutils_get_default_allocator().
  --*
  -- * \see rcutils_get_default_allocator()
  -- * \see rcutils_set_error_state()
  --  

   procedure rmw_set_error_state
     (error_msg : Interfaces.C.Strings.chars_ptr;
      file : Interfaces.C.Strings.chars_ptr;
      line_number : stddef_h.size_t);  -- /opt/ros/bouncy/include/rmw/error_handling.h:46
   pragma Import (C, rmw_set_error_state, "rmw_set_error_state");

  --/ Set the error message, as well as append the current file and line number.
  --*
  -- * \see RCUTILS_SET_ERROR_MSG
  --  

end rmw_error_handling_h;
