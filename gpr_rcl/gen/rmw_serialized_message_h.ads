pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;
with stddef_h;
limited with rcutils_allocator_h;

package rmw_serialized_message_h is

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
  --/ Return a zero initialized serialized message struct.
  --*
  -- * \return rmw_serialized_message_t a zero initialized serialized message struct
  --  

   function rmw_get_zero_initialized_serialized_message return rmw_types_h.rmw_serialized_message_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/serialized_message.h:36
   pragma Import (C, rmw_get_zero_initialized_serialized_message, "rmw_get_zero_initialized_serialized_message");

  --/ Initialize a zero initialized serialized message struct.
  --*
  -- * This function may leak if the serialized message struct is already
  -- * pre-initialized.
  -- * If the capacity is set to 0, no memory is allocated and the internal buffer
  -- * is still NULL.
  -- *
  -- * \param msg a pointer to the to be initialized serialized message struct
  -- * \param buffer_capacity the size of the memory to allocate for the byte stream
  -- * \param allocator the allocator to use for the memory allocation
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs
  --  

   function rmw_serialized_message_init
     (msg : access rmw_types_h.rmw_serialized_message_t;
      buffer_capacity : stddef_h.size_t;
      allocator : access constant rcutils_allocator_h.rcutils_allocator_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/serialized_message.h:54
   pragma Import (C, rmw_serialized_message_init, "rmw_serialized_message_init");

  --/ Finalize a serialized message struct.
  --*
  -- * Cleans up and deallocates any resources used in a rmw_message_serialized_t.
  -- * Passing a rmw_serialized_message_t which has not been zero initialized using
  -- * rmw_get_zero_initialized_serialized_message() to this function is undefined
  -- * behavior.
  -- *
  -- * \param msg pointer to the serialized message to be cleaned up
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_BAD_ALLOC` if memory allocation failed, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs
  --  

   function rmw_serialized_message_fini (msg : access rmw_types_h.rmw_serialized_message_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/serialized_message.h:74
   pragma Import (C, rmw_serialized_message_fini, "rmw_serialized_message_fini");

  --/ Resize the internal buffer for the message byte stream.
  --*
  -- * The internal buffer of the serialized message can be resized dynamically if needed.
  -- * If the new size is smaller than the current capacity, then the memory is
  -- * truncated.
  -- * Be aware, that this will deallocate the memory and therefore invalidates any
  -- * pointers to this storage.
  -- * If the new size is larger, new memory is getting allocated and the existing
  -- * content is copied over.
  -- *
  -- * \param msg pointer to the instance of rmw_serialized_message_t which is being resized
  -- * \param new_size the new size of the internal buffer
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_BAD_ALLOC` if memory allocation failed, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs
  --  

   function rmw_serialized_message_resize (msg : access rmw_types_h.rmw_serialized_message_t; new_size : stddef_h.size_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/serialized_message.h:95
   pragma Import (C, rmw_serialized_message_resize, "rmw_serialized_message_resize");

end rmw_serialized_message_h;
