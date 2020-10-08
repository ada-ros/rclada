pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with stddef_h;
with rcutils_allocator_h;
with rcutils_types_rcutils_ret_h;

package rcutils_types_uint8_array_h is

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
   type rcutils_uint8_array_t is record
      buffer : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:31
      buffer_length : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:32
      buffer_capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:33
      allocator : aliased rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:34
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:29

  --/ Return a zero initialized uint8 array struct.
  --*
  -- * \return rcutils_uint8_array_t a zero initialized uint8 array struct
  --  

   function rcutils_get_zero_initialized_uint8_array return rcutils_uint8_array_t  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_get_zero_initialized_uint8_array";

  --/ Initialize a zero initialized uint8 array struct.
  --*
  -- * This function may leak if the uint8 array struct is already initialized.
  -- * If the capacity is set to 0, no memory is allocated and the internal buffer
  -- * is still NULL.
  -- *
  -- * \param[inout] uint8_array a pointer to the to be initialized uint8 array struct
  -- * \param[in] buffer_capacity the size of the memory to allocate for the byte stream
  -- * \param[in] allocator the allocator to use for the memory allocation
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENTS` if any arguments are invalid, or
  -- * \return 'RCUTILS_RET_BAD_ALLOC` if no memory could be allocated correctly
  -- * \return `RCUTILS_RET_ERROR` if an unexpected error occurs
  --  

   function rcutils_uint8_array_init
     (uint8_array : access rcutils_uint8_array_t;
      buffer_capacity : stddef_h.size_t;
      allocator : access constant rcutils_allocator_h.rcutils_allocator_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_uint8_array_init";

  --/ Finalize a uint8 array struct.
  --*
  -- * Cleans up and deallocates any resources used in a rcutils_uint8_array_t.
  -- * The array passed to this function needs to have been initialized by
  -- * rcutils_uint8_array_init().
  -- * Passing an uninitialized instance to this function leads to undefined
  -- * behavior.
  -- *
  -- * \param[in] uint8_array pointer to the rcutils_uint8_array_t to be cleaned up
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENTS` if the uint8_array argument is invalid
  -- * \return `RCUTILS_RET_ERROR` if an unexpected error occurs
  --  

   function rcutils_uint8_array_fini (uint8_array : access rcutils_uint8_array_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_uint8_array_fini";

  --/ Resize the internal buffer of the uint8 array.
  --*
  -- * The internal buffer of the uint8 array can be resized dynamically if needed.
  -- * If the new size is smaller than the current capacity, then the memory is
  -- * truncated.
  -- * Be aware, that this might deallocate the memory and therefore invalidates any
  -- * pointers to this storage.
  -- *
  -- * \param[inout] uint8_array pointer to the instance of rcutils_uint8_array_t which is
  -- * being resized
  -- * \param[in] new_size the new size of the internal buffer
  -- * \return `RCUTILS_RET_OK` if successful, or
  -- * \return `RCUTILS_RET_INVALID_ARGUMENT` if new_size is set to zero
  -- * \return `RCUTILS_RET_BAD_ALLOC` if memory allocation failed, or
  -- * \return `RCUTILS_RET_ERROR` if an unexpected error occurs
  --  

   function rcutils_uint8_array_resize (uint8_array : access rcutils_uint8_array_t; new_size : stddef_h.size_t) return rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/foxy/include/rcutils/types/uint8_array.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "rcutils_uint8_array_resize";

end rcutils_types_uint8_array_h;
