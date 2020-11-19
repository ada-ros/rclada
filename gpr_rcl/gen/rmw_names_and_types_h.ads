pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_types_string_array_h;
with rmw_ret_types_h;
with stddef_h;
limited with rcutils_allocator_h;

package rmw_names_and_types_h is

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
  --/ Associative array of topic or service names and types.
  --/ Array of names
   type rmw_names_and_types_t is record
      names : aliased rcutils_types_string_array_h.rcutils_string_array_t;  -- /opt/ros/foxy/include/rmw/names_and_types.h:35
      types : access rcutils_types_string_array_h.rcutils_string_array_t;  -- /opt/ros/foxy/include/rmw/names_and_types.h:38
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/names_and_types.h:32

  --/ Dynamic array of arrays of type names, with the same length as `names`
  --/ Return a zero initialized array of names and types.
   function rmw_get_zero_initialized_names_and_types return rmw_names_and_types_t  -- /opt/ros/foxy/include/rmw/names_and_types.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_names_and_types";

  --/ Check that the given `names_and_types` array is zero initialized.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \par Thread-safety
  -- *   Access to the array of names and types is read-only, but it is not synchronized.
  -- *   Concurrent `names_and_types` reads are safe, but concurrent reads and writes are not.
  -- *
  -- * \param[in] names_and_types Array to be checked.
  -- * \return RMW_RET_OK if array is zero initialized, RMW_RET_INVALID_ARGUMENT otherwise.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_names_and_types_check_zero (names_and_types : access rmw_names_and_types_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/names_and_types.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_names_and_types_check_zero";

  --/ Initialize an array of names and types.
  --*
  -- * This function initializes the string array for the names and allocates space
  -- * for all the string arrays for the types according to the given size, but
  -- * it does not initialize the string array for each setup of types.
  -- * However, the string arrays for each set of types is zero initialized.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \par Thread-safety
  -- *   Initialization is a reentrant procedure, but:
  -- *   - Access to arrays of names and types is not synchronized.
  -- *     It is not safe to read or write `names_and_types` during initialization.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \param[inout] names_and_types Array to be initialized on success,
  -- *   but left unchanged on failure.
  -- * \param[in] size Size of the array.
  -- * \param[in] allocator Allocator to be used to populate `names_and_types`.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `names_and_types` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `names_and_types` is not
  -- *   a zero initialized array, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `allocator` is invalid,
  -- *   by rcutils_allocator_is_valid() definition, or
  -- * \returns `RMW_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_names_and_types_init
     (names_and_types : access rmw_names_and_types_t;
      size : stddef_h.size_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/names_and_types.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_names_and_types_init";

  --/ Finalize an array of names and types.
  --*
  -- * This function deallocates the string array of names and the array of string arrays of types,
  -- * and zero initializes the given array.
  -- * If a logical error, such as `RMW_RET_INVALID_ARGUMENT`, ensues, this function will return
  -- * early, leaving the given array unchanged.
  -- * Otherwise, it will proceed despite errors.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \par Thread-safety
  -- *   Finalization is a reentrant procedure, but access to arrays of names and types
  -- *   is not synchronized.
  -- *   It is not safe to read or write `names_and_types` during initialization.
  -- *
  -- * \param[inout] names_and_types Array to be finalized.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `names_and_types` is NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_names_and_types_fini (names_and_types : access rmw_names_and_types_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/names_and_types.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_names_and_types_fini";

end rmw_names_and_types_h;
