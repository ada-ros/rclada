pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;
limited with rmw_topic_endpoint_info_h;
with rmw_ret_types_h;
limited with rcutils_allocator_h;

package rmw_topic_endpoint_info_array_h is

  -- Copyright 2019 Amazon.com, Inc. or its affiliates. All Rights Reserved.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Array of topic endpoint information
  --/ Size of the array.
   type rmw_topic_endpoint_info_array_t is record
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:31
      info_array : access rmw_topic_endpoint_info_h.rmw_topic_endpoint_info_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:28

  --/ Contiguous storage for topic endpoint information elements.
  --/ Return a zero initialized array of topic endpoint information.
   function rmw_get_zero_initialized_topic_endpoint_info_array return rmw_topic_endpoint_info_array_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_topic_endpoint_info_array";

  --/ Check that the given `topic_endpoint_info_array` is zero initialized.
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
  -- *   Access to the array of topic endpoint information is read-only, but it is not synchronized.
  -- *   Concurrent `topic_endpoint_info_array` reads are safe, but concurrent reads
  -- *   and writes are not.
  -- *
  -- * \param[in] topic_endpoint_info_array Array to be checked.
  -- * \returns `RMW_RET_OK` if array is zero initialized, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info_array` is NULL, or
  -- * \returns `RMW_RET_ERROR` if `topic_endpoint_info_array` is not zero initialized.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_array_check_zero (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_check_zero";

  --/ Initialize an array of topic endpoint information.
  --*
  -- * This function allocates space to hold `size` topic endpoint information elements.
  -- * Both `info_array` and `size` members are updated accordingly.
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
  -- *   - Access to the array of topic endpoint information is not synchronized.
  -- *     It is not safe to read or write `topic_endpoint_info_array` during initialization.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \param[inout] topic_endpoint_info_array Array to be initialized on success,
  -- *   but left unchanged on failure.
  -- * \param[in] size Size of the array.
  -- * \param[in] allocator Allocator to be used to populate `names_and_types`.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info_array` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info_array` is not
  -- *   a zero initialized array, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `allocator` is invalid,
  -- *   by rcutils_allocator_is_valid() definition, or
  -- * \returns `RMW_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_array_init_with_size
     (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t;
      size : stddef_h.size_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_init_with_size";

  --/ Finalize an array of topic endpoint information.
  --*
  -- * This function deallocates the given array storage, and then zero initializes it.
  -- * If a logical error, such as `RMW_RET_INVALID_ARGUMENT`, ensues, this function will
  -- * return early, leaving the given array unchanged.
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
  -- *   Finalization is a reentrant procedure, but:
  -- *   - Access to the array of topic endpoint information is not synchronized.
  -- *     It is not safe to read or write `topic_endpoint_info_array` during finalization.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \pre Given `allocator` must be the same used to initialize the given `topic_endpoint_info_array`.
  -- *
  -- * \param[inout] topic_endpoint_info_array object to be finalized.
  -- * \param[in] allocator Allocator used to populate the given `topic_endpoint_info_array`.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info_array` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `allocator` is invalid,
  -- *   by rcutils_allocator_is_valid() definition, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_array_fini (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t; allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:147
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_fini";

end rmw_topic_endpoint_info_array_h;
