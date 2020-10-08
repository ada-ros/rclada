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
  --/ Array of rmw_topic_endpoint_info_t
  --/ Size of the array.
   type rmw_topic_endpoint_info_array_t is record
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:31
      info_array : access rmw_topic_endpoint_info_h.rmw_topic_endpoint_info_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:33
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:28

  --/ Pointer representing an array of rmw_topic_endpoint_info_t
  --/ Return a rmw_topic_endpoint_info_array_t struct with members initialized to `NULL`.
   function rmw_get_zero_initialized_topic_endpoint_info_array return rmw_topic_endpoint_info_array_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:40
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_topic_endpoint_info_array";

  --/ Check that a rmw_topic_endpoint_info_array_t struct is zero initialized.
  --*
  -- * This function checks if the provided rmw_topic_endpoint_info_array_t is zero initialized or not.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[in] topic_endpoint_info_array the data structure to be checked
  -- * \returns `RMW_RET_OK` if topic_endpoint_info_array is zero initialized
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if the parameter is NULL, or
  -- * \returns `RMW_RET_ERROR` if topic_endpoint_info_array is not zero initialized
  --  

   function rmw_topic_endpoint_info_array_check_zero (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_check_zero";

  --/ Initialize the info_array member inside rmw_topic_endpoint_info_array_t with the given size
  --*
  -- * The rmw_topic_endpoint_info_array_t has a member variable info_array which is an array of
  -- * type rmw_topic_endpoint_info_t.
  -- * This function allocates memory to this array to hold n elements,
  -- * where n is the value of the size param to this function.
  -- * The member `size` is updated accordingly.
  -- *
  -- * topic_endpoint_info_array must be zero initialized before being passed into this function.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info_array the data structure to initialise
  -- * \param[in] size the size of the array
  -- * \param[in] allocator the allocator to be used to allocate space
  -- * \returns `RMW_RET_OK` on successful init, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any of the parameters are NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if topic_endpoint_info_array is not zero initialized, or
  -- * \returns `RMW_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_array_init_with_size
     (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t;
      size : stddef_h.size_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_init_with_size";

  --/ Finalize a rmw_topic_endpoint_info_array_t object.
  --*
  -- * The info_array member variable inside of rmw_topic_endpoint_info_array represents an array of
  -- * rmw_topic_endpoint_info_t.
  -- * When initializing this array, memory is allocated for it using the allocator.
  -- * This function reclaims any allocated resources within the object and also sets the value of size
  -- * to 0.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info_array object to be finalized
  -- * \param[in] allocator the allocator used to allocate memory to the object
  -- * \returns `RMW_RET_OK` on successfully reclaiming memory, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_array_fini (topic_endpoint_info_array : access rmw_topic_endpoint_info_array_t; allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info_array.h:107
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_array_fini";

end rmw_topic_endpoint_info_array_h;
