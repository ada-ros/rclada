pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Strings;
with rmw_types_h;
limited with rcutils_allocator_h;
with rmw_ret_types_h;
with stddef_h;

package rmw_topic_endpoint_info_h is

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
  --/ A data structure that encapsulates the node name, node namespace,
  --/ topic_type, gid, and qos_profile of publishers and subscriptions
  --/ for a topic.
  --/ Name of the node
   type rmw_topic_endpoint_info_t_endpoint_gid_array is array (0 .. 23) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rmw_topic_endpoint_info_t is record
      node_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:33
      node_namespace : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:35
      topic_type : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:37
      endpoint_type : aliased rmw_types_h.rmw_endpoint_type_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:39
      endpoint_gid : aliased rmw_topic_endpoint_info_t_endpoint_gid_array;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:41
      qos_profile : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:43
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:30

  --/ Namespace of the node
  --/ The associated topic type
  --/ The endpoint type
  --/ The GID of the endpoint
  --/ QoS profile of the endpoint
  --/ Return zero initialized topic endpoint info data structure.
  --*
  -- * Endpoint type will be invalid.
  -- * Endpoint QoS profile will be the system default.
  --  

   function rmw_get_zero_initialized_topic_endpoint_info return rmw_topic_endpoint_info_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_topic_endpoint_info";

  --/ Finalize a topic endpoint info data structure.
  --*
  -- * This function deallocates all allocated members of the given data structure,
  -- * and then zero initializes it.
  -- * If a logical error, such as `RMW_RET_INVALID_ARGUMENT`, ensues, this function
  -- * will return early, leaving the given data structure unchanged.
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
  -- *   - Access to the topic endpoint info data structure is not synchronized.
  -- *     It is not safe to read or write `topic_endpoint` during finalization.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be finalized.
  -- * \param[in] allocator Allocator used to populate the given `topic_endpoint_info`.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `allocator` is invalid,
  -- *   by rcutils_allocator_is_valid() definition, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_fini (topic_endpoint_info : access rmw_topic_endpoint_info_t; allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_fini";

  --/ Set the topic type in the given topic endpoint info data structure.
  --*
  -- * This functions allocates memory and copies the value of the `topic_type`
  -- * argument to set the data structure `topic_type` member.
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
  -- *   Setting a member is a reentrant procedure, but:
  -- *   - Access to the topic endpoint info data structure is not synchronized.
  -- *     It is not safe to read or write the `topic_type` member of the given `topic_endpoint`
  -- *     while setting it.
  -- *   - Access to C-style string arguments is read-only but it is not synchronized.
  -- *     Concurrent `topic_type` reads are safe, but concurrent reads and writes are not.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \pre Given `topic_type` is a valid C-style string i.e. NULL terminated.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] topic_type Type name to be set.
  -- * \param[in] allocator Allocator to be used.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_type` is NULL, or
  -- * \returns `RMW_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_topic_type
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      topic_type : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:133
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_topic_type";

  --/ Set the node name in the given topic endpoint info data structure.
  --*
  -- * This functions allocates memory and copies the value of the `node_name`
  -- * argument to set the data structure `node_name` member.
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
  -- *   Setting a member is a reentrant procedure, but:
  -- *   - Access to the topic endpoint info data structure is not synchronized.
  -- *     It is not safe to read or write the `node_name` member of the given `topic_endpoint`
  -- *     while setting it.
  -- *   - Access to C-style string arguments is read-only but it is not synchronized.
  -- *     Concurrent `node_name` reads are safe, but concurrent reads and writes are not.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \pre Given `node_name` is a valid C-style string i.e. NULL terminated.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] node_name Node name to be set.
  -- * \param[in] allocator Allocator to be used.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `node_name` is NULL, or
  -- * \returns `RMW_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_node_name
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      node_name : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:176
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_node_name";

  --/ Set the node namespace in the given topic endpoint info data structure.
  --*
  -- * This functions allocates memory and copies the value of the `node_namespace`
  -- * argument to set the data structure `node_namespace` member.
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
  -- *   Setting a member is a reentrant procedure, but:
  -- *   - Access to the topic endpoint info data structure is not synchronized.
  -- *     It is not safe to read or write the `node_namespace` member of the given `topic_endpoint`
  -- *     while setting it.
  -- *   - Access to C-style string arguments is read-only but it is not synchronized.
  -- *     Concurrent `node_namespace` reads are safe, but concurrent reads and writes are not.
  -- *   - The default allocators are thread-safe objects, but any custom `allocator` may not be.
  -- *     Check your allocator documentation for further reference.
  -- *
  -- * \pre Given `node_namespace` is a valid C-style string i.e. NULL terminated.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] node_namespace Node namespace to be set.
  -- * \param[in] allocator Allocator to be used.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `node_namespace` is NULL, or
  -- * \returns `RMW_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_node_namespace
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      node_namespace : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:219
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_node_namespace";

  --/ Set the endpoint type in the given topic endpoint info data structure.
  --*
  -- * This functions assigns the value of the `type` argument to the data structure
  -- * `endpoint_type` member.
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
  -- *   Setting a member is a reentrant procedure, but access to the
  -- *   topic endpoint info data structure is not synchronized.
  -- *   It is not safe to read or write the `endpoint_type` member of the
  -- *   given `topic_endpoint` while setting it.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] type Endpoint type to be set.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_endpoint_type (topic_endpoint_info : access rmw_topic_endpoint_info_t; c_type : rmw_types_h.rmw_endpoint_type_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_endpoint_type";

  --/ Set the endpoint gid in the given topic endpoint info data structure.
  --*
  -- * This functions copies the value of the `gid` argument to the data structure
  -- * `endpoint_gid` member.
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
  -- *   Setting a member is a reentrant procedure, but access to the
  -- *   topic endpoint info data structure is not synchronized.
  -- *   It is not safe to read or write the `gid` member of the
  -- *   given `topic_endpoint` while setting it.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] gid Endpoint gid to be set.
  -- * \param[in] size Size of the given `gid`.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `gid` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `size` is greater than RMW_GID_STORAGE_SIZE, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_gid
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      gid : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
      size : stddef_h.size_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_gid";

  --/ Set the endpoint QoS profile in the given topic endpoint info data structure.
  --*
  -- * This functions assigns the value of the `qos_profile` argument to the data structure
  -- * `qos_profile` member.
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
  -- *   Setting a member is a reentrant procedure, but access to the
  -- *   topic endpoint info data structure is not synchronized.
  -- *   It is not safe to read or write the `qos_profile` member of the
  -- *   given `topic_endpoint` while setting it.
  -- *
  -- * \param[inout] topic_endpoint_info Data structure to be populated.
  -- * \param[in] qos_profile QoS profile to be set.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `topic_endpoint_info` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `qos_profile` is NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  --  

   function rmw_topic_endpoint_info_set_qos_profile (topic_endpoint_info : access rmw_topic_endpoint_info_t; qos_profile : access constant rmw_types_h.rmw_qos_profile_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:324
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_qos_profile";

end rmw_topic_endpoint_info_h;
