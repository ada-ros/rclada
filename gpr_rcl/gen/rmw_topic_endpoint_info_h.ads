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
  --/ A structure that encapsulates the name, namespace, topic_type, gid and qos_profile
  --/ of publishers and subscriptions for a topic.
  --/ Name of the node
   type rmw_topic_endpoint_info_t_endpoint_gid_array is array (0 .. 23) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rmw_topic_endpoint_info_t is record
      node_name : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:32
      node_namespace : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:34
      topic_type : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:36
      endpoint_type : aliased rmw_types_h.rmw_endpoint_type_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:38
      endpoint_gid : aliased rmw_topic_endpoint_info_t_endpoint_gid_array;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:40
      qos_profile : aliased rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:42
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:29

  --/ Namespace of the node
  --/ The associated topic type
  --/ The endpoint type
  --/ The GID of the endpoint
  --/ QoS profile of the endpoint
  --/ Return a rmw_topic_endpoint_info_t struct with members initialized to `NULL`.
   function rmw_get_zero_initialized_topic_endpoint_info return rmw_topic_endpoint_info_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:49
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_topic_endpoint_info";

  --/ Finalize a rmw_topic_endpoint_info_t object.
  --*
  -- * The rmw_topic_endpoint_info_t struct has members which require memory to be allocated to them before
  -- * setting values.
  -- * This function reclaims any allocated resources within the object and zeroes out all other
  -- * members.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info object to be finalized
  -- * \param[in] allocator the allocator used to allocate memory to the object
  -- * \returns `RMW_RET_OK` on successfully reclaiming memory, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_fini (topic_endpoint_info : access rmw_topic_endpoint_info_t; allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:69
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_fini";

  --/ Set the topic_type in rmw_topic_endpoint_info_t.
  --*
  -- * rmw_topic_endpoint_info_t has a member topic_type of type const char *;
  -- * this function allocates memory and copies the value of param passed to it.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] topic_type the topic_type value to set in rmw_topic_endpoint_info_t
  -- * \param[in] allocator the allocator that will be used to allocate memory
  -- * \returns `RMW_RET_OK` on successfully setting the topic_type, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_topic_type
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      topic_type : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_topic_type";

  --/ Set the node_name in rmw_topic_endpoint_info_t.
  --*
  -- * rmw_topic_endpoint_info_t has a member node_name of type const char *;
  -- * this function allocates memory and copies the value of param passed to it.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] node_name the node_name value to set in rmw_topic_endpoint_info_t
  -- * \param[in] allocator the allocator that will be used to allocate memory
  -- * \returns `RMW_RET_OK` on successfully setting the node_name, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_node_name
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      node_name : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_node_name";

  --/ Set the node_namespace in rmw_topic_endpoint_info_t.
  --*
  -- * rmw_topic_endpoint_info_t has a member node_namespace of type const char *;
  -- * this function allocates memory and copies the value of param passed to it.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] node_namespace the node_namespace value to set in rmw_topic_endpoint_info_t
  -- * \param[in] allocator the allocator that will be used to allocate memory
  -- * \returns `RMW_RET_OK` on successfully setting the node_namespace, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_node_namespace
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      node_namespace : Interfaces.C.Strings.chars_ptr;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_node_namespace";

  --/ Set the gid in rmw_topic_endpoint_info_t.
  --*
  -- * Copies the values from gid into the gid member inside topic_endpoint_info.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] gid the gid value to set in rmw_topic_endpoint_info_t
  -- * \param[in] size the size of the gid param
  -- * \returns `RMW_RET_OK` on successfully setting the gid, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` size is greater than RMW_GID_STORAGE_SIZE, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_endpoint_type (topic_endpoint_info : access rmw_topic_endpoint_info_t; c_type : rmw_types_h.rmw_endpoint_type_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_endpoint_type";

  --/ Set the gid in rmw_topic_endpoint_info_t.
  --*
  -- * Copies the values from gid into the gid member inside topic_endpoint_info.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] gid the gid value to set in rmw_topic_endpoint_info_t
  -- * \param[in] size the size of the gid param
  -- * \returns `RMW_RET_OK` on successfully setting the gid, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` size is greater than RMW_GID_STORAGE_SIZE, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_gid
     (topic_endpoint_info : access rmw_topic_endpoint_info_t;
      gid : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
      size : stddef_h.size_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:177
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_gid";

  --/ Set the qos_profile in rmw_topic_endpoint_info_t.
  --*
  -- * rmw_topic_endpoint_info_t has a member qos_profile of type const rmw_qos_profile_t *.
  -- * This function assigns the passed qos_profile pointer to the member.
  -- *
  -- * If a non RMW_RET_OK return value is returned, the RMW error message will be set
  -- *
  -- * \param[inout] topic_endpoint_info pointer to an initialized instance of rmw_topic_endpoint_info_t
  -- * \param[in] qos_profile the qos_profile to set in rmw_topic_endpoint_info_t
  -- * \returns `RMW_RET_OK` on successfully setting the qos_profile, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if any parameters are NULL, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  --  

   function rmw_topic_endpoint_info_set_qos_profile (topic_endpoint_info : access rmw_topic_endpoint_info_t; qos_profile : access constant rmw_types_h.rmw_qos_profile_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/topic_endpoint_info.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_topic_endpoint_info_set_qos_profile";

end rmw_topic_endpoint_info_h;
