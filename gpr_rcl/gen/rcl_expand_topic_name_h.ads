pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with rcutils_types_string_map_h;
with rcl_allocator_h;
with System;
with rcl_types_h;

package rcl_expand_topic_name_h is

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
  --/ Expand a given topic name into a fully-qualified topic name.
  --*
  -- * The input_topic_name, node_name, and node_namespace arguments must all be
  -- * vaid, null terminated c strings.
  -- * The output_topic_name will not be assigned a value in the event of an error.
  -- *
  -- * The output_topic_name will be null terminated.
  -- * It is also allocated, so it needs to be deallocated, when it is no longer
  -- * needed, with the same allocator given to this function.
  -- * Make sure the `char *` which is passed for the output_topic_name does not
  -- * point to allocated memory before calling this function, because it will be
  -- * overwritten and therefore leaked if this function is successful.
  -- *
  -- * Expected usage:
  -- *
  -- * ```c
  -- * rcl_allocator_t allocator = rcl_get_default_allocator();
  -- * rcutils_allocator_t rcutils_allocator = rcutils_get_default_allocator();
  -- * rcutils_string_map_t substitutions_map = rcutils_get_zero_initialized_string_map();
  -- * rcutils_ret_t rcutils_ret = rcutils_string_map_init(&substitutions_map, 0, rcutils_allocator);
  -- * if (rcutils_ret != RCUTILS_RET_OK) {
  -- *   // ... error handling
  -- * }
  -- * rcl_ret_t ret = rcl_get_default_topic_name_substitutions(&substitutions_map);
  -- * if (ret != RCL_RET_OK) {
  -- *   // ... error handling
  -- * }
  -- * char * expanded_topic_name = NULL;
  -- * ret = rcl_expand_topic_name(
  -- *   "some/topic",
  -- *   "my_node",
  -- *   "my_ns",
  -- *   &substitutions_map,
  -- *   allocator,
  -- *   &expanded_topic_name);
  -- * if (ret != RCL_RET_OK) {
  -- *   // ... error handling
  -- * } else {
  -- *   RCUTILS_LOG_INFO_NAMED(ROS_PACKAGE_NAME, "Expanded topic name: %s", expanded_topic_name)
  -- *   // ... when done the output topic name needs to be deallocated:
  -- *   allocator.deallocate(expanded_topic_name, allocator.state);
  -- * }
  -- * ```
  -- *
  -- * The input topic name is validated using rcl_validate_topic_name(),
  -- * but if it fails validation RCL_RET_TOPIC_NAME_INVALID is returned.
  -- *
  -- * The input node name is validated using rmw_validate_node_name(),
  -- * but if it fails validation RCL_RET_NODE_INVALID_NAME is returned.
  -- *
  -- * The input node namespace is validated using rmw_validate_namespace(),
  -- * but if it fails validation RCL_RET_NODE_INVALID_NAMESPACE is returned.
  -- *
  -- * In addition to what is given by rcl_get_default_topic_name_substitutions(),
  -- * there are there these substitutions:
  -- *
  -- * - {node} -> the name of the node
  -- * - {namespace} -> the namespace of the node
  -- * - {ns} -> the namespace of the node
  -- *
  -- * If an unknown substitution is used, RCL_RET_UNKNOWN_SUBSTITUTION is returned.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] input_topic_name topic name to be expanded
  -- * \param[in] node_name name of the node associated with the topic
  -- * \param[in] node_namespace namespace of the node associated with the topic
  -- * \param[in] substitutions string map with possible substitutions
  -- * \param[in] allocator the allocator to be used when creating the output topic
  -- * \param[out] output_topic_name output char * pointer
  -- * \return `RCL_RET_OK` if the topic name was expanded successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_TOPIC_NAME_INVALID` if the given topic name is invalid, or
  -- * \return `RCL_RET_NODE_INVALID_NAME` if the name is invalid, or
  -- * \return `RCL_RET_NODE_INVALID_NAMESPACE` if the namespace_ is invalid, or
  -- * \return `RCL_RET_UNKNOWN_SUBSTITUTION` for unknown substitutions in name, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_expand_topic_name
     (input_topic_name : Interfaces.C.Strings.chars_ptr;
      node_name : Interfaces.C.Strings.chars_ptr;
      node_namespace : Interfaces.C.Strings.chars_ptr;
      substitutions : access constant rcutils_types_string_map_h.rcutils_string_map_t;
      allocator : rcl_allocator_h.rcl_allocator_t;
      output_topic_name : System.Address) return rcl_types_h.rcl_ret_t;  -- /opt/ros/bouncy/include/rcl/expand_topic_name.h:117
   pragma Import (C, rcl_expand_topic_name, "rcl_expand_topic_name");

  --/ Fill a given string map with the default substitution pairs.
  --*
  -- * If the string map is not initialized RCL_RET_INVALID_ARGUMENT is returned.
  -- *
  -- * \param[inout] string_map rcutils_string_map_t map to be filled with pairs
  -- * \return `RCL_RET_OK` if successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_get_default_topic_name_substitutions (string_map : access rcutils_types_string_map_h.rcutils_string_map_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/bouncy/include/rcl/expand_topic_name.h:138
   pragma Import (C, rcl_get_default_topic_name_substitutions, "rcl_get_default_topic_name_substitutions");

end rcl_expand_topic_name_h;
