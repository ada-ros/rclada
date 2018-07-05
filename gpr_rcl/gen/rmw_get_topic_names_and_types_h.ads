pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;
limited with rcutils_allocator_h;
with Interfaces.C.Extensions;
limited with rmw_names_and_types_h;

package rmw_get_topic_names_and_types_h is

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
  --/ Return a list of topic names and their types.
  --*
  -- * This function returns a list of topic names in the ROS graph and their types.
  -- *
  -- * The node parameter must not be `NULL`, and must point to a valid node.
  -- *
  -- * The topic_names_and_types parameter must be allocated and zero initialized.
  -- * The topic_names_and_types is the output for this function, and contains
  -- * allocated memory.
  -- * Therefore, it should be passed to rmw_names_and_types_fini() when
  -- * it is no longer needed.
  -- * Failing to do so will result in leaked memory.
  -- *
  -- * There may be some demangling that occurs when listing the topics from the
  -- * middleware implementation.
  -- * This is the mechanism by which this function can discriminate between ROS
  -- * topics, non-ROS topics, and topics which may be used to implement other
  -- * concepts like ROS Services.
  -- *
  -- * For example, if the underlying implementation is DDS or RTPS, ROS specific
  -- * prefixes may be prepended to the user namespace, and the namespace may be
  -- * stripped of leading and trailing slashes, see:
  -- *
  -- * http://design.ros2.org/articles/topic_and_service_names.html#ros-namespaces-with-dds-partitions
  -- *
  -- * As well as:
  -- *
  -- * http://design.ros2.org/articles/topic_and_service_names.html#communicating-with-non-ros-topics
  -- *
  -- * If the no_demangle argument is true, then the topic names given by the
  -- * middleware will be returned without any demangling or filtering.
  -- * For example, the ROS topic `/foo` may be returned as `rt/foo` or the DDS
  -- * topic (non-ROS topic) with a partition list `['foo', 'bar']` and topic `baz`
  -- * may be returned as `foo/baz` (note that only the first partition is used but
  -- * it is still concatenated to the topic).
  -- *
  -- * \param[in] node the handle to the node being used to query the ROS graph
  -- * \param[in] allocator allocator to be used when allocating space for strings
  -- * \param[in] no_demangle if true, list all topics without any demangling
  -- * \param[out] topic_names_and_types list of topic names and their types
  -- * \return `RMW_RET_OK` if the query was successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if the node is invalid, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_get_topic_names_and_types
     (node : access constant rmw_types_h.rmw_node_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t;
      no_demangle : Extensions.bool;
      topic_names_and_types : access rmw_names_and_types_h.rmw_names_and_types_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/get_topic_names_and_types.h:77
   pragma Import (C, rmw_get_topic_names_and_types, "rmw_get_topic_names_and_types");

end rmw_get_topic_names_and_types_h;
