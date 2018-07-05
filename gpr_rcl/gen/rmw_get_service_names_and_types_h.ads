pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;
limited with rcutils_allocator_h;
limited with rmw_names_and_types_h;

package rmw_get_service_names_and_types_h is

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
  --/ Return a list of service names and their types.
  --*
  -- * This function returns a list of service names in the ROS graph and their types.
  -- *
  -- * The node parameter must not be `NULL`, and must point to a valid node.
  -- *
  -- * The service_names_and_types parameter must be allocated and zero initialized.
  -- * The service_names_and_types is the output for this function, and contains
  -- * allocated memory.
  -- * Therefore, it should be passed to rmw_names_and_types_fini() when
  -- * it is no longer needed.
  -- * Failing to do so will result in leaked memory.
  -- *
  -- * \param[in] node the handle to the node being used to query the ROS graph
  -- * \param[in] allocator allocator to be used when allocating space for strings
  -- * \param[out] service_names_and_types list of service names and their types
  -- * \return `RMW_RET_OK` if the query was successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if the node is invalid, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if memory allocation fails, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_get_service_names_and_types
     (node : access constant rmw_types_h.rmw_node_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t;
      service_names_and_types : access rmw_names_and_types_h.rmw_names_and_types_t) return rmw_types_h.rmw_ret_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/get_service_names_and_types.h:53
   pragma Import (C, rmw_get_service_names_and_types, "rmw_get_service_names_and_types");

end rmw_get_service_names_and_types_h;
