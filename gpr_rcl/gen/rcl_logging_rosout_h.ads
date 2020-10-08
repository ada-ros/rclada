pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with rcl_allocator_h;
with rcl_types_h;
limited with rcl_node_h;
limited with rcutils_logging_h;
with Interfaces.C.Strings;
with rcutils_time_h;
with stdio_h;

package rcl_logging_rosout_h is

  -- Copyright 2018-2019 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Initializes the rcl_logging_rosout features
  --*
  -- * Calling this will initialize the rcl_logging_rosout features. This function must be called
  -- * before any other rcl_logging_rosout_* functions can be called.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] allocator The allocator used for metadata related to the rcl_logging_rosout features
  -- * \return `RCL_RET_OK` if the rcl_logging_rosout features are successfully initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_logging_rosout_init (allocator : access constant rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging_rosout.h:51
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_init";

  --/ Uninitializes the rcl_logging_rosout features
  --*
  -- * Calling this will set the rcl_logging_rosout features into the an unitialized state that is
  -- * functionally the same as before rcl_logging_rosout_init was called.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \return `RCL_RET_OK` if the rcl_logging_rosout feature was successfully unitialized, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_logging_rosout_fini return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging_rosout.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_fini";

  --/ Creates a rosout publisher for a node and registers it to be used by the logging system
  --*
  -- * Calling this for an rcl_node_t will create a new publisher on that node that will be
  -- * used by the logging system to publish all log messages from that Node's logger.
  -- *
  -- * If a publisher already exists for this node then a new publisher will NOT be created.
  -- *
  -- * It is expected that after creating a rosout publisher with this function
  -- * rcl_logging_destroy_rosout_publisher_for_node() will be called for the node to cleanup
  -- * the publisher while the Node is still valid.
  -- *
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node a valid rcl_node_t that the publisher will be created on
  -- * \return `RCL_RET_OK` if the logging publisher was created successfully, or
  -- * \return `RCL_RET_NODE_INVALID` if the argument is invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_logging_rosout_init_publisher_for_node (node : access rcl_node_h.rcl_node_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging_rosout.h:104
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_init_publisher_for_node";

  --/ Deregisters a rosout publisher for a node and cleans up allocated resources
  --*
  -- * Calling this for an rcl_node_t will destroy the rosout publisher on that node and remove it from
  -- * the logging system so that no more Log messages are published to this function.
  -- *
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node a valid rcl_node_t that the publisher will be created on
  -- * \return `RCL_RET_OK` if the logging publisher was finalized successfully, or
  -- * \return `RCL_RET_NODE_INVALID` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_logging_rosout_fini_publisher_for_node (node : access rcl_node_h.rcl_node_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/logging_rosout.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_fini_publisher_for_node";

  --/ The output handler outputs log messages to rosout topics.
  --*
  -- * When called with a logger name and log message this function will attempt to
  -- * find a rosout publisher correlated with the logger name and publish a Log
  -- * message out via that publisher. If there is no publisher directly correlated
  -- * with the logger then nothing will be done.
  -- *
  -- * This function is meant to be registered with the logging functions for rcutils
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] location The pointer to the location struct or NULL
  -- * \param[in] severity The severity level
  -- * \param[in] name The name of the logger, must be null terminated c string
  -- * \param[in] timestamp The timestamp for when the log message was made
  -- * \param[in] format The list of arguments to insert into the formatted log message
  -- * \param[in] args argument for the string format
  --  

   procedure rcl_logging_rosout_output_handler
     (location : access constant rcutils_logging_h.rcutils_log_location_t;
      severity : int;
      name : Interfaces.C.Strings.chars_ptr;
      timestamp : rcutils_time_h.rcutils_time_point_value_t;
      format : Interfaces.C.Strings.chars_ptr;
      args : access stdio_h.va_list)  -- /opt/ros/foxy/include/rcl/logging_rosout.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_logging_rosout_output_handler";

end rcl_logging_rosout_h;
