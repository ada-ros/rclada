pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;
with rcl_allocator_h;
with Interfaces.C.Extensions;
with rcl_arguments_h;
with rcl_types_h;

package rcl_node_options_h is

   --  unsupported macro: RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID RCL_DEFAULT_DOMAIN_ID
  -- Copyright 2019 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Constant which indicates that the default domain id should be used.
  --/ Structure which encapsulates the options for creating a rcl_node_t.
  -- bool anonymous_name;
  -- rmw_qos_profile_t parameter_qos;
  --/ If true, no parameter infrastructure will be setup.
  -- bool no_parameters;
  --/ If set, then this value overrides the ROS_DOMAIN_ID environment variable.
  --*
  --   * It defaults to RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID, which will cause the
  --   * node to use the ROS domain ID set in the ROS_DOMAIN_ID environment
  --   * variable, or on some systems 0 if the environment variable is not set.
  --   *
  --   * \todo TODO(wjwwood):
  --   *   Should we put a limit on the ROS_DOMAIN_ID value, that way we can have
  --   *   a safe value for the default RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID?
  --   *   (currently max size_t)
  --    

   type rcl_node_options_t is record
      domain_id : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rcl/node_options.h:52
      allocator : aliased rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/foxy/include/rcl/node_options.h:55
      use_global_arguments : aliased Extensions.bool;  -- /opt/ros/foxy/include/rcl/node_options.h:58
      arguments : aliased rcl_arguments_h.rcl_arguments_t;  -- /opt/ros/foxy/include/rcl/node_options.h:61
      enable_rosout : aliased Extensions.bool;  -- /opt/ros/foxy/include/rcl/node_options.h:64
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl/node_options.h:32

  --/ Custom allocator used for internal allocations.
  --/ If false then only use arguments in this struct, otherwise use global arguments also.
  --/ Command line arguments that apply only to this node.
  --/ Flag to enable rosout for this node
  --/ Return the default node options in a rcl_node_options_t.
  --*
  -- * The default values are:
  -- *
  -- * - domain_id = RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID
  -- * - allocator = rcl_get_default_allocator()
  -- * - use_global_arguments = true
  -- * - enable_rosout = true
  -- * - arguments = rcl_get_zero_initialized_arguments()
  --  

   function rcl_node_get_default_options return rcl_node_options_t  -- /opt/ros/foxy/include/rcl/node_options.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_default_options";

  --/ Copy one options structure into another.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] options The structure to be copied.
  -- *   Its allocator is used to copy memory into the new structure.
  -- * \param[out] options_out An options structure containing default values.
  -- * \return `RCL_RET_OK` if the structure was copied successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any function arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_options_copy (options : access constant rcl_node_options_t; options_out : access rcl_node_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/node_options.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_options_copy";

  --/ Finalize the given node_options.
  --*
  -- * The given node_options must be non-`NULL` and valid, i.e. had
  -- * `rcl_node_get_default_options()` called on it but not this function yet.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] options object to be finalized
  -- * \return `RCL_RET_OK` if setup is successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_options_fini (options : access rcl_node_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/node_options.h:127
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_options_fini";

end rcl_node_options_h;
