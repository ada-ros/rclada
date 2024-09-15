pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcl_rcl_allocator_h;
with Interfaces.C.Extensions;
with rcl_rcl_arguments_h;
with rmw_rmw_types_h;
with rcl_rcl_types_h;

package rcl_rcl_node_options_h is

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
  --/ @file
  --/ Constant which indicates that the default domain id should be used.
  --/ Structure which encapsulates the options for creating a rcl_node_t.
  -- bool anonymous_name;
  -- rmw_qos_profile_t parameter_qos;
  --/ If true, no parameter infrastructure will be setup.
  -- bool no_parameters;
  --/ Custom allocator used for internal allocations.
   type rcl_node_options_s is record
      allocator : aliased rcl_rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:44
      use_global_arguments : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:47
      arguments : aliased rcl_rcl_arguments_h.rcl_arguments_t;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:50
      enable_rosout : aliased Extensions.bool;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:53
      rosout_qos : aliased rmw_rmw_types_h.rmw_qos_profile_t;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:56
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:34

  --/ If false then only use arguments in this struct, otherwise use global arguments also.
  --/ Command line arguments that apply only to this node.
  --/ Flag to enable rosout for this node
  --/ Middleware quality of service settings for /rosout.
   subtype rcl_node_options_t is rcl_node_options_s;  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:57

  --/ Return the default node options in a rcl_node_options_t.
  --*
  -- * The default values are:
  -- *
  -- * - allocator = rcl_get_default_allocator()
  -- * - use_global_arguments = true
  -- * - enable_rosout = true
  -- * - arguments = rcl_get_zero_initialized_arguments()
  -- * - rosout_qos = rcl_qos_profile_rosout_default
  -- *
  -- * \return A structure with the default node options.
  --  

   function rcl_node_get_default_options return rcl_node_options_t  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:73
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
  -- * \return #RCL_RET_OK if the structure was copied successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any function arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_options_copy (options : access constant rcl_node_options_t; options_out : access rcl_node_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:96
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_options_copy";

  --/ Finalize the given node_options.
  --*
  -- * The given node_options must be non-`NULL` and valid, i.e. had
  -- * rcl_node_get_default_options() called on it but not this function yet.
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
  -- * \return #RCL_RET_OK if setup is successful, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_options_fini (options : access rcl_node_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node_options.h:121
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_options_fini";

end rcl_rcl_node_options_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
