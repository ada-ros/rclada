pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcl_rcl_allocator_h;
with rcl_rcl_types_h;
with stddef_h;
limited with rmw_rmw_init_options_h;
limited with rcutils_rcutils_allocator_h;

package rcl_rcl_init_options_h is

  -- Copyright 2018 Open Source Robotics Foundation, Inc.
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
   type rcl_init_options_impl_s is null record;   -- incomplete struct

   subtype rcl_init_options_impl_t is rcl_init_options_impl_s;  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:32

  --/ Encapsulation of init options and implementation defined init options.
  --/ Implementation specific pointer.
   type rcl_init_options_s is record
      impl : access rcl_init_options_impl_t;  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:38
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:35

   subtype rcl_init_options_t is rcl_init_options_s;  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:39

  --/ Return a zero initialized rcl_init_options_t struct.
   function rcl_get_zero_initialized_init_options return rcl_init_options_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:45
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_init_options";

  --/ Initialize given init_options with the default values and implementation specific values.
  --*
  -- * The given allocator is used, if required, during setup of the init options,
  -- * but is also used during initialization.
  -- *
  -- * In either case the given allocator is stored in the returned init options.
  -- *
  -- * The `impl` pointer should not be changed manually.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] init_options object to be setup
  -- * \param[in] allocator to be used during setup and during initialization
  -- * \return #RCL_RET_OK if setup is successful, or
  -- * \return #RCL_RET_ALREADY_INIT if init_options has already be initialized, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_init_options_init (init_options : access rcl_init_options_t; allocator : rcl_rcl_allocator_h.rcl_allocator_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_init";

  --/ Copy the given source init_options to the destination init_options.
  --*
  -- * The allocator from the source is used for any allocations and stored in the
  -- * destination.
  -- *
  -- * The destination should either be zero initialized with
  -- * rcl_get_zero_initialized_init_options() or should have had
  -- * rcl_init_options_fini() called on it.
  -- * Giving an already initialized init options for the destination will result
  -- * in a failure with return code #RCL_RET_ALREADY_INIT.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] src rcl_init_options_t object to be copied from
  -- * \param[out] dst rcl_init_options_t object to be copied into
  -- * \return #RCL_RET_OK if the copy is successful, or
  -- * \return #RCL_RET_ALREADY_INIT if the dst has already be initialized, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_init_options_copy (src : access constant rcl_init_options_t; dst : access rcl_init_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:107
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_copy";

  --/ Finalize the given init_options.
  --*
  -- * The given init_options must be non-`NULL` and valid, i.e. had
  -- * rcl_init_options_init() called on it but not this function yet.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[inout] init_options object to be setup
  -- * \return #RCL_RET_OK if setup is successful, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_init_options_fini (init_options : access rcl_init_options_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:130
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_fini";

  --/ Return the domain_id stored in the init options.
  --*
  -- * Get the domain id from the specified rcl_init_options_t object.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] init_options object from which the domain id should be retrieved.
  -- * \param[out] domain_id domain id to be set in init_options object.
  -- * \return #RCL_RET_OK if successful, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid.
  --  

   function rcl_init_options_get_domain_id (init_options : access constant rcl_init_options_t; domain_id : access stddef_h.size_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:152
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_get_domain_id";

  --/ Set a domain id in the init options provided.
  --*
  -- * Store the domain id in the specified init_options object.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] init_options objects in which to set the specified domain id.
  -- * \param[in] domain_id domain id to be set in init_options object.
  -- * \return #RCL_RET_OK if successful, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid.
  --  

   function rcl_init_options_set_domain_id (init_options : access rcl_init_options_t; domain_id : stddef_h.size_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:174
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_set_domain_id";

  --/ Return the rmw init options which are stored internally.
  --*
  -- * This function can fail and return `NULL` if:
  -- *   - init_options is NULL
  -- *   - init_options is invalid, e.g. init_options->impl is NULL
  -- *
  -- * If NULL is returned an error message will have been set.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] init_options object from which the rmw init options should be retrieved
  -- * \return pointer to the the rcl init options, or
  -- * \return `NULL` if there was an error
  --  

   function rcl_init_options_get_rmw_init_options (init_options : access rcl_init_options_t) return access rmw_rmw_init_options_h.rmw_init_options_s  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_get_rmw_init_options";

  --/ Return the allocator stored in the init_options.
  --*
  -- * This function can fail and return `NULL` if:
  -- *   - init_options is NULL
  -- *   - init_options is invalid, e.g. init_options->impl is NULL
  -- *
  -- * If NULL is returned an error message will have been set.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] init_options object from which the allocator should be retrieved
  -- * \return pointer to the rcl allocator, or
  -- * \return `NULL` if there was an error
  --  

   function rcl_init_options_get_allocator (init_options : access constant rcl_init_options_t) return access constant rcutils_rcutils_allocator_h.rcutils_allocator_s  -- /opt/ros/jazzy/include/rcl/rcl/init_options.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_get_allocator";

end rcl_rcl_init_options_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
