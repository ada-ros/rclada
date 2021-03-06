pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcl_allocator_h;
with rcl_types_h;
limited with rmw_init_options_h;

package rcl_init_options_h is

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
   type rcl_init_options_impl_t is null record;   -- incomplete struct

  --/ Encapsulation of init options and implementation defined init options.
  --/ Implementation specific pointer.
   type rcl_init_options_t is record
      impl : access rcl_init_options_impl_t;  -- /opt/ros/foxy/include/rcl/init_options.h:36
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rcl/init_options.h:33

  --/ Return a zero initialized rcl_init_options_t struct.
   function rcl_get_zero_initialized_init_options return rcl_init_options_t  -- /opt/ros/foxy/include/rcl/init_options.h:43
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
  -- * \return `RCL_RET_OK` if setup is successful, or
  -- * \return `RCL_RET_ALREADY_INIT` if init_options has already be initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_init_options_init (init_options : access rcl_init_options_t; allocator : rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/init_options.h:73
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_init";

  --/ Copy the given source init_options to the destination init_options.
  --*
  -- * The allocator from the source is used for any allocations and stored in the
  -- * destination.
  -- *
  -- * The destination should either be zero initialized with
  -- * `rcl_get_zero_initialized_init_options()` or should have had
  -- * `rcl_init_options_fini()` called on it.
  -- * Giving an already initialized init options for the destination will result
  -- * in a failure with return code `RCL_RET_ALREADY_INIT`.
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
  -- * \return `RCL_RET_OK` if the copy is successful, or
  -- * \return `RCL_RET_ALREADY_INIT` if the dst has already be initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_init_options_copy (src : access constant rcl_init_options_t; dst : access rcl_init_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/init_options.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_copy";

  --/ Finalize the given init_options.
  --*
  -- * The given init_options must be non-`NULL` and valid, i.e. had
  -- * `rcl_init_options_init()` called on it but not this function yet.
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
  -- * \return `RCL_RET_OK` if setup is successful, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_init_options_fini (init_options : access rcl_init_options_t) return rcl_types_h.rcl_ret_t  -- /opt/ros/foxy/include/rcl/init_options.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_fini";

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

   function rcl_init_options_get_rmw_init_options (init_options : access rcl_init_options_t) return access rmw_init_options_h.rmw_init_options_t  -- /opt/ros/foxy/include/rcl/init_options.h:153
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

   function rcl_init_options_get_allocator (init_options : access constant rcl_init_options_t) return access constant rcl_allocator_h.rcl_allocator_t  -- /opt/ros/foxy/include/rcl/init_options.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_init_options_get_allocator";

end rcl_init_options_h;
