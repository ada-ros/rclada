pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Strings;
with stddef_h;
with rmw_security_options_h;
with rmw_localhost_h;
with rcutils_allocator_h;
with rmw_ret_types_h;

package rmw_init_options_h is

  -- Copyright 2014-2018 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Implementation defined options structure used during rmw_init().
  --*
  -- * This should be defined by the rmw implementation.
  --  

   type rmw_init_options_impl_t is null record;   -- incomplete struct

  --/ Options structure used during rmw_init().
  --/ Locally (process local) unique ID that represents this init/shutdown cycle.
  --*
  --   * This should be set by the caller of `rmw_init()` to a number that is
  --   * unique within this process.
  --   * It is designed to be used with `rcl_init()` and `rcl_get_instance_id()`.
  --    

   type rmw_init_options_t is record
      instance_id : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/foxy/include/rmw/init_options.h:48
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/init_options.h:50
      domain_id : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/init_options.h:52
      security_options : aliased rmw_security_options_h.rmw_security_options_t;  -- /opt/ros/foxy/include/rmw/init_options.h:54
      localhost_only : aliased rmw_localhost_h.rmw_localhost_only_t;  -- /opt/ros/foxy/include/rmw/init_options.h:56
      enclave : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/init_options.h:58
      allocator : aliased rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/foxy/include/rmw/init_options.h:62
      impl : access rmw_init_options_impl_t;  -- /opt/ros/foxy/include/rmw/init_options.h:65
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/init_options.h:40

  --/ Implementation identifier, used to ensure two different implementations are not being mixed.
  --/ ROS domain id
  --/ Security options
  --/ Enable localhost only
  --/ Enclave, name used to find security artifacts in a sros2 keystore.
  -- TODO(wjwwood): replace with rmw_allocator_t when that refactor happens
  --/ Allocator used during internal allocation of init options, if needed.
  --/ Implementation defined init options.
  --* May be NULL if there are no implementation defined options.  
  --/ Return a zero initialized init options structure.
   function rmw_get_zero_initialized_init_options return rmw_init_options_t  -- /opt/ros/foxy/include/rmw/init_options.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_init_options";

  --/ Initialize given init options with the default values and implementation specific values.
  --*
  -- * The given allocator is used, if required, during setup of the init options,
  -- * but is also used during initialization.
  -- *
  -- * In either case the given allocator is stored in the returned init options.
  -- *
  -- * The `impl` pointer should not be changed manually.
  -- *
  -- * \pre The given init options must be zero initialized.
  -- *
  -- * \post If initialization fails, init options will remain zero initialized.
  -- *
  -- * \remark Giving an already initialized init options will result
  -- *   in a failure with return code `RMW_RET_INVALID_ARGUMENT`.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[inout] init_options object to be setup
  -- * \param[in] allocator to be used during setup and during initialization
  -- * \return `RMW_RET_OK` if setup is successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_init (init_options : access rmw_init_options_t; allocator : rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init_options.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_init_options_init";

  --/ Copy the given source init options to the destination init options.
  --*
  -- * The allocator from the source is used for any allocations and stored in the
  -- * destination.
  -- *
  -- * \pre The source init options must have been initialized
  -- *   i.e. had `rmw_init_options_init()` called on.
  -- * \pre The destination init options must be zero initialized.
  -- *
  -- * \post If copy fails, destination init options will remain zero initialized.
  -- *
  -- * \remark Giving an zero initialized init options as a source will result
  -- *   in a failure with return code `RMW_RET_INVALID_ARGUMENT`.
  -- * \remark Giving an already initialized init options for the destination will result
  -- *   in a failure with return code `RMW_RET_INVALID_ARGUMENT`.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[in] src rcl_init_options_t object to be copied from
  -- * \param[out] dst rcl_init_options_t object to be copied into
  -- * \return `RMW_RET_OK` if the copy is successful, or
  -- * \return `RMW_RET_INCORRECT_RMW_IMPLEMENTATION` if the implementation
  -- *   identifier for src does not match the implementation of this function, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_copy (src : access constant rmw_init_options_t; dst : access rmw_init_options_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init_options.h:150
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_init_options_copy";

  --/ Finalize the given init options.
  --*
  -- * This function will return early if a logical error, such as `RMW_RET_INVALID_ARGUMENT`
  -- * or `RMW_RET_INCORRECT_RMW_IMPLEMENTATION`, ensues, leaving the given init options
  -- * unchanged.
  -- * Otherwise, it will proceed despite errors, freeing as much resources as it can and zero
  -- * initializing the given init options.
  -- *
  -- * \pre The given init options must have been initialized
  -- *   i.e. had `rmw_init_options_init()` called on.
  -- *
  -- * \remarks If init options are zero initialized,
  -- *   then `RMW_RET_INVALID_ARGUMENT` is returned.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[inout] init_options object to finalized
  -- * \return `RMW_RET_OK` if finalization is successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_INCORRECT_RMW_IMPLEMENTATION` if the implementation
  -- *   identifier does not match the implementation of this function, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_fini (init_options : access rmw_init_options_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init_options.h:186
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_init_options_fini";

end rmw_init_options_h;
