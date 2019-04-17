pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Strings;
with rcutils_allocator_h;
with System;
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

   --  skipped empty struct rmw_init_options_impl_t

  --/ Options structure used during rmw_init().
  --/ Locally (process local) unique ID that represents this init/shutdown cycle.
  --*
  --   * This should be set by the caller of `rmw_init()` to a number that is
  --   * unique within this process.
  --   * It is designed to be used with `rcl_init()` and `rcl_get_instance_id()`.
  --    

   type rmw_init_options_t is record
      instance_id : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/crystal/include/rmw/init_options.h:45
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/crystal/include/rmw/init_options.h:47
      allocator : aliased rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/crystal/include/rmw/init_options.h:50
      impl : System.Address;  -- /opt/ros/crystal/include/rmw/init_options.h:53
   end record;
   pragma Convention (C_Pass_By_Copy, rmw_init_options_t);  -- /opt/ros/crystal/include/rmw/init_options.h:37

  --/ Implementation identifier, used to ensure two different implementations are not being mixed.
  -- TODO(wjwwood): replace with rmw_allocator_t when that refactor happens
  --/ Allocator used during internal allocation of init options, if needed.
  --/ Implementation defined init options.
  --* May be NULL if there are no implementation defined options.  
  --/ Return a zero initialized init options structure.
   function rmw_get_zero_initialized_init_options return rmw_init_options_t;  -- /opt/ros/crystal/include/rmw/init_options.h:60
   pragma Import (C, rmw_get_zero_initialized_init_options, "rmw_get_zero_initialized_init_options");

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
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[inout] init_options object to be setup
  -- * \param[in] allocator to be used during setup and during initialization
  -- * \return `RMW_RET_OK` if setup is successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if init_options has already be initialized, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_init (init_options : access rmw_init_options_t; allocator : rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t;  -- /opt/ros/crystal/include/rmw/init_options.h:92
   pragma Import (C, rmw_init_options_init, "rmw_init_options_init");

  --/ Copy the given source init options to the destination init options.
  --*
  -- * The allocator from the source is used for any allocations and stored in the
  -- * destination.
  -- *
  -- * The destination should either be zero initialized with
  -- * `rmw_get_zero_initialized_init_options()` or should have had
  -- * `rmw_init_options_fini()` called on it.
  -- * Giving an already initialized init options for the destination will result
  -- * in a failure with return code `RMW_RET_INVALID_ARGUMENT`.
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
  -- * \return `RMW_RET_INVALID_ARGUMENT` if the dst has already be initialized, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_copy (src : access constant rmw_init_options_t; dst : access rmw_init_options_t) return rmw_ret_types_h.rmw_ret_t;  -- /opt/ros/crystal/include/rmw/init_options.h:128
   pragma Import (C, rmw_init_options_copy, "rmw_init_options_copy");

  --/ Finalize the given init_options.
  --*
  -- * The given init_options must be non-`NULL` and valid, i.e. had
  -- * `rmw_init_options_init()` called on it but not this function yet.
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
  -- * \return `RMW_RET_OK` if setup is successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occurs.
  --  

   function rmw_init_options_fini (init_options : access rmw_init_options_t) return rmw_ret_types_h.rmw_ret_t;  -- /opt/ros/crystal/include/rmw/init_options.h:153
   pragma Import (C, rmw_init_options_fini, "rmw_init_options_fini");

end rmw_init_options_h;