pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with Interfaces.C.Strings;
with rmw_init_options_h;
with rmw_ret_types_h;

package rmw_init_h is

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
  --/ Implementation defined context structure returned by rmw_init().
  --*
  -- * This should be defined by the rmw implementation.
  --  

   type rmw_context_impl_t is null record;   -- incomplete struct

  --/ Initialization context structure which is used to store init specific information.
  --/ Locally (process local) unique ID that represents this init/shutdown cycle.
   type rmw_context_t is record
      instance_id : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/foxy/include/rmw/init.h:40
      implementation_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/foxy/include/rmw/init.h:42
      options : aliased rmw_init_options_h.rmw_init_options_t;  -- /opt/ros/foxy/include/rmw/init.h:44
      impl : access rmw_context_impl_t;  -- /opt/ros/foxy/include/rmw/init.h:47
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/init.h:37

  --/ Implementation identifier, used to ensure two different implementations are not being mixed.
  --/ Options used to initialize the context.
  --/ Implementation defined context information.
  --* May be NULL if there is no implementation defined context information.  
  --/ Return a zero initialized context structure.
   function rmw_get_zero_initialized_context return rmw_context_t  -- /opt/ros/foxy/include/rmw/init.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_context";

  --/ Initialize the middleware with the given options, and yielding an context.
  --*
  -- * The given context must be zero initialized, and is filled with
  -- * middleware specific data upon success of this function.
  -- * The context is used when initializing some entities like nodes and
  -- * guard conditions, and is also required to properly call rmw_shutdown().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[in] options initialization options to be used during initialization
  -- * \param[out] context resulting context struct
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_INCORRECT_RMW_IMPLEMENTATION` if the implementation
  -- *   identifier does not match, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are null or invalid, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs.
  --  

   function rmw_init (options : access constant rmw_init_options_h.rmw_init_options_t; context : access rmw_context_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init.h:84
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_init";

  --/ Shutdown the middleware for a given context.
  --*
  -- * The given context must be a valid context which has been initialized
  -- * with rmw_init().
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * This should be defined by the rmw implementation.
  -- *
  -- * \param[in] context resulting context struct
  -- * \return `RMW_RET_OK` if successful, or
  -- * \return `RMW_RET_INCORRECT_RMW_IMPLEMENTATION` if the implementation
  -- *   identifier does not match, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if the argument is null or invalid, or
  -- * \return `RMW_RET_ERROR` if an unexpected error occurs.
  --  

   function rmw_shutdown (context : access rmw_context_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_shutdown";

  --/ Finalize a context.
  --*
  -- * The context to be finalized must have been previously initialized with
  -- * `rmw_init()`, and then later invalidated with `rmw_shutdown()`.
  -- * If context is `NULL`, then `RMW_RET_INVALID_ARGUMENT` is returned.
  -- * If context is zero-initialized, then `RMW_RET_INVALID_ARGUMENT` is returned.
  -- * If context is initialized and valid (`rmw_shutdown()` was not called on it),
  -- * then `RMW_RET_INVALID_ARGUMENT` is returned.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes [1]
  -- * <i>[1] if `atomic_is_lock_free()` returns true for `atomic_uint_least64_t`</i>
  -- *
  -- * \return `RMW_RET_OK` if the shutdown was completed successfully, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RMW_RET_ERROR` if an unspecified error occur.
  --  

   function rmw_context_fini (context : access rmw_context_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/init.h:138
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_context_fini";

end rmw_init_h;
