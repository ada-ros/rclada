pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with rcl_arguments_h;
with System;
with rcl_types_h;
limited with rcl_init_options_h;
with Interfaces.C.Extensions;
limited with rmw_init_h;

package rcl_context_h is

   --  arg-macro: procedure RCL_ALIGNAS (N)
   --    alignas(N)
   --  unsupported macro: RCL_CONTEXT_ATOMIC_INSTANCE_ID_STORAGE_SIZE sizeof(uint_least64_t)
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
   subtype rcl_context_instance_id_t is x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/dashing/include/rcl/context.h:39

   --  skipped empty struct rcl_context_impl_t

  --/ Encapsulates the non-global state of an init/shutdown cycle.
  --*
  -- * The context is used in the creation of top level entities like nodes and
  -- * guard conditions, as well as to shutdown a specific instance of init.
  -- *
  -- * Here is a diagram of a typical context's lifecycle:
  -- *
  -- * ```
  -- *    +---------------+
  -- *    |               |
  -- * +--> uninitialized +---> rcl_get_zero_initialized_context() +
  -- * |  |               |                                        |
  -- * |  +---------------+                                        |
  -- * |                                                           |
  -- * |           +-----------------------------------------------+
  -- * |           |
  -- * |  +--------v---------+                +-----------------------+
  -- * |  |                  |                |                       |
  -- * |  | zero-initialized +-> rcl_init() +-> initialized and valid +-> rcl_shutdown() +
  -- * |  |                  |                |                       |                  |
  -- * |  +------------------+                +-----------------------+                  |
  -- * |                                                                                 |
  -- * |               +-----------------------------------------------------------------+
  -- * |               |
  -- * |  +------------v------------+
  -- * |  |                         |
  -- * |  | initialized but invalid +---> finalize all entities, then rcl_context_fini() +
  -- * |  |                         |                                                    |
  -- * |  +-------------------------+                                                    |
  -- * |                                                                                 |
  -- * +---------------------------------------------------------------------------------+
  -- * ```
  -- *
  -- * A declared but not defined `rcl_context_t` instance is considered to be
  -- * "uninitialized", and passing an uninitialized context to any functions will
  -- * result in undefined behavior.
  -- * Some functions, like `rcl_init()` require the context instance to be
  -- * zero initialized (all members set to "zero" state) before use.
  -- *
  -- * Zero initialization of an `rcl_context_t` should be done with
  -- * `rcl_get_zero_initialized_context()`, which ensures the context is in a safe
  -- * state for initialization with `rcl_init()`.
  -- *
  -- * Initialization of an `rcl_context_t` should be done with `rcl_init()`, after
  -- * which the context is considered both initialized and valid.
  -- * After initialization it can be used in the creation of other entities like
  -- * nodes and guard conditions.
  -- *
  -- * At any time the context can be invalidated by calling `rcl_shutdown()` on
  -- * the `rcl_context_t`, after which the context is still initialized but now
  -- * invalid.
  -- *
  -- * Invalidation indicates to other entities that the context was shutdown, but
  -- * is still accessible for use during cleanup of themselves.
  -- *
  -- * After being invalidated, and after all of the entities which used it have
  -- * been finalized, the context should be finalized with `rcl_context_fini()`.
  -- *
  -- * Finalizing the context while entities which have copies of it have not yet
  -- * been finalized is undefined behavior.
  -- * Therefore, the context's lifetime (between calls to `rcl_init()` and
  -- * `rcl_context_fini()`) should exceed the lifetime of all entities which use
  -- * it directly (e.g. nodes and guard conditions) or indirectly (e.g.
  -- * subscriptions and topics).
  --  

  --/ Global arguments for all nodes which share this context.
  --* Typically generated by the parsing of argc/argv in `rcl_init()`.  
   type rcl_context_t_instance_id_storage_array is array (0 .. 7) of aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
   type rcl_context_t is record
      global_arguments : aliased rcl_arguments_h.rcl_arguments_t;  -- /opt/ros/dashing/include/rcl/context.h:112
      impl : System.Address;  -- /opt/ros/dashing/include/rcl/context.h:115
      instance_id_storage : aliased rcl_context_t_instance_id_storage_array;  -- /opt/ros/dashing/include/rcl/context.h:142
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_context_t);  -- /opt/ros/dashing/include/rcl/context.h:108

  --/ Implementation specific pointer.
  -- The assumption that this is big enough for an atomic_uint_least64_t is
  -- ensured with a static_assert in the context.c file.
  -- In most cases it should just be a plain uint64_t.
  --/ Private storage for instance ID atomic.
  --*
  --   * Accessing the instance id should be done using the function
  --   * `rcl_context_get_instance_id()` because the instance id's type is an
  --   * atomic and needs to be accessed properly to ensure safety.
  --   *
  --   * The instance id should not be changed manually - doing so is undefined
  --   * behavior.
  --   *
  --   * The instance id cannot be protected within the `impl` pointer's type
  --   * because it needs to be accessible even when the context is zero
  --   * initialized and therefore `impl` is `NULL`.
  --   * Specifically, storing the instance id in the `impl` would introduce a
  --   * race condition between accessing it and finalizing the context.
  --   * Additionally, C11 atomics (i.e. "stdatomic.h") cannot be used directly
  --   * here in the case that this header is included into a C++ program.
  --   * See this paper for an effort to make this possible in the future:
  --   *   http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2018/p0943r1.html
  --    

  --/ Return a zero initialization context object.
   function rcl_get_zero_initialized_context return rcl_context_t;  -- /opt/ros/dashing/include/rcl/context.h:149
   pragma Import (C, rcl_get_zero_initialized_context, "rcl_get_zero_initialized_context");

  -- See `rcl_init()` for initialization of the context.
  --/ Finalize a context.
  --*
  -- * The context to be finalized must have been previously initialized with
  -- * `rcl_init()`, and then later invalidated with `rcl_shutdown()`.
  -- * If context is `NULL`, then `RCL_RET_INVALID_ARGUMENT` is returned.
  -- * If context is zero-initialized, then `RCL_RET_INVALID_ARGUMENT` is returned.
  -- * If context is initialized and valid (`rcl_shutdown()` was not called on it),
  -- * then `RCL_RET_INVALID_ARGUMENT` is returned.
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
  -- * \return `RCL_RET_OK` if the shutdown was completed successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occur.
  --  

   function rcl_context_fini (context : access rcl_context_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/context.h:178
   pragma Import (C, rcl_context_fini, "rcl_context_fini");

  -- See `rcl_shutdown()` for invalidation of the context.
  --/ Return the init options used during initialization for this context.
  --*
  -- * This function can fail and return `NULL` if:
  -- *   - context is NULL
  -- *   - context is zero-initialized, e.g. context->impl is `NULL`
  -- *
  -- * If context is uninitialized then that is undefined behavior.
  -- *
  -- * If `NULL` is returned an error message will have been set.
  -- *
  -- * The options are for reference only, and therefore the returned pointer is
  -- * const.
  -- * Changing the values in the options is undefined behavior but will likely
  -- * have no effect.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] context object from which the init options should be retrieved
  -- * \return pointer to the the init options, or
  -- * \return `NULL` if there was an error
  --  

   function rcl_context_get_init_options (context : access rcl_context_t) return access constant rcl_init_options_h.rcl_init_options_t;  -- /opt/ros/dashing/include/rcl/context.h:212
   pragma Import (C, rcl_context_get_init_options, "rcl_context_get_init_options");

  --/ Returns an unsigned integer that is unique to the given context, or `0` if invalid.
  --*
  -- * The given context must be non-`NULL`, but does not need to be initialized or valid.
  -- * If context is `NULL`, then `0` will be returned.
  -- * If context is uninitialized, then it is undefined behavior.
  -- *
  -- * The instance ID may be `0` if the context is zero-initialized or if the
  -- * context has been invalidated by `rcl_shutdown()`.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes [1]
  -- * <i>[1] if `atomic_is_lock_free()` returns true for `atomic_uint_least64_t`</i>
  -- *
  -- * \param[in] context object from which the instance id should be retrieved
  -- * \return a unique id specific to this context instance, or
  -- * \return `0` if invalid, or
  -- * \return `0` if context is `NULL`
  --  

   function rcl_context_get_instance_id (context : access rcl_context_t) return rcl_context_instance_id_t;  -- /opt/ros/dashing/include/rcl/context.h:240
   pragma Import (C, rcl_context_get_instance_id, "rcl_context_get_instance_id");

  --/ Return `true` if the given context is currently valid, otherwise `false`.
  --*
  -- * If context is `NULL`, then `false` is returned.
  -- * If context is zero-initialized, then `false` is returned.
  -- * If context is uninitialized, then it is undefined behavior.
  -- *
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes [1]
  -- * <i>[1] if `atomic_is_lock_free()` returns true for `atomic_uint_least64_t`</i>
  -- *
  -- * \param[in] context object which should be checked for validity
  -- * \return `true` if valid, otherwise `false`
  --  

   function rcl_context_is_valid (context : access rcl_context_t) return Extensions.bool;  -- /opt/ros/dashing/include/rcl/context.h:262
   pragma Import (C, rcl_context_is_valid, "rcl_context_is_valid");

  --/ Return pointer to the rmw context if the given context is currently valid, otherwise `NULL`.
  --*
  -- * If context is `NULL`, then `NULL` is returned.
  -- * If context is zero-initialized, then `NULL` is returned.
  -- * If context is uninitialized, then it is undefined behavior.
  -- *
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes [1]
  -- * <i>[1] if `atomic_is_lock_free()` returns true for `atomic_uint_least64_t`</i>
  -- *
  -- * \param[in] context object from which the rmw context should be retrieved.
  -- * \return pointer to rmw context if valid, otherwise `NULL`
  --  

   function rcl_context_get_rmw_context (context : access rcl_context_t) return access rmw_init_h.rmw_context_t;  -- /opt/ros/dashing/include/rcl/context.h:284
   pragma Import (C, rcl_context_get_rmw_context, "rcl_context_get_rmw_context");

end rcl_context_h;
