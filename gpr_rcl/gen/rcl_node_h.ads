pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with rcl_context_h;
with System;
with Interfaces.C.Strings;
limited with rcl_node_options_h;
with rcl_types_h;
with Interfaces.C.Extensions;
with stddef_h;
limited with rmw_types_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;

package rcl_node_h is

  -- Copyright 2015 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   --  skipped empty struct rcl_guard_condition_t

   --  skipped empty struct rcl_node_impl_t

  --/ Structure which encapsulates a ROS Node.
  --/ Context associated with this node.
   type rcl_node_t is record
      context : access rcl_context_h.rcl_context_t;  -- /opt/ros/dashing/include/rcl/node.h:40
      impl : System.Address;  -- /opt/ros/dashing/include/rcl/node.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_node_t);  -- /opt/ros/dashing/include/rcl/node.h:37

  --/ Private implementation pointer.
  --/ Return a rcl_node_t struct with members initialized to `NULL`.
   function rcl_get_zero_initialized_node return rcl_node_t;  -- /opt/ros/dashing/include/rcl/node.h:50
   pragma Import (C, rcl_get_zero_initialized_node, "rcl_get_zero_initialized_node");

  --/ Initialize a ROS node.
  --*
  -- * Calling this on a rcl_node_t makes it a valid node handle until rcl_shutdown
  -- * is called or until rcl_node_fini is called on it.
  -- *
  -- * After calling, the ROS node object can be used to create other middleware
  -- * primitives like publishers, services, parameters, etc.
  -- *
  -- * The name of the node must not be NULL and adhere to naming restrictions,
  -- * see the rmw_validate_node_name() function for rules.
  -- *
  -- * \todo TODO(wjwwood): node name uniqueness is no yet enforced
  -- *
  -- * The name of the node cannot coincide with another node of the same name.
  -- * If a node of the same name is already in the domain, it will be shutdown.
  -- *
  -- * The namespace of the node should not be NULL and should also pass the
  -- * rmw_validate_namespace() function's rules.
  -- *
  -- * Additionally this function allows namespaces which lack a leading forward
  -- * slash.
  -- * Because there is no notion of a relative namespace, there is no difference
  -- * between a namespace which lacks a forward and the same namespace with a
  -- * leasing forward slash.
  -- * Therefore, a namespace like ``"foo/bar"`` is automatically changed to
  -- * ``"/foo/bar"`` by this function.
  -- * Similarly, the namespace ``""`` will implicitly become ``"/"`` which is a
  -- * valid namespace.
  -- *
  -- * \todo TODO(wjwwood):
  -- *   Parameter infrastructure is currently initialized in the language specific
  -- *   client library, e.g. rclcpp for C++, but will be initialized here in the
  -- *   future. When that happens there will be an option to avoid parameter
  -- *   infrastructure with an option in the rcl_node_options_t struct.
  -- *
  -- * A node contains infrastructure for ROS parameters, which include advertising
  -- * publishers and service servers.
  -- * This function will create those external parameter interfaces even if
  -- * parameters are not used later.
  -- *
  -- * The rcl_node_t given must be allocated and zero initialized.
  -- * Passing an rcl_node_t which has already had this function called on it, more
  -- * recently than rcl_node_fini, will fail.
  -- * An allocated rcl_node_t with uninitialized memory is undefined behavior.
  -- *
  -- * Expected usage:
  -- *
  -- * ```c
  -- * rcl_node_t node = rcl_get_zero_initialized_node();
  -- * rcl_node_options_t node_ops = rcl_node_get_default_options();
  -- * // ... node options customization
  -- * rcl_ret_t ret = rcl_node_init(&node, "node_name", "/node_ns", &node_ops);
  -- * // ... error handling and then use the node, but eventually deinitialize it:
  -- * ret = rcl_node_fini(&node);
  -- * // ... error handling for rcl_node_fini()
  -- * ```
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
  -- * \pre the node handle must be allocated, zero initialized, and invalid
  -- * \post the node handle is valid and can be used in other `rcl_*` functions
  -- *
  -- * \param[inout] node a preallocated rcl_node_t
  -- * \param[in] name the name of the node, must be a valid c-string
  -- * \param[in] namespace_ the namespace of the node, must be a valid c-string
  -- * \param[in] context the context instance with which the node should be
  -- *   associated
  -- * \param[in] options the node options.
  -- *   The options are deep copied into the node.
  -- *   The caller is always responsible for freeing memory used options they
  -- *   pass in.
  -- * \return `RCL_RET_OK` if the node was initialized successfully, or
  -- * \return `RCL_RET_ALREADY_INIT` if the node has already be initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_BAD_ALLOC` if allocating memory failed, or
  -- * \return `RCL_RET_NODE_INVALID_NAME` if the name is invalid, or
  -- * \return `RCL_RET_NODE_INVALID_NAMESPACE` if the namespace_ is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_init
     (node : access rcl_node_t;
      name : Interfaces.C.Strings.chars_ptr;
      namespace_u : Interfaces.C.Strings.chars_ptr;
      context : access rcl_context_h.rcl_context_t;
      options : access constant rcl_node_options_h.rcl_node_options_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/node.h:141
   pragma Import (C, rcl_node_init, "rcl_node_init");

  --/ Finalize a rcl_node_t.
  --*
  -- * Destroys any automatically created infrastructure and deallocates memory.
  -- * After calling, the rcl_node_t can be safely deallocated.
  -- *
  -- * Any middleware primitives created by the user, e.g. publishers, services, etc.,
  -- * are invalid after deinitialization.
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
  -- * \param[in] node rcl_node_t to be finalized
  -- * \return `RCL_RET_OK` if node was finalized successfully, or
  -- * \return `RCL_RET_NODE_INVALID` if the node pointer is null, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_fini (node : access rcl_node_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/node.h:173
   pragma Import (C, rcl_node_fini, "rcl_node_fini");

  --/ Return `true` if the node is valid, else `false`.
  --*
  -- * Also return `false` if the node pointer is `NULL` or the allocator is invalid.
  -- *
  -- * A node is invalid if:
  -- *   - the implementation is `NULL` (rcl_node_init not called or failed)
  -- *   - rcl_shutdown has been called since the node has been initialized
  -- *   - the node has been finalized with rcl_node_fini
  -- *
  -- * There is a possible validity race condition.
  -- *
  -- * Consider:
  -- *
  -- * ```c
  -- * assert(rcl_node_is_valid(node));  // <-- thread 1
  -- * rcl_shutdown();                   // <-- thread 2
  -- * // use node as if valid           // <-- thread 1
  -- * ```
  -- *
  -- * In the third line the node is now invalid, even though on the previous line
  -- * of thread 1 it was checked to be valid.
  -- * This is why this function is considered not thread-safe.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | Yes
  -- * Lock-Free          | Yes [1]
  -- * <i>[1] if `atomic_is_lock_free()` returns true for `atomic_uint_least64_t`</i>
  -- *
  -- * \param[in] node rcl_node_t to be validated
  -- * \return `true` if the node and allocator are valid, otherwise `false`.
  --  

   function rcl_node_is_valid (node : access constant rcl_node_t) return Extensions.bool;  -- /opt/ros/dashing/include/rcl/node.h:212
   pragma Import (C, rcl_node_is_valid, "rcl_node_is_valid");

  --/ Return true if node is valid, except for the context being valid.
  --*
  -- * This is used in clean up functions that need to access the node, but do not
  -- * need use any functions with the context.
  -- *
  -- * It is identical to rcl_node_is_valid except it ignores the state of the
  -- * context associated with the node.
  -- * \sa rcl_node_is_valid()
  --  

   function rcl_node_is_valid_except_context (node : access constant rcl_node_t) return Extensions.bool;  -- /opt/ros/dashing/include/rcl/node.h:225
   pragma Import (C, rcl_node_is_valid_except_context, "rcl_node_is_valid_except_context");

  --/ Return the name of the node.
  --*
  -- * This function returns the node's internal name string.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * The returned string is only valid as long as the given rcl_node_t is valid.
  -- * The value of the string may change if the value in the rcl_node_t changes,
  -- * and therefore copying the string is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the node
  -- * \return name string if successful, otherwise `NULL`
  --  

   function rcl_node_get_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rcl/node.h:252
   pragma Import (C, rcl_node_get_name, "rcl_node_get_name");

  --/ Return the namespace of the node.
  --*
  -- * This function returns the node's internal namespace string.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * The returned string is only valid as long as the given rcl_node_t is valid.
  -- * The value of the string may change if the value in the rcl_node_t changes,
  -- * and therefore copying the string is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the node
  -- * \return name string if successful, otherwise `NULL`
  --  

   function rcl_node_get_namespace (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rcl/node.h:279
   pragma Import (C, rcl_node_get_namespace, "rcl_node_get_namespace");

  --/ Return the fully qualified name of the node.
  --*
  -- * This function returns the node's internal namespace and name combined string.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the node
  -- * \return fully qualified name string if successful, otherwise `NULL`
  --  

   function rcl_node_get_fully_qualified_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rcl/node.h:302
   pragma Import (C, rcl_node_get_fully_qualified_name, "rcl_node_get_fully_qualified_name");

  --/ Return the rcl node options.
  --*
  -- * This function returns the node's internal options struct.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * The returned struct is only valid as long as the given rcl_node_t is valid.
  -- * The values in the struct may change if the options of the rcl_node_t changes,
  -- * and therefore copying the struct is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the node
  -- * \return options struct if successful, otherwise `NULL`
  --  

   function rcl_node_get_options (node : access constant rcl_node_t) return access constant rcl_node_options_h.rcl_node_options_t;  -- /opt/ros/dashing/include/rcl/node.h:329
   pragma Import (C, rcl_node_get_options, "rcl_node_get_options");

  --/ Return the ROS domain ID that the node is using.
  --*
  -- * This function returns the ROS domain ID that the node is in.
  -- *
  -- * This function should be used to determine what `domain_id` was used rather
  -- * than checking the domin_id field in the node options, because if
  -- * `RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID` is used when creating the node then
  -- * it is not changed after creation, but this function will return the actual
  -- * `domain_id` used.
  -- *
  -- * The `domain_id` field must point to an allocated `size_t` object to which
  -- * the ROS domain ID will be written.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node the handle to the node being queried
  -- * \param[out] domain_id storage for the domain id
  -- * \return `RCL_RET_OK` if node the domain ID was retrieved successfully, or
  -- * \return `RCL_RET_NODE_INVALID` if the node is invalid, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_get_domain_id (node : access constant rcl_node_t; domain_id : access stddef_h.size_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/node.h:362
   pragma Import (C, rcl_node_get_domain_id, "rcl_node_get_domain_id");

  --/ Manually assert that this node is alive (for RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE)
  --*
  -- * If the rmw Liveliness policy is set to RMW_QOS_POLICY_LIVELINESS_MANUAL_BY_NODE, the creator of
  -- * this node may manually call `assert_liveliness` at some point in time to signal to the rest
  -- * of the system that this Node is still alive.
  -- * This function must be called at least as often as the qos_profile's liveliness_lease_duration
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node handle to the node that needs liveliness to be asserted
  -- * \return `RCL_RET_OK` if the liveliness assertion was completed successfully, or
  -- * \return `RCL_RET_NODE_INVALID` if the node is invalid, or
  -- * \return `RCL_RET_ERROR` if an unspecified error occurs.
  --  

   function rcl_node_assert_liveliness (node : access constant rcl_node_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/dashing/include/rcl/node.h:387
   pragma Import (C, rcl_node_assert_liveliness, "rcl_node_assert_liveliness");

  --/ Return the rmw node handle.
  --*
  -- * The handle returned is a pointer to the internally held rmw handle.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * The returned handle is made invalid if the node is finalized or if
  -- * rcl_shutdown() is called.
  -- * The returned handle is not guaranteed to be valid for the life time of the
  -- * node as it may be finalized and recreated itself.
  -- * Therefore it is recommended to get the handle from the node using
  -- * this function each time it is needed and avoid use of the handle
  -- * concurrently with functions that might change it.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the rcl node
  -- * \return rmw node handle if successful, otherwise `NULL`
  --  

   function rcl_node_get_rmw_handle (node : access constant rcl_node_t) return access rmw_types_h.rmw_node_t;  -- /opt/ros/dashing/include/rcl/node.h:418
   pragma Import (C, rcl_node_get_rmw_handle, "rcl_node_get_rmw_handle");

  --/ Return the associated rcl instance id.
  --*
  -- * This id is stored when rcl_node_init is called and can be compared with the
  -- * value returned by rcl_get_instance_id() to check if this node was created in
  -- * the current rcl context (since the latest call to rcl_init().
  -- *
  -- * This function can fail, and therefore return `0`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * This function will succeed even if rcl_shutdown() has been called
  -- * since the node was created.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the rcl node
  -- * \return rcl instance id captured during node init or `0` on error
  --  

   function rcl_node_get_rcl_instance_id (node : access constant rcl_node_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/dashing/include/rcl/node.h:447
   pragma Import (C, rcl_node_get_rcl_instance_id, "rcl_node_get_rcl_instance_id");

  --/ Return a guard condition which is triggered when the ROS graph changes.
  --*
  -- * The handle returned is a pointer to an internally held rcl guard condition.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node is invalid
  -- *
  -- * The returned handle is made invalid if the node is finialized or if
  -- * rcl_shutdown() is called.
  -- *
  -- * The guard condition will be triggered anytime a change to the ROS graph occurs.
  -- * A ROS graph change includes things like (but not limited to) a new publisher
  -- * advertises, a new subscription is created, a new service becomes available,
  -- * a subscription is canceled, etc.
  -- *
  -- * \todo TODO(wjwwood): link to exhaustive list of graph events
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the rcl node
  -- * \return rcl guard condition handle if successful, otherwise `NULL`
  --  

   function rcl_node_get_graph_guard_condition (node : access constant rcl_node_t) return System.Address;  -- /opt/ros/dashing/include/rcl/node.h:480
   pragma Import (C, rcl_node_get_graph_guard_condition, "rcl_node_get_graph_guard_condition");

  --/ Return the logger name of the node.
  --*
  -- * This function returns the node's internal logger name string.
  -- * This function can fail, and therefore return `NULL`, if:
  -- *   - node is `NULL`
  -- *   - node has not been initialized (the implementation is invalid)
  -- *
  -- * The returned string is only valid as long as the given rcl_node_t is valid.
  -- * The value of the string may change if the value in the rcl_node_t changes,
  -- * and therefore copying the string is recommended if this is a concern.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node pointer to the node
  -- * \return logger_name string if successful, otherwise `NULL`
  --  

   function rcl_node_get_logger_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr;  -- /opt/ros/dashing/include/rcl/node.h:507
   pragma Import (C, rcl_node_get_logger_name, "rcl_node_get_logger_name");

end rcl_node_h;
