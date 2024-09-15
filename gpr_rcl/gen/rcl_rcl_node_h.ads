pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
limited with rcl_rcl_context_h;
limited with rcl_rcl_node_options_h;
with rcl_rcl_types_h;
with Interfaces.C.Extensions;
with stddef_h;
limited with rmw_rmw_types_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
limited with rcl_rcl_guard_condition_h;
with rcl_rcl_allocator_h;
with System;
limited with type_description_interfaces_type_description_interfaces_srv_detail_get_type_description_ustruct_h;

package rcl_rcl_node_h is

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
  --/ @file
   RCL_DISABLE_LOANED_MESSAGES_ENV_VAR : constant Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/node.h:38
   with Import => True, 
        Convention => C, 
        External_Name => "RCL_DISABLE_LOANED_MESSAGES_ENV_VAR";

   type rcl_node_impl_s is null record;   -- incomplete struct

   subtype rcl_node_impl_t is rcl_node_impl_s;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:40

   type rcl_service_s is null record;   -- incomplete struct

   subtype rcl_service_t is rcl_service_s;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:41

  --/ Structure which encapsulates a ROS Node.
  --/ Context associated with this node.
   type rcl_node_s is record
      context : access rcl_rcl_context_h.rcl_context_s;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:47
      impl : access rcl_node_impl_t;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:44

  --/ Private implementation pointer.
   subtype rcl_node_t is rcl_node_s;  -- /opt/ros/jazzy/include/rcl/rcl/node.h:51

  --/ Return a rcl_node_t struct with members initialized to `NULL`.
   function rcl_get_zero_initialized_node return rcl_node_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_zero_initialized_node";

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
  -- * \todo TODO(wjwwood): node name uniqueness is not yet enforced
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
  -- * leading forward slash.
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
  -- * rcl_context_t context = rcl_get_zero_initialized_context();
  -- * // ... initialize the context with rcl_init()
  -- * rcl_node_t node = rcl_get_zero_initialized_node();
  -- * rcl_node_options_t node_ops = rcl_node_get_default_options();
  -- * // ... node options customization
  -- * rcl_ret_t ret = rcl_node_init(&node, "node_name", "/node_ns", &context, &node_ops);
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
  -- * \pre the context handle must be allocated, initialized, and valid
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
  -- * \return #RCL_RET_OK if the node was initialized successfully, or
  -- * \return #RCL_RET_ALREADY_INIT if the node has already be initialized, or
  -- * \return #RCL_RET_NOT_INIT if the given context is not initialized, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid or context is NULL, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_NODE_INVALID_NAME if the name is invalid, or
  -- * \return #RCL_RET_NODE_INVALID_NAMESPACE if the namespace_ is invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_init
     (node : access rcl_node_t;
      name : Interfaces.C.Strings.chars_ptr;
      namespace_u : Interfaces.C.Strings.chars_ptr;
      context : access rcl_rcl_context_h.rcl_context_s;
      options : access constant rcl_rcl_node_options_h.rcl_node_options_s) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:152
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_init";

  --/ Finalize a rcl_node_t.
  --*
  -- * Destroys any automatically created infrastructure and deallocates memory.
  -- * After calling, the rcl_node_t can be safely deallocated.
  -- *
  -- * All middleware primitives created by the user, e.g. publishers, services, etc,
  -- * which were created from this node must be finalized using their respective
  -- * `rcl_*_fini()` functions before this is called.
  -- * \sa rcl_publisher_fini()
  -- * \sa rcl_subscription_fini()
  -- * \sa rcl_client_fini()
  -- * \sa rcl_service_fini()
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
  -- * \return #RCL_RET_OK if node was finalized successfully, or
  -- * \return #RCL_RET_NODE_INVALID if the node pointer is null, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_fini (node : access rcl_node_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:189
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_fini";

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

   function rcl_node_is_valid (node : access constant rcl_node_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/node.h:228
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_is_valid";

  --/ Return true if node is valid, except for the context being valid.
  --*
  -- * This is used in clean up functions that need to access the node, but do not
  -- * need use any functions with the context.
  -- *
  -- * It is identical to rcl_node_is_valid except it ignores the state of the
  -- * context associated with the node.
  -- * \sa rcl_node_is_valid()
  --  

   function rcl_node_is_valid_except_context (node : access constant rcl_node_t) return Extensions.bool  -- /opt/ros/jazzy/include/rcl/rcl/node.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_is_valid_except_context";

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

   function rcl_node_get_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/node.h:268
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_name";

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

   function rcl_node_get_namespace (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/node.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_namespace";

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

   function rcl_node_get_fully_qualified_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/node.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_fully_qualified_name";

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

   function rcl_node_get_options (node : access constant rcl_node_t) return access constant rcl_rcl_node_options_h.rcl_node_options_s  -- /opt/ros/jazzy/include/rcl/rcl/node.h:345
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_options";

  --/ Return the ROS domain ID that the node is using.
  --*
  -- * This function returns the ROS domain ID that the node is in.
  -- *
  -- * This function should be used to determine what `domain_id` was used rather
  -- * than checking the domain_id field in the node options, because if
  -- * #RCL_NODE_OPTIONS_DEFAULT_DOMAIN_ID is used when creating the node then
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
  -- * \return #RCL_RET_OK if node the domain ID was retrieved successfully, or
  -- * \return #RCL_RET_NODE_INVALID if the node is invalid, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_get_domain_id (node : access constant rcl_node_t; domain_id : access stddef_h.size_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:378
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_domain_id";

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

   function rcl_node_get_rmw_handle (node : access constant rcl_node_t) return access rmw_rmw_types_h.rmw_node_s  -- /opt/ros/jazzy/include/rcl/rcl/node.h:409
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_rmw_handle";

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

   function rcl_node_get_rcl_instance_id (node : access constant rcl_node_t) return x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:438
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_rcl_instance_id";

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

   function rcl_node_get_graph_guard_condition (node : access constant rcl_node_t) return access constant rcl_rcl_guard_condition_h.rcl_guard_condition_s  -- /opt/ros/jazzy/include/rcl/rcl/node.h:471
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_graph_guard_condition";

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

   function rcl_node_get_logger_name (node : access constant rcl_node_t) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rcl/rcl/node.h:498
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_get_logger_name";

  --/ Expand a given name into a fully-qualified topic name and apply remapping rules.
  --*
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node Node object. Its name, namespace, local/global command line arguments are used.
  -- * \param[in] input_name Topic name to be expanded and remapped.
  -- * \param[in] allocator The allocator to be used when creating the output topic.
  -- * \param[in] is_service For services use `true`, for topics use `false`.
  -- * \param[in] only_expand When `true`, remapping rules are ignored.
  -- * \param[out] output_name Output char * pointer.
  -- * \return #RCL_RET_OK if the topic name was expanded successfully, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any of input_name, node_name, node_namespace
  -- *  or output_name are NULL, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if both local_args and global_args are NULL, or
  -- * \return #RCL_RET_BAD_ALLOC if allocating memory failed, or
  -- * \return #RCL_RET_TOPIC_NAME_INVALID if the given topic name is invalid
  -- *  (see rcl_validate_topic_name()), or
  -- * \return #RCL_RET_NODE_INVALID_NAME if the given node name is invalid
  -- *  (see rmw_validate_node_name()), or
  -- * \return #RCL_RET_NODE_INVALID_NAMESPACE if the given node namespace is invalid
  -- *  (see rmw_validate_namespace()), or
  -- * \return #RCL_RET_UNKNOWN_SUBSTITUTION for unknown substitutions in name, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_resolve_name
     (node : access constant rcl_node_t;
      input_name : Interfaces.C.Strings.chars_ptr;
      allocator : rcl_rcl_allocator_h.rcl_allocator_t;
      is_service : Extensions.bool;
      only_expand : Extensions.bool;
      output_name : System.Address) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:533
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_resolve_name";

  --/ Check if loaned message is disabled, according to the environment variable.
  --*
  -- * If the `ROS_DISABLE_LOANED_MESSAGES` environment variable is set to "1",
  -- * `disable_loaned_message` will be set to true.
  -- *
  -- * \param[out] disable_loaned_message Must not be NULL.
  -- * \return #RCL_RET_INVALID_ARGUMENT if an argument is not valid, or
  -- * \return #RCL_RET_ERROR if an unexpected error happened, or
  -- * \return #RCL_RET_OK.
  --  

   function rcl_get_disable_loaned_message (disable_loaned_message : access Extensions.bool) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:553
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_get_disable_loaned_message";

  --/ Initialize the node's ~/get_type_description service.
  --*
  -- * This function initializes the node's ~/get_type_description service
  -- * which can be used to retrieve information about types used by the node's
  -- * publishers, subscribers, services or actions.
  -- *
  -- * Note that this will not register any callback for the service, client-level code
  -- * must register rcl_node_type_description_service_handle_request or a custom callback
  -- * to handle incoming requests, via that client's executor/waitset capabilities.
  -- *
  -- * Note that the returned service must be cleaned up by the caller by calling
  -- * rcl_service_fini.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | Yes
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] service the handle to the type description service to be initialized
  -- * \param[in] node handle to the node for which to initialize the service
  -- * \return #RCL_RET_OK if the service was successfully initialized, or
  -- * \return #RCL_RET_INVALID_ARGUMENT if any arguments are invalid, or
  -- * \return #RCL_RET_ALREADY_INIT if the service is already initialized, or
  -- * \return #RCL_RET_BAD_ALLOC if memory allocation for the service failed, or
  -- * \return #RCL_RET_ERROR if an unspecified error occurs.
  --  

   function rcl_node_type_description_service_init (service : access rcl_service_t; node : access constant rcl_node_t) return rcl_rcl_types_h.rcl_ret_t  -- /opt/ros/jazzy/include/rcl/rcl/node.h:586
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_type_description_service_init";

  --/ Process a single pending request to the GetTypeDescription service.
  --*
  -- * This function may be called to handle incoming requests by any client starting the service.
  -- * It is not intended to be called directly by users.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | No
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] node the handle to the node
  -- * \param[in] request_header ID of the incoming request
  -- * \param[in] request Request that came in to the service
  -- * \param[out] response Allocated, uninitialized response to the request
  -- * \return void
  --  

   procedure rcl_node_type_description_service_handle_request
     (node : access rcl_node_t;
      request_header : access constant rmw_rmw_types_h.rmw_request_id_s;
      request : access constant type_description_interfaces_type_description_interfaces_srv_detail_get_type_description_ustruct_h.type_description_interfaces_u_srv_u_GetTypeDescription_Request;
      response : access type_description_interfaces_type_description_interfaces_srv_detail_get_type_description_ustruct_h.type_description_interfaces_u_srv_u_GetTypeDescription_Response)  -- /opt/ros/jazzy/include/rcl/rcl/node.h:610
   with Import => True, 
        Convention => C, 
        External_Name => "rcl_node_type_description_service_handle_request";

end rcl_rcl_node_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
