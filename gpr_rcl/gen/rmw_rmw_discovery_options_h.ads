pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with stddef_h;
with rcutils_rcutils_allocator_h;
with rmw_rmw_ret_types_h;
with Interfaces.C.Extensions;

package rmw_rmw_discovery_options_h is

   RMW_DISCOVERY_OPTIONS_STATIC_PEERS_MAX_LENGTH : constant := 256;  --  /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:45

  -- Copyright 2022 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Used to control the range that nodes will be discovered
   type rmw_automatic_discovery_range_e is 
     (RMW_AUTOMATIC_DISCOVERY_RANGE_NOT_SET,
      RMW_AUTOMATIC_DISCOVERY_RANGE_OFF,
      RMW_AUTOMATIC_DISCOVERY_RANGE_LOCALHOST,
      RMW_AUTOMATIC_DISCOVERY_RANGE_SUBNET,
      RMW_AUTOMATIC_DISCOVERY_RANGE_SYSTEM_DEFAULT)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:30

  --/ The discovery range has not been set
  --/ Force discovery off
  --/ Allows discovering nodes on the same host
  --/ Allows discovering nodes on the same subnet
  --/ Use discovery settings configured directly with the middleware
   subtype rmw_automatic_discovery_range_t is rmw_automatic_discovery_range_e;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:42

  --/ Maximum length of a peer hostname or IP address
  --/ Struct to typedef some of the peer addresses
   subtype anon_array1161 is Interfaces.C.char_array (0 .. 255);
   type rmw_peer_address_s is record
      peer_address : aliased anon_array1161;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:48

   subtype rmw_peer_address_t is rmw_peer_address_s;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:51

  --/ Used to specify the options that control how discovery is performed
  --/ How far to allow discovering nodes
  --*
  --   * This needs to be set to something other than RMW_AUTOMATIC_DISCOVERY_RANGE_NOT_SET,
  --   * because that is just a sentinel value to see if this was set, but rmw
  --   * implementations should error if this is what is given during context init.
  --    

   type rmw_discovery_options_s is record
      automatic_discovery_range : aliased rmw_automatic_discovery_range_t;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:62
      static_peers : access rmw_peer_address_t;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:69
      static_peers_count : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:72
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:75
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:54

  --/ The list of manually-specified peers to perform static discovery with
  --*
  --   * Each peer is specified as a hostname or an IP address (IPv4 and IPv6 are both acceptable), or
  --   * a subnet, e.g. 192.168.0.0/24.
  --    

  --/ The number of manually-specified peers
  --/ The allocator used to allocate static_peers
   subtype rmw_discovery_options_t is rmw_discovery_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:76

  --/ Return a zero-initialized discovery options structure.
   function rmw_get_zero_initialized_discovery_options return rmw_discovery_options_t  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:82
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_discovery_options";

  --/ Initialize a discovery options structure with a set number of static peers.
  --*
  -- * This function initializes rmw_discovery_options_t with space for a set number of static peers.
  -- *
  -- * \param[in] discovery_options Pointer to a zero initialized option structure to be initialized on
  -- * success, but left unchanged on failure.
  -- * \param[in] size Number of static peers to allocate space for.
  -- * \param[in] allocator Allocator to be used to allocate memory.
  -- * \returns `RMW_RET_OK` if successful, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `discovery_options` is NULL, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `discovery_options` is not
  -- *   zero initialized, or
  -- * \returns `RMW_RET_INVALID_ARGUMENT` if `allocator` is invalid,
  -- *   by rcutils_allocator_is_valid() definition, or
  -- * \returns `RMW_BAD_ALLOC` if memory allocation fails, or
  -- * \returns `RMW_RET_ERROR` when an unspecified error occurs.
  -- * \remark This function sets the RMW error state on failure.
  -- 

   function rmw_discovery_options_init
     (discovery_options : access rmw_discovery_options_t;
      size : stddef_h.size_t;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:105
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_discovery_options_init";

  --/ Compare two discovery parameter instances for equality.
  --*
  -- * Equality means the automatic_discovery_range values are equal, they have the same
  -- * static_peers_count value, and each entry in static_peers is evaluated as
  -- * equal using strncmp.
  -- *
  -- * NOTE: If the two parameter structs list the static peers in different orders
  -- * then this will evaulate as NOT equal.
  -- *
  -- * \param[in] left - The first set of options to compare
  -- * \param[in] right - The second set of options to compare
  -- * \param[out] result - The result of the calculation.
  -- *
  -- * \return RMW_RET_OK when the input arguments are valid.
  -- * \return RMW_RET_INVALID_ARGUMENT will be returned when any input is a nullptr,
  -- * or if something in either struct was malformed, such as static_peers being
  -- * a nullptr while static_peers_count is non-zero.
  --  

   function rmw_discovery_options_equal
     (left : access constant rmw_discovery_options_t;
      right : access constant rmw_discovery_options_t;
      result : access Extensions.bool) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_discovery_options_equal";

  --/ Perform a deep copy of the discovery options from src into dst using the
  --/ given allocator.
  --*
  -- * The dst will be left with an owned copy of the static peers array whose
  -- * string values match the src.
  -- * If successful, src and dst will evaluate as equal using
  -- * rmw_discovery_options_equal.
  -- *
  -- * \param[in] src discovery options to be copied.
  -- * \param[in] allocator to use.
  -- * \param[out] dst Destination options to use.
  -- * \return RMW_RET_OK if success.
  -- * \return RMW_RET_INVALID_ARGUMENT if either the src, allocator or dst is null, or
  -- * \return RMW_RET_INVALID_ARUGMENT if src and dst are the same object.
  -- * \return RMW_RET_BAD_ALLOC if allocation fails.
  --  

   function rmw_discovery_options_copy
     (src : access constant rmw_discovery_options_t;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dst : access rmw_discovery_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:155
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_discovery_options_copy";

  --/ Destructor for rmw_discovery_options_t
  --*
  -- * \param[in] discovery_options to destroy
  -- * \param[in] allocator to be used for destruction.
  -- * \return RMW_RET_OK if success.
  -- * \return RMW_RET_INVALID_ARGUMENT if allocator is invalid
  -- * or discovery_options is null.
  --  

   function rmw_discovery_options_fini (discovery_options : access rmw_discovery_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/discovery_options.h:171
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_discovery_options_fini";

end rmw_rmw_discovery_options_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
