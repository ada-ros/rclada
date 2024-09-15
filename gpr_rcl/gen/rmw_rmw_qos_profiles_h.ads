pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rmw_rmw_types_h;
with Interfaces.C.Strings;
with stddef_h;
with rmw_rmw_ret_types_h;

package rmw_rmw_qos_profiles_h is

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
   rmw_qos_profile_sensor_data : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:25
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL27rmw_qos_profile_sensor_data";

   rmw_qos_profile_parameters : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:38
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL26rmw_qos_profile_parameters";

   rmw_qos_profile_default : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:51
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL23rmw_qos_profile_default";

   rmw_qos_profile_services_default : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:64
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL32rmw_qos_profile_services_default";

   rmw_qos_profile_parameter_events : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:77
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL32rmw_qos_profile_parameter_events";

   rmw_qos_profile_system_default : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:90
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL30rmw_qos_profile_system_default";

  --/ Match majority of endpoints currently available while maintaining the highest level of service
  --*
  -- * Reliability, durability, deadline, liveliness, and liveliness lease duration policies will be
  -- * chosen at the time of creating a subscription or publisher.
  -- *
  -- * The actual QoS policy can be retrieved after the endpoint is created with
  -- * `rmw_get_subscriptions_info_by_topic` or `rmw_get_publishers_info_by_topic`.
  -- *
  -- * The middleware is not expected to update policies after creating a subscription or
  -- * publisher, even if one or more policies are incompatible with newly discovered endpoints.
  -- * Therefore, this profile should be used with care since non-deterministic behavior
  -- * can occur due to races with discovery.
  --  

   rmw_qos_profile_best_available : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:116
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL30rmw_qos_profile_best_available";

   rmw_qos_profile_unknown : aliased constant rmw_rmw_types_h.rmw_qos_profile_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:129
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL23rmw_qos_profile_unknown";

   type rmw_qos_compatibility_type_e is 
     (RMW_QOS_COMPATIBILITY_OK,
      RMW_QOS_COMPATIBILITY_WARNING,
      RMW_QOS_COMPATIBILITY_ERROR)
   with Convention => C;  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:142

  --/ QoS policies are compatible
  --/ QoS policies may not be compatible
  --/ QoS policies are not compatible
   subtype rmw_qos_compatibility_type_t is rmw_qos_compatibility_type_e;  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:152

  --/ Check if two QoS profiles are compatible.
  --*
  -- * Two QoS profiles are compatible if a publisher and subcription
  -- * using the QoS policies can communicate with each other.
  -- *
  -- * If any of the profile policies has the value "system default" or "unknown", then it may not be
  -- * possible to determine the compatibilty.
  -- * In this case, the output parameter `compatibility` is set to `RMW_QOS_COMPATIBILITY_WARNING`
  -- * and `reason` is populated.
  -- *
  -- * If there is a compatibility warning or error, and a buffer is provided for `reason`, then an
  -- * explanation of all warnings and errors will be populated into the buffer, separated by
  -- * semi-colons (`;`).
  -- * Errors will appear before warnings in the string buffer.
  -- * If the provided buffer is not large enough, this function will still write to the buffer, up to
  -- * the `reason_size` number of characters.
  -- * Therefore, it is possible that not all errors and warnings are communicated if the buffer size limit
  -- * is reached.
  -- * A buffer size of 2048 should be more than enough to capture all possible errors and warnings.
  -- *
  -- * <hr>
  -- * Attribute          | Adherence
  -- * ------------------ | -------------
  -- * Allocates Memory   | No
  -- * Thread-Safe        | Yes
  -- * Uses Atomics       | No
  -- * Lock-Free          | Yes
  -- *
  -- * \param[in] publisher_profile: The QoS profile used for a publisher.
  -- * \param[in] subscription_profile: The QoS profile used for a subscription.
  -- * \param[out] compatibility: `RMW_QOS_COMPATIBILITY_OK` if the QoS profiles are compatible, or
  -- *   `RMW_QOS_COMPATIBILITY_WARNING` if the QoS profiles might be compatible, or
  -- *   `RMW_QOS_COMPATIBILITY_ERROR` if the QoS profiles are not compatible.
  -- * \param[out] reason: A detailed reason for a QoS incompatibility or potential incompatibility.
  -- *   Must be pre-allocated by the caller.
  -- *   This parameter is optional and may be set to `NULL` if the reason information is not
  -- *   desired.
  -- * \param[in] reason_size: Size of the string buffer `reason`, if one is provided.
  -- *   If `reason` is `nullptr`, then this parameter must be zero.
  -- * \return `RMW_RET_OK` if the check was successful, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if `compatibility` is `nullptr`, or
  -- * \return `RMW_RET_INVALID_ARGUMENT` if `reason` is `NULL` and  `reason_size` is not zero, or
  -- * \return `RMW_RET_ERROR` if there is an unexpected error.
  --  

   function rmw_qos_profile_check_compatible
     (publisher_profile : rmw_rmw_types_h.rmw_qos_profile_t;
      subscription_profile : rmw_rmw_types_h.rmw_qos_profile_t;
      compatibility : access rmw_qos_compatibility_type_t;
      reason : Interfaces.C.Strings.chars_ptr;
      reason_size : stddef_h.size_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/qos_profiles.h:202
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_qos_profile_check_compatible";

end rmw_rmw_qos_profiles_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
