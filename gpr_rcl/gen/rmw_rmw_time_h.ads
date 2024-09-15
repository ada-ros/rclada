pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with rcutils_rcutils_time_h;
with Interfaces.C.Extensions;

package rmw_rmw_time_h is

   --  unsupported macro: RMW_DURATION_INFINITE {9223372036LL, 854775807LL}
   --  unsupported macro: RMW_DURATION_UNSPECIFIED {0LL, 0LL}
  -- Copyright 2021 Amazon.com, Inc. or its affiliates. All Rights Reserved.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ A struct representing a duration or relative time in RMW - does not encode an origin.
  --/ Seconds component
   type rmw_time_s is record
      sec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:34
      nsec : aliased x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:37
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:31

  --/ Nanoseconds component
   subtype rmw_time_t is rmw_time_s;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:38

   subtype rmw_time_point_value_t is rcutils_rcutils_time_h.rcutils_time_point_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:40

   subtype rmw_duration_t is rcutils_rcutils_time_h.rcutils_duration_value_t;  -- /opt/ros/jazzy/include/rmw/rmw/time.h:41

  --/ Constant representing an infinite duration. Use rmw_time_equal for comparisons.
  --*
  --  * Different RMW implementations have different representations for infinite durations.
  --  * This value is reported for QoS policy durations that are left unspecified.
  --  * Do not directly compare `sec == sec && nsec == nsec`, because we don't want to be sensitive
  --  * to non-normalized values (nsec > 1 second) - use rmw_time_equal instead.
  --  * This value is INT64_MAX nanoseconds = 0x7FFF FFFF FFFF FFFF = d 9 223 372 036 854 775 807
  --  *
  --  * Note: these constants cannot be `static const rmw_time_t` because in C that can't be used
  --  * as a compile-time initializer
  --   

  --/ Check whether two rmw_time_t represent the same time.
   function rmw_time_equal (left : rmw_time_t; right : rmw_time_t) return Extensions.bool  -- /opt/ros/jazzy/include/rmw/rmw/time.h:61
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_time_equal";

  --/ Return the total nanosecond representation of a time.
  --*
  --  * \return INT64_MAX if input is too large to store in 64 bits
  --   

   function rmw_time_total_nsec (time : rmw_time_t) return rmw_duration_t  -- /opt/ros/jazzy/include/rmw/rmw/time.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_time_total_nsec";

  --/ Construct rmw_time_t from a total nanoseconds representation.
  --*
  --  * rmw_time_t only specifies relative time, so the origin is not relevant for this calculation.
  --  * \return RMW_DURATION_INFINITE if input is negative, which is not representable in rmw_time_t
  --   

   function rmw_time_from_nsec (nanoseconds : rmw_duration_t) return rmw_time_t  -- /opt/ros/jazzy/include/rmw/rmw/time.h:80
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_time_from_nsec";

  --/ Ensure that an rmw_time_t does not have nanoseconds > 1 second.
   function rmw_time_normalize (time : rmw_time_t) return rmw_time_t  -- /opt/ros/jazzy/include/rmw/rmw/time.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_time_normalize";

end rmw_rmw_time_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
