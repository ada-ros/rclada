pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;
with System;
limited with rmw_types_h;

package rmw_allocators_h is

  -- Copyright 2014 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   function rmw_allocate (size : stddef_h.size_t) return System.Address;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:28
   pragma Import (C, rmw_allocate, "rmw_allocate");

   procedure rmw_free (pointer : System.Address);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:32
   pragma Import (C, rmw_free, "rmw_free");

   function rmw_node_allocate return access rmw_types_h.rmw_node_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:36
   pragma Import (C, rmw_node_allocate, "rmw_node_allocate");

   procedure rmw_node_free (node : access rmw_types_h.rmw_node_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:40
   pragma Import (C, rmw_node_free, "rmw_node_free");

   function rmw_publisher_allocate return access rmw_types_h.rmw_publisher_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:44
   pragma Import (C, rmw_publisher_allocate, "rmw_publisher_allocate");

   procedure rmw_publisher_free (publisher : access rmw_types_h.rmw_publisher_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:48
   pragma Import (C, rmw_publisher_free, "rmw_publisher_free");

   function rmw_subscription_allocate return access rmw_types_h.rmw_subscription_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:52
   pragma Import (C, rmw_subscription_allocate, "rmw_subscription_allocate");

   procedure rmw_subscription_free (subscription : access rmw_types_h.rmw_subscription_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:56
   pragma Import (C, rmw_subscription_free, "rmw_subscription_free");

   function rmw_guard_condition_allocate return access rmw_types_h.rmw_guard_condition_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:60
   pragma Import (C, rmw_guard_condition_allocate, "rmw_guard_condition_allocate");

   procedure rmw_guard_condition_free (guard_condition : access rmw_types_h.rmw_guard_condition_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:64
   pragma Import (C, rmw_guard_condition_free, "rmw_guard_condition_free");

   function rmw_client_allocate return access rmw_types_h.rmw_client_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:68
   pragma Import (C, rmw_client_allocate, "rmw_client_allocate");

   procedure rmw_client_free (client : access rmw_types_h.rmw_client_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:72
   pragma Import (C, rmw_client_free, "rmw_client_free");

   function rmw_service_allocate return access rmw_types_h.rmw_service_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:76
   pragma Import (C, rmw_service_allocate, "rmw_service_allocate");

   procedure rmw_service_free (service : access rmw_types_h.rmw_service_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:80
   pragma Import (C, rmw_service_free, "rmw_service_free");

   function rmw_wait_set_allocate return access rmw_types_h.rmw_wait_set_t;  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:84
   pragma Import (C, rmw_wait_set_allocate, "rmw_wait_set_allocate");

   procedure rmw_wait_set_free (wait_set : access rmw_types_h.rmw_wait_set_t);  -- /home/jano/local/ros2/ros2_bouncy/install/rmw/include/rmw/allocators.h:88
   pragma Import (C, rmw_wait_set_free, "rmw_wait_set_free");

end rmw_allocators_h;
