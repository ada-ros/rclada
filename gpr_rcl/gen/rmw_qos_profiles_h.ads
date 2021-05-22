pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_types_h;

package rmw_qos_profiles_h is

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
   rmw_qos_profile_sensor_data : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:25
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL27rmw_qos_profile_sensor_data";

   rmw_qos_profile_parameters : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:38
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL26rmw_qos_profile_parameters";

   rmw_qos_profile_default : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:51
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL23rmw_qos_profile_default";

   rmw_qos_profile_services_default : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:64
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL32rmw_qos_profile_services_default";

   rmw_qos_profile_parameter_events : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:77
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL32rmw_qos_profile_parameter_events";

   rmw_qos_profile_system_default : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:90
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL30rmw_qos_profile_system_default";

   rmw_qos_profile_unknown : aliased constant rmw_types_h.rmw_qos_profile_t  -- /opt/ros/foxy/include/rmw/qos_profiles.h:103
   with Import => True, 
        Convention => CPP, 
        External_Name => "_ZL23rmw_qos_profile_unknown";

end rmw_qos_profiles_h;
