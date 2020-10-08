pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package rmw_localhost_h is

  -- Copyright 2019 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Used to specify if the context can only communicate through localhost.
   type rmw_localhost_only_t is 
     (RMW_LOCALHOST_ONLY_DEFAULT,
      RMW_LOCALHOST_ONLY_ENABLED,
      RMW_LOCALHOST_ONLY_DISABLED)
   with Convention => C;  -- /opt/ros/foxy/include/rmw/localhost.h:26

  --/ Uses ROS_LOCALHOST_ONLY environment variable.
  --/ Forces using only localhost.
  --/ Forces disabling localhost only.
end rmw_localhost_h;
