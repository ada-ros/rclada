pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package rcutils_isalnum_no_locale_h is

  -- Copyright 2017 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Custom isalnum() which is not affected by locale.
   function rcutils_isalnum_no_locale (c : char) return Extensions.bool;  -- /opt/ros/bouncy/include/rcutils/isalnum_no_locale.h:26
   pragma Import (C, rcutils_isalnum_no_locale, "rcutils_isalnum_no_locale");

  -- if in '0', ..., '9', then ok
  --0 
  --9 
  -- if in 'A', ..., 'Z', then ok
  --A 
  --Z 
  -- if in 'a', ..., 'z', then ok
  --a 
  --z 
end rcutils_isalnum_no_locale_h;
