pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package rcutils_macros_h is

   --  unsupported macro: RCUTILS_WARN_UNUSED __attribute__((warn_unused_result))
   --  unsupported macro: RCUTILS_THREAD_LOCAL _Thread_local
   --  unsupported macro: RCUTILS_STRINGIFY_IMPL(x) #x
   --  arg-macro: procedure RCUTILS_STRINGIFY (x)
   --    RCUTILS_STRINGIFY_IMPL(x)
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
  -- Note: this block was migrated from rmw/macros.h
  -- This block either sets RCUTILS_THREAD_LOCAL or RCUTILS_THREAD_LOCAL_PTHREAD.
  -- Windows or Cygwin
  -- Apple OS's
  -- iOS Simulator or iOS device
  -- iOS >= 10, thread local storage was added in iOS 10
  -- iOS < 10, no thread local storage, so use pthread instead
  -- macOS
  -- Some other non-Windows, non-cygwin, non-apple OS
end rcutils_macros_h;
