pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcutils_rcutils_allocator_h;
with Interfaces.C.Strings;
with System;
with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h;
limited with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_interface_h;
with rcutils_rcutils_types_rcutils_ret_h;

package rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h is

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
  --/ Polymorphic serialization support interface
  -- Copyright 2022 Open Source Robotics Foundation, Inc.
  --/ Serialization Support Impl
  --/ For anything necessary or useful for the operation of the serialization lib
  --/ (e.g. singleton dynamic type and dynamic data factories)
   type rosidl_dynamic_typesupport_serialization_support_impl_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:39
      serialization_library_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:40
      handle : System.Address;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:37

   function rosidl_dynamic_typesupport_get_zero_initialized_serialization_support_impl return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:46
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_serialization_support_impl";

  --/ Serialization Support
  --/ This is the main structure that encompasses:
  --/   - impl - The library-specific objects or implementation details
  --/   - methods - The shared serialization support interface, populated with serialization
  --/     library-specific function pointers
   type rosidl_dynamic_typesupport_serialization_support_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:55
      serialization_library_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:56
      impl : aliased rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:58
      methods : aliased rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_interface_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:60
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:53

  -- Can't call it `interface` because it's a reserved term in some Windows versions...
   function rosidl_dynamic_typesupport_get_zero_initialized_serialization_support return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_serialization_support";

  -- CORE ============================================================================================
   function rosidl_dynamic_typesupport_serialization_support_get_library_identifier (serialization_support : access constant rosidl_dynamic_typesupport_serialization_support_s) return Interfaces.C.Strings.chars_ptr  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:70
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_serialization_support_get_library_identifier";

   function rosidl_dynamic_typesupport_serialization_support_init
     (impl : access rosidl_dynamic_typesupport_serialization_support_impl_s;
      methods : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_interface_h.rosidl_dynamic_typesupport_serialization_support_interface_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      serialization_support : access rosidl_dynamic_typesupport_serialization_support_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:75
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_serialization_support_init";

  -- OUT
   function rosidl_dynamic_typesupport_serialization_support_fini (serialization_support : access rosidl_dynamic_typesupport_serialization_support_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support.h:83
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_serialization_support_fini";

end rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
