pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcutils_rcutils_allocator_h;
with System;
with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h;
limited with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h;
with Interfaces.C.Extensions;
with rcutils_rcutils_types_rcutils_ret_h;
with stddef_h;
with Interfaces.C.Strings;
limited with rosidl_runtime_c_rosidl_runtime_c_type_description_type_description_ustruct_h;

package rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_type_h is

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
  -- Dynamic Type Builder Impl
   type rosidl_dynamic_typesupport_dynamic_type_builder_impl_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:36
      handle : System.Address;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:37
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:34

   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_builder_impl return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:42
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_builder_impl";

  -- Dynamic Type Builder
   type rosidl_dynamic_typesupport_dynamic_type_builder_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:47
      impl : aliased rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:48
      serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:50
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:45

  -- !!! Lifetime is NOT managed by this struct
   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_builder return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:55
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_builder";

  -- Dynamic Type Impl
   type rosidl_dynamic_typesupport_dynamic_type_impl_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:60
      handle : System.Address;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:61
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:58

   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_impl return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type_impl";

  -- Dynamic Type
   type rosidl_dynamic_typesupport_dynamic_type_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:71
      impl : aliased rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:72
      serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:74
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:69

  -- !!! Lifetime is NOT managed by this struct
   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:79
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_type";

  -- =================================================================================================
  -- DYNAMIC TYPE
  -- =================================================================================================
  -- DYNAMIC TYPE UTILS ==============================================================================
   function rosidl_dynamic_typesupport_dynamic_type_equals
     (dynamic_type : access constant rosidl_dynamic_typesupport_dynamic_type_s;
      other : access constant rosidl_dynamic_typesupport_dynamic_type_s;
      equals : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_equals";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_get_member_count (dynamic_type : access constant rosidl_dynamic_typesupport_dynamic_type_s; member_count : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_get_member_count";

  -- OUT
  -- DYNAMIC TYPE CONSTRUCTION =======================================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_init
     (serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_init";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_builder_clone
     (other : access constant rosidl_dynamic_typesupport_dynamic_type_builder_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:111
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_clone";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_builder_init_from_description
     (serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;
      description : access constant rosidl_runtime_c_rosidl_runtime_c_type_description_type_description_ustruct_h.rosidl_runtime_c_u_type_description_u_TypeDescription;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:118
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_init_from_description";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_builder_fini (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:126
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_fini";

   function rosidl_dynamic_typesupport_dynamic_type_builder_destroy (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_destroy";

   function rosidl_dynamic_typesupport_dynamic_type_init_from_dynamic_type_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:136
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_init_from_dynamic_type_builder";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_init_from_description
     (serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;
      description : access constant rosidl_runtime_c_rosidl_runtime_c_type_description_type_description_ustruct_h.rosidl_runtime_c_u_type_description_u_TypeDescription;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:143
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_init_from_description";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_clone
     (other : access constant rosidl_dynamic_typesupport_dynamic_type_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_type : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:151
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_clone";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_fini (dynamic_type : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:158
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_fini";

   function rosidl_dynamic_typesupport_dynamic_type_destroy (dynamic_type : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:163
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_destroy";

   function rosidl_dynamic_typesupport_dynamic_type_get_name
     (dynamic_type : access constant rosidl_dynamic_typesupport_dynamic_type_s;
      name : System.Address;
      name_length : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:168
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_get_name";

  -- OUT
  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_builder_get_name
     (dynamic_type_builder : access constant rosidl_dynamic_typesupport_dynamic_type_builder_s;
      name : System.Address;
      name_length : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:175
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_get_name";

  -- OUT
  -- OUT
   function rosidl_dynamic_typesupport_dynamic_type_builder_set_name
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:182
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_set_name";

  -- DYNAMIC TYPE PRIMITIVE MEMBERS ==================================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:190
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:198
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_char_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:206
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_char_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:214
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:222
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:246
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:254
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:262
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:270
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:278
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:286
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:294
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:302
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_string_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:310
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_string_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:326
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:344
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:353
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_member";

  -- DYNAMIC TYPE STATIC ARRAY MEMBERS ===============================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:364
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:373
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_char_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:382
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_char_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:400
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:409
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:418
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:427
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:436
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:445
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:454
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:463
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:472
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:481
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:490
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_string_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:499
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_string_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:508
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:517
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:526
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:535
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:544
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_array_member";

  -- DYNAMIC TYPE UNBOUNDED SEQUENCE MEMBERS =========================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:555
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:563
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_char_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:571
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_char_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:579
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:587
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:595
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:603
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:611
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:619
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:627
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:635
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:643
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:651
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:659
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:667
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_string_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:675
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_string_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:683
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:691
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:700
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:709
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:718
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_unbounded_sequence_member";

  -- DYNAMIC TYPE BOUNDED SEQUENCE MEMBERS ===========================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:729
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bool_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:738
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_byte_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_char_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:747
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_char_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:756
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wchar_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:765
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float32_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:774
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float64_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:783
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_float128_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:792
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int8_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:801
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint8_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:810
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int16_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:819
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint16_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:828
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int32_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:837
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint32_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:846
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_int64_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:855
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_uint64_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_string_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:864
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_string_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:873
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_wstring_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:882
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_string_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:891
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_bounded_wstring_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      string_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:900
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_string_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:909
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_fixed_wstring_bounded_sequence_member";

  -- DYNAMIC TYPE NESTED MEMBERS =====================================================================
   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:920
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_array_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct : access rosidl_dynamic_typesupport_dynamic_type_s;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:929
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_array_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_unbounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct : access rosidl_dynamic_typesupport_dynamic_type_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:938
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_unbounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_bounded_sequence_member
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct : access rosidl_dynamic_typesupport_dynamic_type_s;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:947
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_bounded_sequence_member";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_member_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:956
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_member_builder";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_array_member_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      array_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:965
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_array_member_builder";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_unbounded_sequence_member_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:974
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_unbounded_sequence_member_builder";

   function rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_bounded_sequence_member_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      default_value : Interfaces.C.Strings.chars_ptr;
      default_value_length : stddef_h.size_t;
      nested_struct_builder : access rosidl_dynamic_typesupport_dynamic_type_builder_s;
      sequence_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_type.h:983
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_type_builder_add_complex_bounded_sequence_member_builder";

end rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_type_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
