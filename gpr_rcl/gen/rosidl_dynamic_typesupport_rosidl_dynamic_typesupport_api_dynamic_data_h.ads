pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcutils_rcutils_allocator_h;
with System;
with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h;
limited with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h;
with rcutils_rcutils_types_rcutils_ret_h;
with Interfaces.C.Extensions;
with stddef_h;
with Interfaces.C.Strings;
limited with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_type_h;
limited with rcutils_rcutils_types_uint8_array_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;

package rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_data_h is

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
  -- Dynamic Data Impl
   type rosidl_dynamic_typesupport_dynamic_data_impl_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:38
      handle : System.Address;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:36

   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_data_impl return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:44
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_data_impl";

  -- Dynamic Data
   type rosidl_dynamic_typesupport_dynamic_data_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:49
      impl : aliased rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:50
      serialization_support : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_h.rosidl_dynamic_typesupport_serialization_support_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:52
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:47

  -- !!! Lifetime is NOT managed by this struct
   function rosidl_dynamic_typesupport_get_zero_initialized_dynamic_data return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:57
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_dynamic_data";

  -- ===============================================================================================
  -- DYNAMIC DATA
  -- ===============================================================================================
  -- DYNAMIC DATA UTILS ==============================================================================
   function rosidl_dynamic_typesupport_dynamic_data_clear_all_values (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:66
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_clear_all_values";

   function rosidl_dynamic_typesupport_dynamic_data_clear_nonkey_values (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:71
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_clear_nonkey_values";

   function rosidl_dynamic_typesupport_dynamic_data_clear_value (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_clear_value";

   function rosidl_dynamic_typesupport_dynamic_data_equals
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      other : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      equals : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:82
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_equals";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_get_item_count (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s; item_count : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:89
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_item_count";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_get_member_id_by_name
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      name : Interfaces.C.Strings.chars_ptr;
      name_length : stddef_h.size_t;
      member_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:95
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_member_id_by_name";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_get_member_id_at_index
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      index : stddef_h.size_t;
      member_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:102
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_member_id_at_index";

  -- OUT
  -- You must use this for arrays
   function rosidl_dynamic_typesupport_dynamic_data_get_array_index
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      index : stddef_h.size_t;
      array_index : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:110
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_array_index";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_loan_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      loaned_dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_loan_value";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_return_loaned_value (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; inner_dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:125
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_return_loaned_value";

   function rosidl_dynamic_typesupport_dynamic_data_return_and_destroy_loaned_value (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; inner_dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_return_and_destroy_loaned_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_name
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      name : System.Address;
      name_length : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:137
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_name";

  -- OUT
  -- OUT
  -- DYNAMIC DATA CONSTRUCTION =======================================================================
   function rosidl_dynamic_typesupport_dynamic_data_init_from_dynamic_type_builder
     (dynamic_type_builder : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_type_h.rosidl_dynamic_typesupport_dynamic_type_builder_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:146
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_init_from_dynamic_type_builder";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_init_from_dynamic_type
     (dynamic_type : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_type_h.rosidl_dynamic_typesupport_dynamic_type_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_init_from_dynamic_type";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_clone
     (other_dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:160
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_clone";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_fini (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:167
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_fini";

   function rosidl_dynamic_typesupport_dynamic_data_destroy (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:172
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_destroy";

  -- DYNAMIC DATA SERIALIZATION ======================================================================
   function rosidl_dynamic_typesupport_dynamic_data_serialize (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; buffer : access rcutils_rcutils_types_uint8_array_h.rcutils_uint8_array_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:179
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_serialize";

  -- OUT
   function rosidl_dynamic_typesupport_dynamic_data_deserialize (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; buffer : access rcutils_rcutils_types_uint8_array_h.rcutils_uint8_array_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:185
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_deserialize";

  -- OUT
  -- DYNAMIC DATA PRIMITIVE MEMBER GETTERS ===========================================================
   function rosidl_dynamic_typesupport_dynamic_data_get_bool_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:193
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_bool_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_byte_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access unsigned_char) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:199
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_byte_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_char_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : Interfaces.C.Strings.chars_ptr) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:205
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_char_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_wchar_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access char16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_wchar_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_float32_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access float) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_float32_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_float64_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:223
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_float64_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_float128_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access long_double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:229
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_float128_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_int8_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_intn_h.int8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:235
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_int8_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_uint8_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_uint8_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_int16_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_intn_h.int16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:247
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_int16_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_uint16_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:253
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_uint16_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_int32_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_intn_h.int32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:259
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_int32_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_uint32_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:265
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_uint32_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_int64_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_int64_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_uint64_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:277
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_uint64_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_string_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:283
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_wstring_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:289
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_fixed_string_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t;
      string_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:295
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_fixed_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_fixed_wstring_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t;
      wstring_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:302
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_fixed_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_bounded_string_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t;
      string_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_bounded_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_get_bounded_wstring_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : System.Address;
      value_length : access stddef_h.size_t;
      wstring_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:316
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_bounded_wstring_value";

  -- DYNAMIC DATA PRIMITIVE MEMBER SETTERS ===========================================================
   function rosidl_dynamic_typesupport_dynamic_data_set_bool_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_bool_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_byte_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : unsigned_char) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:331
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_byte_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_char_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : char) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_char_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_wchar_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : char16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_wchar_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_float32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : float) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:349
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_float32_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_float64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:355
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_float64_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_float128_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : long_double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:361
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_float128_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_int8_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:367
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_int8_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_uint8_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:373
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_uint8_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_int16_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_int16_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_uint16_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:385
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_uint16_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_int32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:391
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_int32_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_uint32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_uint32_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_int64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:403
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_int64_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_uint64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:409
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_uint64_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:415
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access char16_t;
      value_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:421
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_fixed_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t;
      string_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:427
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_fixed_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_fixed_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access char16_t;
      value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:434
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_fixed_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_bounded_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:441
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_bounded_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_bounded_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access char16_t;
      value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:448
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_bounded_wstring_value";

  -- DYNAMIC DATA SEQUENCES ==========================================================================
   function rosidl_dynamic_typesupport_dynamic_data_clear_sequence_data (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:457
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_clear_sequence_data";

   function rosidl_dynamic_typesupport_dynamic_data_remove_sequence_data (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:462
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_remove_sequence_data";

   function rosidl_dynamic_typesupport_dynamic_data_insert_sequence_data (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s; out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:468
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_sequence_data";

   function rosidl_dynamic_typesupport_dynamic_data_insert_bool_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : Extensions.bool;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:474
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_bool_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_byte_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_byte_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_char_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : char;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:486
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_char_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_wchar_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : char16_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:492
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_wchar_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_float32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : float;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:498
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_float32_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_float64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : double;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:504
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_float64_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_float128_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : long_double;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:510
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_float128_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_int8_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:516
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_int8_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_uint8_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:522
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_uint8_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_int16_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int16_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:528
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_int16_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_uint16_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:534
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_uint16_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_int32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int32_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:540
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_int32_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_uint32_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:546
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_uint32_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_int64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:552
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_int64_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_uint64_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:558
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_uint64_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:564
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : access char16_t;
      value_length : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:570
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_fixed_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t;
      string_length : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:576
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_fixed_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_fixed_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : access char16_t;
      value_length : stddef_h.size_t;
      wstring_length : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:582
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_fixed_wstring_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_bounded_string_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : Interfaces.C.Strings.chars_ptr;
      value_length : stddef_h.size_t;
      string_bound : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:588
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_bounded_string_value";

   function rosidl_dynamic_typesupport_dynamic_data_insert_bounded_wstring_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : access char16_t;
      value_length : stddef_h.size_t;
      wstring_bound : stddef_h.size_t;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:594
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_bounded_wstring_value";

  -- DYNAMIC DATA NESTED =============================================================================
  -- The user is expected to allocate the '** value' outparam outside
  -- This function will then reassign the '** value''s 'serialization_support' member to match the
  -- input's
   function rosidl_dynamic_typesupport_dynamic_data_get_complex_value
     (dynamic_data : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      allocator : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
      value : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:605
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_get_complex_value";

   function rosidl_dynamic_typesupport_dynamic_data_set_complex_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      id : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
      value : access rosidl_dynamic_typesupport_dynamic_data_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:613
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_set_complex_value";

  -- This deep copies the passed data
   function rosidl_dynamic_typesupport_dynamic_data_insert_complex_value_copy
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : access constant rosidl_dynamic_typesupport_dynamic_data_s;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:620
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_complex_value_copy";

   function rosidl_dynamic_typesupport_dynamic_data_insert_complex_value
     (dynamic_data : access rosidl_dynamic_typesupport_dynamic_data_s;
      value : access rosidl_dynamic_typesupport_dynamic_data_s;
      out_id : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/dynamic_data.h:627
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_dynamic_data_insert_complex_value";

end rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_dynamic_data_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
