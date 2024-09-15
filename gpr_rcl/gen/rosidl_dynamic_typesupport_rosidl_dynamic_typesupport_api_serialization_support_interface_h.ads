pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with rcutils_rcutils_allocator_h;
with Interfaces.C.Strings;
with rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h;
with rcutils_rcutils_types_rcutils_ret_h;
with Interfaces.C.Extensions;
with stddef_h;
with System;
limited with rcutils_rcutils_types_uint8_array_h;
with x86_64_linux_gnu_bits_stdint_uintn_h;
with x86_64_linux_gnu_bits_stdint_intn_h;

package rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_interface_h is

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
  --/ Downstream middlewares should populate this interface as appropriate
  --/ This interface must be adopted by all downstream serialization library implementations
  -- =================================================================================================
  -- Interface
  -- =================================================================================================
  --/ Interfaces mimicking the XTypes spec (Section 7.5: Language Binding)
  --/ https://www.omg.org/spec/DDS-XTypes/1.1/PDF
  --/
  --/ Luckily for us, FastRTPS mimics the spec quite well
  -- TODOS??? (though these are just bonuses...)
  --   *   - DynamicType::get_type_descriptor / DynamicType::get_descriptor (and TypeDescriptor class)
  --   *
  --   * I'm not sure if these are necessary, given the fact we will have the type description message
  --   * to guide the traversal? Also it's ambiguous what type we should be returning...:
  --   *   - DynamicType::get_all_members (Returns map of member ID to member)
  --   *   - DynamicType::get_all_members_by_name (Returns map of member name to member)
  --    

  -- CORE
   type rosidl_dynamic_typesupport_serialization_support_interface_s is record
      allocator : aliased rcutils_rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:58
      serialization_library_identifier : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:59
      serialization_support_impl_fini : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:61
      serialization_support_interface_fini : access function (arg1 : access rosidl_dynamic_typesupport_serialization_support_interface_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:64
      dynamic_type_equals : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg3 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg4 : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:72
      dynamic_type_get_member_count : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg3 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:79
      dynamic_type_builder_init : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : stddef_h.size_t;
            arg4 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:86
      dynamic_type_builder_clone : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:92
      dynamic_type_builder_fini : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:98
      dynamic_type_init_from_dynamic_type_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:103
      dynamic_type_clone : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:109
      dynamic_type_fini : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:115
      dynamic_type_get_name : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg3 : System.Address;
            arg4 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:120
      dynamic_type_builder_get_name : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : System.Address;
            arg4 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:126
      dynamic_type_builder_set_name : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:132
      dynamic_type_builder_add_bool_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:139
      dynamic_type_builder_add_byte_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:146
      dynamic_type_builder_add_char_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:153
      dynamic_type_builder_add_wchar_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:160
      dynamic_type_builder_add_float32_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:167
      dynamic_type_builder_add_float64_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:174
      dynamic_type_builder_add_float128_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:181
      dynamic_type_builder_add_int8_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:188
      dynamic_type_builder_add_uint8_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:195
      dynamic_type_builder_add_int16_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:202
      dynamic_type_builder_add_uint16_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:209
      dynamic_type_builder_add_int32_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:216
      dynamic_type_builder_add_uint32_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:223
      dynamic_type_builder_add_int64_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:230
      dynamic_type_builder_add_uint64_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:237
      dynamic_type_builder_add_string_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:244
      dynamic_type_builder_add_wstring_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:251
      dynamic_type_builder_add_fixed_string_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:258
      dynamic_type_builder_add_fixed_wstring_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:266
      dynamic_type_builder_add_bounded_string_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:274
      dynamic_type_builder_add_bounded_wstring_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:282
      dynamic_type_builder_add_bool_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:292
      dynamic_type_builder_add_byte_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:300
      dynamic_type_builder_add_char_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:308
      dynamic_type_builder_add_wchar_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:316
      dynamic_type_builder_add_float32_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:324
      dynamic_type_builder_add_float64_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:332
      dynamic_type_builder_add_float128_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:340
      dynamic_type_builder_add_int8_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:348
      dynamic_type_builder_add_uint8_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:356
      dynamic_type_builder_add_int16_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:364
      dynamic_type_builder_add_uint16_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:372
      dynamic_type_builder_add_int32_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:380
      dynamic_type_builder_add_uint32_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:388
      dynamic_type_builder_add_int64_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:396
      dynamic_type_builder_add_uint64_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:404
      dynamic_type_builder_add_string_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:412
      dynamic_type_builder_add_wstring_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:420
      dynamic_type_builder_add_fixed_string_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:428
      dynamic_type_builder_add_fixed_wstring_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:436
      dynamic_type_builder_add_bounded_string_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:444
      dynamic_type_builder_add_bounded_wstring_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:452
      dynamic_type_builder_add_bool_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:462
      dynamic_type_builder_add_byte_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:469
      dynamic_type_builder_add_char_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:476
      dynamic_type_builder_add_wchar_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:483
      dynamic_type_builder_add_float32_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:490
      dynamic_type_builder_add_float64_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:497
      dynamic_type_builder_add_float128_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:504
      dynamic_type_builder_add_int8_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:511
      dynamic_type_builder_add_uint8_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:518
      dynamic_type_builder_add_int16_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:525
      dynamic_type_builder_add_uint16_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:532
      dynamic_type_builder_add_int32_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:539
      dynamic_type_builder_add_uint32_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:546
      dynamic_type_builder_add_int64_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:553
      dynamic_type_builder_add_uint64_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:560
      dynamic_type_builder_add_string_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:567
      dynamic_type_builder_add_wstring_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:574
      dynamic_type_builder_add_fixed_string_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:581
      dynamic_type_builder_add_fixed_wstring_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:589
      dynamic_type_builder_add_bounded_string_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:597
      dynamic_type_builder_add_bounded_wstring_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:605
      dynamic_type_builder_add_bool_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:615
      dynamic_type_builder_add_byte_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:623
      dynamic_type_builder_add_char_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:631
      dynamic_type_builder_add_wchar_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:639
      dynamic_type_builder_add_float32_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:647
      dynamic_type_builder_add_float64_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:655
      dynamic_type_builder_add_float128_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:663
      dynamic_type_builder_add_int8_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:671
      dynamic_type_builder_add_uint8_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:679
      dynamic_type_builder_add_int16_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:687
      dynamic_type_builder_add_uint16_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:695
      dynamic_type_builder_add_int32_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:703
      dynamic_type_builder_add_uint32_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:711
      dynamic_type_builder_add_int64_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:719
      dynamic_type_builder_add_uint64_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:727
      dynamic_type_builder_add_string_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:735
      dynamic_type_builder_add_wstring_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:743
      dynamic_type_builder_add_fixed_string_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:751
      dynamic_type_builder_add_fixed_wstring_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:759
      dynamic_type_builder_add_bounded_string_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:767
      dynamic_type_builder_add_bounded_wstring_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : stddef_h.size_t;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:775
      dynamic_type_builder_add_complex_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:785
      dynamic_type_builder_add_complex_array_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:793
      dynamic_type_builder_add_complex_unbounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:801
      dynamic_type_builder_add_complex_bounded_sequence_member : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:809
      dynamic_type_builder_add_complex_member_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:818
      dynamic_type_builder_add_complex_array_member_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:826
      dynamic_type_builder_add_complex_unbounded_sequence_member_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:835
      dynamic_type_builder_add_complex_bounded_sequence_member_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : Interfaces.C.Strings.chars_ptr;
            arg7 : stddef_h.size_t;
            arg8 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg9 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:843
      dynamic_data_clear_all_values : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:858
      dynamic_data_clear_nonkey_values : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:862
      dynamic_data_clear_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:866
      dynamic_data_equals : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg4 : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:872
      dynamic_data_get_item_count : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:879
      dynamic_data_get_member_id_by_name : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : stddef_h.size_t;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:884
      dynamic_data_get_member_id_at_index : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : stddef_h.size_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:890
      dynamic_data_get_array_index : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : stddef_h.size_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:896
      dynamic_data_loan_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:903
      dynamic_data_return_loaned_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:910
      dynamic_data_get_name : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : System.Address;
            arg4 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:915
      dynamic_data_init_from_dynamic_type_builder : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:923
      dynamic_data_init_from_dynamic_type : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_type_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:929
      dynamic_data_clone : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:935
      dynamic_data_fini : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:941
      dynamic_data_serialize : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access rcutils_rcutils_types_uint8_array_h.rcutils_uint8_array_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:958
      dynamic_data_deserialize : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access rcutils_rcutils_types_uint8_array_h.rcutils_uint8_array_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:963
      dynamic_data_get_bool_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:970
      dynamic_data_get_byte_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:976
      dynamic_data_get_char_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:982
      dynamic_data_get_wchar_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access char16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:988
      dynamic_data_get_float32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access float) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:994
      dynamic_data_get_float64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1000
      dynamic_data_get_float128_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access long_double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1006
      dynamic_data_get_int8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_intn_h.int8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1012
      dynamic_data_get_uint8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1018
      dynamic_data_get_int16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_intn_h.int16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1024
      dynamic_data_get_uint16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1030
      dynamic_data_get_int32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_intn_h.int32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1036
      dynamic_data_get_uint32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1042
      dynamic_data_get_int64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1048
      dynamic_data_get_uint64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1054
      dynamic_data_get_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1060
      dynamic_data_get_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1067
      dynamic_data_get_fixed_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1074
      dynamic_data_get_fixed_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1082
      dynamic_data_get_bounded_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1090
      dynamic_data_get_bounded_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : System.Address;
            arg5 : access stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1098
      dynamic_data_set_bool_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Extensions.bool) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1108
      dynamic_data_set_byte_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1113
      dynamic_data_set_char_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : char) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1118
      dynamic_data_set_wchar_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : char16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1123
      dynamic_data_set_float32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : float) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1128
      dynamic_data_set_float64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1133
      dynamic_data_set_float128_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : long_double) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1138
      dynamic_data_set_int8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_intn_h.int8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1143
      dynamic_data_set_uint8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1148
      dynamic_data_set_int16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_intn_h.int16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1153
      dynamic_data_set_uint16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1158
      dynamic_data_set_int32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_intn_h.int32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1163
      dynamic_data_set_uint32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1168
      dynamic_data_set_int64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_intn_h.int64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1173
      dynamic_data_set_uint64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1178
      dynamic_data_set_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1183
      dynamic_data_set_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access char16_t;
            arg5 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1189
      dynamic_data_set_fixed_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1195
      dynamic_data_set_fixed_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access char16_t;
            arg5 : stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1202
      dynamic_data_set_bounded_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : Interfaces.C.Strings.chars_ptr;
            arg5 : stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1209
      dynamic_data_set_bounded_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access char16_t;
            arg5 : stddef_h.size_t;
            arg6 : stddef_h.size_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1216
      dynamic_data_clear_sequence_data : access function (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s; arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1225
      dynamic_data_remove_sequence_data : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1229
      dynamic_data_insert_sequence_data : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1234
      dynamic_data_insert_bool_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : Extensions.bool;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1240
      dynamic_data_insert_byte_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1246
      dynamic_data_insert_char_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : char;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1252
      dynamic_data_insert_wchar_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : char16_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1258
      dynamic_data_insert_float32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : float;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1264
      dynamic_data_insert_float64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : double;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1270
      dynamic_data_insert_float128_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : long_double;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1276
      dynamic_data_insert_int8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_intn_h.int8_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1282
      dynamic_data_insert_uint8_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_uintn_h.uint8_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1288
      dynamic_data_insert_int16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_intn_h.int16_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1294
      dynamic_data_insert_uint16_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_uintn_h.uint16_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1300
      dynamic_data_insert_int32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_intn_h.int32_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1306
      dynamic_data_insert_uint32_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_uintn_h.uint32_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1312
      dynamic_data_insert_int64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_intn_h.int64_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1318
      dynamic_data_insert_uint64_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : x86_64_linux_gnu_bits_stdint_uintn_h.uint64_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1324
      dynamic_data_insert_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : stddef_h.size_t;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1330
      dynamic_data_insert_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access char16_t;
            arg4 : stddef_h.size_t;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1336
      dynamic_data_insert_fixed_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : stddef_h.size_t;
            arg5 : stddef_h.size_t;
            arg6 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1342
      dynamic_data_insert_fixed_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access char16_t;
            arg4 : stddef_h.size_t;
            arg5 : stddef_h.size_t;
            arg6 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1348
      dynamic_data_insert_bounded_string_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : Interfaces.C.Strings.chars_ptr;
            arg4 : stddef_h.size_t;
            arg5 : stddef_h.size_t;
            arg6 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1354
      dynamic_data_insert_bounded_wstring_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access char16_t;
            arg4 : stddef_h.size_t;
            arg5 : stddef_h.size_t;
            arg6 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1360
      dynamic_data_get_complex_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access rcutils_rcutils_allocator_h.rcutils_allocator_s;
            arg5 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1368
      dynamic_data_set_complex_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1375
      dynamic_data_insert_complex_value_copy : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access constant rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1382
      dynamic_data_insert_complex_value : access function
           (arg1 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_impl_s;
            arg2 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg3 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_dynamic_data_impl_s;
            arg4 : access rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_member_id_t) return rcutils_rcutils_types_rcutils_ret_h.rcutils_ret_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1388
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:41

  -- ===============================================================================================
  -- DYNAMIC TYPE
  -- ===============================================================================================
  -- DYNAMIC TYPE UTILS
  -- OUT
  -- "member" from XTypes spec
  -- OUT
  -- DYNAMIC TYPE CONSTRUCTION
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- DYNAMIC TYPE PRIMITIVE MEMBERS
  -- DYNAMIC TYPE STATIC ARRAY MEMBERS
  -- DYNAMIC TYPE UNBOUNDED SEQUENCE MEMBERS
  -- DYNAMIC TYPE BOUNDED SEQUENCE MEMBERS
  -- DYNAMIC TYPE NESTED MEMBERS
  -- ===============================================================================================
  -- DYNAMIC DATA
  -- ===============================================================================================
  -- DYNAMIC DATA UTILS
  -- OUT
  -- "item" from XTypes
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- DYNAMIC DATA CONSTRUCTION
  -- OUT
  -- OUT
  -- OUT
  -- DYNAMIC DATA SERIALIZATION
  -- NOTE(methylDragon): I'm not sure if rcutils_uint8_array_t is the right type to pass...
  --                     On the other hand it plays well with rmw and stores the buffer, length, and
  --                     capacity...
  --                     I also considered using a void * instead, but I like the idea of forcing
  --                     serialization support libraries to play with uint8_t *s instead of their
  --                     own native type.
  --                     ... Though I'm betting that any of their types can be accurately
  --                     represented as such a byte array
  -- NOTE(methylDragon): rmw_serialized_message_t is a typedef of rcutils_uint8_array_t
  -- OUT
  -- OUT
  -- DYNAMIC DATA PRIMITIVE MEMBER GETTERS
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  --  OUT
  -- OUT
  --  OUT
  -- OUT
  --  OUT
  -- OUT
  --  OUT
  -- DYNAMIC DATA PRIMITIVE MEMBER SETTERS
  -- DYNAMIC DATA SEQUENCES
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- OUT
  -- DYNAMIC DATA NESTED
  -- OUT (copies)
  -- OUT
  -- OUT
   function rosidl_dynamic_typesupport_get_zero_initialized_serialization_support_interface return rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h.rosidl_dynamic_typesupport_serialization_support_interface_t  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/api/serialization_support_interface.h:1397
   with Import => True, 
        Convention => C, 
        External_Name => "rosidl_dynamic_typesupport_get_zero_initialized_serialization_support_interface";

end rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_api_serialization_support_interface_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
