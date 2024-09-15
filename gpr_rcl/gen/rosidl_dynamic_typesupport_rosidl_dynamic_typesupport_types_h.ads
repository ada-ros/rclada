pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with stddef_h;

package rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h is

   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_NOT_SET rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_NOT_SET
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_NESTED_TYPE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_NESTED_TYPE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT8 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT8
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT8 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT8
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT16 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT16
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT16 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT16
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT32 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT32
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT32 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT32
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT64 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT64
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT64 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT64
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT32 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_DOUBLE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT64 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_LONG_DOUBLE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT128 rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_CHAR rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_CHAR
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WCHAR rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WCHAR
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOOLEAN rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOOLEAN
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BYTE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BYTE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_STRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_STRING
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WSTRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WSTRING
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_STRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_STRING
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_WSTRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_WSTRING
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_STRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_STRING
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_WSTRING rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_WSTRING
   ROSIDL_DYNAMIC_TYPESUPPORT_SEQUENCE_TYPE_DELIMITER : constant := 48;  --  /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:152
   ROSIDL_DYNAMIC_TYPESUPPORT_ARRAY_OFFSET : constant := 48;  --  /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:153
   ROSIDL_DYNAMIC_TYPESUPPORT_UNBOUNDED_SEQUENCE_OFFSET : constant := 96;  --  /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:154
   ROSIDL_DYNAMIC_TYPESUPPORT_BOUNDED_SEQUENCE_OFFSET : constant := 144;  --  /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:155
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_NESTED_TYPE_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_NESTED_TYPE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT8_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT8_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT8_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT8_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT16_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT16_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT16_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT16_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT32_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT32_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT32_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT32_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT64_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT64_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT64_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT64_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT32_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_DOUBLE_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT64_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_LONG_DOUBLE_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT128_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_CHAR_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_CHAR_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WCHAR_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WCHAR_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOOLEAN_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOOLEAN_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BYTE_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BYTE_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_STRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_STRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WSTRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WSTRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_STRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_STRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_WSTRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_WSTRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_STRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_STRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_WSTRING_ARRAY rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_WSTRING_ARRAY
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_NESTED_TYPE_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_NESTED_TYPE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT8_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT8_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT8_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT8_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT16_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT16_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT16_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT16_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT32_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT32_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT32_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT32_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT64_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT64_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT64_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT64_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT32_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_DOUBLE_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT64_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_LONG_DOUBLE_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT128_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_CHAR_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_CHAR_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WCHAR_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WCHAR_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOOLEAN_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOOLEAN_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BYTE_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BYTE_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_STRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_STRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WSTRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WSTRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_STRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_STRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_WSTRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_WSTRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_STRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_STRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_WSTRING_BOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_WSTRING_BOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_NESTED_TYPE_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_NESTED_TYPE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT8_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT8_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT8_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT8_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT16_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT16_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT16_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT16_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT32_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT32_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT32_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT32_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_INT64_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_INT64_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_UINT64_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_UINT64_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT32_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FLOAT_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_DOUBLE_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT64_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_DOUBLE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_LONG_DOUBLE_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FLOAT128_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_LONG_DOUBLE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_CHAR_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_CHAR_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WCHAR_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WCHAR_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOOLEAN_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOOLEAN_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BYTE_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BYTE_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_STRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_STRING_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_WSTRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_WSTRING_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_STRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_STRING_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_FIXED_WSTRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_FIXED_WSTRING_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_STRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_STRING_UNBOUNDED_SEQUENCE
   --  unsupported macro: ROSIDL_DYNAMIC_TYPESUPPORT_FIELD_TYPE_BOUNDED_WSTRING_UNBOUNDED_SEQUENCE rosidl_runtime_c__type_description__FieldType__FIELD_TYPE_BOUNDED_WSTRING_UNBOUNDED_SEQUENCE

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
  -- =================================================================================================
  -- DYNAMIC TYPESUPPORT
  -- =================================================================================================
  -- ID for accessing specific members of dynamic type or dynamic data
   subtype rosidl_dynamic_typesupport_member_id_t is stddef_h.size_t;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:40

  -- Forward Declarations ============================================================================
   type rosidl_dynamic_typesupport_serialization_support_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_serialization_support_t is rosidl_dynamic_typesupport_serialization_support_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:45

   type rosidl_dynamic_typesupport_serialization_support_impl_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_serialization_support_impl_t is rosidl_dynamic_typesupport_serialization_support_impl_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:48

   type rosidl_dynamic_typesupport_serialization_support_interface_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_serialization_support_interface_t is rosidl_dynamic_typesupport_serialization_support_interface_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:51

   type rosidl_dynamic_typesupport_dynamic_type_builder_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_type_builder_t is rosidl_dynamic_typesupport_dynamic_type_builder_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:55

   type rosidl_dynamic_typesupport_dynamic_type_builder_impl_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_type_builder_impl_t is rosidl_dynamic_typesupport_dynamic_type_builder_impl_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:58

   type rosidl_dynamic_typesupport_dynamic_type_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_type_t is rosidl_dynamic_typesupport_dynamic_type_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:62

   type rosidl_dynamic_typesupport_dynamic_type_impl_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_type_impl_t is rosidl_dynamic_typesupport_dynamic_type_impl_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:65

   type rosidl_dynamic_typesupport_dynamic_data_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_data_t is rosidl_dynamic_typesupport_dynamic_data_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:69

   type rosidl_dynamic_typesupport_dynamic_data_impl_s is null record;   -- incomplete struct

   subtype rosidl_dynamic_typesupport_dynamic_data_impl_t is rosidl_dynamic_typesupport_dynamic_data_impl_s;  -- /opt/ros/jazzy/include/rosidl_dynamic_typesupport/rosidl_dynamic_typesupport/types.h:72

  -- =================================================================================================
  -- FIELD TYPE INDICES
  -- =================================================================================================
  -- This mapping must match the constants defined in type_description_interfaces/msgs/FieldType.msg
  -- Layout of constants across the 0-255 decimal values in the uint8:
  -- - 000    : Reserved for "not set"
  -- - 001-048: Primitive types, strings, and reserved space for future primitive types
  -- - 049-096: Fixed sized array of primitive and string types
  -- - 097-144: Bounded Sequences of primitive and string types
  -- - 145-192: Unbounded Sequences of primitive and string types
  -- - 193-255: Reserved space for future array/sequence-like types
  -- SPECIALS ========================================================================================
  -- Nested type defined in other .msg/.idl files.
  -- PRIMITIVES ======================================================================================
  -- NOTE(methylDragon): Unsure how to implement these. Are each pair equivalent?
  -- SEQUENCES =======================================================================================
end rosidl_dynamic_typesupport_rosidl_dynamic_typesupport_types_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
