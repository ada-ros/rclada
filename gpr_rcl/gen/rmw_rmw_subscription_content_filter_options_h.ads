pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with rcutils_rcutils_types_string_array_h;
with stddef_h;
with System;
limited with rcutils_rcutils_allocator_h;
with rmw_rmw_ret_types_h;

package rmw_rmw_subscription_content_filter_options_h is

  -- Copyright 2021 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --*
  --   * Specify the criteria to select the data samples of interest.
  --   *
  --   * It is similar to the WHERE part of an SQL clause.
  --    

   type rmw_subscription_content_filter_options_s is record
      filter_expression : Interfaces.C.Strings.chars_ptr;  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:37
      expression_parameters : aliased rcutils_rcutils_types_string_array_h.rcutils_string_array_t;  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:45
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:30

  --*
  --   * Give values to the tokens placeholder ‘parameters’ (i.e., "%n" tokens begin from 0) in the
  --   * filter_expression. The number of supplied parameters must fit with the requested values.
  --   *
  --   * It can be NULL if there is no "%n" tokens placeholder in filter_expression.
  --   * The maximum index number must be smaller than 100.
  --    

   subtype rmw_subscription_content_filter_options_t is rmw_subscription_content_filter_options_s;  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:46

  --/ Get zero initialized content filter options.
   function rmw_get_zero_initialized_content_filter_options return rmw_subscription_content_filter_options_t  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:52
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_content_filter_options";

  --/ Initialize the given content filter options.
  --*
  -- * \param[in] filter_expression The filter expression.
  -- * \param[in] expression_parameters_argc The expression parameters argc.
  -- * \param[in] expression_parameter_argv The expression parameters argv.
  -- * \param[in] allocator The allocator used when copying data to the content filter options.
  -- * \param[out] options The content filter options to be set.
  -- * \returns RMW_RET_INVALID_ARGUMENT, or
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_subscription_content_filter_options_init
     (filter_expression : Interfaces.C.Strings.chars_ptr;
      expression_parameters_argc : stddef_h.size_t;
      expression_parameter_argv : System.Address;
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      options : access rmw_subscription_content_filter_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_subscription_content_filter_options_init";

  --/ Set the given content filter options.
  --*
  -- * \param[in] filter_expression The filter expression.
  -- * \param[in] expression_parameters_argc The expression parameters argc.
  -- * \param[in] expression_parameter_argv The expression parameters argv.
  -- * \param[in] allocator The allocator used when copying data to the content filter options.
  -- * \param[out] options The content filter options to be set.
  -- * \returns RMW_RET_INVALID_ARGUMENT, or
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_subscription_content_filter_options_set
     (filter_expression : Interfaces.C.Strings.chars_ptr;
      expression_parameters_argc : stddef_h.size_t;
      expression_parameter_argv : System.Address;
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      options : access rmw_subscription_content_filter_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_subscription_content_filter_options_set";

  --/ Copy the given content filter options.
  --*
  -- * \param[in] src content filter options to be copied.
  -- * \param[in] allocator allocator used when copying data to the new content filter options.
  -- * \param[out] dst content filter options to be set.
  -- * \returns RMW_RET_INVALID_ARGUMENT, or
  -- * \returns RMW_RET_BAD_ALLOC, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_subscription_content_filter_options_copy
     (src : access constant rmw_subscription_content_filter_options_t;
      allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s;
      dst : access rmw_subscription_content_filter_options_t) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_subscription_content_filter_options_copy";

  --/ Finalize the content filter options.
  --*
  -- * \param[in] options content filter options to be finalized.
  -- * \param[in] allocator allocator used to deallocate the content filter options.
  -- * \returns RMW_RET_INVALID_ARGUMENT, or
  -- * \returns RMW_RET_ERROR, or
  -- * \returns RMW_RET_OK
  --  

   function rmw_subscription_content_filter_options_fini (options : access rmw_subscription_content_filter_options_t; allocator : access constant rcutils_rcutils_allocator_h.rcutils_allocator_s) return rmw_rmw_ret_types_h.rmw_ret_t  -- /opt/ros/jazzy/include/rmw/rmw/subscription_content_filter_options.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_subscription_content_filter_options_fini";

end rmw_rmw_subscription_content_filter_options_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
