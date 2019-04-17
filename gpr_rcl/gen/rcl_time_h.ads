pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rcutils_time_h;
with Interfaces.C.Extensions;
with System;
with stddef_h;
with rcl_types_h;
with rcl_allocator_h;

package rcl_time_h is

   --  unsupported macro: RCL_S_TO_NS RCUTILS_S_TO_NS
   --  unsupported macro: RCL_MS_TO_NS RCUTILS_MS_TO_NS
   --  unsupported macro: RCL_US_TO_NS RCUTILS_US_TO_NS
   --  unsupported macro: RCL_NS_TO_S RCUTILS_NS_TO_S
   --  unsupported macro: RCL_NS_TO_MS RCUTILS_NS_TO_MS
   --  unsupported macro: RCL_NS_TO_US RCUTILS_NS_TO_US
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
  --/ Convenience macro to convert seconds to nanoseconds.
  --/ Convenience macro to convert milliseconds to nanoseconds.
  --/ Convenience macro to convert microseconds to nanoseconds.
  --/ Convenience macro to convert nanoseconds to seconds.
  --/ Convenience macro to convert nanoseconds to milliseconds.
  --/ Convenience macro to convert nanoseconds to microseconds.
  --/ A single point in time, measured in nanoseconds since the Unix epoch.
   subtype rcl_time_point_value_t is rcutils_time_h.rcutils_time_point_value_t;  -- /opt/ros/crystal/include/rcl/time.h:44

  --/ A duration of time, measured in nanoseconds.
   subtype rcl_duration_value_t is rcutils_time_h.rcutils_duration_value_t;  -- /opt/ros/crystal/include/rcl/time.h:46

  --/ Time source type, used to indicate the source of a time measurement.
   type rcl_clock_type_t is 
     (RCL_CLOCK_UNINITIALIZED,
      RCL_ROS_TIME,
      RCL_SYSTEM_TIME,
      RCL_STEADY_TIME);
   pragma Convention (C, rcl_clock_type_t);  -- /opt/ros/crystal/include/rcl/time.h:49

  --/ A duration of time, measured in nanoseconds and its source.
   type rcl_duration_t is record
      nanoseconds : aliased rcl_duration_value_t;  -- /opt/ros/crystal/include/rcl/time.h:60
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_duration_t);  -- /opt/ros/crystal/include/rcl/time.h:58

  --/ Enumeration to describe the type of time jump.
   subtype rcl_clock_change_t is unsigned;
   RCL_ROS_TIME_NO_CHANGE : constant rcl_clock_change_t := 1;
   RCL_ROS_TIME_ACTIVATED : constant rcl_clock_change_t := 2;
   RCL_ROS_TIME_DEACTIVATED : constant rcl_clock_change_t := 3;
   RCL_SYSTEM_TIME_NO_CHANGE : constant rcl_clock_change_t := 4;  -- /opt/ros/crystal/include/rcl/time.h:64

  --/ The source before and after the jump is ROS_TIME.
  --/ The source switched to ROS_TIME from SYSTEM_TIME.
  --/ The source switched to SYSTEM_TIME from ROS_TIME.
  --/ The source before and after the jump is SYSTEM_TIME.
  --/ Struct to describe a jump in time.
  --/ Indicate whether or not the source of time changed.
   type rcl_time_jump_t is record
      clock_change : aliased rcl_clock_change_t;  -- /opt/ros/crystal/include/rcl/time.h:80
      c_delta : aliased rcl_duration_t;  -- /opt/ros/crystal/include/rcl/time.h:82
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_time_jump_t);  -- /opt/ros/crystal/include/rcl/time.h:77

  --/ The new time minus the last time before the jump.
  --/ Signature of a time jump callback.
  --/ \param[in] time_jump A description of the jump in time.
  --/ \param[in] before_jump Every jump callback is called twice: once before the clock changes and
  --/ once after. This is true the first call and false the second.
  --/ \param[in] user_data A pointer given at callback registration which is passed to the callback.
   type rcl_jump_callback_t is access procedure
        (arg1 : access constant rcl_time_jump_t;
         arg2 : Extensions.bool;
         arg3 : System.Address);
   pragma Convention (C, rcl_jump_callback_t);  -- /opt/ros/crystal/include/rcl/time.h:90

  --/ Describe the prerequisites for calling a time jump callback.
  --/ True to call callback when the clock type changes.
   type rcl_jump_threshold_t is record
      on_clock_change : aliased Extensions.bool;  -- /opt/ros/crystal/include/rcl/time.h:99
      min_forward : aliased rcl_duration_t;  -- /opt/ros/crystal/include/rcl/time.h:102
      min_backward : aliased rcl_duration_t;  -- /opt/ros/crystal/include/rcl/time.h:105
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_jump_threshold_t);  -- /opt/ros/crystal/include/rcl/time.h:96

  --/ A positive duration indicating the minimum jump forwards to be considered exceeded, or zero
  --/ to disable.
  --/ A negative duration indicating the minimum jump backwards to be considered exceeded, or zero
  --/ to disable.
  --/ Struct to describe an added callback.
   type rcl_jump_callback_info_t is record
      callback : rcl_jump_callback_t;  -- /opt/ros/crystal/include/rcl/time.h:111
      threshold : aliased rcl_jump_threshold_t;  -- /opt/ros/crystal/include/rcl/time.h:112
      user_data : System.Address;  -- /opt/ros/crystal/include/rcl/time.h:113
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_jump_callback_info_t);  -- /opt/ros/crystal/include/rcl/time.h:109

  --/ Encapsulation of a time source.
   type rcl_clock_t is record
      c_type : aliased rcl_clock_type_t;  -- /opt/ros/crystal/include/rcl/time.h:119
      jump_callbacks : access rcl_jump_callback_info_t;  -- /opt/ros/crystal/include/rcl/time.h:121
      num_jump_callbacks : aliased stddef_h.size_t;  -- /opt/ros/crystal/include/rcl/time.h:123
      get_now : access function (arg1 : System.Address; arg2 : access rcl_time_point_value_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:124
      data : System.Address;  -- /opt/ros/crystal/include/rcl/time.h:126
      allocator : aliased rcl_allocator_h.rcl_allocator_t;  -- /opt/ros/crystal/include/rcl/time.h:127
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_clock_t);  -- /opt/ros/crystal/include/rcl/time.h:117

  --/ An array of added jump callbacks.
  --/ Number of callbacks in jump_callbacks.
  -- void (*set_now) (rcl_time_point_value_t);
  --/ A single point in time, measured in nanoseconds, the reference point is based on the source.
   type rcl_time_point_t is record
      nanoseconds : aliased rcl_time_point_value_t;  -- /opt/ros/crystal/include/rcl/time.h:133
      clock_type : aliased rcl_clock_type_t;  -- /opt/ros/crystal/include/rcl/time.h:134
   end record;
   pragma Convention (C_Pass_By_Copy, rcl_time_point_t);  -- /opt/ros/crystal/include/rcl/time.h:131

  -- typedef struct rcl_rate_t
  -- {
  --   rcl_time_point_value_t trigger_time;
  --   int64_t period;
  --   rcl_clock_type_t clock;;
  -- } rcl_rate_t;
  -- TODO(tfoote) integrate rate and timer implementations
  --/ Check if the clock has valid values.
  --*
  -- * This function returns true if the time source appears to be valid.
  -- * It will check that the type is not uninitialized, and that pointers
  -- * are not invalid.
  -- * Note that if data is uninitialized it may give a false positive.
  -- *
  -- * \param[in] clock the handle to the clock which is being queried
  -- * \return true if the source is believed to be valid, otherwise return false.
  --  

   function rcl_clock_valid (clock : access rcl_clock_t) return Extensions.bool;  -- /opt/ros/crystal/include/rcl/time.h:158
   pragma Import (C, rcl_clock_valid, "rcl_clock_valid");

  --/ Initialize a clock based on the passed type.
  --*
  -- * This will allocate all necessary internal structures, and initialize variables.
  -- *
  -- * \param[in] clock_type the type identifying the time source to provide
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \param[in] allocator The allocator to use for allocations
  -- * \return `RCL_RET_OK` if the time source was successfully initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_clock_init
     (clock_type : rcl_clock_type_t;
      clock : access rcl_clock_t;
      allocator : access rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:174
   pragma Import (C, rcl_clock_init, "rcl_clock_init");

  --/ Finalize a clock.
  --*
  -- * This will deallocate all necessary internal structures, and clean up any variables.
  -- * It can be combined with any of the init functions.
  -- *
  -- * Passing a clock with type RCL_CLOCK_UNINITIALIZED will result in
  -- * RCL_RET_INVALID_ARGUMENT being returned.
  -- *
  -- * \param[in] clock the handle to the clock which is being finalized
  -- * \return `RCL_RET_OK` if the time source was successfully finalized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_clock_fini (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:194
   pragma Import (C, rcl_clock_fini, "rcl_clock_fini");

  --/ Initialize a clock as a RCL_ROS_TIME time source.
  --*
  -- * This will allocate all necessary internal structures, and initialize variables.
  -- * It is specifically setting up a RCL_ROS_TIME time source.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \param[in] allocator The allocator to use for allocations
  -- * \return `RCL_RET_OK` if the time source was successfully initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_ros_clock_init (clock : access rcl_clock_t; allocator : access rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:211
   pragma Import (C, rcl_ros_clock_init, "rcl_ros_clock_init");

  --/ Finalize a clock as a `RCL_ROS_TIME` time source.
  --*
  -- * This will deallocate all necessary internal structures, and clean up any variables.
  -- * It is specifically setting up a `RCL_ROS_TIME` time source. It is expected
  -- * to be paired with the init fuction.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \return `RCL_RET_OK` if the time source was successfully finalized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_ros_clock_fini (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:229
   pragma Import (C, rcl_ros_clock_fini, "rcl_ros_clock_fini");

  --/ Initialize a clock as a `RCL_STEADY_TIME` time source.
  --*
  -- * This will allocate all necessary internal structures, and initialize variables.
  -- * It is specifically setting up a `RCL_STEADY_TIME` time source.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \param[in] allocator The allocator to use for allocations
  -- * \return `RCL_RET_OK` if the time source was successfully initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_steady_clock_init (clock : access rcl_clock_t; allocator : access rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:246
   pragma Import (C, rcl_steady_clock_init, "rcl_steady_clock_init");

  --/ Finalize a clock as a `RCL_STEADY_TIME` time source.
  --*
  -- * Finalize the clock as a `RCL_STEADY_TIME` time source.
  -- *
  -- * This will deallocate all necessary internal structures, and clean up any variables.
  -- * It is specifically setting up a steady time source. It is expected to be
  -- * paired with the init fuction.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \return `RCL_RET_OK` if the time source was successfully finalized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_steady_clock_fini (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:266
   pragma Import (C, rcl_steady_clock_fini, "rcl_steady_clock_fini");

  --/ Initialize a clock as a `RCL_SYSTEM_TIME` time source.
  --*
  -- * Initialize the clock as a `RCL_SYSTEM_TIME` time source.
  -- *
  -- * This will allocate all necessary internal structures, and initialize variables.
  -- * It is specifically setting up a system time source.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized
  -- * \param[in] allocator The allocator to use for allocations
  -- * \return `RCL_RET_OK` if the time source was successfully initialized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_system_clock_init (clock : access rcl_clock_t; allocator : access rcl_allocator_h.rcl_allocator_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:285
   pragma Import (C, rcl_system_clock_init, "rcl_system_clock_init");

  --/ Finalize a clock as a `RCL_SYSTEM_TIME` time source.
  --*
  -- * Finalize the clock as a `RCL_SYSTEM_TIME` time source.
  -- *
  -- * This will deallocate all necessary internal structures, and clean up any variables.
  -- * It is specifically setting up a system time source. It is expected to be paired with
  -- * the init fuction.
  -- *
  -- * \param[in] clock the handle to the clock which is being initialized.
  -- * \return `RCL_RET_OK` if the time source was successfully finalized, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_system_clock_fini (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:305
   pragma Import (C, rcl_system_clock_fini, "rcl_system_clock_fini");

  --/ Compute the difference between two time points
  --*
  -- * This function takes two time points and computes the duration between them.
  -- * The two time points must be using the same time abstraction, and the
  -- * resultant duration will also be of the same abstraction.
  -- *
  -- * The value will be computed as duration = finish - start. If start is after
  -- * finish the duration will be negative.
  -- *
  -- * \param[in] start The time point for the start of the duration.
  -- * \param[in] finish The time point for the end of the duration.
  -- * \param[out] delta The duration between the start and finish.
  -- * \return `RCL_RET_OK` if the difference was computed successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_difference_times
     (start : access rcl_time_point_t;
      finish : access rcl_time_point_t;
      c_delta : access rcl_duration_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:327
   pragma Import (C, rcl_difference_times, "rcl_difference_times");

  --/ Fill the time point value with the current value of the associated clock.
  --*
  -- * This function will populate the data of the time_point_value object with the
  -- * current value from it's associated time abstraction.
  -- * \param[in] clock The time source from which to set the value.
  -- * \param[out] time_point_value The time_point value to populate.
  -- * \return `RCL_RET_OK` if the last call time was retrieved successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_clock_get_now (clock : access rcl_clock_t; time_point_value : access rcl_time_point_value_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:343
   pragma Import (C, rcl_clock_get_now, "rcl_clock_get_now");

  --/ Enable the ROS time abstraction override.
  --*
  -- * This method will enable the ROS time abstraction override values,
  -- * such that the time source will report the set value instead of falling
  -- * back to system time.
  -- *
  -- * \param[in] clock The clock to enable.
  -- * \return `RCL_RET_OK` if the time source was enabled successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_enable_ros_time_override (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:360
   pragma Import (C, rcl_enable_ros_time_override, "rcl_enable_ros_time_override");

  --/ Disable the ROS time abstraction override.
  --*
  -- * This method will disable the `RCL_ROS_TIME` time abstraction override values,
  -- * such that the time source will report the system time even if a custom
  -- * value has been set.
  -- *
  -- * \param[in] clock The clock to disable.
  -- * \return `RCL_RET_OK` if the time source was disabled successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_disable_ros_time_override (clock : access rcl_clock_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:376
   pragma Import (C, rcl_disable_ros_time_override, "rcl_disable_ros_time_override");

  --/ Check if the `RCL_ROS_TIME` time source has the override enabled.
  --*
  -- * This will populate the is_enabled object to indicate if the
  -- * time overide is enabled. If it is enabled, the set value will be returned.
  -- * Otherwise this time source will return the equivalent to system time abstraction.
  -- *
  -- * \param[in] clock The clock to query.
  -- * \param[out] is_enabled Whether the override is enabled..
  -- * \return `RCL_RET_OK` if the time source was queried successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_is_enabled_ros_time_override (clock : access rcl_clock_t; is_enabled : access Extensions.bool) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:394
   pragma Import (C, rcl_is_enabled_ros_time_override, "rcl_is_enabled_ros_time_override");

  --/ Set the current time for this `RCL_ROS_TIME` time source.
  --*
  -- * This function will update the internal storage for the `RCL_ROS_TIME`
  -- * time source.
  -- * If queried and override enabled the time source will return this value,
  -- * otherwise it will return the system time.
  -- *
  -- * \param[in] clock The clock to update.
  -- * \param[in] time_value The new current time.
  -- * \return `RCL_RET_OK` if the time source was set successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occur.
  --  

   function rcl_set_ros_time_override (clock : access rcl_clock_t; time_value : rcl_time_point_value_t) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:413
   pragma Import (C, rcl_set_ros_time_override, "rcl_set_ros_time_override");

  --/ Add a callback to be called when a time jump exceeds a threshold.
  --*
  -- * The callback is called twice when the threshold is exceeded: once before the clock is
  -- * updated, and once after.
  -- * The user_data pointer is passed to the callback as the last argument.
  -- * A callback and user_data pair must be unique among the callbacks added to a clock.
  -- *
  -- * \param[in] clock A clock to add a jump callback to.
  -- * \param[in] threshold Criteria indicating when to call the callback.
  -- * \param[in] callback A callback to call.
  -- * \param[in] user_data A pointer to be passed to the callback.
  -- * \return `RCL_RET_OK` if the callback was added successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` an unspecified error occurs.
  --  

   function rcl_clock_add_jump_callback
     (clock : access rcl_clock_t;
      threshold : rcl_jump_threshold_t;
      callback : rcl_jump_callback_t;
      user_data : System.Address) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:434
   pragma Import (C, rcl_clock_add_jump_callback, "rcl_clock_add_jump_callback");

  --/ Remove a previously added time jump callback.
  --*
  -- * \param[in] clock The clock to remove a jump callback from.
  -- * \param[in] threshold Criteria indicating when to call callback.
  -- * \param[in] callback The callback to call.
  -- * \param[in] user_data A pointer to be passed to the callback.
  -- * \return `RCL_RET_OK` if the callback was added successfully, or
  -- * \return `RCL_RET_INVALID_ARGUMENT` if any arguments are invalid, or
  -- * \return `RCL_RET_ERROR` the callback was not found or an unspecified error occurs.
  --  

   function rcl_clock_remove_jump_callback
     (clock : access rcl_clock_t;
      callback : rcl_jump_callback_t;
      user_data : System.Address) return rcl_types_h.rcl_ret_t;  -- /opt/ros/crystal/include/rcl/time.h:451
   pragma Import (C, rcl_clock_remove_jump_callback, "rcl_clock_remove_jump_callback");

end rcl_time_h;
