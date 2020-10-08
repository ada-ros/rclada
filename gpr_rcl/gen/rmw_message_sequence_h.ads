pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with stddef_h;
limited with rcutils_allocator_h;
limited with rmw_types_h;
with rmw_ret_types_h;

package rmw_message_sequence_h is

  -- Copyright 2020 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
  --/ Structure to hold a sequence of ROS messages.
  --/ Array of pointers to ROS messages.
   type rmw_message_sequence_t is record
      data : System.Address;  -- /opt/ros/foxy/include/rmw/message_sequence.h:33
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:35
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:37
      allocator : access rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/message_sequence.h:30

  --/ The number of valid entries in `data`.
  --/ The total allocated capacity of the data array.
  --/ The allocator used to allocate the data array.
  --/ Structure to hold a sequence of message infos.
  --/ Array of message info.
   type rmw_message_info_sequence_t is record
      data : access rmw_types_h.rmw_message_info_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:46
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:48
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:50
      allocator : access rcutils_allocator_h.rcutils_allocator_t;  -- /opt/ros/foxy/include/rmw/message_sequence.h:52
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/rmw/message_sequence.h:43

  --/ The number of valid entries in data.
  --/ The total allocated capacity of the data array.
  --/ The allocator used to allocate the data array.
  --/ Return an rmw_message_sequence_t struct with members initialized to `NULL`
   function rmw_get_zero_initialized_message_sequence return rmw_message_sequence_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_message_sequence";

  --/ Initialize an rmw_message_sequence_t object.
  --*
  -- * \param[inout] sequence sequence object to be initialized.
  -- * \param[in] size capacity of the sequence to be allocated.
  -- * \param[in] allocator the allcator used to allocate memory.
  --  

   function rmw_message_sequence_init
     (sequence : access rmw_message_sequence_t;
      size : stddef_h.size_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:68
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_message_sequence_init";

  --/ Finalize an rmw_message_sequence_t object.
  --*
  -- * The rmw_message_sequence_t struct has members which require memory to be allocated to them
  -- * before setting values.
  -- * This function reclaims any allocated resources within the object and zeroes out all other
  -- * members.
  -- *
  -- * Note: This will not call `fini` or deallocate the underlying message structures.
  -- *
  -- * \param[inout] sequence sequence object to be finalized.
  --  

   function rmw_message_sequence_fini (sequence : access rmw_message_sequence_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:86
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_message_sequence_fini";

  --/ Return an rmw_message_info_sequence_t struct with members initialized to `NULL`
   function rmw_get_zero_initialized_message_info_sequence return rmw_message_info_sequence_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:91
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_get_zero_initialized_message_info_sequence";

  --/ Initialize an rmw_message_info_sequence_t object.
  --*
  -- * \param[inout] sequence sequence object to be initialized.
  -- * \param[in] size capacity of the sequence to be allocated.
  -- * \param[in] allocator the allcator used to allocate memory.
  --  

   function rmw_message_info_sequence_init
     (sequence : access rmw_message_info_sequence_t;
      size : stddef_h.size_t;
      allocator : access rcutils_allocator_h.rcutils_allocator_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:101
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_message_info_sequence_init";

  --/ Finalize an rmw_message_sequence_t object.
  --*
  -- * The rmw_message_sequence_t struct has members which require memory to be allocated to them
  -- * before setting values.
  -- * This function reclaims any allocated resources within the object and zeroes out all other
  -- * members.
  -- *
  -- * \param[inout] sequence sequence object to be finalized.
  --  

   function rmw_message_info_sequence_fini (sequence : access rmw_message_info_sequence_t) return rmw_ret_types_h.rmw_ret_t  -- /opt/ros/foxy/include/rmw/message_sequence.h:117
   with Import => True, 
        Convention => C, 
        External_Name => "rmw_message_info_sequence_fini";

end rmw_message_sequence_h;
