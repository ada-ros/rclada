pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with unique_identifier_msgs_unique_identifier_msgs_msg_detail_uuid_ustruct_h;
with builtin_interfaces_builtin_interfaces_msg_detail_time_ustruct_h;
with stddef_h;

package action_msgs_action_msgs_msg_detail_goal_info_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:msg/GoalInfo.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "action_msgs/msg/goal_info.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'goal_id'
  -- Member 'stamp'
  --/ Struct defined in msg/GoalInfo in the package action_msgs.
  --*
  --  * Goal ID
  --  

   type action_msgs_u_msg_u_GoalInfo is record
      goal_id : aliased unique_identifier_msgs_unique_identifier_msgs_msg_detail_uuid_ustruct_h.unique_identifier_msgs_u_msg_u_UUID;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:34
      stamp : aliased builtin_interfaces_builtin_interfaces_msg_detail_time_ustruct_h.builtin_interfaces_u_msg_u_Time;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:36
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:32

  --/ Time when the goal was accepted
  -- Struct for a sequence of action_msgs__msg__GoalInfo.
   type action_msgs_u_msg_u_GoalInfo_u_Sequence is record
      data : access action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:42
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:44
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:46
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_info__struct.h:40

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_action_msgs_msg_detail_goal_info_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
