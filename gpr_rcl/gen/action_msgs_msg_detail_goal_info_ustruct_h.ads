pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with unique_identifier_msgs_msg_detail_uuid_ustruct_h;
with builtin_interfaces_msg_detail_time_ustruct_h;
with stddef_h;

package action_msgs_msg_detail_goal_info_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:msg/GoalInfo.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'goal_id'
  -- Member 'stamp'
  -- Struct defined in msg/GoalInfo in the package action_msgs.
   type action_msgs_u_msg_u_GoalInfo is record
      goal_id : aliased unique_identifier_msgs_msg_detail_uuid_ustruct_h.unique_identifier_msgs_u_msg_u_UUID;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:29
      stamp : aliased builtin_interfaces_msg_detail_time_ustruct_h.builtin_interfaces_u_msg_u_Time;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:30
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:27

  -- Struct for a sequence of action_msgs__msg__GoalInfo.
   type action_msgs_u_msg_u_GoalInfo_u_Sequence is record
      data : access action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:36
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:38
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:40
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_info__struct.h:34

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_msg_detail_goal_info_ustruct_h;
