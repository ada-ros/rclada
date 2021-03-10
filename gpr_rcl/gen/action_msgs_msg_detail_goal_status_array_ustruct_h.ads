pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with action_msgs_msg_detail_goal_status_ustruct_h;
with stddef_h;

package action_msgs_msg_detail_goal_status_array_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:msg/GoalStatusArray.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'status_list'
  -- Struct defined in msg/GoalStatusArray in the package action_msgs.
   type action_msgs_u_msg_u_GoalStatusArray is record
      status_list : aliased action_msgs_msg_detail_goal_status_ustruct_h.action_msgs_u_msg_u_GoalStatus_u_Sequence;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:27
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:25

  -- Struct for a sequence of action_msgs__msg__GoalStatusArray.
   type action_msgs_u_msg_u_GoalStatusArray_u_Sequence is record
      data : access action_msgs_u_msg_u_GoalStatusArray;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:33
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:35
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:37
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status_array__struct.h:31

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_msg_detail_goal_status_array_ustruct_h;
