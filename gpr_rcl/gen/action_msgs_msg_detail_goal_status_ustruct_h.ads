pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with action_msgs_msg_detail_goal_info_ustruct_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with stddef_h;

package action_msgs_msg_detail_goal_status_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:msg/GoalStatus.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  --/ Constant 'STATUS_UNKNOWN'.
  --/ Constant 'STATUS_ACCEPTED'.
  --/ Constant 'STATUS_EXECUTING'.
  --/ Constant 'STATUS_CANCELING'.
  --/ Constant 'STATUS_SUCCEEDED'.
  --/ Constant 'STATUS_CANCELED'.
  --/ Constant 'STATUS_ABORTED'.
  -- Include directives for member types
  -- Member 'goal_info'
  -- Struct defined in msg/GoalStatus in the package action_msgs.
   type action_msgs_u_msg_u_GoalStatus is record
      goal_info : aliased action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:69
      status : aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:70
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:67

  -- Struct for a sequence of action_msgs__msg__GoalStatus.
   type action_msgs_u_msg_u_GoalStatus_u_Sequence is record
      data : access action_msgs_u_msg_u_GoalStatus;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:76
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:78
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:80
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/msg/detail/goal_status__struct.h:74

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_msg_detail_goal_status_ustruct_h;
