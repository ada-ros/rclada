pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with action_msgs_msg_detail_goal_info_ustruct_h;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_intn_h;

package action_msgs_srv_detail_cancel_goal_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:srv/CancelGoal.idl
  -- generated code does not contain a copyright notice
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'goal_info'
  -- Struct defined in srv/CancelGoal in the package action_msgs.
   type action_msgs_u_srv_u_CancelGoal_Request is record
      goal_info : aliased action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:27
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:25

  -- Struct for a sequence of action_msgs__srv__CancelGoal_Request.
   type action_msgs_u_srv_u_CancelGoal_Request_u_Sequence is record
      data : access action_msgs_u_srv_u_CancelGoal_Request;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:33
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:35
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:37
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:31

  --/ The number of valid items in data
  --/ The number of allocated items in data
  -- Constants defined in the message
  --/ Constant 'ERROR_NONE'.
  --/ Constant 'ERROR_REJECTED'.
  --/ Constant 'ERROR_UNKNOWN_GOAL_ID'.
  --/ Constant 'ERROR_GOAL_TERMINATED'.
  -- Include directives for member types
  -- Member 'goals_canceling'
  -- already included above
  -- #include "action_msgs/msg/detail/goal_info__struct.h"
  -- Struct defined in srv/CancelGoal in the package action_msgs.
   type action_msgs_u_srv_u_CancelGoal_Response is record
      return_code : aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:75
      goals_canceling : aliased action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo_u_Sequence;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:76
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:73

  -- Struct for a sequence of action_msgs__srv__CancelGoal_Response.
   type action_msgs_u_srv_u_CancelGoal_Response_u_Sequence is record
      data : access action_msgs_u_srv_u_CancelGoal_Response;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:82
      size : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:84
      capacity : aliased stddef_h.size_t;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:86
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/foxy/include/action_msgs/srv/detail/cancel_goal__struct.h:80

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_srv_detail_cancel_goal_ustruct_h;
