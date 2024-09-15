pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with action_msgs_action_msgs_msg_detail_goal_info_ustruct_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with stddef_h;

package action_msgs_action_msgs_msg_detail_goal_status_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:msg/GoalStatus.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "action_msgs/msg/goal_status.h"
  -- Constants defined in the message
  --/ Constant 'STATUS_UNKNOWN'.
  --*
  --  * Indicates status has not been properly set.
  --  

  --/ Constant 'STATUS_ACCEPTED'.
  --*
  --  * The goal has been accepted and is awaiting execution.
  --  

  --/ Constant 'STATUS_EXECUTING'.
  --*
  --  * The goal is currently being executed by the action server.
  --  

  --/ Constant 'STATUS_CANCELING'.
  --*
  --  * The client has requested that the goal be canceled and the action server has
  --  * accepted the cancel request.
  --  

  --/ Constant 'STATUS_SUCCEEDED'.
  --*
  --  * The goal was achieved successfully by the action server.
  --  

  --/ Constant 'STATUS_CANCELED'.
  --*
  --  * The goal was canceled after an external request from an action client.
  --  

  --/ Constant 'STATUS_ABORTED'.
  --*
  --  * The goal was terminated by the action server without an external request.
  --  

  -- Include directives for member types
  -- Member 'goal_info'
  --/ Struct defined in msg/GoalStatus in the package action_msgs.
  --*
  --  * An action goal can be in one of these states after it is accepted by an action
  --  * server.
  --  *
  --  * For more information, see http://design.ros2.org/articles/actions.html
  --  

  --/ Goal info (contains ID and timestamp).
   type action_msgs_u_msg_u_GoalStatus is record
      goal_info : aliased action_msgs_action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:100
      status : aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:102
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:97

  --/ Action goal state-machine status.
  -- Struct for a sequence of action_msgs__msg__GoalStatus.
   type action_msgs_u_msg_u_GoalStatus_u_Sequence is record
      data : access action_msgs_u_msg_u_GoalStatus;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:108
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:110
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:112
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/msg/detail/goal_status__struct.h:106

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_action_msgs_msg_detail_goal_status_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
