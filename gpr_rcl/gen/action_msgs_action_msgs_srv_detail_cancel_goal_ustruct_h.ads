pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C; use Interfaces.C;
with action_msgs_action_msgs_msg_detail_goal_info_ustruct_h;
with stddef_h;
with x86_64_linux_gnu_bits_stdint_intn_h;
with service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h;

package action_msgs_action_msgs_srv_detail_cancel_goal_ustruct_h is

  -- generated from rosidl_generator_c/resource/idl__struct.h.em
  -- with input from action_msgs:srv/CancelGoal.idl
  -- generated code does not contain a copyright notice
  -- IWYU pragma: private, include "action_msgs/srv/cancel_goal.h"
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'goal_info'
  --/ Struct defined in srv/CancelGoal in the package action_msgs.
  --/ Goal info describing the goals to cancel, see above.
   type action_msgs_u_srv_u_CancelGoal_Request is record
      goal_info : aliased action_msgs_action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:31
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:28

  -- Struct for a sequence of action_msgs__srv__CancelGoal_Request.
   type action_msgs_u_srv_u_CancelGoal_Request_u_Sequence is record
      data : access action_msgs_u_srv_u_CancelGoal_Request;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:37
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:39
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:41
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:35

  --/ The number of valid items in data
  --/ The number of allocated items in data
  -- Constants defined in the message
  --/ Constant 'ERROR_NONE'.
  --*
  --  * Indicates the request was accepted without any errors.
  --  *
  --  * One or more goals have transitioned to the CANCELING state. The
  --  * goals_canceling list is not empty.
  --  

  --/ Constant 'ERROR_REJECTED'.
  --*
  --  * Indicates the request was rejected.
  --  *
  --  * No goals have transitioned to the CANCELING state. The goals_canceling list is
  --  * empty.
  --  

  --/ Constant 'ERROR_UNKNOWN_GOAL_ID'.
  --*
  --  * Indicates the requested goal ID does not exist.
  --  *
  --  * No goals have transitioned to the CANCELING state. The goals_canceling list is
  --  * empty.
  --  

  --/ Constant 'ERROR_GOAL_TERMINATED'.
  --*
  --  * Indicates the goal is not cancelable because it is already in a terminal state.
  --  *
  --  * No goals have transitioned to the CANCELING state. The goals_canceling list is
  --  * empty.
  --  

  -- Include directives for member types
  -- Member 'goals_canceling'
  -- already included above
  -- #include "action_msgs/msg/detail/goal_info__struct.h"
  --/ Struct defined in srv/CancelGoal in the package action_msgs.
  --/ Return code, see above definitions.
   type action_msgs_u_srv_u_CancelGoal_Response is record
      return_code : aliased x86_64_linux_gnu_bits_stdint_intn_h.int8_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:103
      goals_canceling : aliased action_msgs_action_msgs_msg_detail_goal_info_ustruct_h.action_msgs_u_msg_u_GoalInfo_u_Sequence;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:105
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:100

  --/ Goals that accepted the cancel request.
  -- Struct for a sequence of action_msgs__srv__CancelGoal_Response.
   type action_msgs_u_srv_u_CancelGoal_Response_u_Sequence is record
      data : access action_msgs_u_srv_u_CancelGoal_Response;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:111
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:113
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:115
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:109

  --/ The number of valid items in data
  --/ The number of allocated items in data
  -- Constants defined in the message
  -- Include directives for member types
  -- Member 'info'
  -- constants for array fields with an upper bound
  -- request
  -- response
  --/ Struct defined in srv/CancelGoal in the package action_msgs.
   type action_msgs_u_srv_u_CancelGoal_Event is record
      info : aliased service_msgs_service_msgs_msg_detail_service_event_info_ustruct_h.service_msgs_u_msg_u_ServiceEventInfo;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:139
      request : aliased action_msgs_u_srv_u_CancelGoal_Request_u_Sequence;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:140
      response : aliased action_msgs_u_srv_u_CancelGoal_Response_u_Sequence;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:141
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:137

  -- Struct for a sequence of action_msgs__srv__CancelGoal_Event.
   type action_msgs_u_srv_u_CancelGoal_Event_u_Sequence is record
      data : access action_msgs_u_srv_u_CancelGoal_Event;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:147
      size : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:149
      capacity : aliased stddef_h.size_t;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:151
   end record
   with Convention => C_Pass_By_Copy;  -- /opt/ros/jazzy/include/action_msgs/action_msgs/srv/detail/cancel_goal__struct.h:145

  --/ The number of valid items in data
  --/ The number of allocated items in data
end action_msgs_action_msgs_srv_detail_cancel_goal_ustruct_h;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
