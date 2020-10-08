pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with rmw_ret_types_h;
with rmw_serialized_message_h;

package rcl_types_h is

   --  unsupported macro: RCL_RET_OK RMW_RET_OK
   --  unsupported macro: RCL_RET_ERROR RMW_RET_ERROR
   --  unsupported macro: RCL_RET_TIMEOUT RMW_RET_TIMEOUT
   --  unsupported macro: RCL_RET_BAD_ALLOC RMW_RET_BAD_ALLOC
   --  unsupported macro: RCL_RET_INVALID_ARGUMENT RMW_RET_INVALID_ARGUMENT
   --  unsupported macro: RCL_RET_UNSUPPORTED RMW_RET_UNSUPPORTED
   RCL_RET_ALREADY_INIT : constant := 100;  --  /opt/ros/foxy/include/rcl/types.h:36

   RCL_RET_NOT_INIT : constant := 101;  --  /opt/ros/foxy/include/rcl/types.h:38

   RCL_RET_MISMATCHED_RMW_ID : constant := 102;  --  /opt/ros/foxy/include/rcl/types.h:40

   RCL_RET_TOPIC_NAME_INVALID : constant := 103;  --  /opt/ros/foxy/include/rcl/types.h:42

   RCL_RET_SERVICE_NAME_INVALID : constant := 104;  --  /opt/ros/foxy/include/rcl/types.h:44

   RCL_RET_UNKNOWN_SUBSTITUTION : constant := 105;  --  /opt/ros/foxy/include/rcl/types.h:46

   RCL_RET_ALREADY_SHUTDOWN : constant := 106;  --  /opt/ros/foxy/include/rcl/types.h:48

   RCL_RET_NODE_INVALID : constant := 200;  --  /opt/ros/foxy/include/rcl/types.h:52
   RCL_RET_NODE_INVALID_NAME : constant := 201;  --  /opt/ros/foxy/include/rcl/types.h:53
   RCL_RET_NODE_INVALID_NAMESPACE : constant := 202;  --  /opt/ros/foxy/include/rcl/types.h:54

   RCL_RET_NODE_NAME_NON_EXISTENT : constant := 203;  --  /opt/ros/foxy/include/rcl/types.h:56

   RCL_RET_PUBLISHER_INVALID : constant := 300;  --  /opt/ros/foxy/include/rcl/types.h:60

   RCL_RET_SUBSCRIPTION_INVALID : constant := 400;  --  /opt/ros/foxy/include/rcl/types.h:64

   RCL_RET_SUBSCRIPTION_TAKE_FAILED : constant := 401;  --  /opt/ros/foxy/include/rcl/types.h:66

   RCL_RET_CLIENT_INVALID : constant := 500;  --  /opt/ros/foxy/include/rcl/types.h:70

   RCL_RET_CLIENT_TAKE_FAILED : constant := 501;  --  /opt/ros/foxy/include/rcl/types.h:72

   RCL_RET_SERVICE_INVALID : constant := 600;  --  /opt/ros/foxy/include/rcl/types.h:76

   RCL_RET_SERVICE_TAKE_FAILED : constant := 601;  --  /opt/ros/foxy/include/rcl/types.h:78

   RCL_RET_TIMER_INVALID : constant := 800;  --  /opt/ros/foxy/include/rcl/types.h:84

   RCL_RET_TIMER_CANCELED : constant := 801;  --  /opt/ros/foxy/include/rcl/types.h:86

   RCL_RET_WAIT_SET_INVALID : constant := 900;  --  /opt/ros/foxy/include/rcl/types.h:90

   RCL_RET_WAIT_SET_EMPTY : constant := 901;  --  /opt/ros/foxy/include/rcl/types.h:92

   RCL_RET_WAIT_SET_FULL : constant := 902;  --  /opt/ros/foxy/include/rcl/types.h:94

   RCL_RET_INVALID_REMAP_RULE : constant := 1001;  --  /opt/ros/foxy/include/rcl/types.h:98

   RCL_RET_WRONG_LEXEME : constant := 1002;  --  /opt/ros/foxy/include/rcl/types.h:100

   RCL_RET_INVALID_ROS_ARGS : constant := 1003;  --  /opt/ros/foxy/include/rcl/types.h:102

   RCL_RET_INVALID_PARAM_RULE : constant := 1010;  --  /opt/ros/foxy/include/rcl/types.h:104

   RCL_RET_INVALID_LOG_LEVEL_RULE : constant := 1020;  --  /opt/ros/foxy/include/rcl/types.h:106

   RCL_RET_EVENT_INVALID : constant := 2000;  --  /opt/ros/foxy/include/rcl/types.h:110

   RCL_RET_EVENT_TAKE_FAILED : constant := 2001;  --  /opt/ros/foxy/include/rcl/types.h:112

  -- Copyright 2014 Open Source Robotics Foundation, Inc.
  -- Licensed under the Apache License, Version 2.0 (the "License");
  -- you may not use this file except in compliance with the License.
  -- You may obtain a copy of the License at
  --     http://www.apache.org/licenses/LICENSE-2.0
  -- Unless required by applicable law or agreed to in writing, software
  -- distributed under the License is distributed on an "AS IS" BASIS,
  -- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  -- See the License for the specific language governing permissions and
  -- limitations under the License.
   subtype rcl_ret_t is rmw_ret_types_h.rmw_ret_t;  -- /opt/ros/foxy/include/rcl/types.h:20

  --/ Success return code.
  --/ Unspecified error return code.
  --/ Timeout occurred return code.
  --/ Failed to allocate memory return code.
  --/ Invalid argument return code.
  --/ Unsupported return code.
  -- rcl specific ret codes start at 100
  --/ rcl_init() already called return code.
  --/ rcl_init() not yet called return code.
  --/ Mismatched rmw identifier return code.
  --/ Topic name does not pass validation.
  --/ Service name (same as topic name) does not pass validation.
  --/ Topic name substitution is unknown.
  --/ rcl_shutdown() already called return code.
  -- rcl node specific ret codes in 2XX
  --/ Invalid rcl_node_t given return code.
  --/ Failed to find node name
  -- rcl publisher specific ret codes in 3XX
  --/ Invalid rcl_publisher_t given return code.
  -- rcl subscription specific ret codes in 4XX
  --/ Invalid rcl_subscription_t given return code.
  --/ Failed to take a message from the subscription return code.
  -- rcl service client specific ret codes in 5XX
  --/ Invalid rcl_client_t given return code.
  --/ Failed to take a response from the client return code.
  -- rcl service server specific ret codes in 6XX
  --/ Invalid rcl_service_t given return code.
  --/ Failed to take a request from the service return code.
  -- rcl guard condition specific ret codes in 7XX
  -- rcl timer specific ret codes in 8XX
  --/ Invalid rcl_timer_t given return code.
  --/ Given timer was canceled return code.
  -- rcl wait and wait set specific ret codes in 9XX
  --/ Invalid rcl_wait_set_t given return code.
  --/ Given rcl_wait_set_t is empty return code.
  --/ Given rcl_wait_set_t is full return code.
  -- rcl argument parsing specific ret codes in 1XXX
  --/ Argument is not a valid remap rule
  --/ Expected one type of lexeme but got another
  --/ Found invalid ros argument while parsing
  --/ Argument is not a valid parameter rule
  --/ Argument is not a valid log level rule
  -- rcl event specific ret codes in 20XX
  --/ Invalid rcl_event_t given return code.
  --/ Failed to take an event from the event handle
  --/ typedef for rmw_serialized_message_t;
   subtype rcl_serialized_message_t is rmw_serialized_message_h.rmw_serialized_message_t;  -- /opt/ros/foxy/include/rcl/types.h:115

end rcl_types_h;
