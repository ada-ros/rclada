with Ada.Finalization;

with Rcl_Publisher_H; use Rcl_Publisher_H;

limited with RCL.Nodes;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

package RCL.Publishers is

   type Publisher (Node : not null access Nodes.Node'Class) is 
     new Ada.Finalization.Limited_Controlled with private;
   
   --  Use the Node to get a publisher directly, or the init function below
   
   procedure Init (This     : in out Publisher;
                   Msg_Type :        ROSIDL.Typesupport.Message_Support;
                   Topic    :        String);
   
   overriding
   procedure Finalize (This : in out Publisher);
   
   function Is_Valid (This : Publisher) return Boolean;
   
   procedure Publish (This : in out Publisher;
                      Msg  : in out ROSIDL.Dynamic.Message);
   --  Blocking behavior of Publish is still under debate.
   --  See http://docs.ros2.org/ardent/api/rcl/publisher_8h.html#a082c7e5c9e8d8db2e857cc38f74b2580
   --  See https://github.com/ros2/ros2/issues/255
   
private   
   
   type Publisher (Node : not null access Nodes.Node'Class) is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased Rcl_Publisher_T         := Rcl_Get_Zero_Initialized_Publisher;
      Opts : aliased Rcl_Publisher_Options_T := Rcl_Publisher_Get_Default_Options;
   end record;      

end RCL.Publishers;
