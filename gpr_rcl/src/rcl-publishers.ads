with Ada.Finalization;

with Rcl_Publisher_H; use Rcl_Publisher_H;

limited with RCL.Nodes;

with ROSIDL.Dynamic;

package RCL.Publishers is

   type Publisher (<>) is new Ada.Finalization.Limited_Controlled with private;
   
   --  Use the Node to get a publisher      
   
   overriding
   procedure Finalize (This : in out Publisher);
   
   function Is_Valid (This : Publisher) return Boolean;
   
   procedure Publish (This : in out Publisher;
                      Msg  : in out ROSIDL.Dynamic.Message);
   --  Blocking behavior of Publish is still under debate.
   --  See http://docs.ros2.org/ardent/api/rcl/publisher_8h.html#a082c7e5c9e8d8db2e857cc38f74b2580
   --  See https://github.com/ros2/ros2/issues/255
   
private   
   
   type Publisher is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased Rcl_Publisher_T;
      Node :  access Nodes.C_Node;
      Opts : aliased Rcl_Publisher_Options_T;
   end record;      

end RCL.Publishers;
