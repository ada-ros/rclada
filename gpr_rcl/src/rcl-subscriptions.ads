limited with RCL.Nodes;

with ROSIDL.Dynamic;
with ROSIDL.Static.Message;

package RCL.Subscriptions is

   --  Typed subscriptions

   generic
      with package Handling is new ROSIDL.Static.Message (<>);
      with procedure Callback (Node : in out Nodes.Node'Class;
                               Msg  :        Handling.C_Message;
                               Info :        ROSIDL.Message_Info);
   package Typed is

      procedure Subscribe (Node  : in out Nodes.Node'Class;
                           Topic :        String);

   end Typed;

   --  Untyped subscriptions

   type Callback is access procedure (Node : in out Nodes.Node'Class;
                                      Msg  : in out ROSIDL.Dynamic.Message;
                                      Info :        ROSIDL.Message_Info);

end RCL.Subscriptions;
