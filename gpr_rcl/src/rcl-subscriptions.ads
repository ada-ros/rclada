limited with RCL.Nodes;

with ROSIDL.Dynamic;

package RCL.Subscriptions is

   type Callback is access procedure (Node : in out Nodes.Node'Class;
                                      Msg  : in out ROSIDL.Dynamic.Message;
                                      Info :        ROSIDL.Message_Info);

end RCL.Subscriptions;
