limited with RCL.Nodes;
with RCL.Options;

with ROSIDL.Dynamic;

package RCL.Subscriptions is

   type Callback is access procedure (Node : in out Nodes.Node'Class;
                                      Msg  : in out ROSIDL.Dynamic.Message;
                                      Info :        ROSIDL.Message_Info);

   subtype Options is RCL.Options.Topic_Options;
   Defaults : Options renames RCL.Options.Topic_Defaults;

end RCL.Subscriptions;
