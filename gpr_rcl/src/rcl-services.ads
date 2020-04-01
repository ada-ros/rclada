limited with RCL.Nodes;

with ROSIDL.Dynamic;

package RCL.Services is

   type Callback is access procedure (Node : in out Nodes.Node'Class;
                                      Req  : in out ROSIDL.Dynamic.Message;
                                      Resp : in out ROSIDL.Dynamic.Message);

end RCL.Services;
