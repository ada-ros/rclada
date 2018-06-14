limited with RCL.Nodes;

with ROSIDL.Dynamic;

package RCL.Clients is

   type Callback is access procedure (Node     : in out Nodes.Node'Class;
                                      Response :        ROSIDL.Dynamic.Message);

end RCL.Clients;
