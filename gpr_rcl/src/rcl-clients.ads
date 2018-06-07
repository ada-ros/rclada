with ROSIDL.Dynamic;

package RCL.Clients is

   type Callback is access procedure (Response : ROSIDL.Dynamic.Message);

end RCL.Clients;
