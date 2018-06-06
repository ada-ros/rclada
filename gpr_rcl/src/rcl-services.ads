with ROSIDL.Dynamic;

package RCL.Services is

   type Callback is access procedure (Req  : in out ROSIDL.Dynamic.Message;
                                      Resp : in out ROSIDL.Dynamic.Message);  

end RCL.Services;
