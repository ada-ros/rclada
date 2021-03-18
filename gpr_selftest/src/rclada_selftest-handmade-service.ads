with ROSIDL.Typesupport;

package Rclada_Selftest.Handmade.Service is

   --  A service, handcrafted. This was done initially to help with the
   --  creation of the static message generator.

   use ROSIDL;

   --  GENERATION BEGIN  --

   package Handling is

      Support : constant Typesupport.Service_Support :=
                  Typesupport.Get_Service_Support
                    (Pkg => "rclada",
                     Srv => "Test");

   end Handling;

   --  GENERATION END  --

end Rclada_Selftest.Handmade.Service;
