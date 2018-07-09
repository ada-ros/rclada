with Rcl_Node_H;    use Rcl_Node_H;
with Rcl_Service_H; use Rcl_Service_H;

with System;

package RCL.Services.Impl is
  
   type C_Service is tagged private;
   
   procedure Finalize (This : in out C_Service; Node : access Rcl_Node_T);
   
   function To_C_Service (C : Rcl_Service_T) return C_Service;
   
   function To_C (This : aliased C_Service) return access constant Rcl_Service_T; 
   
   function To_Unique_Addr (This : C_Service) return System.Address;
   
private

   type C_Service is tagged record
      C : aliased Rcl_Service_T;
   end record;
   
   function To_C_Service (C : Rcl_Service_T) return C_Service is (C => C);
   
   function To_C (This : aliased C_Service) return access constant Rcl_Service_T is
     (This.C'Access);
   
   function To_Unique_Addr (This : C_Service) return System.Address is
      (This.C.Impl);

end RCL.Services.Impl;
