with Rcl_Service_H; use Rcl_Service_H;

package RCL.Services.Impl is
  
   type C_Service is tagged private;
   
   function To_C_Service (C : Rcl_Service_T) return C_Service;
   
   function To_C (This : aliased in out C_Service) return access Rcl_Service_T; 
   
private

   type C_Service is tagged record
      C : aliased Rcl_Service_T;
   end record;
   
   function To_C_Service (C : Rcl_Service_T) return C_Service is (C => C);
   
   function To_C (This : aliased in out C_Service) return access Rcl_Service_T is
      (This.C'Access);

end RCL.Services.Impl;
