with Rcl_Client_H; use Rcl_Client_H;

with System;

package RCL.Clients.Impl is
  
   type C_Client is tagged private;
   
   function To_C_Client (C : Rcl_Client_T) return C_Client;
   
   function To_C (This : aliased C_Client) return access constant Rcl_Client_T;
   
   function To_Var_C (This : aliased in out C_Client) return access Rcl_Client_T;
   
   function To_Unique_Addr (This : C_Client) return System.Address;
   
private

   type C_Client is tagged record
      C : aliased Rcl_Client_T;
   end record;
   
   function To_C_Client (C : Rcl_Client_T) return C_Client is (C => C);
   
   function To_C (This : aliased C_Client) return access constant Rcl_Client_T is
     (This.C'Access);
   
   function To_Var_C (This : aliased in out C_Client) return access Rcl_Client_T is
      (This.C'Access);
   
   function To_Unique_Addr (This : C_Client) return System.Address is
      (This.C.Impl);

end RCL.Clients.Impl;
