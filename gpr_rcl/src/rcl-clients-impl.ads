with Rcl_Client_H; use Rcl_Client_H;

package RCL.Clients.Impl is
  
   type C_Client is tagged private;
   
   function To_C_Client (C : Rcl_Client_T) return C_Client;
   
   function To_C (This : aliased in out C_Client) return access Rcl_Client_T; 
   
private

   type C_Client is tagged record
      C : aliased Rcl_Client_T;
   end record;
   
   function To_C_Client (C : Rcl_Client_T) return C_Client is (C => C);
   
   function To_C (This : aliased in out C_Client) return access Rcl_Client_T is
      (This.C'Access);

end RCL.Clients.Impl;
