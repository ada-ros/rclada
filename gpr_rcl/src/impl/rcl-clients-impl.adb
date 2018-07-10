package body RCL.Clients.Impl is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out C_Client; Node : access Rcl_Node_T) is
   begin
      Check (Rcl_Client_Fini (This.C'Access, Node));
   end Finalize;

end RCL.Clients.Impl;
