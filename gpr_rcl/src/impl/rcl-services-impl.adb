package body RCL.Services.Impl is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out C_Service; Node : access Rcl_Node_T) is
   begin
      Check (Rcl_Service_Fini (This.C'Access, Node));
   end Finalize;

end RCL.Services.Impl;
