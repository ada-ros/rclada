with RCL.Nodes;

package body RCL.Services.Impl is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out C_Service; Node : in out Nodes.C_Node) is
   begin
      Check (Rcl_Service_Fini (This.C'Access, Node.Impl'Access));
   end Finalize;

end RCL.Services.Impl;
