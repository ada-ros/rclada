with RCL.Nodes;

package body RCL.Clients.Impl is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out C_Client; Node : in out Nodes.C_Node) is
   begin
      Check (Rcl_Client_Fini (This.C'Access, Node.Impl'Access));
   end Finalize;

end RCL.Clients.Impl;
