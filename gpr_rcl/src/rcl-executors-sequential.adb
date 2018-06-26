package body RCL.Executors.Sequential is

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This   : in out Executor;
                       Node   : access Nodes.Node'Class;
                       Handle :        Callbacks.Handle) is
      pragma Unreferenced (This);
   begin
      Common_Dispatch (Node, Handle);
   end Dispatch;

end RCL.Executors.Sequential;
