with RCL.Nodes;

package body RCL.Executors is

   ---------
   -- Add --
   ---------

   procedure Add (This :         in out Executor;
                  Node : aliased in out Nodes.Node'Class)
   is
   begin
      This.Nodes.Insert (Node'Unchecked_Access);
   end Add;

   ---------------------
   -- Common_Dispatch --
   ---------------------

   procedure Common_Dispatch (CB : in out Callbacks.Dispatcher'Class) is
   begin
      CB.Dispatch;
   end Common_Dispatch;

   ------------
   -- Remove --
   ------------

   procedure Remove (This :         in out Executor;
                     Node : aliased in out Nodes.Node'Class) is
   begin
      This.Nodes.Delete (Node'Unchecked_Access);
   end Remove;

   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Executor;
                   Once   :        Boolean       := False;
                   During :        ROS2_Duration := 0.1) is
   begin
      null;
   end Spin;

end RCL.Executors;
