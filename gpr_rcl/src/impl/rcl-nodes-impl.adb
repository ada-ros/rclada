package body RCL.Nodes.Impl is

   --------------------
   -- Client_Success --
   --------------------

   procedure Client_Success (This : in out Node; Client : Dispatchers.Handle) is
   begin
      This.Dispatchers.Client_Success (Client);
   end Client_Success;

   -------------------
   -- Get_Callbacks --
   -------------------

   procedure Get_Callbacks (This : in out Node'Class; Set : in out Dispatchers.Maps.Map) is
   begin
      This.Dispatchers.Union (Set);
   end Get_Callbacks;

   -------------
   -- Trigger --
   -------------

   procedure Trigger (This : in out Node'Class; Dispatcher : Dispatchers.Handle) is
   begin
      This.Dispatchers.Get (Dispatcher).Dispatch;
   end Trigger;

end RCL.Nodes.Impl;
