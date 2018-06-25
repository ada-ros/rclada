package body RCL.Executors is

   ---------------------
   -- Common_Dispatch --
   ---------------------

   procedure Common_Dispatch (CB : in out Callbacks.Dispatcher'Class) is
   begin
      CB.Dispatch;
   end Common_Dispatch;

end RCL.Executors;
