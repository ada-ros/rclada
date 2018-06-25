package body RCL.Executors.Sequential is

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Executor;
                       Call : in out Callbacks.Dispatcher'Class) is
      pragma Unreferenced (This);
   begin
      Common_Dispatch (Call);
   end Dispatch;

end RCL.Executors.Sequential;
