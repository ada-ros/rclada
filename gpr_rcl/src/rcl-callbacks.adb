with RCL.Logging;

package body RCL.Callbacks is

   procedure Dispatch (This : in out Subscription_Dispatcher) is
      Msg  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (This.Support);
        -- (This.Support.Message_Class.Package_Name, This.Support.Message_Class.Message_Name);
      Info : ROSIDL.Message_Info;
   begin
      if Subscriptions.Take_Raw (This.Subscription,
                                 Msg.To_Ptr,
                                 Info)
      then
         This.Callback (Msg, Info);
      else
         Logging.Warn ("Subscription dispatcher: Take_Raw failed when expecting message");
      end if;
   end Dispatch;

end RCL.Callbacks ;
