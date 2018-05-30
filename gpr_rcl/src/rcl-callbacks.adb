with Rcl_Timer_H; use Rcl_Timer_H;

with ROSIDL.Dynamic;

package body RCL.Callbacks is

   --------------
   -- Dispatch --
   --------------

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
         raise Program_Error with "Subscription dispatcher: Take_Raw failed when message was expected";
      end if;
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Timer_Dispatcher) is
      use Ada.Real_Time;
      Temp : Timers.Timer  := Timers.Bind (This.Timer);
      Now  : constant Time := Clock;
   begin
      This.Callback (Temp, -- temporary timer for the callee
                     To_Duration (Now - This.Last_Call));
      This.Last_Call := Now;

      Check (Rcl_Timer_Call (Timers.To_C (This.Timer)));
   end Dispatch;

end RCL.Callbacks ;
