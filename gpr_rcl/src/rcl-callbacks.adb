with Rcl_Timer_H; use Rcl_Timer_H;
with Rcl_Types_H; use Rcl_Types_H;

with Rmw_Types_H; use Rmw_Types_H;

with Rcl.Logging;
with RCL.Nodes; pragma Unreferenced (RCL.Nodes);

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
      Temp : Timers.Timer  := Timers.Bind (This.Timer, This.Node.all);
      Now  : constant Time := Clock;
      Ret  : Rcl_Error_Code;
   begin
      Ret := Rcl_Timer_Call (Timers.To_C (This.Timer));
      --  This "snoozes" the C timer

      This.Callback (Temp, -- temporary timer for the callee
                     To_Duration (Now - This.Last_Call));
      This.Last_Call := Now;

      case Ret is
         when RCL_RET_TIMER_CANCELED =>
            Logging.Debug ("Attempt to call canceled timer");
            -- Happens once after canceling, not important
         when Rmw_Ret_OK =>
            null;
         when others =>
            Check (Ret);
      end case;
   end Dispatch;

end RCL.Callbacks ;
