with Rcl_Service_H; use Rcl_Service_H;
with Rcl_Timer_H;   use Rcl_Timer_H;
with Rcl_Types_H;   use Rcl_Types_H;

with RCL.Logging;
with RCL.Timers.Impl;

with ROSIDL.Dynamic;

package body RCL.Impl.Callbacks is

   ----------
   -- Call --
   ----------

   procedure Call (This : Definite_Callback) is
   begin
      case This.Kind is
         when Client       => This.Client.Call;
         when Service      => This.Service.Call;
         when Subscription => This.Subscription.Call;
         when Timer        => This.Timer.Call;
         when others       => raise Program_Error;
      end case;
   end Call;

   ----------
   -- Call --
   ----------

   overriding procedure Call (This : Client_Callback) is
   begin
      This.User_Callback (This.Node.all,
                          This.Response.Constant_Reference.Element.Msg.all);
   end Call;

   ----------
   -- Call --
   ----------

   overriding procedure Call (This : Service_Callback) is
      Response : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (This.Support.Response_Support);
      Header   : aliased Rmw_Request_Id_T := This.Header;
   begin
      This.User_Callback (This.Node.all,
                          This.Request.Constant_Reference.Element.Msg.all,
                          Response);

      Check
        (Rcl_Send_Response
           (This.Service.To_C,
            Header'Access,
            Response.To_Ptr));
   end Call;

   ----------
   -- Call --
   ----------

   overriding procedure Call (This : Subscription_Callback) is
   begin
      This.User_Callback (This.Node.all,
                          This.Message.Constant_Reference.Element.Msg.all,
                          This.Info);
   end Call;

   ----------
   -- Call --
   ----------

   overriding procedure Call (This : Timer_Callback) is
      Ret     : Rcl_Error_Code;
      Elapsed : constant Duration := This.Timer.Time_Since_Last_Call;
      Temp    : Timers.Timer      := This.Timer;
   begin
      Ret := Rcl_Timer_Call (Timers.Impl.To_C_Var (Temp));
      --  This "snoozes" the C timer and resets time since last call

      case Ret is
         when RCL_RET_TIMER_CANCELED =>
            Logging.Warn ("Attempt to call canceled timer");
            -- Happens once after canceling, not important
         when Rmw_Ret_OK =>
            This.User_Callback (This.Node.all,
                                Temp, -- temporary timer for the callee
                                Elapsed);
         when others =>
            Check (Ret);
      end case;
   end Call;

end RCL.Impl.Callbacks;
