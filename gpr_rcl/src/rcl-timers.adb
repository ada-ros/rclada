with RCL.Nodes;

package body RCL.Timers is

   ------------
   -- Cancel --
   ------------

   procedure Cancel (This : in out Timer) is
   begin
      This.Node.Timer_Cancel (This);
   end Cancel;

   -------------------
   -- Change_Period --
   -------------------

   procedure Change_Period (This       : in out Timer;
                            New_Period :        Duration)
   is
      Old : aliased C.long;
   begin
      Check
        (Rcl_Timer_Exchange_Period
           (This.Impl'Access,
            To_Nanoseconds (New_Period),
            Old'Access));
   end Change_Period;

   ----------------
   -- Get_Period --
   ----------------

   function Get_Period (This : Timer) return Duration is
      Period : aliased C.long;
   begin
      Check (Rcl_Timer_Get_Period (This.Impl'Access, Period'Access));
      return To_Duration (Period);
   end Get_Period;

   -----------------
   -- Is_Canceled --
   -----------------

   function Is_Canceled (This : Timer) return Boolean is
      Canceled : aliased CX.Bool;
   begin
      Check (Rcl_Timer_Is_Canceled (This.Impl'Access, Canceled'Access));
      return To_Boolean (Canceled);
   end Is_Canceled;

   --------------------------
   -- Time_Since_Last_Call --
   --------------------------

   function Time_Since_Last_Call (This : Timer) return Duration is
      Time : aliased C.long;
   begin
      Check (Rcl_Timer_Get_Time_Since_Last_Call (This.Impl'Access, Time'Access));
      return To_Duration (Time);
   end Time_Since_Last_Call;

end RCL.Timers;
