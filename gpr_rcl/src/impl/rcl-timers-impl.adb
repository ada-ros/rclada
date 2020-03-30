with RCL.Calendar;
with RCL.Nodes; -- Full view

package body RCL.Timers.Impl is

   Default_Clock: Calendar.Clock;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Timer) is
   begin
      Check (Rcl_Timer_Fini (This.Impl'Access));
   end Finalize;

   ----------
   -- Init --
   ----------

   function Init (Node      : not null access Nodes.Node'Class;
                  Period    : Duration;
                  Allocator : Allocators.Handle) return Timer
   is
   begin
      return This : Timer (Node) do
         This.Impl     := Rcl_Get_Zero_Initialized_Timer;

         --  TODO: allow using a clock not the global/default one hidden here
         Check
           (Rcl_Timer_Init
              (Timer     => This.Impl'Access,
               Clock     => Default_Clock.To_C,
               Context   => Node.Context.To_C,
               Period    => To_Nanoseconds (Period),
               Callback  => null,
               Allocator => Allocator.To_C.all));
      end return;
   end Init;

begin
   Calendar.Init (Default_Clock);
end RCL.Timers.Impl;
