with RCL.Allocators.Impl;
with RCL.Init;

package body RCL.Timers.Impl is
   
   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Timer) is
   begin
      Check (Rcl_Timer_Fini (This.Impl'Access));
      RCL.Init.Finalize;
   end Finalize;
   
   ----------
   -- Init --
   ----------

   function Init (Node      : not null access Nodes.Node'Class;
                  Period    : Duration;
                  Allocator : Allocators.Handle) return Timer
   is
   begin
      RCL.Init.Initialize (Allocator, RCL.Init.Dont_Care);

      return This : Timer (Node) do
         This.Impl     := Rcl_Get_Zero_Initialized_Timer;
         Check
           (Rcl_Timer_Init
              (This.Impl'Access,
               To_Nanoseconds (Period),
               null,
               Allocators.Impl.To_C (Allocator.all)));
      end return;
   end Init;

end RCL.Timers.Impl;
