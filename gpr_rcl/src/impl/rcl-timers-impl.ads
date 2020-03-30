with RCL.Allocators;
limited with RCL.Nodes;

package RCL.Timers.Impl is

   procedure Finalize (This : in out Timer);
   --  Note! This type is not controlled and this is not autocalled.
   --  The node calls it when the timer is deleted
   --  It's not for client use

   function Init (Node      : not null access Nodes.Node'Class;
                  Period    : Duration;
                  Allocator : Allocators.Handle) return Timer;
   --  Note: the timer won't work by itself; it must be created through
   --  Node facilities

   function To_C (This : aliased Timer) return access constant Rcl_Timer_T;

   function To_C_Var (This : aliased in out Timer) return access Rcl_Timer_T;

   function To_Unique_Addr (This : Timer) return System.Address;

private

   function To_C (This : aliased Timer) return access constant Rcl_Timer_T is (This.Impl'Access);

   function To_C_Var (This : aliased in out Timer) return access Rcl_Timer_T is (This.Impl'Access);

   function To_Unique_Addr (This : Timer) return System.Address is (This.Impl.Impl);

end RCL.Timers.Impl;
