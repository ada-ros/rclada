with Ada.Unchecked_Deallocation;

with RCL.Allocators;

with ROSIDL.Types;

package body RCL.Timers is

   ----------
   -- Bind --
   ----------

   function Bind (C_Timer : Timer_Id) return Timer is (Impl => C_Timer);

   -------------------
   -- Change_Period --
   -------------------

   procedure Change_Period (This       : in out Timer;
                            New_Period :        Duration)
   is
      Old : aliased ROSIDL.Types.Int64;
   begin
      Check
        (Rcl_Timer_Exchange_Period
           (This.Impl,
            ROSIDL.Types.Int64 (New_Period * 1_000_000_000.0),
            Old'Access));
   end Change_Period;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Timer) is
   begin
      Check (Rcl_Timer_Fini (This.Impl));
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Timer_Id) is
      procedure Dealloc is new Ada.Unchecked_Deallocation (Rcl_Timer_T, Timer_Id);
   begin
      Dealloc (This);
   end Free;

   ----------------
   -- Get_Period --
   ----------------

   function Get_Period (This : Timer) return Duration is
      Period : aliased ROSIDL.Types.Int64;
   begin
      Check (Rcl_Timer_Get_Period (This.Impl, Period'Access));
      return Duration (Period) / 1_000_000_000.0;
   end Get_Period;

   --------
   -- Id --
   --------

   function Id (This : Timer) return Timer_Id is (This.Impl);

   ----------
   -- Init --
   ----------

   function Init (Period   : Duration) return Timer
   is
   begin
      return This : aliased Timer do
         This.Impl.all := Rcl_Get_Zero_Initialized_Timer;
         Check
           (Rcl_Timer_Init
              (This.Impl,
               ROSIDL.Types.Int64 (Period * 1_000_000_000.0),
               Null,
               Allocators.Get_Default_Allocator));
      end return;
   end Init;

end RCL.Timers;
