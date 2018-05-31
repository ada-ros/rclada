with Ada.Unchecked_Deallocation;

with RCL.Allocators;
with RCL.Nodes;

with ROSIDL.Types;

package body RCL.Timers is

   ----------
   -- Bind --
   ----------

   function Bind (C_Timer :                Timer_Id;
                  Node    : aliased in out Nodes.Node) return Timer is
     (Impl => C_Timer,
      Node => Node'Access);

   ------------
   -- Cancel --
   ------------

   procedure Cancel (This : in out Timer) is
   begin
      This.Node.Timer_Cancel (This.Id);
   end Cancel;

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
         This.Impl     := new Rcl_Timer_T;
         This.Impl.all := Rcl_Get_Zero_Initialized_Timer;
         Check
           (Rcl_Timer_Init
              (This.Impl,
               ROSIDL.Types.Int64 (Period * 1_000_000_000.0),
               Null,
               Allocators.Get_Default_Allocator));
      end return;
   end Init;

   -----------------
   -- Is_Canceled --
   -----------------

   function Is_Canceled (This : Timer_Id) return Boolean is
      Canceled : aliased CX.Bool;
   begin
      Check (Rcl_Timer_Is_Canceled (This, Canceled'Access));
      return To_Boolean (Canceled);
   end Is_Canceled;

end RCL.Timers;
