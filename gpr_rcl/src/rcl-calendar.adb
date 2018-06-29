with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;

package body RCL.Calendar is

   ----------
   -- Init --
   ----------

   procedure Init (This  : in out Clock; 
                   Kind  : Kinds := ROS;
                   Alloc : Allocators.Allocator := Allocators.Global_Allocator) 
   is
      Local_Alloc : aliased Allocators.C_Allocator := Alloc.To_C;
   begin
      Check (Rcl_Clock_Init (
             (case Kind is
                when ROS    => RCL_ROS_TIME,
                when Steady => RCL_STEADY_TIME,
                when System => RCL_SYSTEM_TIME),
             This.Impl'Access,
             Local_Alloc'Access));
      --  This unrestricted access will go away once we have allocators in place
      
      This.Inited := True;
      This.Mark   := This.Now;
   end Init;
   
   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Clock) is
   begin
      if This.Inited then 
         Check (Rcl_Clock_Fini (This.Impl'Access));
         This.Inited := False;
      end if;
   exception
      when E : others =>
         Logging.Warn ("Exception during finalization: " &
                         Exception_Information (E));
   end Finalize;
         
   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in out Clock) return Boolean is
     (This.Inited and then To_Boolean (Rcl_Clock_Valid (This.Impl'Access)));
   
   ---------
   -- Now --
   ---------

   function Now (This : in out Clock) return Time is
      C_Time : aliased Rcl_Time_Point_T;
   begin
      Check (Rcl_Clock_Get_Now (This.Impl'Access, C_Time'Access));
      
      return Time (To_Duration (C_Time.Nanoseconds));
   end Now;
   
   -------------
   -- Elapsed --
   -------------

   function Elapsed (This : in out Clock) return Duration is
      (This.Now - This.Mark);
   --  Time elapsed since last clock reset or initialization
   
   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Clock) is
   begin
      This.Mark := This.Now;
   end Reset;

end RCL.Calendar;
