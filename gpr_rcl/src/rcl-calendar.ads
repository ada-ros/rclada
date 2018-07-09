with Ada.Finalization; use Ada.Finalization;

with RCL.Allocators;

with Rcl_Time_H; use Rcl_Time_H;

package RCL.Calendar is

   --  Corresponds to rcl/time.h facilities
   
   subtype Nanoseconds is Rcl_Time_Point_Value_T; -- Actually C.long;
   
   type Time is private;
   
   function To_Duration (NS : Nanoseconds) return Duration;
   
   function To_Nanoseconds (D : Duration) return Nanoseconds;
   
   type Kinds is (ROS,     -- Simulated or system time
                  Steady,  -- Monotonic time for hardware timeouts
                  System); -- System wall-time
   --  See http://design.ros2.org/articles/clock_and_time.html
   
   type Clock is tagged limited private;   
   
   procedure Init (This  : in out Clock; 
                   Kind  : Kinds := ROS;
                   Alloc : Allocators.Handle := Allocators.Global_Allocator);
   
--     overriding
   procedure Finalize (This : in out Clock);   
   
   function Is_Valid (This : in out Clock) return Boolean;
   
   function Now (This : in out Clock) return Time;
   --  Time elapsed since some unknown epoch.
   --  No way to use this directly, only for relative operations with durations
   
   function Elapsed (This : in out Clock) return Duration;
   --  Time elapsed since last clock reset or initialization
   
   procedure Reset (This : in out Clock);
   --  See elapsed
   --  Beware of time drift if relying on this for anything serious
   
   --  Overrides not bound since documentation is unclear
   
   --  Time operations
   function "-" (L : Time; R : Time)     return Duration;
   function "+" (L : Time; R : Duration) return Time;
   function "-" (L : Time; R : Duration) return Time;
   
private 
   
   type Time is new Duration;
   
   type Clock is new Limited_Controlled with record
      Impl   : aliased Rcl_Clock_T;
      Inited :         Boolean := False;
      Mark   :         Time;
   end record;
   
   function To_Duration (NS : Nanoseconds) return Duration is 
     (Duration (Long_Long_Float (NS) / 1_000_000_000.0));
   
   function To_Nanoseconds (D : Duration) return Nanoseconds is
     (Nanoseconds (Long_Long_Float (D) * 1_000_000_000.0));
   
   function "-" (L : Time; R : Time)     return Duration is
     (Duration (L) - Duration (R));
         
   function "+" (L : Time; R : Duration) return Time is 
     (Time (Duration (L) + R));
   
   function "-" (L : Time; R : Duration) return Time is
     (L + (-R));

end RCL.Calendar;
