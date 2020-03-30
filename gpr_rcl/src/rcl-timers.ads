limited with RCL.Nodes;

with Rcl_Timer_H; use Rcl_Timer_H;

with System;

package RCL.Timers is

   --  See RCL.Nodes for creation/destruction of timers, since the node
   --  manages them to avoid reference counting

   --  Although Ada has native timers, this may be useful if you want
   --  to stay in a single-threaded, ROS2-managed situation

   type Timer (Node : not null access Nodes.Node'Class) is tagged private;

   function "=" (L, R : Timer) return Boolean;

   type Callback is
     access procedure (Node    : in out Nodes.Node'Class;
                       Timer   : in out Timers.Timer;
                       Elapsed :        Duration);

   procedure Cancel (This : in out Timer);

   procedure Change_Period (This       : in out Timer;
                            New_Period :        Duration);

   function Get_Period (This : Timer) return Duration;

   function Is_Canceled (This : Timer) return Boolean;

   function Time_Since_Last_Call (This : Timer) return Duration;

private

   --  The user gets to keep a pointer to the node and timer.

   type Timer (Node : not null access Nodes.Node'Class) is tagged record
      Impl : aliased Rcl_Timer_T;
   end record;

   use all type System.Address;

   function "=" (L, R : Timer) return Boolean is
     (L.Impl.Impl = R.Impl.Impl);

   function To_Duration (Nanoseconds : C.Long) return Duration is
     (Duration (Nanoseconds) / 1_000_000_000.0);

   function To_Nanoseconds (Dur : Duration) return C.Long is
      (C.Long (Dur * 1_000_000_000.0));

end RCL.Timers;
