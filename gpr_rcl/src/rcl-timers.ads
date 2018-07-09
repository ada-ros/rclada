limited with RCL.Nodes;

with RCL.Allocators;

with Rcl_Timer_H; use Rcl_Timer_H;

with System;

package RCL.Timers is  
   
   --  See RCL.Nodes for creation/destruction of timers, since the node
   --  manages them to avoid reference counting

   --  Although Ada has native timers, this may be useful if you want
   --  to stay in a single-threaded, ROS2-managed situation
   
   type Timer_Id is private;
   
   function "=" (L, R : Timer_Id) return Boolean;
   
   
   type Timer (<>) is tagged limited private;   
   --  Note: this is not a controlled type. 
   --  The Node finalizes it on timer deletion      
   
   type Callback is 
     access procedure (Node    : in out Nodes.Node'Class;
                       Timer   : in out Timers.Timer;
                       Elapsed :        Duration);  
   
   procedure Cancel (This : in out Timer);
   --  Will internally call Node.Timer_Cancel
   
   procedure Change_Period (This       : in out Timer; 
                            New_Period :        Duration);   
   
   function Get_Period (This : Timer) return Duration;
   
   function Id (This : Timer) return Timer_Id;
   
   function Time_Since_Last_Call (This : Timer) return Duration;
   
   -----------------------------------
   --  Not intended for client use  --
   
   function Bind (C_Timer :                Timer_Id; 
                  Node    : aliased in out Nodes.Node'Class) return Timer;   
   
   procedure Finalize (This : in out Timer);
   --  Note! This type is not controlled and this is not autocalled.
   --  The node calls it when the timer is deleted
   --  It's not for client use
   
   procedure Free (This : in out Timer_Id);
   
   function Init (Period    : Duration;
                  Allocator : Allocators.Handle) return Timer;
   --  Note: the timer won't work by itself; it must be created through
   --  Node facilities
   
   function Is_Canceled (This : Timer_Id) return Boolean;  
   
   function To_C (This : Timer_Id) return access Rcl_Timer_T;
      
   function To_Unique_Addr (This : Timer_Id) return System.Address;
   
private   
   
   type Timer_Id is access Rcl_Timer_T;
   --  Rcl_Timer_T is just a husk on top of a pointer, we can use it directly
   
   type Timer is tagged limited record
      Impl : Timer_Id;
      Node : access Nodes.Node;
   end record;
   
   use all type System.Address;
   
   function "=" (L, R : Timer_Id) return Boolean is
      (L.Impl = R.Impl);

   function To_C (This : Timer_Id) return access Rcl_Timer_T is (This);
   
   function To_Unique_Addr (This : Timer_Id) return System.Address is
      (This.Impl);
   
end RCL.Timers;
