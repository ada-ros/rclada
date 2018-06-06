with Ada.Real_Time;

limited with RCL.Nodes;
with RCL.Services.Impl;
with RCL.Subscriptions;
with RCL.Timers;

with ROSIDL.Typesupport;

package RCL.Callbacks is

   --  Helper types to couple an element with its callback, and dispatch calls
   
   type Dispatcher is limited interface;
   
   procedure Dispatch (This : in out Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc

   --------------
   -- Services --
   --------------
   
   type Service_Dispatcher is new Dispatcher with record
      Service  : Services.Impl.C_Service;
      Callback : Services.Callback;
      Support  : ROSIDL.Typesupport.Service_Support;
   end record;
   
   procedure Dispatch (This : in out Service_Dispatcher);
   --  This is insufficient for what has to be done; actual logic is in Nodes
   
   -------------------
   -- Subscriptions --
   -------------------
   
   type Subscription_Dispatcher is new Dispatcher with record
      Subscription : aliased Subscriptions.C_Subscription;
      Callback     :         Subscriptions.Callback;
      Support      :         ROSIDL.Typesupport.Message_Support;
   end record;
   
   procedure Dispatch (This : in out Subscription_Dispatcher);
   
   ------------
   -- Timers --
   ------------
   
   type Timer_Dispatcher is new Dispatcher with record
      Timer     : aliased Timers.Timer_Id;
      Callback  :         Timers.Callback;
      Last_Call :         Ada.Real_Time.Time := Ada.Real_Time.Clock;
      Node      :  access Nodes.Node;
   end record;
   
   function "=" (L, R : Timer_Dispatcher) return Boolean;
   
   procedure Dispatch (This : in out Timer_Dispatcher);
   
private
   
   use all type Timers.Timer_Id;
   
   function "=" (L, R : Timer_Dispatcher) return Boolean is
      (L.Timer = R.Timer);
   
end RCL.Callbacks;
