with Ada.Real_Time;

with RCL.Subscriptions;
with RCL.Timers;

with ROSIDL.Typesupport;

package RCL.Callbacks is

   --  Helper types to couple an element with its callback, and dispatch calls
   
   type Dispatcher is limited interface;
   
   procedure Dispatch (This : in out Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc

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
   end record;
   
   procedure Dispatch (This : in out Timer_Dispatcher);
   
end RCL.Callbacks;
