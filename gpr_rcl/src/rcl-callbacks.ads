with Ada.Real_Time;

limited with RCL.Nodes;

with RCL.Clients.Impl;
with RCL.Services.Impl;
with RCL.Subscriptions;
with RCL.Timers;

with ROSIDL.Impl;
with ROSIDL.Typesupport;

with System;

package RCL.Callbacks is

   --  Helper types to couple an element with its callback, and dispatch calls
   --  Used only privately by the Node implementation
   
   type Dispatcher (Node : not null access Nodes.Node'Class) is abstract tagged null record;
   
   procedure Dispatch (This : in out Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc
   
   function To_Ptr (This : in out Dispatcher) return System.Address is abstract;
   --  This is the pointer to the C object that is expected by the
   --    rcl_wait_add_* functions. It also serves to locate the dispatcher
   --    once it has been triggered on the C side.

   -------------
   -- Clients --
   -------------
   
   type Client_Dispatcher is new Dispatcher with record
      Client   : Clients.Impl.C_Client;
      Callback : Clients.Callback;
      
      --  Used for the blocking version
      Blocking : Boolean;
      Response : ROSIDL.Impl.Message_Holder;
      Success  : Boolean;
   end record;
   
   procedure Dispatch (This : in out Client_Dispatcher);
   
   function To_Ptr (This : in out Client_Dispatcher) return System.Address;
   
   --------------
   -- Services --
   --------------
   
   type Service_Dispatcher is new Dispatcher with record
      Service  : Services.Impl.C_Service;
      Callback : Services.Callback;
      Support  : ROSIDL.Typesupport.Service_Support;
   end record;
   
   procedure Dispatch (This : in out Service_Dispatcher);
   
   function To_Ptr (This : in out Service_Dispatcher) return System.Address;   
   
   -------------------
   -- Subscriptions --
   -------------------
   
   type Subscription_Dispatcher is new Dispatcher with record
      Subscription : aliased Subscriptions.C_Subscription;
      Callback     :         Subscriptions.Callback;
      Support      :         ROSIDL.Typesupport.Message_Support;
   end record;
   
   procedure Dispatch (This : in out Subscription_Dispatcher);
   
   function To_Ptr (This : in out Subscription_Dispatcher) return System.Address;
   
   ------------
   -- Timers --
   ------------
   
   type Timer_Dispatcher is new Dispatcher with record
      Timer     : aliased Timers.Timer_Id;
      Callback  :         Timers.Callback;
      Last_Call :         Ada.Real_Time.Time := Ada.Real_Time.Clock;
   end record;
   
   function "=" (L, R : Timer_Dispatcher) return Boolean;
   
   procedure Dispatch (This : in out Timer_Dispatcher);
   
   function To_Ptr (This : in out Timer_Dispatcher) return System.Address;
   
private
   
   use all type Timers.Timer_Id;
   
   function "=" (L, R : Timer_Dispatcher) return Boolean is
     (L.Timer = R.Timer);
   
   function To_Ptr (This : in out Client_Dispatcher) return System.Address is
     (This.Client.To_C.all'Address);

   function To_Ptr (This : in out Service_Dispatcher) return System.Address is
     (This.Service.To_C.all'Address);
   
   function To_Ptr (This : in out Subscription_Dispatcher) return System.Address is
     (This.Subscription.C'Address);
   
   function To_Ptr (This : in out Timer_Dispatcher) return System.Address is
     (Timers.To_C (This.Timer).all'Address);
   
end RCL.Callbacks;
