with Ada.Containers.Indefinite_Ordered_Sets;

limited with RCL.Nodes;

with RCL.Clients.Impl;
with RCL.Services.Impl;
with RCL.Subscriptions;
with RCL.Timers;

with ROSIDL.Impl;
with ROSIDL.Typesupport;

with System;

package RCL.Dispatchers is

   --  Helper types to couple an element with its dispatcher, and dispatch calls
   --  Used only privately by the Node implementation
   
   type Handle is new System.Address;
   --  This address uniquely designates a callback, by the implementation pointer
   --    of the C client data type. This will be hidden in the near future.
   function "+" (Addr : System.Address) return Handle;
   
   type Dispatcher (Node : not null access Nodes.Node'Class) is abstract tagged null record;
   
   procedure Dispatch (This : Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc
   --  The dispatcher can't be modified, since it comes from a protected
   --    structure in the node. One must directly call node functions to
   --    indicate changes or replace with an updated callback
   
   procedure Finalize (This : in out Dispatcher) is null;
   --  Override to free any resources remaining
   --  The dispatcher will be deleted immediately after this call.
   --  This is called by the node, is not Finalization in Ada sense.
   
   function To_Handle (This : Dispatcher) return Handle is abstract;
   --  This is the pointer to the C object that is expected by the
   --    rcl_wait_add_* functions. It also serves to locate the dispatcher
   --    once it has been triggered on the C side.
   
   function "<" (L, R : Dispatcher'Class) return Boolean;
   
   use all type System.Address;
   
   package Dispatcher_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Dispatcher'Class);
   
   type Set is new Dispatcher_Sets.Set with null record;
   
   function Num_Clients       (This : Set) return Natural;
   function Num_Services      (This : Set) return Natural;
   function Num_Subscriptions (This : Set) return Natural;
   function Num_Timers        (This : Set) return Natural;
   
   function Contains (This : Set; Addr : Handle) return Boolean;
   
   function Get (This : Set; Addr : Handle) return Dispatcher'Class;
   
   procedure Delete (This : in out Set; Addr : Handle);

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
   
   procedure Dispatch (This : Client_Dispatcher);
   
   procedure Finalize (This : in out Client_Dispatcher);
   
   function To_Handle (This : Client_Dispatcher) return Handle;
   
   --------------
   -- Services --
   --------------
   
   type Service_Dispatcher is new Dispatcher with record
      Service  : Services.Impl.C_Service;
      Callback : Services.Callback;
      Support  : ROSIDL.Typesupport.Service_Support;
   end record;
   
   procedure Dispatch (This : Service_Dispatcher);

   procedure Finalize (This : in out Service_Dispatcher);
   
   function To_Handle (This : Service_Dispatcher) return Handle;   
   
   -------------------
   -- Subscriptions --
   -------------------
   
   type Subscription_Dispatcher is new Dispatcher with record
      Subscription : aliased Subscriptions.C_Subscription;
      Callback     :         Subscriptions.Callback;
      Support      :         ROSIDL.Typesupport.Message_Support;
   end record;
   
   procedure Dispatch (This : Subscription_Dispatcher);

   procedure Finalize (This : in out Subscription_Dispatcher);
   
   function To_Handle (This : Subscription_Dispatcher) return Handle;
   
   ------------
   -- Timers --
   ------------
   
   type Timer_Dispatcher is new Dispatcher with record
      Timer     : aliased Timers.Timer_Id;
      Callback  :         Timers.Callback;
   end record;
   
   function "=" (L, R : Timer_Dispatcher) return Boolean;
   
   procedure Dispatch (This : Timer_Dispatcher);
   
   procedure Finalize (This : in out Timer_Dispatcher);
   
   function To_Handle (This : Timer_Dispatcher) return Handle;
   
private
   
   use all type Timers.Timer_Id;
   
   function "+" (Addr : System.Address) return Handle is (Handle (Addr));
   
   function "=" (L, R : Timer_Dispatcher) return Boolean is
     (L.Timer = R.Timer);
   
   function To_Handle (This : Client_Dispatcher) return Handle is
     (+This.Client.To_Unique_Addr);

   function To_Handle (This : Service_Dispatcher) return Handle is
     (+This.Service.To_Unique_Addr);
   
   function To_Handle (This : Subscription_Dispatcher) return Handle is
     (+This.Subscription.To_Unique_Addr);
   
   function To_Handle (This : Timer_Dispatcher) return Handle is
     (+Timers.To_Unique_Addr (This.Timer));
   
end RCL.Dispatchers;
