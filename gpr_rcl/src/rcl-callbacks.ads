with Ada.Containers.Indefinite_Ordered_Sets;
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
   
   subtype Handle is System.Address;
   --  This address uniquely designates a callback, by the implementation pointer
   --    of the C client data type. This will be hidden in the near future.
   
   type Dispatcher (Node : not null access Nodes.Node'Class) is abstract tagged null record;
   
   procedure Dispatch (This : in out Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc
   
   function To_Ptr (This : in out Dispatcher) return System.Address is abstract;
   --  This is the pointer to the C object that is expected by the
   --    rcl_wait_add_* functions. It also serves to locate the dispatcher
   --    once it has been triggered on the C side.
   
   function "<" (L, R : Dispatcher'Class) return Boolean;
   
   use all type System.Address;
   
   package Callback_Sets is new Ada.Containers.Indefinite_Ordered_Sets
     (Dispatcher'Class);
   
   type Set is new Callback_Sets.Set with null record with  
     Variable_Indexing => Get;
   
   function Num_Clients       (This : Set) return Natural;
   function Num_Services      (This : Set) return Natural;
   function Num_Subscriptions (This : Set) return Natural;
   function Num_Timers        (This : Set) return Natural;
   
   function Contains (This : Set; Addr : System.Address) return Boolean;
   
   type Reference (Element : not null access Dispatcher'Class) is null record with
     Implicit_Dereference => Element;
   
   function Get (This : in out Set; Addr : System.Address) 
                 return Reference;
   
   procedure Delete (This : in out Set; Addr : System.Address);

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
   
   type Client_Reference (Element : not null access Client_Dispatcher'Class) is null record with
     Implicit_Dereference => Element;
   
   function Get_Client (This : in out Set; Addr : System.Address) return Client_Reference;
   
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
   
   function Get_Client (This : in out Set; Addr : System.Address) return Client_Reference is
      (Client_Reference'(Element => Client_Dispatcher'Class (This.Get (Addr).Element.all)'Access));
   
   function "=" (L, R : Timer_Dispatcher) return Boolean is
     (L.Timer = R.Timer);
   
   function To_Ptr (This : in out Client_Dispatcher) return System.Address is
     (This.Client.To_Unique_Addr);

   function To_Ptr (This : in out Service_Dispatcher) return System.Address is
     (This.Service.To_Unique_Addr);
   
   function To_Ptr (This : in out Subscription_Dispatcher) return System.Address is
     (This.Subscription.To_Unique_Addr);
   
   function To_Ptr (This : in out Timer_Dispatcher) return System.Address is
     (Timers.To_Unique_Addr (This.Timer));
   
end RCL.Callbacks;
