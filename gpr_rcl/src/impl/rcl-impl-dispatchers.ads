limited with RCL.Executors;
limited with RCL.Nodes;

private with Rcl_Node_H;

with RCL.Clients.Impl;
with RCL.Services.Impl;
with RCL.Subscriptions.Impl;
with RCL.Timers.Impl;

with ROSIDL.Impl;
with ROSIDL.Typesupport;

with System;

package RCL.Impl.Dispatchers is

   --  Helper types to couple an element with its dispatcher, and dispatch calls
   --  Used only privately by the Node implementation
   
   type Handle is new System.Address;
   --  This address uniquely designates a callback, by the implementation pointer
   --    of the C client data type.
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
   --  This is the unique pointer to the C implementation inside each C object.
   --  NOTE: IT IS NOT the address of the C struct, but e.g. rcl_timer_t.impl.
   --    It also serves to locate the dispatcher
   --    once it has been triggered on the C side.
   
   function "<" (L, R : Dispatcher'Class) return Boolean;
   
   use all type System.Address;  

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
      Subscription : aliased Subscriptions.Impl.C_Subscription;
      Callback     :         Subscriptions.Callback;
      Support      :         ROSIDL.Typesupport.Message_Support;
   end record;
   
   procedure Dispatch (This : Subscription_Dispatcher);

   procedure Finalize (This : in out Subscription_Dispatcher);
   
   function To_Handle (This : Subscription_Dispatcher) return Handle;
   
   ------------
   -- Timers --
   ------------
   
   type Timer_Dispatcher (Node : not null access Nodes.Node'Class) is new Dispatcher (Node) with record
      Timer     : aliased Timers.Timer (Node);
      Callback  :         Timers.Callback;
   end record;
   
   procedure Dispatch (This : Timer_Dispatcher);
   
   procedure Finalize (This : in out Timer_Dispatcher);
   
   function To_Handle (This : Timer_Dispatcher) return Handle;
   
   --  Horrible klunkyness to avoid dynamic allocation while preserving
   --  finalization. Bounded_Holders would have been ideal but it obviously
   --  doesn't work with Controlled components or types
   --  Basically we're manually dispatching, so this could have been the
   --  whole dispatcher from the beginning. On the bright side, this way
   --  dispatcher implementations are a bit more isolated.
   
   type Node_Ptr is access all Nodes.Node'Class with Storage_Size => 0;
   
   type Kinds is (Invalid, Client, Service, Subscription, Timer);
   
   type Definite_Dispatcher (Kind : Kinds    := Invalid; 
                             Node : Node_Ptr := null) is 
      record
         case Kind is 
            when Client       => Client       : aliased Client_Dispatcher (Node);
            when Service      => Service      : aliased Service_Dispatcher (Node);
            when Subscription => Subscription : aliased Subscription_Dispatcher (Node);
            when Timer        => Timer        : aliased Timer_Dispatcher (Node);
            when Invalid      => null;
         end case;
      end record;
   
   function To_Definite (This : Dispatcher'Class;
                         Node : Node_Ptr) return Definite_Dispatcher;
   
   procedure Dispatch (This : Definite_Dispatcher);
   procedure Finalize (This : in out Definite_Dispatcher);
   function To_Handle (This : Definite_Dispatcher) return Handle;
   function Reference (This : aliased in out Definite_Dispatcher) return access Dispatcher'Class;         
   
private      
   
   use Rcl_Node_H;
   
   use all type Timers.Timer;
   
   function "+" (Addr : System.Address) return Handle is (Handle (Addr));  
   
   function C_Node (This : Dispatcher'Class) return access Rcl_Node_T;
   
   function Current_Executor (This : Dispatcher'Class) return Executors.Handle;
   
   function To_Handle (This : Client_Dispatcher) return Handle is
     (+This.Client.To_Unique_Addr);

   function To_Handle (This : Service_Dispatcher) return Handle is
     (+This.Service.To_Unique_Addr);
   
   function To_Handle (This : Subscription_Dispatcher) return Handle is
     (+This.Subscription.To_Unique_Addr);
   
   function To_Handle (This : Timer_Dispatcher) return Handle is
     (+Timers.Impl.To_Unique_Addr (This.Timer));
   
end RCL.Impl.Dispatchers;
