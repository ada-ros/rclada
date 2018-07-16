with RCL.Clients;
limited with RCL.Nodes;
with RCL.Services.Impl;
with RCL.Subscriptions;
with RCL.Timers;

with Rmw_Types_H; use Rmw_Types_H;

with ROSIDL.Impl;
with ROSIDL.Typesupport;

package RCL.Impl.Callbacks is

   --  Needed privately by Executors and Dispatchers.
   --  Not needed by users at all

   type Callback (Node : not null access Nodes.Node'Class) is abstract tagged null record;

   procedure Call (This : Callback) is abstract;

   ---------------------------------------------
   --  Storable callbacks for multicore dispatch

   type Client_Callback is new Callback with record
      User_Callback : Clients.Callback;
      Response      : ROSIDL.Impl.Message_Holder;
   end record;
   overriding procedure Call (This : Client_Callback);


   type Service_Callback is new Callback with record
      User_Callback : Services.Callback;
      Request       : ROSIDL.Impl.Message_Holder;
      Header        : Rmw_Request_Id_T;
      Service       : Services.Impl.C_Service;
      Support       : ROSIDL.Typesupport.Service_Support;
   end record;
   overriding procedure Call (This : Service_Callback);


   type Subscription_Callback is new Callback with record
      User_Callback : Subscriptions.Callback;
      Message       : ROSIDL.Impl.Message_Holder;
      Info          : ROSIDL.Message_Info;
   end record;
   overriding procedure Call (This : Subscription_Callback);


   type Timer_Callback (Node : not null access Nodes.Node'Class) is new Callback (Node) with record
      User_Callback : Timers.Callback;
      Timer         : Timers.Timer (Node);
   end record;
   overriding procedure Call (This : Timer_Callback);

   type Kinds is (Invalid, Client, Service, Subscription, Timer);

   --  Following klunk is to avoid the use of dynamic allocation

   type Node_Ptr is access all Nodes.Node'Class with Storage_Size => 0;

   type Definite_Callback (Kind : Kinds := Invalid;
                           Node : Node_Ptr := null) is record
      case Kind is
         when Invalid      => null;
         when Client       => Client       : Client_Callback (Node);
         when Service      => Service      : Service_Callback (Node);
         when Subscription => Subscription : Subscription_Callback (Node);
         when Timer        => Timer        : Timer_Callback (Node);
      end case;
   end record;

   procedure Call (This : Definite_Callback);

   function To_Definite (This : Callback'Class;
                         Node : Node_Ptr)
                         return Definite_Callback;

private

   function To_Definite (This : Callback'Class;
                         Node : Node_Ptr)
                         return Definite_Callback is
     (if    This in Client_Callback       then (Client,       Node, Client_Callback (This))
      elsif This in Service_Callback      then (Service,      Node, Service_Callback (This))
      elsif This in Subscription_Callback then (Subscription, Node, Subscription_Callback (This))
      elsif This in Timer_Callback        then (Timer,        Node, Timer_Callback (This))
      else raise Program_Error with "Unknown callback");

end RCL.Impl.Callbacks;
