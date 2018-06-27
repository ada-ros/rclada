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


   type Timer_Callback is new Callback with record
      User_Callback : Timers.Callback;
      Timer         : Timers.Timer_Id;
   end record;
   overriding procedure Call (This : Timer_Callback);

end RCL.Impl.Callbacks;
