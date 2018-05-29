with RCL.Subscriptions;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

package RCL.Callbacks is

   --  Helper types to couple an element with its callback, and dispatch calls
   
   type Dispatcher is limited interface;
   
   procedure Dispatch (This : in out Dispatcher) is abstract;
   --  Does whatever applies: fetch a message and call the back, etc

   -------------------
   -- Subscriptions --
   -------------------

   type For_Subscription is access procedure (Msg  : in out ROSIDL.Dynamic.Message;
                                              Info :        ROSIDL.Message_Info);
   
   type Subscription_Dispatcher is new Dispatcher with record
      Subscription : aliased Subscriptions.C_Subscription;
      Callback     :         For_Subscription;
      Support      :         ROSIDL.Typesupport.Message_Support;
   end record;
   
   procedure Dispatch (This : in out Subscription_Dispatcher);
   
end RCL.Callbacks;
