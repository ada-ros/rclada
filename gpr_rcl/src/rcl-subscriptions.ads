with Ada.Finalization;

limited with RCL.Nodes;

with Rcl_Node_H;         use Rcl_Node_H;
with Rcl_Subscription_H; use Rcl_Subscription_H; 

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

with System;

package RCL.Subscriptions is
   
   type Callback is access procedure (Msg  : in out ROSIDL.Dynamic.Message;
                                      Info :        ROSIDL.Message_Info);
   
   --  Not really intended to be used by clients either  --
   --  See Node.Subscribe instead                        --

   type Subscription (<>) is new Ada.Finalization.Limited_Controlled with private;
   type C_Subscription is record
      C : aliased Rcl_Subscription_T;  
   end record;
   --  This should be a subtype but there's a bug in GNAT 2017
   
   function Init (Node     : in out Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return Subscription;
   --  TODO: options
   
   procedure Detach (This : in out Subscription);
   --  Forget the internal C subscription, so it is not finished on this
   --    object finalization
   --  Somebody else should take care of freeing the C subscription!
   
   function Take_Raw (This   : aliased in out C_Subscription;
                      Buffer :                System.Address;
                      Info   :            out ROSIDL.Message_Info)
                      return                  Boolean;
   
   function Take_Raw (This   : in out Subscription;
                      Buffer :        System.Address;
                      Info   :    out ROSIDL.Message_Info)
                      return          Boolean;
   --  Raw take, not really intended to be used by clients
   --  There's no way of knowing the necessary buffer size, nor  
   --    how many bytes were taken
   --  TRUE if a message was available.
   
   overriding procedure Finalize (This : in out Subscription);
   
   function To_C (This : Subscription) return C_Subscription;
   
private 
   
   type Subscription is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased C_subscription := (C => Rcl_Get_Zero_Initialized_Subscription);
      Node : aliased Rcl_Node_T;
   end record;

end RCL.Subscriptions;
