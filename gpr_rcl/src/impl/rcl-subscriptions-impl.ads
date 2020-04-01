with Rcl_Node_H;         use Rcl_Node_H;
with Rcl_Subscription_H; use Rcl_Subscription_H;

with ROSIDL.Typesupport;

with System;

package RCL.Subscriptions.Impl is

   type C_Subscription is tagged record
      Impl : aliased Rcl_Subscription_T;
   end record;
   --  This should be a subtype but there's a bug in GNAT 2017

   function Init (Node     : in out Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return C_Subscription;

   procedure Finalize (This : in out C_Subscription; Node : access Rcl_Node_T);

   function Take_Raw (This   : aliased in out C_Subscription;
                      Buffer :                System.Address;
                      Info   :            out ROSIDL.Message_Info)
                      return                  Boolean;

   function To_Unique_Addr (This : C_Subscription) return System.Address;

private

   type Node_Access is access all Rcl_Node_T with Storage_Size => 0;

   function To_Unique_Addr (This : C_Subscription) return System.Address is
      (This.Impl.Impl);


end RCL.Subscriptions.Impl;
