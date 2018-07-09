limited with RCL.Nodes;

with Rcl_Node_H;         use Rcl_Node_H;
with Rcl_Subscription_H; use Rcl_Subscription_H; 

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

with System;

package RCL.Subscriptions is
   
   type Callback is access procedure (Node : in out Nodes.Node'Class;
                                      Msg  : in out ROSIDL.Dynamic.Message;
                                      Info :        ROSIDL.Message_Info);
   
   --  Not really intended to be used by clients either  --
   --  See Node.Subscribe instead                        --

   type Subscription (<>) is tagged limited private;
   type C_Subscription is tagged record
      C : aliased Rcl_Subscription_T;  
   end record;
   --  This should be a subtype but there's a bug in GNAT 2017
   
   function Init (Node     : in out Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return Subscription;
   --  TODO: options
   
   procedure Finalize (This : in out C_Subscription; Node : access Rcl_Node_T);
   
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
   
   function To_C (This : Subscription'Class) return C_Subscription;
   
   function To_Unique_Addr (This : C_Subscription) return System.Address;
   
private 
   
   type Node_Access is access all Rcl_Node_T with Storage_Size => 0;
   
   type Subscription is tagged limited record
      Impl : aliased C_subscription := (C => Rcl_Get_Zero_Initialized_Subscription);
      Node :         Node_Access;
   end record;
      
   function To_C (This : Subscription'Class) return C_Subscription is
      (This.Impl);
   
   function To_Unique_Addr (This : C_Subscription) return System.Address is
      (This.C.Impl);

end RCL.Subscriptions;
