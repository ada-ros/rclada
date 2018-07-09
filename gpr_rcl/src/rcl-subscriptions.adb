with Ada.Unchecked_Conversion;

with C_Strings; use C_Strings;

with RCL.Nodes;

with Rcl_Types_H; use Rcl_Types_H;
with Rmw_Types_H; use Rmw_Types_H;
with Rosidl_Generator_C_Message_Type_Support_Struct_H; use Rosidl_Generator_C_Message_Type_Support_Struct_H;

package body RCL.Subscriptions is

   ----------
   -- Init --
   ----------

   function Init (Node     : in out Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return Subscription
   is
      Opts : aliased constant Rcl_Subscription_Options_T :=
               Rcl_Subscription_Get_default_Options;

      type Ptr is access constant Rosidl_Message_Type_Support_T;
      function To_Ptr is new
        Ada.Unchecked_Conversion (ROSIDL.Typesupport.Msg_Support_Handle,
                                  Ptr);
   begin
      return Sub : Subscription do
         Sub.Node := Node.To_C.Ptr;

         Check (Rcl_Subscription_Init
                  (Sub.Impl.C'Access,
                   Sub.Node.Impl'Access,
                   To_Ptr (Msg_Type.To_C),
                   To_C (Topic).To_Ptr,
                   Opts'Access));
      end return;
   end Init;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out C_Subscription; Node : in out Nodes.C_Node) is
   begin
      Check (Rcl_Subscription_Fini (This.C'Access, Node.Impl'Access));
   end Finalize;

   --------------
   -- Take_Raw --
   --------------

   function Take_Raw (This   : aliased in out C_Subscription;
                      Buffer :                System.Address;
                      Info   :            out ROSIDL.Message_Info)
                      return                  Boolean
   is
      Impl_Info : aliased Rmw_Message_Info_T;
      Ret       : constant Rcl_Ret_T := Rcl_Take (This.C'Access,
                                                  Buffer,
                                                  Impl_Info'Access);
   begin
      if Ret = RMW_RET_OK then
         Info.Intra_Process := To_Boolean (Impl_Info.From_Intra_Process);
         return True;
      elsif Ret = RCL_RET_SUBSCRIPTION_TAKE_FAILED then
         return False;
      else
         raise Program_Error with "Take_Raw:" & Ret'Img;
      end if;
   end Take_Raw;

   --------------
   -- Take_Raw --
   --------------

   function Take_Raw (This   : in out Subscription;
                      Buffer :        System.Address;
                      Info   :    out ROSIDL.Message_Info)
                      return          Boolean is
      (Take_Raw (This.Impl, Buffer, Info));

end RCL.Subscriptions;
