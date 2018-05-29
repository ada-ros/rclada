with Ada.Exceptions;

with RCL.Allocators;
with RCL.Subscriptions;

package body RCL.Nodes is

   Default_Wait_Size : constant := 19; -- No reason

   ----------------------
   -- Add_Subscription --
   ----------------------

   procedure Add_Subscription (This : in out Node;
                               Sub  :        Subscriptions.Subscription) is
   begin
      null;
   end Add_Subscription;

   ----------
   -- Init --
   ----------

   function Init (Name      : String;
                  Namespace : String  := "/";
                  Opt       : Options := Default_Options) return Node
   is
      pragma Unreferenced (Opt);

      Opts  : aliased constant Rcl_Node_Options_T :=
                Rcl_Node_Get_Default_Options;
   begin
      return This : Node do
         Check (Rcl_Node_Init
                  (This.Impl'Access,
                   To_C (Name).To_Ptr,
                   To_C (Namespace).To_Ptr,
                   Opts'Access));

         Check (Rcl_Wait_Set_Init
                (This.Wait'Access,
                   Default_Wait_Size,
                   Default_Wait_Size,
                   Default_Wait_Size,
                   Default_Wait_Size,
                   Default_Wait_Size,
                   Allocators.Get_Default_Allocator));
      end return;
   end Init;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node) is
   begin
      if Correct (Rcl_Node_Is_Valid (This.Impl'Access, null)) then
         Check (Rcl_Node_Fini (This.Impl'Access));
      else
         null;
         --  Log attempt at finalizing finalized node
      end if;
   exception
      when E : others =>
         Put_Line ("Exception while finalizing node:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (This     : in out Node;
                       Msg_Type :        ROSIDL.Typesupport.Msg_Support_Ptr;
                       Topic    :        String;
                       Callback : access procedure (Msg : ROSIDL.Dynamic.Message))
                       return Subscriptions.Subscription
   is
   begin
      return Sub : Subscriptions.Subscription := Subscriptions.Init (This, Msg_Type, Topic) do
         This.Add_Subscription (Sub);
      end return;
   end Subscribe;

end RCL.Nodes;
