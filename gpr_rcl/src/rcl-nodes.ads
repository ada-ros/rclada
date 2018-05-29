with Ada.Finalization;

limited with Rcl.Subscriptions;

with Rcl_Node_H; use Rcl_Node_H;
with Rcl_Wait_H; use Rcl_Wait_H;

with ROSIDL.Dynamic;

package RCL.Nodes is

   type Node (<>) is new Ada.Finalization.Limited_Controlled with private;
   
   type Options is null record;
   --  TBD, nothing really critical in there right now
   
   Default_Options : constant Options;
   
   ----------
   -- Init --
   ----------

   function Init (Name      : String; 
                  Namespace : String  := "/";
                  Opt       : Options := Default_Options) return Node
     with Pre => Name'Length > 0 and then Namespace'Length >= 0;
   
   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node);
   --  Can be called prematurely to shut down a node   
   
   ---------------
   -- Subscribe --
   ---------------

   function Subscribe (This     : in out Node;                       
                       Msg_Type :        ROSIDL.Typesupport.Msg_Support_Ptr;
                       Topic    :        String;
                       Callback : access procedure (Msg : ROSIDL.Dynamic.Message))
                       return Subscriptions.Subscription;
   
   -------------------------------------------------
   --  Low level access not intended for clients  --
   
   type Reference (Ptr : access Rcl_Node_T) is null record
     with Implicit_Dereference => Ptr;
   
   function To_C (This : aliased in out Node) return Reference;
   
private   
   
   type Node is new Ada.Finalization.Limited_Controlled with record 
      Impl : aliased Rcl_Node_T     := Rcl_Get_Zero_Initialized_Node;
      Wait : aliased Rcl_Wait_Set_T := Rcl_Get_Zero_Initialized_Wait_Set;
   end record;
   
   procedure Add_Subscription (This : in out Node; 
                               Sub  :        Subscriptions.Subscription);
   
   function To_C (This : aliased in out Node) return Reference is
     (Ptr => This.Impl'Access);
   
   Default_Options : constant Options := (null record);

end RCL.Nodes;
