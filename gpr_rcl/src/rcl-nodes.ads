with Ada.Containers.Vectors;
with Ada.Finalization;

with Rcl.Callbacks;

with Rcl_Node_H;         use Rcl_Node_H;

with ROSIDL.Typesupport;

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
   
   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Node; 
                   During :        Duration := 0.1);
   --  Check blocking events and dispatch to callbacks for at least During seconds
   
   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in out Node;                       
                        Msg_Type :        ROSIDL.Typesupport.Message_Support;
                        Topic    :        String;
                        Callback :        Callbacks.For_Subscription);
   
   -------------------------------------------------
   --  Low level access not intended for clients  --
   
   type Reference (Ptr : access Rcl_Node_T) is null record
     with Implicit_Dereference => Ptr;
   
   function To_C (This : aliased in out Node) return Reference;
   
private   
   
   use Callbacks;
   
   package Sub_Vectors is new Ada.Containers.Vectors (Positive, 
                                                      Callbacks.Subscription_Dispatcher);
   
   type Node is new Ada.Finalization.Limited_Controlled with record 
      Impl          : aliased Rcl_Node_T := Rcl_Get_Zero_Initialized_Node;
      Subscriptions :         Sub_Vectors.Vector;
   end record;
   
   function To_C (This : aliased in out Node) return Reference is
     (Ptr => This.Impl'Access);
   
   Default_Options : constant Options := (null record);

end RCL.Nodes;
