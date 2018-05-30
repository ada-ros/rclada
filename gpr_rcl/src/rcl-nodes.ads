with Ada.Containers.Vectors;
with Ada.Finalization;

with RCL.Callbacks;
with RCL.Subscriptions;
with RCL.Timers;

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
                        Callback :        Subscriptions.Callback);
   
   ---------------
   -- Timer_Add -- et other timer functions
   ---------------

   function Timer_Add (This     : in out Node;
                       Period   :        Duration;
                       Callback :        Timers.Callback)
                       return            Timers.Timer_Id;
   
   procedure Timer_Add (This     : in out Node;
                        Period   :        Duration;
                        Callback :        Timers.Callback);
   
   procedure Timer_Cancel (This  : in out Node;
                           Timer :        Timers.Timer_Id);
   --  Disables (but not deletes) a timer
   
   procedure Timer_Delete (This  : in out Node;
                           Timer :        Timers.Timer_Id);
   --  Entirely removes a timer.
   
   function Timer_Exists (This  : Node; 
                          Timer : Timers.Timer_Id) return Boolean;
   
   procedure Timer_Reset (This  : in out Node;
                          Timer :        Timers.Timer_Id);
   --  Resets the elapsed time (if enabled), or reenables (if cancelled) a timer
   
   -------------------------------------------------
   --  Low level access not intended for clients  --
   
   type Reference (Ptr : access Rcl_Node_T) is null record
     with Implicit_Dereference => Ptr;
   
   function To_C (This : aliased in out Node) return Reference;
   
private   
   
   use Ada.Containers;   
   use Callbacks;
   
   package Sub_Vectors is new Vectors (Positive, 
                                       Callbacks.Subscription_Dispatcher);
   
   package Timer_Vectors is new Vectors (Positive,
                                         Callbacks.Timer_Dispatcher);
   
   type Timer_Vector is new Timer_Vectors.Vector with null record;
   
   procedure Delete_If_Existing (V     : in out Timer_Vector;
                                 Timer : Timers.Timer_Id);
   
   type Node is new Ada.Finalization.Limited_Controlled with record 
      Impl          : aliased Rcl_Node_T := Rcl_Get_Zero_Initialized_Node;
      Subscriptions :         Sub_Vectors.Vector;
      Timers        :         Timer_Vector;
      Self          :  access Node := Node'Unchecked_Access;
   end record;   
   
   procedure Timer_Assert (This  : Node;
                           Timer : Timers.Timer_Id);
   
   function To_C (This : aliased in out Node) return Reference is
     (Ptr => This.Impl'Access);
   
   Default_Options : constant Options := (null record);

end RCL.Nodes;
