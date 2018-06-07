with Ada.Containers.Vectors;
with Ada.Finalization;

with RCL.Callbacks;
with RCL.Clients;
with RCL.Publishers;
limited with RCL.Subscriptions;
with Rcl.Services;
with RCL.Timers;

with Rcl_Node_H;         use Rcl_Node_H;

with ROSIDL.Dynamic;
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
                   Once   :        Boolean  := False;
                   During :        Duration := 0.1);
   --  Check blocking events and dispatch to callbacks for at least During seconds
   --  If Once, it will return early immediately after processing one event
   
   -----------------------
   --  ROS2 FACILITIES  --
   -----------------------
   
   -----------------
   -- Client_Call --
   -----------------
   
   procedure Client_Call (This     : in out Node;
                          Support  :        ROSIDL.Typesupport.Service_Support;
                          Name     :        String;
                          Request  :        ROSIDL.Dynamic.Message;
                          Callback :        Clients.Callback;
                          Timeout  :        Duration := 0.0);
   --  If Timeout > 0.0 the call will block for as much time
   --    or raise RCL_Timeout.
   --    In either case the callback won't be called after the call returns.
   --    It's thus safe to use a local callback for blocking calls
   --    The node will be spun internally so calls to other unrelated callbacks
   --       might happen nonetheless.
   --  THIS IS INTENDED TO BE USED BY A SINGLE CLIENT SIMULTANEOUSLY AT MOST
   --  In other words, no concurrent calls from several threads.
   
   -------------
   -- Publish --
   -------------

   function Publish (This     : in out Node;
                     Msg_Type :        ROSIDL.Typesupport.Message_Support;
                     Topic    :        String)
                     return            Publishers.Publisher;
   
   -----------
   -- Serve --
   -----------

   procedure Serve (This     : in out Node;
                    Support  :        ROSIDL.Typesupport.Service_Support;
                    Name     :        String;
                    Callback :        Services.Callback);
   
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
   
   type C_Node is record 
      Impl : aliased Rcl_Node_T;
   end record;
   --  Not directly the access because of the subtypes-blind bug
   
   type Reference (Ptr : access C_Node) is null record
     with Implicit_Dereference => Ptr;
   
   function To_C (This : aliased in out Node) return Reference;
   
private   
   
   use Ada.Containers;   
   use Callbacks;
   
   package Client_Vectors is new Vectors (Positive,
                                          Callbacks.Client_Dispatcher);
   
   package Srv_Vectors is new Vectors (Positive,
                                       Callbacks.Service_Dispatcher);
   
   package Sub_Vectors is new Vectors (Positive, 
                                       Callbacks.Subscription_Dispatcher);
   
   package Timer_Vectors is new Vectors (Positive,
                                         Callbacks.Timer_Dispatcher);
   
   type Timer_Vector is new Timer_Vectors.Vector with null record;
   
   procedure Delete_If_Existing (V     : in out Timer_Vector;
                                 Timer : Timers.Timer_Id);
   
   type Node is new Ada.Finalization.Limited_Controlled with record 
      Impl          : aliased C_Node := (Impl => Rcl_Get_Zero_Initialized_Node);
      Self          :  access Node   := Node'Unchecked_Access;
      
      Clients       :         Client_Vectors.Vector;
      Services      :         Srv_Vectors.Vector;
      Subscriptions :         Sub_Vectors.Vector;
      Timers        :         Timer_Vector;
                  
      Block_Success : Boolean;
      --  For the client blocking call
      --  Set to true by Spin once the client response is received
   end record;   
   
   procedure Client_Free (This : in out Node;
                          Pos  : Positive);
   
   procedure Timer_Assert (This  : Node;
                           Timer : Timers.Timer_Id);
   
   function To_C (This : aliased in out Node) return Reference is
     (Ptr => This.Impl'Access);
   
   Default_Options : constant Options := (null record);

end RCL.Nodes;
