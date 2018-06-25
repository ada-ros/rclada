with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

private with RCL.Callbacks;

with RCL.Clients;
with RCL.Executors;
with RCL.Executors.Sequential;
with RCL.Publishers;
with RCL.Subscriptions;
with RCL.Services;
with RCL.Timers;
with RCL.Utils;

with Rcl_Node_H;         use Rcl_Node_H;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

package RCL.Nodes is

   Default_Executor : aliased Executors.Sequential.Executor;
   
   type Node (Executor : access Executors.Executor'Class) is 
     new Ada.Finalization.Limited_Controlled with private;
   
   type Options is null record;
   --  TBD, nothing really critical in there right now
   
   Default_Options : constant Options;
   
   ----------
   -- Init --
   ----------
   --  Classes overriding these should call the parent implementation

   function Init (Name      : String; 
                  Namespace : String  := "/";
                  Opt       : Options := Default_Options;
                  Executor  : access Executors.Executor'Class := null) return Node
     with Pre'Class => Name'Length > 0 and then Namespace'Length >= 0;
   
   procedure Init (This      : in out Node;
                   Name      : String; 
                   Namespace : String  := "/";
                   Opt       : Options := Default_Options) with
     Pre'Class => Name'Length > 0 and then Namespace'Length >= 0;
   
   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node);
   --  Can be called prematurely to shut down a node   
   
   ----------
   -- Name --
   ----------

   function Name (This : in out Node) return String;
                         
   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Node; 
                   Once   :        Boolean       := False;
                   During :        ROS2_Duration := 0.1);
   --  Check blocking events and dispatch to callbacks for at least During seconds
   --  If Once, it will return early immediately after processing one event
   
   -----------------------
   --  ROS2 FACILITIES  --
   -----------------------
   
   -----------------
   -- Client_Call --
   -----------------
   
   function Client_Call (This     : in out Node;
                         Support  :        ROSIDL.Typesupport.Service_Support;
                         Name     :        String;
                         Request  :        ROSIDL.Dynamic.Message;
                         Timeout  :        ROS2_Duration := Forever)
                         return            ROSIDL.Dynamic.Shared_Message;
   --  See the documentation on Client_Call below
   
   procedure Client_Call (This     : in out Node;
                          Support  :        ROSIDL.Typesupport.Service_Support;
                          Name     :        String;
                          Request  :        ROSIDL.Dynamic.Message;
                          Callback :        Clients.Callback;
                          Timeout  :        ROS2_Duration := 0.0);
   --  If Timeout > 0.0 the call will block for as much time
   --    or raise RCL_Timeout.
   --    In either case the callback won't be called after the call returns.
   --    It's thus safe to use a local callback for blocking calls
   --    The node will be spun internally so calls to other unrelated callbacks
   --       might happen nonetheless.
   --  BLOCKING concurrency depends on internal concurrency of ROS2.
   --    At this time I'm unsure if this would be advisable.
   --  Note that, even in the non-blocking case, a certain amount of blocking
   --    might happen until the service becomes available to the client.
   
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
   
   ------------------
   --  ROS2 GRAPH  --
   ------------------
   --  Functions returning information about the connection graph
   
   function Graph_Count_Publishers (This : Node; Topic : String) return Natural;
   
   function Graph_Count_Subscribers (This : Node; Topic : String) return Natural;
   
   function Graph_Node_Names (This : Node) return Utils.Node_Name_Vector;
   
   function Graph_Services (This : Node) return Utils.Services_And_Types;
   
   function Graph_Topics (This : Node; Demangle : Boolean := True) return Utils.Topics_And_Types;   
   
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
   
   package Client_Vectors is new Indefinite_Vectors (Positive,
                                                     Callbacks.Client_Dispatcher);
   
   package Srv_Vectors is new Indefinite_Vectors (Positive,
                                                  Callbacks.Service_Dispatcher);
   
   package Sub_Vectors is new Indefinite_Vectors (Positive, 
                                                  Callbacks.Subscription_Dispatcher);
   
   package Timer_Vectors is new Indefinite_Vectors (Positive,
                                                    Callbacks.Timer_Dispatcher);
   
   type Timer_Vector is new Timer_Vectors.Vector with null record;
   
   procedure Delete_If_Existing (V     : in out Timer_Vector;
                                 Timer : Timers.Timer_Id);
   
   type Node (Executor : access Executors.Executor'Class)  is new Ada.Finalization.Limited_Controlled with record 
      Impl          : aliased C_Node := (Impl => Rcl_Get_Zero_Initialized_Node);
      Self          :  access Node   := Node'Unchecked_Access;
      
      Options       : aliased Rcl_Node_Options_T := Rcl_Node_Get_Default_Options; 
      
      Clients       :         Client_Vectors.Vector;
      Services      :         Srv_Vectors.Vector;
      Subscriptions :         Sub_Vectors.Vector;
      Timers        :         Timer_Vector;
   end record;   
   
   function Current_Executor (This : in out Node'Class) return access Executors.Executor'Class is
     (if This.Executor /= null 
      then This.Executor
      else Default_Executor'Access);
   
   procedure Base_Init (This : in out Node'Class);
   
   procedure Client_Free (This : in out Node;
                          Pos  : Positive);
   
   procedure Timer_Assert (This  : Node;
                           Timer : Timers.Timer_Id);
   
   function To_C (This : aliased in out Node) return Reference is
     (Ptr => This.Impl'Access);
   
   Default_Options : constant Options := (null record);

end RCL.Nodes;
