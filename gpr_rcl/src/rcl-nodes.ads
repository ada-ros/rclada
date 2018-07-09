with Ada.Finalization;

with RCL.Allocators;
with RCL.Impl.Dispatchers;
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

private with RCL.Allocators.Impl;

package RCL.Nodes is

   Default_Executor : aliased Executors.Sequential.Executor;
   
   type Node is new Ada.Finalization.Limited_Controlled with private;
   --  Use of Node prior to an Init call is erroneous.
   --  Recommended approach is to use Init function to initialize on the spot.
   
   type Node_Options is record
      Allocator : Allocators.Handle  := Allocators.Global_Allocator;
      Executor  : Executors.Handle   := Executors.Handle (No_Executor);
   end record;
   --  TBD, not complete
   
   Default_Options : constant Node_Options;
   
   ----------
   -- Init --
   ----------
   --  Classes overriding init MUST call the parent implementation

   function Init (Name      : String; 
                  Namespace : String  := "/";
                  Options   : Node_Options := Default_Options) return Node
     with Pre'Class => Name'Length > 0 and then Namespace'Length >= 0;
   
   procedure Init (This      : in out Node;
                   Name      : String; 
                   Namespace : String  := "/";
                   Options   : Node_Options := Default_Options) with
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
   
   function Graph_Services (This : in out Node) return Utils.Services_And_Types;
   
   function Graph_Topics (This : in out Node; Demangle : Boolean := True) return Utils.Topics_And_Types;   
   
   -------------------
   --  Misc access  --

   function Allocator (This : Node) return Allocators.Handle;      
   
private   

   use RCL.Impl;
   
   use all type Impl.Dispatchers.Handle;
   
   function To_C (Options : Node_Options) return Rcl_Node_Options_T;
   
   use Impl.Dispatchers;
   
   protected type Safe_Dispatchers (Parent : access Node'Class) is
      
      function  Contains (CB : Impl.Dispatchers.Handle) return Boolean;
      procedure Delete   (CB : Impl.Dispatchers.Handle);
      function  Get      (CB : Impl.Dispatchers.Handle) return Dispatcher'Class;
      procedure Insert (CB                 : Dispatcher'Class; 
                        Is_Blocking_Client : Boolean := False);
      function  Is_Empty return Boolean;
      procedure Union    (Dst : in out Impl.Dispatchers.Set);      
      
      function  Current_Client return Impl.Dispatchers.Client_Dispatcher'Class;
      procedure Client_Success (Client : Impl.Dispatchers.Handle);
      
      procedure Finalize;
      
   private
      CBs       : RCL.Impl.Dispatchers.Set;
      Client    : Handle; -- Client that's blocking and waiting
   end Safe_Dispatchers;
   
   type Node is new Ada.Finalization.Limited_Controlled with record 
      Impl        : aliased Rcl_Node_T := Rcl_Get_Zero_Initialized_Node;
      Self        : access Node    := Node'Unchecked_Access;
      Dispatchers : Safe_Dispatchers (Node'Access);
       
      -- Must be initialized
      Options     : Node_Options;                  
      
      -- Are derived from Options, no need to initialize
      Allocator   : Allocators.Handle := Allocators.Global_Allocator;
      C_Options   : aliased Rcl_Node_Options_T;
   end record;      
   
   procedure Base_Init (This : in out Node'Class);      
   
   function C_Allocator (This : Node) return Allocators.Impl.Allocator_Reference is
     (Allocators.Impl.To_C (This.Allocator.all));
   
   procedure Client_Free (This : in out Node;
                          Ptr  :        Dispatchers.Handle);
   
   procedure Timer_Assert (This  : Node; 
                           Timer : Timers.Timer_Id);
   
   function Timer_Exists (This  : Node; 
                          Timer : Timers.Timer_Id) return Boolean is
      (This.Dispatchers.Contains (+Timers.To_Unique_Addr (Timer)));   
   
   function Allocator (This : Node) return Allocators.Handle is
     (This.Allocator);   
   
   Default_Options  : constant Node_Options := (others => <>);
   
   use all type Executors.Handle;   
   
   function Current_Executor (This : in out Node'Class) return Executors.Handle is
     (if This.Options.Executor /= null 
      then This.Options.Executor
      else Default_Executor'Access);

end RCL.Nodes;
