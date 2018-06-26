with Ada.Calendar;
with Ada.Exceptions;

with RCL.Allocators;
with RCL.Clients.Impl;
with RCL.Logging;
with RCL.Publishers.Impl;
with RCL.Services.Impl;
with RCL.Utils.Names_And_Types;
with RCL.Utils.String_Arrays;

with Rcl_Client_H;  use Rcl_Client_H;
with Rcl_Graph_H;   use Rcl_Graph_H;
with Rcl_Service_H; use Rcl_Service_H;
with Rcl_Timer_H;   use Rcl_Timer_H;

with ROSIDL.Impl;

package body RCL.Nodes is

   use all type Timers.Timer_Id;

   ---------------
   -- Base_Init --
   ---------------

   procedure Base_Init (This : in out Node'Class) is
   begin
      if This.Executor /= null then
         This.Executor.Add (This);
      else
         Default_Executor.Add (This);
      end if;
   end Base_Init;

   -------------------------
   -- Client_Call_Prepare --
   -------------------------

   procedure Client_Call_Prepare (This     : in out Node;
                                  Support  :        ROSIDL.Typesupport.Service_Support;
                                  Name     :        String;
                                  Request  :        ROSIDL.Dynamic.Message;
                                  Callback :        Clients.Callback;
                                  Blocking :        Boolean;
                                  Timeout  :        ROS2_Duration)
   is
      Client : aliased Rcl_Client_T         := Rcl_Get_Zero_Initialized_Client;
      Opts   : aliased Rcl_Client_Options_T := Rcl_Client_Get_Default_Options;
      Seq    : aliased C.Long;
   begin
      Check
        (Rcl_Client_Init
           (Client'Access,
            This.Impl.Impl'Access,
            Support.To_C,
            C_Strings.To_C (Name).To_Ptr,
            Opts'Access));

      This.Callbacks.Insert (Callbacks.Client_Dispatcher'
                               (Node     => This.Self,
                                Client   => Clients.Impl.To_C_Client (Client),
                                Callback => Callback,
                                Blocking => Blocking,
                                Response => ROSIDL.Impl.Message_Holders.To_Holder
                                  (ROSIDL.Dynamic.Init_Shared (Support.Response_Support)),
                                Success  => False));

      This.Client := Clients.Impl.To_C_Client (Client).To_Unique_Addr;

      declare
         use Ada.Calendar;
         Start     : constant Time    := Clock;
         Available : aliased  CX.Bool := Bool_False;
         Bother    :          Time    := Clock - 1.1;
      begin
         while Clock - Start < Timeout loop
            delay 0.01; -- Really...
            Check (Rcl_Service_Server_Is_Available
                     (This.Impl.Impl'Access,
                      Client'Access,
                      Available'Access));

            exit when To_Boolean (Available);

            if Clock - Bother >= 1.0 then
               Logging.Warn ("Service unavailaible, waiting...");
               Bother := Clock;
            end if;
         end loop;
      end;

      Check
        (Rcl_Send_Request
           (Client'Access,
            Request.To_Ptr,
            Seq'Access));
   end Client_Call_Prepare;

   -----------------
   -- Client_Call --
   -----------------

   function Client_Call (This     : in out Node;
                         Support  :        ROSIDL.Typesupport.Service_Support;
                         Name     :        String;
                         Request  :        ROSIDL.Dynamic.Message;
                         Timeout  :        ROS2_Duration := Forever)
                         return            ROSIDL.Dynamic.Shared_Message
      is
         use Ada.Calendar;
         Start : constant Time := Clock;
   begin
      Client_Call_Prepare (This     => This,
                           Support  => Support,
                           Name     => Name,
                           Request  => Request,
                           Callback => null,
                           Blocking => True,
                           Timeout  => Timeout);

      loop
         This.Spin (Once   => True,
                    During => Timeout - (Clock - Start));
         if This.Current_Client.Success then
            return Resp : constant ROSIDL.Dynamic.Shared_Message :=
              This.Current_Client.Response.Element
            do
               This.Client_Free (This.Client);
            end return;
         elsif Clock - Start >= Timeout then
            This.Client_Free (This.Client);
            raise RCL_Timeout;
         end if;
      end loop;
   end Client_Call;

   -----------------
   -- Client_Call --
   -----------------

   procedure Client_Call (This     : in out Node;
                          Support  :        ROSIDL.Typesupport.Service_Support;
                          Name     :        String;
                          Request  :        ROSIDL.Dynamic.Message;
                          Callback :        Clients.Callback;
                          Timeout  :        ROS2_Duration := 0.0)
   is
   begin
      if Timeout = 0.0 then
         Client_Call_Prepare (This     => This,
                              Support  => Support,
                              Name     => Name,
                              Request  => Request,
                              Callback => Callback,
                              Blocking => False,
                              Timeout  => Forever); -- Only for the connection attempt
      else
         declare
            Response : constant ROSIDL.Dynamic.Shared_Message :=
                         This.Client_Call (Support, Name, Request, Timeout);
         begin
            Callback (This.Self.all, Response.Msg.all);
         end;
      end if;
   end Client_Call;

   -----------------
   -- Client_Free --
   -----------------

   procedure Client_Free (This : in out Node;
                          Ptr  :        System.Address)
   is
      CB : Callbacks.Client_Dispatcher'Class := This.Callbacks.Get_Client (Ptr);
   begin
      Check (Rcl_Client_Fini (CB.Client.To_Var_C, This.Impl.Impl'Access));

      CB.Response.Clear;
      --  This should happen automatically on deletion from the container,
      --    but it doesn't. The Indefinite_Holders where buggy in 2017

      This.Callbacks.Delete (Ptr);
   end Client_Free;

   ----------
   -- Init --
   ----------

   function Init (Name      : String;
                  Namespace : String  := "/";
                  Opt       : Options := Default_Options;
                  Executor  : access Executors.Executor'Class := null) return Node
   is
      pragma Unreferenced (Opt);

      Cname : C_String := To_C (Name);
      Cnms  : C_String := To_C (Namespace);
   begin
      return This : Node (Executor) do
         Check (Rcl_Node_Init
                  (This.Impl.Impl'Access,
                   Cname.To_Ptr,
                   Cnms.To_Ptr,
                   This.Options'Access));
         This.Base_Init;
      end return;
   end Init;

   ----------
   -- Init --
   ----------

   procedure Init (This      : in out Node;
                   Name      : String;
                   Namespace : String  := "/";
                   Opt       : Options := Default_Options) is
      pragma Unreferenced (Opt);
      Cname : C_String := To_C (Name);
      Cnms  : C_String := To_C (Namespace);
   begin
      Check (Rcl_Node_Init
             (This.Impl.Impl'Access,
                Cname.To_Ptr,
                Cnms.To_Ptr,
                This.Options'Access));
      This.Base_Init;
   end Init;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node) is
   begin
      --  TODO: fini clients, services, etc

      if To_Boolean (Rcl_Node_Is_Valid (This.Impl.Impl'Access, null)) then
         if This.Executor /= null then
            This.Executor.Remove (This);
         else
            Default_Executor.Remove (This);
         end if;
         Check (Rcl_Node_Fini (This.Impl.Impl'Access));
      else
         Logging.Warn ("Attempt to finalize already finalized node");
      end if;
   exception
      when E : others =>
         Put_Line ("Exception while finalizing node:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   -------------------
   -- Get_Callbacks --
   -------------------

   procedure Get_Callbacks (This : in out Node; Set : in out Callbacks.Set) is
   begin
      Set.Union (This.Callbacks);
   end Get_Callbacks;

   ----------------------------
   -- Graph_Count_Publishers --
   ----------------------------

   function Graph_Count_Publishers (This : Node; Topic : String) return Natural is
      Count : aliased C.Size_T;
   begin
      Check
        (Rcl_Count_Publishers
           (This.Impl.Impl'Access,
            To_C (Topic).To_Ptr,
            Count'Access));

      return Natural (Count);
   end Graph_Count_Publishers;

   -----------------------------
   -- Graph_Count_Subscribers --
   -----------------------------

   function Graph_Count_Subscribers (This : Node; Topic : String) return Natural is
      Count : aliased C.Size_T;
   begin
      Check
        (Rcl_Count_Subscribers
           (This.Impl.Impl'Access,
            To_C (Topic).To_Ptr,
            Count'Access));

      return Natural (Count);
   end Graph_Count_Subscribers;

   ----------------------
   -- Graph_Node_Names --
   ----------------------

   function Graph_Node_Names (This : Node) return Utils.Node_Name_Vector is
      Arr : aliased Utils.String_Arrays.String_Array;
   begin
      Check
        (Rcl_Get_Node_Names
           (This.Impl.Impl'Access,
            Allocators.Get_Default_Allocator,
            Arr.To_C));

      return V : Utils.Node_Name_Vector do
         for I in 1 .. Arr.Length loop
            V.Append (Arr.Element (I));
         end loop;
      end return;
   end Graph_Node_Names;

   --------------------
   -- Graph_Services --
   --------------------

   function Graph_Services (This : Node) return Utils.Services_And_Types is
      Arr   : aliased Utils.Names_And_Types.Vector;
      Alloc : aliased Allocators.Allocator := Allocators.Get_Default_Allocator;
   begin
      Check
        (rcl_get_service_names_and_types
           (This.Impl.Impl'Access,
            Alloc'Access,
            Arr.To_C));

      return V : Utils.Services_And_Types do
         for I in 1 .. Arr.Length loop
            V.Insert (Arr.Names (I), Arr.Types (I));
         end loop;
      end return;
   end Graph_Services;

   ------------------
   -- Graph_Topics --
   ------------------

   function Graph_Topics (This : Node; Demangle : Boolean := True) return Utils.Topics_And_Types is
      Arr   : aliased Utils.Names_And_Types.Vector;
      Alloc : aliased Allocators.Allocator := Allocators.Get_Default_Allocator;
   begin
      Check
        (rcl_get_topic_names_and_types
           (This.Impl.Impl'Access,
            Alloc'Access,
            (if Demangle then Bool_False else Bool_True), -- Note: in C side is No_Demangle (bool)
            Arr.To_C));

      return V : Utils.Topics_And_Types do
         for I in 1 .. Arr.Length loop
            V.Insert (Arr.Names (I), Arr.Types (I));
         end loop;
      end return;
   end Graph_Topics;

   ----------
   -- Name --
   ----------

   function Name (This : in out Node) return String is
      (C_Strings.Value (Rcl_Node_Get_Name (This.Impl.Impl'Access)));

   -------------
   -- Publish --
   -------------

   function Publish (This     : in out Node;
                     Msg_Type :        ROSIDL.Typesupport.Message_Support;
                     Topic    :        String)
                     return            Publishers.Publisher is
      (Publishers.Impl.Init (This.Self, Msg_Type, Topic));

   -----------
   -- Serve --
   -----------

   procedure Serve (This     : in out Node;
                    Support  :        ROSIDL.Typesupport.Service_Support;
                    Name     :        String;
                    Callback :        Services.Callback)
   is
      Srv        : aliased Rcl_Service_T := Rcl_Get_Zero_Initialized_Service;
      Opts       : aliased constant Rcl_Service_Options_T :=
                     Rcl_Service_Get_Default_Options;
   begin
      Check
        (Rcl_Service_Init
           (Srv'Access,
            This.Impl.Impl'Access,
            Support.To_C,
            C_Strings.To_C (Name).To_Ptr,
            Opts'Access));

      This.Callbacks.Insert (Callbacks.Service_Dispatcher'
                               (Node     => This.Self,
                                Service  => Services.Impl.To_C_Service (Srv),
                                Callback => Callback,
                                Support  => Support));
   end Serve;

   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Node;
                   Once   :        Boolean       := False;
                   During :        ROS2_Duration := 0.1)
   is
      use Ada.Calendar;
      Start : constant Time := Clock;
   begin
      loop
         if This.Callbacks.Is_Empty then
            delay During - (Clock - Start);
         else
            if This.Current_Executor.Spin_Once (During, This.Self) and then Once then
               exit;
            end if;
         end if;

         exit when Clock - Start >= During;
      end loop;
   exception
      when E : others =>
         Logging.Error ("Node.Spin caught: " &
                          Ada.Exceptions.Exception_Information (E));
         raise;
   end Spin;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in out Node;
                        Msg_Type :        ROSIDL.Typesupport.Message_Support;
                        Topic    :        String;
                        Callback :        Subscriptions.Callback)
   is
      Sub : Subscriptions.Subscription :=
              Subscriptions.Init (This, Msg_Type, Topic);
   begin
      This.Callbacks.Insert
        (Subscription_Dispatcher'(This.Self, Sub.To_C, Callback, Msg_Type));
      Sub.Detach;
   end Subscribe;

   ---------------
   -- Timer_Add --
   ---------------

   function Timer_Add (This     : in out Node;
                       Period   :        Duration;
                       Callback :        Timers.Callback)
                       return            Timers.Timer_Id
   is
      Timer : constant Timers.Timer := Timers.Init (Period);
   begin
      This.Callbacks.Insert
        (Timer_Dispatcher'
           (This.Self,
            Timer.Id,
            Callback,
            Last_Call => <>));

      return Timer.Id;
   end Timer_Add;

   ---------------
   -- Timer_Add --
   ---------------

   procedure Timer_Add (This     : in out Node;
                        Period   :        Duration;
                        Callback :        Timers.Callback)
   is
      Id : constant Timers.Timer_Id := This.Timer_Add (Period, Callback)
        with Unreferenced;
   begin
      null;
   end Timer_Add;

   ------------------
   -- Timer_Assert --
   ------------------

   procedure Timer_Assert (This  : Node;
                           Timer : Timers.Timer_Id) is
   begin
      if not This.Timer_Exists (Timer) then
         raise Constraint_Error with "Timer doesn't exist";
      end if;
   end Timer_Assert;

   ------------------
   -- Timer_Cancel --
   ------------------

   procedure Timer_Cancel (This  : in out Node;
                           Timer :        Timers.Timer_Id)
   is
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Cancel (Timers.To_C (Timer)));
   end Timer_Cancel;

   ------------------
   -- Timer_Delete --
   ------------------

   procedure Timer_Delete (This  : in out Node;
                           Timer :        Timers.Timer_Id)
   is
      Tmp : Timers.Timer_Id := Timer;
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Fini (Timers.To_C (Timer)));

      if This.Timer_Exists (Timer) then
         This.Callbacks.Delete (Timers.To_Unique_Addr (Timer));
         Timers.Free (Tmp);
      end if;
   end Timer_Delete;

   -----------------
   -- Timer_Reset --
   -----------------

   procedure Timer_Reset (This  : in out Node;
                          Timer :        Timers.Timer_Id)
   is
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Reset (Timers.To_C (Timer)));
   end Timer_Reset;

   -------------
   -- Trigger --
   -------------

   procedure Trigger (This : in out Node; CB : System.Address) is
   begin
      This.Callbacks (CB).Dispatch;
   end Trigger;

end RCL.Nodes;
