with Ada.Calendar;
with Ada.Exceptions;

with RCL.Clients.Impl;
with RCL.Init;
with RCL.Logging;
with RCL.Publishers.Impl;
with RCL.Services.Impl;
with RCL.Subscriptions.Impl;

with Rcl_Client_H;       use Rcl_Client_H;
with Rcl_Graph_H;        use Rcl_Graph_H;
with Rcl_Node_Options_H; use Rcl_Node_Options_H;
with Rcl_Service_H;      use Rcl_Service_H;
with Rcl_Timer_H;        use Rcl_Timer_H;

with ROSIDL.Impl;

package body RCL.Nodes is

   ---------------
   -- Base_Init --
   ---------------

   procedure Base_Init (This : in out Node'Class) is
   begin
      This.Current_Executor.Add (This);
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
            This.Impl'Access,
            Support.To_C,
            C_Strings.To_C (Name).To_Ptr,
            Opts'Access));

      This.Dispatchers.Insert (Impl.Dispatchers.Client_Dispatcher'
                               (Node     => This.Self,
                                Client   => Clients.Impl.To_C_Client (Client),
                                Callback => Callback,
                                Blocking => Blocking,
                                Response => ROSIDL.Impl.Message_Holders.To_Holder
                                  (ROSIDL.Dynamic.Init_Shared (Support.Response_Support)),
                                Success  => False),
                             Is_Blocking_Client => True);

      declare
         use Ada.Calendar;
         Start     : constant Time    := Clock;
         Available : aliased  CX.Bool := Bool_False;
         Bother    :          Time    := Clock - 1.1;
      begin
         while Clock - Start < Timeout loop
            delay 0.01; -- Really...
            Check (Rcl_Service_Server_Is_Available
                     (This.Impl'Access,
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
         declare
            Current_Client : constant Impl.Dispatchers.Client_Dispatcher'Class :=
                               This.Dispatchers.Current_Client;
         begin
            if Current_Client.Success then
               return Resp : constant ROSIDL.Dynamic.Shared_Message :=
                 Current_Client.Response.Element
               do
                  This.Client_Free (Current_Client.To_Handle);
               end return;
            elsif Clock - Start >= Timeout then
               This.Client_Free (Current_Client.To_Handle);
               raise RCL_Timeout;
            end if;
         end;
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
                          Ptr  :        Handle)
   is
      CB : Dispatchers.Client_Dispatcher'Class :=
             Dispatchers.Client_Dispatcher'Class (This.Dispatchers.Get (Ptr));
   begin
      Check (Rcl_Client_Fini (CB.Client.To_Var_C, This.Impl'Access));

      CB.Response.Clear;
      --  This should happen automatically on deletion from the container,
      --    but it doesn't. The Indefinite_Holders where buggy in 2017

      This.Dispatchers.Delete (Ptr);
   end Client_Free;

   ----------
   -- Init --
   ----------

   function Init (Name      : String;
                  Namespace : String  := "/";
                  Options   : Node_Options := Default_Options) return Node
   is
   begin
      return This : Node do
         This.Init (Name, Namespace, Options);
      end return;
   end Init;

   ----------
   -- Init --
   ----------

   procedure Init (This      : in out Node;
                   Name      : String;
                   Namespace : String  := "/";
                   Options   : Node_Options := Default_Options)
   is
   begin
      RCL.Init.Initialize (Options.Allocator, RCL.Init.Dont_Care);

      This.Options     := Options;
      This.C_Options   := To_C (Options);
      This.Allocator   := Options.Allocator;

--  FIXME
--      Check (Rcl_Node_Init
--             (This.Impl'Access,
--                To_C (Name).To_Ptr,
--                To_C (Namespace).To_Ptr,
--                This.C_Options'Access));

      This.Base_Init;
   end Init;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node) is
   begin
      This.Dispatchers.Finalize;

      if TRUE then -- FIXME To_Boolean (Rcl_Node_Is_Valid (This.Impl'Access, null)) then
         This.Current_Executor.Remove (This);
         Check (Rcl_Node_Fini (This.Impl'Access));

         RCL.Init.Finalize;
      else
         Logging.Warn ("Attempt to finalize already finalized node");
      end if;
   exception
      when E : others =>
         Logging.Warn ("Exception while finalizing node:");
         Logging.Warn (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ----------------------------
   -- Graph_Count_Publishers --
   ----------------------------

   function Graph_Count_Publishers (This : Node; Topic : String) return Natural is
      Count : aliased C.Size_T;
   begin
      Check
        (Rcl_Count_Publishers
           (This.Impl'Access,
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
           (This.Impl'Access,
            To_C (Topic).To_Ptr,
            Count'Access));

      return Natural (Count);
   end Graph_Count_Subscribers;

   ----------------------
   -- Graph_Node_Names --
   ----------------------

   function Graph_Node_Names (This : Node) return Utils.String_Arrays.String_Array is

   begin
      return Arr : aliased Utils.String_Arrays.String_Array do
         null; --  FIXME
--         Check
--           (Rcl_Get_Node_Names
--              (This.Impl'Access,
--               Allocators.Impl.To_C (This.Options.Allocator.all),
--               Arr.To_C));
      end return;
   end Graph_Node_Names;

   --------------------
   -- Graph_Services --
   --------------------

   function Graph_Services (This : Node) return Utils.Names_And_Types.Vector is
   begin
      return Arr : aliased Utils.Names_And_Types.Vector do
         Check
           (Rcl_Get_Service_Names_And_Types
              (This.Impl'Access,
               This.C_Allocator.Impl,
               Arr.To_C));
      end return;
   end Graph_Services;

   ------------------
   -- Graph_Topics --
   ------------------

   function Graph_Topics (This     : Node;
                          Demangle : Boolean := True) return Utils.Names_And_Types.Vector is
   begin
      return Arr : aliased Utils.Names_And_Types.Vector do
         Check
           (Rcl_Get_Topic_Names_And_Types
              (This.Impl'Access,
               This.C_Allocator.Impl,
               (if Demangle then Bool_False else Bool_True), -- Note: in C side is No_Demangle (bool)
               Arr.To_C));
      end return;
   end Graph_Topics;

   ----------
   -- Name --
   ----------

   function Name (This : in out Node) return String is
      (C_Strings.Value (Rcl_Node_Get_Name (This.Impl'Access)));

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
            This.Impl'Access,
            Support.To_C,
            C_Strings.To_C (Name).To_Ptr,
            Opts'Access));

      This.Dispatchers.Insert (Impl.Dispatchers.Service_Dispatcher'
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
         if This.Dispatchers.Is_Empty then
            Logging.Warn ("Nothing to spin on [node]: sleeping for" & During'Img & " seconds!");
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
      Sub : constant Subscriptions.Impl.C_Subscription :=
              Subscriptions.Impl.Init (This, Msg_Type, Topic);
   begin
      This.Dispatchers.Insert
        (Subscription_Dispatcher'(This.Self, Sub, Callback, Msg_Type));
   end Subscribe;

   ---------------
   -- Timer_Add --
   ---------------

   function Timer_Add (This     : in out Node;
                       Period   :        Duration;
                       Callback :        Timers.Callback)
                       return            Timers.Timer is
   begin
      return Timer : constant Timers.Timer := Timers.Impl.Init (This'Access, Period, This.Options.Allocator) do
         This.Dispatchers.Insert
           (Timer_Dispatcher'
              (This.Self,
               Timer,
               Callback));
      end return;
   end Timer_Add;

   ---------------
   -- Timer_Add --
   ---------------

   procedure Timer_Add (This     : in out Node;
                        Period   :        Duration;
                        Callback :        Timers.Callback)
   is
      Id : constant Timers.Timer := This.Timer_Add (Period, Callback)
        with Unreferenced;
   begin
      null;
   end Timer_Add;

   ------------------
   -- Timer_Assert --
   ------------------

   procedure Timer_Assert (This  : Node;
                           Timer : Timers.Timer) is
   begin
      if not This.Timer_Exists (Timer) then
         raise Constraint_Error with "Timer doesn't exist";
      end if;
   end Timer_Assert;

   ------------------
   -- Timer_Cancel --
   ------------------

   procedure Timer_Cancel (This  : in out Node;
                           Timer : in out Timers.Timer)
   is
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Cancel (Timers.Impl.To_C_Var (Timer)));
   end Timer_Cancel;

   ------------------
   -- Timer_Delete --
   ------------------

   procedure Timer_Delete (This  : in out Node;
                           Timer : in out Timers.Timer)
   is
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Fini (Timers.Impl.To_C_Var (Timer)));

      if This.Timer_Exists (Timer) then
         This.Dispatchers.Delete (+Timers.Impl.To_Unique_Addr (Timer));
      end if;
   end Timer_Delete;

   -----------------
   -- Timer_Reset --
   -----------------

   procedure Timer_Reset (This  : in out Node;
                          Timer : in out Timers.Timer)
   is
   begin
      This.Timer_Assert (Timer);
      Check (Rcl_Timer_Reset (Timers.Impl.To_C_Var (Timer)));
   end Timer_Reset;

   ----------
   -- To_C --
   ----------

   function To_C (Options : Node_Options) return Rcl_Node_Options_T is
      Defaults : constant Rcl_Node_Options_T := Rcl_Node_Get_Default_Options;
   begin
      return Rcl_Node_Options_T'(Domain_Id            => Defaults.Domain_Id,
                                 allocator            => Allocators.Impl.To_C (Options.Allocator.all),
                                 use_global_arguments => Defaults.use_global_arguments,
                                 Arguments            => Defaults.Arguments);
   end To_C;

   --------------------
   -- Safe_Callbacks --
   --------------------

   protected body Safe_Dispatchers is

      --------------
      -- Contains --
      --------------

      function  Contains (CB : Impl.Dispatchers.Handle) return Boolean is
         (CBs.Contains (CB));

      ------------
      -- Delete --
      ------------

      procedure Delete   (CB : Impl.Dispatchers.Handle) is
      begin
         CBs.Delete (CB);
      end Delete;

      ---------
      -- Get --
      ---------

      function Get (CB : Impl.Dispatchers.Handle) return Dispatcher'Class is
         (Element (CBs.Element (CB)));

      ------------
      -- Insert --
      ------------

      procedure Insert (CB                 : Dispatcher'Class;
                        Is_Blocking_Client : Boolean := False) is
      begin
         CBs.Insert (CB.To_Handle, To_Definite (CB, CB.Node));
         if Is_Blocking_Client then
            Client := CB.To_Handle;
         end if;
      end Insert;

      --------------
      -- Is_Empty --
      --------------

      function  Is_Empty return Boolean is
         (CBs.Is_Empty);

      -----------
      -- Union --
      -----------

      procedure Union (Dst : in out Impl.Dispatchers.Maps.Map) is
      begin
         for CB of CBs loop
            Dst.Insert (Element (CB).To_Handle, CB);
         end loop;
      end Union;

      --------------------
      -- Current_Client --
      --------------------

      function Current_Client return Impl.Dispatchers.Client_Dispatcher'Class is
         (Impl.Dispatchers.Client_Dispatcher'Class (Element (CBs.Element (Client))));

      --------------------
      -- Client_Success --
      --------------------

      procedure Client_Success (Client : Impl.Dispatchers.Handle) is
         Disp : Impl.Dispatchers.Client_Dispatcher'Class :=
                  Impl.Dispatchers.Client_Dispatcher'Class (Get (Client));
      begin
         Disp.Success := True;
         CBs.Include (Disp.To_Handle, To_Definite (Disp, Disp.Node));
      end Client_Success;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         for I in CBs.Iterate loop
            declare
               Disp : Impl.Dispatchers.Dispatcher'Class := Element (CBs (I));
               --  Writable copy
            begin
               Disp.Finalize;
            end;
         end loop;

         CBs.Clear;
      end Finalize;

   end Safe_Dispatchers;

end RCL.Nodes;
