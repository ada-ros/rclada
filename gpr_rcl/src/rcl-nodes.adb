with Ada.Calendar;
with Ada.Containers;
with Ada.Exceptions;

with RCL.Clients.Impl;
with RCL.Logging;
with RCL.Publishers.Impl;
with RCL.Services.Impl;
with RCL.Subscriptions;
with RCL.Wait;

with Rcl_Client_H;  use Rcl_Client_H;
with Rcl_Service_H; use Rcl_Service_H;
with Rcl_Timer_H;   use Rcl_Timer_H;

with ROSIDL.Impl;

package body RCL.Nodes is

   use all type Timers.Timer_Id;

   -------------------------
   -- Client_Call_Prepare --
   -------------------------

   procedure Client_Call_Prepare (This     : in out Node;
                                  Support  :        ROSIDL.Typesupport.Service_Support;
                                  Name     :        String;
                                  Request  :        ROSIDL.Dynamic.Message;
                                  Callback :        Clients.Callback;
                                  Blocking :        Boolean)
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

      This.Clients.Prepend (Callbacks.Client_Dispatcher'
                              (Client   => Clients.Impl.To_C_Client (Client),
                               Callback => Callback,
                               Blocking => Blocking,
                               Response => ROSIDL.Impl.Message_Holders.To_Holder
                                 (ROSIDL.Dynamic.Init_Shared (Support.Response_Support)),
                               Success  => False));

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
   begin
      Client_Call_Prepare (This     => This,
                           Support  => Support,
                           Name     => Name,
                           Request  => Request,
                           Callback => null,
                           Blocking => True);

      declare
         use Ada.Calendar;
         Start : constant Time := Clock;
      begin
         loop
            This.Spin (Once   => True,
                       During => Timeout - (Clock - Start));

            if This.Clients (1).Success then
               return Resp : constant ROSIDL.Dynamic.Shared_Message :=
                               This.Clients (1).Response.Element
               do
                  This.Client_Free (This.Clients.First_Index);
               end return;
            elsif Clock - Start >= Timeout then
               This.Client_Free (This.Clients.First_Index);
               raise RCL_Timeout;
            end if;
         end loop;
      end;
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
                              Blocking => False);
      else
         declare
            Response : constant ROSIDL.Dynamic.Shared_Message :=
                         This.Client_Call (Support, Name, Request, Timeout);
         begin
            Callback (Response.Msg.all);
         end;
      end if;
   end Client_Call;

   -----------------
   -- Client_Free --
   -----------------

   procedure Client_Free (This : in out Node;
                          Pos  : Positive) is
   begin
      Check (Rcl_Client_Fini (This.Clients (Pos).Client.To_C, This.Impl.Impl'Access));

      This.Clients (Pos).Response.Clear;
      --  This should happen automatically on deletion from the container,
      --    but it doesn't. The Indefinite_Holders where buggy in 2017

      This.Clients.Delete (Pos);
   end Client_Free;

   ------------------------
   -- Delete_If_Existing --
   ------------------------

   procedure Delete_If_Existing (V     : in out Timer_Vector;
                                 Timer : Timers.Timer_Id) is
   begin
      for I in V.First_Index .. V.Last_Index loop
         if V (I).Timer = Timer then
            V.Delete (I);
            return;
         end if;
      end loop;
   end Delete_If_Existing;

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

      Cname : C_String := To_C (Name);
      Cnms  : C_String := To_C (Namespace);
   begin
      return This : Node do
         Check (Rcl_Node_Init
                  (This.Impl.Impl'Access,
                   Cname.To_Ptr,
                   Cnms.To_Ptr,
                   Opts'Access));
      end return;
   end Init;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node) is
   begin
      --  TODO: fini clients, services, etc

      if To_Boolean (Rcl_Node_Is_Valid (This.Impl.Impl'Access, null)) then
         Check (Rcl_Node_Fini (This.Impl.Impl'Access));
      else
         Logging.Warn ("Attempt to finalize already finalized node");
      end if;
   exception
      when E : others =>
         Put_Line ("Exception while finalizing node:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   -------------
   -- Publish --
   -------------

   function Publish (This     : in out Node;
                     Msg_Type :        ROSIDL.Typesupport.Message_Support;
                     Topic    :        String)
                     return            Publishers.Publisher is
      (Publishers.Impl.Init (This.Impl'Access, Msg_Type, Topic));

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

      This.Services.Append (Callbacks.Service_Dispatcher'
                              (Service  => Services.Impl.To_C_Service (Srv),
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

      -------------
      -- Process --
      -------------

      procedure Process (T : Wait.Trigger) is
         use all type Wait.Kinds;
      begin
         case T.Kind is
            when Client =>
               This.Clients (T.Index).Dispatch;
               if not This.Clients (T.Index).Blocking then
                  --  If blocking, the returned response is needed yet, and
                  --    will be freed in function Client_Call
                  This.Client_Free (T.Index);
               end if;
            when Service =>
               This.Services (T.Index).Dispatch;
            when Subscription =>
               This.Subscriptions (T.Index).Dispatch;
            when Timer =>
               This.Timers (T.Index).Dispatch;
         end case;
      end Process;

      ---------------
      -- Spin_Once --
      ---------------

      function Spin_Once return Boolean is
         --  True if something was processed

         use all type Wait.Wait_Outcomes;

         Set : Wait.Set := Wait.Init
           (Num_Clients       => Natural (This.Clients.Length),
            Num_Services      => Natural (This.Services.Length),
            Num_Subscriptions => Natural (This.Subscriptions.Length),
            Num_Timers        => Natural (This.Timers.Length));
      begin
         for Cli of This.Clients loop
            Set.Add (Cli.Client);
         end loop;

         for Srv of This.Services loop
            Set.Add (Srv.Service);
         end loop;

         for Sub of This.Subscriptions loop
            Set.Add (Sub.Subscription);
         end loop;

         for Timer of This.Timers loop
            if not Timers.Is_Canceled (Timer.Timer) then
               Set.Add (Timer.Timer);
            end if;
         end loop;

--           Logging.Info ("WAITING:" & ROS2_Duration'Image (During - (Clock - Start)) &
--                           " LONGLAST:" & C.Long'Last'Img &
--                           " EQUIV:" & Long_Long_Integer'(Long_Long_Integer (During - (Clock - Start) * 1_000_000_000.0))'Img);

         case Set.Wait (During - (Clock - Start)) is
            when Error     =>
               raise Program_Error with "Error in Set.Wait";

            when Timeout   =>
               return False;

            when Triggered =>
               for Trigger of Set loop
                  Process (Trigger);
               end loop;
               return True;

         end case;
      end Spin_Once;

   begin
      loop
         if This.Clients.Is_Empty       and then This.Services.Is_Empty and then
            This.Subscriptions.Is_Empty and then This.Timers.Is_Empty
         then
            delay During - (Clock - Start);
         else
            if Spin_Once and then Once then
               exit;
            end if;
         end if;

         exit when Clock - Start >= During;
      end loop;
   exception
      when E : others =>
         Logging.Error ("Node.Spin caught: " &
                          Ada.Exceptions.Exception_Information (E));
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
      This.Subscriptions.Append
        (Subscription_Dispatcher'(Sub.To_C, Callback, Msg_Type));
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
      This.Timers.Append
        (Timer_Dispatcher'
           (Timer.Id,
            Callback,
            Last_Call => <>,
            Node      => This.Self));

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
         This.Timers.Delete_If_Existing (Timer);
         Timers.Free (Tmp);
      end if;
   end Timer_Delete;

   ------------------
   -- Timer_Exists --
   ------------------

   function Timer_Exists (This  : Node;
                          Timer : Timers.Timer_Id) return Boolean is
   begin
      for T of This.Timers loop
         if T.Timer = Timer then
            return True;
         end if;
      end loop;

      return False;
   end Timer_Exists;

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

end RCL.Nodes;
