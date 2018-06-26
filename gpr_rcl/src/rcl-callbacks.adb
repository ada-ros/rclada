with Rcl_Client_H;  use Rcl_Client_H;
with Rcl_Service_H; use Rcl_Service_H;
with Rcl_Timer_H;   use Rcl_Timer_H;
with Rcl_Types_H;   use Rcl_Types_H;

with Rmw_Types_H; use Rmw_Types_H;

with RCL.Logging;
with RCL.Nodes; pragma Unreferenced (RCL.Nodes);

with ROSIDL.Dynamic;

package body RCL.Callbacks is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Dispatcher'Class) return Boolean is
      L2 : constant access Dispatcher'Class := L'Unrestricted_Access;
      R2 : constant access Dispatcher'Class := R'Unrestricted_Access;
      --  Should be safe as those are by ref (but yikes)
   begin
      return L2.To_Ptr < R2.To_Ptr;
   end "<";

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Client_Dispatcher) is
      use all type Clients.Callback;
      Header : aliased Rmw_Request_Id_T;
   begin
      This.Success := True;

      Check
        (Rcl_Take_Response
           (This.Client.To_C,
            Header'Access,
            This.Response.Element.Msg.To_Ptr));

      if not This.Blocking and then This.Callback /= null then
         This.Callback (This.Node.all, This.Response.Element);
      end if;

      if not This.Blocking then -- The client is not needed any longer
         This.Node.Client_Free (This.To_Ptr);
      end if;
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Service_Dispatcher) is
      Request  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (This.Support.Request_Support);
      Response : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (This.Support.Response_Support);

      Header  : aliased Rmw_Request_Id_T;
   begin
      Check
        (Rcl_Take_Request
           (This.Service.To_C,
            Header'Access,
            Request.To_Ptr));

      This.Callback (This.Node.all, Request, Response);

      Check
        (Rcl_Send_Response
           (This.Service.To_C,
            Header'Access,
            Response.To_Ptr));
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Subscription_Dispatcher) is
      Msg  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (This.Support);
        -- (This.Support.Message_Class.Package_Name, This.Support.Message_Class.Message_Name);
      Info : ROSIDL.Message_Info;
   begin
      if Subscriptions.Take_Raw (This.Subscription,
                                 Msg.To_Ptr,
                                 Info)
      then
         This.Callback (This.Node.all, Msg, Info);
      else
         raise Program_Error with "Subscription dispatcher: Take_Raw failed when message was expected";
      end if;
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : in out Timer_Dispatcher) is
      use Ada.Real_Time;
      Temp : Timers.Timer  := Timers.Bind (This.Timer, This.Node.all);
      Now  : constant Time := Clock;
      Ret  : Rcl_Error_Code;
   begin
      Ret := Rcl_Timer_Call (Timers.To_C (This.Timer));
      --  This "snoozes" the C timer

      This.Callback (This.Node.all,
                     Temp, -- temporary timer for the callee
                     To_Duration (Now - This.Last_Call));
      This.Last_Call := Now;

      case Ret is
         when RCL_RET_TIMER_CANCELED =>
            Logging.Debug ("Attempt to call canceled timer");
            -- Happens once after canceling, not important
         when Rmw_Ret_OK =>
            null;
         when others =>
            Check (Ret);
      end case;
   end Dispatch;

   function Force_Addr (This : Dispatcher'Class) return System.Address is
      Ptr : constant access Dispatcher'Class := This'Unrestricted_Access;
   begin
      return Ptr.To_Ptr;
   end Force_Addr;

   package Keys is new Callback_Sets.Generic_Keys (System.Address,
                                                   Force_Addr);

   --------------
   -- Contains --
   --------------

   function Contains (This : Set; Addr : System.Address) return Boolean is
      (Keys.Contains (Callback_Sets.Set (This), Addr));

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Set; Addr : System.Address) is
   begin
      Keys.Delete (Callback_Sets.Set (This), Addr);
   end Delete;

   ---------
   -- Get --
   ---------

   function Get (This : in out Set; Addr : System.Address) return Reference is
     (Element => Keys.Reference_Preserving_Key (Callback_Sets.Set (This), Addr).Element);

   -----------------
   -- Num_Clients --
   -----------------

   function Num_Clients       (This : Set) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D in Client_Dispatcher'Class then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Clients;

   ------------------
   -- Num_Services --
   ------------------

   function Num_Services      (This : Set) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D in Service_Dispatcher'Class then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Services;

   -----------------------
   -- Num_Subscriptions --
   -----------------------

   function Num_Subscriptions (This : Set) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D in Subscription_Dispatcher'Class then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Subscriptions;

   ----------------
   -- Num_Timers --
   ----------------

   function Num_Timers        (This : Set) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D in Timer_Dispatcher'Class then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Timers;

end RCL.Callbacks ;
