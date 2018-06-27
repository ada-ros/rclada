with Rcl_Client_H;  use Rcl_Client_H;
with Rcl_Service_H; use Rcl_Service_H;

with Rmw_Types_H; use Rmw_Types_H;

with RCL.Impl.Callbacks;
with RCL.Nodes; pragma Unreferenced (RCL.Nodes);

with ROSIDL.Dynamic;

package body RCL.Dispatchers is

   use Impl;

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

   procedure Dispatch (This : Client_Dispatcher) is
      use all type Clients.Callback;
      Header : aliased Rmw_Request_Id_T;
   begin
      This.Node.Client_Success (This.To_Ptr);

      Check
        (Rcl_Take_Response
           (This.Client.To_C,
            Header'Access,
            This.Response.Element.Msg.To_Ptr));

      if not This.Blocking and then This.Callback /= null then
         This.Node.Current_Executor.Call
           (Callbacks.Client_Callback'(Node => This.Node,
                                       User_Callback => This.Callback,
                                       Response      => This.Response));
      end if;

      if not This.Blocking then -- The client is not needed any longer
         This.Node.Client_Free (This.To_Ptr);
      end if;
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : Service_Dispatcher) is
      Request  : constant ROSIDL.Dynamic.Shared_Message :=
                   ROSIDL.Dynamic.Init_Shared (This.Support.Request_Support);

      Header   : aliased Rmw_Request_Id_T;
   begin
      Check
        (Rcl_Take_Request
           (This.Service.To_C,
            Header'Access,
            Request.To_Ptr));

      This.Node.Current_Executor.Call
        (Callbacks.Service_Callback'(Node          => This.Node,
                                     User_Callback => This.Callback,
                                     Request       => ROSIDL.Impl.To_Holder (Request),
                                     Header        => Header,
                                     Service       => This.Service,
                                     Support       => This.Support));
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : Subscription_Dispatcher) is
      Msg  : constant ROSIDL.Dynamic.Shared_Message := ROSIDL.Dynamic.Init_Shared (This.Support);
        -- (This.Support.Message_Class.Package_Name, This.Support.Message_Class.Message_Name);
      Info : ROSIDL.Message_Info;
      Sub  : Subscriptions.C_Subscription := This.Subscription;
   begin
      if Subscriptions.Take_Raw (Sub,
                                 Msg.To_Ptr,
                                 Info)
      then
         This.Node.Current_Executor.Call
           (Callbacks.Subscription_Callback'(Node          => This.Node,
                                             User_Callback => This.Callback,
                                             Message       => ROSIDL.Impl.To_Holder (Msg),
                                             Info          => Info));
      else
         raise Program_Error with "Subscription dispatcher: Take_Raw failed when message was expected";
      end if;
   end Dispatch;

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This : Timer_Dispatcher) is
   begin
      This.Node.Current_Executor.Call
        (Callbacks.Timer_Callback'(Node          => This.Node,
                                   User_Callback => This.Callback,
                                   Timer         => This.Timer));
   end Dispatch;

   ----------------
   -- Force_Addr --
   ----------------

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

   function Get (This : Set; Addr : System.Address) return Dispatcher'Class is
     (Keys.Element (Callback_Sets.Set (This), Addr));

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

end RCL.Dispatchers ;
