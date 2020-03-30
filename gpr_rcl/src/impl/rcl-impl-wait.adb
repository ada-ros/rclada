with RCL.Logging;
with RCL.Subscriptions;

with Rcl_Types_H;
with Rmw_Ret_Types_H; use Rmw_Ret_Types_H;

package body RCL.Impl.Wait is

   use all type System.Address;

   function Rclada_Wait_Set_Client_Check (Set : access constant Rcl_Wait_Set_T;
                                          Pos : C.Int)
                                          return System.Address with Import, Convention => C;

   function Rclada_Wait_Set_Service_Check (Set : access constant Rcl_Wait_Set_T;
                                           Pos : C.Int)
                                              return System.Address with Import, Convention => C;

   function Rclada_Wait_Set_Subscription_Check (Set : access constant Rcl_Wait_Set_T;
                                                Pos : C.Int)
                                                   return System.Address with Import, Convention => C;

   function Rclada_Wait_Set_Timer_Check (Set : access constant Rcl_Wait_Set_T;
                                         Pos : C.Int)
                                            return System.Address with Import, Convention => C;

   --  THOSE ARE 0-BASED!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   type Access_Checker is access function (Set : access constant Rcl_Wait_Set_T;
                                           Pos : C.Int)
                                           return System.Address with Convention => C;

   Checkers : constant array (Kinds) of Access_Checker
     := (Client       => Rclada_Wait_Set_Client_Check'Access,
         Service      => Rclada_Wait_Set_Service_Check'Access,
         Subscription => Rclada_Wait_Set_Subscription_Check'Access,
         Timer        => Rclada_Wait_Set_Timer_Check'Access);


   ---------
   -- Add --
   ---------

   procedure Add (This : aliased in out Set;
                  Cli  : aliased in out Clients.Impl.C_Client) is
   begin
      null;
      --  FIXME Check (Rcl_Wait_Set_Add_Client (This.Impl'Access, Cli.To_C));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (This : aliased in out Set;
                  Srv  : aliased in out Services.Impl.C_Service) is
   begin
      null;
      --  FIXME Check (Rcl_Wait_Set_Add_Service (This.Impl'Access, Srv.To_C));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (This : aliased in out Set;
                  Sub  : Aliased in out Subscriptions.Impl.C_Subscription) is
   begin
      null;
      --  FIXME Check (Rcl_Wait_Set_Add_Subscription (This.Impl'Access, Sub.Impl'Access));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (This  : aliased in out Set;
                  Timer : Aliased in out Timers.Timer)
   is
   begin
      null;
      --  FIXME Check (Rcl_Wait_Set_Add_Timer (This.Impl'Access, Timers.Impl.To_C (Timer)));
   end Add;



   -------------
   -- Advance --
   -------------

   function Advance (This : Set;
                     Pos  : Cursor) return Cursor is
   begin
      if Pos.Ended then
         raise Program_Error with "Can't advance past ended cursor";
      else
         if Pos.T.Index < This.Size (Pos.T.Kind) then
            return (Ended => False,
                    T     => (Pos.T.Kind,
                              Pos.T.Index + 1,
                              System.Null_Address)); -- The proper address is filled by Find_Next_Valid
         elsif Pos.T.Kind = Kinds'Last then
            return Ended_Cursor;
         else
            return (Ended  => False,
                    T      => (Kinds'Succ (Pos.T.Kind),
                               1,
                               System.Null_Address)); -- The proper address is filled by Find_Next_Valid
         end if;
      end if;
   end Advance;

   --------------
   -- Get_Addr --
   --------------

   function Get_Addr (This : Set;
                      Kind : Kinds;
                      Pos  : Positive) return System.Address is
     (Checkers (Kind).all (This.Impl'Access, C.Int (Pos) - 1));

   --------------
   -- Is_Ready --
   --------------

   function Is_Ready (This : Set;
                   Kind : Kinds;
                   Pos  : Positive) return Boolean is
     (This.Get_Addr (Kind, Pos) /= System.Null_Address);


   -------------
   -- Element --
   -------------

   function Element (This : Set; Pos : Cursor) return Trigger'Class is (Pos.T);

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Set) is
   begin
      Check (Rcl_Wait_Set_Fini (This.Impl'Access));
   end Finalize;

   ---------------------
   -- Find_Next_Valid --
   ---------------------

   function Find_Next_Valid (This : Set;
                             Pos  : Cursor) return Cursor
   is
      Curr : Cursor := Pos;
   begin
      while not Curr.Ended loop
         if Curr.T.Index <= This.Size (Curr.T.Kind) and then
            This.Is_Ready (Curr.T.Kind, Curr.T.Index)
         then
            return Cursor'(T     => (Kind  => Curr.T.Kind,
                                     Index => Curr.T.Index,
                                     Ptr   => This.Get_Addr (Curr.T.Kind, Curr.T.Index)),
                           Ended => False);
         else
            Curr := This.Advance (Curr);
         end if;
      end loop;

      return Curr;
   end Find_Next_Valid;

   -----------
   -- First --
   -----------

   function First (I : Iterator) return Cursor is
   begin
      return Find_Next_Valid (I.Over.all, (T     => (Kinds'First, 1, System.Null_Address),
                                           Ended => False));
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Cursor) return Boolean is (not C.Ended);

   ----------
   -- Init --
   ----------

   function Init (Allocator         : Allocators.Handle;
                  Context           : aliased in out Contexts.Context;
                  Num_Clients       : Natural := 0;
                  Num_Services      : Natural := 0;
                  Num_Subscriptions : Natural := 0;
                  Num_Timers        : Natural := 0) return Set is
   begin
      if Num_Clients + Num_Services + Num_Subscriptions + Num_Timers = 0 then
         raise Constraint_Error with "Nothing to wait on!";
      end if;

      return S : Set do
         Check
           (Rcl_Wait_Set_Init
              (S.Impl'Access,
               Number_Of_Clients          => C.Size_T (Num_Clients),
               Number_Of_Guard_Conditions => 0,
               Number_Of_Services         => C.Size_T (Num_Services),
               Number_Of_Subscriptions    => C.Size_T (Num_Subscriptions),
               Number_Of_Timers           => C.Size_T (Num_Timers),
               number_of_events           => 0,
               Context                    => Context.To_C,
               Allocator                  => Allocator.To_C.all
              ));
         --  TODO: guards and events are UNIMPLEMENTED
      end return;
   end Init;

   ----------
   -- Init --
   ----------

   function Init (Allocator : Allocators.Handle;
                  Context   : aliased in out Contexts.Context;
                  Callbacks : aliased in out RCL.Impl.Dispatchers.Maps.Map) return Set is
   begin
      return S : Set := Init (Allocator         => Allocator,
                              Context           => Context,
                              Num_Clients       => Callbacks.Num_Clients,
                              Num_Services      => Callbacks.Num_Services,
                              Num_Subscriptions => Callbacks.Num_Subscriptions,
                              Num_Timers        => Callbacks.Num_Timers)
      do
         for CB of Callbacks loop
            declare
               CBR : Impl.Dispatchers.Dispatcher'Class renames
                       Impl.Dispatchers.Reference (CB).all;
            begin
               if CBR in RCL.Impl.Dispatchers.Client_Dispatcher'Class then
                  S.Add (RCL.Impl.Dispatchers.Client_Dispatcher'Class (CBR).Client);

               elsif CBR in RCL.Impl.Dispatchers.Service_Dispatcher'Class then
                  S.Add (RCL.Impl.Dispatchers.Service_Dispatcher'Class (CBR).Service);

               elsif CBR in RCL.Impl.Dispatchers.Subscription_Dispatcher'Class then
                  S.Add (RCL.Impl.Dispatchers.Subscription_Dispatcher'Class (CBR).Subscription);

               elsif CBR in RCL.Impl.Dispatchers.Timer_Dispatcher'Class then
                  if not Timers.Is_Canceled (RCL.Impl.Dispatchers.Timer_Dispatcher'Class (CBR).Timer) then
                     S.Add (RCL.Impl.Dispatchers.Timer_Dispatcher'Class (CBR).Timer);
                  end if;

               else
                  raise Program_Error with "Unknown callback";
               end if;
            end;
         end loop;
      end return;
   end Init;

   -------------
   -- Iterate --
   -------------

   function Iterate (This : Set) return Set_Iterators.Forward_Iterator'Class is
      (Iterator'(Over => This.Self));

   ----------
   -- Next --
   ----------

   function Next (I        : Iterator;
                  Position : Cursor) return Cursor is
      (Find_Next_Valid (I.Over.all, Advance (I.Over.all, Position)));

   ----------
   -- Size --
   ----------

   function Size (This : Set;
                  Kind : Kinds) return Natural is
     (case Kind is
         when Client       => Natural (This.Impl.Size_Of_Clients),
         when Service      => Natural (This.Impl.Size_Of_Services),
         when Subscription => Natural (This.Impl.Size_Of_Subscriptions),
         when Timer        => Natural (This.Impl.Size_Of_Timers));

   ----------
   -- Wait --
   ----------

   function Wait (This    : in out Set;
                  Timeout : ROS2_Duration := Forever) return Wait_Outcomes
   is
      use Rcl_Types_H;

      Ret : constant Rcl_Ret_T :=
              Rcl_Wait (This.Impl'Access,
                        C.long (Timeout * 1_000_000_000.0)); -- Nanosecs
   begin
      case Ret is
         when RMW_RET_OK =>
            return Triggered;
         when RMW_RET_TIMEOUT =>
            return Impl.Wait.Timeout;
         when others =>
            Logging.Warn ("Wait failed with code" & Ret'Img);
            return Error;
      end case;
   end Wait;

end RCL.Impl.Wait;
