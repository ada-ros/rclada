with RCL.Allocators;
with RCL.Logging;
with RCL.Subscriptions;

with Rcl_Types_H;
with Rmw_Types_H;

package body RCL.Wait is



   ---------
   -- Add --
   ---------

   procedure Add (This : aliased in out Set;
                  Sub  : aliased in out Subscriptions.C_Subscription) is
   begin
      Check (Rcl_Wait_Set_Add_Subscription
               (This.Impl'Access,
                Sub.C'Access));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (This  : aliased in out Set;
                  Timer : aliased in out Timers.Timer_Id)
   is
   begin
      Check (Rcl_Wait_Set_Add_Timer
               (This.Impl'Access,
                Timers.To_C (Timer)));
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
                    T     => (Pos.T.Kind, Pos.T.Index + 1));
         elsif Pos.T.Kind = Kinds'Last then
            return Ended_Cursor;
         else
            return (Ended  => False,
                    T      => (Kinds'Succ (Pos.T.Kind), 1));
         end if;
      end if;
   end Advance;

   -----------
   -- Check --
   -----------

   function Check (This : Set;
                   Kind : Kinds;
                   Pos  : Positive) return Boolean
   is
      type Access_Checker is access function (Set : access constant Rcl_Wait_Set_T;
                                              Pos : C.Int)
                                              return CX.Bool with Convention => C;

      function Rclada_Wait_Set_Subscription_Check (Set : access constant Rcl_Wait_Set_T;
                                                   Pos : C.Int)
                                                   return CX.Bool with Import, Convention => C;

      function Rclada_Wait_Set_Timer_Check (Set : access constant Rcl_Wait_Set_T;
                                                   Pos : C.Int)
                                                   return CX.Bool with Import, Convention => C;

      Checkers : constant array (Kinds) of Access_Checker
        := (Subscription => Rclada_Wait_Set_Subscription_Check'Access,
            Timer        => Rclada_Wait_Set_Timer_Check'Access);

   begin
      return To_Boolean (Checkers (Kind).all (This.Impl'Access, C.Int (Pos) - 1));
   end Check;

   -------------
   -- Element --
   -------------

   function Element (This : Set; Pos : Cursor) return Trigger is (Pos.T);

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
           Check (This, Curr.T.Kind, Curr.T.Index)
         then
            return Curr;
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
      return Find_Next_Valid (I.Over.all, (T     => (Kinds'First, 1),
                                           Ended => False));
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Cursor) return Boolean is (not C.Ended);

   ----------
   -- Init --
   ----------

   function Init (Num_Subscriptions : Natural := 0;
                  Num_Timers        : Natural := 0) return Set is
   begin
      if Num_Subscriptions + Num_Timers = 0 then
         raise Constraint_Error with "Nothing to wait on!";
      end if;

      return S : Set do
         Check
           (Rcl_Wait_Set_Init
              (S.Impl'Access,
               Allocator                  => Allocators.Get_Default_Allocator,
               Number_Of_Clients          => 0,
               Number_Of_Guard_Conditions => 0,
               Number_Of_Services         => 0,
               Number_Of_Subscriptions    => C.Size_T (Num_Subscriptions),
               Number_Of_Timers           => C.Size_T (Num_Timers)));
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
         when Subscription => Natural (This.Impl.Size_Of_Subscriptions),
         when Timer        => Natural (This.Impl.Size_Of_Timers));

   ----------
   -- Wait --
   ----------

   function Wait (This    : in out Set;
                  Timeout : Duration := Duration'Last) return Wait_Outcomes
   is
      use Rcl_Types_H;
      use Rmw_Types_H;

      Ret : constant Rcl_Ret_T :=
              Rcl_Wait (This.Impl'Access,
                        C.long (Timeout * 1000**3)); -- Nanosecs
   begin
      case Ret is
         when RMW_RET_OK =>
            return Triggered;
         when RMW_RET_TIMEOUT =>
            return RCL.Wait.Timeout;
         when others =>
            Logging.Warn ("Wait failed with code" & Ret'Img);
            return Error;
      end case;
   end Wait;

end RCL.Wait;
