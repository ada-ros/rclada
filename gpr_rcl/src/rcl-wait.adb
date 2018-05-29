with RCL.Allocators;
with RCL.Logging;
with RCL.Subscriptions;

with ROSIDL.Types;

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

   -----------
   -- Check --
   -----------

   function Check (This : Set;
                   Kind : Kinds;
                   Pos  : Positive) return Boolean
   is
      function Rclada_Wait_Set_Subscription_Check (Set : access constant Rcl_Wait_Set_T;
                                                   Pos : C.Int)
                                                   return CX.Bool with
        Import,
        Convention => C;
   begin
      case Kind is
         when Subscription =>
            return To_Boolean (Rclada_Wait_Set_Subscription_Check (This.Impl'Access,
                               C.Int (Pos) - 1));
      end case;
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
                             Pos  : Cursor) return Cursor is
   begin
      case Pos.T.Kind is
         when Subscription =>
            if Pos.T.Index <= Integer (This.Impl.Size_Of_Subscriptions) then
               if Check (This, Subscription, Pos.T.Index) then
                  return Pos;
               else
                  return Find_Next_Valid (This, (T     => (Pos.T.Kind, Pos.T.Index + 1),
                                                 Ended => False));
               end if;
            else
               --  Nothing left to check
               return (T => <>, Ended => True);
            end if;
      end case;
   end Find_Next_Valid;

   -----------
   -- First --
   -----------

   function First (I : Iterator) return Cursor is
   begin
      return Find_Next_Valid (I.Over.all, (T     => (Subscription, 1),
                                           Ended => False));
   end First;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (C : Cursor) return Boolean is (not C.Ended);

   ----------
   -- Init --
   ----------

   function Init (Num_Subscriptions : Natural := 0) return Set is
   begin
      if Num_Subscriptions + Num_Subscriptions = 0 then
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
               Number_Of_Timers           => 0));
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

   function Next (Object   : Iterator;
                  Position : Cursor) return Cursor is
      (Find_Next_Valid (Object.Over.all, Position));

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
                        ROSIDL.Types.Int64 (Timeout * 1000)); -- Nanosecs
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
