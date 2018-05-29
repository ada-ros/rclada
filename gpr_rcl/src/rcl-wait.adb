with RCL.Subscriptions;

package body RCL.Wait is

   procedure Add (This : in out Set;
                  Sub  :        Subscriptions.C_Subscription) is
   begin
      null;
   end Add;

   function Check (This : Set;
                   Kind : Kinds;
                   Pos  : Positive) return Boolean is (Check (This, Kind, Pos));
   --  Manually check if a member was triggered (after wait return!)

   function Element (This : Set; Pos : Cursor) return Trigger is (Element (This, Pos));

   procedure Finalize (This : in out Set) is null;

   function Has_Element (C : Cursor) return Boolean is (Has_Element (C));

   function Init (Num_Subscriptions : Natural := 0) return Set is (Init (Num_Subscriptions));
   --  At least one of these must be nonzero

   function Iterate (This : Set) return Set_Iterators.Forward_Iterator'Class is (Iterate (This));

   overriding function Next (Object   : Iterator;
                             Position : Cursor) return Cursor is (Next (Object, Position));

   function Wait (This    : in out Set;
                  Timeout : Duration := Duration'Last) return Wait_Outcomes is (Wait (This, Timeout));

end RCL.Wait;
