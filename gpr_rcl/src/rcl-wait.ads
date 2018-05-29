with Ada.Finalization;

limited with RCL.Subscriptions;

with Rcl_Wait_H; use Rcl_Wait_H;

package RCL.Wait is

   --  Not really intended for clients, but the C structs are of so poor
   --    quality that even for internal use a manual binding is needed.
   
   type Wait_Outcomes is (Error, Timeout, Triggered);
   
   type Set (<>) is new Ada.Finalization.Limited_Controlled with private with
     Default_Iterator => Iterate;
   
   type Iterator is null record;
   
   function Init (Num_Subscriptions : Natural := 0) return Set;
   --  At least one of these must be nonzero
   
   procedure Add (This : in out Set; 
                  Sub  :        Subscriptions.Subscription);
   
   overriding 
   procedure Finalize (This : in out Set);
   
   function Iterate (This : in out Set) return Iterator;
   
   function Wait (This    : in out Set;
                  Timeout : Duration := Duration'Last) return Wait_Outcomes;
   
private
   
   type Set is new Ada.Finalization.Limited_Controlled with record
      Impl : Rcl_Wait_Set_T := Rcl_Get_Zero_Initialized_Wait_Set;
   end record;

end RCL.Wait;
