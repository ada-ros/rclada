with Ada.Finalization;
with Ada.Iterator_Interfaces;

with RCL.Clients.Impl;
with RCL.Services.Impl;
limited with RCL.Subscriptions;
with RCL.Timers;

with Rcl_Wait_H; use Rcl_Wait_H;

package RCL.Wait is

   --  Not really intended for clients, but the C structs are of so poor
   --    quality that even for internal use a manual binding is needed.
   
   type Kinds is (Client,
                  Service, 
                  Subscription, 
                  Timer);
   
   type Wait_Outcomes is (Error, Timeout, Triggered);
   
   type Trigger is record
      Kind  : Kinds;
      Index : Positive;
   end record;
   
   type Set (<>) is new Ada.Finalization.Limited_Controlled with private with 
     Default_Iterator  => Iterate,
     Iterator_Element  => Trigger,
     Constant_Indexing => Element;
   
   type Cursor is private;
   function Has_Element (C : Cursor) return Boolean;
   
   package Set_Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);
   
   function Init (Num_Clients       : Natural := 0;
                  Num_Services      : Natural := 0;
                  Num_Subscriptions : Natural := 0;
                  Num_Timers        : Natural := 0) return Set;
   --  At least one of these must be nonzero

   procedure Add (This : aliased in out Set; 
                  Cli  : aliased in out Clients.Impl.C_Client); 
   
   procedure Add (This : aliased in out Set; 
                  Srv  : aliased in out Services.Impl.C_Service); 
   
   procedure Add (This : aliased in out Set; 
                  Sub  : aliased in out Subscriptions.C_Subscription); 
   
   procedure Add (This  : aliased in out Set; 
                  Timer : aliased in out Timers.Timer_Id); 
   
   function Check (This : Set;
                   Kind : Kinds;
                   Pos  : Positive) return Boolean;
   --  Manually check if a member was triggered (after wait return!)
   
   function Element (This : Set; Pos : Cursor) return Trigger;
   
   function Iterate (This : Set) return Set_Iterators.Forward_Iterator'Class;
   
   overriding 
   procedure Finalize (This : in out Set);  
   
   function Size (This : Set;
                  Kind : Kinds) return Natural;
   
   function Wait (This    : in out Set;
                  Timeout : ROS2_Duration := Forever) return Wait_Outcomes;
   
private
   
   type Set is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased Rcl_Wait_Set_T := Rcl_Get_Zero_Initialized_Wait_Set;
      
      Self : access Set := Set'Unchecked_Access;
   end record;
   
   type Cursor is record
      T     : Trigger;
      Ended : Boolean;
   end record;
   
   Ended_Cursor : constant Cursor := (Ended  => True,
                                      others => <>);
   
   type Iterator is new Set_Iterators.Forward_Iterator with record
      Over : access constant Set;
   end record;
   
   overriding function First (I : Iterator) return Cursor;
   
   overriding function Next (I        : Iterator;
                             Position : Cursor) return Cursor;

end RCL.Wait;
