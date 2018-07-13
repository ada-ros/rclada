with Ada.Finalization;
with Ada.Iterator_Interfaces;

with rmw_names_and_types_h; use Rmw_Names_And_Types_H;

package RCL.Utils.Names_And_Types is 

   type Name_And_Type (Name_Length, Type_Length : Natural) is record
      Name  : String (1 .. Name_Length);
      Ttype : String (1 .. Type_Length);
   end record;
   
   type Vector is new Ada.Finalization.Limited_Controlled with private with
     Constant_Indexing => Element,
     Default_Iterator  => Iterate,
     Iterator_Element  => Name_And_Type;
   
   function Element (This : Vector; Pos : Positive) return Name_And_Type;   
   
   function Length (This : Vector) return Natural;
   
   function Names (This : Vector; Pos : Positive) return String;
   
   function Types (This : Vector; Pos : Positive) return String;   
   
   type Cursor (<>) is private;
   
   function Element (This : Vector; Pos : Cursor) return Name_And_Type;
   
   function Has_Element (Pos : Cursor) return Boolean;
   
   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);
   
   function Iterate (This : Vector) return Iterators.Forward_Iterator'Class;
   
   function To_C (This : aliased in out Vector) return access Rmw_Names_And_Types_T;
   
private 
   
   type Cursor (V : access constant Vector) is record
      Pos : Positive;
   end record;
   
   type Vector is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased rmw_names_and_types_t := Rmw_Get_Zero_Initialized_Names_And_Types;
   end record;  
   
   overriding procedure Finalize (This : in out Vector);   
   
   function Element (This : Vector; Pos : Cursor) return Name_And_Type is
      (This.Element (Pos.Pos));
   
   function Has_Element (Pos : Cursor) return Boolean is (Pos.Pos <= Pos.V.Length);
   
end RCL.Utils.Names_And_Types;
