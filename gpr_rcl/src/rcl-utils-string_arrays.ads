with Ada.Finalization;
with Ada.Iterator_Interfaces;

with Rcutils_Types_String_Array_H; use Rcutils_Types_String_Array_H;

package RCL.Utils.String_Arrays is

   type String_Array is new Ada.Finalization.Limited_Controlled with private with
     Constant_Indexing => Element,
     Default_Iterator  => Iterate,
     Iterator_Element  => String;
   
   function Length (This : String_Array) return Natural;
   
   function Element (This : String_Array; Pos : Positive) return String;        
   
   type Cursor (<>) is private;
   
   function Element (This : String_Array; Pos : Cursor) return String;
   
   function Has_Element (Pos : Cursor) return Boolean;
   
   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);
   
   function Iterate (This : String_Array) return Iterators.Forward_Iterator'Class;
   
   function To_C (This : aliased in out String_Array) return access Rcutils_String_Array_T;
   
private 
   
   type Cursor (Arr : access constant String_Array) is record
      Pos : Positive;
   end record;   
   
   type String_Array is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased Rcutils_String_Array_T := Rcutils_Get_Zero_Initialized_String_Array;
   end record;  
   
   overriding procedure Finalize (This : in out String_Array);   
   
   function Element (This : String_Array; Pos : Cursor) return String is
     (This.Element (Pos.Pos));
   
   function Has_Element (Pos : Cursor) return Boolean is
     (Pos.Pos <= Pos.Arr.Length);
   
end RCL.Utils.String_Arrays;
