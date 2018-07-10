with Ada.Finalization;

with Rcutils_Types_String_Array_H; use Rcutils_Types_String_Array_H;

package RCL.Utils.String_Arrays is

   type String_Array is new Ada.Finalization.Limited_Controlled with private;
   
   function Length (This : String_Array) return Natural;
   
   function Element (This : String_Array; Pos : Positive) return String;
   
   function To_C (This : aliased in out String_Array) return access Rcutils_String_Array_T;
   
private 
   
   type String_Array is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased Rcutils_String_Array_T := Rcutils_Get_Zero_Initialized_String_Array;
   end record;  
   
   overriding procedure Finalize (This : in out String_Array);   
   
end RCL.Utils.String_Arrays;
