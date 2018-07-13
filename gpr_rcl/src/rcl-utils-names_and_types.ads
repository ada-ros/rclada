with Ada.Finalization;

with rmw_names_and_types_h; use Rmw_Names_And_Types_H;

package RCL.Utils.Names_And_Types is 

   type Vector is new Ada.Finalization.Limited_Controlled with private;
   
   function Length (This : Vector) return Natural;
   
   function Names (This : Vector; Pos : Positive) return String;
   
   function Types (This : Vector; Pos : Positive) return String;   
   
   function To_C (This : aliased in out Vector) return access Rmw_Names_And_Types_T;
   
private 
   
   type Vector is new Ada.Finalization.Limited_Controlled with record
      Impl : aliased rmw_names_and_types_t := Rmw_Get_Zero_Initialized_Names_And_Types;
   end record;  
   
   overriding procedure Finalize (This : in out Vector);   
   
end RCL.Utils.Names_And_Types;
