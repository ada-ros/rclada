pragma Warnings (Off);
with Ada.Text_Io; use Ada.Text_IO;
pragma Warnings (On);

with Ada.Unchecked_Conversion; -- Uh oh...

with C_Strings; pragma Unreferenced (C_Strings);

with Interfaces.C.Extensions;
with Interfaces.C.Strings; pragma Unreferenced (Interfaces.C.Strings);

with Rcl_Types_H;

package RCL is
   
   ROS_Exception : exception;

private
   
   --  To simplify the use of the low-level binding through child packages,
   --    some convenience things are declared here, not intended for the
   --    actual users of this library.
  
   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   package CX renames Interfaces.C.Extensions;
   
   use C_Strings;
   
   use all type C.int;
   use all type C.size_t;
   use all type C.unsigned_char;
   
   procedure Check (Ret : rcl_types_h.Rcl_Ret_T);
   
   function To_Boolean (Ret : CX.Bool) return Boolean is (Ret /= 0);
   
end RCL;
