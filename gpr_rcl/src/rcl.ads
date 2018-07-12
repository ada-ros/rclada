--  pragma Restriction_Warnings (No_Allocators);
--  pragma Restriction_Warnings (No_Anonymous_Allocators);
--  pragma Restriction_Warnings (No_Standard_Allocators_After_Elaboration);
--  pragma Restriction_Warnings (No_Standard_Storage_Pools);

--  pragma Default_Storage_Pool (null);
--    Seems we hit a bug on reference types with this one...
--    It also hits false positives on No_Allocators it seems

with C_Strings; pragma Unreferenced (C_Strings);

with Interfaces.C.Extensions;
with Interfaces.C.Strings; pragma Unreferenced (Interfaces.C.Strings);

limited with RCL.Executors;

with Rcl_Types_H;

package RCL is
   
   ROS_Exception : exception;
   --  Raised on errors from the C layer
   --  Any raise will be a serious problem either in the user code or in ROS2
   
   RCL_Timeout   : exception;
   --  Raised on certain subprograms from the RCL Ada binding
   --  This is not necessarily a fatal error
   
   Forever : constant Duration := 
               Duration (Long_Long_Integer (Interfaces.C.Long'Last) / 2 / 1_000_000_000);
   --  Max nanoseconds that can be accepted by the C side
   --  The / 2 is needed because some internal overflow in the C side 
   
   subtype ROS2_Duration is Duration range Duration'First .. Forever;
   
   No_Executor : constant access Executors.Executor'Class := null;

private
   
   --  To simplify the use of the low-level binding through child packages,
   --    some convenience things are declared here, not intended for the
   --    actual users of this library.
  
   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   package CX renames Interfaces.C.Extensions;
   
   pragma Warnings (Off); --  Those are used in child packages   
   use          C_Strings;   
   use all type C.int;
   use all type C.Long;
   use all type C.size_t;
   use all type C.unsigned_char;
   pragma Warnings (On);
   
   pragma Warnings (Off);
   use all type CX.Bool;
   pragma Warnings (On);
   --  GPL 2018 has redefined that bool and versions prior now complain
   
   subtype Rcl_Error_Code is Rcl_Types_H.Rcl_Ret_T;  
   
   procedure Check (Ret : Rcl_Error_Code);
   
   Bool_False : constant CX.Bool := CX.Bool'First;
   Bool_True  : constant CX.Bool := CX.Bool'Succ (Bool_False);
   --  Saves future problems with CX.bool being made a new Boolean instead of char
   
   function To_Boolean (Ret : CX.Bool) return Boolean is (Ret /= Bool_False);
   
end RCL;
