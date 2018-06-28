with Ada.Unchecked_Conversion;

with Stddef_H; use Stddef_H;

with System; use System;

package body RCL.Allocators is

   function To_Pool_Ptr is new Ada.Unchecked_Conversion (Address, Pool_Access);
   function To_Address  is new Ada.Unchecked_Conversion (Pool_Access, Address);

   --   Common functions for the C side
   function Allocate (Size : stddef_h.size_t; Pool_Addr : System.Address) return System.Address is (Null_Address) with Convention => C;

   procedure Deallocate (arg1 : System.Address; Pool_Addr : System.Address) with Convention => C;
   procedure Deallocate (arg1 : System.Address; Pool_Addr : System.Address) is
      Pool : Pool_Access := To_Pool_Ptr (Pool_Addr);
   begin
      null;
   end Deallocate;

   function Reallocate
     (Ptr       : System.Address;
      Size      : Stddef_H.Size_T;
      Pool_Addr : System.Address) return System.Address is (Null_Address) with Convention => C;

   function Zero_Allocate
     (Element_Count : Stddef_H.Size_T;
      Element_Size  : Stddef_H.Size_T;
      Pool_Addr     : System.Address) return System.Address is (Null_Address) with Convention => C;


   Global_Ada_Allocator : Allocator := (Pool => null);

   ----------------------
   -- Global_Allocator --
   ----------------------

   function Global_Allocator return Allocator is (Global_Ada_Allocator);

   ---------------------------
   -- Set_Default_Allocator --
   ---------------------------

   procedure Set_Global_Allocator (Alloc : Allocator) is
   begin
      Global_Ada_Allocator := Alloc;
   end Set_Global_Allocator;

   ----------
   -- To_C --
   ----------

   function To_C (This : Allocator) return Rcl_Allocator_T is
   begin
      if This.Pool = null then
         return Default_C_Allocator_Instance;
      else
         return Rcl_Allocator_T'(Allocate      => Allocate'Access,
                                 Deallocate    => Deallocate'Access,
                                 Reallocate    => Reallocate'Access,
                                 Zero_Allocate => Zero_Allocate'Access,
                                 State         => To_Address (This.Pool));
      end if;
   end To_C;

end RCL.Allocators;
