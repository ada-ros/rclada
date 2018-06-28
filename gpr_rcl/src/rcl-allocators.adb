with Ada.Unchecked_Conversion;

with Stddef_H; use Stddef_H;

with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

package body RCL.Allocators is

   function To_Pool_Ptr is new Ada.Unchecked_Conversion (Address, Pool_Access);
   function To_Address  is new Ada.Unchecked_Conversion (Pool_Access, Address);

   --   Common functions for the C side

   --------------
   -- Allocate --
   --------------

   function Allocate (Size      : stddef_h.size_t;
                      Pool_Addr : System.Address)
                      return System.Address with Convention => C;
   function Allocate (Size      : stddef_h.size_t;
                      Pool_Addr : System.Address)
                      return System.Address
   is
      Pool : constant Pool_Access := To_Pool_Ptr (Pool_Addr);
      Ptr  : Address;
   begin
      Pool.Allocate (Storage_Address          => Ptr,
                     Size_In_Storage_Elements => Storage_Count (Size),
                     Alignment                => 1);

      return Ptr;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (Ptr : System.Address; Pool_Addr : System.Address) with Convention => C;
   procedure Deallocate (Ptr : System.Address; Pool_Addr : System.Address) is
      Pool : constant Pool_Access := To_Pool_Ptr (Pool_Addr);
   begin
      Pool.Deallocate (Storage_Address          => Ptr,
                       Size_In_Storage_Elements => 0,
                       Alignment                => 1);
   end Deallocate;

   ----------------
   -- Reallocate --
   ----------------

   function Reallocate
     (Ptr       : System.Address;
      Size      : Stddef_H.Size_T;
      Pool_Addr : System.Address) return System.Address with Convention => C;
   function Reallocate
     (Ptr       : System.Address;
      Size      : Stddef_H.Size_T;
      Pool_Addr : System.Address) return System.Address is
   begin
      Deallocate (Ptr, Pool_Addr);
      return Allocate (Size, Pool_Addr);
   end Reallocate;

   -------------------
   -- Zero_Allocate --
   -------------------

   function Zero_Allocate
     (Element_Count : Stddef_H.Size_T;
      Element_Size  : Stddef_H.Size_T;
      Pool_Addr     : System.Address) return System.Address with Convention => C;
   function Zero_Allocate
     (Element_Count : Stddef_H.Size_T;
      Element_Size  : Stddef_H.Size_T;
      Pool_Addr     : System.Address) return System.Address
   is
      Size : constant Stddef_H.Size_T := Element_Count * Element_Size;
      Data : constant Address := Allocate (Size, Pool_Addr);
      Mem  : constant Storage_Array (1 .. Storage_Offset (Size)) := (others => 0)
        with Address => Data;
   begin
      return Data;
   end Zero_Allocate;


   ----------------------
   -- Global_Allocator --
   ----------------------

   Global_Ada_Allocator : Allocator := (Pool => null);

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
