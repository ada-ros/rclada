with Ada.Exceptions; use Ada.Exceptions;

with RCL.Allocators.Impl;
with RCL.Logging;

with Rcl_Rcl_H;                use Rcl_Rcl_H;

with Rcutils_Error_Handling_H; use Rcutils_Error_Handling_H;

with System.Atomic_Counters; use System.Atomic_Counters;

package body RCL.Init is
   
   -----------
   -- Users --
   -----------

   Users : aliased Atomic_Unsigned := 0;
   --  Initial value is zero

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Allocator : Allocators.Handle;
                         Assurance : Assurances) is
      Gnat_Argc : C.Int          with Import, Convention => C;
      Gnat_Argv : System.Address with Import, Convention => C;
   begin
      if Users = 0 then
         Rcutils_Reset_Error;

         Check (Rcl_Init
                (Argc      => Gnat_Argc,
                 Argv      => Gnat_Argv,
                 Allocator => Allocators.Impl.To_C (Allocator.all)));
      elsif Assurance = Ensure_First then
         raise Program_Error with "Initialization happened too late";
      end if;

      Increment (Users);
--        Logging.Warn ("USER COUNT++:" & Users'Img);
   exception
      when E : others =>
         Put_Line ("Exception while initializing rcl:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      if Decrement (Users) then
         Logging.Shutdown;
         Check (Rcl_Shutdown);
      end if;
--        Logging.Warn ("USER COUNT--:" & Users'Img);
   exception
      when E : others =>
         Put_Line ("Exception while finalizing rcl:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;
   
   ----------------
   -- User_Count --
   ----------------

   function User_Count return Natural is (Natural (Users));

end RCL.Init;
