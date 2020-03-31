with Ada.Exceptions; use Ada.Exceptions;

with RCL.Allocators;
with RCL.Logging;

with Rcl_Init_H;         use Rcl_Init_H;
with Rcl_Init_Options_H; use Rcl_Init_Options_H;

with Rcutils_Error_Handling_H; use Rcutils_Error_Handling_H;

with System.Atomic_Counters; use System.Atomic_Counters;

package body RCL.Init is

   Active_Contexts : aliased Atomic_Unsigned := 0;
   --  Initial value is zero

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Context : aliased in out Contexts.Context) is
      Gnat_Argc : C.Int          with Import, Convention => C;
      Gnat_Argv : System.Address with Import, Convention => C;

      --  TODO: allow using init options and an allocator not the default one
      Init_Options : aliased Rcl_Init_Options_T :=
                       Rcl_Get_Zero_Initialized_Init_Options;
   begin
      Rcutils_Reset_Error;

      --  TODO: allow using a custom allocator instead of the global one.
      Check
        (Rcl_Init_Options_Init
           (Init_Options => Init_Options'Access,
            Allocator    => Allocators.Global_Allocator.To_C.all));

      Check
        (Rcl_Init
           (Argc    => Gnat_Argc,
            Argv    => Gnat_Argv,
            Options => Init_Options'Access,
            Context => Context.To_C));

      Increment (Active_Contexts);
   exception
      when E : others =>
         Logging.Warn ("Exception while initializing rcl:");
         Logging.Warn (Ada.Exceptions.Exception_Information (E));
   end Initialize;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Context : aliased in out Contexts.Context) is
   begin
      Check
        (Rcl_Shutdown
           (Context => Context.To_C));

      if Decrement (Active_Contexts) then
         Logging.Shutdown;
      end if;
   exception
      when E : others =>
         Logging.Warn ("Exception while finalizing rcl:");
         Logging.Warn (Ada.Exceptions.Exception_Information (E));
   end Shutdown;

   ----------------
   -- User_Count --
   ----------------

   function User_Count return Natural is (Natural (Active_Contexts));

end RCL.Init;
