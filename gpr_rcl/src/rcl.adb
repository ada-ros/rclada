with Ada.Exceptions;
with Ada.Finalization;

with Rcl_Allocator_H; use Rcl_Allocator_H;
with Rcl_Rcl_H; use Rcl_Rcl_H;

with Rcutils_Error_Handling_H; use Rcutils_Error_Handling_H;

with Rmw_Types_H; use Rmw_Types_H;

with Rcutils_Allocator_H; use Rcutils_Allocator_H;

with System;

package body RCL is

   generic
      type Int is range <>;
   procedure Generic_Check (I : Int);

   -------------------
   -- Generic_Check --
   -------------------

   procedure Generic_Check (I : Int) is
   begin
      if I /= RMW_RET_OK then
         raise ROS_Exception with "Code:" & I'Img;
      else
         null;
--           Put_Line ("CHECKED CALL: " & I'Img);
      end if;
   end Generic_Check;

   -----------
   -- Check --
   -----------

   procedure Check (Ret : rcl_types_h.Rcl_Ret_T) is
      procedure Internal is new Generic_Check (Rcl_Types_H.Rcl_Ret_T);
   begin
      Internal (Ret);
   end Check;

   -----------------
   -- Initializer --
   -----------------

   type Initializer is new Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Initialize (This : in out Initializer);
   overriding procedure Finalize   (This : in out Initializer);

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Initializer) is
      pragma Unreferenced (This);
      Gnat_Argc : C.int;
      Gnat_Argv : System.Address;

      pragma Import (C, Gnat_Argc);
      pragma Import (C, Gnat_Argv);

      Allocator : constant Rcutils_Allocator_T :=
                    Rcutils_Get_Default_Allocator;
   begin
      Check (Rcl_Init
             (Argc      => Gnat_Argc,
              Argv      => Gnat_Argv,
              Allocator => Rcl_Allocator_T (Allocator)));

      Rcutils_Reset_Error;
   exception
      when E : others =>
         Put_Line ("Exception while initializing rcl:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (This : in out Initializer) is
      pragma Unreferenced (This);
   begin
      Check (Rcl_Shutdown);
   exception
      when E : others =>
         Put_Line ("Exception while finalizing rcl:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ---------------
   -- Singleton --
   ---------------

   RCL_Init : Initializer with Unreferenced;

begin
   --     Put_Line ("ROS2_Duration'Last =" & ROS2_Duration'Last'Image);
   null;
end RCL;
