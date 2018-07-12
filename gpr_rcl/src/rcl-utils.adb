with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;  use Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;

package body RCL.Utils is

   function Command_Name return String is
      (Simple_Name (Ada.Command_Line.Command_Name));

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Initshut) is
   begin
      if This.On_Initialize /= null then
         This.On_Initialize.all;
      end if;
   exception
      when E : others =>
         Put_line ("Exception when initializing:");
         Put_line (Exception_Information (E));
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (This : in out Initshut) is
   begin
      if This.On_Finalize /= null then
         This.On_Finalize.all;
      end if;
   exception
      when E : others =>
         Put_line ("Exception when finalizing:");
         Put_line (Exception_Information (E));
   end Finalize;

end RCL.Utils;
