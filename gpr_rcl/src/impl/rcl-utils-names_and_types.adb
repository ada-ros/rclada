with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;

package body RCL.Utils.Names_And_Types is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Vector) is
   begin
      Check (Rmw_Names_And_Types_Fini (This.Impl'Access));
   exception
      when E : others =>
         Logging.Error ("Vectors.Finalize: " & Exception_Information (E));
   end Finalize;

   -----------
   -- Names --
   -----------

   function Names (This : Vector; Pos : Positive) return String is
      Elems : array (1 .. This.Length) of CS.Chars_Ptr with
        Convention => C,
        Import,
        Address => This.Impl.Names.Data;
   begin
      return Value (Elems (Pos));
   end Names;

   -----------
   -- Types --
   -----------

   function Types (This : Vector; Pos : Positive) return String is
      Elems : array (1 .. This.Length) of CS.Chars_Ptr with
        Convention => C,
        Import,
        Address => This.Impl.Types.Data;
   begin
      return Value (Elems (Pos));
   end Types;

   ------------
   -- Length --
   ------------

   function Length (This : Vector) return Natural is
     (Natural (This.Impl.Names.Size));

   ----------
   -- To_C --
   ----------

   function To_C (This : aliased in out Vector) return access Rmw_Names_And_Types_T is
     (This.Impl'Access);

end RCL.Utils.Names_And_Types;
