with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;

package body RCL.Utils.String_Arrays is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out String_Array) is
   begin
      Check (Rcutils_String_Array_Fini (This.Impl'Access));
   exception
      when E : others =>
         Logging.Error ("String_Arrays.Finalize: " & Exception_Information (E));
   end Finalize;

   -------------
   -- Element --
   -------------

   function Element (This : String_Array; Pos : Positive) return String is
      Elems : array (1 .. This.Length) of CS.Chars_Ptr with
        Convention => C,
        Import,
        Address => This.Impl.Data;
   begin
      return Value (Elems (Pos));
   end Element;

   ------------
   -- Length --
   ------------

   function Length (This : String_Array) return Natural is
     (Natural (This.Impl.Size));

   ----------
   -- To_C --
   ----------

   function To_C (This : aliased in out String_Array) return access Rcutils_String_Array_T is
     (This.Impl'Access);

end RCL.Utils.String_Arrays;
