with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with RCL.Logging;

with Rcutils_Types_String_Array_H; use Rcutils_Types_String_Array_H;

package body RCL.Utils.Names_And_Types is

   type Iterator is
     new Iterators.Forward_Iterator with
      record
         V   : access constant Vector;
         Pos : Positive;
      end record;

   -----------
   -- First --
   -----------

   overriding function First (This : Iterator) return Cursor is (This.V, Positive'First);

   ----------
   -- Next --
   ----------

   overriding function Next
     (This   : Iterator;
      Position : Cursor) return Cursor is (This.V, Position.Pos + 1);

   -------------
   -- Iterate --
   -------------

   function Iterate (This : aliased Vector) return Iterators.Forward_Iterator'Class is
      (Iterator'(This'Unchecked_Access, Positive'First));


   -------------
   -- Element --
   -------------

   function Element (This : Vector; Pos : Positive) return Name_And_Type is
      N : constant String := This.Names (Pos);
      T : constant String := This.Types (Pos);
   begin
      return (N'Length, T'Length, N, T);
   end Element;

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
      --  The C side is an "associative map": There is one array of Names,
      --    and for each name there is another array, in this case of length 1,
      --    containing the type corresponding to that name.

      Elems : array (1 .. This.Length) of Rcutils_String_Array_T with
        Convention => C,
        Import,
        Address => This.Impl.Types.all'Address;
      --  Binding reports the array of string arrays as a pointer to the first string_array

      Elem  : CS.Chars_Ptr with
      --  Since we know theres only one element, we can bypass it and just point
      --  to the first element in it.
        Convention => C,
        Import,
        Address => Elems (Pos).data;
   begin
      return Value (Elem);
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
