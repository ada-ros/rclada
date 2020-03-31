with RCL.Init;

package body RCL.Contexts is

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Context) is
   begin
      if This.State = Finalized then
         This.State := Initialized;
         Init.Initialize (This);
      end if;
   end Initialize;

   overriding procedure Finalize   (This : in out Context) is
   begin
      if This.State = Initialized then
         This.State := Finalized;
         Init.Shutdown (This);
      end if;
   end Finalize;

   The_Context : aliased Context;

   --------------------
   -- Global_Context --
   --------------------

   function Global_Context return not null access Context is
     (The_Context'Access);

end RCL.Contexts;
