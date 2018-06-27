with Ada.Exceptions; use Ada.Exceptions;
with Ada.Tags;       use Ada.Tags;

with RCL.Logging;

package body RCL.Executors.Sequential is

   ----------
   -- Call --
   ----------

   overriding procedure Call (This : in out Executor;
                              CB   :        Impl.Callbacks.Callback'Class) is
      pragma Unreferenced (This);
   begin
      CB.Call;
   exception
      when E : others =>
         Logging.Error ("User callback with tag " & External_Tag (CB'Tag));
         Logging.Error ("raised: " & Exception_Information (E));
   end Call;

end RCL.Executors.Sequential;
