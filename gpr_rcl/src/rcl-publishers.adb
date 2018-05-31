with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;
with RCL.Nodes; pragma Unreferenced (RCL.Nodes);

package body RCL.Publishers is

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Publisher) is
   begin
      if This.Is_Valid then
         Check (Rcl_Publisher_Fini (This.Impl'Access,
                                    This.Node.Impl'Access));
      end if;
   exception
      when E : others =>
         Logging.Error ("Publisher.Finalize raised: " &
                          Exception_Information (E));
   end Finalize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Publisher) return Boolean is
    (To_Boolean
        (Rcl_Publisher_Is_Valid (This.Impl'Access, null)));

   -------------
   -- Publish --
   -------------

   procedure Publish (This : in out Publisher;
                      Msg  : in out ROSIDL.Dynamic.Message)
   is
   begin
      Check (Rcl_Publish (This.Impl'Access, Msg.To_Ptr));
   end Publish;

end RCL.Publishers;
