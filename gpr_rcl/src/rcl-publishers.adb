with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;
with RCL.Nodes.Impl;

package body RCL.Publishers is

   ----------
   -- Init --
   ----------

   procedure Init (This     : in out Publisher;
                   Msg_Type :        ROSIDL.Typesupport.Message_Support;
                   Topic    :        String) is
   begin
      Check
        (Rcl_Publisher_Init
           (This.Impl'Access,
            Nodes.Impl.To_C (This.Node.all).Ptr,
            Msg_Type.To_C,
            C_Strings.To_C (Topic).To_Ptr,
            This.Opts'Access));
   end Init;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Publisher) is
   begin
      if This.Is_Valid then
         Check (Rcl_Publisher_Fini (This.Impl'Access,
                Nodes.Impl.To_C (This.Node.all).Ptr));
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
   (True); -- FIXME
--    (To_Boolean
--        (Rcl_Publisher_Is_Valid (This.Impl'Access, null)));

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
