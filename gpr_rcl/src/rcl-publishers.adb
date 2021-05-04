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

   overriding procedure Finalize (This : in out Publisher) is
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
     (Boolean
        (Rcl_Publisher_Is_Valid
             (Publisher => This.Impl'Access)));

   -------------
   -- Publish --
   -------------

   procedure Publish (This : in out Publisher;
                      Msg  : in out ROSIDL.Dynamic.Message)
   is
   begin
      This.Publish (Msg.To_Ptr);
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish (This : in out Publisher;
                      Msg  : System.Address)
   is
   begin
      Check
        (Rcl_Publish
           (publisher   => This.Impl'Access,
            ros_message => Msg,
            Allocation  => null));
      --  TODO: what about Allocation (new in Crystal)? Seems to be the way
      --  to pass an allocator. Verify that a plain rcl_allocator_t is the
      --  expected type here.
   end Publish;

end RCL.Publishers;
