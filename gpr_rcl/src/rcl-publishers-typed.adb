with Ada.Unchecked_Deallocation;

package body RCL.Publishers.Typed is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Publisher) is
      procedure Free is new Ada.Unchecked_Deallocation (Publishers.Publisher,
                                                        Base_Access);
   begin
      Free (This.Untyped);
   end Finalize;

   ----------
   -- Init --
   ----------

   function Init (Topic : String;
                  Options : Publishers.Options := Defaults) return Publisher
   is (Ada.Finalization.Limited_Controlled with
       Untyped => new Publishers.Publisher'
         (Node.Publish (Handling.Support, Topic, Options)));

   -------------
   -- Publish --
   -------------

   procedure Publish (This : in out Publisher;
                      Msg  : Handling.Message) is
   begin
      This.Untyped.Publish (Msg.Address);
   end Publish;

   -------------
   -- Publish --
   -------------

   procedure Publish (This : in out Publisher;
                      Msg  : Handling.C_Message) is
   begin
      This.Untyped.Publish (Msg'Address);
   end Publish;

end RCL.Publishers.Typed;
