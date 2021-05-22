private with Ada.Finalization;

with RCL.Nodes;
with ROSIDL.Static.Message;

generic
   with package Handling is new ROSIDL.Static.Message (<>);
   Node  : in out Nodes.Node'Class;
package RCL.Publishers.Typed is

   type Publisher (<>) is tagged limited private;

   function Init (Topic   : String;
                  Options : Publishers.Options := Defaults)
                  return Publisher;

   procedure Publish (This : in out Publisher;
                      Msg  : Handling.Message); -- Wrapped type

   procedure Publish (This : in out Publisher;
                      Msg  : Handling.C_Message);     -- Raw C type

private

   --  This controlled with a pointer is not really necessary. A bug in gnat
   --  9.3, that calls finalization too early, forces me to use it. Otherwise
   --  a simple type extension would suffice.

   type Base_Access is access RCL.Publishers.Publisher;

   type Publisher is new Ada.Finalization.Limited_Controlled with record
      Untyped : Base_Access;
   end record;

   overriding procedure Finalize (This : in out Publisher);

end RCL.Publishers.Typed;
