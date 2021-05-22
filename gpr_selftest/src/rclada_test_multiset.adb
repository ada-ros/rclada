with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers.Typed;

with ROSIDL.Static.Rclada.Std_Msgs.Messages.String; use ROSIDL.Static.Rclada;
with ROSIDL.Types;
with ROSIDL.Typesupport;

procedure Rclada_Test_Multiset is

   --  Just a simple check that several callbacks are properly handled

   use RCL;

   Topic_1 : constant String := "/chat1";
   Topic_2 : constant String := "/chat2";

   Node : Nodes.Node'Class := Nodes.Init ("rclada_test_multiset");

   package Publishers Is
     new RCL.Publishers.Typed (Std_Msgs.Messages.String.Handling,
                               Node);

   --  package Pub1 is new Nodes.Typed_Publish (Std_Msgs.Messages.String.Handling,
   --                                           Node,
   --                                           Topic_1);
   --  package Pub2 is new Nodes.Typed_Publish (Std_Msgs.Messages.String.Handling,
   --                                           Node,
   --                                           Topic_2);

   --  Previous commented approach works the same

   Pub1 : Publishers.Publisher := Publishers.Init (Topic_1);
   Pub2 : Publishers.Publisher := Publishers.Init (Topic_2);

   Msg : Std_Msgs.Messages.String.Handling.Message;

   generic
      Id : String;
   procedure Listener (Node : in out Nodes.Node'Class;
                       Msg  :        Std_Msgs.Messages.String.Message;
                       Info :        ROSIDL.Message_Info);

   procedure Listener (Node : in out Nodes.Node'Class;
                       Msg  :        Std_Msgs.Messages.String.Message;
                       Info :        ROSIDL.Message_Info)
   is
      pragma Unreferenced (Node, Info);
   begin
      Logging.Info ("At " & Id
                    & " I heard " & ROSIDL.Types.Get_String (Msg.Data));
   end Listener;

   procedure Listen_1 is new Listener ("listener 1");
   procedure Listen_2 is new Listener ("listener 2");

   procedure Subscribe_1 is new Nodes.Typed_Subscribe
     (Std_Msgs.Messages.String.Handling,
      Listen_1);

   Procedure Subscribe_2 is new Nodes.Typed_Subscribe
     (Std_Msgs.Messages.String.Handling,
      Listen_2);

begin
   Subscribe_1 (Node, Topic_1);
   Subscribe_2 (Node, Topic_2);

   ROSIDL.Types.Set_String (Msg.Data.Data, "hello 1");
   Pub1.Publish (Msg);

   ROSIDL.Types.Set_String (Msg.Data.Data, "saluton 2"); -- longer string on purpose
   Pub2.Publish (Msg);

   Node.Spin (During => 1.0);
end Rclada_Test_Multiset;
