with Ada.Command_Line; use Ada.Command_Line;

with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Timers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Rclada_Double_Node is
   use RCL;

   Support : constant ROSIDl.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ((if Argument_Count >= 1 then Argument (1) else "rosidl_generator_ada"),
                  (if Argument_Count >= 2 then Argument (2) else "Test"));

   Topic  : constant String := "/rclada_test";

   Done   :          Boolean := False with Volatile;

   task Sender_Task;
   task Receiver_Task;

   ------------
   -- Sender --
   ------------

   task body Sender_Task is

      Node : Nodes.Node           := Nodes.Init (Utils.Command_Name & "_sender");
      Pub    : Publishers.Publisher := Node.Publish (Support, Topic);

      procedure Sender (Node    : in out Nodes.Node'Class;
                        Timer   : in out Timers.Timer;
                        Elapsed :        Duration) is
         pragma Unreferenced (Elapsed, Node);
         Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

      begin
         if not Done then
            Logging.Info ("Chatting");
            Pub.Publish (Msg);
         else
            Timer.Cancel;
         end if;
      exception
         when others =>
            Done := True;
            raise;
      end Sender;

   begin
      Node.Timer_Add (0.1,            Sender'Unrestricted_Access);

      while not Done loop
         Node.Spin;
      end loop;
   end Sender_Task;

   --------------
   -- Receiver --
   --------------

   task body Receiver_Task is

      Node : Nodes.Node           := Nodes.Init (Utils.Command_Name & "_receiver");

      procedure Receiver (Node : in out Nodes.Node'Class;
                          Msg  : in out ROSIDL.Dynamic.Message;
                          Info :        ROSIDL.Message_Info) is
         pragma Unreferenced (Info, Node);
      begin
         Done := True;
         Msg.Print_Metadata;

         Logging.Info ("Got chatter");
      end Receiver;

   begin
      Node.Subscribe (Support, Topic, Receiver'Unrestricted_Access);

      while not Done loop
         Node.Spin;
      end loop;
   end Receiver_Task;

begin
   Logging.Set_Name (Utils.Command_Name);
end Rclada_Double_Node;
