with RCL.Logging;
with RCL.Nodes;
with Rcl.Publishers;
with Rcl.Timers;

with ROSIDL.Dynamic;
with ROSIDL.Types;
with ROSIDL.Typesupport;

procedure Rclada_Selftest is
   use RCL;

   use all type ROSIDL.Types.Int64;

   Support : constant ROSIDl.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support ("rosidl_generator_ada", "Test");

   Topic : constant String := "/rclada_test";
   Node  :          Nodes.Node           := Nodes.Init ("rclada_tester");
   Pub   :          Publishers.Publisher := Node.Publish (Support, Topic);
   Done  :          Boolean := False;

   ------------
   -- Sender --
   ------------

   procedure Sender (Timer   : in out Timers.Timer;
                     Elapsed :        Duration) is
      pragma Unreferenced (Elapsed);
      Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

   begin
      if not Done then
         Logging.Info ("Chatting");
         Msg ("number").As_Int64 := 6976;
         Pub.Publish (Msg);
      else
         Timer.Cancel;
      end if;
   end Sender;

   --------------
   -- Receiver --
   --------------

   procedure Receiver (Msg  : in out ROSIDL.Dynamic.Message;
                       Info :        ROSIDL.Message_Info) is
      pragma Unreferenced (Info);
   begin
      Logging.Info ("Got chatter");
      pragma Assert (Msg ("number").As_Int64 = 6976, "int64 failed");
      Done := True;
   end Receiver;

begin
   Node.Subscribe (Support, Topic, Receiver'Unrestricted_Access);
   Node.Timer_Add (0.1,            Sender'Unrestricted_Access);

   while not Done loop
      Node.Spin;
   end loop;

   Logging.Info ("Test successful");
end Rclada_Selftest;
