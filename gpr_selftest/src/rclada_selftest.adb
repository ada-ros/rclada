with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics;

with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Timers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Types;
with ROSIDL.Typesupport;

procedure Rclada_Selftest is
   use RCL;

   use all type ROSIDL.Types.Int64;

   Support : constant ROSIDl.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ((if Argument_Count >= 1 then Argument (1) else "rosidl_generator_ada"),
                  (if Argument_Count >= 2 then Argument (2) else "Test"));

   Topic : constant String := "/rclada_test";
   Node  :          Nodes.Node           := Nodes.Init (Utils.Command_Name);
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
         Msg ("text").Set_String (Topic);
         Msg ("bounded_string").Set_String ("12345678");
         Msg ("real").As_Float64 := ROSIDL.Types.Float64 (Ada.Numerics.Pi);
         Pub.Publish (Msg);
      else
         Timer.Cancel;
      end if;
   exception
      when others =>
         Done := True;
         raise;
   end Sender;

   --------------
   -- Receiver --
   --------------

   procedure Receiver (Msg  : in out ROSIDL.Dynamic.Message;
                       Info :        ROSIDL.Message_Info) is
      pragma Unreferenced (Info);
   begin
      Done := True;
      Logging.Info ("Got chatter");
      Msg.Print_Metadata;
      pragma Assert (Msg ("number").As_Int64 = 6976,  "int64 failed");
      pragma Assert (Msg ("text").Get_String = Topic, "string failed");
      pragma Assert (Msg ("bounded_string").Get_String = "12345678", "bounded_string failed");
      pragma Assert (Msg ("real").As_Float64 = ROSIDL.Types.Float64 (Ada.Numerics.Pi), "float64 failed");
   end Receiver;

begin
   Logging.Set_Name (Utils.Command_Name);
   Node.Subscribe (Support, Topic, Receiver'Unrestricted_Access);
   Node.Timer_Add (0.1,            Sender'Unrestricted_Access);

   while not Done loop
      Node.Spin;
   end loop;

   Logging.Info ("Test successful");
end Rclada_Selftest;
