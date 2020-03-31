with Ada.Command_Line; use Ada.Command_Line;

with GNAT.Debug_Pools; use GNAT.Debug_Pools;

with RCL.Allocators;
with RCL.Executors.Concurrent;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Rclada_Test_Allocators Is

   --  This is the multicore test but using GNAT debug pool as allocator
   --  Sample output for 4 cores:
   --     Total allocated bytes :  8095
   --     Total logically deallocated bytes :  8095
   --     Total physically deallocated bytes :  0
   --     Current Water Mark:  0
   --     High Water Mark:  415

   use RCL;

   Pool      : aliased Debug_Pool;
   Allocator : aliased Allocators.Allocator (Pool'Unchecked_Access);
begin
   if Argument_Count < 1 then
      Logging.Error ("First argument must be amount of jobs per second");
      return;
   end if;

   Allocators.Set_Global_Allocator (Allocator'Unchecked_Access);
   --  This shouldn't be necessary since we are explicitly passing it to
   --  both node and executor. But to err on the safe side...

   declare
      Executor : aliased Executors.Concurrent.Executor;

      Node     :         Nodes.Node := Nodes.Init (Name      => Utils.Command_Name,
                                                   Namespace => "/",
                                                   Options   =>
                                                     (Executor  => Executor'Unchecked_Access,
                                                      Allocator => Allocator'Unchecked_Access));

      Support  : constant ROSIDl.Typesupport.Message_Support :=
                   ROSIDL.Typesupport.Get_Message_Support ("std_msgs", "String");

      Topic    : constant String := "/chatter";

      Publisher : Publishers.Publisher := Node.Publish (Support, Topic);

      Workload  : constant Positive := Positive'Value (Argument (1));

      Job_Id    : Positive := 1;

      Msg       : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

      ------------------
      -- Publish_Work --
      ------------------

      procedure Publish_Work is
      begin
         for I in 1 .. Workload loop
            Msg ("data").Set_String ("Job" & Job_Id'Img);
            Logging.Info ("Creating job" & Job_Id'Img);
            Publisher.Publish (Msg);
            Job_Id := Job_Id + 1;
         end loop;
      end Publish_Work;

      ------------------
      -- Process_Work --
      ------------------

      procedure Process_Work (Node : in out Nodes.Node'Class;
                              Msg  : in out ROSIDL.Dynamic.Message;
                              Info :        ROSIDL.Message_Info) is
         pragma Unreferenced (Node, Info);
      begin
         delay 1.0;
         Logging.Info (Msg ("data").Get_String & " done");
      end Process_Work;

      Test_Period : constant Duration := 10.0;

      task Boss is
         entry Start;
      end Boss;

      task body Boss is
      begin
         accept Start;

         for I in 1 .. Positive (Test_Period) loop
            Publish_Work;
            delay 1.0;
         end loop;
      end Boss;

   begin
      Node.Subscribe (Support, Topic, Process_Work'Unrestricted_Access);
      Boss.Start;
      Executor.Spin (During => Test_Period);
      Executor.Shutdown;

      Logging.Info ("Test period ended, will dump pool info in 3 seconds...");
      delay 3.0;
   end;

   Pool.Print_Info_Stdout;

   if Pool.Current_Water_Mark /= 0 then
      raise Constraint_Error with "There is leaked memory";
   end if;
end Rclada_Test_Allocators;
