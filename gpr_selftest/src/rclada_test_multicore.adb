with Ada.Command_Line; use Ada.Command_Line;

with RCL.Executors.Concurrent;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

with System;

procedure Rclada_test_multicore is
   
   --  Demo using concurrent executor dispatch. 
   --  It requires one argument which is the amount of tasks generated per second
   --  The consumers of these tasks take 1 second to complete, so as long as
   --  you generate as many tasks as cores (which is the size of the default pool)
   --  the processing should be barely able to keep up
   
   use RCL;      
   
begin  
   if Argument_Count < 1 then 
      Logging.Error ("First argument must be amount of jobs per second");
      return;
   end if;
   
   declare
      Pool     : aliased Executors.Concurrent.Executor (4, 1, System.Max_Priority);   
      Executor : constant access Executors.Executor'Class := 
--                     Nodes.Default_Executor'Access;
                   Pool'Access;
      
      Node     :         Nodes.Node := Nodes.Init (Name      => Utils.Command_Name,
                                                   Namespace => "/",
                                                   Executor  => Executor);
      
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
      begin
--           delay 1.0;
         Logging.Info (Msg ("data").Get_String & " done");
      end Process_Work;
      
      task Boss;
      task body Boss is
      begin
         loop
            delay 1.0;
            Publish_Work;
         end loop;
      end Boss;
      
   begin
      Node.Subscribe (Support, Topic, Process_Work'Unrestricted_Access);
      
      Executor.Spin (During => Forever);
   end;
end Rclada_test_multicore;
