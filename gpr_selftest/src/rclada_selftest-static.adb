with Ada.Unchecked_Conversion;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Debug_Pools; use GNAT.Debug_Pools;
with GNAT.Source_Info;

with RCL.Allocators;
with RCL.Calendar;
with RCL.Contexts;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Timers;
with RCL.Utils;

with Rclada_Selftest.Handmade;

with ROSIDl.Types;
with ROSIDl.Typesupport;

procedure Rclada_Selftest.Static is

   use RCL;
   use ROSIDL.Types;
   use all type RCL.Calendar.Time;

   Pool      : aliased Debug_Pool;
   Allocator : aliased Allocators.Allocator (Pool'Unchecked_Access);

   Use_Debug_Allocator : constant Boolean := True;

   Success : Boolean := False;

   ----------
   -- Test --
   ----------

   procedure Test is
      --  Nested so the allocator can be checked for mem leaks

      Clock : Calendar.Clock;

      Support : constant ROSIDl.Typesupport.Message_Support :=
                  ROSIDL.Typesupport.Get_Message_Support
                    ((if Argument_Count >= 1
                     then ROSIDL.Package_Name (Argument (1))
                     else "rosidl_generator_ada"),
                     (if Argument_Count >= 2
                      then Argument (2)
                      else "Test"));

      Topic : constant String := "/rclada_test";
      Node  :          Nodes.Node := Nodes.Init
        (Utils.Command_Name,
         Options => (Allocator => (if Use_Debug_Allocator
                                   then Allocator'Unchecked_Access
                                   else Allocators.Global_Allocator),
                     others    => <>));

      Pub   :          Publishers.Publisher := Node.Publish (Support, Topic);
      Topic_Done :     Boolean := False with Volatile;

      ------------
      -- Sender --
      ------------

      procedure Sender (Node    : in out Nodes.Node'Class;
                        Timer   : in out Timers.Timer;
                        Elapsed :        Duration) is
         pragma Unreferenced (Elapsed, Node);
         Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

      begin
         --  Create and fill the message with the proven dynamic facilities.
         --  TODO: send a second message created & filled statically.

         if not Topic_Done then
            Logging.Info ("Chatting");

            --  Primitive types
            Msg ("number").As_Int64 := Test_Int;
            Msg ("text").Set_String (Topic);
            Msg ("bounded_string").Set_String (Test_String);
            Msg ("real").As_Float64 := Test_Real;

            --  Structured types
            Msg ("time").Get_Message.Field ("sec").As_Int32 := Test_Int;

            --  Arrays
            Msg ("dynamic_array").As_Array.Resize (Test_Size);
            for I in 1 .. Msg ("dynamic_array").As_Array.Length loop
               Msg ("dynamic_array").As_Array.Element (I).As_Float32 := Float32 (I);
            end loop;

            pragma Assert (Msg ("static_array").As_Array.Length = 8);
            for I in 1 .. Msg ("static_array").As_Array.Length loop
               Msg ("static_array").As_Array.Element (I).As_Int32 := Int32 (I);
            end loop;

            Msg ("bounded_array").As_Array.Resize (Test_Size);
            Msg ("bounded_array").As_Array.Element (Test_Size).As_Int8 := Test_Size;

            for I in 1 .. Msg ("fix_msg_arr").As_Array.Length loop
               Msg ("fix_msg_arr").As_Array.Element (I).Get_Message.Field ("sec").As_Int32 := Int32 (I);
            end loop;

            Msg ("dyn_msg_arr").As_Array.Resize (Test_Size);
            pragma Assert (Msg ("dyn_msg_arr").As_Array.Length = Test_Size);
            for I in 1 .. Msg ("dyn_msg_arr").As_Array.Length loop
               Msg ("dyn_msg_arr").As_Array.Element (I).Get_Message.Field ("sec").As_Int32 := Int32 (I);
            end loop;

            --  Matrices
            declare
               Mat : ROSIDL.Dynamic.Matrix_View renames Msg ("matrix").As_Matrix;
            begin
               pragma Assert (Mat.Size = 0);
               Mat.Resize (Matrix_Indices);

               Logging.Info ("Filling as array");
               for I in 1 .. Mat.Size loop
                  Mat.As_Array.Element (I).As_Uint64 := Uint64 (I);
               end loop;

               Logging.Info ("Filling as matrix");
               for I in 1 .. Matrix_Indices (1) loop
                  for J in 1 .. Matrix_Indices (2) loop
                     for K in 1 .. Matrix_Indices (3) loop
                        Mat.Element ((I, J, K)).As_Uint64 := Uint64 (I * 100 + J * 10 + K);
                     end loop;
                  end loop;
               end loop;
            end;

            Logging.Info ("Publishing message");
            Pub.Publish (Msg);
         else
            Timer.Cancel;
         end if;
      exception
         when others =>
            Topic_Done := True;
            raise;
      end Sender;

      --------------
      -- Receiver --
      --------------

      procedure Receiver (Node : in out Nodes.Node'Class;
                          Dyn  : in out ROSIDL.Dynamic.Message;
                          Info :        ROSIDL.Message_Info) is
         pragma Unreferenced (Info, Node);

         Msg : constant access Handmade.Message :=
                 Handmade.Utils.To_Message_Access (Dyn.To_Ptr);
      begin
         Topic_Done := True;

         Dyn.Print_Metadata;

         Logging.Info ("Got chatter");

         --  Primitive types
         pragma Assert (Msg.Number = Test_Int);
         pragma Assert (Msg.Real   = Test_Real);

         --  Strings
         pragma Assert (Get_String (Msg.Text) = Topic);
         pragma Assert (+Msg.Bounded = Test_string); -- alternative syntax for Get_String

         --  TODO

         Success := Success or True;
         Logging.Info ("Topic testing done");
      end Receiver;

      --  TODO: test static service typesupport
      --  TODO: test static action typesupport

      Start : Calendar.Time;

   ----------
   -- TEST --
   ----------

   begin
      pragma Assert (not Clock.Is_Valid, "Uninitialized clock says it's valid!");
      Clock.Init (Calendar.ROS);
      pragma Assert (Clock.Is_Valid, "Initialized clock says it's invalid!");
      Start := Clock.Now;

      Node.Subscribe (Support, Topic, Receiver'Unrestricted_Access);
      Node.Timer_Add (0.1,            Sender'Unrestricted_Access);

      while not Topic_Done loop
         Node.Spin;
         Nodes.Default_Executor.Spin;
         --  Only one of the two is needed, but this way we test both
      end loop;

      Logging.Info ("Last built: "
                    & GNAT.Source_Info.Compilation_ISO_Date & " "
                    & GNAT.Source_Info.Compilation_Time);

      pragma Assert (Success, "Some partial test failed!");

      Logging.Info ("Test successful");
      Logging.Info ("Elapsed seconds (computed):  " & Duration'Image (Clock.Now - Start));
      Logging.Info ("Elapsed seconds (from clock):" & Duration'Image (Clock.Elapsed));
   end Test;

begin
   if Use_Debug_Allocator then
      Allocators.Set_Global_Allocator (Allocator'Unchecked_Access);
   end if;

   Test;

   --  The node is finalized on exit of Test. This causes a (harmless?) error at
   --  https://github.com/ros2/rcl/blob/241f3a5f51e9af93650f418dfd7090e9601a2d80/rcl/src/rcl/timer.c#L216
   --  with the timer, which is still ticking in the node, so it should
   --  finalize normally. Indeed removing the finalizatin is reported as a leak,
   --  and re-doing it is reported as a double-free. So, that's not the problem.
   --  It might be in the C side? To keep track in future releases (FIXME).

   if Use_Debug_Allocator then
      New_Line;
      Pool.Print_Info_Stdout;

      if Pool.Current_Water_Mark /= 0 then
         Logging.Warn ("There is leaked memory");
         raise Constraint_Error with "There is leaked memory";
      end if;
   end if;

   --  Finalize manually and early the global context so user count matches:
   Contexts.Global_Context.Finalize;

   pragma Assert (Contexts.User_Count = 0, "Remaining user at test end");
end Rclada_Selftest.Static;
