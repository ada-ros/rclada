with Ada.Command_Line; use Ada.Command_Line;
with Ada.Numerics;

with RCL.Calendar;
with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Timers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Types;
with ROSIDL.Typesupport;

--  with System.Address_Image;

procedure Rclada_Selftest is

   use RCL;
   use ROSIDL.Types;

   use all type RCL.Calendar.Time;

   Clock : Calendar.Clock;

   Support : constant ROSIDl.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ((if Argument_Count >= 1 then Argument (1) else "rosidl_generator_ada"),
                  (if Argument_Count >= 2 then Argument (2) else "Test"));

   Topic : constant String := "/rclada_test";
   Node  :          Nodes.Node           := Nodes.Init (Utils.Command_Name);
   Pub   :          Publishers.Publisher := Node.Publish (Support, Topic);
   Topic_Done :     Boolean := False with Volatile;

   Test_Int  : constant := 6976;
   Test_Real : constant := Ada.Numerics.Pi;
   Test_Size : constant := 6;

   Matrix_Indices : constant ROSIDL.Dynamic.Matrix_Indices := ( 2,  3, 4);
   Matrix_Strides : constant ROSIDL.Dynamic.Matrix_Indices := (24, 12, 4);

   procedure Assert_Matrix (Mat : ROSIDL.Dynamic.Matrix_View) is
   begin
      pragma Assert (Mat.Size = 2 * 3 * 4);
      pragma Assert (Mat.Dimensions = Matrix_Indices'Length);
      for I in 1 .. Mat.Dimensions loop
         pragma Assert (Mat.Label  (I) = ROSIDL.Dynamic.Default_Names (I));
         pragma Assert (Mat.Stride (I) = Matrix_Strides (I));
      end loop;

      for E of Mat.As_Array loop
         pragma Assert (E.As_Uint64 /= 0);
      end loop;

      for I in 1 .. Matrix_Indices (1) loop
         for J in 1 .. Matrix_Indices (2) loop
            for K in 1 .. Matrix_Indices (3) loop
               pragma Assert (Mat.Element ((I, J, K)).As_Uint64 = Uint64 (I*100 + J*10 + K));
            end loop;
         end loop;
      end loop;
   end Assert_Matrix;

   ------------
   -- Sender --
   ------------

   procedure Sender (Node    : in out Nodes.Node'Class;
                     Timer   : in out Timers.Timer;
                     Elapsed :        Duration) is
      pragma Unreferenced (Elapsed);
      Msg : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

   begin
      if not Topic_Done then
         Logging.Info ("Chatting");

         --  Primitive types
         Msg ("number").As_Int64 := Test_Int;
         Msg ("text").Set_String (Topic);
         Msg ("bounded_string").Set_String ("12345678");
         Msg ("real").As_Float64 := Test_Real;

         --  Structured types
         Msg ("time").Get_Message.Field ("sec").As_Int32 := Test_Int;

         --  Arrays
         Msg ("dynamic_array").As_Array.Resize (Test_Size);
         for I in 1 .. Msg ("dynamic_array").As_Array.Length loop
            Msg ("dynamic_array").As_Array.Element (I).As_Float32 := Float32 (I);
         end loop;

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
                     Mat.Element ((I, J, K)).As_Uint64 := Uint64 (I*100 + J*10 + K);
                  end loop;
               end loop;
            end loop;

            Assert_Matrix (Mat);
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
                       Msg  : in out ROSIDL.Dynamic.Message;
                       Info :        ROSIDL.Message_Info) is
      pragma Unreferenced (Info, Node);
   begin
      Topic_Done := True;
      Msg.Print_Metadata;

      Logging.Info ("Got chatter");

      --  Primitive types
      pragma Assert (Msg ("number").As_Int64 = 6976,  "int64 failed");
      pragma Assert (Msg ("text").Get_String = Topic, "string failed");
      pragma Assert (Msg ("bounded_string").Get_String = "12345678", "bounded_string failed");
      pragma Assert (Msg ("real").As_Float64 = Test_Real, "float64 failed");

      --  Structured types
      pragma Assert (Msg ("time.sec").As_Int32 = Test_Int, "message field failed");

      --  Arrays
      pragma Assert (Msg ("dynamic_array").As_Array.Length = Test_Size, "dynamic resize failed");
      for I in 1 .. Msg ("dynamic_array").As_Array.Length loop
--           Logging.Info (Msg ("dynamic_array").As_Array.Element (I).As_Float32.Element.all'Img);
--           Logging.Info (System.Address_Image (Msg ("dynamic_array").As_Array.Element (I).As_Float32.Element.all'Address));
         pragma Assert (Msg ("dynamic_array").As_Array.Element (I).As_Float32 = Float32 (I), "dynamic array assignment failed");
      end loop;

      for I in 1 .. Msg ("static_array").As_Array.Length loop
         pragma Assert (Msg ("static_array").As_Array.Element (I).As_Int32 = Int32 (I), "dynamic array assignment failed");
      end loop;

      pragma Assert (Msg ("bounded_array").As_Array.Element (Test_Size).As_Int8 = Test_Size, "bounded array assignment failed");

      pragma Assert (Msg ("fix_msg_arr").As_Array.Length /= 0);
      for I in 1 .. Msg ("fix_msg_arr").As_Array.Length loop
         pragma Assert (Msg ("fix_msg_arr").As_Array.Element (I).Get_Message.Field ("sec").As_Int32 = Int32 (I));
      end loop;

      pragma Assert (Msg ("dyn_msg_arr").As_Array.Length = Test_Size);
      for I in 1 .. Msg ("dyn_msg_arr").As_Array.Length loop
         pragma Assert (Msg ("dyn_msg_arr").As_Array.Element (I).Get_Message.Field ("sec").As_Int32 = Int32 (I));
      end loop;

      Assert_Matrix (Msg ("matrix").As_Matrix);

      Logging.Info ("Topic testing done");
   end Receiver;

   Service_Support : constant ROSIDl.Typesupport.Service_Support :=
                       ROSIDL.Typesupport.Get_Service_Support
                         ("rosidl_generator_ada", "Test");

   Service_Name : constant String := "rclada_test_service";

   Service_Done : Boolean := False with Volatile;

   -----------
   -- Adder --
   -----------

   procedure Adder (Node : in out Nodes.Node'Class;
                    Req  : in out ROSIDL.Dynamic.Message;
                    Resp : in out ROSIDL.Dynamic.Message)
   is
      pragma Unreferenced (Node);
      A : constant UInt64 := Req ("a").As_UInt64;
      B : constant UInt64 := Req ("b").As_UInt64;
   begin
      Logging.Info ("Got request, serving" & A'Img & " +" & B'Img);
      Resp ("sum").As_UInt64 := A + B;
      Logging.Info ("Service testing done");
   end Adder;

   ---------------------
   -- Client_Listener --
   ---------------------

   procedure Client_Listener (Node : in out Nodes.Node'Class;
                              Resp : ROSIDL.Dynamic.Message) is
      pragma Unreferenced (Node);
   begin
      Service_Done := True;
      Logging.Info ("Got reply, sum is" & Resp ("sum").As_Uint64.Image);
      Logging.Info ("Client testing done");
   end Client_Listener;

   Request : ROSIDL.Dynamic.Message :=
               ROSIDL.Dynamic.Init (Service_Support.Request_Support);

   Start : Calendar.Time;
begin
   Logging.Set_Name (Utils.Command_Name);

   pragma Assert (not Clock.Is_Valid, "Uninitialized clock says it's valid!");
   Clock.Init (Calendar.ROS);
   pragma Assert (Clock.Is_Valid, "Initialized clock says it's invalid!");
   Start := Clock.Now;

   Node.Serve (Service_Support, Service_Name, Adder'Unrestricted_Access);

   Request ("a").As_UInt64 := 2;
   Request ("b").As_UInt64 := 3;
   Node.Client_Call (Service_Support, Service_Name, Request, Client_Listener'Unrestricted_Access);
   --  Non-blocking client test

   Node.Subscribe (Support, Topic, Receiver'Unrestricted_Access);
   Node.Timer_Add (0.1,            Sender'Unrestricted_Access);

   while not (Topic_Done and then Service_Done) loop
      Node.Spin;
   end loop;

   --  Blocking client test (callback version)
   declare
      procedure Get_Sum (Node : in out Nodes.Node'Class;
                         Resp : ROSIDL.Dynamic.Message) is
      begin
         Logging.Info ("Got reply, sum is" & Resp ("sum").As_Uint64.Image);
         Logging.Info ("Client blocking (procedure) testing done");
      end Get_Sum;
   begin
      Node.Client_Call (Service_Support,
                        Service_Name,
                        Request,
                        Get_Sum'Unrestricted_Access,
                        Timeout => 1.0);
   end;

   --  Blocking client test (function version)
   declare
      Response : constant ROSIDL.Dynamic.Shared_Message :=
                   Node.Client_Call (Service_Support,
                                     Service_Name,
                                     Request,
                                     Timeout => 1.0);
   begin
      Logging.Info ("Got reply, sum is" & Response ("sum").As_Uint64.Image);
      Logging.Info ("Client blocking (function) testing done");
   end;

   Logging.Info ("Test successful");
   Logging.Info ("Elapsed seconds (computed):  " & Duration'Image (Clock.Now - Start));
   Logging.Info ("Elapsed seconds (from clock):" & Duration'Image (Clock.Elapsed));
end Rclada_Selftest;
