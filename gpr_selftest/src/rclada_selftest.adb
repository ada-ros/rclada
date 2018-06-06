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

--  with System.Address_Image;

procedure Rclada_Selftest is
   use RCL;
   use ROSIDL.Types;

   Support : constant ROSIDl.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ((if Argument_Count >= 1 then Argument (1) else "rosidl_generator_ada"),
                  (if Argument_Count >= 2 then Argument (2) else "Test"));

   Topic : constant String := "/rclada_test";
   Node  :          Nodes.Node           := Nodes.Init (Utils.Command_Name);
   Pub   :          Publishers.Publisher := Node.Publish (Support, Topic);
   Done  :          Boolean := False with Volatile;

   Test_Int  : constant := 6976;
   Test_Real : constant := Ada.Numerics.Pi;
   Test_Size : constant := 6;

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
