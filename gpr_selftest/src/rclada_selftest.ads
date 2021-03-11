with Ada.Numerics;

with ROSIDL.Dynamic;

package Rclada_Selftest is

   --  Constants used in testing the messages

   Test_Int    : constant := 6976;
   Test_Real   : constant := Ada.Numerics.Pi;
   Test_Size   : constant := 6;
   Test_String : constant String := "D3ADB33F";

   Matrix_Indices : constant ROSIDL.Dynamic.Matrix_Indices := ( 2,  3, 4);
   Matrix_Strides : constant ROSIDL.Dynamic.Matrix_Indices := (24, 12, 4);

end Rclada_Selftest;
