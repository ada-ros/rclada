with Builtin_Interfaces_Msg_Detail_Time_Ustruct_H;
use  Builtin_Interfaces_Msg_Detail_Time_Ustruct_H;

with ROSIDL.Static.Message;
with ROSIDL.Types;

package Rclada_Selftest.Handmade is

   --  A test message, entirely handcrafted. This was done initially to help
   --  with the creation of the static message generator.

   use ROSIDL;

   --  GENERATION BEGIN  --

   --  Text in between the "generation marks" will have to be produced by our
   --  static generator. Chiefly, the following record.

   type Message is limited record
      Number  : Types.Int64;
      Text    : aliased Types.ROS_String; -- Still untested, to replace by a high-level type.
      Bounded : aliased Types.ROS_String;
      Real    : Types.Float64;

      Time    : Builtin_Interfaces_U_Msg_U_Time; -- Should be our own generated message, but for now...
   end record
     with Convention => C;

   package Utils is New
     ROSIDL.Static.Message
       (Pkg  => "rosidl_generator_ada",
        Name => "test",
        Msg  => Message);

   --  GENERATION END  --

end Rclada_Selftest.Handmade;
