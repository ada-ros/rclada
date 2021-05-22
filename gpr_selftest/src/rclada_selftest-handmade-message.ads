with Builtin_Interfaces_Msg_Detail_Time_Ustruct_H;
use  Builtin_Interfaces_Msg_Detail_Time_Ustruct_H;

with ROSIDL.Static.Message;
with ROSIDL.Types;

package Rclada_Selftest.Handmade.Message is

   --  A test message, entirely handcrafted. This was done initially to help
   --  with the creation of the static message generator.

   use ROSIDL;

   --  GENERATION BEGIN  --

   --  Text in between the "generation marks" will have to be produced by our
   --  static generator. Chiefly, the following record.

   type Message is limited record
      Number  : aliased Types.Int64;
      Text    : aliased Types.ROS_String; -- Still untested, to replace by a high-level type.
      Bounded : aliased Types.ROS_String;
      Real    : aliased Types.Float64;

      Time    : Builtin_Interfaces_U_Msg_U_Time; -- Should be our own generated message, but for now...
   end record
     with Convention => C;

   package Sequences is new Types.Helpers.Sequences (Message);
   subtype Sequence is Sequences.Sequence;

   package Handling is new
     ROSIDL.Static.Message
       (Pkg        => "rclada",
        Name       => "Test",
        Part       => ROSIDL.Message,
        C_Message  => Message);

   --  GENERATION END  --

end Rclada_Selftest.Handmade.Message;
