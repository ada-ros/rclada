with ROSIDL.Static.Message;
with ROSIDL.Types;

package Rclada_Selftest.Handmade is

   --  A test message, entirely handcrafted. This was done initially to help
   --  with the creation of the static message generator.

   use ROSIDL;

   --  GENERATION BEGIN  --

   --  Text in between the "generation marks" will have to be produced by our
   --  static generator. Chiefly, the following record.

   type Message is record
      Number  : Types.Int64;
      Text    : Types.ROS_String; -- Still untested, to replace by a high-level type.
      Bounded : Types.ROS_String;
      Real    : Types.Float64;
   end record
     with Convention => C;

   package Utils is New
     ROSIDL.Static.Message
       (Pkg  => "rosidl_generator_ada",
        Name => "test",
        Msg  => Message);

   --  GENERATION END  --

end Rclada_Selftest.Handmade;
