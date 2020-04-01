with Rmw_Ret_Types_H; use Rmw_Ret_Types_H;

package body RCL is

   generic
      type Int is range <>;
   procedure Generic_Check (I : Int);

   -------------------
   -- Generic_Check --
   -------------------

   procedure Generic_Check (I : Int) is
   begin
      if I /= RMW_RET_OK then
         raise ROS_Exception with "Code:" & I'Img;
      else
         null;
--           Put_Line ("CHECKED CALL: " & I'Img);
      end if;
   end Generic_Check;

   -----------
   -- Check --
   -----------

   procedure Check (Ret : rcl_types_h.Rcl_Ret_T) is
      procedure Internal is new Generic_Check (Rcl_Types_H.Rcl_Ret_T);
   begin
      Internal (Ret);
   end Check;


end RCL;
