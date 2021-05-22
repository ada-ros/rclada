--  with Rmw_Qos_Profiles_H; use Rmw_Qos_Profiles_H;
with Rmw_Types_H;        use Rmw_Types_H;

package RCL.QoS is

   --  Options to set Quality of Service for publishers/subscribers

   subtype Profile is Rmw_Qos_Profile_T;

   --  Predefined profiles

   package Profiles is

      --  The C imports are re-exported in our rclada_c.c because they're
      --  declared as static in rmw/qos_profiles.h

      function Default return Profile
        with Import,
        Convention => C,
        External_Name => "get_rmw_qos_profile_default";

      function Sensor_Data return Profile
      --  Lossy, HiFreq topics (UDP-like)
        with Import,
        Convention => C,
        External_Name => "get_rmw_qos_profile_sensor_data";

   end Profiles;

   procedure Print (This : Profile; Name : String := "anonymous");
   --  For debugging contents, essentially

end RCL.QoS;
