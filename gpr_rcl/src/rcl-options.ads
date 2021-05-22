with RCL.Allocators;
with RCL.QoS;

package RCL.Options is

   --  Options to configure certains aspects like topic QoS, etc

   type Topic_Options is tagged record
      Allocator   : Allocators.Handle := Allocators.Global_Allocator;
      QoS_Profile : QoS.Profile       := QoS.Profiles.Default;
   end record;

   Topic_Defaults : constant Topic_Options;

   --  Chainable modifiers for the options

   function Using (This    : Topic_Options;
                   Profile : QoS.Profile)
                   return Topic_Options;

   function Using (This      : Topic_Options;
                   Allocator : Allocators.Handle)
                   return Topic_Options;

   function Using (This      : Topic_Options;
                   Allocator : aliased in out Allocators.Allocator)
                   return Topic_Options;

private

   Topic_Defaults : constant Topic_Options :=
                      (Allocator   => <>,
                       QoS_Profile => <>);

   -----------
   -- Using --
   -----------

   function Using (This    : Topic_Options;
                   Profile : QoS.Profile)
                   return Topic_Options
   is (Allocator   => This.Allocator,
       Qos_Profile => Profile);

   function Using (This      : Topic_Options;
                   Allocator : Allocators.Handle)
                   return Topic_Options
   is (Allocator   => Allocator,
       Qos_Profile => This.QoS_Profile);

   function Using (This      : Topic_Options;
                   Allocator : aliased in out Allocators.Allocator)
                   return Topic_Options
   is (Allocator   => Allocator'Access,
       Qos_Profile => This.QoS_Profile);

end RCL.Options;
