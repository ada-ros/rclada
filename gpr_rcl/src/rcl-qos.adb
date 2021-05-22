with RCL.Logging;

package body RCL.QoS is

   -----------
   -- Print --
   -----------

   procedure Print (This : Profile; Name : String := "anonymous") is
   begin
      Logging.Info ("QoS PROFILE " & Name);
      Logging.Info (This.History'Image);
      Logging.Info ("Depth:" & This.Depth'Image);
      Logging.Info (This.Reliability'Image);
      Logging.Info (This.Durability'Image);
      Logging.Info (This.Liveliness'Image);
   end Print;

end RCL.QoS;
