package body RCL.Publishers.Impl is

   ----------
   -- Init --
   ----------

   function Init (Node     : access Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return Publisher is
   begin
      return This : Publisher := (Ada.Finalization.Limited_Controlled with
                                  Impl => Rcl_Get_Zero_Initialized_Publisher,
                                  Node => Node,
                                  Opts => Rcl_Publisher_Get_Default_Options)
      do
         Check
           (Rcl_Publisher_Init
              (This.Impl'Access,
               Node.To_C.Ptr.Impl'Access,
               Msg_Type.To_C,
               C_Strings.To_C (Topic).To_Ptr,
               This.Opts'Access));
      end return;
   end Init;

end RCL.Publishers.Impl;
