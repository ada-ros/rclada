with RCL.Nodes;

package body RCL.Subscriptions is

   package body Typed is

      procedure Untyped_Callback (Node : in out Nodes.Node'Class;
                                  Msg  : in out ROSIDL.Dynamic.Message;
                                  Info :        ROSIDL.Message_Info);

      ---------------
      -- Subscribe --
      ---------------

      procedure Subscribe (Node  : in out Nodes.Node'Class;
                           Topic :        String) is
      begin
         Node.Subscribe (Msg_Type => Handling.Support,
                         Topic    => Topic,
                         Callback => Untyped_Callback'Unrestricted_Access);
      end Subscribe;

      ----------------------
      -- Untyped_Callback --
      ----------------------

      procedure Untyped_Callback (Node : in out Nodes.Node'Class;
                                  Msg  : in out ROSIDL.Dynamic.Message;
                                  Info :        ROSIDL.Message_Info)
      is
      begin
         Callback (Node,
                   Handling.To_Message_Access (Msg.To_Ptr).all,
                   Info);
      end Untyped_Callback;

   end Typed;


end RCL.Subscriptions;
