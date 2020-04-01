with RCL.Nodes;

with ROSIDL.Typesupport;

package RCL.Publishers.Impl is

   --  This package is reserved for the implementation, no use to users (ha)

   function Init (Node     : access Nodes.Node;
                  Msg_Type :        ROSIDL.Typesupport.Message_Support;
                  Topic    :        String) return Publisher;

   type Reference (Impl : access Rcl_Publisher_T) is limited null record with
     Implicit_Dereference => Impl;

   function To_C (This : in out Publisher) return Reference;

private

   function To_C (This : in out Publisher) return Reference is
     (Reference'(Impl => This.Impl'Access));

end RCL.Publishers.Impl;
