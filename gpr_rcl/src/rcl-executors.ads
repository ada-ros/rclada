with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Conversion;

with RCL.Callbacks;
limited with RCL.Nodes;
with RCL.Wait;

with System;

package RCL.Executors is

   --  Executors encapsulate multithreading. They're not provided by the
   --  C RCL, but they exist in both rclcpp and rclpy.
   
   --  Essentially, they allow to have several nodes running simultaneously in
   --  the same process.
   
   --  They aren't necessarily multithreading. In rclada, two impls are provided,
   --  a single-task one and a multi-task one.
   
   --  Executors are expected to outlive nodes (they should be library-level).
   --  Nodes self-manage their registration-unregistration
   
   type Executor is abstract tagged limited private;
   
   procedure Dispatch (This   : in out Executor;
                       Node   : access Nodes.Node'Class;
                       Handle :        Callbacks.Handle) is abstract;
   --  This is the only procedure that derived Executors must override
   
      
   procedure Add (This :         in out Executor; 
                  Node : aliased in out Nodes.Node'Class);
   
   procedure Remove (This :         in out Executor; 
                     Node : aliased in out Nodes.Node'Class);
   
   procedure Spin (This   : in out Executor; 
                   Once   :        Boolean       := False;
                   During :        ROS2_Duration := 0.1);
   
   function Spin_Once (This    : in out Executor;
                       Timeout :        ROS2_Duration;
                       Node    : access Nodes.Node'Class := null) return Boolean;
   --  Will spin on the given node or all of them when null
   
private
   
   type Node_Access is not null access all Nodes.Node'Class;
   
   function To_Ptr is new Ada.Unchecked_Conversion (Node_Access, System.Address);
   
   use all type System.Address;
   
   function "<" (L, R : Node_Access) return Boolean is
      (To_Ptr (L) < To_Ptr (R));
   
   package Node_Sets is new Ada.Containers.Ordered_Sets (Node_Access);
   
   type Executor is abstract tagged limited record
      Nodes : Node_Sets.Set;
   end record;
   
   procedure Common_Dispatch (Node   : access Nodes.Node'Class;
                              Handle :        Callbacks.Handle);
   --  The actual logic of the dispatch that derived dispatchers may use
   --    if they do not do anything special
   
   procedure Trigger (This : in out Executor'Class;
                      Set  : in out Callbacks.Set;
                      T    :        RCL.Wait.Trigger);

end RCL.Executors;
